# ACE Orchestration — rebuild recipe + process lessons (persisted context)

This is the **only persisted** record of how org-air is built by the ACE
supervisor + sandboxed worker seats. Everything else the rig needs
(notify.sh, prompts, references, raw learnings) lives in `/tmp` and is
**expected to be wiped on reboot** — rebuild it from this doc. Do NOT
track ACE runtime artifacts in this git repo.

## Topology
- Repo: `/home/ryota/Coding/github.com/rytswd/org-air` (jj + git colocated)
- Worker jj workspaces: `../org-air-{design,impl,test}` — each is a full
  working copy; its `.jj/repo` points at the main repo's `.jj/repo` store.
- Ever store data: `/home/ryota/Coding/github.com/withre/ace-stack/data`
  (export as `EVER_DATA_DIR`).
- Supervisor (this agent) is reachable via crosstalk handle
  `org-air-orchestrator` (NOT a chronoa session — that name routes via
  `pi-crosstalk send org-air-orchestrator`).
- Ever topics: `org-air3.work.{design,impl,test}.{status,done,learnings}`.

## jj incantation (MANDATORY — yubikey/gpg signing is sandbox-denied)
    export GIT_CONFIG_GLOBAL=/dev/null
    jj --config signing.behavior=drop <cmd>
- `make clean` before EVERY verify (stale .elc shadows results).
- Commit early/often with `jj commit <paths>`. **NEVER bare `jj describe`**
  — it parks an empty `@` and the next edit silently rewrites it (caused
  data loss twice). Verify non-empty: `jj diff -r @- --stat`.

## Rig rebuild after a reboot (run from the repo)
    export EVER_DATA_DIR=/home/ryota/Coding/github.com/withre/ace-stack/data
    mkdir -p /tmp/org-air-ace
    # Topics MUST exist before `ever pub` (else "topic not found"); create all:
    for r in design impl test; do for e in status done learnings; do \
      ever topic create org-air3.work.$r.$e 2>/dev/null; done; done
    ever topic create org-air3.monitor.tick 2>/dev/null
    # notify.sh — host-side hook; relays worker ever-events back to me.
    # CRITICAL: the crosstalk --from tag MUST use the @ever: scheme or the ACL
    # denies it ("acl:unknown-sender") and the message silently never arrives.
    cat > /tmp/org-air-ace/notify.sh <<'SH'
    #!/usr/bin/env bash
    EVENT=$(cat)
    case "$EVER_TOPIC" in
      *.status) echo "$EVENT" >> /tmp/org-air-ace/status.log ;;
      *) pi-crosstalk send org-air-orchestrator --from "@ever:$EVER_TOPIC" "$EVENT" ;;
    esac
    SH
    chmod +x /tmp/org-air-ace/notify.sh
    ever hook add org-air3.work. -- /tmp/org-air-ace/notify.sh
    # Monitor timer (backstop: pings me on new commits / seat exits even if a
    # worker never publishes). Filters to WORKER-work descriptions only (so my
    # own orchestration/context commits are not reported). Dedupes via the
    # snapshot file + exited-<role> flags. BASE = the round's spec-landing tip
    # (here qqqzwtol); update it each round.
    cat > /tmp/org-air-ace/tick.sh <<'SH'
    #!/usr/bin/env bash
    export EVER_DATA_DIR=/home/ryota/Coding/github.com/withre/ace-stack/data
    export GIT_CONFIG_GLOBAL=/dev/null
    cd /home/ryota/Coding/github.com/rytswd/org-air 2>/dev/null || exit 0
    BASE=qqqzwtol   # round-10 spec-landing tip; bump per round
    RS="($BASE:: ~ $BASE) & ~empty() & (description(glob:\"R10*\") | description(glob:\"Design round-10*\") | description(glob:\"tests:*\") | description(glob:\"test:*\"))"
    STATE=/tmp/org-air-ace/monitor-commits.txt
    CUR=$(jj --config signing.behavior=drop log --no-graph -r "$RS" -T 'change_id.short() ++ "|" ++ description.first_line() ++ "\n"' 2>/dev/null)
    PREV=$(cat "$STATE" 2>/dev/null)
    if [ "$CUR" != "$PREV" ]; then
      NEW=$(comm -13 <(printf '%s' "$PREV" | sort) <(printf '%s' "$CUR" | sort))
      [ -n "$NEW" ] && pi-crosstalk send org-air-orchestrator --from "@ever:org-air3.monitor" "[monitor] new round-10 worker commits:
    $NEW"
      printf '%s' "$CUR" > "$STATE"
    fi
    for s in design impl test; do
      line=$(chronoa list 2>/dev/null | sed 's/\x1b\[[0-9;]*m//g' | grep "org-air-$s")
      flag=/tmp/org-air-ace/exited-$s
      if echo "$line" | grep -q "exited"; then
        [ ! -f "$flag" ] && { touch "$flag"; pi-crosstalk send org-air-orchestrator --from "@ever:org-air3.monitor" "[monitor] seat org-air-$s EXITED"; }
      else rm -f "$flag"; fi
    done
    SH
    chmod +x /tmp/org-air-ace/tick.sh
    # seed the snapshot once (so existing commits aren't re-reported), then arm:
    EVER_TOPIC=x /tmp/org-air-ace/tick.sh >/dev/null 2>&1
    ever hook add org-air3.monitor.tick -- /tmp/org-air-ace/tick.sh
    ever timer add --name org-air-monitor --every 3m org-air3.monitor.tick '{"tick":1}'

## Worker seat spawn (sandboxed; per role = design|impl|test)
    ROLE=design
    cat > /tmp/org-air-ace/on-exit-$ROLE.sh <<SH
    #!/usr/bin/env bash
    ever pub org-air3.work.$ROLE.done "{\"agent\":\"org-air-$ROLE\",\"status\":\"exited\",\"exit_code\":\$EXIT_CODE}"
    SH
    chmod +x /tmp/org-air-ace/on-exit-$ROLE.sh
    mkdir -p /tmp/org-air-ace/logs
    chronoa new --daemon --log /tmp/org-air-ace/logs/$ROLE.log \
      --cwd /home/ryota/Coding/github.com/rytswd/org-air-$ROLE \
      --sandbox cwd \
      --sandbox-dir /home/ryota/Coding/github.com/rytswd/org-air/.jj \
      --sandbox-dir /home/ryota/Coding/github.com/rytswd/org-air/.git \
      --sandbox-dir /home/ryota/Coding/github.com/withre/ace-stack/data \
      --sandbox-dir /home/ryota/.pi \
      --sandbox-ro /home/ryota/Coding/github.com/rytswd/pi-agent-extensions \
      --sandbox-ro /home/ryota/Coding/github.com/rytswd/pi-agent-extensions-extra \
      --env EVER_DATA_DIR=/home/ryota/Coding/github.com/withre/ace-stack/data \
      --env GIT_CONFIG_GLOBAL=/dev/null \
      --env PI_NO_GATE=1 \
      --tag project=org-air --tag role=$ROLE \
      --on-exit /tmp/org-air-ace/on-exit-$ROLE.sh \
      org-air-$ROLE -- pi --no-session
    chronoa send org-air-$ROLE "@/tmp/org-air-ace/prompt-$ROLE.md"
- **CRITICAL auth lesson:** do NOT pass `--no-extensions`, `--model`, or
  `--api-key`. pi's extensions (settings.json packages
  `pi-agent-extensions{,-extra}`) include `claude-max-fix` — the auth/model
  GATEWAY that routes billing to the plan. Without them pi uses raw oauth and
  every call 400s "You're out of extra usage" (a RED HERRING, not a real
  billing problem). The extension dirs MUST be `--sandbox-ro` so they load in
  the sandbox (else "cannot find module"). settings.json defaults
  model=claude-opus-4-8, so bare `pi --no-session` gives Opus 4.8. Verify the
  `chronoa output` status line reads `Opus 4.8 ❯ … ❯ org-air-$ROLE …` and a
  test message replies with no 400.
- Other gotchas: pi has no own sandbox flag (Landlock is chronoa's
  `--sandbox`); `--sandbox-dir ~/.pi` REQUIRED (auth + trust lock); `--log`
  needs a PATH; bisect spawn issues by starting with NO sandbox + a test
  message, then add `--sandbox` paths one at a time.
- Workers signal via `ever pub org-air3.work.$ROLE.{done,failed,learnings}`
  (reliable through the sandbox; the host hook relays to me). `/tmp`,
  `$XDG_RUNTIME_DIR`, `/nix/store` are always in sandbox scope.
- Demo data for screenshots: `python3 examples/demo/generate.py /tmp/org-air`.

## Hard-won process lessons (do not relearn)
1. **One authoritative message per decision.** A live A/B (e.g. flush vs
   spine rail) must be settled in ONE message: final pick + exact change
   ids + "earlier blesses are VOID". Crossed messages corrupted even the
   design authority's view of what shipped; only a byte-verified
   ground-truth message resolved it. Wait for the *decisive informed*
   ruling before integrating — do not act on a repeated-but-uninformed
   instruction.
2. **Workers NEVER touch integration / trunk / mark-complete / scratch
   commits.** That is the orchestrator's job. Eager fresh seats that
   committed "after settled" spawned head-soup (sibling branches). Enforce
   ONE linear tip + hard freeze; reseat fresh each round.
3. **Don't message a mid-turn worker except as a steer.** Re-points sent
   while a worker was mid-regen spawned divergent branches.
4. **Byte-verify what shipped.** Ancestry ≠ shipped bytes (a later commit
   overwrites a sibling's fixture bytes). Grep the actual fixture/render.
5. **jj rebase relocates a change-id**, silently re-pairing code with stale
   fixtures (`-s X -d Y` carried a blessed commit onto a later impl tip →
   pre-flush fixtures under flush code → false mockup fails). Don't rebase
   a named integration anchor; verify ancestry before merging; build
   integration off the CODE tip + `jj restore --from BLESSED tests/`.
6. **Fixture↔code pairing is the test deliverable** — prove every
   integration: `jj diff --from BLESSED --to TIP tests/fixtures` = 0
   (byte-identical), grep render .el for the feature marker, `make clean`
   before verify.

## Binary gate
- `make check` = compile + lint(0) + tests. Green = "Ran N tests, N as
  expected, 0 unexpected". Self-policing known-failures manifest must be
  EMPTY at integration. Tests are byte fixtures (svg is GUI-cosmetic only;
  every svg element needs a TTY/byte fallback).
