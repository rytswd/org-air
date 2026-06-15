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
    # notify.sh — host-side hook; relays worker ever-events back to me.
    cat > /tmp/org-air-ace/notify.sh <<'SH'
    #!/usr/bin/env bash
    EVENT=$(cat)
    case "$EVER_TOPIC" in
      *.status) echo "$EVENT" >> /tmp/org-air-ace/status.log ;;
      *) pi-crosstalk send org-air-orchestrator --from "$EVER_TOPIC" "$EVENT" ;;
    esac
    SH
    chmod +x /tmp/org-air-ace/notify.sh
    ever hook add org-air3.work. -- /tmp/org-air-ace/notify.sh

## Worker seat spawn (sandboxed; per role = design|impl|test)
    ROLE=design
    cat > /tmp/org-air-ace/on-exit-$ROLE.sh <<SH
    #!/usr/bin/env bash
    ever pub org-air3.work.$ROLE.done "{\"agent\":\"org-air-$ROLE\",\"status\":\"exited\",\"exit_code\":\$EXIT_CODE}"
    SH
    chmod +x /tmp/org-air-ace/on-exit-$ROLE.sh
    chronoa new --daemon \
      --cwd /home/ryota/Coding/github.com/rytswd/org-air-$ROLE \
      --sandbox cwd \
      --sandbox-dir /home/ryota/Coding/github.com/rytswd/org-air/.jj \
      --sandbox-dir /home/ryota/Coding/github.com/rytswd/org-air/.git \
      --sandbox-dir /home/ryota/Coding/github.com/withre/ace-stack/data \
      --env EVER_DATA_DIR=/home/ryota/Coding/github.com/withre/ace-stack/data \
      --env GIT_CONFIG_GLOBAL=/dev/null \
      --env PI_NO_GATE=1 \
      --tag project=org-air --tag role=$ROLE \
      --on-exit /tmp/org-air-ace/on-exit-$ROLE.sh \
      org-air-$ROLE -- pi --no-session --sandbox --model opus
    chronoa send org-air-$ROLE "@/tmp/org-air-ace/prompt-$ROLE.md"
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
