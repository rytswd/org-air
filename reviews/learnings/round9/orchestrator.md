# Round-9 orchestrator learnings (post-restart + flush/spine churn)

1. WAIT FOR THE DECISIVE DESIGN RULING BEFORE INTEGRATING. I integrated the
   green-ready SPINE set on design's repeated-but-uninformed instruction, then
   had to supersede with FLUSH when design saw the render and reversed. A live
   A/B render choice must be settled in ONE authoritative message (final pick +
   exact change ids + "earlier blesses VOID") before any integration.

2. DON'T MESSAGE A MID-TURN WORKER EXCEPT AS A STEER. Crossed messages
   (re-points sent while a worker was mid-regen) spawned a thicket of sibling
   branches. Queue directives on my side; deliver after the worker's .done.

3. AFTER A RESTART, RE-ESTABLISH ONE LINEAR TIP + HARD FREEZE BEFORE LETTING
   SEATS RUN. Fresh eager seats committed after each "settled", and even made
   integration/mark-complete/scratch commits (the orchestrator's job), creating
   head-soup. Workers must NEVER touch integration/trunk/mark-complete.

4. jj DESCRIBE-PARK TRAP COST THE WORK. The restart lost uncommitted working
   copies left under empty `jj describe`-parked commits. Enforce `jj commit`
   early/often (never bare describe); verify non-empty with `jj diff -r @- --stat`.

5. CHECK FIXTURE-LEAK ON HAND-MERGES. My merge(qqpuoqlv,vxmtqvqm) "failed 4
   mockups" — those bytes were the flush-set leaking from a sibling, not a real
   mismatch. Let the test track (fixture↔code pairing owner) produce the green
   integration tip; don't hand-merge divergent worker branches.

## Addendum: ship was FLUSH, not spine
Test's learnings frame the outcome as "integrate qqpuoqlv lineage, defer
rskvknsz" — that is test's stale spine viewpoint. The ACTUAL ship is FLUSH
(rskvknsz + ryqqlzwprtux, trunk uoqvktux, byte-verified). Design reversed
spine->flush after seeing the render and re-reading D5b. Test's rebase-
relocation + fixture<->code pairing lessons are correct regardless of the
spine/flush framing. The meta-lesson: crossed messages got so tangled that
design AND test each held a stale view of what shipped; only a single
authoritative byte-verified ground-truth message resolved it.
