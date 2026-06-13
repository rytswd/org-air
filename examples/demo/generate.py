#!/usr/bin/env python3
"""Generate the org-air demo dataset with dates RELATIVE TO TODAY.

Usage:  python3 generate.py [TARGET_DIR]   (default: ./ next to this script)

Re-run any time to refresh the relativity so every section stays
populated and the calendar keeps a constellation of marks.  Point
org-air at the output:

    (setq org-air-files (directory-files "/tmp/org-air" t "\\\\.org\\\\'")
          org-air-inbox-file "/tmp/org-air/inbox.org")
    M-x org-air
"""
import sys, os, datetime

TODAY = datetime.date.today()

def d(offset):
    """Active org timestamp <YYYY-MM-DD Dow> at TODAY+offset days."""
    dt = TODAY + datetime.timedelta(days=offset)
    return dt.strftime("<%Y-%m-%d %a>")

def inactive(offset):
    """Inactive [YYYY-MM-DD Dow] timestamp (counts as activity, not a plan)."""
    dt = TODAY + datetime.timedelta(days=offset)
    return dt.strftime("[%Y-%m-%d %a]")

def entry(title, *, todo=None, prio=None, tags=(), sched=None, dead=None,
          created=None, body=None):
    head = "* "
    if todo:  head += todo + " "
    if prio:  head += f"[#{prio}] "
    head += title
    if tags:  head += "  :" + ":".join(tags) + ":"
    lines = [head]
    if sched is not None: lines.append(f"  SCHEDULED: {d(sched)}")
    if dead is not None:  lines.append(f"  DEADLINE: {d(dead)}")
    props = []
    if created is not None: props.append(f"  :CREATED:  {inactive(created)}")
    if props:
        lines += ["  :PROPERTIES:", *props, "  :END:"]
    if body: lines.append("  " + body)
    return "\n".join(lines) + "\n"

# ── Files: 6 groups for origin variety + an inbox ───────────────────────
FILES = {}

FILES["inbox.org"] = "#+TITLE: Inbox\n#+FILETAGS: :inbox:\n\n" + "".join([
    entry("Call the plumber back about the leak", created=-1),
    entry("Idea: weekly review automation", created=-2),
    entry("Read the Rougier text-editor design paper", todo="TODO", created=-3),
    entry("Follow up with Sam on the proposal", todo="TODO", created=0),
    entry("Cancel the unused SaaS subscription", created=-4),
])

FILES["work.org"] = "#+TITLE: Work\n\n" + "".join([
    entry("Ship the quarterly board report", todo="TODO", prio="A",
          tags=("work","report"), dead=-5, created=-12),          # OVERDUE
    entry("Reply to the client escalation", todo="TODO",
          tags=("work","client"), created=-1),                    # no date → attention
    entry("Stand-up notes for the team", todo="TODO",
          tags=("work","meeting"), sched=0),                       # today
    entry("Prep the client demo deck", todo="TODO", prio="A",
          tags=("work","client"), sched=1),                        # tomorrow
    entry("Submit the expense report", todo="TODO",
          tags=("work","finance"), dead=2),                        # this week
    entry("1:1 with manager", todo="TODO", tags=("work","meeting"), sched=4),
    entry("Draft the OKRs for next quarter", todo="TODO",
          tags=("work","planning"), sched=8),                      # constellation
])

FILES["projects.org"] = "#+TITLE: Projects\n\n" + "".join([
    entry("Fix the production outage runbook", todo="TODO", prio="A",
          tags=("projects","urgent","infra"), dead=-2, created=-9), # OVERDUE
    entry("Design review: new onboarding flow", todo="TODO",
          tags=("projects","design"), sched=3),
    entry("Cut the v2 release branch", todo="TODO", prio="A",
          tags=("projects","release"), sched=6),
    entry("Triage the bug backlog", todo="TODO",
          tags=("projects","infra"), created=-1),                  # no date → attention
    entry("Refactor the importer module", todo="TODO",
          tags=("projects","code"), created=-34),                  # STALE
    entry("Write the architecture decision record", todo="TODO",
          tags=("projects","docs"), created=-28),                  # STALE
    entry("Plan the offsite agenda", todo="TODO",
          tags=("projects","planning"), sched=13),                 # constellation
])

FILES["home.org"] = "#+TITLE: Home\n\n" + "".join([
    entry("Pay the rent", todo="TODO", tags=("home","finance"), dead=-1, created=-6), # OVERDUE
    entry("Water the garden", todo="TODO", tags=("home","chores"), sched=0),          # today
    entry("Book the dentist appointment", todo="TODO",
          tags=("home","health"), sched=2),
    entry("Call mum", todo="TODO", tags=("home","family"), sched=5),
    entry("Renew the car insurance", todo="TODO",
          tags=("home","finance"), sched=10),                       # constellation
    entry("Sort the garage", todo="TODO", tags=("home","chores"), created=-40), # STALE
])

FILES["health.org"] = "#+TITLE: Health\n\n" + "".join([
    entry("Morning run — 5k", todo="TODO", tags=("health","fitness"), sched=1),
    entry("Meal-prep for the week", todo="TODO", tags=("health","food"), sched=0),
    entry("Schedule the annual check-up", todo="TODO",
          tags=("health",), created=-25),                           # STALE
    entry("Physio exercises", todo="TODO", tags=("health","fitness"), sched=15),  # constellation
])

FILES["learning.org"] = "#+TITLE: Learning\n\n" + "".join([
    entry("Finish the Rust ownership chapter", todo="TODO",
          tags=("reading","course"), sched=4),
    entry("Watch the distributed-systems lecture", todo="TODO",
          tags=("reading","course"), created=-30),                  # STALE
    entry("Notes: structure & interpretation", todo="TODO",
          tags=("reading","notes"), created=-45),                   # STALE
    entry("Practice keyboard shortcuts", todo="TODO",
          tags=("reading","emacs"), sched=18),                      # constellation
])

def main():
    target = sys.argv[1] if len(sys.argv) > 1 else os.path.dirname(os.path.abspath(__file__))
    os.makedirs(target, exist_ok=True)
    for name, content in FILES.items():
        with open(os.path.join(target, name), "w") as fh:
            fh.write(content)
    print(f"Wrote {len(FILES)} files to {target} (today = {TODAY})")

if __name__ == "__main__":
    main()
