#!/usr/bin/env python3
"""Generate a LARGE org-air demo dataset (dates RELATIVE TO TODAY).

~500+ entries across dozens of files, including long Denote-style
filenames (YYYYMMDDTHHMMSS--long-title__tag_tag.org).  Deterministic
(seeded) so re-runs are stable; re-run any time to refresh relativity.

Usage:  python3 generate-large.py [TARGET_DIR]   (default /tmp/org-air)

    (setq org-air-files (directory-files "/tmp/org-air" t "\\\\.org\\\\'")
          org-air-inbox-file "/tmp/org-air/inbox.org")
"""
import sys, os, random, datetime

TARGET = sys.argv[1] if len(sys.argv) > 1 else "/tmp/org-air"
TODAY = datetime.date.today()
R = random.Random(20260712)  # deterministic (reseed = a fresh task set)

# Dates spread EVENLY across the current month +-2 months (~+-75 days) so no
# single month (least of all the current one) dominates the calendar dots.
SPREAD = 75

def stamp(offset, active=True):
    dt = TODAY + datetime.timedelta(days=offset)
    return dt.strftime("<%Y-%m-%d %a>" if active else "[%Y-%m-%d %a]")

WORDS = ("refactor importer module review quarterly board report ship release "
         "branch migrate database schema onboarding flow client demo deck "
         "incident runbook production outage retrospective planning sprint "
         "architecture decision record dependency upgrade security audit "
         "performance profiling cache invalidation api gateway rate limit "
         "documentation website accessibility pass localization strings "
         "weekly review automation inbox triage garden watering meal prep "
         "dentist appointment morning run reading notes distributed systems "
         "lecture taxes invoice renewal subscription budget forecast").split()
TAGS = ("work home health projects reading learning finance chores urgent "
        "report client meeting release design code docs notes course fitness "
        "food admin research ops infra ui backend").split()
TODOS = (["TODO"] * 9) + (["NEXT"] * 3) + (["WAIT"] * 2) + (["DONE"] * 4) + [None] * 3
REPEATERS = (None, None, None, None, " .+1d", " .+1w", " ++1w", " .+2d")

def title(n=None):
    n = n or R.randint(3, 9)
    return " ".join(R.choice(WORDS) for _ in range(n)).capitalize()

def slug(t):
    return "-".join(t.lower().split())[:90]

def entry():
    todo = R.choice(TODOS)
    prio = R.choice((None, None, None, "A", "B", "C"))
    tags = tuple(sorted(set(R.choice(TAGS) for _ in range(R.randint(0, 3)))))
    t = title()
    head = "* "
    if todo: head += todo + " "
    if prio: head += f"[#{prio}] "
    head += t
    if tags: head += "  :" + ":".join(tags) + ":"
    lines = [head]
    kind = R.random()
    rep = R.choice(REPEATERS)
    if kind < 0.30:      # scheduled, spread evenly across +-2 months
        lines.append(f"  SCHEDULED: {stamp(R.randint(-SPREAD, SPREAD))[:-1]}{rep}>")
    elif kind < 0.50:    # deadline, spread evenly across +-2 months
        lines.append(f"  DEADLINE: {stamp(R.randint(-SPREAD, SPREAD))[:-1]}{rep}>")
    elif kind < 0.58:    # both (scheduled earlier, deadline a bit later)
        s = R.randint(-SPREAD, SPREAD - 20)
        lines.append(f"  SCHEDULED: {stamp(s)}")
        lines.append(f"  DEADLINE: {stamp(s + R.randint(3, 20))}")
    # CREATED is inherently in the past; spread it over the past two months.
    created = R.randint(-SPREAD, -1)
    lines += ["  :PROPERTIES:", f"  :CREATED:  {stamp(created, active=False)}"]
    if todo == "DONE":
        lines.append(f"  :CLOSED:   {stamp(R.randint(created, 0), active=False)}")
    lines += ["  :END:"]
    if R.random() < 0.3:
        lines.append("  " + title(R.randint(6, 14)) + ".")
    return "\n".join(lines)

def file_header(title_):
    return f"#+title: {title_}\n#+filetags: :{R.choice(TAGS)}:{R.choice(TAGS)}:\n\n"

os.makedirs(TARGET, exist_ok=True)
for f in os.listdir(TARGET):
    if f.endswith(".org"):
        os.remove(os.path.join(TARGET, f))

total = 0
# 1) the canonical named files (the inbox + a few plain ones)
named = ["inbox", "work", "home", "health", "projects", "learning", "reading", "finance"]
for name in named:
    n = R.randint(8, 18)
    body = file_header(name.capitalize()) + "\n\n".join(entry() for _ in range(n)) + "\n"
    open(os.path.join(TARGET, f"{name}.org"), "w").write(body)
    total += n

# 2) dozens of Denote-style long-name files
for i in range(42):
    h = R.randint(8, 18); mi = R.randint(0, 59); s = R.randint(0, 59)
    day = TODAY + datetime.timedelta(days=-R.randint(1, 120))
    ident = day.strftime("%Y%m%d") + f"T{h:02d}{mi:02d}{s:02d}"
    t = title(R.randint(6, 12))
    kw = "_".join(sorted(set(R.choice(TAGS) for _ in range(R.randint(1, 3)))))
    fn = f"{ident}--{slug(t)}__{kw}.org"
    n = R.randint(6, 16)
    body = file_header(t) + "\n\n".join(entry() for _ in range(n)) + "\n"
    open(os.path.join(TARGET, fn), "w").write(body)
    total += n

print(f"Wrote {total} entries across {len(os.listdir(TARGET))} files to {TARGET} (today={TODAY})")
