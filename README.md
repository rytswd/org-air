# org-air

> A modern, aesthetic GTD dashboard **and** Air project viewer for Emacs — built on [org-ql](https://github.com/alphapapa/org-ql), with no `org-agenda`.

<p align="center">
  <img alt="Emacs 29.1+" src="https://img.shields.io/badge/Emacs-29.1%2B-7F5AB6">
  <img alt="Built on org-ql" src="https://img.shields.io/badge/backend-org--ql-4E9A06">
  <img alt="License GPL-3.0-or-later" src="https://img.shields.io/badge/license-GPL--3.0--or--later-blue">
  <img alt="Version 0.1.0" src="https://img.shields.io/badge/version-0.1.0-lightgrey">
</p>

org-air answers one question well — *what deserves my attention right now?* —
and gets out of the way. It reads your Org files through `org-ql`, classifies
each heading into a handful of meaningful buckets, and renders them with a
quiet, legible visual language inspired by [N Λ N O](https://github.com/rougier/nano-emacs)
and mu4e.

---

## 🌄 Overview

org-air is **two surfaces over your Org files**, sharing one visual language,
one keymap core, and one context rail:

| Surface | Command | What it is |
| --- | --- | --- |
| **GTD dashboard** | `M-x org-air` | A read-first board: capture → glance → process. Items grouped into Inbox / Needs attention / Upcoming / High priority / Stale, with a live calendar, filter, sort, and inbox workflow. |
| **Air project viewer** | `M-x org-air-project` | A planning-doc tree for repositories managed with [Air](https://github.com/rytswd/air) — state-grouped documents (`airctl -Da` parity), a side-rail inspector, and an in-place doc session. |

The philosophy:

- **Org files are the source of truth.** org-air never owns your data; it is a
  view. There are no agenda buffers, no custom-command DSL, no global state.
- **A replacement, not a reskin.** It is an independent view built on `org-ql`,
  so it carries none of `org-agenda`'s configuration surface or buffer machinery.
- **Planning-first.** Capture is one keystroke into a single inbox; processing
  (refile, schedule, tag) is a deliberate, separate step.
- **Quiet by design.** Following nano's "1 + 6" model, colour is used to *mean*
  something, sparingly. Overdue looks overdue; everything else stays calm.

---

## 📸 Screenshots

<!-- TODO: add screenshots. No image assets ship in this repository yet.
     Drop board / project / rail captures under a docs/ directory and
     reference them here (e.g. ![dashboard](docs/dashboard.png)). -->

*Screenshots coming soon — the dashboard, the project viewer, and the context
rail.*

---

## ✨ Features

**Dashboard (`org-air`)**

- Five semantic buckets: **Inbox** (`:inbox:` capture queue), **Needs
  attention** (overdue, or active with no schedule/deadline), **Upcoming**
  (within `org-air-upcoming-days`), **High priority** (`[#A]` etc.), **Stale**
  (untouched for `org-air-stale-days`).
- A month **calendar** marking scheduled/deadline/created days, with today
  highlighted — navigate months without leaving the board.
- **Filtering** by tag *and* free text: a bare token substring-matches the
  title/origin, a `#tag` token matches tags; multi-term combines per
  `org-air-filter-match` (AND / OR, toggled live).
- **Scope** the whole dataset to one tag, group, or file.
- **Sort** within each bucket (date / priority / title / recency).
- One-keystroke **capture** to the inbox, a guided **refile / process** flow,
  and an **inbox walk** that dispositions items one at a time.
- Toggleable **columns** — filename (origin), date/schedule, tags — that reflow
  and stay aligned.

**Project viewer (`org-air-project`)**

- `airctl -Da` **parity tree**: documents grouped by state
  (draft → ready → work-in-progress → complete → dropped), directory, or tag.
- A side-rail **inspector** showing the doc's state, tags, path, and metadata.
- An in-place **doc session**: open a planning doc in the same window, edit it
  live, and jump back to the tree — with a rail **outline** that follows point.

**Everywhere**

- A **context rail** (calendar / summary / filters / inspector / outline) that
  pops in inline or out into a native side window.
- **`org-air-outline-mode`** — the outline rail + current-heading highlight as a
  standalone opt-in minor mode for **any** Org buffer (no Air dependency).
- **evil-awareness**: single-key bindings resolve in motion state; a `C-c`
  leader is state-agnostic. Soft dependency — nothing required.
- **dimmer soft-integration**: org-air's own buffers auto-register as excluded.
- **Cache-first** load and chunked background refresh for large file sets.

---

## 🧰 Requirements

| Dependency | Version | Required? |
| --- | --- | --- |
| Emacs | 29.1+ | ✅ yes |
| Org | 9.6+ | ✅ yes |
| [org-ql](https://github.com/alphapapa/org-ql) | 0.8+ | ✅ yes |
| [evil](https://github.com/emacs-evil/evil) | any | ⚪ optional (auto-integrates) |
| [dimmer](https://github.com/gonewest818/dimmer.el) | any | ⚪ optional (auto-integrates) |

`evil` and `dimmer` are **soft** dependencies: if present, org-air integrates
with them automatically; if absent, nothing changes and nothing is required.

---

## 🚀 Installation

`org-ql` is on MELPA. Put org-air's `.el` files on your `load-path`, then use the
`use-package` block below (or a plain `require`).

### `use-package` (recommended)

```elisp
(use-package org-air
  ;; org-ql (+ Org) is the only hard dependency.
  :commands (org-air org-air-project org-air-capture org-air-outline-mode)
  :init
  ;; Recommended global entry points.
  (global-set-key (kbd "C-c a") #'org-air)          ; the GTD dashboard
  (global-set-key (kbd "C-c A") #'org-air-project)  ; the Air project viewer
  (global-set-key (kbd "C-c c") #'org-air-capture)  ; capture to the inbox
  :custom
  ;; Where to look — files and/or directories (scanned recursively for .org).
  (org-air-files '("~/org/"))
  ;; Where `c' / M-x org-air-capture drops new inbox items.
  (org-air-inbox-file "~/org/inbox.org")
  ;; Classification horizons.
  (org-air-upcoming-days 7)
  (org-air-stale-days 21)
  ;; Board columns (defaults shown): filename OFF, dates + tags ON.
  (org-air-show-origin nil)
  (org-air-show-dates t)
  (org-air-show-tags t)
  ;; Multi-tag filter combinator: `all' (AND) or `any' (OR).
  (org-air-filter-match 'all)
  ;; Keep your own mode-line (`default'), or use the calm nano-style one (`calm').
  (org-air-modeline-style 'default)
  ;; Main-window action leader (rail toggle / outline jump / sort / filter).
  (org-air-leader-key "C-c C-a"))

;; Opt-in: the standalone outline rail in ANY Org buffer.
;;   M-x org-air-outline-mode
;; evil and dimmer integrate automatically — no configuration needed.
```

### Plain `require`

```elisp
(require 'org-ql)                       ; from MELPA
(require 'org-air)                      ; after adding the .el files to load-path
(setq org-air-files '("~/org/"))        ; files and/or directories to scan
```

`org-air-files` accepts any mix of files and directories; directories are
scanned recursively for `.org` (and `.org.gpg`) files.

---

## ⚙️ Configuration

org-air is useful out of the box; everything here is optional. These are the
knobs most users actually tune — run `M-x customize-group RET org-air RET` for
the full ~60.

| Variable | Default | Meaning |
| --- | --- | --- |
| `org-air-files` | `nil` | Org files/directories to scan. |
| `org-air-inbox-file` | `~/.emacs.d/org-air-inbox.org` | Where `c` / `org-air-capture` stores items. |
| `org-air-upcoming-days` | `7` | Horizon (days) for the Upcoming bucket. |
| `org-air-stale-days` | `21` | Quiet period before an item is Stale. |
| `org-air-show-origin` | `nil` | Show the board filename (origin) column. |
| `org-air-show-dates` | `t` | Show the board date/schedule column. |
| `org-air-show-tags` | `t` | Show the board tags column. |
| `org-air-filter-match` | `all` | Multi-tag filter combinator: `all` (AND) or `any` (OR). |
| `org-air-priority-show` | `(?A ?B ?C ?D ?E)` | Priority cookies rendered on rows. |
| `org-air-sort-key` | `date` | Default within-bucket sort: `date`/`priority`/`title`/`recency`. |
| `org-air-visit-display` | `other-window` | How `S-RET` shows a source heading: `other-window`/`same`/`side`/`frame`. |
| `org-air-modeline-style` | `default` | `default` (keep yours) or `calm` (nano-style). |
| `org-air-tag-style` | `pill` | Tag chips: `pill` (svg) or `text`. |
| `org-air-priority-style` | `square` | Priority cookie: `square`/`badge`/`text`. |
| `org-air-leader-key` | `"C-c C-a"` | Main-window action leader prefix. |
| `org-air-inspector-max-title-lines` | `nil` | Cap on wrapped inspector title lines (`nil` = wrap fully). |
| **Project** | | |
| `org-air-sources` | `nil` | Where the project viewer finds Air content. |
| `org-air-project-group` | `directory` | Default grouping: `state`/`directory`/`tag`. |
| `org-air-project-state-style` | `svg` | State badge: `svg`/`nerd`/`text`/`emoji`/`badge`. |
| **Rail** | | |
| `org-air-rail-placement` | `((board . inline) (project . side-window))` | Default rail placement per view. |
| `org-air-outline-rail-placement` | `side-window` | Where `org-air-outline-mode` hosts its rail. |
| **Faces** | | |
| `org-air-tag-color` | `nil` | Give tag chips per-tag accent hues. |
| `org-air-faces-prefer-nano` | `nil` | Link org-air faces to nano-theme's, when available. |

All colour lives in `org-air-faces.el` — one default face plus six semantic
roles (faded, subtle, salient, strong, popout, critical), each with explicit
light *and* dark specs and a TTY fallback. Retint everything by editing
`org-air-palette`.

### Right-edge glyphs in a fringe-less GUI

org-air sizes every line to the window's *usable* columns, so with a zero
right fringe nothing overflows the visible edge. A few header/legend glyphs
(the `·` separator, `…`, `✕`, and the `↑ ↓ → ↻` arrows) are Unicode
*East-Asian Ambiguous-width* characters: Emacs measures them as one column
(so the layout is exact), but some fonts *paint* them two columns wide. If
you run a fringe-less GUI and see a stray truncation glyph at the extreme
right edge, that is a font/`char-width-table` mismatch, not an org-air
layout bug. The fix is a config one: either pick a font that renders those
glyphs single-width, or tell Emacs to treat the ambiguous set as one column
(e.g. keep the default `east-asian-ambiguous`/`char-width-table` mapping,
or `(set-language-environment "UTF-8")`). org-air's own width math already
compensates for whatever `char-width-table` reports — it is purely the
painted advance that can differ.

---

## ⌨️ Keybindings

### Dashboard (`org-air`)

| Key | Action |
| --- | --- |
| `n` / `p` | Next / previous item |
| `j` / `k` | Line down / up (lands on the title) |
| `RET` | Open the item in the bottom view pane |
| `S-RET` / `O` | Visit the source heading in the other window |
| `SPC` | Peek at the item |
| `v` / `V` | Open / close the bottom view pane |
| `TAB` / `S-TAB` | Toggle / previous section |
| `M-n` / `M-p` / `M-TAB` | Forward / back / next section |
| `c` | Capture to the inbox |
| `r` | Refile / process the item |
| `I` | Walk the Inbox one item at a time |
| `m` | Toggle mark |
| `t` / `T` | Set tag / cycle TODO keyword |
| `d` / `D` | Set deadline / mark done |
| `a` / `x` / `u` | Archive / kill (guarded) / undo triage |
| `/` | Filter by tag or free text |
| `\` | Clear the filter |
| `M-/` | Toggle filter combinator (AND ↔ OR) |
| `s` / `S` | Scope / clear scope |
| `o` / `O` | Sort: cycle key / reverse direction |
| `z f` / `z d` / `z t` | Toggle the filename / dates / tags column |
| `\|` | Pop the context rail in / out of a side window |
| `g` | `g`-prefix: `g r` refresh, `g g` top, `g R` refresh-all, `g RET` visit |
| `G` | Go to the bottom |
| `<` / `>` / `.` | Calendar: previous / next month / today |
| `P` | Open the Air project viewer |
| `?` | Help |
| `q` | Quit (progressively closes one surface per press) |
| `C-c C-a` | Leader: `\|` rail · `o` outline jump · `s` sort · `/` filter |

### Project viewer (`org-air-project`)

| Key | Action |
| --- | --- |
| `n` / `p` / `j` / `k` | Motion between doc rows |
| `RET` | Open the doc in a same-window session |
| `S-RET` / `O` | Visit the doc in the other window |
| `s` / `d` / `t` | Group by state / directory / tag |
| `(` | Flip rows between title and relative path |
| `/` | Filter docs by tag |
| `o` / `O` | Sort: cycle (name/created/updated) / reverse |
| `v` / `V` | Bottom peek pane open / close |
| `\|` | Pop the context rail in / out |
| `g` | Refresh |
| `?` / `q` | Help / quit |
| `C-c C-a` | Leader: `\|` rail · `o` outline jump · `s` sort · `/` filter |

### Doc session (an open planning doc — editable Org buffer)

| Key | Action |
| --- | --- |
| `C-c C-q` | Back to the tree |
| `C-c C-a \|` | Toggle the rail |
| `C-c C-a o` | Jump to the current heading |
| `C-c C-a n` / `C-c C-a p` | Next / previous heading |

Because the doc buffer is fully editable (single keys self-insert), the
main-window actions live under the `C-c C-a` leader. Legends show the
context-correct key — the bare key where it works, the leader form where it
doesn't.

---

## 🗂️ The two views

### The GTD dashboard — `M-x org-air`

A sticky header shows the date, a visible-item count, and any active filter or
scope. Below it, items sit in five sections, each with an icon and a count
badge (badges turn attention-coloured when a section demands it). Each row reads
left to right: TODO state, priority cookie, title, a semantic date label, tag
chips, and (optionally) an origin breadcrumb.

The capture → process loop is two steps:

1. `c` captures a titled note straight to `org-air-inbox-file`, tagged
   `:inbox:`. It appears in the Inbox on the next refresh.
2. `r` on an inbox item opens one completion prompt offering `@group` (move),
   `>today`/`>tomorrow`/`>week`/`>someday` (schedule), `#tag`, and `⌂file` — one
   choice does the move and refreshes the board.

`I` walks the whole Inbox one item at a time with single-key dispositions.

### The Air project viewer — `M-x org-air-project`

For repositories planned with [Air](https://github.com/rytswd/air), the project
viewer renders the planning docs as a state-grouped tree (parity with
`airctl -Da`), sortable and filterable, with a side-rail inspector. `RET` opens
a doc in a same-window **session**: you edit the real file, the rail shows a live
outline that follows point, and `C-c C-q` returns you to the tree. org-air the
package has no runtime dependency on Air or `airctl`.

---

## 🧵 `org-air-outline-mode`

The doc-session outline rail — a heading list plus a current-heading highlight
that follows point — is also a **standalone, opt-in minor mode** usable in *any*
Org buffer:

```elisp
M-x org-air-outline-mode
```

Enabling it in an `org-mode` buffer pops the org-air context rail showing that
buffer's headings (via the same rail machinery the doc session uses) and follows
point with the current-heading highlight. It has **no dependency** on
`org-air-project`, org-ql, or airctl — a light, generic scaffold. It is off by
default and a no-op outside `org-mode`. Choose inline vs. side-window placement
with `org-air-outline-rail-placement`.

---

## 📄 License

**GPL-3.0-or-later** (`SPDX-License-Identifier: GPL-3.0-or-later`), per the
headers in the source files. org-air is **not** part of GNU Emacs.

<!-- TODO: no top-level LICENSE file ships in this repository yet; the license
     is declared only in the .el file headers. Consider adding a LICENSE file
     with the full GPL-3.0 text before publishing. -->
