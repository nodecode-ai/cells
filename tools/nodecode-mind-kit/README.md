# nodecode-mind-kit

Biomimetic Brain: 25 subsystem state files, one folded brain-state.json and a read-only PROCEED / CAUTION / VETO gate. A Nodecode rewrite of [ousiaresearch/biomimetic-brain](https://github.com/ousiaresearch/biomimetic-brain) by [ousiaresearch](https://github.com/ousiaresearch).

Most agents have no idea how they are: the same answer at 3am as at 10am, after ten hours of work or none. This cell gives one a small set of readings that rise and fall — energy, fatigue, circadian phase, motivation, novelty, attention, conflict, open questions — folds them into one file the rest of the agent reads, and gates a decision on them. It decides nothing by itself: it reads and writes files, and says `stale` or `missing` rather than guessing.

## Get it

Tell the agent "add the mind-kit cell", or run `nodecode add nc://mind-kit`.

## Set it up

One setting, in `/settings` or the settings page:

- `agent_dir` — the directory this agent's own state lives in. The cell reads `<agent_dir>/brain/<subsystem>/<name>.json` and writes `<agent_dir>/brain-state.json`.
- `mode` (optional, default `standard`) — the gate's default rule set: `standard`, `low_scope`, `minimal` or `essential`. Only `essential` changes a verdict: it bypasses quiet hours.

No key, no network, no model call. Then seed it once, from the conversation or from eval:

```lisp
(mind-kit:seed)    ; copies the 25 shipped example files in, leaving any file already there
```

The shipped files carry structure, not truth. Filling them with what is actually true for this agent is the install, and no pass can do it.

## Use it

**Read the aggregate.**

```lisp
(mind-kit:state)    ; folds every subsystem file, writes brain-state.json, answers the health summary
(mind-kit:health)   ; the same, one line: "2/25 healthy, 0 stale, 23 missing"
```

Every reading carries its age and the age it is allowed to reach, so a subsystem that stopped updating reads `stale` instead of asserting an old number — and one whose file is gone reads `missing`, never `stale`. The aggregate also names what is critical: fatigue that needs recovery, a fear reading above 0.7, conflict, an urge above 0.8, prediction accuracy under 0.3.

**Gate a decision.**

```lisp
(mind-kit:gate)                              ; PROCEED / CAUTION / VETO with the numbers behind it
(mind-kit:gate :now "23:30")                 ; pin the clock to assert the quiet-hours rules
(mind-kit:gate :mode "essential")            ; bypass quiet hours
```

It answers a plist: `:verdict`, `:reason` (one of the 26 documented reason codes), `:exit-code` (0 PROCEED, 2 CAUTION, 1 VETO), `:detail` (the inputs that caused it), `:read` and `:defaulted` (which files it actually read), and `:warnings`. It is read-only: gating never mutates the state that produced it, and every value it had to default because a file was absent is reported as a default.

The model gets the same two things as tools, `brain_state` and `decision_gate`, and the operator gets `/mind`. `(help :mind-kit)` carries the same guidance to the model.

The original's four maintenance passes are shell and Python scripts on a schedule; here the agent's own passes write the subsystem files and the cell only reads them. Seeding, folding and gating are the whole cell.

## Tests

From the cells repository:

```sh
.github/test-cell.sh tools/nodecode-mind-kit
```

Offline: a scratch agent directory per test, no provider, no network, the clock pinned where quiet hours are asserted.

MIT. See [NOTICE](NOTICE) and [LICENSE](LICENSE).
