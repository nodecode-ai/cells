# nodecode-scratchpad

Scratchpad: a per-session set of named draft buffers the model edits in place, removed with the session. A Nodecode rewrite of [Zek-Takai/hermes-devkit](https://github.com/Zek-Takai/hermes-devkit) by [Zek-Takai](https://github.com/Zek-Takai).

Drafts used to land as a new file per revision — `page.html`, `page_v2.html`,
`page_final.html`. The scratchpad gives each session one home for the drafts
you look at and discuss: code, HTML pages, notes. The model writes a buffer
once and then patches it, so a revision is a change in place and not another
file. A short rule rides every request asking it to put drafts here rather
than in a loose temp file. Buffers are removed with the session's folder, and
an idle folder is cleaned up after `idle_days`.

## Get it

Tell the agent "add the scratchpad cell", or run `nodecode add nc://scratchpad`.

## Use it

Ask for a draft — a snippet, a page, a note — and the model calls the
`scratchpad` tool: `action=write` with a name and the content, `action=patch`
with the exact text to replace, `action=read`, `action=list`, `action=delete`.
A patch whose `old_text` matches twice is refused rather than guessed, and the
model is told to add context or set `replace_all`. When the work becomes a
real project the model promotes the buffer: `action=promote` with an absolute
or `~` destination writes it as a normal file, refusing to replace one already
there unless `overwrite` is set.

From a shell, `/scratchpad` lists this session's buffers and carries a picker;
`/scratchpad read NAME` shows one, `/scratchpad delete NAME` removes one, and
`/scratchpad prune` removes idle folders now. From eval the same verbs are
`(scratchpad:write-buffer "main.py" "...")`, `patch-buffer`, `read-buffer`,
`list-buffers`, `delete-buffer`, `promote-buffer` and `(scratchpad:prune)`;
`(help :scratchpad)` is the page the model reads.

Nothing leaves the machine and there is no key. Two settings, both optional:

```jsonc
{
  "scratchpad": {
    "idle_days": 90,       // remove a session's buffers after this many idle days
    "max_buffer_kb": 2048  // how large one buffer may be, in kilobytes
  }
}
```

Buffers live under the Nodecode home, one folder per session, marked with the
session id; cleanup touches only folders that carry that marker and only ever
its own.

## Tests

Run `.github/test-cell.sh tools/nodecode-scratchpad` from the cells repository,
or `just scratchpad-test` from Nodecode. The tests cover the lifecycle, every
action through the tool seam, an ambiguous patch, promotion and its overwrite
refusal, a name that climbs out of the folder, a call with no session,
`/scratchpad`, the per-turn rule, and cleanup leaving a stranger's folder
alone.
