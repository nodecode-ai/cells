# nodecode-field-notes

Field Notes: pitfalls, local patches and observations kept as notes, with a read-only check that each patch survived the last update. A Nodecode rewrite of [itpartypattaya/hermes-field-notes](https://github.com/itpartypattaya/hermes-field-notes) by [itpartypattaya](https://github.com/itpartypattaya).

The model gets four tools and `/fieldnotes`. `fieldnote_search` reads the
notes by the error text or the symptom before it starts debugging.
`fieldnote_new` records what a diagnosis taught: the root cause, the fix, how
to verify it, what misled the first attempt. `fieldnote_check` reads the signs
a local patch adds to a code tree and says, patch by patch, OK, MISSING,
UPSTREAMED or UNKNOWN, without editing the tree or running a patch script.
`fieldnote_doctor` is the one verdict line over all of it. Nothing leaves the
machine and there is no key.

A note is one Markdown file with a flat frontmatter block — title, date, type,
area, status, one dense summary — and its body below. A patch note also names
the files and signs that show the patch is in place, the old lines it replaced
and upstream's fix, so after an update the answer is evidence, not a guess.

## Get it

Tell the agent "add the field-notes cell", or run `nodecode add nc://field-notes`.

## Set it up

One required setting, one optional, in the `field_notes` section of
`config.jsonc` (or in `/settings`):

```jsonc
{
  "field-notes": {
    "store_dir": "~/.nodecode/field-notes/",  // where the notes are kept
    "core_root": "~/src/thing/",              // the tree patch checks read (optional)
    "ai": true                                 // let the model use the notes
  }
}
```

`core_root` may be empty: notes are then recorded and searched, and a patch
check says its code tree is not configured. The checks compare against the
`VERSION` file at the root of that tree; with no version, an edition with no
version bounds is used, else the check is UNKNOWN.

`/fieldnotes ai off` stops the four tools answering and leaves `/fieldnotes`
working; `ai on` turns them back on.

## Use it

- "Let's write this bug down" — the model calls `fieldnote_new` with the
title, the one-line summary and the body.
- A gateway error, a cron job that did not fire, "it broke after the update" —
the model calls `fieldnote_search` first, then `fieldnote_check`.
- After an update: `fieldnote_doctor`; every patch that is not OK is walked
with you — MISSING is re-applied only with your yes, UPSTREAMED becomes
`fixed-upstream`, UNKNOWN is read.
- `/fieldnotes` shows the store and its verdict; `/fieldnotes cron` searches;
`/fieldnotes ai off` switches the model's access.

### The checks file

A patch note is registered with a checks object:

```json
{"schema_version": 1,
 "editions": [
   {"when": {"min_version": "0.21.3"},
    "targets": [{"file": "path/in/the/tree.py",
                 "contains": ["MY_PATCH_MARKER", "new_line_of_code()"],
                 "not_contains": ["old_line_of_code()"]}],
    "upstream": [{"file": "path/in/the/tree.py", "contains": ["def upstream_fix("]}]}]}
```

Paths are relative to `core_root`; `..` and absolute paths are refused. The
first edition whose bounds hold is used; nothing in the file is executed.
`OK` means the signs are present, not that the patch works — keep the
behavioural check in the note.

## Files

The store the original used is the store this uses: `notes/*.md` with the same
frontmatter, `notes/<id>.checks.json`, and a generated `INDEX.md` at the root.
