# nodecode-jot

Jot: notes, checklists and documents the model can read and update, with revision checks. A Nodecode rewrite of [Totoro-qaq/hermes-jot](https://github.com/Totoro-qaq/hermes-jot) by [Totoro-qaq](https://github.com/Totoro-qaq).

The model gets six tools — `jot_list`, `jot_read`, `jot_create`,
`jot_update`, `jot_set_task`, `jot_delete` — that find, read, write and
tick the notes in a library file on this computer, plus `/jot` to look at the
library, turn the model's access on and off, or search it. Nothing leaves the
machine, and there is no key.

Each edit quotes the revision the model just read. If you wrote in the
meantime the edit is refused, not applied, and the model is told to read
again. Edits change words in place: the paragraphs, tables and lists around
them stay as they were. A note the model moves out of the way waits in Trash,
where you can put it back.

## Get it

Tell the agent "add the jot cell", or run `nodecode add nc://jot`.

## Set it up

One setting: the folder the library file lives in.

```jsonc
{
  "jot": {
    "data_dir": "~/.nodecode/jot/",  // the folder; one library per folder
    "ai": true,                      // let the model use the notes
    "max_notes": 2000                // the ceiling, Trash included
  }
}
```

`data_dir` has no default on purpose: the notes are yours, kept where you say.
Without it the cell refuses at start and names the member. The section is in
`/settings` too.

| Member | Default | What it does |
| --- | --- | --- |
| `data_dir` | none (required) | the folder `library.json` is kept in |
| `ai` | `true` | whether the tools answer at all; `/jot ai on` and `/jot ai off` change it |
| `max_notes` | `2000` | how many notes the library holds, Trash included |

## Use it

Ask for a note and the model writes one; ask what your checklist says and it
reads the note and the items in it:

> Keep a release checklist with the steps to ship.

> Tick "write the release notes" on the release checklist.

The tools, as the model calls them:

| Tool | Argument | What it does |
| --- | --- | --- |
| `jot_list` | `query?`, `folderId?`, `limit?`, `offset?` | notes matching words in title or text, with excerpts and the folders |
| `jot_read` | `id` | one note as Markdown, its checklist items, its revision |
| `jot_create` | `title`, `text`, `folderId?`, `format?` | a new note; a folder name that is new makes the folder |
| `jot_update` | `id`, `revision`, `edits?` \| `text?` \| `appendText?`, `title?`, `folderId?` | change one note |
| `jot_set_task` | `id`, `revision`, `index`, `checked` | tick or untick one checklist item |
| `jot_delete` | `id`, `revision` | move a note to Trash |

From the keyboard, `/jot` shows the switch, how many notes there are and the
few you changed last; `/jot ai off` stops the model touching any of them;
`/jot tea` searches titles and text. The model cannot change the switch.

A note is its Markdown text, so `jot_read` answers exactly what is in the
file. Replacing a whole note with `text` is refused when the note carries
something Markdown cannot hold — an image or file placeholder, an underline
or a colored span — and the model is told to make targeted edits, append, or
ask you first, then retry with `allowFormattingLoss: true`.

The library is one `library.json` in `data_dir`, written atomically under
the folder's lock, so two Nodecode processes can share one folder. Nothing
else is written there.
