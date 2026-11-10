# nodecode-mdvault

Markdown Vault: a folder of Markdown notes as the agent's long-term memory. A Nodecode rewrite of [kailashrajh/hermes-mdvault](https://github.com/kailashrajh/hermes-mdvault) by [kailashrajh](https://github.com/kailashrajh).

The vault *is* the memory. Point the cell at any folder of Markdown notes -- an
Obsidian vault, a notes repo, a diary, a docs tree -- and it becomes what the
agent remembers: searchable, editable, portable, and readable by a person
without Nodecode. Nothing is embedded and no service runs; the index is built in
the Nodecode process from the files the operator already owns.

Four tools, one command and a recall before each turn:

- `mdvault_search` finds the notes that answer a question: the path, the title,
  the date, the tags and the passage around the match.
- `mdvault_note` reads one note in full.
- `mdvault_recent` answers "what was I working on".
- `mdvault_remember` writes a new note into the write folder; it never
  overwrites one, a taken name gets a numeric suffix.
- `/mdvault` runs the same by hand.

Ranking is bm25 over the note's words, with title, heading and tag hits
weighted up, a whole-query phrase bonus and a light recency tiebreak, so search
by the words the notes actually use. CJK runs are indexed as character bigrams,
so a Japanese or Chinese note ranks without a segmenter.

## Get it

Tell the agent "add the mdvault cell", or run:

```
nodecode add nc://mdvault
```

## Set it up

One setting is required: `vault_path`, the folder the notes live in. Set it in
`/settings`, on the page's Settings tab, or by asking the agent ("point mdvault
at ~/notes"). There is no key; nothing leaves the machine.

The rest have defaults worth knowing:

| Setting | Default | What it does |
| --- | --- | --- |
| `write_folder` | `inbox` | vault-relative folder new notes are written into |
| `auto_recall` | true | recall the notes matching a turn's input before the turn |
| `recall_limit` | 4 | notes recalled per turn; 0 disables recall |
| `recall_chars` | 1800 | the character budget of the recalled block |
| `search_limit` | 6 | notes one search answers by default |
| `max_notes` | 5000 | stop indexing after this many; 0 means no cap |
| `max_file_kb` | 1024 | a note bigger than this is not indexed |
| `exclude_dirs` | `.obsidian .git node_modules .trash .venv __pycache__` | folders never walked, besides any starting with a dot |
| `refresh_seconds` | 2 | how stale the index may be before the folder is swept again |

A vault folder that is not there is a refusal that names the setting, not a
silent empty memory.

## Use it

Ask for something the operator wrote down -- "what did we decide about the
release date", "what am I working on" -- and the agent searches the vault, or
reads the note a search answered. Say "remember this" and it writes a note
titled like a filing label into the write folder.

`/mdvault` alone reports the vault, its note count and the recall settings;
`/mdvault WORDS` searches; `/mdvault recent` lists what changed lately;
`/mdvault note PATH` reads one note; `/mdvault reindex` rebuilds the index.

What the vault never does: read a file that is a symbolic link out of the vault
(a link may point at anything on disk), walk a folder whose name is excluded or
starts with a dot, write outside the vault, or overwrite a note.

## What it is not

The original also mirrored the harness's own memory writes into a vault note.
Nodecode keeps its durable memory elsewhere and has no single write event to
fold, so here the notes are written by the tools above; see NOTICE.
