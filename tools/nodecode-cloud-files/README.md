# nodecode-cloud-files

Cloud Files: a file manager over the folders you expose, as five tools. A Nodecode rewrite of [electricsheephq/hermes-cloud-file-manager](https://github.com/electricsheephq/hermes-cloud-file-manager) by [electricsheephq](https://github.com/electricsheephq).

Name a few folders once, and the model can list them, find a file by name, read it, save it back safely and make folders -- inside those folders and nowhere else. A path that climbs out with `..`, a symbolic link pointing away, an absolute path, or Nodecode's own home folder is refused before anything is touched.

| Tool | What it does |
| --- | --- |
| `files_list` | One folder: entries with name, size, mtime, whether it is a folder, whether it can be saved. With no root named, the exposed folders themselves. |
| `files_search` | Files and folders by name below a folder: a substring, or a glob with `*` and `?`. |
| `files_read` | A file as text, with the sha256 of what was read. |
| `files_save` | Save a `.md`, `.markdown` or `.txt` file atomically -- and refuse to overwrite a change made since it was read. |
| `files_mkdir` | Create a folder, and whatever is missing above it. |

Ask "what is in my notes folder?", "find the README under projects", "read docs/plan.md and save it with the meeting notes appended" -- the model browses with the tools and does not need the paths up front.

## Get it

Tell the agent "add the cloud-files cell", or run `nodecode add nc://cloud-files`.

## Set it up

One setting, and it is the point of the cell: which folders it may see. Absolute paths, in the `cloud-files` section of `config.jsonc` (or `/settings`):

```jsonc
"cloud-files": {
  "roots": ["/srv/notes", "/srv/projects"],   // absolute folders only
  "max_file_mb": 100                            // the largest file files_read answers
}
```

A folder that is not there, or is not absolute, refuses at start with a line naming it, and `(restart-cells)` tries again after the fix. There is no key and no account: the cell talks to the filesystem and nothing else.

## Use it

The model calls the tools when a question is about your files. A person can call the same functions from eval, in the `cloud-files` package:

```lisp
(cloud-files:list-files (nlk:make-json-object "root" "r0" "path" "docs"))
```

`(help :cloud-files)` is the model's manual for the five verbs, kept while the cell runs. `/files` lists the exposed folders, and `/files r0 docs` lists one folder of one of them.

### What the guards are

The original plugin's guards, kept:

- **A path is relative and cannot climb.** `..`, an absolute path, a leading `~` and a NUL are refused; every segment is resolved in turn, and a segment that exists must resolve inside the root, so a symbolic link pointing out is caught.
- **Writes do not go through links.** The file being saved must not be a symbolic link, and Nodecode's own home folder (config, database, cells) is out of reach unless a root sits inside it.
- **No blind overwrite.** `files_read` answers the sha256 of the bytes read; `files_save` takes it as `base_sha256` and refuses when the file changed in between, answering the file as it is now so the caller can merge. A new file needs no base sha256; an existing one without it is refused.
- **The write is atomic.** The text goes to a staging name in the same folder and is renamed over the target, so a reader never sees half a file.
- **Saving is Markdown and text only**, up to 1 MiB, as the original's editor was.

For defense in depth, expose only the folders you mean it to have: these guards bound the tools, not the account Nodecode runs as.

### What the rewrite leaves out

The original is a Hermes Desktop tab as much as an API: its browser, drag and drop, chunked upload of whole folders with resume, image and PDF preview, chat drop target, folder notes and Google Drive import are all left out -- a cell has a tool call and a shell rather than a window. The chunked upload protocol and the 24-hour sweep of its staging names go with them; this cell writes whole files. `NOTICE` has the detail.

## License

MIT. The folder model, the path guard and the save protocol are electricsheephq's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
