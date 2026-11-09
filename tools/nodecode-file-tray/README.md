# nodecode-file-tray

File Tray: the paths this session read or wrote, in one list. A Nodecode rewrite of [aydnOktay/hermes-file-tray](https://github.com/aydnOktay/hermes-file-tray) by [aydnOktay](https://github.com/aydnOktay).

## Get it

Tell the agent "add the file-tray cell", or run:

```sh
nodecode add nc://file-tray
```

Then restart, or run `(restart-cells "nodecode-file-tray")` from eval. Nothing is
required in the config: the cell runs on its defaults the moment its folder is
there.

## Use it

Ask for a file to be read or edited, then ask what was touched.

- `file_tray_list` is the tool the model calls when the operator asks which
  files were opened or edited. It answers JSON — `ok`, `count`, `items` (each
  with `path`, `name`, `action`, `at`), and a `markdown` bullet list unless the
  call said `"markdown": false`.
- `file_tray_clear` empties the list.
- `/files` and `/files list` show the same list to the operator; typed alone
  `/files` also opens a panel whose one row clears it, and `/files clear`
  empties it from the command line.
- `(help :files)` is the model's primer for both.

A path appears once. Touching it again updates its action and time in place, as
the original did, and past `max_items` the oldest leave. Paths that are URLs,
carry a line break, or run past 500 characters are ignored, and a path through
`\` is written with `/`.

## Where the list comes from

Hermes watched a `post_tool_call` for the tool names `read_file`,
`write_file` and `patch` and kept the paths it saw in one `state.json` for the
whole install. Nodecode's file verbs are Lisp functions the eval tool calls,
not tools of their own, so the same record is kept by two seams instead:

- the file verbs are advised — `nle:edit` records `edit`, `nle:write-file`
  records `write`, `nle:look` records `read` — so a path joins the list after
  the verb that touched it landed;
- the `:tool` point catches any tool call whose arguments name a path —
  `path`, `file`, `filepath`, `file_path` or a `paths` array, the same
  argument scan the original ran — so an MCP server's `read_file` is recorded
  without this cell knowing its server.

The list is per session, in that session's state under the key
`file-tray.items`, so two sessions never mix and a restart keeps each. This
cell reads no user file and writes none: a path on the list is a string a verb
left there.

## Config

```jsonc
"file-tray": { "max_items": 40 }
```

`max_items` is the most paths one session's list keeps (40 by default, the
original's). `config.example.jsonc` is the same section, commented. The cell
refuses a value that is not a whole number of 1 or more.

## Tests

Run `.github/test-cell.sh tools/nodecode-file-tray` from the cells repository,
or `just file-tray-test` from Nodecode.
