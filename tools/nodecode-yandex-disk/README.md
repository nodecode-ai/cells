# nodecode-yandex-disk

Yandex Disk: browse, read, write, upload, download, share, delete and restore files. A Nodecode rewrite of [akinfold/hermes-yandex-disk](https://github.com/akinfold/hermes-yandex-disk) by [akinfold](https://github.com/akinfold).

The original gave a Hermes agent fifteen tools over the Yandex Disk REST API
v1. This cell gives a Nodecode agent the same fifteen, over the same
endpoints, with the same path rules and the same action allow-list; and the
same calls are in the `YADISK` package for `eval`:

```lisp
(yadisk:list-path "/Documents")                    ; a folder, with every child's path
(yadisk:search-file "invoice" :limit 20)           ; hits by name or by contents
(yadisk:read-file "/Documents/notes.txt")           ; a text file into the conversation
(yadisk:write-file "/Documents/new.txt" "hi" :overwrite t)
(yadisk:upload "/Backup/dump.tar" :local-path "~/dump.tar")
(yadisk:delete "/Documents/old.pdf")                ; to the bin; :permanently t to destroy
```

`/yadisk PATH` is the operator's own listing, and `(help :yandex-disk)` is
the manual the agent reads.

## Get it

Tell the agent "add the yandex-disk cell", or run `nodecode add nc://yandex-disk`.

## Set it up

Make an OAuth token for Yandex Disk at
<https://yandex.ru/dev/disk/poligon/> with the scopes `cloud_api:disk.info`,
`cloud_api:disk.read` and `cloud_api:disk.write`, then name it in the
`yandex-disk` section — either pasted into `token`, or in the environment
variable `token_env` names (the original's own `YANDEX_DISK_OAUTH_TOKEN`):

```jsonc
{
  "yandex-disk": {
    "token_env": "YANDEX_DISK_OAUTH_TOKEN",
    "root": "/Backup",          // optional: confine every path to one folder
    "actions": ["read", "write"] // optional: withhold the rest
  }
}
```

`root` makes every path the tools take and answer relative to that folder, and
refuses anything outside it — what the original's `YANDEX_DISK_ROOT` did.
`actions` takes `all`, `read`, `download`, `write`, `share`, `delete`, or
single names such as `list` or `upload`; a deployment that lists only `read`
never registers a tool that can change the disk.

## Use it

| Tool | What it does |
| --- | --- |
| `yadisk_disk_info` | the account's quota, bin size and login |
| `yadisk_list` | a folder's children, or one file's metadata, each with a path another tool takes back |
| `yadisk_search` | hits by file name or by contents, ranked by Yandex |
| `yadisk_read_file` | a text file into the conversation (`utf-8`, `ascii`, `latin-1`, `cp1251`, `koi8-r`) |
| `yadisk_download` | a file onto this machine, complete or not at all |
| `yadisk_upload` | a file from this machine, or a public URL Yandex fetches itself |
| `yadisk_write_file` | text to a file, the folders above it made first |
| `yadisk_mkdir` | a folder and its missing parents |
| `yadisk_copy`, `yadisk_move` | on Yandex's side, never through this machine |
| `yadisk_publish` | a public link, or its removal |
| `yadisk_delete` | to the bin, or `permanently` |
| `yadisk_trash_list`, `yadisk_trash_restore`, `yadisk_trash_empty` | the bin: what is in it, where it came from, and how to bring it back |

Two Yandex behaviours are handled here once rather than in every caller: a
bulk endpoint that defers its work behind an `operations/<id>` link is polled
to completion, and a transfer asks the API for a single-use URL on another
host and sends the bytes there without the token. A refusal is Yandex's own
sentence, in `ERROR: YANDEX-DISK-ERROR: ...`.

Some tokens are not permitted to use Yandex's search endpoint; the original
probed for that at load time and withheld the tool. Here `yadisk_search`
answers with the sentence instead, and every other tool still works.

The cell needs no dependency of its own beyond the image: everything goes
through `nlk:http`.
