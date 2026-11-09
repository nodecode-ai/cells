# nodecode-birkin

Birkin Mnemosyne: local Markdown memory with multilingual lexical recall. A Nodecode rewrite of [ashmoonori-afk/birkin-mnemosyne](https://github.com/ashmoonori-afk/birkin-mnemosyne) by [ashmoonori-afk](https://github.com/ashmoonori-afk).

Notes are ordinary Markdown files under this home. There is no service, no
API key and no telemetry: the model writes a note, searches it by the words it
shares with the vault, reads one back, and archives one it no longer needs.
Nothing is deleted — archiving moves a note under `_archive` and restoring
moves it back, previewed first.

## Get it

Tell the agent "add the birkin cell", or run `nodecode add nc://birkin`.

## Set it up

Nothing is required. `/birkin` shows the vault it is on. In `/settings`, or
the `birkin` section of `config.jsonc`:

```jsonc
{
  "birkin": {
    "vault": null,          // a folder; a birkin folder under this home unless answered
    "max_results": 8,       // how many notes one search or the digest returns
    "prefetch": true        // put the newest notes on each turn's first request
  }
}
```

An existing Hermes vault can be pointed at with `vault`: the notes are the same
Markdown with a `title:` frontmatter line, so a vault written by the original
plugin is read as it stands.

## Use it

Five tools, all exact:

- `birkin_remember` — `title` and `body`; an existing title is refused, never
  overwritten.
- `birkin_search` — `query`, optional `limit` (1 to 20) and `labels`
  (`namespace:value`, matched with AND). Lexical, in any script: a Korean or
  Chinese note is found by the words a query shares with it.
- `birkin_get_note` — `title`, the whole body.
- `birkin_forget` — `note`, an optional `reason`, and `confirm`; preview by
  default, and only `confirm=true` moves the file under `_archive`.
- `birkin_restore` — `note`, optional `zone` (default `inbox`), `confirm`;
  brings an archived note back unchanged.

`/birkin` with a query searches the vault, without one it shows where the vault
is, how many notes it holds and how many are archived. With `prefetch` on, the
newest notes ride the first request of each turn as a digest, framed as data,
not instructions. The manual is `(help :birkin)`.
