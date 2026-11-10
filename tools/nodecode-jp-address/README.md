# nodecode-jp-address

JP Address Splitter: splits one Japanese address into prefecture, city and town, with a coarse point and the licence's attribution. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-address](https://github.com/TakeshiTGAL/hermes-plugin-address) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

## Get it

Tell the agent "add the jp-address cell", or run `nodecode add nc://jp-address`.

## Set it up

Nothing to set up: the address data is published openly and the cell asks no key. Two settings exist if you want them, in the `jp-address` section of `config.jsonc`:

| Setting | Default | What it is |
| --- | --- | --- |
| `base_url` | `""` | the address data base, ending before `.json`; empty uses Geolonia's public host |
| `timeout_seconds` | `15` | how long one address file may take to arrive |

A `base_url` must be a scheme, a host, an optional port and a path, and nothing else: plain http is accepted only for the loopback host, and a query, a fragment, user information or a trailing slash refuses the cell at start, with the refusal naming the setting.

## Use it

The model calls `address_split` on its own when someone asks about a Japanese address: `address_split(address="東京都千代田区霞が関2-1-2")` answers `level` 3, `pref` 東京都, `city` 千代田区, `town` 霞が関二丁目, `other` 1-2, a `point`, and the `attribution`. You can ask for one too: `/jp-address 東京都千代田区霞が関2-1-2`, or `/jp-address` alone to type the address in.

Levels are 0 (nothing matched), 1 (the prefecture), 2 (the municipality) and 3 (one town). A result with no level carries a `status`: `ambiguous_town` lists the towns it could not choose between, `ambiguous_city` the municipalities or prefectures, `bad_args` what to fix, and `upstream` says the address file could not be read. Coarser and honest beats a wrong success: a town name that has a longer sibling in the same file stays at the municipality, and an Arabic number written without 丁目 lists both readings instead of picking one.

The `point` is the town's, or the municipality's when the town has none; it is coarse and is not a rooftop geocode. The attribution is Geolonia's, CC BY 4.0, built on Japan's Digital Agency address base registry, and the model is told to copy it into its reply.

The address is normalized the way the original did: every separator is dropped, fullwidth digits, letters and the fullwidth hyphen are folded, and a prefecture written without 都, 府 or 県 is read in full. Compatibility kanji are not folded to their modern forms and neither is kana, so 澤 stays 澤.

The only peer is the address data host: `https://japanese-addresses-v2.geoloniamaps.com/api.json` for the national list, then `https://japanese-addresses-v2.geoloniamaps.com/api/<prefecture>/<municipality>.json` for the one municipality that matched. At most two GETs per call, no key, no request body; only the matched prefecture and municipality names go into the URL path, so the house number, the lot and the building name never travel. The cell writes no file and spawns nothing.

## What is in it

| File | What it is |
| --- | --- |
| `split.lisp` | The two files, the normalization, the longest-prefix match and the result object. |
| `cell.lisp` | The `address_split` tool, the `/jp-address` command, the settings and the one declaration. |
| `test/` | Offline tests over a scripted network seam. |

## License

MIT, the original's: see `LICENSE` and `NOTICE`. The address data a result is cut from is Geolonia's, CC BY 4.0, built on Japan's Digital Agency address base registry.
