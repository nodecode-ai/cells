# nodecode-spotify

Spotify: playback, devices, the queue, search, playlists, albums and the library, as seven tools the model calls. A Nodecode rewrite of [NousResearch/hermes-official-plugins](https://github.com/NousResearch/hermes-official-plugins) by [NousResearch](https://github.com/NousResearch).

## Get it

Tell the agent "add the spotify cell", or run `nodecode add nc://spotify`.

## Set it up

Spotify gives no app a shared public client, so every user registers a small app of
their own and signs in against it. Twice, once with the operator and once in a
browser:

1. Open <https://developer.spotify.com/dashboard>, **Create app**, tick **Web API**,
   and set the redirect URI to `http://127.0.0.1:43827/spotify/callback` (Spotify
   allows plain http on a loopback address).
2. Put that app's **Client ID** in the cell's `spotify` section (`/settings`, or
   `~/.nodecode/config.jsonc`), and restart the cell: `/cells` or `(restart-cells)`.
   The cell refuses to start without a client id, and says so.
3. `/spotify login` answers an authorize URL. Open it, approve, and the browser
   lands on the redirect URI with a page that will not load: that is the callback.
   Copy the whole address from the browser's bar and give it back --
   `/spotify login http://127.0.0.1:43827/spotify/callback?code=...`.
   No port is opened, so this works over SSH.

`/spotify status` says whether a sign-in is stored and when its token runs out;
`/spotify logout` forgets it. Tokens are kept in `spotify/auth.json` under the
Nodecode home, mode 0600, and refreshed by the cell as needed.

| Setting | What it is |
| --- | --- |
| `client_id` | your Spotify app's Client ID (required) |
| `redirect_uri` | the redirect URI that app registers; defaults to `http://127.0.0.1:43827/spotify/callback` |
| `scopes` | what the sign-in asks for; the ten the tools need unless you know better |
| `api_base_url` | `https://api.spotify.com/v1` |
| `accounts_base_url` | `https://accounts.spotify.com` |

## Use it

Ask in words, or name a tool. An id, a `spotify:<type>:<id>` URI and an
`open.spotify.com` link are the same thing to every tool.

| Tool | What it does |
| --- | --- |
| `spotify_playback` | play, pause, skip, seek, shuffle, repeat, volume, the current state, recently played |
| `spotify_devices` | list Spotify Connect devices, transfer playback |
| `spotify_queue` | read the queue, add to it |
| `spotify_search` | tracks, albums, artists, playlists, shows, episodes, audiobooks |
| `spotify_playlists` | list, read, page, create, edit, add to and remove from playlists |
| `spotify_albums` | album metadata and track lists |
| `spotify_library` | saved tracks and albums: list, save, remove |

"put on the new album, then queue two more tracks", "what is playing?", "make a
playlist of the songs I saved this month", "search for that cover and add the
album to my library".

A Free account searches, reads playlists, albums, the library, the queue and the
device list. Playback control -- play, pause, skip, seek, volume, shuffle,
repeat, queue add, transfer -- needs Premium and an active Spotify Connect
device: open Spotify on a phone, a desktop or a speaker. Nothing ever plays on
the machine running Nodecode; the tools steer Spotify wherever it is open.

When the model asks to play a bare list of tracks, the first one plays inside
its album and the rest queue behind it: Spotify's desktop client ignores a bare
`uris` request, and this is the request it honours.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the declaration: the section, the seven tool schemas, `/spotify`, the manual |
| `auth.lisp` | the sign-in: PKCE, the token file, the refresh |
| `tools.lisp` | the one API call, ids and URIs, and `spotify_playback` |
| `items.lisp` | the other six tools |
| `wire.lisp` | the encodings and JSON-on-disk helpers |
| `test/cell-test.lisp` | the lifecycle, every tool over a stubbed wire, the sign-in |

Run `.github/test-cell.sh tools/nodecode-spotify` from the cells repository. The
tests stub every HTTP call, bind the clock and keep the token file under a
scratch home, so they need no network and touch no sign-in.

## License

MIT. See `LICENSE` and `NOTICE`: the tools and their behaviour are NousResearch's
original plugin, rewritten in Common Lisp.
