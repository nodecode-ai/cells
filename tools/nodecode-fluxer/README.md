# nodecode-fluxer

Fluxer: a chat platform's bot, as a Nodecode tool — post, read, react and pin in its channels. A Nodecode rewrite of [elkimek/hermes-fluxer-plugin](https://github.com/elkimek/hermes-fluxer-plugin) by [elkimek](https://github.com/elkimek).

The cell speaks the same bot REST API as the original Hermes plugin, so it
reaches the same service, the same guilds and channels and the same
messages: [Fluxer](https://fluxer.app), hosted, or a self-hosted deployment
that names its own API root.

## Get it

Tell the agent "add the fluxer cell", or run `nodecode add nc://fluxer`.

## Set it up

1. Make a bot for a Fluxer application and copy its token, `<application id>.<secret>`.
2. In `/settings`, put it in `fluxer.bot_token`.
3. `base_url` defaults to `https://api.fluxer.app/v1`; a self-hosted
   deployment sets its own.
4. `home_channel` is the channel `/fluxer send` posts to when none is named.
5. `/fluxer` prints the endpoint, the home channel and the mention switch;
   without a token the cell refuses at start and says what to set.

## Use it

The model has five tools:

| Tool | What it does |
| --- | --- |
| `fluxer_send_message` | post a message in a channel, as a reply when you name one |
| `fluxer_read_messages` | the last N messages of a channel, oldest first, each with its message id |
| `fluxer_list_channels` | a guild's channels, or every guild's the bot is in |
| `fluxer_react` | put an emoji reaction on one message |
| `fluxer_pin` | pin a message, or take the pin off one |

and you have the same by hand from `/fluxer`:

| Command | What it does |
| --- | --- |
| `/fluxer` | the endpoint, the home channel, the switches, the last failure |
| `/fluxer channels [GUILD]` | the channel directory, with the ids a call takes |
| `/fluxer read CHANNEL [LIMIT]` | recent messages of a channel |
| `/fluxer send TEXT` | post into `home_channel` |
| `/fluxer send #CHANNEL TEXT` | post into that channel |

From eval, the same verbs are Lisp: `(fluxer:send-message "123" "hello")`,
`(fluxer:read-messages "123" :limit 10)`, `(fluxer:list-channels)`,
`(fluxer:react "123" "456" "👍")`, `(fluxer:pin-message "123" "456")`.

A message the cell posts carries `allowed_mentions` and has `@everyone`,
role and user mentions broken with a zero-width space, so a model cannot ping
a whole channel by writing one; `allow_mentions` in the section turns that
off. A refusal answers `ERROR: FLUXER-ERROR: …` and says what the API said.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | the package, its one condition, `(fluxer:…)` |
| `wire.lisp` | the REST calls, the token, the mention rule, the rendering |
| `cell.lisp` | the five tools, the contracts, `/fluxer`, the manual |
| `test/` | the slice: lifecycle, a missing token, every verb over a stubbed wire |

## What it does not do

The original is a gateway platform: people talk to the agent through a
Fluxer channel, over the gateway websocket, with sessions, backlog recovery,
typing beats, streaming edits, approval buttons, threads and a realtime voice
lane. This cell is the bot's client half — post, read, react, pin, list —
so inbound dispatches, voice and the component interactions are not carried
over: see `NOTICE`.

## License

MIT, Nous Research's: see `LICENSE` and `NOTICE`.
