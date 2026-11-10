# nodecode-channel-onebot

OneBot: talk to Nodecode on QQ through your own bridge. A Nodecode rewrite of [mario841859784/hermes-plugin-onebot](https://github.com/mario841859784/hermes-plugin-onebot) by [mario841859784](https://github.com/mario841859784).

QQ gives no bot API of its own. A self-hosted OneBot 11 bridge — NapCat,
Lagrange, LLOneBot, go-cqhttp — logs in as a QQ account and answers an HTTP
API, and this cell is a client of that API and nothing else. Mention the bot
in a group, or reply to something it wrote, and it answers in the chat; a
private message to its number is answered too. The bot's own machine needs
no public address: the cell polls the bridge on loopback, so nothing is
exposed and no port is opened.

## Get it

Tell the agent "add the onebot cell", or run `nodecode add nc://channel-onebot`.

The channel kit that ships with Nodecode comes with it, and does the rooms,
lanes, cards and delivery; this folder contributes OneBot's API, its message
segments and its CQ codes.

## Set it up

1. Run a OneBot 11 bridge of your own, logged in as the QQ account the bot
   speaks as — not your personal account.
2. Turn the bridge's **HTTP API** on and note its address (for the bridges
   above it is a port on loopback, e.g. `http://127.0.0.1:3000`).
3. If the bridge has an access token, keep it in a file (mode 600) or an
   environment variable, never in the config.
4. Put the bot's account in every group it should answer in.
5. Add a `channels.onebot` section to `~/.nodecode/config.jsonc`; the
   folder's `config.example.jsonc` shows it. `base_url` is the bridge's
   API root, `allowed_groups` the group numbers it serves, `allowed_users`
   the QQ numbers allowed to drive it in private, and `owner` the number
   whose word is standing policy.

## Use it

In a group, `@` the bot or reply to its message; in private, just type.
Everything else — sessions per chat, cards, the mention rule, approvals,
`/channels` — is the channel kit's. A picture that arrives is noted as
`(image)` in the words; the bridge still holds it for a tool that fetches
it.

The lane reads the bridge's own history: `get_group_msg_history` for a
group and `get_friend_msg_history` for a person, both of which every bridge
above answers. An answer goes out with `auto_escape`, so a model that
writes `[CQ:at,qq=all]` says those words and never pings a group.
