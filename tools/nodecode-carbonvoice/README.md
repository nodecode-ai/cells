# nodecode-carbonvoice

Carbon Voice: read the messages that came into a voice-messaging workspace and answer them in the thread. A Nodecode rewrite of [PhononX/hermes-plugin-carbonvoice](https://github.com/PhononX/hermes-plugin-carbonvoice) by [PhononX](https://github.com/PhononX).

Carbon Voice turns what people say into text. The model gets four tools over the same API a Carbon Voice client speaks to, and a person gets one command:

| What | Does |
| --- | --- |
| `carbonvoice_recent` | The messages that came in lately, newest last: who sent each one, the conversation it is in, the transcript Carbon Voice made of it, and the thread it answers. |
| `carbonvoice_message` | One message by id, so a voice memo whose transcript has not landed yet can be read once it has. |
| `carbonvoice_send` | A text message into a conversation, as the agent's own Carbon Voice account, threaded under the message it answers. |
| `carbonvoice_channel` | One conversation: its name, its kind, and who is in it — how a sender id becomes a name. |
| `/carbonvoice` | Who the token is, and what came in: `status`, `recent [minutes]`. |

Ask it "what did anyone say in Carbon Voice today?" and it calls `carbonvoice_recent`; ask it to answer someone and it threads the reply under that person's message.

## Get it

Tell the agent "add the carbonvoice cell", or run `nodecode add nc://carbonvoice`.

## Set it up

You need a Carbon Voice account — the agent speaks as that account — and a Personal Access Token from [developer.carbonvoice.app](https://www.developer.carbonvoice.app/).

```sh
export CARBONVOICE_PAT=cv_pat_...
```

The section names the variable, never the token:

```jsonc
"carbonvoice": {
  "pat_env": "CARBONVOICE_PAT",
  "base_url": "https://api.carbonvoice.app",
  "since_minutes": 60,
  "limit": 25,
  "exclude_self": true,
  "timeout": 20
}
```

| Setting | What it does |
| --- | --- |
| `pat_env` | The variable holding the token. |
| `base_url` | The API to speak to; the default is Carbon Voice's own. |
| `since_minutes` | How far back `carbonvoice_recent` looks when the call does not say. |
| `limit` | How many messages it answers with at most. |
| `exclude_self` | Leave the agent's own messages out, so a reply is not read back as a new message. |
| `timeout` | How long one request may take, in seconds. |

## Use it

`/carbonvoice` says who the token is and what the window holds. The model reads the manual at `(help :carbonvoice)`.

From eval, in the `carbonvoice` package:

```lisp
(carbonvoice:recent :minutes 30)
(carbonvoice:read-message "<message id>")
(carbonvoice:send "<conversation id>" "the deploy is live" :reply-to "<message id>")
(carbonvoice:channel "<conversation id>")
(carbonvoice:whoami)
```

A voice memo arrives in two pieces: the audio, then its transcript a few seconds later. `carbonvoice_recent` marks a message it has no words for yet as `pending`, and `carbonvoice_message` answers the same message once Carbon Voice has transcribed it.

### What it does not do

The original plugin is a chat platform: it keeps a Socket.IO connection, polls as a fallback, and answers people by itself as they speak, with an allowlist, approval prompts, acknowledgement reactions and a cursor file for the messages that arrived while it was down. Here the agent reads the inbox when it is asked and answers from the same account — the tools above — rather than a bot running beside the gateway. Attachments, voice replies through a text-to-speech pipeline and the `/cv-allow-*` commands are not carried over.
