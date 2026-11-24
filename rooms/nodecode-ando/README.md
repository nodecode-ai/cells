# nodecode-ando

Ando Platform: answer the workspace messages your Ando agent is invited to, from a Nodecode session. A Nodecode rewrite of [Ando-Corporation/hermes-ando](https://github.com/Ando-Corporation/hermes-ando) by [Ando-Corporation](https://github.com/Ando-Corporation).

[Ando](https://ando.so) is a workspace where people and their agents share
conversations. An agent joins the way a person does: someone in the workspace
opens **Invite members > Agent**, copies the invitation link and gives it to
the agent. From then on the agent's messages arrive in Ando and its answers
land in the same conversation, as itself.

This cell is that connector for Nodecode. It polls Ando every few seconds,
runs each message as a turn in a Nodecode session - the same agent, tools and
models you have in the terminal - and posts the answer back with
`(ando:reply "...")`.

## Get it

Tell the agent "add the ando cell", or run `nodecode add nc://ando`.

## Set it up

1. In the Ando workspace, open **Invite members > Agent** and copy the
   invitation link (`https://agents.ando.so/invite/...`). It is one-use.
2. Ask Nodecode to set up the cell, or put the link in `~/.nodecode/config.jsonc`:

   ```jsonc
   {
     "ando": {
       "invite": "https://agents.ando.so/invite/...",
       "agent_name": "Nodecode"
     }
   }
   ```

   `agent_name` names a *new* agent; a link for an existing agent reconnects it
   under its own name.
3. Restart the cell (`(restart-cells)` or the page's **Restart cells**). On the
   first start it redeems the invitation, writes the key to a mode-600 file
   under the Nodecode home (or to `credential_file` when the section names
   one), and starts answering.

The key never goes into the config, and the invitation is not needed again
once the credential file exists. If the key was rotated - someone reconnected
the agent from **Settings > Members** - paste the new invitation as `invite`
and run `/ando connect`.

## Use it

- **Incoming.** A mention, a direct message or a reply in a thread the agent
  is in arrives as a turn. The turn names who wrote, the conversation and, for
  a reply, its thread; the answer is `(ando:reply "...")`, once, in markdown.
- **The answer** posts to the conversation the message came from: as a reply
  in the thread when it was a thread, as a new message otherwise.
- **A turn that stops** without answering still says so in the conversation,
  once, so a person is not left with silence.
- **`/ando`** prints the agent and workspace Ando confirms, the poll interval
  and the session; `/ando sweep` reads the inbox now; `/ando connect` redeems
  the section's invitation again.
- **`(help :ando)`** is the model's own primer: how to answer an Ando message.

`allowed_members` and `allowed_conversations` narrow who and where the agent
answers; both empty admits everything Ando already lets in.

### A conversation per ask

Every message arrives in the section's one session (`session`, `"ando"` by
default), so a workspace conversation keeps its context there.

## Configuration

| Key | Default | What it does |
| --- | --- | --- |
| `invite` | | The one-use invitation link; the cell redeems it and writes the credential file. |
| `credential_file` | a private file in the Nodecode home | Where the redeemed connection is kept (mode 600). |
| `agent_name` | `Nodecode` | The name a new invitation registers; a reconnected agent keeps its own. |
| `session` | `ando` | The Nodecode session incoming messages run in. |
| `allowed_members` | | Workspace membership ids that may drive the agent; empty admits everyone Ando lets in. |
| `allowed_conversations` | | Conversation ids the agent answers in; empty answers every conversation it was invited to. |
| `poll_seconds` | `5` | How often Ando is asked for messages. |
| `reply_timeout_seconds` | `300` | How long one message's turn may run before the agent says it stopped. |

[`config.example.jsonc`](config.example.jsonc) is a commented section to copy.
The section needs `invite` or `credential_file`; neither and the cell refuses
at start with that sentence in `/cells`.

## What it does not do yet

- No file or voice attachments: a message with one arrives as `[an attachment]`.
- No realtime WebSocket: the cell polls the inbox, so an answer starts within
  `poll_seconds` rather than at once.
- No per-conversation sessions and no Ando tool preferences: the whole of the
  agent's tools are offered on every turn.

## Testing

From this repository, `.github/test-cell.sh rooms/nodecode-ando` runs this
cell's tests alone against a pinned Nodecode tree, offline: the invitation,
the MCP framing, the sweep, the answer and the refusal, with every address
stubbed.
