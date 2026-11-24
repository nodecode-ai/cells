# nodecode-channel-whatsapp-agent

WhatsApp Agent Platform: talk to your agent on WhatsApp through Meta's agent API. A Nodecode rewrite of [ahtishamdilawar/hermes-whatsapp-agent-platform](https://github.com/ahtishamdilawar/hermes-whatsapp-agent-platform) by [ahtishamdilawar](https://github.com/ahtishamdilawar).

The agent is one made on Meta's WhatsApp Agent Platform: it has its own
identity in WhatsApp, and the person who created it messages it from their
phone like any other chat. This cell is the Nodecode end of that: it long-
polls the platform for the creator's messages, runs each as an ask in the
session you are in, and posts the answer back. A photo, a voice note or a
document they send arrives with the ask, fetched from the platform and read
by the agent's own models; a file the answer names goes back the same way.
No linked device, no public address, no webhook: the API key is the whole
setup.

The rooms, lanes, turn cards, admission and commands are the channel kit's,
in [`../nodecode-channel-kit/`](../nodecode-channel-kit). This folder holds
what is the platform's own: the long poll, the three calls, the candidates
and the section.

## Get it

Tell the agent "add the channel-whatsapp-agent cell", or run
`nodecode add nc://channel-whatsapp-agent`.

## Set it up

1. Create an agent on Meta's WhatsApp Agent Platform and copy the API key it
   is called with. The agent's creator is the one person who can chat with
   it; note their WhatsApp user id (digits, as the platform writes them).
2. Add a `channels.whatsapp-agent` section to `~/.nodecode/config.jsonc`:

   ```jsonc
   {
     "channels": {
       "whatsapp-agent": {
         "api_key_file": "<a file only you can read, holding the key>",
         "allowed_users": ["<the creator's WhatsApp user id>"]
       }
     }
   }
   ```

   `api_key_env` or `api_key_file` keeps the key out of the config; `owner`
   names whose word is standing policy, and an owner may always chat.
   `media_enabled` (on by default) carries files in and out; set it to
   `false` for a text-only lane that never uploads or downloads anything.
3. Restart the gateway, or `/channels` then try again. One organism polls a
   key: a second profile's lane refuses until the first stops.

## Use it

Message the agent from the creator's WhatsApp. Each message is an ask: it
lands in a session of its own per chat, with the tools and models you
configured, and the answer comes back in the chat. What the answer is
written as -- markdown -- leaves as WhatsApp shows it: `**bold**` as
*bold*, `*it*` as _it_, `~~gone~~` as ~gone~, a heading as a bold line, a
link as its words and its address, code as it was written. An answer longer
than 4000 characters is several messages, cut at a paragraph, a line or a
word, counted as WhatsApp counts an emoji (two units). While a turn runs, the
typing indicator and a read receipt stand for it; the platform's messages
cannot be edited, so a turn's card is posted once and the answer follows it
rather than growing in place. `/stop`, `/new`, `/models` and the rest of the
chat commands work as they do in any channel.

**Files:** an ask carrying an image, a voice note, a video or a document is
downloaded through `GET /media/{id}` and handed to the turn as an
attachment, the way any channel's files are; an answer's file is uploaded
(`POST /media`) and posted as the message of its kind, a document keeping
its filename. The words above a file ride on it as its caption when they fit
1024 characters; a longer caption, or one on an audio or a sticker, which
take none, is said first, as a message of its own. Files move only when
`media_enabled` is on.

**What the platform does not have:** message edits (a sent message stays as
it was sent), threads, a command menu, buttons, and any chat other than a
direct one — the creator's. A turn's card is posted once; the typing
indicator and the read receipt stand for the rest.

## Test

```
NODECODE_SRC=<a nodecode checkout> .github/test-cell.sh rooms/nodecode-channel-whatsapp-agent
```

The tests are offline: the markdown-to-WhatsApp conversion, the cut of a
long answer, the caption rule, the remembered place across a restart,
candidates, the request plans, the media upload and download (with
`NLK:HTTP` stubbed) and the channel entry, against a recording executor,
never the network.
