# nodecode-telegram-business

Telegram Business: drafts a reply to each customer who writes to your business account, DMs it to you, and delivers it only once you approve. A Nodecode rewrite of [NousResearch/hermes-labs-plugins](https://github.com/NousResearch/hermes-labs-plugins) by [NousResearch](https://github.com/NousResearch).

The original was a Hermes plugin that watched a connected Telegram Business account and, for every customer message, had the host model draft a reply and DM it to the owner with **Send**, **Edit** and **Discard** buttons. Nothing reached the customer without a tap. This cell keeps that loop as three tools and one command, so the model drafts, the owner approves in Telegram or in conversation, and the state lives in one file under the home.

| Tool | What it does |
| --- | --- |
| `tg_business_poll` | What arrived: new customer messages, connection changes, and the owner's button presses, which it applies. |
| `tg_business_draft` | Records a drafted reply and DMs it to the owner with Send, Edit and Discard. |
| `tg_business_send` | Delivers a recorded draft now, as drafted or with the owner's own text. |

Ask "what did my customers write?", "draft an answer to Robin's question" or "send that reply" — and `/biz` shows the connections, the drafts waiting and the muted chats.

The properties the original promised hold here too: **no auto-send** (a draft reaches the customer only through the owner's Send, or through a `tg_business_send` the owner asked for), **owner-only buttons** (a press by anyone else is answered ⛔ and changes nothing), **a draft TTL** (24 hours by default; an aged-out draft is refused and settled as expired), **one pending draft per customer chat** (a newer one supersedes the older), and **Send hidden when Telegram does not grant reply-on-your-behalf**, so a button never fails silently.

## Get it

Tell the agent "add the telegram-business cell", or run `nodecode add nc://telegram-business`.

## Set it up

1. In Telegram, talk to **@BotFather**, `/newbot`, and copy the token.
2. In the Telegram app: **Settings → Business → Chatbots**, add that bot, and grant it **Reply to messages** if you want the cell to be able to deliver a reply for you. Without that grant the draft still arrives, with Send hidden.
3. Start a chat with the bot yourself: that DM is where the drafts are delivered.
4. Name the token's environment variable in the cell's section of `config.jsonc` (or in `/settings`):

```jsonc
"telegram-business": {
  "bot_token_env": "TELEGRAM_BUSINESS_BOT_TOKEN"
}
```

The token is named, never written in the config: export it, or keep it where the shell reads it. Optional members: `persona` (the instruction drafts follow), `draft_ttl_hours` (24), `max_customer_text_chars` (4000) and `timeout` (15, one Bot API request's ceiling).

## Use it

Ask the agent what is waiting:

> what did my customers write today?

The model calls `tg_business_poll`, reads the customer events, drafts an answer in your voice, and calls `tg_business_draft`. Your DM gets the customer's message and the suggested reply, with the three buttons.

- **Send** delivers that text to the customer over your business connection.
- **Edit** makes your next DM the outgoing text.
- **Discard** drops it; the customer sees nothing.

The next poll applies your press and tells the model what you chose, so it can answer "sent", or draft another. If you would rather approve in conversation — "yes, send it" — the model calls `tg_business_send` with the draft id.

`/biz` shows the connections and the drafts waiting; `/biz pause` stops drafting while messages are still read, `/biz resume` starts again, and `/biz off <chat_id>` mutes one customer chat.

What the rewrite does not do: the original drafted on its own whenever a customer wrote, with a debounce that coalesced a typing burst into one draft. A cell has no handler of its own, so here the model calls the tools (or a cron prompt calls `tg_business_poll` every few minutes) and the coalescing is the one-pending-draft rule. Its SQLite store is one private JSON file, and the customer names it quoted are the ones the events carried.
