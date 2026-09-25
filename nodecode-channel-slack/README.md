# nodecode-channel-slack

A Nodecode add-on that lets people talk to your Nodecode through a Slack app.
It connects over Socket Mode, a websocket the app opens itself, so your
machine needs no public address.

- **DMs:** every message is an ask, answered in the DM.
- **Channels:** mention the bot and it answers in a thread under your message.
  Inside that thread, keep talking without mentioning it; each reply
  continues the same conversation.
- **Answers:** written in Markdown, which Slack renders. While a turn runs, a
  status line with Stop and Show tools buttons sits in the thread.
- **Commands:** `/nodecode help`, `/nodecode models`, `/nodecode stop`, and
  so on, run a Nodecode command. Slack doesn't allow slash commands inside
  threads; mention the bot with the command instead (`@nodecode /stop`).
- **Files:** Nodecode reads images and recordings people post. It doesn't
  post files back yet.

## Install

In Nodecode, run `/setup` → **Choose** and pick **nodecode-channel-slack**.
Or copy this folder into `~/.nodecode/addons/nodecode-channel-slack/`.
Either way, the channel kit that ships with Nodecode comes with it.

## Make the Slack app

1. Go to [api.slack.com/apps](https://api.slack.com/apps) and choose
   **Create New App** → **From a manifest**.
2. Pick your workspace and paste [`manifest.json`](manifest.json). It turns on
   Socket Mode, the bot user, the Messages tab (so people can DM the bot), and
   the scopes, events and `/nodecode` command the add-on uses.
3. Choose **Install to Workspace**. Copy the **Bot User OAuth Token**
   (`xoxb-…`) from **OAuth & Permissions**.
4. Under **Basic Information** → **App-Level Tokens**, choose **Generate**,
   add the `connections:write` scope, and copy the token (`xapp-…`).
5. Keep both tokens in files or environment variables, never in the config.
6. `/invite @nodecode` into a channel, or open a DM with it.

Slack's free plan allows custom apps (up to ten per workspace).

## Configure

Add a `channels.slack` section to `~/.nodecode/config.jsonc`, or tell
Nodecode "set up slack" and it walks you through it. It checks both tokens
and lists your channels and people by name, so you pick ids from a list.

```jsonc
{
  "channels": {
    "slack": {
      "bot_token_env": "SLACK_BOT_TOKEN",   // or bot_token_file
      "app_token_env": "SLACK_APP_TOKEN",   // or app_token_file
      "allowed_users": ["U0123ABCD"],       // who may drive it
      "owner": ["U0123ABCD"],               // whose word is standing policy
      "require_mention": true
    }
  }
}
```

The bot refuses to start unless `allowed_users` or `allowed_channels` names
someone or somewhere, because a message it answers runs with your machine's
full authority. See [`config.example.jsonc`](config.example.jsonc) for every
key.

## Test

From a Nodecode checkout's `src/`:

```sh
sbcl --non-interactive --eval '(require :asdf)' \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(push (truename "addons/channels/kit/") asdf:*central-registry*)' \
  --eval '(push #p"/path/to/addons/nodecode-channel-slack/" asdf:*central-registry*)' \
  --eval '(asdf:test-system :nodecode-channel-slack)'
```

The tests run the add-on against a fake Slack on 127.0.0.1 that speaks the
Web API and Socket Mode, so they need no Slack account.

MIT licensed.
