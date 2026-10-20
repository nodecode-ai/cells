# nodecode-channel-x

Talk to nodecode on X (Twitter): the bot answers mentions from allowed people, in reply under the ask.

It speaks as an X account of its own over a logged-in web session. X offers no bot API to such a session, so a small child process, `bridge.py`, signs requests the way the web client does, using [tweetkit-x](https://github.com/nsozturk/tweetkit-x) (MIT), run under `uvx` and pinned.

## What it does

- Reads the account's mentions every `poll_seconds` (60 by default, 15 at least).
- Answers a mention from someone in `allowed_users`. It also answers a reply under one of its own posts, even when that reply does not name it.
- Answers in reply under the ask, as plain text. A long answer goes on as numbered replies, up to `max_posts` (4); the rest is cut and the last post says so.
- Posts at most `daily_posts` (50) a UTC day.
- Each X conversation is one room, and a reply chain is one lane, as a Discord thread is.

## What it never does

- Tweets on its own. Every post is a reply under an ask; a post with nothing to reply to is refused before it reaches X.
- Sends or reads direct messages. X moved DMs to encrypted XChat, which a web session cannot open.
- Starts as an account other than `handle`. The probe and the start both ask X who the cookie is signed in as, and refuse a mismatch.
- Edits a post (X cannot), so the turn's live card is never posted.

## The rule that decides the design

X's [automation rules](https://help.x.com/en/rules-and-policies/x-automation) say non-API automation "such as scripting the X website" may lead to permanent suspension. Give the bot a throwaway account of its own. Never use your own cookie.

## Setup

1. Make an X account for the bot and sign it in on x.com in a browser.
2. Copy that tab's cookie header (it must hold `auth_token` and `ct0`) into a file, mode 600.
3. Add `channels.x` to `~/.nodecode/config.jsonc` (see `config.example.jsonc`): `handle`, `token_file`, `allowed_users`.
4. Run `(nck:probe "x")`. It names the signed-in account, says whether it matches `handle`, and counts the readable mentions.

The bridge needs `uvx` ([uv](https://github.com/astral-sh/uv)) on the organism's PATH; `bridge_command` overrides the interpreter.

## Files

- `bridge.py`: the child. One JSON request per line in, one answer per line out. Its ops are login, me, user_id, mentions, tweet, reply and delete; there is no op for a tweet that is not a reply, and none for DMs.
- `bridge.lisp`: owns the child, restarts it, and keeps the cookie off argv and the environment.
- `events.lisp`: turns mentions into the kit's candidates; the gate's policy; plain text.
- `rest.lisp`: the plans, the reply chain, the daily cap, the executor.
- `probe.lisp`: the probe and the `channels.x` section declaration.
- `adapter.lisp`: the platform, the poll lap, `ncx:request`, `start-channel`.

## Tests

`nodecode-channel-x/test` registers 14 `CHANNEL-X-` tests, including the lane end to end over the real gateway with a fake bridge.
