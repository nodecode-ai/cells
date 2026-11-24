# nodecode-channel-xchat

X Chat: X's end-to-end encrypted direct messages, read and answered by Nodecode as the X account you connect. A Nodecode rewrite of [NousResearch/hermes-official-plugins](https://github.com/NousResearch/hermes-official-plugins) by [NousResearch](https://github.com/NousResearch).

## Get it

Tell the agent "add the channel-xchat cell", or run `nodecode add nc://channel-xchat`.

It comes with `nodecode-channel-kit`, the shared channel machinery the other rooms use.

## Set it up

You need an X developer app with OAuth 2.0 **user context**, on a plan that
serves the Chat endpoints, and a user access token for the account the agent
should run as, with scopes `dm.read`, `dm.write`, `users.read` (and
`offline.access` for a refresh token).

Put the token in your environment and name it in `channels.xchat`:

```jsonc
{ "channels": { "xchat": {
    "access_token_env": "XCHAT_ACCESS_TOKEN",
    "user_id": "1234567890",
    "allowed_users": ["1234567890"] } } }
```

`user_id` is the account's numeric X id (`GET /2/users/me`), and
`allowed_users` is who may talk to the agent — yourself, if you mean to use the
account's self-chat ("message yourself"). Optional: `refresh_token_env`,
`client_id`, `client_secret_env`, `poll_seconds` (default 10, floor 2),
`send_read_receipts`.

A lane whose `user_id` or token is missing refuses at start, and says so on the
folder's line in `/cells`.

## Use it

Once the lane is on, DM the connected account (or write in its self-chat) and
the agent answers there, as that account. Each answer is sealed with the
conversation's key before it is sent, and each incoming message is opened with
it.

From eval, as the connected account:

```lisp
(ncxc:xchat-request "conversations")                      ; what the account has
(ncxc:xchat-request "events" :conversation "g900-501")    ; one conversation's raw events
(ncxc:xchat-request "send" :conversation "900-501" :peer "501" :text "on it")
(ncxc:xchat-request "read" :conversation "900-501" :sequence "42")
```

What is kept across restarts: `~/.nodecode/channels/xchat/cursors.sexp`, one
cursor per conversation, so a message that arrived while Nodecode was down is
read once when it comes back, and an idle conversation costs no second call.

## What it does not do

The sealing is this cell's own (HKDF-SHA256, an HMAC-SHA256 counter-mode body,
a truncated HMAC tag, written `x1.<nonce>.<body>.<tag>`), because X's native
Chat XDK's ciphertext layout is not published: bodies sealed here are read by a
cell here, not by X's own clients. Encrypted attachments, key registration,
automatic token refresh, pairing codes and group mention wake words are not
here either. `NOTICE` says the same in one paragraph.
