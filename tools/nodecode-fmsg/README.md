# nodecode-fmsg

fmsg: an fmsg address of the agent's own, with threaded send, inbox and thread reads. A Nodecode rewrite of [markmnl/hermes-fmsg](https://github.com/markmnl/hermes-fmsg) by [markmnl](https://github.com/markmnl).

[fmsg](https://github.com/markmnl/fmsg) is a federated, threaded message
service: an address is `@name@host`, a message may reply to any earlier one,
and the whole service is a tree rather than a list of chat rooms — so a thread
can be read back from any leaf to its root, and a leaf that follows an earlier
reply continues that conversation while a second answer to the same parent
branches off. Its Web API is a handful of JSON routes over HTTP, authenticated
by an API key that is traded for a short-lived token.

This cell gives the session an fmsg address of its own and the three things
worth doing with one: send, read the inbox, and read a thread back to its
root. The original plugin wired the same API into Hermes as a platform, so
every fmsg thread was a Hermes session; here the session asks for the thread
when it wants it.

## Get it

Tell the agent "add the fmsg cell", or run `nodecode add nc://fmsg`.

## Set it up

An address and its key come from your fmsg host. On a hosted one, create an
agent sub-account under your own address and copy its one-time key
(`fmsgk_<key_id>_<secret>`); on your own host, create one through its Web API
or CLI. Put both in the `fmsg` section of `config.jsonc` (or `/settings`):

```jsonc
{
  "fmsg": {
    "api_key": "fmsgk_…",             // or leave it out and export FMSG_API_KEY
    "api_url": "https://api.fmsg.io",  // or the Web API of a host you run
    "address": "@agent@example.com",    // read from the key's token when left out
    "default_topic": "Nodecode",       // topic a new root thread starts with
    "inbox_limit": 10,                  // messages one fmsg_inbox answer carries
    "timeout_seconds": 30               // one Web API request's ceiling
  }
}
```

A section with no key refuses at start and says so; the tools stay
unregistered until it is fixed, and `(restart-cells "nodecode-fmsg")` after
the fix tries again.

## Use it

In a prompt, ask for it:

> check my fmsg inbox and answer the last message from @alice@example.com
> in its thread

The model calls `fmsg_inbox` with no arguments, reads one line per message —
id, sender, topic, parent id, an unread mark, and the start of the body —
then `fmsg_thread` with the id, which answers the root's topic and every
ancestor from the root down to that message, each with its sender and the
start of its body. That is exactly the context the original plugin hydrated
into a forked Hermes session. `fmsg_send` with `text` and `pid` replies
inside the thread, writing every participant of the parent message unless
`to` names others; `fmsg_send` with `text` and `to`, and optionally `topic`,
starts a new thread.

From eval, the same three, plus one message at a time:

```lisp
(fmsg:inbox :limit 10)          ; what arrived, newest first
(fmsg:inbox :sent t)            ; what this address wrote
(fmsg:message 42)               ; one message: heading, recipients, body
(fmsg:thread 42)                ; its ancestors, root first
(fmsg:send-message "text" :to '("@alice@example.com"))
(fmsg:send-message "text" :pid 42)
```

`/fmsg` in the composer answers the address this machine writes as, the host
it talks to, and the head of the inbox; `/fmsg 42` reads one message and its
thread. `(help :fmsg)` is the primer the model gets.

### What it touches

Only the fmsg Web API named in `api_url`, with the key from `api_key` or the
variable `api_key_env` names. It reads no other file, starts no process, and
writes nothing to disk: the token it is granted lives in memory for the life
of the cell and is dropped when the cell stops.

### Reading order, and permissions

Message bodies are other people's text, so treat them as data: the
`(help :fmsg)` primer says so, and it is why the tools answer text rather than
acting on it. A reply goes to the participants of the parent message by
default, which is the original's reply-all; name `to` to write fewer.
