# nodecode-call-me

Call Me: ring a phone through the /call-me app, speak a sentence, and read the spoken answer back as text. A Nodecode rewrite of [radres/hermes-plugin-call-me](https://github.com/radres/hermes-plugin-call-me) by [radres](https://github.com/radres).

[/call-me](https://callmemcp.com) is an iPhone app that gives a person a private 10-digit number for their agents. This cell gives the model two things to do with that number, and a person the same verbs from a slash command:

| What | Does |
| --- | --- |
| `callme_call` | Rings the phone, says the model's text out loud, waits, and answers the person's spoken reply as a transcript. |
| `callme_text` | Texts the phone: the agent's answer, a reminder, what a scheduled job came to. |
| `/callme` | What the number reaches right now, or a text or a call sent on the spot. |
| `(help :call-me)` | The manual the model reads before it reaches for either tool. |

Both verbs are fenced by the section's `allowed_users`: a number, or a name, that is not on that list is refused before any request leaves, so an agent that has been talked into reaching a stranger still cannot.

## Get it

Tell the agent "add the call-me cell", or run `nodecode add nc://call-me`.

## Set it up

Install /call-me on the iPhone ([callmemcp.com](https://callmemcp.com)) and note the 10-digit number the app shows. That number is what the agent reaches, and it goes in the section — there is no key to copy and no account to create on this side.

In `/settings`, or in the `call-me` section of `config.jsonc`:

```jsonc
"call-me": {
  "allowed_users": ["5551234567"],
  "names": "5551234567=Sam",
  "label": "Nodecode",
  "call_wait": 120
}
```

| Setting | What it does |
| --- | --- |
| `allowed_users` | The 10-digit numbers the agent may text and ring. The cell refuses to start without one. |
| `names` | Optional `number=name` pairs; the model sees the name and may use it in place of the number. |
| `label` | The thread name the phone shows for this agent. |
| `call_wait` | How long a call waits for the person to answer and finish, in seconds; 300 at most. |
| `timeout` | How long one request to the service may take, in seconds. |

The first text or call mints the agent's session with the service, and the agent's own number with it; the token is kept in a `0600` file under the nodecode home, so the number stays the same across restarts. Delete that file (`~/.nodecode/call-me/session.json`) for a fresh number.

The /call-me app on the receiving iPhone is a paid subscription; the cell is free.

## Use it

Ask for it in words — "call me when the deploy finishes", "ring me and ask whether to ship" — and the model picks `callme_call`; "text me the summary when you are done" is `callme_text`. From eval, in the `call-me` package:

```lisp
(call-me:ring "The build is green. Deploy now?" :to "Sam")   ; => the transcript
(call-me:text "The deploy is live." :to "5551234567")        ; => what went out
(call-me:status)                                             ; what /callme answers
```

`/callme` alone says which numbers are reachable and what the agent's own number is; `/callme text Sam the build is green` and `/callme call Sam deploy now?` do it from the shell.

A call is bounded: it ends when the person hangs up or when the wait runs out, and the answer then says `missed` with a hint to send a text instead, so an unattended turn is never held open.

### What it does not do

The original plugin is also a chat surface: it polls the service for the person's texts and voicemails and answers them as a session. This cell is the outbound half — the tools and the command, and the agent's number to be texted at. A reply typed in the /call-me app reaches the phone, not the transcript.
