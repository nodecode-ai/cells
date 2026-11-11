# nodecode-secret-handoff

Secret Handoff: a password typed straight into the page it is for, and never into the conversation. A Nodecode rewrite of [aean0x/hermes-secret-handoff](https://github.com/aean0x/hermes-secret-handoff) by [aean0x](https://github.com/aean0x).

An agent that has to log in somewhere is in a bad spot: the password has to
be asked for in the chat, so it lands in the transcript, in the model's
context, and in whatever the model writes next. This cell is the detour
around that. The model says which site needs a password; you type it into a
form this cell serves on loopback; the cell types it into the focused
password field of the browser tab for that site, over CDP; and the model
reads back a status — `ok`, `failed`, the reason — never the value.

## Get it

Tell the agent "add the secret-handoff cell", or run:

```sh
nodecode add nc://secret-handoff
```

## Set it up

The browser has to answer CDP, and the operator has to be able to reach the
form. Nothing here needs a key.

```sh
# a Chrome or Chromium that answers CDP on 9222
google-chrome --remote-debugging-port=9222
```

Then, in `~/.nodecode/config.jsonc` or on the Settings page:

```jsonc
"secret-handoff": {
  "cdp_url": "http://127.0.0.1:9222",  // where the browser answers CDP
  "prompt_seconds": 90,                // how long a handoff waits for you to type
  "cdp_seconds": 8,                    // one CDP exchange's whole budget
  "allow_remote_cdp": false            // accept a cdp_url that is not this machine
}
```

`allow_remote_cdp` is off by design: the value crosses a websocket, and
refusing a non-loopback endpoint by default means a typo in `cdp_url` cannot
send a password to another host.

## Use it

From a session, the model calls `request_secret` with the site's domain
once it has focused the password field in the browser tab. What happens then:

- the cell reads the CDP target list, picks the attached page (or the tab
  the caller named), and refuses a tab whose host is not that domain or a
  subdomain of it — before you are asked anything;
- a notice names the address of a form, `/secret-handoff/?id=…`, where you
  type the password into a real password field. It is served on the
  gateway's own loopback port, and the value goes from the form to the
  waiting call; and
- the cell re-checks the tab's live origin and that focus is on a
  password or one-time-code input, then types it in, once, and drops it.

The model reads only `{"status": "ok", "service": "example.com", "detail":
"injected"}`. A `no_response` status means nobody typed anything inside
`prompt_seconds`; it carries the form's address so the handoff can still be
answered, and nothing was typed.

The command is yours:

- `/secret-handoff` lists what is waiting, with the address to answer it at;
- `/secret-handoff cancel` drops it;
- `/secret-handoff SERVICE PASSWORD` answers by hand — the value is then in
  your shell's own scrollback, which is what the form exists to avoid.

`(help :secret-handoff)` carries the same text into a session's prompt.

Run `.github/test-cell.sh tools/nodecode-secret-handoff` from the cells repository
for the cell's own tests: no browser, no network, every CDP answer stubbed.
