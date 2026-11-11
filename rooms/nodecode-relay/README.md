# nodecode-relay

Relay: a second listener in front of this machine's page, with a password, single-use pairing codes and one token per device. A Nodecode rewrite of [sorsama/deepseek-harness-relay](https://github.com/sorsama/deepseek-harness-relay) by [sorsama](https://github.com/sorsama).

The page itself never leaves loopback. The relay binds its own socket on the
address you name, and every request through it is let in only as a password
session or a paired device, so a phone on the same network reaches your agent
without the agent being open to the network.

## Get it

Tell the agent "add the relay cell", or run `nodecode add nc://relay`.

## Set it up

Say where the page is and where the relay should answer, in the `relay` section
of the config:

```jsonc
"relay": {
  "bind": "0.0.0.0",        // the address the relay listens on
  "port": 3443,             // the relay's own port
  "harness_host": "127.0.0.1",
  "harness_port": 3080,     // the page's own port, which `nodecode web` prints
  "trust_loopback": true   // a browser on this machine is already you
}
```

Then, from this machine:

```
/relay password swordfish     # the one password every browser signs in with
/relay pair phone             # an 8-digit code, good for five minutes, once
```

Open `http://<this machine>:3443/relay/pair` on the phone and type the code
with a name for the device. It holds a token of its own from then on.

## Use it

- `/relay` — whether it is listening, on which address, and what is paired.
- `/relay start` and `/relay stop` — the listener, without touching the page.
- `/relay pair NAME` — a code for one device: single use, five minutes.
- `/relay devices`, `/relay revoke ID` — what was let in, and taking one out.
- `/relay password SECRET` — set or change the password.
- `/relay signout` — rotate the signing key: every session and every device
  ends at once, including any browser that signed in with the password.
- The tools `relay_pair` and `relay_devices` are the same two verbs for the
  model.

Five wrong passwords from one address lock that address for five minutes.
`/relay/health` answers without a session, so a monitor can tell the relay is
up without a password.

## Credits

A Nodecode rewrite of [sorsama/deepseek-harness-relay](https://github.com/sorsama/deepseek-harness-relay)
by [sorsama](https://github.com/sorsama), commit
62d7315bce02cb42c3c326effc8c39f5cfd29666, MIT, Copyright (c) 2026 dsh-relay
contributors (LICENSE). NOTICE says what was rewritten and what was left out.
