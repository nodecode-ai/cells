# nodecode-chrome

Lets the model drive the user's real, signed-in Chrome. `browser-extension/` is
pi-chrome's MV3 extension, vendored as the Nodecode Chrome Connector with the
changes its NOTICE lists (MIT). The host side is Lisp: an HTTP long-poll bridge
on `127.0.0.1:17318`, the port the extension's manifest hard-codes, and the
formatters that fit a page snapshot into a tool result.

There is no `chrome` tool. The model calls functions in the `chrome:` package
through `eval`, such as `(chrome:snapshot)`, `(chrome:click "el-12" :snapshot t)`
and `(chrome:navigate url)`, or `(chrome:send "page.scroll" :delta-y 800)` for
any extension action, and reads the full list from `(help :chrome)`. Without a
target, actions run in the session's own automation tab, never the user's
active one. `(chrome:screenshot)` saves a file and returns its path.

## Setup

The cell copies the extension to `~/.nodecode/chrome-extension/`, and
`/chrome onboard` names that folder to load at `chrome://extensions`. Loading
the cell and the extension is the consent: every session may drive Chrome.

`/chrome status | doctor | onboard | background [on|off]` are the operator's
controls. `background` decides whether actions leave Chrome's focus alone.

```jsonc
"chrome": {"enabled": true, "background": true,
           "screenshot_dir": "~/.nodecode/chrome-screenshots"}
```

## Threat model

As pi-chrome states it: the bridge listens on loopback only and refuses
browser origins other than the extension, but it does not authenticate local
processes, since the extension carries no token. It is a speed bump, like the
guard.

Tests: `just chrome-check` in nodecode.
