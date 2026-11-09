# nodecode-browserless

Browserless: renders pages, screenshots them and prints them to PDF in a stealth cloud browser. A Nodecode rewrite of [browserless/browserless-mcp](https://github.com/browserless/browserless-mcp) by [browserless](https://github.com/browserless).

[Browserless](https://www.browserless.io) is a hosted Chromium: one HTTP request renders one page on their machines, with stealth on, optionally through a residential IP, with ad blocking and captcha solving. This cell gives the model three tools that use it, so a script-heavy, login-gated or datacenter-blocked page loads without a browser on your machine. Everything runs on Browserless' side; the cell installs nothing and starts nothing.

## Get it

Tell the agent "add the browserless cell", or run `nodecode add nc://browserless`.

## Set it up

Get an API token from <https://www.browserless.io/account>, then name it in the cell's section — in `/settings`, on the settings page, or by asking the agent to set `browserless.token_env`:

```jsonc
{
  "browserless": {
    // the environment variable holding your token ...
    "token_env": "BROWSERLESS_TOKEN",
    // ... or a file holding it (set one, not both)
    "token_file": "",
    // the region endpoint your account answers on
    "base_url": "https://production-sfo.browserless.io"
  }
}
```

`stealth` is on by default. `proxy` = `"residential"` (with `proxy_country`, e.g. `"us"`) routes the page through a residential IP, which uses more of your plan's units; `solve_captchas` and `block_ads` are off by default. `timeout_seconds` bounds one page and one request, and `content_chars` caps how much page text a tool answers.

## Use it

The model calls the tools itself; you can also call them from a snippet:

- `browserless_fetch` — a page as its words (default) or its rendered HTML, with an optional `wait_for_selector` and `wait_ms` for content that fills in late.
- `browserless_screenshot` — a PNG of the page (optionally full-page, with a viewport size), answered as a path to look at with `(look PATH)`.
- `browserless_pdf` — the page printed to A4, answered as a path.

`/browserless` shows what the cell will do — endpoint, whether the token is set, stealth, proxy, timeout — and `/browserless <url>` renders one page and shows its words, which is the quickest way to see whether your token and proxy settings work.

Pages are rendered on Browserless, so a `localhost` or LAN address is not reachable from there: use a local fetch for this machine. A page longer than `content_chars` is cut, and the answer says how much there was.
