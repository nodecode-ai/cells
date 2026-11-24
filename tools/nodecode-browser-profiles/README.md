# nodecode-browser-profiles

Browser Profiles: point Nodecode at a named Chrome, Chromium, Brave or Edge profile over CDP, so the tabs and the logins that profile holds are the ones worked with. A Nodecode rewrite of [anpicasso/hermes-plugin-chrome-profiles](https://github.com/anpicasso/hermes-plugin-chrome-profiles) by [anpicasso](https://github.com/anpicasso).

A profile is a name for a Chromium-family browser on its own user-data-dir and remote-debugging port, or for a browser on another machine that is already listening. Switching to one launches it if it is local and not running, waits for its CDP endpoint, and leaves it open; the verbs below then speak that browser's DevTools HTTP endpoints.

## Get it

Tell the agent "add the browser-profiles cell", or run `nodecode add nc://browser-profiles`.

## Set it up

Give the cell its profiles in `/settings`, or in `~/.nodecode/config.jsonc`:

```jsonc
{
  "browser-profiles": {
    "launch_timeout": 20,          // seconds to wait for a launched browser
    "profiles": {
      "work":    {"port": 9250},
      "scraper": {"port": 9252, "headless": true},
      "uni":     {"port": 9251, "browser": "edge"},
      "box":     {"host": "192.0.2.10", "port": 9222}   // remote: never launched
    }
  }
}
```

Every member but `port` has a default: `browser` is the first Chromium-family browser found on this machine (`chrome`, `chromium`, `brave`, `brave-origin` or `edge`), `binary` overrides the lookup with an absolute path, `data_dir` defaults to `~/.nodecode/browser-profiles/<name>/`, `profile_directory` selects a profile inside that directory, and `headless` defaults to a headed launch except on a Unix with no display. A profile with `host` is **remote**: reachability is checked and nothing is launched.

A `port` that is not an integer 1–65535 refuses that profile with a line saying so, and the rest of the profiles keep working. `config.example.jsonc` in this folder lists the same members.

## Use it

Ask the agent to switch profiles, or call the verbs from eval (or `/browser-profile` in the shell):

| Verb | What it does |
| --- | --- |
| `(browser-profiles:available)` | Every configured profile: its port, whether CDP answers, which one is active. |
| `(browser-profiles:switch "work")` | Make `work` active, launching its browser if needed; answers the endpoint. |
| `(browser-profiles:disconnect)` | Forget the active profile; the browser keeps running. |
| `(browser-profiles:cdp-url)` | The active profile's endpoint, `http://host:port`. |
| `(browser-profiles:version)` | The browser's own name, version and WebSocket URL. |
| `(browser-profiles:tabs)` | The open tabs: id, title, url. |
| `(browser-profiles:open-tab "https://…")` | Open a tab; answers its id. |
| `(browser-profiles:activate-tab "ID")` | Bring that tab forward. |
| `(browser-profiles:close-tab "ID")` | Close it. |

The model has the same three entry points as a tool, `browser_profile` (a `name`, or `disconnect`), and the operator as `/browser-profile [name|off]`. A verb with no profile argument works on the active one, so switch first; without one it refuses and says so.

A browser the cell launched is one you can see and use. Stopping or reloading the cell terminates the browsers it started, and only those: one that was already listening on the port is left alone.

### What the rewrite does not do

- **It does not redirect another tool's browser backend.** The original set `BROWSER_CDP_URL` so every browser tool of the harness followed the switch. Nodecode has no such variable and no CDP consumer: its own browser cells drive an extension (`nodecode-chrome`) or a server of their own (`nodecode-camofox`). This cell therefore speaks CDP itself, and hands the endpoint to anyone who wants it with `cdp-url`.
- **No page-level driving.** Clicks, typing and screenshots ride a CDP WebSocket session, and these verbs use the HTTP endpoints only: tabs are listed, opened, brought forward and closed. Drive a page with `nodecode-chrome` or `nodecode-camofox`.
- **No read of the harness's browser session.** The original flushed live browser sessions and attached a CDP supervisor on the host's own seams; there is nothing of that here to flush.
- **`browser.use_real_profile` is not touched.** The original refused a `data_dir` that is a browser's own default directory and pointed at that setting instead; this rewrite keeps the refusal and the advice, but neither reads nor changes the setting.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-browser-profiles
```

Nothing here starts a browser or opens a socket: the cell's one network seam and its launcher are rebound to a fake DevTools endpoint and a recorder, so the tests prove the folder starts and stops cleanly, that a local profile is launched with its port and data dir and stopped with the cell, that a remote profile is never launched, that a malformed port refuses, that no profiles answers where to put them, that the tab verbs answer over their routes, and that the tool and the command answer through the real dispatch seams.

MIT licensed. See `NOTICE` for what came from the original and what did not.
