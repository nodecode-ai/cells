# nodecode-webly

Webly: publish a website or share files at a live webly.site link. A Nodecode rewrite of [Webly-AI/webly-plugin](https://github.com/Webly-AI/webly-plugin) by [Webly-AI](https://github.com/Webly-AI).

## Get it

Tell the agent "add the webly cell", or run `nodecode add nc://webly`.

## Set it up

Nothing. Publishing without an account works at once: a folder becomes a
public page at a `*.webly.site` address, live for 24 hours and claimable for
7 days. The section is optional:

```jsonc
"webly": { "api_url": "https://api.webly.ai", "timeout": 60, "name": "" }
```

## Use it

The model has three tools:

- `webly_publish` takes a folder (or one `.html` file) and answers the live
  URL. The first publish creates this machine's site; every later one updates
  the same site at the same URL. Limits: 25 files, 1 MiB each, 5 MiB in all;
  hidden files and `node_modules` are left out.
- `webly_share` takes files or folders and answers a public link for each,
  plus one folder link, for 24 hours.
- `webly_site` answers where the saved site stands: its URL, when it goes
  offline, when it can still be claimed, and what the API says is next.

The same verbs are callable through eval, `(help :webly)` teaches them, and
`/webly` is the operator's switch:

```
/webly                     the site's state, with a row that publishes a folder
/webly publish ./site      publish that folder
/webly share ./shots       share a folder as links
/webly claim               open the page that signs in and claims the site
```

A claim needs a browser: `/webly claim` prints the page, which carries the
site's token in the fragment, so nothing can read it but the page itself.
After a claim the site belongs to an account, and publishing again goes
through Webly's signed-in tools, which this cell does not carry (see
[NOTICE](NOTICE)).

## What is in it

| File | What it is |
| --- | --- |
| `client.lisp` | The anonymous API: credential, site, files, uploads, status, claim. |
| `cell.lisp` | The primer, the three tools, the `/webly` command, the section. |
| `package.lisp` | The `webly` package and its one condition. |

Every request goes to the API the section names and to nothing else, except
the presigned upload URLs that API returns. The credential is kept under the
home as `webly/state.json`, mode 0600, and is never printed.

## License

Apache-2.0. The helper this rewrites is Copyright (c) Webly: see
[LICENSE](LICENSE) and [NOTICE](NOTICE).
