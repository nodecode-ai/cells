# nodecode-developh

Developh Website Audit: audits a public website with Google PageSpeed Insights and returns a ranked list of fixes. A Nodecode rewrite of [Developh/skills](https://github.com/Developh/skills) by [Developh](https://github.com/Developh).

## Get it

Tell the agent "add the developh cell", or run `nodecode add nc://developh`.

## Set it up

Nothing. The audit runs on Developh's free MCP server, with no sign-up and no
key. The cell's section is optional and only tunes it:

| Setting | Default | What it does |
| --- | --- | --- |
| `endpoint` | `https://www.developh.co/mcp` | the MCP endpoint every tool posts to, for a proxy of your own |
| `locale` | `en` | answer language: `en`, or `pt` for European Portuguese |
| `timeout_seconds` | `30` | one profile, case-study or inquiry call's ceiling |
| `audit_timeout_seconds` | `150` | one audit's ceiling: an audit takes 20 to 90 seconds |
| `max_chars` | `8000` | how much of one answer the model reads |

They are set in `/settings` (or the page's Settings tab) under `developh`, or
in `config.jsonc`; `config.example.jsonc` is the section with its comments.
`(help :developh)` is the model's guide to the tools, the report and the fix
list.

## Use it

Ask for what you want; the model picks the tool:

- "Why is my website slow?" or "audit example.com's SEO" →
  `run_website_audit`, a mobile PageSpeed Insights audit: the performance,
  accessibility, best-practices and SEO scores, Core Web Vitals, the issues
  that weigh most on the score, missing security headers and page weight. It
  takes 20 to 90 seconds and only reaches public http(s) addresses.
- "What does this studio do?" → `developh_studio_info`.
- "Show me their work with restaurants" → `developh_case_studies`.
- "Send them an inquiry" → `developh_contact`, which needs the person's
  confirmation.

From the keyboard:

```
/developh example.com     one audit, printed in the conversation
/developh studio          the studio's profile
/developh work            its published case studies
/developh                 what the cell is connected to
```

## What it sends where

Every tool call is one JSON-RPC POST to `www.developh.co/mcp`, the endpoint
the plugin shipped as its MCP server. `run_website_audit` passes the address
you give it to Google PageSpeed Insights through that server.

`developh_contact` is the only tool that writes: it creates an inquiry with
the name, email and message the person gave, which the studio stores with its
other inquiries and answers by email, and it sends them a confirmation. The
model is told to use it only after the person asked for it, having shown them
the message, and the tool refuses a call that does not carry `confirmed` true.

The server allows about 20 audits per 10 minutes and 5 inquiries per hour.
Privacy policy: <https://www.developh.co/en/privacy-policy>.

## What changed in the rewrite

The plugin was an Agent Plugins package: an `mcp.json` entry for the studio's
hosted server and one skill that taught the report and the fix list. Here the
skill is the `developh` help topic and the server's four tools are four
`tools/call` JSON-RPC requests the cell makes itself — no MCP client in
between, and the answers' SSE frames are read directly. The tool names, the
arguments and the service are unchanged. See [NOTICE](NOTICE).

## License

MIT. See [LICENSE](LICENSE).
