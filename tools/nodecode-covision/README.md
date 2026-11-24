# nodecode-covision

Covision: the client's assigned tasks, meetings and briefs, and published operating knowledge, read-only, through the vendor's own connector. A Nodecode rewrite of [Covision-Consultants/covision-agent-plugin](https://github.com/Covision-Consultants/covision-agent-plugin) by [Covision-Consultants](https://github.com/Covision-Consultants).

The model gets two tools — `covision_tools`, which lists the read-only tools
the signed-in user's connector approves, and `covision_call`, which runs one
of them by name — plus every tool the connector itself lists, registered
under its own name (assigned tasks, accessible meetings and briefs, published
knowledge). `/covision` shows the connection or describes one tool, and the
original's skill rules ride every request, so the model cites what it read and
never claims a record was written.

## Get it

Tell the agent "add the covision cell", or run `nodecode add nc://covision`.

## Set it up

Covision's connector answers only an OAuth bearer token, and the sign-in is
the operator's: complete Covision's browser login in any MCP client that
supports it, then put the access token in the `covision` section of
`config.jsonc` (or `/settings`), inline or through an environment variable:

```jsonc
{
  "covision": {
    "access_token": "…",              // or access_token_env: "COVISION_TOKEN"
    "endpoint": "https://connector.covisionperformance.com/mcp",
    "timeout_seconds": 30
  }
}
```

The grant is scoped to `tasks:read`, `meetings:read` and `knowledge:read`,
belongs to you and your organization, expires, and can be revoked in Covision.
Without a token the cell refuses to start and says which member it misses.

## Use it

Ask about a client: "what's overdue for Northwind", "prep me for Thursday's
review", "what does our onboarding guidance say". The model calls
`covision_tools` when it does not know the connector's tool names yet, then
`covision_call` with that tool and its arguments; `/covision` answers the same
listing on the command line, and `/covision <tool>` describes one.

Nothing here writes: the connector is read-only, so a task, meeting or brief
is never created, changed, assigned or sent from this cell.

## Credits

Apache-2.0, as the original. See [NOTICE](NOTICE); the original's LICENSE is
here unchanged.
