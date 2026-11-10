# nodecode-ambassly

Ambassly: run a Stripe-based affiliate program -- programs, per-content referral links, referrals, commissions and payouts -- through Ambassly's hosted MCP server, with the money-review playbook on the manual. A Nodecode rewrite of [MelMayssonOwen/hermes-plugins](https://github.com/MelMayssonOwen/hermes-plugins) by [MelMayssonOwen](https://github.com/MelMayssonOwen).

## Get it

Tell the agent "add the ambassly cell", or run `nodecode add nc://ambassly`.

## Set it up

Create a scoped key in Ambassly's dashboard, put it in an environment variable, and name that variable in the `ambassly` section:

```jsonc
"ambassly": {
  // the variable holding your scoped key, sent as a Bearer header to ambassly.com and nowhere else
  "api_key": "AMBASSLY_API_KEY",
  "url": "https://ambassly.com/api/mcp",
  "timeout_seconds": 30
}
```

Keys are scoped per side: a **company** key operates the merchant side (programs, affiliates, commissions, payouts), an **affiliate** key one creator's own portal. A tool that refuses usually means the key is for the other side, not that it is broken.

## Use it

Ask the agent about your program and it calls two tools: `ambassly_tools` lists what the key may call -- the list is the server's own, so it is always current -- and `ambassly_call` calls one of them by name with a JSON object of arguments. `/ambassly` does the same from the shell:

```
/ambassly                                         the tools this key may call
/ambassly list_programs                           one tool, no arguments
/ambassly list_pending_commissions                what is waiting for a decision
/ambassly create_link {"program_id":"...","content":"spring launch"}
```

The manual entry carries the playbook the original plugin's skill shipped, so `(help :ambassly)` (or asking "review my commissions") gets the same discipline: `list_programs` before creating one, the name, destination URL, rate, duration and cookie window confirmed with the person first, a tracking snippet and a Stripe webhook installed before attribution works, one link per video or post, an explicit decision on each commission before `approve_commission` or `reject_commission`, `get_payout_batch` presented as a list to pay that sends nothing, and no estimate standing in for a number the server did not return.

From a script:

```lisp
(nodecode-ambassly:ambassly-text)                        ; the catalog
(nodecode-ambassly:ambassly-call "list_programs")        ; one tool's answer
(nodecode-ambassly:ambassly-call "create_program"
                                 (nlk:json-object "name" "Spring" "rate" 20))
```

Every call goes to `ambassly.com` with your key as a Bearer header and to no other host. The tool answers, and the affiliate and commission data in them, reach your model provider -- as any tool result does.
