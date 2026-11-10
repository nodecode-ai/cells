# nodecode-blueprint

Blueprint: turns one raw idea into a source-checked, critiqued and estimated build plan. A Nodecode rewrite of [winchxyz/hermes-blueprint](https://github.com/winchxyz/hermes-blueprint) by [winchxyz](https://github.com/winchxyz).

Six gated phases, and the rules that keep a model honest are checked in code, not asked for in prose:

1. **Understanding**: the interview, the problem brief, the builder profile confirmed for the project.
2. **Context**: research. A claim carries a check mark only when its quote appears in a page this cell fetched itself, and a market size or traction claim needs two independent sites. "Nothing like this exists" is never a check mark: it is a logged absence. The briefing is refused until the tier's coverage is there.
3. **Generation**: three really different variants, names distinct, each with a summary.
4. **Deep dive**: the choice, which must name one of the recorded variants.
5. **Critique**: the verdict starts from a named reference class with a sourced base rate, is stored before it is shown, and changes only with evidence recorded after it.
6. **Plan**: A (agent hours), H (your hours) and W (named waits) units with P10/P50/P90. Effort and calendar are computed in code; a single artifact over 8 agent hours needs an effort driver, a plan with agent work needs review units, and a wait that depends on nothing starts on day 1.

## Get it

Tell the agent "add the blueprint cell", or run `nodecode add nc://blueprint`.

## Set it up

Nothing is required. A project starts from `/blueprint start <your idea>` or `blueprint_phase action=start`. The section has four optional settings: `tier` (quick, standard or deep: how many sourced claims a briefing needs), `min_quote_chars` (the shortest quote that can carry a check mark), `max_fetch_chars` and `timeout_seconds` (one page read's ceiling), and `state_file` (where the projects live; empty means one file under this home).

## Use it

```
/blueprint start a WhatsApp bot that books padel courts for small clubs
/blueprint status           the phase, what its gate still needs, the next call
/blueprint phase 4 why      jump: forward only past every gate
/blueprint list             the projects here
/blueprint resume 2         pick one up
/blueprint stop
/blueprint profile          the builder profile
/blueprint recheck          the plan's numbers against what happened
```

After `/blueprint start`, just talk. A short note rides each request with the phase, what the gate lacks and the next call, and `(help :blueprint)` is the whole method.

The tools the model calls: `blueprint_phase`, `blueprint_profile`, `blueprint_research`, `blueprint_verdict`, `blueprint_plan`, `blueprint_recheck`. Every project, claim, source, verdict, prediction and actual hour lives in one file under this home, and `/blueprint handoff` is not part of this rewrite: the plan is rendered as markdown by `blueprint_plan action=show`.

## Licence

MIT. The rewrite keeps the original's terms; see LICENSE and NOTICE.
