# nodecode-fusion

Fusion Router: reads what the agent harnesses on this machine can call, tiers it frontier or cheap, and builds Frontier / Middle Manager / Frontier rosters from it. A Nodecode rewrite of [BoredSexyJordan/hermes-fusion](https://github.com/BoredSexyJordan/hermes-fusion) by [BoredSexyJordan](https://github.com/BoredSexyJordan).

## Get it

Tell the agent "add the fusion cell", or run `nodecode add nc://fusion`.

## Set it up

Nothing is required. With no settings the cell reads `~/.hermes/config.yaml` with its `auth.json`, `~/.codex/config.toml` and `~/.claude/settings.json`, and lists what those name: each (provider, model) tiered by the original's name heuristics, then composed into rosters. A machine with none of them still gets the rosters — with no models detected — and can pin its own through a templates file.

Set a member in `/settings` or in `~/.nodecode/config.jsonc` under `fusion`, then `(restart-cells "nodecode-fusion")`. `config.example.jsonc` beside this file is the whole section, commented.

| Member | Default | Notes |
| --- | --- | --- |
| `harnesses` | all four | which configs to read: `hermes`, `codex`, `claude`, `grok` |
| `hermes_home` | `~/.hermes` | the folder holding Hermes' `config.yaml` and `auth.json` |
| `codex_home` | `~/.codex` | the folder holding Codex' `config.toml` |
| `claude_home` | `~/.claude` | the folder holding Claude's `settings.json` |
| `count_env_keys` | `true` | count a provider whose API key is set here, when no config names a model |
| `templates_file` | unset | a JSON file of `{"team_templates": {name: roster}}` merged over the detected rosters |

## Use it

Ask what models this machine has, or call the tools:

- `fusion_teams` — the detected fleet, then every roster: a cheap middle manager plans and delegates, frontier workers implement, a frontier consult owns the final decision. `harness=codex` reads one harness; `template=technical` answers one roster's JSON.
- `fusion_validate` — a Fusion plan packet (v3 or v4), as JSON or inside raw planner output: `VALID`, or `INVALID` and every rule it broke.

And from the operator's side: `/fusion` lists the fleet and the rosters, `/fusion technical` shows one roster's JSON.

Nothing leaves the machine: no network, no subprocess, no model call of its own.
