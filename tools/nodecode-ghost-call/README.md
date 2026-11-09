# nodecode-ghost-call

Ghost Call: a silent coach for the operator's own phone call — one-line hints as the other party talks, and one summary with what was agreed when it ends. A Nodecode rewrite of [winchxyz/hermes-ghost-call](https://github.com/winchxyz/hermes-ghost-call) by [winchxyz](https://github.com/winchxyz).

## Get it

Tell the agent "add the ghost-call cell", or run `nodecode add nc://ghost-call`.

## Set it up

The cell talks to OpenAI with a key from
[platform.openai.com](https://platform.openai.com/api-keys). Either export it:

```sh
export OPENAI_API_KEY=sk-...
```

or set it in `/settings` (or the page's Settings tab) under `ghost-call`:
`api_key` for the key itself, or `api_key_env` for the name of the variable
holding it. A key pasted into the panel is kept in a 0600 file under
`~/.nodecode/secrets/`; the folder holds no secret.

Nothing else is required. `/ghost` with no argument says where the call
stands, and `(help :ghost)` is the model's guide to the tools.

## Use it

You are on the call; the agent listens and whispers, never speaks to the other
party. Say what you want and it opens the call:

> "ghost this call — I'm calling the restaurant to book a table Friday at 8 for
> four, budget $80, and I won't go over $60 a head."

Then, as the call goes, the agent hands the coach each line the other party
says and reads the hint back. From the keyboard:

| Command | What it does |
| --- | --- |
| `/ghost start <goal>` | open a call on a goal, with the facts and your private target |
| `/ghost hint <what they said>` | one line through the same gate and model |
| `/ghost status` | state, duration, hints given and hints left |
| `/ghost allow` | let the coach say one line to the other party, with the AI disclosure in front |
| `/ghost hangup` | end the call and read the one summary |

| Tool | Use |
| --- | --- |
| `ghost_start` | open the call: goal, who is on the other side, your private target |
| `ghost_line` | what the other party just said; answers the next hint or SILENT and why |
| `ghost_hint_latest` | the hints already given, for a question asked after the call |
| `ghost_intervene` | one short phrase to the other party, once `/ghost allow` |
| `ghost_hangup` | end the call and answer the summary |
| `ghost_summary` | that summary again |
| `ghost_status` | where the call stands |

### What the original taught, kept here

The hints are not free-form chat: most of a phone call never reaches a model.
Fillers, greetings, backchannels, repeats and the operator's own words are
gated out locally, a hint waits at least four seconds after the last one, a
call is capped at forty hints and fifteen minutes, and a hint that reads as a
question to the operator is dropped. The prompt is the original's: one action
per hint, react to the last thing said, and never say the operator's target or
walk-away number. The disclosure on the first intervention line is the
original's too.

## Not ported

The original's FastAPI bridge — the two browser pages, the mu-law audio
relay, OpenAI realtime transcription, the earpiece whisper over TTS, and the
Twilio legs with their dial allowlist. A cell runs inside the organism with no
audio device and no public URL, so the lines arrive as text: the agent feeds
them from the conversation, or the operator types them. Everything above the
audio — the gate, the prompt, the limits, the intervene gate, the one summary
— is the original's, in Common Lisp.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-ghost-call
```

Every request is a stubbed `nlk:http` and the clock is a counter, so nothing
reaches a model and no key is needed.

MIT licensed, as the original is.
