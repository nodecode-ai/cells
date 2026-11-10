# nodecode-mood

Mood: a small evolving inner state - mood, energy, rapport and three tone traits - that colours how the agent talks to you. A Nodecode rewrite of [huy-sec/hermes-mood](https://github.com/huy-sec/hermes-mood) by [huy-sec](https://github.com/huy-sec).

The agent keeps a mood (upset to happy, calm to lively), a rapport level and
three slow traits (warmth, playfulness, directness). They change with what you
write, drift back toward neutral over a few hours, and reach the model as one
short line at the start of every turn:

```text
[Your current inner state (let it colour your tone quietly; don't mention it unless asked): mood good, steady; rapport: friendly.]
```

It costs no model call - the mood moves on keyword rules, in a millisecond -
and the line is a few dozen tokens riding the request that was going to be
made anyway. A mood saved under the home is the same at the next launch.

## Get it

Tell the agent "add the mood cell", or run `nodecode add nc://mood`.

## Set it up

Nothing is required: the cell runs on its defaults the moment it is on. The
`mood` section of the config, or `/settings`, has three members:

- `half_life_hours` (default 6) - how long the mood takes to halve its
  distance to baseline. A busy afternoon of praise does not last a week.
- `quiet` (default on) - the line asks the model not to mention the state
  unless asked. Turn it off and the line says only what the state is.
- `state_file` - where the mood is kept; blank is `mood/state.json` under
  the Nodecode home, one file per profile.

`config.example.jsonc` is the section written out.

## Use it

Nothing to do, mostly. What moves it:

| You write | Effect |
|---|---|
| thanks, thank you, appreciate | mood up, warmth and rapport up |
| great, perfect, nice, love it | mood and energy up |
| lol, haha, 😂 | livelier, more playful |
| wrong, broken, ugh, useless | mood down, more direct |
| too long, tl;dr, just tell me | more direct |
| more detail, explain more | more detailed |
| stop joking, be serious | less playful |
| tired, stressed, rough day | calmer, warmer |

One message moves the mood by at most 0.35, the mood halves its distance to
baseline every six hours, and the traits move 0.02 a signal between 0.1 and
0.9. After a break of an hour or more the next turn's line says how long it
has been.

Ask it where it stands, and steer it:

- `/mood` - the line it would send, and the numbers behind it
- `/mood reset` - back to the starting state
- `/mood off` / `/mood on` - pause and resume the line (the state stays)

The same from eval: `(mood:show)`, `(mood:reset)`, `(mood:set-enabled nil)`,
and `(help :mood)` for the manual.

## Limits

- It is a mood, not a memory: nothing about the conversation is kept beyond
  the numbers and the turn count.
- The traits learn slowly and never leave 0.1 to 0.9, so one annoyed message
  does not rewire the agent.
- The scoring is keyword rules on the turn's opening words, as the original's
  was; a tool-using turn's later rounds do not score again.
- The line rides the turn's first request only, where the original added it to
  every model call.
