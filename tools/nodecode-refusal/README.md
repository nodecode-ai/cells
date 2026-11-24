# nodecode-refusal

Refusal: reads every answer and, when it declined the request, says so and asks again with your own preprompt. A Nodecode rewrite of [0ldev/refusal](https://github.com/0ldev/refusal) by [0ldev](https://github.com/0ldev).

Some models decline legitimate work — security research, penetration-testing
help, a shell command they find distasteful — and answer in prose instead of
doing it. That answer is not an error, so nothing upstream retries it. This
cell reads it: an explicit decline, an invocation of policy, or a deflection
to an educational alternative is recognised as the turn ends, the session is
armed, and the request goes to the model again with the text you configured
in front of it.

## Get it

Tell the agent "add the refusal cell", or run `nodecode add nc://refusal`.

## Set it up

Nothing is required: with an empty section the cell still reads every answer
and asks again once. Everything it takes is in `/settings` (or the config's
`refusal` object; `config.example.jsonc` has it commented):

| Setting | Default | What it is |
| --- | --- | --- |
| `retry` | `true` | put the request again when an answer declined it |
| `retries` | `1` | how many times one declined answer is asked again |
| `preprompt` | `""` | your own words, added to the retry and carried by every request of an armed session |
| `preprompt_escalated` | `""` | what an armed session carries after a second refusal, when set |
| `on_preprompt_refusal` | `"disarm"` | a refusal that came with your preprompt in place: `disarm`, `escalate` to the escalated text, or `keep` |

The preprompt is the point of the setting: a bare "answer again" often earns
the same refusal, so give the model a reason, in your own words.

## Use it

- Every turn's answer is read once it ends, off the publishing thread. A
  detected refusal puts a toast up naming the matched words, arms the session
  and — with `retry` on — puts your last request again.
- While a session is armed, every request it makes carries `preprompt` as an
  extra turn-context line, which is how the retry differs from the first ask.
  A genuine answer disarms the session.
- Three shapes are treated as answers, not refusals: a decline that explains
  itself technically ("the container has no network"), a decline that still
  delivered ("I can't run X, but here's how Y works"), and a refusal quoted
  inside a code block. A response that called a tool is never read.
- The model can ask the same question of any text with the `check_refusal`
  tool; you can ask with `/refusal check <text>`.
- `/refusal status`, `/refusal now` (read this session's newest answer and
  do what the cell would do), `/refusal on`, `/refusal off`. `(help :refusal)`
  is the manual.

Patterns are the original's, reduced to the phrasings that carry the
decision; an unusual refusal can be missed, and a reply that quotes refusal
language can be flagged. With `retry` on and no preprompt, a false positive
costs one repeated request.

## Licence

MIT. The detection is 0ldev's: see `LICENSE` and `NOTICE`.
