# nodecode-axioma

Axioma: marks the few phrases of a reply that deserve a second look, and stages an advisory card before a risky call. A Nodecode rewrite of [percyhendrux123-prog/hermes-axioma](https://github.com/percyhendrux123-prog/hermes-axioma) by [percyhendrux123-prog](https://github.com/percyhendrux123-prog).

## Get it

Tell the agent "add the axioma cell", or run `nodecode add nc://axioma`.

## Use it

The agent writes a mark around a short phrase of its reply, and the cell renders it:

```text
Ship it [signal:caution]without an eval or a test[/signal] and it lands under your name. Run the eval first?
```

On a colour terminal that phrase is yellow, underlined, and followed by `(caution)`; on a plain one it is bold and followed by the same word. Colour is never the only cue.

| Mark | Means | Cue | Word |
| --- | --- | --- | --- |
| `stop` | understood, do not proceed | bold + underline | (stop) |
| `caution` | works, but conflicts with your interest | underline | (caution) |
| `go` | a tool call reported successful in this session backs it | italic | (go) |
| `whisper` | worth a second look; stops nothing | bold | (second look) |

- **A reply with no mark comes back exactly as written.** In a reply that carries one, a tag the model malformed is removed, never shown.
- **Green rests on a real call.** `[signal:go;check=CALL-ID]` renders green only when that call, or that tool, was reported successful in this session; otherwise it renders plain and still says its word.
- **A budget.** At most 3 stop/caution/go marks, 2 whispers and 16 words a mark, so marks stay rare; anything over renders plain.
- **Two palettes, or none.** `palette: cvd` is the colour-vision-safe set (Okabe-Ito); `palette: none` gives style and words only. `words: plain_only` puts the word only where there is no colour.

Before a call that sends, spends or deletes, the agent calls the `axioma_card` tool with the tool, its exact arguments, and a card: a light, the intent, one sentence of why, one question. The cell answers the card and a card code that is a keyed digest of the exact text shown, the tool, the arguments and the card's id.

```text
YELLOW  EVAL
intent: clear the build folder
signal: a delete outside the workspace
question: is build/ the one you mean?
advice only; the card is not consent
card code 4XK2-9QRA
```

`/axioma` lists the recent facts (no bodies), `/axioma cards` lists this session's codes, and `/axioma verify 4XK2-9QRA` says in plain words whether the card was staged and then shown: it confirms the log, not the screen.

`risky_tools` names the tools that deserve a card before they run (`["eval", "sh"]`). A call to one of them with no card staged raises a standing notice; it never blocks, approves or executes anything, and it never stops a call.

## What it keeps

A line per fact in `axioma/events.jsonl` under the Nodecode home — level, tool, code, digests, never a phrase or an argument — and one 0600 key file beside it, `axioma/card-code.key`. No network, no subprocess, no model call.

## What is different from the original

The Hermes plugin colored the reply by rewriting it for each surface (KaTeX on Desktop, ANSI in the terminal, bold plus the word on Telegram, Discord and Slack) and staged its card through Hermes's own approval prompt. Nodecode has one shell surface per session, so the rewrite is done where a reader asks for it — `(axioma:render TEXT)`, or `/axioma render TEXT` — with ANSI for a terminal and markdown cues elsewhere; the card is a tool answer and a log the operator can verify, because a cell cannot open an approval prompt of its own. See [NOTICE](NOTICE).
