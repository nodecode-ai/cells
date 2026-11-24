# nodecode-crew

Crew: one request becomes a board card that is done only when its proof command exits 0. A Nodecode rewrite of [macd2/Crew](https://github.com/macd2/Crew) by [macd2](https://github.com/macd2).

## Get it

Tell the agent "add the crew cell", or run `nodecode add nc://crew`.

## Set it up

Nothing is required. `crew.board` names the board (one file under the home, one board per name), `crew.proof_seconds` is how long one proof may run (300), and `crew.budget_tokens` is a card's default token budget (0 for none).

## Use it

Ask the agent for something and it opens a card: a title, a goal, where the result lands, what holds when it is done, and the shell command that decides it. A card with a gap in it is a draft and cannot be proved.

The card is proved the same way twice, from the chat or from eval:

```lisp
(crew:plan :title "Ship the pricing page" :goal "a live pricing page"
           :artifact "site/pricing.html" :done-when "GET /pricing answers 200"
           :proof "curl -sf localhost:8080/pricing")
(crew:verdict "crew-1")   ; runs the proof, logs PASS or FAIL with its exit status
(crew:progress "crew-1" :name "the copy" :result "pass" :evidence "reviewed")
(crew:diagnose "crew-1")  ; why the card stands where it does
```

Only a proof that exits 0 passes a card: every run is kept on the card with its exit status and its output, so what a card says about itself is never the verdict. A proof that names a hardline command (a root wipe, a `mkfs`, a fork bomb) is refused when the card opens, before anything runs unattended.

`/crew` in a session's shell reads the board and offers its rows: **Open a card** (a draft, its contract completed by the agent), **Prove a card**, **Read a card** and **Drop a card**. Options typed after it do the same directly: `/crew open <what it delivers>`, `/crew prove crew-1`, `/crew diagnose crew-1`, `/crew drop crew-1`.

The agent finds the verbs in its index and reads them with `(help :crew)`. The five tools it calls are `crew_plan`, `crew_verdict`, `crew_progress`, `crew_diagnose` and `crew_proof_check`.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The cards, the proof run, the verdict log, the five tools and `/crew`. |
| `package.lisp` | The package and the one condition. |
| `test/` | The offline tests: a real board file and a real shell, no network. |

## License

Apache-2.0. The original is macd2's; see `NOTICE` and `LICENSE`.
