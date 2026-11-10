# nodecode-prediction-markets

PredictionMarketsPicks: EV, Kelly sizing, Bayesian updating, odds conversion, base-rate gaps and combo edge for Kalshi and Polymarket contracts, as six tools the model calls and one command you type. A Nodecode rewrite of [predictionmarketspicks/mcp](https://github.com/predictionmarketspicks/mcp) by [predictionmarketspicks](https://github.com/predictionmarketspicks).

## Get it

Tell the agent "add the prediction-markets cell", or run `nodecode add nc://prediction-markets`.

## Use it

Ask a question the numbers answer: "I think this contract is 55% and it trades at 40 cents — what is my edge?", "how much of a $2,000 bankroll is half-Kelly at 45 cents if I put the win probability at 60%?", "what is +150 as a probability?", "is this 2-leg combo worth the price my venue quoted?".

The model calls one of the six tools:

| Tool | What it answers |
| --- | --- |
| `calculate_ev` | the edge in percent from a contract's price in cents and your probability, with a BUY / SELL / SKIP read at ±5%. |
| `kelly_size` | the stake in dollars at full, half, quarter or eighth Kelly, its share of the bankroll and a risk rating. |
| `bayes_update` | a prior updated by one or more pieces of evidence, with every step and its change. |
| `convert_probability` | implied probability, American odds and decimal odds, from any one of the three. |
| `base_rate_gap` | a market price against one of twelve curated historical base rates (or your own): the gap in points, a signal and the sample quality. |
| `combo_edge` | a same-game combo's fair-value band, and — given the price a venue quoted — its EV and a 7-tier verdict. |

From eval, the same six are functions in the `prediction-markets` package:

```lisp
(prediction-markets:calculate-ev :market-price 40 :your-probability 55)
(prediction-markets:kelly-size :win-probability 60 :market-price 45 :bankroll 1000 :fraction "half")
(prediction-markets:base-rate-gap :market-price 60 :base-rate-id "fed_hold_unemp_below_4")
```

and `/prediction-markets` does the same from a line you type:

```
/prediction-markets ev 40 55
/prediction-markets kelly 60 45 2000
/prediction-markets convert -150 american
/prediction-markets base-rate 60 fed_hold_unemp_below_4
/prediction-markets combo 50,45 22 -150
/prediction-markets ids
```

Two rules the calculators keep, from the original: a probability is a percent (71, not 0.71) and a value between 0 and 1 is refused rather than guessed at, unless you set `percent_guard` to false; and a combo is graded against the quote a venue gave you, never against the product of its leg prices, which no venue pays on legs that settle together.

## Set it up

Nothing is required: the calculators are offline arithmetic on the numbers you pass, with no key and no network. Three settings are optional:

| Setting | Default | What it is |
| --- | --- | --- |
| `bankroll` | `1000` | the bankroll `/prediction-markets kelly` sizes against, when the call names none |
| `default_fraction` | `"half"` | the Kelly fraction that command applies when it names none |
| `percent_guard` | `true` | refuse a probability passed as a decimal fraction (0.71) instead of a percent (71) |

## What is in it

| File | What it is |
| --- | --- |
| `calc.lisp` | the six calculators' arithmetic, ported from the original's `src/lib/formulas.js`, `src/lib/combo-price.js` and `src/tools.js`. |
| `cell.lisp` | the twelve curated base rates, the six boundary contracts, the six tools, `/prediction-markets` and the manual. |
| `package.lisp` | the `prediction-markets` package and the cell's one condition. |

Every answer is arithmetic on what you supplied: no live price is read, so quote the price you passed along with it.

## License

MIT. The calculators and the base rates are Copyright (c) 2026 PredictionMarketsPicks: see `LICENSE` and `NOTICE`.
