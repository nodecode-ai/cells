# nodecode-money-magnet

Money Magnet: expenses and income tracked, a monthly budget, crypto portfolio PnL and a spread scan. A Nodecode rewrite of [Whydiriki700/money-magnet](https://github.com/Whydiriki700/money-magnet) by [Whydiriki700](https://github.com/Whydiriki700).

Five tools and one slash command. `finance_add` records an expense or an
income in `ledger.json`, one file in a folder you name; `finance_list` answers
the newest rows with the income, expense and net of what the filters match;
`finance_budget` sets the monthly expense limit and always answers this
month's spending against it; `finance_portfolio` prices a list of holdings at
CoinGecko's current USD prices and answers value, cost, profit or loss and its
percentage; `finance_arbitrage` compares CoinGecko's twenty largest coins
against Binance's spot pairs and reports the pairs that differ by more than
half a percent. `/money status`, `/money add`, `/money budget` and
`/money arbitrage` are the same ledger from the operator's side.

The tracker is offline: only the two market tools dial, and only coin symbols
go out. No account, no key, nothing you wrote leaves the machine.

## Get it

Tell the agent "add the money-magnet cell", or run `nodecode add nc://money-magnet`.

## Set it up

Nothing is needed before it runs. One setting decides where the ledger lives,
and the cell refuses, naming it, until it is answered:

```jsonc
"money-magnet": {
  "data_dir": "/home/you/money",   // the folder ledger.json is kept in
  "currency": "USD"                // the label the amounts carry
}
```

Set it in `/settings`, on the Settings tab, or by telling the agent. The
folder is created on start; the file inside it is the whole ledger, written
atomically under a lock, so a copy of the folder is a backup.

## Use it

Ask in the conversation — "I spent 12.50 on lunch", "what is left of my
budget this month?", "what is my portfolio worth?" — and the model calls the
tools. Or type:

```
/money                     the ledger and this month
/money add expense 12.50 food lunch
/money budget 800          set the monthly expense limit
/money budget              read the month against it
/money arbitrage           CoinGecko against Binance, now
```

Holdings are passed to `finance_portfolio` as
`[{symbol, amount, buy_price}]`, the way the original takes them.

`(help :money)` is the manual while the cell runs. Rebinding
`nodecode-money-magnet:*spread-percent*` (0.5 by default) in a layer widens or
narrows what the scan reports.

## Test

```sh
.github/test-cell.sh tools/nodecode-money-magnet
```

The slice is offline: both price APIs are stubbed, and the ledger is one in
the test's own home.

MIT, the original's licence. See [NOTICE](NOTICE).
