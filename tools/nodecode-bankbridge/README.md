# nodecode-bankbridge

BankBridge: your own bank, card and brokerage accounts, read-only, as a Nodecode cell. A Nodecode rewrite of [bankbridge-money/bankbridge-plugin](https://github.com/bankbridge-money/bankbridge-plugin) by [bankbridge-money](https://github.com/bankbridge-money).

Ask "how much did I spend on restaurants last month", "what subscriptions am I paying for", "am I making more than I spend", "did my paycheck come in", "how are my investments doing" — and the answer comes from your own accounts, live, through BankBridge's remote MCP server. Nothing can move money, pay a bill or place a trade.

## Get it

Tell the agent "add the bankbridge cell", or run `nodecode add nc://bankbridge`.

## Set it up

BankBridge needs an account and a connected bank; pricing is theirs ($5 a month or $50 a year per connected bank). Then:

1. Copy a key (`bbk_...`) from the BankBridge dashboard.
2. Export it in the shell that starts Nodecode, under the name the section reads:

```sh
export BANKBRIDGE_API_KEY=bbk_...
```

Tell the agent "set up the bankbridge cell" to write the section, or write it yourself:

```jsonc
"bankbridge": {
  "api_key_env": "BANKBRIDGE_API_KEY",   // the variable holding the key, never the key
  "url": "https://bankbridge.money/api/mcp",
  "timeout": 60,
  "result_chars": 20000
}
```

Then `/bankbridge check` proves the key with one call and lists a few of the tools the server offers.

## Use it

| What | Does |
| --- | --- |
| `bankbridge_list_accounts`, `bankbridge_get_account` | every connected account with its live balance; credit utilization from the limits |
| `bankbridge_get_spending_summary`, `bankbridge_list_transactions`, `bankbridge_search_transactions` | how much went where, the raw rows with filters and paging, one charge by merchant or amount |
| `bankbridge_get_merchant_history`, `bankbridge_get_recurring_charges` | one merchant over time and its price drift; the subscriptions and bills BankBridge detected |
| `bankbridge_get_monthly_cashflow`, `bankbridge_list_categories` | income against spending for one month; the category names the filters take |
| `bankbridge_list_holdings`, `bankbridge_list_investment_transactions` | portfolio, gains and dividends |
| `bankbridge_connect_bank` | a link to connect another bank |
| `/bankbridge` | where it points, which variable holds the key, and what it offers; `check` proves the key |
| `/balances`, `/subscriptions`, `/monthly-report`, ... | the original plugin's 19 money reviews, each as a slash command: it submits its own prompt to the session that ran it |

The cell is a small MCP client of its own: every call is three JSON-RPC messages over Streamable HTTP (initialize, initialized, tools/call) to the one endpoint, and each tool's arguments go to the server as given, so the server owns the schema. A `warnings` array in an answer means a bank needs reconnecting; tell it and it says so.

Amounts are dollars: positive is money leaving the account, negative is money coming in. Card payments from checking and transfers between your own accounts are not spending. That guidance rides `(help :bankbridge)`, so the model reads it before it answers.
