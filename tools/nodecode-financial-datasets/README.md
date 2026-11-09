# nodecode-financial-datasets

Financial Datasets: stock-market data from Financial Datasets' API as 28 read-only tools. A Nodecode rewrite of [financial-datasets/hermes-plugin](https://github.com/financial-datasets/hermes-plugin) by [financial-datasets](https://github.com/financial-datasets).

## Get it

Tell the agent "add the financial-datasets cell", or run `nodecode add nc://financial-datasets`.

## Set it up

One member is the step left: a key. Create one at [financialdatasets.ai](https://financialdatasets.ai), then paste it into `/settings` or put it in the config:

```jsonc
{
  "financial-datasets": { "api_key": "your key" }
}
```

Or name the variable that holds it and leave `api_key` unset, as the original did:

```jsonc
{
  "financial-datasets": { "api_key_env": "FINANCIAL_DATASETS_API_KEY" }
}
```

Then `(restart-cells "nodecode-financial-datasets")`. `/financial` says which service the cell talks to, whether a key answers, and what it registered; `/financial AAPL` answers that stock's price snapshot.

| Member | Default | Notes |
| --- | --- | --- |
| `api_key` | unset | sent in an `X-API-KEY` header, never in the URL |
| `api_key_env` | `FINANCIAL_DATASETS_API_KEY` | the variable read when `api_key` is unset |
| `base_url` | `https://api.financialdatasets.ai/` | the one host the cell dials |
| `timeout` | `30` | seconds one request may take |

The key is read from the section or the variable it names, sent to the host you named and nowhere else, and a refusal never prints it back. Without either, the cell still starts: the 28 tools are there, and the first call refuses naming the member to write. Every call is billed to that key on the Financial Datasets account: see its [pricing](https://www.financialdatasets.ai/pricing).

`config.example.jsonc` beside this file is the whole section, commented.

## Use it

Ask about a company, a price, a filing, an owner, the macro picture or a screen. The tools are on the model from the first turn, so "Apple's revenue and net income over the last four quarters" reaches them without being told to.

| Tool | What you get |
| --- | --- |
| `fd_get_company_facts` | market cap, employees, sector, exchange, SIC codes, events |
| `fd_get_income_statement`, `fd_get_balance_sheet`, `fd_get_cash_flow_statement` | one statement per period, standardized or as reported |
| `fd_get_segmented_financials` | revenue and profit by segment and geography |
| `fd_get_financial_metrics`, `fd_get_financial_metrics_snapshot` | valuation ratios and metrics, over time or latest |
| `fd_get_earnings` | earnings from 8-K/10-Q/10-K/20-F, or the market-wide feed |
| `fd_get_kpi_metrics`, `fd_get_kpi_guidance`, `fd_get_kpi_non_gaap` | the operating KPIs and the non-GAAP measures a company reports |
| `fd_get_stock_price`, `fd_get_stock_prices` | the latest snapshot, or a window of prices at a chosen interval |
| `fd_get_filings`, `fd_get_filing_items`, `fd_list_filing_item_types` | SEC filings, their items (risk factors, MD&A), and the item names |
| `fd_get_insider_trades`, `fd_get_insider_ownership` | Form 4 transactions, and what insiders hold (Form 3/5) |
| `fd_get_institutional_investors`, `fd_get_institutional_holdings` | 13F filers, and what a filer holds or who holds a security |
| `fd_get_beneficial_owners`, `fd_get_beneficial_ownership` | 5%+ owners from Schedules 13D/13G, activist or passive |
| `fd_get_index_fund` | an ETF's holdings and weights, latest or as of a date |
| `fd_get_macro_data`, `fd_get_interest_rates` | the Treasury yield curve, central bank policy rates, US CPI |
| `fd_get_news` | company news, or market-wide news when no ticker is given |
| `fd_screen_stocks`, `fd_list_stock_screener_filters` | screen by metrics and attributes; the fields and operators it takes |

Each answers JSON text, and one call reaches one dataset. A null figure means the API has no value for that period, not zero. Paging is the cell's: a list longer than one page comes back whole, up to 100 pages.

The same calls are in the `financial-datasets` package, for a prompt you would rather type in `eval`:

```lisp
(financial-datasets:fetch "fd_get_income_statement" :ticker "AAPL" :period "annual" :limit 4)
(financial-datasets:fetch "fd_get_news" :ticker "NVDA" :limit 5)
(financial-datasets:fetch "fd_screen_stocks" :filters (list (list :field "pe_ratio" :operator "lt" :value 20)))
```

Each answers the text the tool answers. `(help :financial-datasets)` has the same map of tools in the manual.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the `financial-datasets` section, the 28 registrations, `/financial`, `(help :financial-datasets)` |
| `api.lisp` | the HTTP client, the query it builds, paging, and the table of 28 tools |
| `test/` | offline tests: the lifecycle, every tool through the dispatch seam against a stubbed API, the request shape, paging, the screener's POST, the macro routes and each refusal |

Nothing is installed and no dependency is added: `nlk:http` is the one seam that reaches outside, and it reaches one host.

## Licence

MIT. The original plugin is Copyright (c) 2026 Financial Datasets, Inc.: see `LICENSE` and `NOTICE`.
