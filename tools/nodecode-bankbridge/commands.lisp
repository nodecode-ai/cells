;;;; commands.lisp --- the plugin's 19 slash-command prompts, whole.
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Data, not code: (NAME DESCRIPTION PROMPT), one entry per commands/NAME.md of the
;;;; original at c7fcda4 -- frontmatter's description, and the body below it, unchanged.
(in-package #:nodecode-bankbridge)

(defparameter *prompts*
  '(
    ("balances" "Quick balance snapshot — every account, a rolling total, and credit utilization at a glance."
     "You have access to BankBridge MCP tools. The user just wants a fast look at where their money is.

Steps:
1. Call `list_accounts` to get every connected account with live balances.
2. Group by account type (depository, credit, loan, investment).
3. Sum the depository side and the credit-owed side, and compute credit utilization if any credit card with a limit is returned.

Output (keep it tight):

```
💰 Balances

Depository
  <name> (••mask)    $ X
  ...
  ───────────────
  Total             $ X

Credit
  <name> (••mask)   −$ X  (N% of $limit)
  ...

Investment (if present)
  <name> (••mask)    $ X
```

Use Unicode box-drawing minimally. Prefix negative credit balances with `−`. If a credit limit is present, show utilization as `(N%)` rounded to whole percent. Skip sections that are empty.

Relay any `warnings` from the tool call above the summary.")

    ("budget-draft" "Draft a realistic monthly budget based on your actual last-3-months spending."
     "You have access to BankBridge MCP tools. The user wants a budget drafted from their real history, not aspirational numbers.

Steps:
1. Call `get_spending_summary` with `group_by: \"category\"` for each of the last 3 months (pass `start_date` and `end_date` per month). Average the totals per category.
2. Call `get_recurring_charges` — these are must-pays, separate them.
3. Call `get_monthly_cashflow` for the last 3 months to get typical income.

Output a markdown budget in this shape:

```
# Monthly budget draft

Based on your actual spending from <start> to <end>.

## Fixed (recurring charges) — <total>/mo
<list each with amount>

## Variable categories — budgeted caps
| Category | Avg last 3mo | Suggested cap | Delta |
| -------- | -----------: | ------------: | ----- |
<rows, cap rounded to nearest $10>

## Income baseline
<average monthly income from get_monthly_cashflow>

## Savings target
<income − (fixed + variable cap) = target savings>
```

Set the suggested cap ~10% below the 3-month average for discretionary categories (FOOD, ENTERTAINMENT, etc.) and equal to the average for non-discretionary (RENT, UTILITIES, TRANSPORTATION). Flag any category where the cap would require more than a 30% cut — those need the user's attention, not an automatic cut.

Relay any `warnings` from tool calls before the budget.")

    ("cashflow-trend" "Multi-month cashflow trend — income, expenses, net, savings rate as a table + quick-read takeaways."
     "You have access to BankBridge MCP tools. Give the user a trend view of their income vs expenses over time.

`$ARGUMENTS` is the number of months to include. Default: 6. Accepts 3–24.

Steps:
1. For each of the last N months, call `get_monthly_cashflow` with `month: \"<YYYY-MM>\"`.
2. Compute monthly savings rate = (income − expenses) / income. Treat zero income months as N/A.
3. Compute the average income, average expenses, average net over the window.

Output:

```
📈 Cashflow — last N months

| Month    | Income  | Expenses | Net   | Savings rate |
| -------- | ------: | -------: | ----: | -----------: |
| 2026-03  | $ 8,420 | $ 6,315  | +2,105|          25% |
| ...

Averages  $<avg income>  $<avg expenses>  ±<avg net>   <avg rate>%

🔍 Takeaways
- <e.g. \"Expenses have crept up 12% over the window.\">
- <e.g. \"Your most profitable month was 2026-01 (savings rate 31%).\">
- <e.g. \"Savings rate is trending ↓ over the last 3 months.\">
```

Keep the takeaways tight — at most 3. Only flag a trend if it's directionally consistent over at least 3 months. Avoid advice; stick to observations.

Relay any `warnings` above the trend.")

    ("category-deep-dive" "Pick a category, get every transaction in it plus patterns — frequency, top merchants, largest, weekly cadence."
     "You have access to BankBridge MCP tools. The user wants to zoom in on ONE category.

`$ARGUMENTS` should be the category name (BankBridge uses uppercase underscore names like `FOOD_AND_DRINK`, `TRANSPORTATION`, `GENERAL_MERCHANDISE`). If the user typed a friendly name (\"food\", \"restaurants\", \"groceries\") map it to the closest category. If unclear, run `list_categories` first and ask which they meant.

Window: default last 90 days, or honor anything in `$ARGUMENTS` (\"last 6 months\", \"this year\", \"march\").

Steps:
1. Call `list_categories` if needed to confirm the category exists in the user's data.
2. Call `list_transactions` with `category: \"<CATEGORY>\"`, the resolved date range, `limit: 200`.
3. Aggregate: total, transaction count, average, median, largest, most frequent merchant, top 5 merchants.
4. Compute weekly cadence (transactions per week over the window).

Output:

```
🔎 <CATEGORY> — <window>

Total:        $ X
Count:        N
Average:      $ X.XX
Median:       $ X.XX
Largest:      $ X at <merchant> on <date>
Most frequent: <merchant> (N times)

Top 5 merchants
  <merchant>  $ X  (N)
  ...

Weekly cadence: N.N transactions / week

Full list (date — merchant — amount)
  <date>  <merchant>  $ X
  ...
```

If more than 50 transactions, show the top 25 by amount in the full list and note the total count.

Relay any `warnings` above the output.")

    ("category-pie" "Pie chart of spending by category. Writes spending-pie.png (or falls back to ASCII if no plotting library)."
     "You have access to BankBridge MCP tools. The user wants a visualization of their spending composition.

`$ARGUMENTS` optionally specifies a window (e.g. \"march\", \"last 30 days\", \"2026-q1\"). Default: last 30 days.

Steps:
1. Call `get_spending_summary` with `group_by: \"category\"` for the resolved window.
2. Keep the top 7 categories by spend; bucket the rest into \"Other\".
3. Render a pie chart via matplotlib (if available) or a fallback ASCII donut.

Matplotlib version:
- Sort wedges by size; use a muted palette (no neon).
- Label each wedge with category name + percentage.
- Title: \"Spending by category — <window>\"
- Save as `spending-pie.png` in the current working directory.
- Confirm the file was written.

ASCII fallback (if matplotlib not available):
```
     FOOD_AND_DRINK ███████████████████  34%  $ X
   TRANSPORTATION  ███████████          19%  $ X
RENT_AND_UTILITIES ████████              14%  $ X
   ENTERTAINMENT  ███████               12%  $ X
         GROCERIES ██████                10%  $ X
        GENERAL_MERCH ████                7%  $ X
            HEALTH ██                     3%  $ X
             Other █                      1%  $ X
```

Keep the bar glyph consistent. Widths proportional to percentage, rounded down.

Relay any `warnings` above the chart.")

    ("compare-months" "Side-by-side month comparison. Spot what changed, not just the totals."
     "You have access to BankBridge MCP tools. Compare two months of spending side by side, category-by-category.

`$ARGUMENTS` optionally specifies the two months as `\"2026-03 vs 2026-02\"` or just `\"2026-03\"` (which defaults to comparing against the month prior). If nothing is passed, default to this month vs last month.

Steps:
1. Call `get_monthly_cashflow` for each of the two months.
2. Call `get_spending_summary` with `group_by: \"category\"` for each of the two months.
3. Build a full union of categories across both months.
4. Compute deltas (absolute $ and %).

Output:

```
📊 <month A> vs <month B>

Cashflow
  Income     <A>  →  <B>   (±$X, ±Y%)
  Expenses   <A>  →  <B>   (±$X, ±Y%)
  Net        <A>  →  <B>   (±$X)

By category
| Category | <month A> | <month B> | Δ $  | Δ %   |
| -------- | --------: | --------: | ---: | ----: |
<rows, sorted by abs(Δ $) descending>

🚨 Callouts
- <category>: up X% (>$Y) — <brief note>
- <category>: new this month
- <category>: disappeared (was $Z last month)
```

For the callouts, only flag deltas >20% AND >$50 absolute — don't be noisy. Categories that appeared / disappeared are always worth a line.

Negative percentages use `−` prefix. Keep tables rendering-friendly.

Relay any `warnings` above the comparison.")

    ("dividends" "Dividends received — YTD breakdown by ticker, run rate, and upcoming expected dividends where knowable."
     "You have access to BankBridge MCP tools. The user wants to know what they've earned in dividends.

`$ARGUMENTS` is an optional year (e.g. `2025`). Default: current year.

Steps:
1. Call `list_investment_transactions` with `type: \"dividend\"` (or subtype that covers dividend-like income — check `list_categories` output if unsure of the exact filter), `start_date: \"<year>-01-01\"`, `end_date: \"<year>-12-31\"`, paginating until `has_more` is false.
2. Group by ticker. Sum per ticker.
3. Compute run rate (YTD total / months elapsed × 12).

Output:

```
💸 Dividends — <year>

YTD received:  $ X.XX
Run rate:      $ Y.YY / year

By ticker
| Ticker | Received | Payments | Avg/payment |
| ------ | -------: | -------: | ----------: |
<rows, sorted by received desc>

Recent payments
  <date>  <ticker>  $ X.XX
  ...
```

If the result is empty, say so plainly: \"No dividends recorded for <year>. Either no dividend-paying holdings, or your brokerage hasn't reported any yet.\"

Don't extrapolate unstated dividend income. Stick to what BankBridge returns.

Relay any `warnings` above the output.")

    ("duplicate-check" "Find duplicate or suspicious transactions — same merchant + same amount within 48 hours."
     "You have access to BankBridge MCP tools. The user wants you to look for duplicate charges or signs of fraud.

Steps:
1. Call `list_transactions` for the last 90 days (or `$ARGUMENTS` days if specified).
2. Group transactions by (merchant_name, absolute-value of amount). Flag any group with ≥2 transactions within 48 hours of each other.
3. Separately, flag any single transaction ≥$500 from a merchant the user has fewer than 3 total transactions with historically (call `get_merchant_history` per candidate to confirm). Those are statistical outliers worth checking.

Output:

```
# Possible duplicates or outliers

## Likely duplicates
| Merchant | Amount | Dates | Action |
| -------- | -----: | ----- | ------ |
<rows>

## Unusual big-ticket transactions
| Date | Merchant | Amount | Why flagged |
| ---- | -------- | -----: | ----------- |
<rows, e.g. \"first-ever transaction with this merchant\">

## All clean
<if both sections are empty, just say so>
```

Don't cry wolf — a weekly grocery store visit at the same $X isn't a duplicate. Only flag if the second charge is within 48 hours AND the merchant isn't one of the user's top 20 frequent merchants.

Relay any `warnings` from tool calls before the report.")

    ("fraud-check" "Broader suspicious-transaction finder — duplicates, unfamiliar merchants, unusual amounts, off-hours charges."
     "You have access to BankBridge MCP tools. The user wants a wider net than `/duplicate-check` — they're looking for anything suspicious.

`$ARGUMENTS` is an optional number of days to look back. Default: 30.

Steps:
1. Call `list_transactions` for the window.
2. Run FOUR checks in parallel:
   - **Duplicates**: same merchant + same amount within 48 hours.
   - **Amount outliers**: transactions > 3× the user's 90-day average transaction amount.
   - **Unfamiliar merchants**: transactions from merchants the user has <3 historical transactions with (use `get_merchant_history` per candidate to confirm).
   - **Off-hours**: transactions at 1am–5am local time, if timestamp resolution allows.

Output:

```
🛡 Suspicious activity — last N days

## Duplicates
<rows or \"none\">

## Amount outliers (>3× your typical transaction)
| Date | Merchant | Amount | Typical |
| ---- | -------- | -----: | ------: |
<rows>

## Unfamiliar merchants
| Date | Merchant | Amount | Prior history |
| ---- | -------- | -----: | ------------: |
<rows — \"first ever\" or \"N prior charges\" context>

## Off-hours (1am–5am)
<rows or omit section if empty>

## Verdict
<One sentence: \"No red flags.\" | \"Two items worth checking — see above.\" | \"Multiple anomalies — take a minute.\">
```

Tune sensitivity to avoid being the boy who cried wolf. A Tuesday $4.50 coffee shop repeat from a regular is NOT a duplicate even if the amount matches yesterday. Only include the \"unfamiliar merchants\" rows that are >$50 — sub-dollar prior-unknown charges are usually legit.

If BankBridge returns `warnings` about a connection needing reauth, surface that first — a stale feed is the most likely explanation for \"weird\" data.")

    ("monthly-chart" "Plot a bar chart of your income vs expenses for the last 6 months. Writes cashflow.png."
     "You have access to BankBridge MCP tools. The user wants a visualization of recent cashflow.

Steps:
1. Call `get_monthly_cashflow` for each of the last 6 months. Collect `{month, income, expenses, net}` tuples.
2. Write a Python script using matplotlib (or whatever plotting library is available in your environment) that:
   - Draws a grouped bar chart: green bars for income, red-ish bars for expenses, plotted side-by-side per month
   - Overlays a line for net (income − expenses)
   - Labels the x-axis with the months (`YYYY-MM`)
   - Formats the y-axis as dollars
   - Saves to `cashflow.png` in the current directory
3. Execute the script and confirm the file was written.

If the user specified a different number of months in `$ARGUMENTS`, use that. Defaults to 6.

Keep the chart minimalist — no gradients, no 3D, no decorative elements. Just clean bars + axis labels + a title: \"Cashflow — last N months\".

If matplotlib isn't available in the environment, fall back to a simple ASCII bar chart printed to stdout rather than failing.

Relay any tool `warnings` before plotting.")

    ("monthly-check" "Full monthly financial review — cashflow, top spending, subscriptions, investment delta."
     "You have access to BankBridge MCP tools. The user has asked for a monthly financial check.

If the user hasn't specified a month, use the current month. Accept input like \"March 2026\" or \"2026-03\" as `$ARGUMENTS`.

Build a one-page markdown report in this structure:

```
# Monthly check — <Month Year>

## Cashflow
<income, expenses, net, savings rate>

## Top 5 spending categories
<markdown table, biggest first>

## Top 10 merchants
<markdown table, biggest first>

## Recurring charges
<list every one with amount, frequency, total monthly impact>

## Investment delta (if any brokerage is connected)
<total value, gain/loss since last month if derivable, top mover>
```

Steps:
1. Call `get_monthly_cashflow` with `month: \"<YYYY-MM>\"` → top of the report
2. Call `get_spending_summary` with `group_by: \"category\"` → top 5 table
3. Call `get_spending_summary` with `group_by: \"merchant\"` → top 10 table
4. Call `get_recurring_charges` → the subscriptions section
5. Call `list_holdings` → if non-empty, add the investments section; otherwise omit

If any tool returns a `warnings` array, **relay each warning to the user verbatim at the top of your response** before the report. Never silently drop warnings.

Keep numbers in plain dollars with 2 decimals. Prefix negatives with `−`. No prose filler — the report is the deliverable.")

    ("monthly-report" "Full written markdown report for a given month — narrative, not just tables. Meant for sharing or archiving."
     "You have access to BankBridge MCP tools. The user wants a polished, narrative-style monthly report.

`$ARGUMENTS` is the month (e.g. \"2026-03\" or \"march 2026\"). Default: most recently completed month.

This is the longer, prose-friendly cousin of `/monthly-check`. Use it when someone wants to save a file or email it.

Steps:
1. Call `get_monthly_cashflow`, `get_spending_summary` (by category and by merchant), `get_recurring_charges`, and `list_holdings` (if connected).
2. Write a narrative report, not just tables.

Output structure:

```
# <Month Year> — Financial report

## The headline
<1–2 sentence summary of the month: profitable, tight, big purchase, etc. Lead with the most salient number.>

## Cashflow
<Income, expenses, net — as a table. Follow with 1 paragraph contextualizing: how this compared to typical, any outlier inflows/outflows.>

## Where the money went
<Top 5 categories as a table, followed by 2–3 sentences of narrative pointing out what stood out — a category that spiked, a category that was quiet, etc.>

## Top merchants
<Top 10 as a table. 1–2 sentences on any pattern (e.g. \"coffee spend was dominated by Blue Bottle, 14 visits\").>

## Recurring charges
<Total monthly recurring + list every recurring charge. 1 sentence on anything that changed vs last month.>

## Investments (if connected)
<Portfolio value, change over the month, top mover, any notable trades.>

## What to watch
<2–3 bullets of things the user might want to pay attention to next month. Not advice — observations: \"Food spend was 28% higher than February. Worth watching in April.\">
```

Tone: calm, factual, warm. Like a good CFO email. No hype, no emoji (save that for /year-in-review). Numbers get commas and 2-decimal formatting.

Offer to save to `report-<year>-<month>.md` if the user asks — don't save unless asked.

Relay any `warnings` above the report.")

    ("portfolio-health" "Portfolio health check — total value, gain/loss, concentration, and any single position > 20% of portfolio."
     "You have access to BankBridge MCP tools. The user wants to know if their portfolio is concentrated or diversified.

Steps:
1. Call `list_holdings`. If the result is empty or `warnings` say no investments connected, stop and tell the user to connect a brokerage account.
2. For each holding, compute: current value, cost basis, unrealized gain/loss ($ and %), and % of total portfolio.
3. Flag any position > 20% of the total portfolio as concentration risk.
4. Call `list_investment_transactions` with `limit: 20` to get recent activity.

Output:

```
📈 Portfolio health

Total value:      $ X
Total cost basis: $ Y
Unrealized P/L:   ±$ Z  (±N%)

Positions
| Ticker | Qty   | Value  | Cost   | Δ $    | Δ %   | % of portfolio |
| ------ | ----: | -----: | -----: | -----: | ----: | -------------: |
<rows, sorted by value desc>

⚠ Concentration risk (>20% of portfolio)
- <ticker>: X%  (consider diversifying)

Recent activity (last 20)
  <date>  <type>  <ticker>  qty <n>  $ <amount>
  ...
```

If no positions are >20%, replace the concentration section with a one-liner: \"Your most concentrated position is <ticker> at N% — within typical single-stock thresholds.\"

Don't give investment advice beyond pointing out concentration math. If the user asks \"should I sell?\", decline and suggest they speak with a fiduciary.

Relay any `warnings` above the report.")

    ("price-creep" "Find recurring charges whose price has crept up over the last 12 months. Spot Netflix hikes before your credit card does."
     "You have access to BankBridge MCP tools. The user wants to know which subscriptions have raised their prices.

Steps:
1. Call `get_recurring_charges` to get every detected recurring charge.
2. For each one, call `get_merchant_history` with a 12-month window.
3. Compare first-charge amount to most-recent-charge amount. Compute absolute delta and percentage.

Only report merchants where:
- Price increased by >5% AND >$1 absolute, OR
- Price dropped by >5% AND >$1 absolute (user deserves a high-five)

Output:

```
📈 Subscription price changes — last 12 months

Increases
| Merchant | Was    | Now    | Δ $    | Δ %   | First charge |
| -------- | -----: | -----: | -----: | ----: | ------------ |
<rows, sorted by Δ % desc>

Decreases (good news)
| Merchant | Was    | Now    | Δ $    | Δ %   |
| -------- | -----: | -----: | -----: | ----: |
<rows>

Stable
<N subscriptions unchanged — list names only>

Total incremental cost from increases: $ X / month  (~$ Y / year)
```

If no changes found, a one-liner is enough: \"Every recurring charge is stable over the last 12 months. 🎉\"

Don't conflate price changes with usage changes (going from a monthly to annual plan). If first and most-recent amounts differ by a plausible annual factor (11–13×), flag that as a cadence change rather than a hike.

Relay any `warnings` above the output.")

    ("spending-by" "Flexible group-by. \"/spending-by merchant last 90 days\" or \"/spending-by category march\" — group and slice however you want."
     "You have access to BankBridge MCP tools. Parse the user's free-form request in `$ARGUMENTS` for:

- **dimension** — one of `category`, `merchant`, `month`, `week`. Default: `category`.
- **window** — a date range. Accepts forms like \"last 30 days\", \"last 90 days\", \"march\", \"march 2026\", \"2026-03\", \"Q1 2026\", \"this year\". Default: last 30 days.

Steps:
1. Resolve the window to `start_date` + `end_date`.
2. Call `get_spending_summary` with the resolved params.
3. Render the result.

Output:

```
📊 Spending by <dimension> — <window>

| <dimension>          | Total  | Count | Avg   |
| -------------------- | -----: | ----: | ----: |
<rows, sorted by total desc, top 25>

Grand total: $ X over N transactions.
```

If the dimension is `month` or `week`, include a mini sparkline using unicode block characters (▁▂▃▄▅▆▇█) alongside the totals column so the user can eye the trend at a glance.

If `$ARGUMENTS` is ambiguous (e.g. \"march\" — which year?), pick the most recent occurrence and note the assumption in one line above the table.

Relay any `warnings` above the output.")

    ("subscriptions" "Audit every recurring charge you're paying for. Flag small-ticket items you might have forgotten."
     "You have access to BankBridge MCP tools. The user wants a complete subscription audit.

Steps:
1. Call `get_recurring_charges` to get every detected recurring charge.
2. For each one, call `get_merchant_history` to find when the first charge was and if the price has drifted.

Output in this shape:

```
# Subscription audit

## Active recurring charges — <total>/mo
| Merchant | Amount | Frequency | Started | Price trend |
| -------- | -----: | --------- | ------- | ----------- |
<sorted by amount desc>

## Potentially forgettable (<$20/mo and started >6 months ago)
<callout list — these are the ones to review>

## Price increases
<any merchant whose charge is >10% higher than its first charge>
```

Use \"stable\", \"+X%\", or \"−X%\" for the price-trend column based on first vs most recent charge.

Be direct about what the user could cancel — if a $9/mo charge from 18 months ago hasn't been used based on no related activity nearby, say \"review this — hasn't moved since <date>\". Don't be preachy.

Relay any `warnings` from tool calls before the audit.")

    ("tax-prep" "Export a year's worth of transactions to CSV, grouped by category. Useful for tax prep or bookkeeping."
     "You have access to BankBridge MCP tools. The user wants a year's worth of transaction data in CSV form.

`$ARGUMENTS` is the year (e.g. `2025`). If not provided, use the most recently completed calendar year.

Steps:
1. Call `list_transactions` with `start_date: \"<year>-01-01\"`, `end_date: \"<year>-12-31\"`, paginating if needed until `has_more` is false.
2. Write the results to `transactions-<year>.csv` with columns:
   `date,account_id,merchant,category,amount_cents,pending`
3. Also write `summary-<year>.csv` with columns:
   `category,total_cents,count`
   from `get_spending_summary` with `group_by: \"category\"` for the full year.

Output:
```
Wrote:
  transactions-<year>.csv  (N rows)
  summary-<year>.csv       (M categories)

Top 5 categories:
1. <cat>  $X
2. <cat>  $X
...
```

BankBridge's amount convention: **positive = expense, negative = income/refund**. Preserve that in the CSV — don't invert. If writing to a file isn't possible in the current environment, print the CSV content to stdout so the user can redirect it themselves.

Relay any `warnings` from tool calls before writing.")

    ("weekly-review" "Last 7 days at a glance — total spend, biggest charges, and anything new or unusual."
     "You have access to BankBridge MCP tools. The user wants a weekly pulse.

`$ARGUMENTS` optionally specifies a reference week. Default: the last 7 calendar days ending today.

Steps:
1. Call `list_transactions` with `start_date: \"<7 days ago>\"`, `end_date: \"<today>\"`, `limit: 100`.
2. Compute: total out (positive amounts), total in (negative amounts), net, largest single expense.
3. Group by `category` via `get_spending_summary` with the same date range.
4. Flag any single transaction > 2× this week's average (loose \"surprise\" heuristic).

Output:

```
📆 Week of <start> – <end>

Total spent:   $ X
Total in:      $ Y
Net:          ±$ Z

Biggest single charge: <merchant> — $ X (<date>)

By category
  <cat>  $ X (N transactions)
  ...

Surprises (>2× avg daily)
  <date> <merchant> $ X
  ...
```

Skip the \"Surprises\" section if there are none. Stay brief — this is a check-in, not a deep report.

Relay any `warnings` above the summary.")

    ("year-in-review" "Spotify Wrapped for your money — full-year cashflow, spending rank, top merchants, recurring tax, and a few fun stats."
     "You have access to BankBridge MCP tools. The user wants a full-year retrospective.

`$ARGUMENTS` is an optional year (e.g. `2025`). Default: most recently completed calendar year.

Steps:
1. Call `get_monthly_cashflow` for each month of the year. Sum income, expenses, net.
2. Call `get_spending_summary` with `group_by: \"category\"` for the full year.
3. Call `get_spending_summary` with `group_by: \"merchant\"` for the full year.
4. Call `get_recurring_charges`.
5. Call `list_holdings` and `list_investment_transactions` for the year (if any).

Output:

```
🎁 Year in review — <year>

## Cashflow
Total income:    $ X
Total expenses:  $ Y
Net:            ±$ Z
Best month:     <month> (+$X saved)
Worst month:    <month> (−$X)
Savings rate:   N%

## You spent the most on
1. <merchant>  $ X   (<N transactions>)
2. <merchant>  $ X
3. <merchant>  $ X
...
<top 10>

## By category
| Category | Total  | % of expenses |
| -------- | -----: | ------------: |
<rows, top 10>

## Subscription tax
You paid $ X / year in recurring charges:
  <merchant>  $ X / mo  (annual: $ Y)
  ...

## Fun stats
- Most expensive single day: <date> — $ X across N transactions
- Busiest spending day of week: <weekday> (avg $ X)
- Number of unique merchants: N
- Weirdest spend (single largest outlier): <merchant> $ X on <date>

## Investments (if connected)
Portfolio change: ±$ X  (±N%)
Dividends received: $ Y
Top mover: <ticker> (±N%)
```

Make it feel like a gift, not an audit. Be warm about the numbers. Don't shame; just report. Skip sections that don't apply (no investments → drop that section entirely).

Relay any `warnings` at the top.")
))
