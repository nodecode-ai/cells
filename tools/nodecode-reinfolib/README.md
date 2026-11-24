# nodecode-reinfolib

JP Reinfolib: quarterly Japanese real-estate transaction prices for one city, from the MLIT real estate information library. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-reinfolib](https://github.com/TakeshiTGAL/hermes-plugin-reinfolib) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

| Surface | What it does |
| --- | --- |
| `reinfolib_quarter` (tool) | The model reads one city code and one quarter: the row count, the median `TradePrice`, counts by type, and at most 20 rows. |
| `/reinfolib CITY YEAR QUARTER` (command) | The same answer, typed by you: `/reinfolib 13104 2015 2`. |
| `(help :reinfolib)` | The manual: the city-code rule, what a 404 means, and the attribution to keep. |

The data is the Ministry of Land, Infrastructure, Transport and Tourism's 「不動産取引価格情報」, read from its real estate information library ([reinfolib.mlit.go.jp](https://www.reinfolib.mlit.go.jp/), XIT001).

## Get it

Tell the agent "add the reinfolib cell", or run `nodecode add nc://reinfolib`.

## Set it up

The API wants a subscription key, free from MLIT's [API usage page](https://www.reinfolib.mlit.go.jp/help/apiManual/): register, and the key is shown with your account.

```jsonc
"reinfolib": {
  "api_key": "",                      // the key itself, or leave it empty and name a variable:
  "api_key_env": "REINFOLIB_API_KEY", // the variable that holds it
  "base_url": "",                     // empty means MLIT's own host; an origin only, no path
  "timeout_seconds": 30
}
```

Set `api_key` or `api_key_env`, not both. The cell refuses at start, naming the member, when neither is set, when both are, when the key has whitespace in it, or when `base_url` is not an origin (`https://www.reinfolib.mlit.go.jp`, or `http://` only for `127.0.0.1` and `localhost`).

## Use it

Ask for municipal transaction prices, or type the command:

```
/reinfolib 13104 2015 2
```

```
13104 · 2015年第2四半期 · 12 rows, median TradePrice 48500000
by type: 宅地(土地) 9 (median 32000000) · 中古マンション等 3 (median 63000000)
  1. 中古マンション等 · 東京都 新宿区 西新宿 · TradePrice 63000000 · Area 42 · UnitPrice 1500000
  ...
fetched 2026-11-08T05:00:00Z · 出典：国土交通省　不動産情報ライブラリ · https://www.reinfolib.mlit.go.jp/
「不動産取引価格情報」（国土交通省）をもとに件数と抜粋を作成
「このサービスは、国土交通省の不動産情報ライブラリのAPI機能を使用していますが、提供情報の最新性、正確性、完全性等が保証されたものではありません」
```

From eval, the same read as a plist:

```lisp
(reinfolib:quarter "13104" 2015 2)   ; => (:ok t :count 12 :median "48500000" ...)
(reinfolib:report-text (reinfolib:quarter "13104" 2015 2))
```

**The city is a code, not a name.** Five digits, the first two 01–47: 13104 is Shinjuku-ku. A designated city needs a ward code — 14101, not 14100 — and the cell says so rather than asking the API. Nothing looks names up. The year starts at 2005 (quarter 3), a quarter is 1 to 4, and a quarter whose first month is still ahead is refused.

**A 404 is no data.** HTTP 404 means no rows for that city, year and quarter: it is not zero rows, and it is not unchanged prices. Publication lags the quarter, so the latest started quarter is often 404. A weekly check — with the `cron` cell — reads the latest started quarter and the one before it, and says nothing when both are 404.

**Nothing compares one read with another**, so the answer never calls a row new and never calls a price unchanged. Keep MLIT's attribution line and its terms sentence beside any row you quote.

## What it sends, and to whom

One HTTPS `GET` to `https://www.reinfolib.mlit.go.jp/ex-api/external/XIT001?year=YYYY&quarter=Q&city=CCCCC&priceClassification=01&language=ja`, with the key in the `Ocp-Apim-Subscription-Key` header and nowhere else; the answer is scanned for the key's text before it is shown, and dropped if it appears. Nothing else of yours travels: no conversation, no files, no client details, and nothing is written to disk. Requests are spaced at least a second apart, one request stops after `timeout_seconds` without an answer, and nothing is retried. `base_url` sends the same request, and the same key, to a host you name instead.

## What the rewrite leaves out

The original is a Hermes plugin: a tool, a `/jp-reinfolib` command, a bundled skill and an optional Automation Blueprint that schedules a weekly check. This cell keeps the tool, a command and the procedure as `(help :reinfolib)`; the schedule is the `cron` cell's job, and the blueprint, its `[SILENT]` delivery rule and Hermes's `tool_search`/`tool_call` indirection are Hermes surfaces that do not exist here. The JSON payload (`ok`, `error_type`, `records`, `truncated` …) becomes the text above: same facts, one shape, no `statusCode` scaffolding. Redirects are followed by the shared HTTP client (up to five) where the original refused them, so a redirect is reported only when the last hop is one.

## Test

```sh
.github/test-cell.sh tools/nodecode-reinfolib
```

Thirteen tests: the lifecycle from the declaration, the tool through the real dispatch against a stubbed MLIT, the twenty-row cut, a 404, a refused key, an answer that echoes the key, a designated-city code refused before the wire, the command and its usage, and the argument rules the plugin applied.

## License

MIT. The tool, its argument rules and its answers are TakeshiTGAL's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
