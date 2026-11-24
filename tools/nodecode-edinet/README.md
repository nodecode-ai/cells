# nodecode-edinet

EDINET Filings: search Japan's corporate disclosures and read their headline financials. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-edinet](https://github.com/TakeshiTGAL/hermes-plugin-edinet) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

EDINET is the Financial Services Agency of Japan's disclosure system: every listed company's 有価証券報告書 (annual securities report) and the rest of what it files. The cell gives the model three tools over EDINET API v2, so an answer comes with a filing page to cite rather than a web page to trust.

## Get it

Tell the agent "add the edinet cell", or run `nodecode add nc://edinet`.

## Set it up

The API key is free. Open FSA's [EDINET API materials page](https://disclosure2dl.edinet-fsa.go.jp/guide/static/disclosure/WEEK0060.html), follow the API specification to get a key, then paste it in `/settings` under `edinet` → `api_key`, or name it in `api_key_env` (`EDINET_API_KEY` is read either way). `/edinet` alone says whether the key came through.

The other settings have usable defaults: `base_url` is EDINET's own endpoint, `timeout_seconds` bounds one request, `max_calendar_days` (31 at most) the widest file-date range one search may cover, and `pace_seconds` the gap between two list requests, which EDINET's terms ask a client to keep.

## Use it

Ask for a company's latest annual report, its revenue and total assets, or the filings of a given week:

- `edinet_search_filings` — by `sec_code` (7203 or 72030), `edinet_code`, `corporate_number` or part of `company_name`, over `date_from`/`date_to`; `doc_types` narrows the kind. One line per filing: doc id, securities code, filer, document kind, submission time, and whether it carries a PDF or a CSV.
- `edinet_financials` — one filing's revenue, operating income, net income, total assets, net assets and operating cash flow, with the unit and the accounting standard (Japan GAAP, IFRS, US GAAP). A figure no consolidated current-year row carries is reported missing, never guessed.
- `edinet_save_document` — the PDF (type 2), the submission ZIP (type 1) or the CSV ZIP (type 5), saved under `~/.nodecode/edinet/docs/`.

The operator can run one search from the keyboard: `/edinet 7203 [YYYY-MM-DD [YYYY-MM-DD]]`.

Two habits the toolset rewards: search by securities code rather than a name (a short name matches group companies), and give a file-date range around when the document is filed — Japanese annual reports cluster in late June. The list API is by file date, not by fiscal period, and a range longer than `max_calendar_days` is refused rather than shortened.

## What is in it

| File | What it is |
| --- | --- |
| `wire.lisp` | The one network seam: the list GET and the document GET, both under `/api/v2` on api.edinet-fsa.go.jp, with the key in `Subscription-Key` and the key scrubbed from every refusal. |
| `csv.lisp` | A ZIP's central directory read directly and its member inflated with CHIPZ, the UTF-16 TSV decoded here, and the headline rows picked by the plugin's own taxonomy element ids. |
| `filings.lisp` | File dates (Japan time), the local filters, the per-day search and its answer. |
| `verbs.lisp` | The three tools' contracts and what each answers; the save path under the Nodecode home. |
| `cell.lisp` | The section, the manual topic, `/edinet`, and the start that refuses a section with no key. |

`(help :edinet)` carries the guidance the plugin shipped as a skill.

## Test

```sh
.github/test-cell.sh tools/nodecode-edinet
```

Every request is a stubbed `nlk:http`: the tests build a file date's list and a CSV ZIP in Lisp, so nothing reaches EDINET and no key is needed.

## License

MIT. The original is Copyright (c) 2026 TakeshiTGAL: see `LICENSE` and `NOTICE`.
