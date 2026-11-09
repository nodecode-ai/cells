# nodecode-freight

Freight Reconciliation: carrier bills reconciled against trip ledgers and contracted rates, locally and deterministically. A Nodecode rewrite of [harrylabsj/hermes-plugin-freight-reconciliation](https://github.com/harrylabsj/hermes-plugin-freight-reconciliation) by [harrylabsj](https://github.com/harrylabsj).

A freight bill is paid line by line, and every line has an arithmetic answer:
the rate the contract sets for that route and vehicle, the waiting blocks a
gateway log proves, or the toll receipt itself. This cell keeps a case — one
carrier, one currency, one billing period — and works that arithmetic on the
operator's own machine.

The model imports the files, runs the pass and reads the difference list. It
never approves a payment, writes to an ERP or sends a message: a difference
list is evidence for a person, and the person confirms.

## Get it

Tell the agent "add the freight cell", or run:

```sh
nodecode add nc://nodecode-freight
```

## Set it up

The cell needs one thing: a folder for its case library, on a local disk.

```jsonc
"freight": {
  "data_dir": "freight",       // where cases live: one JSON file a case
  "currency": "CNY",            // a case's currency when the tool names none
  "page_size": 20,              // differences one page carries
  "max_rows": 20000             // data rows one imported file may carry
}
```

`data_dir` is required and the start refuses without it: nothing is written
anywhere the operator did not name. No key, no network.

## Use it

From a session, through eval:

```lisp
(freight:open-case "november" :carrier "C-01"
                   :period-from "2026-11-01T00:00:00+08:00"
                   :period-to "2026-12-01T00:00:00+08:00")
(freight:load-source "november" "bill" "~/freight-input/bills.csv")
(freight:load-source "november" "trips" "~/freight-input/trips.csv")
(freight:load-source "november" "rates" "~/freight-input/rates.csv")
(freight:reconcile "november")          ; the summary
(freight:differences "november")        ; the difference list, a page at a time
(freight:export-review "november")      ; the list as a CSV under exports/
(freight:cases)                         ; what the library holds
```

The model has the same five verbs as tools: `freight_case`, `freight_import`,
`freight_run`, `freight_differences`, `freight_export`. `/freight` shows the
library and runs one case, and `(help :freight)` carries the manual.

### The files it reads

CSV, a header row with these column names; other columns are ignored, and a
column the kind needs but the file lacks is refused by name.

| kind | columns |
| --- | --- |
| `bill` | `line_id`, `carrier_id`, `trip_ref`, `service_leg_id`, `charge_code` (`BASE`, `WAIT`, `TOLL`), `occurrence_id`, `currency`, `amount_basis`, `amount`, `reversal_of` |
| `trips` | `trip_id`, `carrier_id`, `route_id`, `vehicle_class`, `execution_status`, `departed_at` |
| `rates` | `rule_id`, `version`, `carrier_id`, `route_id`, `vehicle_class`, `charge_code`, `currency`, `amount_basis`, `effective_from`, `effective_to`, `formula` (`PER_TRIP`, `WAIT_BLOCKS`, `ACTUAL_RECEIPT`), `rate`, `free_seconds`, `block_seconds`, `cap`, `confirmed` |
| `waiting` | `trip_id`, `occurrence_id`, `elapsed_seconds`, `verified` |
| `receipts` | `trip_id`, `occurrence_id`, `amount`, `verified` |

Amounts are decimal strings with at most two places; every total inside is an
integer in minor units, so no float ever reaches a sum. Times are ISO with an
explicit timezone; a billing period spans two, so a time without one is
refused.

### What the arithmetic is

- **Match.** A bill line joins the trip whose `trip_id` its `trip_ref` names,
  when the carrier is the same. An unknown ref is `UNMATCHED`; a ref whose
  trip belongs to another carrier is `CARRIER_CONFLICT`, never a match.
- **Rate.** One confirmed rule must be in force at the trip's departure for
  that carrier, route, vehicle class and charge code: none is `RATE_NOT_FOUND`,
  two is `RATE_AMBIGUOUS`.
- **Compute.** `BASE` is the per-trip rate; `WAIT` takes the free seconds off
  first, rounds the rest up to whole blocks and multiplies; `TOLL` is the sum
  of the verified receipts, capped when the rule sets a cap.
- **Compare.** The difference is billed minus expected, and each group says
  `AMOUNT_HIGH` or `AMOUNT_LOW`, `DUPLICATE_SUSPECT` when two positive lines
  share a group, `MISSING_RECEIPT` or `WAIT_EVIDENCE_INVALID` when the evidence
  is not there, `MISSING_POD` when no delivery evidence was indexed,
  `REVERSAL_INVALID` for a credit that names no positive original.
- **Conserve.** Comparable, unresolved and out-of-scope bill amounts add up to
  the bill total; the differences add up to the net. A case with no rate in
  force keeps `expected` **unknown**, never zero.
- **The other direction.** An executed trip inside the period that no bill line
  names is listed as `NOT_BILLED`.

The same imported files answer the same summary every time: a run stores beside
each source the digest of the file it came from, and reads nothing else. A case
is one JSON file, `cases/<name>.json` under `data_dir`, mode 0600; exports are
CSVs under `exports/`.

## Credit

Rewritten from harrylabsj's Hermes Agent plugin `hermes-plugin-freight-reconciliation`, MIT. See [NOTICE](NOTICE): what was kept, and what was left out.

Run `.github/test-cell.sh tools/nodecode-freight` from the cells repository.
