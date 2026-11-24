# nodecode-caduceus

Caduceus: a private local vault for your DNA, Apple Health numbers and health notes, with four tools and a `/caduceus` command. A Nodecode rewrite of [electricsheephq/caduceus](https://github.com/electricsheephq/caduceus) by [electricsheephq](https://github.com/electricsheephq).

The vault is a folder of plain tab-separated files: what the cell knows is readable with any tool you already have. Nothing is uploaded, nothing dials, and the model sees only the slices a tool answer puts in the conversation. Information with limits, not medical advice.

## Get it

Tell the agent "add the caduceus cell", or run `nodecode add nc://caduceus`.

## Set it up

One required setting, the folder the vault lives in; the rest has defaults:

```jsonc
{
  "caduceus": {
    "data_dir": "~/health/caduceus",
    "privacy_level": "open",        // or "standard": sensitive categories stay hidden until unlocked
    "hidden_categories": [],       // never shown at any level
    "unlocked_categories": []      // sensitive categories shown at the standard level
  }
}
```

`standard` keeps Alzheimer's risk, cancer predisposition, carrier status, mental health and reproductive findings out of every tool answer until `unlocked_categories` names them. A category name nothing types is refused at start rather than silently hiding nothing.

Nothing here needs a key or a network: a `.zip` import uses the `unzip` command you already have, and everything else is read from disk.

## Use it

| Tool | What it answers |
| --- | --- |
| `caduceus_capture` | `op=import` a file you name by absolute path: 23andMe or AncestryDNA raw data (`.txt`), a Promethease report table (`.tsv`/`.zip`), an unzipped Apple Health `export.xml`, or a FHIR Bundle of Observations (`.json`). `op=medication` (only with `confirm=true` after you agree), `op=journal`, `op=status`, `op=settings`, `op=sources`, `op=brief` (a local HTML brief). |
| `caduceus_genome` | `op=lookup` a marker, `op=gene` a gene, `op=pgx` a drug (gene-drug evidence with each marker called, absent or off the chip), `op=derived` (APOE and the enzymes a chip can combine), `op=summary`, `op=filter`, `op=report` (a local HTML variant report). |
| `caduceus_trend` | `op=metrics`, `op=series` by day, week or month, `op=compare` before and after a date or a medication's start, `op=labs`, `op=check` (the latest day against your own 5th to 95th percentile). |
| `caduceus_search` | Words across DNA annotations, notes, medications and imported reports; `ref` fetches one record whole. |

`/caduceus` answers the vault's status, `/caduceus search WORDS` and `/caduceus pgx DRUG` answer the same questions in text. `(help :caduceus)` carries the same manual to the model.

Consumer chips test common variants: rare ones can be missed or miscalled, a phenotype from a chip is a guess from the common alleles, and a gene-drug answer is a conversation with a prescriber, never a dose.

## What is in the folder

| File | What it is |
| --- | --- |
| `cell.lisp` | The four tools, `/caduceus`, the section and the manual. |
| `genome.lisp` | Raw data parsing, the evidence pack, APOE and enzyme derivations, the gene-drug entries, both HTML reports. |
| `health.lisp` | The Apple Health and FHIR imports, and the trend arithmetic. |
| `store.lisp` | The vault files, the privacy policy and the category rules. |
| `contracts.lisp` | One declaration per tool that takes arguments. |
| `test/` | The offline tests and their four fixtures. |

Run `.github/test-cell.sh tools/nodecode-caduceus` from the cells repository.

## License

Apache-2.0. The plugin this rewrite comes from is Copyright (c) Electric Sheep: see `LICENSE` and `NOTICE`.
