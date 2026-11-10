# nodecode-charts

JP Charts: a bar, line, stacked or share chart drawn as a PNG that reads well on a phone, with every bar and printed number checked against the table before it is returned. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-charts](https://github.com/TakeshiTGAL/hermes-plugin-charts) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

## Get it

Tell the agent "add the charts cell", or run `nodecode add nc://charts`.

## Set it up

Nothing is required: the cell is on from the first message and draws at 1080x1350, in the language the data is in, into a charts folder under the agent's home. The `charts` section of the config refines that - `preset` (`mobile`, `square` or `wide`), `locale` (`auto`, `ja` or `en`), and `directory` for where a chart is written. `config.example.jsonc` is the section written out.

## Use it

Ask for the graph in words ("chart this by prefecture", 「これを使って棒グラフにして」) and the model calls `chart_render` with the table and a chart spec; the answer carries a Markdown image line, so the picture is in the reply. A model can also call the tools from `eval`.

- `chart_render` takes `data` - row objects, columns and rows, pasted CSV/TSV or a Markdown table, JSON text, or an absolute `.csv`/`.tsv`/`.txt`/`.json` path - and `chart_spec` with `chart_type`, `unit` and `source`. The unit and the source are always printed, so both are required.
- `chart_validate` reads a table and reports what it makes of it (categories, series, unit, sort, the first rows) without drawing anything.
- `chart_types` says what each type is for, the data formats and the limits: 8 series, 40 bar categories, 30 stacked, 15 share rows, 500 line points.
- `/charts` says what the cell draws; `/charts types` lists the four types; `(help :charts)` is the manual the model reads.

```
data:  { "text": "pref,pop\nTokyo,14178000\nOsaka,8780000" }
spec:  { "chart_type": "bar", "title": "Population", "unit": "people",
         "source": "Statistics Bureau" }
```

What comes back:

```
{"success": true, "path": ".../charts/chart-20261110-000614-5c55cc18.png",
 "markdown": "![Bar chart: Population (people). largest Tokyo; smallest Osaka; ...](.../chart-...png)",
 "alt_text": "...", "chart": {"chart_type": "bar", "size_px": [1080, 1350], ...},
 "verification": {"status": "ok", "checked": 6}}
```

The picture is read back before it is answered: every bar's drawn length is compared with the value it stands for, and every printed number is parsed back out of the image's own text, so a chart that does not match the table is deleted rather than returned. The refusals say where and what next - a missing unit, a pie chart (answered with a share band), a cell that is not a number (named by its row), a date out of order (sorted, and said so).

What a chart carries: bars from zero, a share band that is 100% wide, dates in time order, a bar sorted largest first, one shared scale and one rounding for its numbers, 万/億 (or K/M) compact labels for large numbers, color-blind-safe colors, a note for a top-N cut and for the marks a statistics table uses for no value, and the source under the plot.

Two limits worth knowing: the numbers are drawn in a five-by-seven dot face, so a label in Japanese sets as dots-sized capitals on the picture while `alt_text` carries the same words in the reply; and a chart is written under the agent's home (`charts/`) unless the section names a directory.
