# nodecode-fashion-design

Fashion Design Toolkit: drafting verbs for briefs, tech packs, size sheets, bills of materials, fabric yardage, costing, construction guides, colour palettes, measurement charts and collection plans. A Nodecode rewrite of [priticolours/fashion-design](https://github.com/priticolours/fashion-design) by [priticolours](https://github.com/priticolours).

Ten verbs, each a tool the model calls and a name `/fashion-design` runs. They
answer JSON built from the studio's own tables — standard body measurements,
grading increments, points of measure per garment, the yardage a garment takes,
four seasonal colour stories, construction steps, a standard bill of materials —
and nothing here reaches the network or needs a key.

## Get it

Tell the agent "add the fashion-design cell", or run `nodecode add nc://fashion-design`.

## Set it up

Nothing is required and no key is needed. Its section in `config.jsonc` (or
`/settings`) is optional:

| Setting | Default | What it is |
| --- | --- | --- |
| `units` | `in` | `in` or `cm`: the unit of the measurements the cell computes |
| `currency` | `USD` | the label on the money in the bill of materials and the cost sheet |
| `waste_percent` | 12 | cutting waste the fabric calculator adds |
| `retail_markup_percent` | 250 | retail as a percentage of the unit cost |

## Use it

Ask for what you need — "draft a tech pack for a linen shirt", "how much fabric
for 40 dresses in size L", "what does this cost per unit at 8 for fabric, 5 for
labour and 2 for trims" — and the model calls the matching tool:

| Tool | What it answers |
| --- | --- |
| `design_brief` | silhouette, colour palette, fabrics and key details from a concept |
| `tech_pack` | the pack, with a measurement table over the garment's points of measure |
| `size_sheet` | one graded row per size from a base size |
| `bom` | seven components with quantity, unit, supplier, cost, and the total per unit |
| `fabric_calculator` | yards per garment and for the run, cutting waste included |
| `cost_sheet` | unit cost with 10 % overhead, the run's total, and a retail price |
| `construction_guide` | numbered steps, a tip for the skill level, a rough time |
| `color_palette` | a five-colour story with hex codes and Pantone references |
| `measurement_chart` | standard body measurements for one size |
| `collection_plan` | the pieces in a balanced order, with the colour and fabric story |

The same ten run from the command line of the shell, one verb and its one
argument:

```
/fashion-design tech-pack jacket
/fashion-design fabric-calculator dress
/fashion-design measurement-chart L
```

A bare `/fashion-design` lists them. From eval the verbs are plain functions:
`(fashion-design:measurement-chart (fashion-design::measurement-chart-call "M"))`,
and `(help :fashion-design)` has the manual.

Run its tests with `.github/test-cell.sh tools/nodecode-fashion-design` from the
cells repository.
