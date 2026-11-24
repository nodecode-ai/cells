# nodecode-dike

Dike: checks every Chilean-law article citation in a legal brief against a
local, hash-identified statute pack. A Nodecode rewrite of
[Grizaceo/hermes-dike](https://github.com/Grizaceo/hermes-dike) by
[Grizaceo](https://github.com/Grizaceo).

Ask the model to check a sentence it is about to write, and the cell answers
citation by citation:

```
El artículo 1545 del Código Civil dispone que el deudor responde.
  articulo 1545  VERIFICADA   cl.norma.codigo_civil:art:1545

Los artículos 1545, 1546 y 1547 del Código Civil, y el artículo 4 de la Ley 19.880.
  articulo 1545  VERIFICADA
  articulo 1546  VERIFICADA
  articulo 1547  VERIFICADA
  articulo 4     FALLO        ley 19.880 no está en el corpus
```

It proves three things per citation and no more: that the statute is in the
pack, that the article is one of its articles, and that the pack's in-force
window for that article covers the date asked. It does not read today's
*Diario Oficial*, and it does not check that the statute says what the brief
claims it says. A statute named with no article is not verified at all, and an
article the pack lacks is not thereby absent from the law.

## Get it

Tell the agent "add the dike cell", or run `nodecode add nc://dike`.

## Set it up

Nothing is required: the pack rides with the cell. The section carries two
optional settings -- `as_of`, the date vigencia is judged at (empty is today),
and `max_bytes`, the longest brief (400,000 by default, as the original
capped it). Say `"enabled": false` to switch the cell off.

```jsonc
{
  "dike": {
    // YYYY-MM-DD; empty is today
    "as_of": "",
    // the longest brief, in UTF-8 bytes
    "max_bytes": 400000
  }
}
```

## Use it

The model calls the tool `dike_verificar_citas` with `{"texto": "..."}`, or
with `{"ruta": "brief.md"}` (a `.md` or `.txt` file). It answers one JSON
object, in the original plugin's Spanish vocabulary:

- `veredicto`: `TODAS_VERIFICADAS`, `CON_PROBLEMAS` or `SIN_CITAS`
- `detalles`: one row a citation -- `cita_raw`, `estado` (`VERIFICADA` or
  `FALLO`), `resolucion` (`RESOLVED`, `AMBIGUOUS`, `UNRESOLVABLE`), `vigencia`
  (`IN_FORCE` or null), `gid`, `fuente_url`, `fecha_recuperacion`,
  `texto_hash` (the SHA-256 of the article's text in the pack) and `motivo`
- `no_verificadas`: the citations it could not prove, as written
- `menciones_sin_articulo`: statutes the brief names with no article, which
  nothing here verifies
- `meta`: `as_of`, `fecha_constancia`, `pack_hash`, the counts

`/dike` followed by the text prints the same answer as lines. `(help :dike)`
carries the rules, and `(dike:corpus)` lists the pack.

What it reads:

- the article prefix: `art.`, `artículo`, `artículos`, `arts.`, with `n°`,
  `nº`, `número`, `núm.`, `nro.` before the number
- the whole enumeration: `arts. 1545, 1546 y 1547` is three citations
- the ordinal forms (`artículo primero`), the transitory form (`art. t. 5`,
  `artículo transitorio 5`), the square degree (`39° bis`), the Latin suffixes
  (`bis`, `ter`, `quáter`, `quinquies`, `sexies`, `septies`, `octies`,
  `nonies`) and the letter after a suffix (`152 quáter N`), which are
  different articles of the Código del Trabajo
- the statute a citation is read against: the one its sentence names -- `Código
  Civil`, `Código Penal`, `Código de Procedimiento Civil`, `Código Procesal
  Penal`, `Código de Comercio`, `Código del Trabajo`, `Código Orgánico de
  Tribunales`, `Código de Aguas`, `Código Tributario`, `Constitución`, `ley
  19.880`, `decreto ley 3.500`, `dfl 1`, and the short forms `cc`, `cpc`,
  `cpp`, `ct`, `cot`, `ccom`, `cpr`. A sentence that names none is
  `UNRESOLVABLE`, which is the safe direction: sub-detecting a citation would
  let an unverified one pass.

## The pack

The cell ships the original's `chile-base` snapshot, `af1422d66d1e6eee`: 25
statutes, 8,544 articles, each with the SHA-256 of its text, its in-force
window and its retrieval date, as two TSV tables under `data/` (964 KB). It is
a dated copy of Ley Chile -- Biblioteca del Congreso Nacional de Chile,
retrieved 2026-08-18, not today's law, and its terms are not the MIT License
that covers this code: see [NOTICE](NOTICE).

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-dike
```

## Not ported

The Motor/NIF studio, the human-in-the-loop gates, the content signing of a
commitment, the .docx and .pdf readers and the pack's JSON-LD graph all stay
in the original; [NOTICE](NOTICE) says what each was and why it was left.
