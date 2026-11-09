# nodecode-aurora

Aurora Design System: page markup looked up offline, and a page checked against the census. A Nodecode rewrite of [Ntizar/hermes-aurora](https://github.com/Ntizar/hermes-aurora) by [Ntizar](https://github.com/Ntizar).

Aurora is a pure CSS design system: no build step, no JavaScript, every public
class namespaced `.nz-`. This cell carries its catalogue the way the original
plugin did -- a snapshot of the design system's own `components.json`, shipped
in `data/censo.json` -- and gives the model two tools over it: one that answers
what a need is called, one that measures a page before it is delivered.

## Get it

Tell the agent "add the aurora cell", or run `nodecode add nc://aurora`.

## Set it up

Nothing is required: the census ships with the folder, and there is no key, no
service and no network call. The section only says how the tools behave.

```jsonc
{ "aurora": { "limit": 5, "channel": "v10", "require_footer": true } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `limit` | `5` | families a lookup answers with when the call names none (1 to 20) |
| `channel` | `v10` | the channel or pinned tag a page links (`v10`, or a release such as `v10.10.0`) |
| `footer` | `Hecho con <3 por David Antizar` | the attribution a page must carry, verbatim; the design system writes a heart where `<3` stands |
| `require_footer` | `true` | whether a page without the attribution fails the check |

A limit under 1 refuses the start and names the member; set it in `/settings`
and `(restart-cells "nodecode-aurora")`.

## Use it

The model does the work in three steps: search the object, write the page from
the markup the search returned, then check the page.

```
aurora_search("tabla que se apila en movil")
aurora_search("kpi", limit: 2, with_markup: false)
aurora_validate(path: "site/panel.html")
aurora_validate(html: "<html lang=\"es\" ...>")
```

`aurora_search` answers, one block a family, the family name, the pack its
classes live in, the demo page, the classes, what not to use, the terms that
matched and the canonical markup from the catalogue. The same lookup from
eval, and from the command line:

```lisp
(aurora:lookup "aviso de error")                    ; the section's limit
(aurora:lookup "chat" :limit 3 :with-markup nil)    ; a call's own parameters
(aurora:census-info)                                ; system, version, channel, counts
```

`/aurora table that stacks on mobile` runs the same search.

`aurora_validate` fails a page for an unknown `nz-` class, a pack a used class
lives in but the page does not link, a gradient, a `backdrop-filter`, an
`!important`, a hand-written colour in the page's own `<style>`, an emoji in
the UI, more than one `<h1>`, and a missing attribution footer; it only warns
about a missing `lang`, a missing `data-nz-theme`, a `<body>` without `class="nz"`
and a `max-width` breakpoint. The same check from eval, as values:

```lisp
(multiple-value-bind (ok failures warnings measured)
    (aurora:check page :footer (aurora::default-footer))
  (list ok failures warnings))
```

`(help :aurora)` answers the doctrine the original's four skills carried: the
namespace, tokens only, no gradients, BEM, mobile-first breakpoints, the
typography floor, the footer, and the order of work. `*primer*` is that text,
so a layer can reword it.

The cell is offline and read-only: `data/censo.json` is the only file it
opens, a page it is handed is the only other thing it reads, and it writes
nothing.

Run the cell's tests from the cells repository:

```sh
.github/test-cell.sh tools/nodecode-aurora
```
