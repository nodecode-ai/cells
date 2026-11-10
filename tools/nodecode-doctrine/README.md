# nodecode-doctrine

Axiom Doctrine: nine articles of agent conduct in every request, and which enforcer cells are installed. A Nodecode rewrite of [percyhendrux123-prog/hermes-axiom-agent](https://github.com/percyhendrux123-prog/hermes-axiom-agent) by [percyhendrux123-prog](https://github.com/percyhendrux123-prog).

The doctrine answers one question about an agent with tools: who said it could
act. It is nine short articles -- one owner and one operator, what counts as an
order, the four actions that need a yes (send, spend, spin, scrub), proof over
claims, earned autonomy, a record of every refusal, a handoff before a long
session loses its footing, outbound messages that follow the owner's rules, and
secrets never read aloud. The text is the original plugin's own file, shipped
here unchanged as `doctrine.md` (public domain, as that repository dedicates
it), and it rides every request as a standing harness section.

This cell enforces nothing. It says the rules, and it says which of them have a
cell of this harness behind them and which are advice only.

## Get it

Tell the agent "add the doctrine cell", or run `nodecode add nc://doctrine`.

## Set it up

Nothing is required: the nine articles ship with the folder, and there is no
key, no service and no network call. The section says how the doctrine rides a
request.

```jsonc
{
  "doctrine": {
    "form": "compact",       // compact: the axiom, each article's opening sentence and its state
                             // full: the whole text and one install-state line an article
    "inject": true,          // false: leave the request alone and keep /doctrine
    "additions": "",         // a file of the owner's own rules; empty reads doctrine.md in this home
    "cells_dir": ""          // where installed cells sit; empty reads this home's cells folder
  }
}
```

A `form` outside the two, or a `cells_dir` that is not a path, refuses the
start and names the member; `(restart-cells "nodecode-doctrine")` after the
fix tries again. The compact form is capped at 1,800 characters and the full
one at 4,000, and the owner's additions carry a cap of their own at 800.

## Use it

The model reads the doctrine without being asked, because it is in the
standing block every request carries. The command is for the operator, and for
the model when it wants to quote an article:

```text
/doctrine                 each article, the enforcer it names, and its state here
/doctrine show            the whole doctrine, then the owner's additions
/doctrine show 3          one article, by number or roman numeral (III works too)
/doctrine plugins         every enforcer the doctrine names, the cell that carries it, and whether it is installed
```

`ENFORCED` is an install state, not a proof: an article reads ENFORCED when a
cell carrying one of the enforcers its own `*Enforced by:*` line names sits in
this home's cells folder, and ADVISED otherwise. The doctrine's enforcers were
separate Hermes plugins; here they are separate cells, and the map is in
`doctrine.lisp`: `hermes-axioma` is `nodecode-axioma`, `tripwire` is
`nodecode-tripwire` and `handydandy` is `nodecode-handydandy`. No cell carries
`standing` or `hermes-gatekeeper` yet, so articles IV, V and VIII always read
ADVISED, and the report says so on its own line.

The same answers from eval, as strings:

```lisp
(doctrine:status-text)
(doctrine:show-text "IV")
(doctrine:plugins-text)
```

`(help :doctrine)` answers the manual: what the nine articles are, and what
each verb does.

### The owner's own additions

The owner may add rules of their own in a markdown file, which rides after the
nine articles under the label `Owner additions:`:

```markdown
## X. Local review
Show evidence before sharing a result.
```

By default that file is `doctrine.md` in this home; `additions` names another
one. The cell never creates it, and a missing or empty file is fine. Keep
secrets out of it: the text goes to the model with every request.

### What it reads

Two things: `doctrine.md` beside this cell's source, and the owner's additions
file. It also lists the names of the folders in the home's cells folder, which
is the whole of its install check -- it never imports, starts or configures
another cell. Nothing is written, nothing is dialled, and no subprocess runs.

## Test it

```sh
.github/test-cell.sh tools/nodecode-doctrine
```

Twelve tests, offline: the lifecycle of the section hook, the command and the
manual topic; the articles parsed out of the shipped file and a `form` outside
the two refusing the start; the compact and full forms, and a stopped cell
leaving the request alone; the report over a scratch cells folder with and
without an installed enforcer cell; each verb of the command and its usage
line; and the owner's additions under their cap.
