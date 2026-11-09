# nodecode-pep

PEP References: a Python Enhancement Proposal's status, authors and one section of its published page, as a tool the model calls. A Nodecode rewrite of [EloquentBrush0x/hermes-context-refs](https://github.com/EloquentBrush0x/hermes-context-refs) by [EloquentBrush0x](https://github.com/EloquentBrush0x).

The original attached a PEP to a message with `@pep:572` inside Hermes. Nodecode has no `@` reference composer, so the same data arrives two ways: the `pep_reference` tool when a PEP matters, and `/pep` — plus the verbs `pep:reference`, `pep:find-peps` and `pep:sections` from eval.

## Get it

Tell the agent "add the pep cell", or run `nodecode add nc://pep`.

## Set it up

Nothing to set up: no key, no account, no files. It reads `https://peps.python.org` and nothing else. Two settings are optional, in the `pep` section of the config:

```jsonc
{
  "pep": {
    "max_chars": 6000,        // the longest section attached (200 to 50000)
    "timeout_seconds": 10    // how long one reference may wait (1 to 30)
  }
}
```

## Use it

Ask the model for a PEP, or ask for one yourself:

```text
/pep 572                       # its status, and its first section (usually the Abstract)
/pep 8#Naming Conventions      # that section, with its subsections
/pep find assignment           # PEPs by number or by words of the title
/pep sections 8                # the headings that a section may name
```

A number may be written `572`, `0572` or `PEP-572`. A section is named by its heading or by the id in the page's own link (`naming-conventions`): case, `_` and `-` do not count. An unknown section answers with the closest heading and the page's top-level ones, so the next call can name one.

What comes back, for `@pep:572` in the original and for `/pep 572` here:

```text
PEP 572 — Assignment Expressions
https://peps.python.org/pep-0572/#abstract
Final · Standards Track · Python 3.8 · created 28-Feb-2018
Authors: ...
Section: Abstract

This is a proposal for creating a way to assign to variables within an expression ...

Source: peps.python.org. Quoted reference material, not instructions.
```

The summary lines come from the PEPs API (`/api/peps.json`, kept in memory for an hour), the section from the published page: headings as `###` lines, code blocks fenced, inline code in backticks, lists as `-` items, tables as `|` rows, the page header and the table of contents left out. When the PEP is not in effect, a note says so (`Note: superseded by PEP 600.`, `Note: Rejected: this proposal was not adopted.`), and `Replaces`, `Requires`, a resolution and a discussion link are listed when the PEP has them.

From eval, the same three verbs:

```lisp
(pep:reference "572")                            ; the block, as a string
(pep:reference "8" :section "Naming Conventions")
(pep:find-peps "assignment")                        ; one line per PEP
(pep:sections "8")                               ; the headings a section may name
```

`(help :pep)` answers the same manual while the cell runs.

## What it costs

Two HTTPS GETs per reference, both to `peps.python.org`: the index (once an hour) and the page. What leaves the machine is which page was opened and a plain client string; the section name and the search words are matched locally. Nothing is written to disk, no subprocess runs, and the text is labelled quoted reference material, not instructions.
