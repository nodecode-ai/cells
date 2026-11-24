# nodecode-github-refs

GitHub references: a public issue or pull request, with its description and latest comments, as one model tool. A Nodecode rewrite of [EloquentBrush0x/hermes-context-refs](https://github.com/EloquentBrush0x/hermes-context-refs) by [EloquentBrush0x](https://github.com/EloquentBrush0x).

The original turns `@gh:owner/repo#123` in a Hermes message into that issue or
pull request attached to the turn. There is no `@`-reference seam in Nodecode,
so here the same block arrives as a tool call:

| Tool | What it does |
| --- | --- |
| `github_reference` | Reads a public GitHub issue or pull request and answers it as one block: title and url, state (merged, open, draft, closed with its reason), who opened it and when, the comment count, labels, assignees and milestone, the description, and the newest comments oldest first. A pull request also gets its branches, commits and change size. |

And `/gh owner/repo#123` for the operator, answered the same way.

## Get it

Tell the agent "add the github-refs cell", or run `nodecode add nc://github-refs`.

## Set it up

Nothing is required. The cell reads public repositories through GitHub's REST
API without a token, which is 60 requests an hour per IP address, shared by
everything on that address. Say `/gh owner/repo#123` to try it.

Three optional members, in `/settings` or in the `github-refs` section of
`config.jsonc`:

```jsonc
{ "github-refs": { "max_chars": 6000, "max_comments": 10, "timeout_seconds": 10 } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `max_chars` | `6000` | the longest description attached per reference (200 to 50000); longer text is cut at a word boundary |
| `max_comments` | `10` | how many of the newest comments arrive (0 to 50; `0` attaches none) |
| `timeout_seconds` | `10` | how long one reference, its follow-up requests included, may wait (1 to 30) |

A member of the wrong shape refuses the start and says which one; the tool is
not offered until it is right. Then `(restart-cells "nodecode-github-refs")`.

## Use it

Ask about an issue or a pull request by name: "what does octo/tools#42 say, and
what did they answer?" The model calls `github_reference` and reads the block.
A private repository, a number that does not exist, or a used-up anonymous
limit all come back as a refusal naming the reason.

From eval, the same call:

```lisp
(gh-ref:reference "octo/tools#42")                                  ; an issue or a pull request
(gh-ref:reference "https://github.com/octo/tools/pull/77")           ; a copied url
(gh-ref:reference "octo/tools#42" :max-comments 50 :max-chars 20000) ; more of it
```

`(help :gh-ref)` answers the same guidance the model reads; `*primer*` is the
text, so a layer can reword it.

What it does not bring: the diff, review comments on lines, or timeline events
(commits, label changes). What it does bring is text anyone who can comment on
the repository wrote, so the block ends by saying it is quoted reference
material, not instructions.
