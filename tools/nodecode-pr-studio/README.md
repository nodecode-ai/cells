# nodecode-pr-studio

PR Studio: review GitHub pull requests from the chat — the cross-repo inbox, one PR read whole, its diff and threads, then comments, replies and reviews. A Nodecode rewrite of [Murali-lns/hermes-pr-studio](https://github.com/Murali-lns/hermes-pr-studio) by [Murali-lns](https://github.com/Murali-lns).

The original is a pane beside the chat in Hermes Desktop: it runs the user's own
`gh` login, paints the inbox, the diff and the threads, and hands a review
comment to the agent in one click. This cell keeps what the pane computes and
gives it to the model instead of a window, through six tools and `/prs`.

| Tool | What it does |
| --- | --- |
| `pr_inbox` | Open pull requests grouped as *Needs your review*, *Your pull requests* and *Involves you* — all of GitHub, or one repository — one row each: repo, number, age, author, draft, review decision, title. |
| `pr_show` | One PR whole: size, branches, head, review decision, mergeable state, labels, checks with the failing ones named, description, commits, and the conversation with each review's state. |
| `pr_diff` | The unified diff, a churn line a file first, then the patch — cut at `max_chars`, so one file at a time when it is long. The line numbers a review comment takes are the new file's. |
| `pr_threads` | The review threads already on the PR: file, line, side, resolved or outdated, each comment, and the comment id a reply takes. |
| `pr_comment` | A comment on the conversation, or — with `reply_to` — a reply inside an existing thread. |
| `pr_review` | One review: comment, approve or request changes, with comments on lines of the diff. |

There is also `/prs [owner/name]`, which prints the inbox for a person.

## Get it

Tell the agent "add the pr-studio cell", or run `nodecode add nc://pr-studio`.

## Set it up

Nothing to configure: the cell runs the [`gh`](https://cli.github.com/) you
already have, in the account you already signed in with (`gh auth status` shows
it). It holds no token, reads none, and contacts nothing itself — `gh`
authenticates itself.

The `pr-studio` section has defaults worth knowing:

```jsonc
"pr-studio": {
  "gh": "gh",              // the program to run: its name on PATH, or a path
  "limit": 10,             // rows per inbox group
  "events": 25,            // conversation entries pr_show shows
  "max_chars": 30000,      // the most text one call answers
  "read_only": false,      // true refuses every comment, reply and review
  "timeout_seconds": 30    // one gh call's ceiling
}
```

## Use it

Ask about what is waiting on you:

- "What pull requests need my review?"
- "What is in my PR inbox for nodecode-ai/cells?"

Ask for a read before an opinion:

- "Read PR 42 in o/r and tell me if the retry loop is right."
- "Show me the diff of #42 one file at a time — start with src/app.lisp."
- "Which review threads on #42 are still open, and what did they say?"

Ask for the answer on GitHub, when you want it there:

- "Reply to the thread about the nil guard on #42 that it is guarded above."
- "Comment on #42 that we will take it after the release."
- "Submit the review: comment, one nit on src/app.lisp line 42, and approve."

Every write happens only when you ask, and the cell names what it posted. With
`read_only` set, every write is refused before `gh` runs. Run `/prs` to read
the inbox yourself, and `(help :pr-studio)` for the primer the model reads.
