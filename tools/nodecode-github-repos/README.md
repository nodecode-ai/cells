# nodecode-github-repos

GitHub Repos: list your GitHub repositories with their stars, forks, language and last push, and the totals across them. A Nodecode rewrite of [tomekdot/hermes-my-github-plugin](https://github.com/tomekdot/hermes-my-github-plugin) by [tomekdot](https://github.com/tomekdot).

The original is a Hermes Dashboard plugin: a statusbar chip, a palette command
and a tab that list the repositories of the account the `gh` CLI is signed in
as, with stats, search, a visibility filter and five sort orders, read from the
GitHub REST API through `gh api` and cached for a minute. This cell keeps what
the tab computes and gives it to the model and to the person typing, instead of
a window: the same endpoint, the same per-repository fields, the same sort
keys, the same filter, the same cache and the same 500-repository ceiling,
answered as text.

| Tool | What it does |
| --- | --- |
| `github_repos` | The list: a header, one line per repository (visibility, stars, forks, language, open issues, how long ago it last moved, description), and how many were cut. Narrowed by `query`, `visibility`, `sort`, `limit` and `account`. |
| `github_repo_stats` | The totals: public and private, forks and archived, stars and open issues across them, and the languages most used. |

There is also `/repos [query]`, which lists the same thing in the section's
sort, as text a person reads.

## Get it

Tell the agent "add the github-repos cell", or run `nodecode add nc://github-repos`.

## Set it up

Nothing to paste: the cell runs the `gh` you already have, signed in with
`gh auth login`, and holds no token of its own. Install it from
<https://cli.github.com/> if `gh` is missing; `gh auth status` says whether it
is signed in. Without that, every read comes back as a failed result carrying
gh's own words.

## Use it

Ask for the repositories, not for a request:

- "Which of my repositories have moved this week?"
- "My archived repositories, and how many there are."
- "Anything I own with `parser` in the name?"
- "Total up my repos: stars, open issues, what they are written in."
- "List the public repositories of another account."

`github_repos` answers up to `limit` rows of one read, sorted by last push,
stars, forks, creation or name; `query` matches a name, a description or a
language, and `visibility` keeps only public, private, fork or archived ones.
A list is read at most `max_pages` pages of 100 repositories, so an account
with more than that is only partly seen, and the answer says how many rows
were cut.

## The section

```jsonc
{
  "github-repos": {
    "gh": "gh",              // the GitHub CLI every read runs
    "account": "",           // empty lists the signed-in account's own repositories
    "sort": "pushed",        // pushed | stars | forks | created | name
    "limit": 40,             // rows one answer shows
    "cache_seconds": 60,     // a fetched list is reused this long; 0 reads gh every time
    "max_pages": 5,          // pages of 100 repositories one read follows
    "timeout_seconds": 30    // one gh call's ceiling
  }
}
```

Nothing in it is required once `gh` is signed in. A blank `gh` refuses the
start, naming the key.

## Left out

Everything that is drawing rather than computing: the statusbar chip, the
palette dialog, the dashboard tab and its table, the theme-aware CSS, the
dialog's search-as-you-type and its click-to-open in the default browser.
Left out with them: the desktop/dashboard split (a cell has no window), the
plugin's own `/api/plugins/...` routes, and its 403 "rate limit or missing
token" panel. The list is a tool's answer now, so it is read in the
conversation and quoted there rather than drawn.

## License

MIT. The original plugin is Copyright (c) 2026 tomekdot: see `LICENSE` and
`NOTICE`.
