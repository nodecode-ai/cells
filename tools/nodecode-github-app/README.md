# nodecode-github-app

GitHub App: seven tools that read and write GitHub as one resolved identity — a GitHub App installation, a personal access token, or your own `gh` — with every write gated and every answer attributed. A Nodecode rewrite of [himanusia/hermes-github-app-plugin](https://github.com/himanusia/hermes-github-app-plugin) by [himanusia](https://github.com/himanusia).

The original gives Hermes a bot-first GitHub identity: a GitHub App signs a JWT, exchanges it for an installation token, and opens issues, comments, reviews and merges as the App, with attribution on every result and an approval prompt before a write. This cell keeps the identity, the seven verbs and the gate, and drops the Hermes approval hook in favour of the setting the original already had.

| Tool | What it does |
| --- | --- |
| `github_identity` | Who the cell acts as — the auth method, the actor every result is attributed to, and the repositories it reaches. |
| `github_list_issues` | The issues and pull requests of a repository: number, title, state, author, labels, and whether it is a pull request. |
| `github_get_issue` | One thread: number, title, state, author, body, labels, and its comments when asked. |
| `github_create_issue` | Open an issue with labels and assignees. Needs `allow_write_actions`. |
| `github_comment_issue` | Comment on an issue or a pull request. Needs `allow_write_actions`. |
| `github_review_pr` | Approve, request changes or comment on a pull request, with inline comments, against the head SHA you read. Needs `allow_write_actions`. |
| `github_merge_pr` | Merge a pull request — squash, merge or rebase — only while its head is still the SHA you read. Needs `allow_write_actions`. |

There is also `/github-app`: with no argument it says who the cell acts as and what it reaches; `/github-app o/r` lists that repository's open issues and pull requests.

## Get it

Tell the agent "add the github-app cell", or run `nodecode add nc://github-app`.

## Set it up

Two ways in, and the original's order is kept: an App wins over a token.

**A GitHub App** (the bot-first identity the original recommended). Create one under Settings → Developer settings → GitHub Apps, install it on the repositories it may reach, and download its private key. Then in `config.jsonc`:

```jsonc
"github_app": {
  "app_id": "123456",
  "installation_id": "45678901",
  "private_key_file": "~/.config/github-app.pem",
  "allow_write_actions": false
}
```

`private_key` takes the PEM itself instead, for a key that comes out of a secret store; `private_key_file` wins when both are set. `app_slug` names the App without asking GitHub; left empty it is read once from the API.

**A personal access token**, as yourself. Create one (classic, scope `repo`; or fine-grained with the repositories' issues and pull requests, read and — if the agent may act — write), then:

```jsonc
"github_app": {
  "token_env": "GITHUB_TOKEN",
  "allow_write_actions": false
}
```

`token_env` names the environment variable that holds it, `token_file` a file only you can read; set one, not both. With neither, and `gh_cli` on (the default), an already-signed-in `gh` CLI answers for its own host.

Optional:

| Setting | What it does |
| --- | --- |
| `allow_write_actions` | The gate. While false (the default) the four writing tools answer `write_actions_disabled` and send nothing. |
| `gh_cli` | Read a token from `gh auth token` when the section names none (true by default). |
| `api_url` | The REST API base, https only, for GitHub Enterprise Server (default `https://api.github.com`). |
| `timeout` | How long one GitHub request may take, in seconds (30 by default). |

The key is read once at start: a section naming no credential, or an App without its installation or key, refuses the start, and `/cells` says so.

## Use it

Ask "what is waiting on GitHub in o/r?", "open an issue about the flaky test in o/r", "review pull request 9 and approve it", "merge 9 if its head is still 3f9a1c7". The model calls the tools; writes answer `write_actions_disabled` until you turn on `allow_write_actions`.

A person can call the same functions from eval, in the `github-app` package:

```lisp
(github-app:run-slash "o/r" "s1")        ; what /github-app o/r answers
(nodecode-github-app:github-identity-info) ; who the cell acts as, from eval
```

`(help :github-app)` is the model's manual for the seven verbs, kept while the cell runs.

### What the rewrite leaves out

The original's `pre_tool_call` hook, which asked the operator to approve each write with the exact payload. A cell has no approval prompt of its own, so the gate is the original's own `allow_write_actions` setting, and the two dangerous writes carry the SHA they were read against: a review names `commit_id` and a merge names `expected_head_sha`, so neither can act on a head the model has not seen. Also gone: the Hermes profile `.env` fallback (a cell reads its section), the `/app` slug lookup when `app_slug` is set, and the once-per-call thread-local client bookkeeping.

## Credit and license

himanusia's [hermes-github-app-plugin](https://github.com/himanusia/hermes-github-app-plugin), commit 2adfd020f93a5d71a563e94c88738fb8e64335f8, MIT licensed. The tool names, arguments, answers and the write gate are the original author's, rewritten in Common Lisp: `NOTICE` says what was rewritten and what was left out, and `LICENSE` carries the original MIT text.
