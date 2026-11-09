# nodecode-github-mentions

GitHub Mentions: your unread GitHub @mentions become tasks the agent reads and answers in the thread. A Nodecode rewrite of [luantak/hermes-github-mentions](https://github.com/luantak/hermes-github-mentions) by [luantak](https://github.com/luantak).

The original gives a Hermes bot account a job from a GitHub mention: it polls the bot's notifications, accepts the mentions of the people you approved, and has the agent reply where it was mentioned. This cell keeps the rules and the answer, and drops the poller: the model calls the tools when you ask, and GitHub's own unread notifications are the queue.

| Tool | What it does |
| --- | --- |
| `github_mentions` | The unread notifications that mention your login and whose author you approved: repository, thread, author, URL, the paragraph that named you, and the comment itself. |
| `github_mention_thread` | The conversation around one mention: title, body, state and the most recent comments. |
| `github_mention_reply` | Answers in the mention's own thread — a comment, an inline reply in a review thread, a discussion comment — and marks the notification read. An empty body just marks it read. |

There is also `/mentions`, which lists what awaits an answer, as text a person reads, with an optional repository: `/mentions o/r`.

## Get it

Tell the agent "add the github-mentions cell", or run `nodecode add nc://github-mentions`.

## Set it up

The cell speaks to api.github.com with a token of your own GitHub user, and with nothing else. Create one:

- **Classic token**: Settings → Developer settings → Personal access tokens → Tokens (classic), scope `repo`.
- **Fine-grained token**: Issues, Pull requests and Discussions (read and write) on the repositories it may reach, plus Notifications (read).

Then, in `/settings` (or in the `github_mentions` section of `config.jsonc`):

```jsonc
"github_mentions": {
  "token_env": "GITHUB_TOKEN",              // or "token_file": "~/.config/github-token"
  "allowed_authors": ["your-login"]        // the gate: whose @mentions count
}
```

A token is named, never written into the config: `token_env` holds the name of the environment variable that carries it (export `GITHUB_TOKEN` in the shell that starts Nodecode), `token_file` a file only you can read. Set exactly one. `allowed_authors` is required and never empty: an @mention from anyone else is data, not a task.

Optional:

| Setting | What it does |
| --- | --- |
| `max_mentions` | The most mentions one `github_mentions` call answers (10 by default). |
| `body_chars` | How much of a mention's body one answer carries (2,000 by default). |
| `timeout` | How long one GitHub request may take, in seconds (30 by default). |

## Use it

Ask "what @mentions are waiting on GitHub?", "anything in o/r that needs an answer?", "answer alice's mention on the parser bug". The model calls `github_mentions`, reads the thread if it needs the history, and answers with `github_mention_reply`; the reply is posted as your GitHub user and the notification is marked read, so the next list is shorter.

A person can call the same functions from eval, in the `github-mentions` package:

```lisp
(github-mentions:list-mentions (nlk:make-json-object "repo" "o/r"))
```

`(help :github-mentions)` is the model's manual for the three verbs, kept while the cell runs.

### What counts as a mention

The rules are the original plugin's:

- **Unread, and reason `mention`.** GitHub's notification becomes the task; it stays listed until it is answered or dismissed, so a restart loses nothing and a second agent on the same account sees the same queue.
- **An approved author.** Only the logins in `allowed_authors` can give the agent work, and a mention by anyone else is counted as `unresolved`, never acted on.
- **Your own @login, with boundaries.** The mention must be the token's login, case-insensitively, not glued to another word or handle.
- **The nearest item wins.** The cell reads the thread the notification names — the body, the conversation's comments and, on a pull request, its review comments and reviews — and answers the newest item by an approved author that mentions you near the notification's own time.
- **The answer goes where the mention was.** An inline review comment gets an inline reply in the same review thread; an issue, a pull request or a review gets a comment in the conversation; a discussion gets a discussion comment.

### What the rewrite leaves out

The poller and its state file (a cell is called, it does not wake itself), the signed local webhook and its route documents, the Discord copies and the progress notices read from Hermes' own database, and the `status`/`routes` CLI subcommands. There is no separate bot account: the token belongs to the GitHub user Nodecode speaks as. Discussions are read and answered over the REST comment endpoints, so a reply to a reply inside a discussion is posted as a new discussion comment rather than nested under it.

## Credit and license

luantak's [hermes-github-mentions](https://github.com/luantak/hermes-github-mentions), commit d8a88220a547180663fcec76a069c8fbbf63bff9, MIT licensed. The mention rules and the answer shapes are the original author's, rewritten in Common Lisp: `NOTICE` says what was rewritten and what was left out, and `LICENSE` carries the original MIT text.
