# nodecode-gitlab-duo-agent

A Nodecode cell for the [GitLab Duo Agent Platform](https://docs.gitlab.com/user/duo_agent_platform/).
No Nodecode lane speaks its wire, so the cell registers one,
`gitlab-duo-agent`: each round runs as a Duo workflow over GitLab's
WebSocket, the way oh-my-pi runs it, and comes back as the chat-shaped
message every lane answers.

- `/gitlab-duo-agent login` signs in with GitLab (its own sign-in record,
  apart from `gitlab-duo`'s), and `/models` lists the models the account's
  namespace offers
- a round sets the workflow up over REST (namespace, its Duo flags, a
  project, a workflow token, an empty workflow, the namespace's pinned
  model), then sends a `startRequest` on the socket: an inline `ambient`
  flow whose agent's system slot is the round's system prompt, whose goal is
  the conversation as a flat ChatML transcript, and whose MCP tools are the
  round's tools, all pre-approved
- what the service streams back is folded: checkpoint snapshots into text
  and reasoning (only what grew since it was last seen), a `runMCPTool`
  action into a tool call that ends the round
- the workflow waits on its socket while the tool runs; the next round
  answers it there (`actionResponse`) and reads on. A user message after the
  result, or a result that never came, abandons it (stopped at GitLab) for a
  fresh workflow whose goal carries everything
- restarts on a fresh workflow as omp bounds them: a silent socket (once),
  the step limit (4), a workflow that stopped advancing (2), the catch-all
  failure (1); an approval the service asks for is granted on a new socket
- a goal past 2,000,000 bytes is not sent, and one past 1 MiB that fails is
  reported as `prompt is too long`, which the core answers by evicting

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s
`gitlab-duo-agent` provider. See [NOTICE](NOTICE).

## Sign in

```
/gitlab-duo-agent login
```

answers at once with the address to open. It is GitLab's authorization-code
flow with PKCE, scope `api`, through GitLab's own VS Code OAuth application,
whose redirect is VS Code's scheme
(`vscode://gitlab.gitlab-workflow/authentication`). Nothing here can receive
that, so copy the whole `vscode://...` address the browser (or VS Code) is
sent to and paste it:

```
/gitlab-duo-agent code vscode://gitlab.gitlab-workflow/authentication?code=...&state=...
```

The sign-in finishes in the background, says so once, keeps the token in
auth.json under `oauth_tokens.gitlab-duo-agent` (`access_token`,
`refresh_token`, `expires_at` five minutes inside what GitLab granted), and
looks for the models the account offers. The token is refreshed when it is
within a minute of `expires_at` (the redirect sent again, as this
application requires) and written back; a refresh that fails stands as a
notice until the next sign-in.

Or skip the sign-in: set `GITLAB_TOKEN` to a personal access token with the
`api` scope. The sign-in wins over the variable. No other provider's key is
ever sent to GitLab.

`/gitlab-duo-agent models` looks for the models again, `/gitlab-duo-agent
status` says where things stand, `/gitlab-duo-agent logout` forgets the
token and stops every workflow waiting on it.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-gitlab-duo-agent/` and
restart Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "gitlab-duo-agent": {
    "gitlab_url": "https://gitlab.com",
    "namespace_id": "",
    "project": "",
    "workflow_definition": "ambient"
  }
}
```

Without `namespace_id` or `project` the workflow runs in the root group of
the session directory's GitLab remote, else in the first top-level group you
belong to (Duo-enabled groups first). The first round in a namespace turns on
its agent platform, MCP and experiment flags, which needs a maintainer; it
goes on without them when you are not one.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-gitlab-duo-agent
```

The workflow socket is a scripted stand-in there (`*SOCKET-FACTORY*`), and
GitLab's REST and GraphQL a stubbed `dex:request`.

## Gaps

- **A pause between rounds.** omp ends an assistant message mid-checkpoint
  (`pause_turn`) when the service logs a request or tool boundary after new
  text in the same snapshot, and its agent loop calls again without a tool
  call. The core's turn loop ends the turn on a round with no tool call, so
  the cell folds such a snapshot into the one round, a blank line between
  its messages. The seam that would carry it: a finish reason a lane can
  answer from its stream function (`"pause"` beside `"stop"` and
  `"tool_calls"`) that `nle::call-provider`'s caller treats as "call the lane
  again with the same history".
- **A conversation's end.** omp stops a workflow left waiting on a tool when
  its session is reset or disposed (`ProviderSessionState.close`). The core
  tells a cell nothing when a session is reset, compacted away or closed, so
  such a workflow stays open until the next round of that conversation
  abandons it, the cell stops, `/gitlab-duo-agent logout` runs, or GitLab
  times it out. The seam: a hook point (say `:session`, `(op next)`) the
  core runs with `(:kind :reset|:closed :session-id ID)`.

## Not ported

- the socket does not go through `providers.gitlab-duo-agent.proxy`:
  websocket-driver dials the host itself
- omp's opt-in credential redaction of the goal (off by default there), its
  trace file (`GITLAB_DUO_WORKFLOW_TRACE`), and its test-only
  `workflowId`/`workflowToken` options
- omp's model cache partitioned by account and directory: discovery runs
  after a sign-in, at start when a sign-in is kept, and on
  `/gitlab-duo-agent models`, and its last answer is what `/models` lists

MIT licensed.
