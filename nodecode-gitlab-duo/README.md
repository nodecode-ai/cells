# nodecode-gitlab-duo

A Nodecode cell for [GitLab Duo](https://docs.gitlab.com/user/gitlab_duo/)'s
non-agentic chat models. With it, `/gitlab-duo login` signs in with GitLab,
`/models` lists Duo's models (`duo-chat-opus-4-6`, `duo-chat-gpt-5-codex`, ...),
and a turn goes through GitLab's AI gateway the way oh-my-pi sends it:

- the GitLab token is traded at `gitlab.com/api/v4/ai/third_party_agents/direct_access`
  for a Duo grant and the headers the gateway wants (reused for 25 minutes);
  the round sends the grant as a bearer, never the GitLab token
- Claude models go to the gateway's Anthropic proxy on the Messages wire,
  thinking on a budget; the GPT-5 codex models to its OpenAI proxy on the
  Responses wire; the other GPT models to that proxy on the chat wire,
  without sampling parameters
- the proxy is asked for the upstream model a Duo alias stands for
  (`duo-chat-opus-4-6` is `claude-opus-4-6`)

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `gitlab-duo`
provider. See [NOTICE](NOTICE).

## Sign in

```
/gitlab-duo login
```

answers at once with the address to open and listens on
`http://localhost:8080/callback`, the redirect omp's GitLab OAuth
application registers (another port when 8080 is taken, which GitLab will
then refuse). It is GitLab's authorization-code flow with PKCE, scope `api`.
When the browser comes back the code is exchanged for a token, kept in
auth.json under `oauth_tokens.gitlab-duo` (`access_token`, `refresh_token`,
`expires_at` five minutes inside what GitLab granted), and a notice says so once.
The token is refreshed when it is within a minute of that and written back;
a refresh that fails stands as a notice until the next sign-in.
`http://localhost:<port>/launch` redirects to the sign-in page.

On another machine, paste the address the browser ended on, or the code:

```
/gitlab-duo code <address or code>
```

If GitLab answers "The redirect URI included is not valid", register an
OAuth application of your own (scope `api`) and set `GITLAB_CLIENT_ID` and
`GITLAB_REDIRECT_URI`; a redirect that is not on this machine is pasted.
Or skip the sign-in: set `GITLAB_TOKEN` to a personal access token with the
`api` scope. The sign-in wins over the variable. Nothing else is ever sent
to GitLab: with neither, the round goes keyless and GitLab says why.

`/gitlab-duo status` says whether GitLab is signed in, `/gitlab-duo logout`
forgets the token.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-gitlab-duo/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "gitlab-duo": {
    "gitlab_url": "https://gitlab.com",
    "gateway_url": "https://cloud.gitlab.com"
  }
}
```

A `providers.gitlab-duo` entry naming an `sdk` (for the provider or one
model) outranks omp's route for the wire.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-gitlab-duo
```

## Not ported

- omp marks the Anthropic proxy route as an Anthropic OAuth credential, and
  so sends Claude Code's request fingerprint there: its beta list, its
  `X-Stainless-*` and `User-Agent` headers, its system instruction ("You are
  Claude Code, ...") and a `_` prefix on tool names. This cell sends the grant as the bearer
  with the gateway's own headers and the core's Messages body; the
  fingerprint is omp's Claude-subscription disguise, not something the
  direct-access grant is documented to need.

MIT licensed.
