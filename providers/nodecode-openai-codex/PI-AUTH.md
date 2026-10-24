# Pi Codex subscriptions in Nodecode

The optional `nodecode-openai-codex` cell discovers Pi's Codex subscriptions and
shows them through Nodecode's normal `/models` picker, under their exact names:
`openai-codex`, `openai-codex-2`, `openai-codex-3`, and so on. You do **not** need
to invent aliases, hand-write a provider for each account, or sign in again.
Only Codex subscriptions are supported in this increment, not other OAuth
providers, multi-pass pools, selectors or project-affinity policies.

## Install and choose

Install the updated cell folder under your Nodecode home's
`cells/nodecode-openai-codex/`, following the normal cell contract. Until these
changes are published, an older hub copy may not include them. Do not overwrite
an existing installation or restart a busy gateway blindly.

Pi reuse requires Node.js 22+ and an installed Pi 1.1-compatible coding-agent
package. Set `NODECODE_PI_PACKAGE_DIR` to that installed **package directory** if
its launcher is a wrapper and discovery on `PATH` cannot locate it. No package
is downloaded automatically. Nodecode-native sign-in does not require Node/Pi.

Once the cell is running, open `/models` and choose a model under the account
name you want. A numbered-only Pi store stays numbered-only: no base account is
invented. Model metadata comes from the cell's existing Codex catalog; listing
is not a live guarantee that every model is available on your subscription.

Discovery imports only non-secret source metadata into Nodecode's normal config:

```json
{
  "providers": {
    "openai-codex-2": {
      "sdk": "openai-responses",
      "pi_auth": "openai-codex-2",
      "pi_discovered": true
    }
  }
}
```

This is generated for discovered accounts, not required manual setup. No access
or refresh token, email or account claim is copied. The source binding remains
after logout/restart so a saved Pi selection cannot silently become an ambient
login. Discovery does not change the default/auxiliary model or create failover.
A writable Nodecode config is needed to remember a newly discovered source.

Existing explicit non-Pi provider entries are preserved. An existing
Nodecode-native Codex sign-in retains ownership of `openai-codex`; two different
sources cannot independently occupy that same provider ID. If you deliberately
want Pi instead, an explicit `pi_auth` entry without `pi_discovered` chooses Pi.
Explicit/config API keys and native stored API keys retain their normal higher
precedence. Remove such overrides if you intend to use Pi authentication.

## Paths and optional controls

The default store is `~/.pi/agent/auth.json`. `PI_CODING_AGENT_DIR` selects another
Pi directory, including `~/...`. To choose a different file or stop importing new
accounts, use the cell's optional section:

```json
{
  "openai-codex": {
    "pi_auth_file": "/absolute/path/to/pi/auth.json",
    "pi_accounts": true
  }
}
```

Leave `pi_auth_file` empty to use Pi's directory. `pi_accounts: false` disables
new discovery; it does not erase already imported source bindings. A provider's
optional `pi_auth_file` overrides the cell-level path. Malformed explicit paths
or selectors fail closed rather than opening another store.

Discovery is cached briefly (five seconds), uses Pi's read-lock protocol and
does not wait behind a busy refresh. Missing Pi or unavailable inventory does
not break unrelated providers. Returning only account-key names does not verify
that a credential is valid. Credentials are checked/refreshed only through the
selected account's private resolution path; probes never refresh.

## Transport, refresh and privacy

Pi accounts use `https://chatgpt.com/backend-api/codex/responses`, never chat
completions. Custom listing/base URLs cannot receive their grants. The cell
reuses its Codex request shaping, account headers, `response.done` handling and
account-scoped cache/thread/sticky-response state. Incompatible SDK/model-wire
overrides are refused.

Pi owns refresh locking, rotation and persistence for the exact selected key.
Missing, invalid or logged-out selections do not borrow another subscription or
environment login. The cell's `/openai-codex login`, `status` and `logout` commands
still manage **Nodecode-native** sign-ins. Manage Pi subscriptions in Pi instead.
Never paste `auth.json` into chat or invoke the credential helper by hand against
your real store: its successful credential stdout is a private process pipe.
Errors are sanitized, and grants are not added to model prompts or diagnostics.

**Not a sandbox:** Nodecode's unrestricted Lisp evaluator can deliberately read
its user's files and memory. Preventing that absolutely requires OS isolation
and a separate privileged credential proxy. This integration avoids automatic
exposure; it does not turn the evaluator into a security boundary.

## Optional failover

Switching accounts is manual unless you configure Nodecode's normal Backup chain:

```json
{
  "failover": {
    "on": ["rate_limit"],
    "chain": [{ "provider": "openai-codex-2", "model": "gpt-6.1-sol" }]
  }
}
```

Choose an available model; that model ID is an example. `/models`' Backup tab edits
the same chain. Omit it or use an empty chain to disable failover. Each candidate
uses its own exact login, and the next call starts from the selected primary.
Authentication failures alone never authorize switching accounts.

## Development checks

Portable native ASDF tests use synthetic helper stubs, including automatic
inventory/import and native ownership collisions. The separate installed-Pi
`nodecode-openai-codex/pi-integration-test` and Node helper SDK tests also use
synthetic stores; HTTP is mocked. See the README for commands.

`test/installed-entry-smoke.sh` exercises an installed Nodecode binary with no
hand-written provider entries, a throwaway home, two synthetic Pi accounts and
disabled network. It checks the real private pipe, native picker IDs/catalog,
Responses transport, account isolation and explicit 429 failover. It requires
Linux user/network namespaces, `ip`, `NODECODE_BIN`, and
`NODECODE_PI_PACKAGE_DIR`. Live subscription acceptance is a separate user test.
