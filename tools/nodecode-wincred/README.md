# nodecode-wincred

Windows Credential Manager: resolve provider keys from the current user's generic credentials instead of plaintext `.env` files. A Nodecode rewrite of [Yicheng-Mark/hermes-windows-plugins](https://github.com/Yicheng-Mark/hermes-windows-plugins) by [Yicheng-Mark](https://github.com/Yicheng-Mark).

## Get it

Tell the agent "add the wincred cell", or run `nodecode add nc://wincred`.

## Set it up

Windows 10 or 11, and nothing else: the cell reads the vault through advapi32's
`CredReadW` from SBCL's alien interface — no dependency, no subprocess, no network.

Store the key in Credential Manager, as a credential of type **generic credential**
(Control Panel, Credential Manager, Windows Credentials, Add a generic credential).
The target name is what the cell reads; the user name is ignored by the vault's
API, so anything will do. Or, from a shell (the key then also lands in the shell's
history):

```powershell
cmdkey /generic:hermes/openai /user:hermes /pass:sk-...
```

Then map an environment variable to that target in `~/.nodecode/config.jsonc`:

```jsonc
{
  "wincred": {
    "map": ["OPENAI_API_KEY=hermes/openai", "ZAI_API_KEY=wincred://hermes/zai"]
  }
}
```

`cmdkey /list` lists the names you have. The value may be UTF-8 or UTF-16; the
Credential UI and PowerShell write UTF-16, most CLI tools write UTF-8, and a blob
that decodes to neither is skipped with a warning rather than injected as a
mangled key.

## Use it

At start the cell applies the map: each credential's value becomes an environment
variable of the Nodecode process, so a command started from a session sees it,
and `override_existing` decides whether a variable already set wins (it does by
default, as `.env` does). Stop puts back exactly the variables the start touched.

Three tools, and the `/wincred` command:

- `wincred_status` — the map, and for each entry whether the credential is there,
  how many bytes it is, and whether this process already has it. No value comes
  out; start here.
- `wincred_read` — one credential's value, by the variable the map names or by
  its target (`hermes/openai`, `wincred://hermes/openai`). The answer is the
  value alone.
- `wincred_apply` — apply the whole map again, answering names and byte counts,
  never a value.
- `/wincred` — the status; `/wincred apply`; `/wincred OPENAI_API_KEY` for one.

`(help :wincred)` carries the same, and `(nodecode-wincred:status-text)` is the
status as a form.

## What it discloses

- It reads the current user's generic credentials, and only the targets the map
  names — there is no enumeration of the vault.
- Values become environment variables of the Nodecode process and of what it
  starts, exactly like `.env` entries: anything that can read the process
  environment (a shell command, a child) can see them.
- Credential Manager is user-scoped and DPAPI-backed. This raises the at-rest bar
  (backups, cloud sync, a dotfiles repo no longer carry the key in plaintext). It
  is not a boundary against code already running as you.
- Read-only: the cell never creates, updates or deletes a credential, writes no
  file, starts no process and makes no network call.

## Windows only

On another platform the cell still loads, the section still parses and the tools
still answer — `wincred_status` says the vault is unavailable and
`wincred_apply` applies nothing. The stub used by the tests is the same seam the
real vault sits behind.
