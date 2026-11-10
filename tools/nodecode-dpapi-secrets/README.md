# nodecode-dpapi-secrets

DPAPI Secrets: one DPAPI-encrypted store of named secrets on Windows, resolved by name. A Nodecode rewrite of [Yicheng-Mark/hermes-windows-plugins](https://github.com/Yicheng-Mark/hermes-windows-plugins) by [Yicheng-Mark](https://github.com/Yicheng-Mark).

The original is a Hermes secret source: at startup it decrypted one file,
`secrets.dpapi`, under the current Windows user's Data Protection API key, and handed the
values it held to the orchestrator as environment variables, so a provider key never sat in
plaintext in `.env` or `auth.json`. Nodecode has no secret-source seam, so the same store is
read where a session needs it: six tools, one command, a manual topic, and one Lisp
function for a form. The file format is the original's, byte for byte, so a store the
Hermes plugin wrote is read here unchanged.

## Get it

Tell the agent "add the dpapi-secrets cell", or run `nodecode add nc://dpapi-secrets`.

## Set it up

Nothing is required: the section is on, the store is `secrets.dpapi` in this home, and
`/dpapi-secrets set NAME` creates it. A secret is asked for, never passed on a command
line, and the store never echoes it back.

```jsonc
{
  "dpapi-secrets": {
    // the store, relative to this home; a store the Hermes plugin wrote is read unchanged
    "file": "secrets.dpapi",
    // "auto" is DPAPI on Windows and a 0600 file everywhere else
    "protection": "auto",
    // the variables the store answers, as NAME=entry
    "map": ["OPENAI_API_KEY=openai", "ZAI_API_KEY=dpapi://zai"]
  }
}
```

The `map` is the original's `secrets.dpapi.map`, written one `NAME=entry` line at a time; a
`dpapi://` prefix is read and ignored, as the original's parser did.

## Use it

Ask the agent for a secret by name, or type a command yourself:

- `dpapi_list` lists the entries and the variable each is mapped to: names only, no value.
- `dpapi_show` reads one entry and answers the value alone.
- `dpapi_set` writes one entry, `dpapi_unset` removes one.
- `dpapi_env` reads the whole mapping the way the original injected it: one `NAME=value`
  line per `map` line the store answers, with a warning for each entry it cannot (missing,
  empty), and a refusal when nothing resolves.
- `dpapi_status` says where the store is, how it is protected on this platform, how many
  entries it holds and how the mapping stands, reading no value.
- `/dpapi-secrets` with no arguments is that status, with a dialog for storing and showing
  one value; `list`, `show NAME`, `set NAME`, `unset NAME`, `env`, `path`, `status` type
  the same verbs.

A value read here is as sensitive as the key it is, so put it where the operator asked for
it. `(nodecode-dpapi-secrets:secret "openai")` reads one entry in this image without a tool
call, and `(nodecode-dpapi-secrets:resolve)` answers the whole mapping as an alist.

## How the store is protected

- **Windows.** Every call is `CryptProtectData`/`CryptUnprotectData` on `crypt32`, with
  `CRYPTPROTECT_UI_FORBIDDEN` and never the machine-wide flag: the key comes from the
  current user profile, and a blob written here is readable by that user on that machine
  and by nobody else. Another local account, a copy on another machine, and the same user
  after their master keys were reset all fail to read it. No dialog is ever popped on the
  startup path.
- **Everywhere else.** There is no DPAPI, so the same map is written under
  `NODECODE-DPAPI-FILE-1`, with the JSON in the clear, and the file mode set to `0600`.
  `dpapi_status` says so in the same words; nothing pretends the file is encrypted. The
  header is what decides how a file is read, so a store carried between platforms still
  says what wrote it, and a Windows store on another platform refuses in words instead of
  decrypting into nonsense.
- **Fail closed.** A file with no header, a header this cell did not write, a blob this
  account cannot decrypt, and a decrypted payload that is not a flat string map are each an
  error: nothing is guessed and nothing is injected out of a file that only nearly parses.
- **Atomic writes.** A write goes to a temporary file in the same directory and is renamed
  into place, so an interrupted write cannot leave a truncated store.
- **No network, no subprocess, no self-update.** The store is one local file, read when a
  tool asks for it.

Limitations, as the original had them: the store is not portable (copy it to another
account or machine and re-create it there), and one store belongs to one home — point
`file` elsewhere for a profile of its own.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-dpapi-secrets
```

MIT licensed; see [NOTICE](NOTICE) for what was rewritten, and [LICENSE](LICENSE) for the
original's terms.
