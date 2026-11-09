# nodecode-pass-secrets

Pass Secrets: a local GPG-encrypted password store, read as secrets a session can use. A Nodecode rewrite of [dyiapanis/pass-secrets](https://github.com/dyiapanis/pass-secrets) by [dyiapanis](https://github.com/dyiapanis).

## Get it

Tell the agent "add the pass-secrets cell", or run `nodecode add nc://pass-secrets`.

## Set it up

The cell runs [`pass`](https://www.passwordstore.org/), which is yours: nothing here vendors it, downloads it or talks to a network. An initialized store and a GPG key are all it needs — the key's passphrase is asked by `gpg-agent`, never by the cell:

```sh
# Debian/Ubuntu: sudo apt install pass; macOS: brew install pass
pass init <GPG_KEY_ID>
```

Then name the store's directories in `~/.nodecode/config.jsonc`. Nothing is required: the defaults read `~/.password-store`, `shared` only. Keep `subdirs` narrow, because `pass_env` reads every entry under them:

```jsonc
{
  "pass-secrets": {
    "store": "~/.password-store",
    "subdirs": ["shared", "work"],
    "timeout": 30,
    "binary": "pass"
  }
}
```

No key is a setting: the store is local and the GPG key stays where `pass` put it.

## Use it

Ask the agent for a secret by name, or type a command yourself:

- `pass_list` lists the entries under the configured subdirectories — names only, no value — marked with the environment variable each one is named for.
- `pass_show` reads one entry with `pass show -- NAME` and answers the value alone; `first_line` takes only the first line of an entry whose rest is metadata.
- `pass_env` reads the whole store the way the original plugin injected it: one `NAME=value` line per entry whose leaf name is an environment variable's.
- `pass_status` says whether `pass` runs here, where the store is, and how many entries each subdirectory holds — no value.
- `/pass` with no arguments is that status; `/pass ls`, `/pass find …`, `/pass git …` run `pass` itself.

A value read here is as sensitive as the key it is. Evaluate `(nodecode-pass-secrets:secret "shared/OPENAI_API_KEY")` in this image to use one without putting it in the transcript.
