# nodecode-cline-pass

A Nodecode cell for [ClinePass](https://cline.bot), Cline's model
subscription. With it, `/connect` offers ClinePass, `/models` lists its
models (the subscription ones and the free `cline-free/...` ones), and a turn
on `cline-pass/<model>` goes to `https://api.cline.bot/api/v1` the way the
Cline CLI sends it:

- the model is named `cline-pass/<id>` on the wire for a subscription model,
  and as it is for a free one
- every request carries the Cline CLI's client headers, which the gateway
  uses to admit some roster entries

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `cline-pass`
provider. See [NOTICE](NOTICE).

## Key

Make a key at <https://app.cline.bot/dashboard/account> (Settings, API Keys).
Save it with `/connect`, or set `CLINE_API_KEY`. A saved key wins over the
variable.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-cline-pass/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "cline-pass": {
    // where ClinePass is served; a relay's address goes here
    "base_url": "https://api.cline.bot/api/v1"
  }
}
```

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-cline-pass
```

MIT licensed.
