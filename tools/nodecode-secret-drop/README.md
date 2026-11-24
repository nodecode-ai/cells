# nodecode-secret-drop

Secret Drop: give the agent a password or token in a drop, not in the chat; it lands in the environment the agent's commands inherit, and an exact copy is redacted. A Nodecode rewrite of [Goitonthefloor/hermes-secret-drop](https://github.com/Goitonthefloor/hermes-secret-drop) by [Goitonthefloor](https://github.com/Goitonthefloor).

A key pasted into a chat is in the model's context for the rest of the session,
in every request, in the log and in every transcript read off it. Secret Drop
takes the value somewhere else: `/secret-drop DEPLOY_TOKEN` opens a field, you
type the value there, and it goes into a store only you read and into this
image's environment. A command the model runs then uses `$DEPLOY_TOKEN` and the
model never sees what it is worth. `request_secret` is how the model asks for
one, and `list_secrets` names what is stored without ever showing a value.

## Get it

Tell the agent "add the secret-drop cell", or run:

```sh
nodecode add nc://secret-drop
```

## Use it

```
/secret-drop                     the drop field, and what is stored
/secret-drop DEPLOY_TOKEN        the field for that one value
/secret-drop DEPLOY_TOKEN VALUE  store it now, in your own input box
/secret-drop list                the names, never the values
/secret-drop rm DEPLOY_TOKEN     out of the store and the environment
/secret-drop doctor              the store path, passthrough, redaction, held back
```

The model has two tools and one paragraph of standing guidance. `request_secret`
takes the variable name a task needs and puts it on your board: "run
/secret-drop DEPLOY_TOKEN VALUE". `list_secrets` answers the names, their
lengths, and whether each is in the environment and redacted. The guidance tells
the model never to ask for a paste, to use `$NAME`, and never to print, log or
encode it.

## What is safe to search for

A value is redacted from the text that reaches the model only when searching for
it cannot rewrite ordinary prose: at least 8 characters with a digit or a
symbol, or at least 24 characters. That is the original's rule, and it is why
`password` is stored and handed to the shell but never used as a global
search-and-replace. Both bounds are settings (`min_redaction_chars`, and the
long one fixed at 24, as in the original).

A provider credential — `OPENAI_API_KEY` and its neighbours — is stored but held
back from the environment by default, as the original kept provider credentials
out of its sandboxed commands. `hold_back` names more.

## Settings

The `secret-drop` section of `config.jsonc`:

```jsonc
{
  "secret-drop": {
    "store": null,              // the store file; default secret-drop/store.lisp under the home
    "passthrough": true,       // put each value in this image's environment at start
    "hold_back": [],            // names, or prefixes, stored but never exported
    "redact": true,             // replace exact copies in the text that reaches the model
    "min_redaction_chars": 8   // shorter values are never searched for
  }
}
```

The store is written whole, one alist of names to values, mode 0600 where the
OS has modes. The values are the environment for as long as the image runs; a
stop puts back whatever was there before. It is not a boundary against a model
that goes looking: a value in the environment can be read by a command, which
is exactly why the redaction is there.

## What the original had that this does not

The desktop key button and the dashboard route that fed it: Nodecode's drop is
`/secret-drop` and the tool that asks for it. Hermes's profile `.env` and
`terminal.env_passthrough` become a store of the cell's own and this image's
environment. See [NOTICE](NOTICE).
