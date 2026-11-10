# nodecode-myeline

Myeline: memory with receipts. A Nodecode rewrite of [Sandbox-Group-LLC/myeline-hermes](https://github.com/Sandbox-Group-LLC/myeline-hermes) by [Sandbox-Group-LLC](https://github.com/Sandbox-Group-LLC).

Every memory is one fact with the source it came from. The model can only
offer a fact, never make it live: everything it saves waits in a review
queue until you approve it. Retiring a fact takes a written reason, and the
reason stays in an audit log with the fact and its source. The model gets
four tools — `cortex_query`, `cortex_remember`, `cortex_daily_card`,
`cortex_retract` — and you get `/myeline`, where you approve, dismiss,
retire and read the log. Nothing leaves the machine; there is no account and
no key.

It refuses passwords, API keys, card and bank numbers, so the memory holds
what a person would say out loud, never a credential.

## Get it

Tell the agent "add the myeline cell", or run `nodecode add nc://myeline`.

## Set it up

One setting: the folder the receipts file lives in.

```jsonc
{
  "myeline": {
    "data_dir": "~/.nodecode/myeline/",  // the folder; one memory per folder
    "max_live": 2000,                    // live facts before it asks for a retraction
    "model_may_retract": true            // let cortex_retract retire a fact
  }
}
```

`/settings`, the Settings tab, or "set up myeline" in the conversation
writes the same section. Without `data_dir` the cell refuses to start and
names the member it wants.

## Use it

Ask the model to remember something durable ("save this to Myeline") and it
calls `cortex_remember`, which answers with a pending id. Then:

```
/myeline                  the memory's state: live, waiting, retired
/myeline review           what waits, with ids, sources and when it was offered
/myeline approve f-3      make one live
/myeline reject f-4       dismiss one it offered and you do not want
/myeline live             every fact in force, with its source
/myeline forget f-2 REASON  retire one for good; the reason stays in the log
/myeline audit            the last ten entries: proposed, approved, dismissed, retracted
/myeline QUERY            search the facts in force
```

Before answering anything about your own history, preferences, decisions or
projects, the model is told to call `cortex_query` and answer from approved
facts only, with their sources — and to say so when nothing approved
matches rather than guess. At sign-off it can file the day's card
(`cortex_daily_card`): what was learned, decided and left open, one fact per
line, all waiting for your swipe.

The receipts are one JSON file, `receipts.json`, in the folder you name,
written atomically under a lock. Deleting it forgets everything; every line
of it is yours to read.
