# nodecode-long-conversations

Long Conversations: handoff briefs that carry a session's state past its context limit, so a fresh session continues where the last one stopped. A Nodecode rewrite of [cspears32/hermes-long-conversations](https://github.com/cspears32/hermes-long-conversations) by [cspears32](https://github.com/cspears32).

Before a session runs out of context, the model writes a brief: the topic,
what is in progress, the decisions already settled, the files in play, the
assumptions, the open questions, the dead ends not to try again, and the next
step. A brief is a JSON file in a folder you name, so it outlives the process
that wrote it. When the next session makes its first model request, it reads
the brief once and starts knowing where the work stood.

Four tools — `handoff_save`, `handoff_list`, `handoff_load`, `handoff_prune` —
and `/long-conversations`, which lists the briefs, shows one, prunes the old ones, or
starts one from a topic. Nothing leaves the machine, and there is no key.

## Get it

Tell the agent "add the long-conversations cell", or run `nodecode add nc://long-conversations`.

## Set it up

One required setting: the folder the briefs are kept in.

```jsonc
{
  "long-conversations": {
    "data_dir": "~/.nodecode/long-conversations/",  // the folder; one brief a JSON file
    "keep_days": 14,                      // older briefs are pruned at start
    "pending": true                       // hand a saved brief to the next session
  }
}
```

`data_dir` has no default on purpose: the briefs describe your work, kept
where you say. Without it the cell refuses at start and names the member. The
section is in `/settings` too.

| Member | Default | What it does |
| --- | --- | --- |
| `data_dir` | none (required) | the folder the briefs are kept in |
| `keep_days` | `14` | briefs older than this go at start and on `/long-conversations prune`; 30 days of a few kilobytes each |
| `pending` | `true` | hand a saved brief to the first model request of another session, once |

## Use it

Ask for it plainly; the model writes the brief:

> We are near the end of the context — write a handoff brief.

Then, in a new session:

> Read the last handoff brief and carry on.

| Tool | Argument | What it does |
| --- | --- | --- |
| `handoff_save` | `topic`, `work_in_progress?`, `decisions_made?`, `active_state_files?`, `key_assumptions?`, `open_questions?`, `dead_ends?`, `next_step?`, `id?` | write one brief; answers its id |
| `handoff_list` | `limit?` | the briefs this folder holds, newest first |
| `handoff_load` | `id` (or `"latest"`) | one brief as the block a fresh session reads |
| `handoff_prune` | `older_than_days?` | delete the old briefs; the section's `keep_days` by default |

From the keyboard, `/long-conversations` shows the briefs with the ones you can open;
`/long-conversations show ID` reads one, `/long-conversations latest` the newest, `/long-conversations prune`
deletes the old ones, and `/long-conversations save TOPIC` keeps a brief for the model
to fill in.

A brief deliberately stays small: a topic, a few lists, a next step. Detail
belongs in the files the brief names, not in the brief. It arrives in the new
session as data from the session before — what was decided and what failed —
not as instructions from you.
