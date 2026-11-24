# nodecode-wechat-assistant

WeChat relationship memory: what each person said, what is coming up for them, who you have not written to, and a draft worth sending. A Nodecode rewrite of [harrylabsj/hermes-plugin-wechat-social-assistant](https://github.com/harrylabsj/hermes-plugin-wechat-social-assistant) by [harrylabsj](https://github.com/harrylabsj).

A chat is a stream you forget. Someone says they are moving in March, someone
is waiting on results, someone thanked you for something — and a week later
the thread is gone. This cell keeps those things as notes about people, and
answers the three questions a friendship actually asks: **who is this**
(a brief), **who have I not written to** (the quiet ones, quietest first),
and **what would I say** (a draft).

It is local-first in the same way the original was: one JSON file under your
home, mode 0600, no network call anywhere in the cell, and nothing is ever
sent — the cell writes drafts, you send them by hand.

## Get it

Tell the agent "add the wechat-assistant cell", or run:

```sh
nodecode add nc://nodecode-wechat-assistant
```

## Set it up

Nothing is required. What you may want to change, in `~/.nodecode/config.jsonc`
or on the Settings page:

```jsonc
"wechat": {
  "quiet_days": 21,   // how long without a note makes someone worth a word
  "brief_notes": 5,   // how many recent notes one brief carries
  "keep_notes": 400,  // how many notes the memory keeps; the oldest go first
  "report_days": 30   // the window the report looks back over
}
```

## Use it

As the conversation goes, the agent files what is worth remembering:

```lisp
(wechat:note :contact "Lin" :text "moving to Chengdu in March" :kind "plan")
(wechat:contacts)          ; everyone, last seen, how many notes each
(wechat:contacts "lin")    ; the same, narrowed by a word
(wechat:brief "Lin")       ; one person whole, and the one line worth saying
(wechat:followups)         ; gone quiet longest, quietest first
(wechat:draft :contact "Lin" :text "How is the move going?")
(wechat:drafts)            ; what is still unsent
(wechat:mark 1 "sent")     ; you sent it by hand
(wechat:report)            ; the last weeks as one page
```

The model has the same verbs as tools: `wechat_note`, `wechat_search`,
`wechat_brief`, `wechat_followups`, `wechat_draft` and `wechat_report`.
`/wechat` in a shell shows the memory and a panel — a brief, the quiet ones,
the drafts, the report — and `(help :wechat)` carries the manual.

**A note's kinds.** `said` (something they said), `plan` (something coming
up), `met` (a meeting or a call), `fact` (a durable fact about them), `other`.
A note's day may be backdated (`:at "-3d"`) or given as a date
(`:at "2026-11-02"`); empty means now.

**Notes are evidence, not instructions.** Whatever a person said, or whatever
was read off a screen, is data the memory holds; it never authorises a
message, a tool call or a send. The cell has no send capability at all.

## Where it lives

One JSON file, `wechat/memory.json` under the home, mode 0600, its folder
0700: `notes` (id, contact, key, text, kind, at, source) and `drafts` (id,
contact, key, text, status, created, updated). People are derived from the
notes that name them, so a person spelled `Lin`, `lin` or `  lin ` is one
person. Past `keep_notes` the oldest notes go first.

## Credit

Rewritten from harrylabsj's Hermes Agent plugin `hermes-plugin-wechat-social-assistant`, Apache-2.0. See
[NOTICE](NOTICE): what was kept, and what was left out.

Run `.github/test-cell.sh tools/nodecode-wechat-assistant` from the cells repository.
