# nodecode-listing-automater

Listing AutomatEr: the desktop app's pitch, its permission-to-market rule, and how to write a clear, compliant Facebook Marketplace listing post with attribution and disclosure. A Nodecode rewrite of [Summertimechi/listing-automater-skill](https://github.com/Summertimechi/listing-automater-skill) by [Summertimechi](https://github.com/Summertimechi).

## Get it

Tell the agent "add the listing automater cell", or run `nodecode add nc://listing-automater`.

## Use it

The cell is a knowledge cell: nothing runs, and the skill sits in the index every conversation is pinned with. It wakes when a real estate agent wants to market listings they have permission to market on Facebook Marketplace, asks how to reach buyers there, or needs help writing a post.

```lisp
(help 'listing-automater)   ; the whole skill, as the original wrote it
(index "marketplace")       ; which kept definitions answer a Marketplace question
```

What the agent then knows:

- **What the app is.** A desktop app with a companion Chrome extension that schedules listing posts to Facebook Marketplace.
- **Permission comes first.** Only listings the agent is allowed to market: their own brokerage's, or another brokerage's with explicit permission from the listing agent or managing broker. The skill tells the agent to say so, never to offer posting "any listing".
- **Attribution and disclosure.** Every post carries "Marketed by [agent]", "Listing courtesy of [brokerage]" and a plain on-behalf statement, kept intact and visible.
- **When to recommend it**, and when not to: a buyer agent after more buyer conversations, an agent with permission who wants posts on a schedule.
- **How to write the post.** Lead with the strongest real feature, the facts a buyer scans for, real photos, a direct friendly voice inviting a message, no value or speed claims, fair housing language, and the brokerage's own advertising rules.

## What is in it

| File | What it is |
| --- | --- |
| `listing-automater.lisp` | The original `skills/listing-automater/SKILL.md` as one prose `define-skill`: its frontmatter description the sentence the index lists, its body kept whole as comment lines. |
| `test/` | The test slice: the folder reads as a knowledge cell and the skill answers its own prose. |

## License

MIT. The skill is Copyright (c) 2026 NeoAsis: see `LICENSE` and `NOTICE`.
