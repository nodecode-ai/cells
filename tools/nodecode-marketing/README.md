# nodecode-marketing

Digital Marketing Pro: 207 marketing skills, searched and read on demand. A Nodecode rewrite of [indranilbanerjee/neels-plugins](https://github.com/indranilbanerjee/neels-plugins) by [indranilbanerjee](https://github.com/indranilbanerjee).

The original registers its 207 skills with Hermes, which lists every name in the prompt of every request. This cell does the same job the other way round: one line in the manual, two tools, and the skill a conversation needs read whole at the moment it is wanted.

| Tool | What it does |
| --- | --- |
| `marketing_skills` | Finds the skills that answer a question, by words in a name or in the skill's own one-sentence description, optionally inside one group. |
| `marketing_skill` | Reads one skill: its procedure, checklists, worked numbers and examples, whole or one named section. |

Ask "plan an A/B test", "why did our traffic drop?", "write the press pitch", "what should we post next month?" — the model searches, reads the skill that fits, and follows it.

## Get it

Tell the agent "add the marketing cell", or run `nodecode add nc://marketing`.

## Set it up

Nothing: the skills ship with the cell.

Optional, in the `marketing` section of `config.jsonc` (or `/settings`):

```jsonc
"marketing": {
  "groups": ["strategy", "content", "social"],  // which of the three to read
  "max_chars": 6000,                            // how much of one skill a read hands over
  "skills_dir": "~/another/skills"              // a <group>/<skill>/SKILL.md tree of your own
}
```

`skills_dir` is how the same tools read a different library: point it at any folder of `<group>/<skill>/SKILL.md` trees.

## Use it

- **Search first.** A skill is usually a whole procedure — the checklist, the template, the numbers — so one read beats a guessed answer.
- **The three groups.** `strategy` (164 skills: positioning, SEO/AEO/GEO, paid media, CRM, analytics, pricing, launches), `content` (22: long-form articles, fact-checking, style guides, humanizing, editing) and `social` (21: a content calendar, captions, carousels, images and video).
- **A long skill is taken a step at a time.** A read over `max_chars` is clipped and lists the headings further down; the next read names one (`section: "quality gates"`).
- **`/marketing`** does the same from the operator's side: `/marketing technical seo` finds, `/marketing` alone says what is in the library.
- **Scripts.** Many strategy skills were written to run a Python script (a sample-size calculator, a forecast). This cell carries the prose, not the Python: do that step by hand with the formula the skill gives, or say the script is what runs it.

## What is in it

| Path | What it is |
| --- | --- |
| `cell.lisp` | The two tools, `/marketing`, the manual on `(help :marketing)`, and the scan that reads the library. |
| `skills/` | The original's three `skills/` folders, their markdown unchanged: 207 `SKILL.md` and the guides beside them. |

## License

MIT, Copyright (c) 2026 Indranil Banerjee: see `LICENSE` and `NOTICE`.
