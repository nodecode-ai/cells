# nodecode-attack-frameworks

Five LLM-driven attack frameworks that have already turned up in real
intrusions, with what each one is and what it leaves in a log.

The facts are one post by [DFIR Radar](https://x.com/DFIR_Radar) (13 November
2026), read against each repository's own GitHub metadata the next day. The
post's own finding is why the cell is worth having: **none of the five
enforces authorization in code by default**, and artifacts like a User-Agent or
a port move between releases while the behaviour does not.

| framework | repository | stars | licence |
|---|---|---|---|
| PentAGI | [vxcontrol/pentagi](https://github.com/vxcontrol/pentagi) | 34,149 | MIT |
| HexStrike | [0x4m4/hexstrike-ai](https://github.com/0x4m4/hexstrike-ai) | 13,588 | MIT |
| CyberStrike AI | [AIPentest/CyberStrikeAI](https://github.com/AIPentest/CyberStrikeAI) | 8,629 | Apache-2.0 |
| Cairn | [oritera/Cairn](https://github.com/oritera/Cairn) | 5,144 | AGPL-3.0 |
| ARTEX | [jiwoochris/artex-global](https://github.com/jiwoochris/artex-global) | 3,298 | AGPL-3.0 |

## The verbs

```
attack_frameworks_list     all five, or those under one licence
attack_frameworks_get      one whole: what it is, how it fails to ask, who saw it, what it leaves
attack_frameworks_hunt     only the huntable defaults, one line each
attack_frameworks_search   any word: a name, a repository, a hunt string, an actor
attack_frameworks_summary  how many, how starred, and the post's reading
```

`/attack-frameworks` answers the same four ways:
`/attack-frameworks` (summary), `/attack-frameworks hunt`, `/attack-frameworks
PentAGI`, `/attack-frameworks agpl`, `/attack-frameworks 8888`.

`attack_frameworks_hunt` is the one to reach for in an incident: it prints only
the strings a log search wants - `HexStrike-HTTP-Framework/1.0`,
`artex-enrich/1.0`, `update.pentagi[.]com`, the port range `28000-29999`,
`<hexstrikeXSSTest123>`, the persistence names - grouped by framework.

## What it is not

- **Not a signature set.** The post says it itself: ARTEX logged about 390
  commits in ten and a half weeks, and several of these write or install tools
  mid-task. A hunt string is a lead.
- **Not an endorsement, and not an authorization.** These are attack tools.
  Describing one, or running one against an estate you own, is the use; aiming
  one at somebody else's is a legal question the cell does not answer.
- **Not code from those projects.** Nothing of theirs is redistributed here.
  Two of the five are AGPL-3.0, so that matters; see NOTICE.

## Sources

- DFIR Radar, [the post](https://x.com/DFIR_Radar/status/2121287724430774519),
  13 November 2026: the frameworks, their posture, the hunt strings, the
  revocation note and the detection advice.
- Each repository's GitHub API record, read 14 November 2026: stars, licence,
  language, last push, creation date.
