# nodecode-on-brand

On-Brand: brand voice and compliance rules a deterministic linter enforces. A Nodecode rewrite of [Mhiah/On-Brand](https://github.com/Mhiah/On-Brand) by [Mhiah](https://github.com/Mhiah).

A prompt can ask a model to write in a brand's voice; it still slips — a "we"
in a third-person brand, a banned buzzword, "guaranteed returns" in a crypto
post, a missing NFA. On-Brand moves those rules out of the prompt and into
the harness. Every draft is scored by a rule-based linter, so the same text
always gets the same verdict and the model never grades its own work. While a
brand is active its rules ride every model request, and a tool call that
sends, posts or publishes off-brand copy is refused before it goes out.

## Get it

Tell the agent "add the on-brand cell", or run `nodecode add nc://on-brand`.

## Set it up

One required setting, the folder the profiles are kept in:

```jsonc
{
  "on-brand": {
    "enabled": true,
    "store_dir": "~/.nodecode/on-brand/",   // profiles and the check log, one file
    "gate": true,                           // refuse off-brand outbound tool calls
    "gate_patterns": []                     // tool names that send; the built-in list when empty
  }
}
```

`store_dir` has no default on purpose: the profiles are yours, kept where you
say. Without it the cell refuses at start and names the member. The section is
in `/settings` too. Nothing here reaches the network and there is no key.

| Member | Default | What it does |
| --- | --- | --- |
| `store_dir` | none (required) | the folder the brand profiles and the check log live in |
| `gate` | `true` | refuse a tool call that sends, posts or publishes copy breaking a hard rule |
| `gate_patterns` | the built-in list | the tool-name patterns that count as outbound (`send`, `post`, `tweet`, `publish`, `reply`, ...) |

## Use it

```
/on-brand starter        # two example brands to try
/on-brand use nimbus     # turn enforcement on for one
/on-brand check We are going to the moon!!! GUARANTEED returns. LFG
```

```
nimbus: 0/100: first person (we, we're) in a third-person brand; banned word
'to the moon'; banned word 'LFG'; 3 exclamation marks, limit is 0; Crypto /
Web3 promotion: promises guaranteed returns; Crypto / Web3 promotion: add 'NFA'
or 'DYOR' when talking about price, tokens or returns
```

Or just describe the brand: *"Set up an on-brand profile for Acme Labs: third
person, calm and precise, never say synergy or game-changer, max 280
characters, crypto compliance."* The model calls `onbrand_rules` and saves it.

### The four tools

| Tool | What the model uses it for |
| --- | --- |
| `onbrand_check` | score copy 0-100 against a brand and list every rule it breaks; `fix=true` also answers a corrected version with the pinned spellings applied |
| `onbrand_rules` | create or update a profile: person, tone, banned words, preferred spellings, required phrases, packs, limits, examples |
| `onbrand_learn` | draft a profile from 2 or more approved posts: the person, the emoji, hashtag and exclamation ceilings, the exact spelling of the name and whether the crypto pack applies |
| `onbrand_voiceover` | turn on-brand copy into a script that reads aloud: no markdown or links, numbers and money spelled out, optional SSML pauses |

### The rules

A profile carries the person (third, first, any), a tone, banned words, exact
spellings, required phrases, the character / hashtag / emoji / exclamation
limits, sample lines, and compliance packs:

- **crypto**: no guaranteed returns, no "risk-free", no "100x" or "to the
  moon", no urgent buy; `NFA` or `DYOR` once the copy talks price, tokens or
  yield.
- **health**: no cure claims, no rapid weight-loss promises, no guaranteed
  results; "not medical advice" once the copy makes a health claim.

Hard rules (person, banned words, required phrases, length, compliance) fail a
draft. Soft rules (spellings, hashtag, emoji and exclamation limits) lower the
score and are repaired when the copy is fixed.

### `/on-brand`

```
/on-brand                      status: active brand, the gate, the totals
/on-brand list                 every brand profile
/on-brand use BRAND            turn enforcement on for a brand
/on-brand off                  turn enforcement off
/on-brand show [BRAND]        a brand's rules
/on-brand new BRAND            create an empty profile
/on-brand starter              add two example profiles to try
/on-brand set BRAND FIELD VALUE  person, tone, display_name, max_chars,
                               max_hashtags, max_emojis, max_exclamations
/on-brand ban BRAND WORD ...   /on-brand unban BRAND WORD
/on-brand spell BRAND WRONG RIGHT
/on-brand require BRAND PHRASE
/on-brand pack BRAND crypto|health [off]
/on-brand check TEXT           score TEXT against the active brand
/on-brand fix TEXT             the pinned spellings, corrected
/on-brand log                  the last checks, fixes and blocks
/on-brand gate on|off          where the gate switch lives
/on-brand delete BRAND
```

The same verbs are callable through eval: `(on-brand:check "text")`,
`(on-brand:rules "acme")`, `(on-brand:learn "acme" '("..."))`,
`(on-brand:voiceover "text")`; each answers a string, and each refusal is
`ERROR: ON-BRAND-ERROR` with the reason.

### What is enforced, and where

- **Every request.** While a brand is active, a standing harness section
  carries its rules and asks for copy in ` ```copy ` blocks, so a first draft
  starts on brand instead of being corrected afterwards.
- **Every outbound call.** A tool whose name sends, posts, publishes, replies
  or messages is checked at the `:tool` point: copy that breaks a hard rule
  fails the call, with every rule named, and the model rewrites and retries.
  Clean copy goes through; `onbrand_*` tools and reads are never gated.
- **The log.** `/on-brand log` reads the last checks, fixes and blocks, with
  the score each scored.

## Tests

```sh
.github/test-cell.sh tools/nodecode-on-brand
```

Fourteen tests, offline: the lifecycle and its refusals, the linter through
the tool seam, a verdict that does not move, a profile that outlives a
restart, `/on-brand`, the standing section, the gate, learn and the voiceover
script. Nothing here dials anything: a check is a linter, not a service.
