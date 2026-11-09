# nodecode-jev-curator

Jev Curator: typed skill-relation judgments and hash-bound consolidation plans. A Nodecode rewrite of [anpicasso/hermes-jev-curator](https://github.com/anpicasso/hermes-jev-curator) by [anpicasso](https://github.com/anpicasso).

Point it at the folders that hold your `SKILL.md` skills. It inventories them, proposes the pairs whose text and names overlap, and asks TypeSafe's System One one typed question set per pair: are these the same skill, a subset of one another, one capability in two pieces, complementary, in conflict, or unrelated -- and behind that label, did the offered options cover the pair, would each skill's rules survive inside the other, do they disagree. Those probabilities, not the label, decide whether two skills may become one.

## Get it

Tell the agent "add the jev-curator cell", or run `nodecode add nc://jev-curator`.

## Set it up

A key is needed only for judgments: make one at <https://console.typesafe.ai/> and either save it in `auth.json` under `api_keys.typesafe` (the same entry the TypeSafe cell uses) or set `TYPESAFE_API_KEY`. Nothing is sent until you also say so: `egress` is the consent, and judgements are only asked while `mode` is not `off`.

```jsonc
{
  "jev-curator": {
    "roots": ["~/.nodecode/skills"],
    "mode": "observe",
    "egress": true,
    "protected": ["deploy"],
    "top_k": 5,
    "max_pairs": 100
  }
}
```

`mode` is `off` (nothing asked), `observe` (judge and plan, change nothing) or `apply` (an apply may also move a folder). `protected` names the skills an apply never touches. `base_url` and `model` are optional: empty reads `TYPESAFE_BASE_URL` and `TYPESAFE_DEFAULT_MODEL`, else `https://api.typesafe.ai` and `jev-latest`. A `base_url` of your own, such as a local judgment server, need not take a key.

## Use it

`/curator` says where the library and the key stand, and offers the same lines to type: `/curator inventory`, `/curator scan`, `/curator plans`, `/curator judge A B`, `/curator apply ID`. The model calls the same work through eval -- `(help :jev-curator)` is the manual:

```lisp
(curator:inventory)                     ; every skill, its digest and size
(curator:candidates)                    ; the pairs worth judging, and why
(curator:judge "deploy" "deploy-logs")  ; one typed judgment, as a plist
(curator:plans)                         ; the merge plans this evidence supports
(curator:apply-plan "merge-1a2b3c4d5e6f")
```

A plan is star-shaped and hash-bound: every skill it would absorb carries its own direct judgment against the plan's canonical, never a chain, and the plan's id is a hash of the digests and judgments it was built from -- so a skill that changed after the plan refuses to apply and asks for a rescan. An apply never deletes: it re-checks every digest and moves each absorbed skill's folder into `~/.nodecode/jev-curator/archived/`, writing one line to `apply.log` there.

The evidence policy is versioned in code, not configurable: an absorbing relation needs confidence 0.85, coverage 0.70, conflict at most 0.15, and the absorbed skill's rules surviving at 0.90. A pair too large for one request is refused rather than truncated.

## What it asks, and what it sends

One `POST /v1/systemone` per pair, to TypeSafe's host and nowhere else: both skills' names, descriptions and text, redacted of this cell's own key, under a contract line telling the judge the texts are data. The HTTP client is `nlk:http`; the network is reached only while `mode` is not `off` and `egress` is true.
