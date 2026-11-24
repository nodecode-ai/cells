# nodecode-ichor

Ichor: every correction you give the agent, kept as a preference pair you can train on. A Nodecode rewrite of [mystiquemide/ichor](https://github.com/mystiquemide/ichor) by [mystiquemide](https://github.com/mystiquemide).

Every "no, use pnpm", every re-sent message, every "that's not what I asked" is
labelled data: a prompt, the answer you rejected, and the answer you kept. A
session trace does not say which answer was rejected; Ichor does. When a
completed turn answers the one before it with a rejection, Ichor stores the two
answers as one pending pair -- scrubbed on your machine first, for keys, tokens,
emails, IP addresses, phone numbers and home paths -- and asks you once what to
do with it. Kept pairs export as TRL's conversational preference JSONL and as an
Atropos `pairs.jsonl`, so the corrections you give can train the next model you
run. Nothing leaves the machine unless you run `/ichor share`.

```
you     show disk usage of this folder
agent   du -sh .
you     no, make it human readable and sorted
ichor   captured pair #1 · missing option
        rejected  du -sh .
        accepted  du -sh * | sort -h
        /ichor review · keep 1 · share 1 · discard 1
```

## Get it

Tell the agent "add the ichor cell", or run:

```sh
nodecode add nc://ichor
```

## Set it up

Nothing is required. Correct the agent once in a session and the pair is there;
`/ichor review` reads it. What the `ichor` section may set:

```jsonc
{
  "ichor": {
    "capture": true,          // watch completed turns for a correction
    "dir": "",                // where pairs.jsonl and the exports live; empty keeps them under the Nodecode home
    "scrub": true,            // redact secrets, emails, IPs, phones and home paths before a pair is stored
    "hf_repo": "ichor-pairs", // the Hugging Face dataset /ichor share writes to
    "hf_private": true,       // create that dataset private
    "hf_token_env": "HF_TOKEN" // the environment variable holding the Hugging Face write token
  }
}
```

For `/ichor share`, put a Hugging Face token with write access to your own
repositories in that environment variable. Ichor reads it there and nowhere
else, sends it only to `huggingface.co`, and never stores it.

## Use it

| You do | Ichor does |
| --- | --- |
| correct the agent | records the pair, scrubbed, as pending; a notice names it |
| `/ichor` | how many pairs are kept, shared and pending, and your top mistake types |
| `/ichor review` | the oldest pending pair, as a card: what you said, rejected, accepted, what was scrubbed |
| `/ichor keep <id>` | keeps it private: it exports, it is never uploaded |
| `/ichor share <id>` | queues it for `/ichor share` |
| `/ichor discard <id>` | drops it, and the stored text with it |
| `/ichor export` | writes `dpo.jsonl` (TRL conversational: `prompt`, `chosen`, `rejected`) and `atropos_env/pairs.jsonl` under `dir` |
| `/ichor share` | uploads the queued pairs to your Hugging Face dataset, private by default |
| `/ichor off` / `/ichor on` | pauses or resumes capture in this session; `--global` for every session |
| `/ichor forget-all yes` | deletes every pair |

The same verbs are Lisp, callable from eval: `(ichor:status)`,
`(ichor:review)`, `(ichor:keep-pair 1)`, `(ichor:share-pair 1)`,
`(ichor:discard-pair 1)`, `(ichor:export-pairs)`, `(ichor:upload)`,
`(ichor:forget-all)`.

A pair is one JSON line in `pairs.jsonl`; the file is yours, plain text, and
`(help :ichor)` describes the whole thing. A mistake label comes from your own
words (`wrong package manager`, `missing option`, `wrong fact`, ...), so no
model is asked anything and no extra call is made.

## What it counts as a correction

An opener that rejects an answer ("no, ...", "wrong", "actually", "not what I
asked", "you forgot", "i said", ...), a redirect ("use X instead of Y", "that
broke", "still failing", "should be", ...) in a short message, or the same
message sent twice with a different answer, which is a retry pair. A bare "no"
to a yes-or-no question is an answer, not a correction, and "thanks, perfect"
is not one either.
