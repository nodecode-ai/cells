# nodecode-antidote

LLM Antidote: runtime guardrails on what a tool call reads and what it sends. A Nodecode rewrite of [GroveMinting/llmAntidote](https://github.com/GroveMinting/llmAntidote) by [GroveMinting](https://github.com/GroveMinting).

## Get it

Tell the agent "add the antidote cell", or run `nodecode add nc://antidote`.

## Set it up

Nothing is needed to start: the defaults guard a session as soon as the cell is on. Its settings are in the `antidote` section of `~/.nodecode/config.jsonc`, or in `/settings`.

| Key | Default | What it does |
| --- | --- | --- |
| `mode` | `"balanced"` | `balanced` asks, `strict` refuses, `audit` only records. |
| `ask` | `true` | In balanced mode, ask Allow once or Deny in the web page or the desktop app. With nobody there, the call is refused. |
| `ask_seconds` | `120` | How long a question waits for an answer. |
| `taint_minutes` | `60` | How long a sensitive read, once let through, keeps gating an egress-capable call. |
| `max_scan_bytes` | `1048576` | The longest call text read; a longer call is refused rather than half-read. |
| `sensitive` | none | Extra regular expressions naming material to treat as sensitive. |
| `allow_egress` | none | Tool names exempt from the network rules, for a tool that is meant to send. |
| `audit_capacity` | `200` | How many decisions `/antidote status` keeps, newest first. |

## Use it

It is a hook, not a command you call: every tool call is judged before it runs, and a call that does not run hands the model a refusal naming the rule.

- **What it reads as sensitive**: a dotenv or environment read (`.env`, `printenv`, `getenv`), a credential directory (`.ssh`, `.aws`, `.kube`, `.gnupg`, `.docker`, gcloud, Azure), an authentication file (`id_rsa`, `id_ed25519`, `credentials.json`, a service-account key).
- **What it reads as egress**: a shell command that can reach the network (`curl`, `wget`, `nc`, `scp`, `sftp`, `rsync`, `socat`, an `http` URL), or a Lisp call that can (`nlk:http`, `dexador`, `url-request`), and the bodies an upload adds (`-d`, `--data`, `-F`, `-T`, `-X POST`, `:method :post`).
- **What is refused whatever the mode**: a reverse shell, a download piped into a shell, a call that carries sensitive material and egress at once, a call that reaches the network after a sensitive read was allowed, and a call too long to read whole.
- **Balanced** puts a question up for the first two kinds and refuses if nobody answers; **strict** refuses them; **audit** allows everything and records what strict would have refused.

`/antidote status` shows the mode, whether a sensitive read is remembered and for how long, and the last decisions — the rule, the tool and the action, never the text of a call. `/antidote clear` forgets the remembered read and the record. The manual topic is `(help :antidote)`.

The memory is the image's, not a session's: one Nodecode process serves every session, and it is cleared by `clear`, by `taint_minutes` passing, or by a restart of the cell.

**A speed bump, not a security boundary.** It reads the text of a call, so a command assembled from pieces it cannot see is a command it cannot judge. It closes the ordinary chain — read a credential, then send it somewhere — and the common download-and-run shapes.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The whole cell: the rule family, the policy, the `:tool` hook, `/antidote` and the manual topic. |
| `test/` | The tests: lifecycle, each rule class, the allow/deny seam, the taint, the scan limit and the command. |

## Testing

From a Nodecode checkout, `.github/test-cell.sh tools/nodecode-antidote`, offline.

## License

MIT, Copyright (c) 2026 Grove Minting Company: see `LICENSE` and `NOTICE`.
