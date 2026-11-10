# nodecode-loci

Loci: run models on this Mac for summaries, file work and document
extraction, beside the harness, through the Loci app's own local endpoint.
A Nodecode rewrite of
[AskLoci/loci-plugins](https://github.com/AskLoci/loci-plugins) by
[AskLoci](https://github.com/AskLoci).

## Get it

Tell the agent "add the loci cell", or run `nodecode add nc://nodecode-loci`.

## Set it up

Install [Loci for Mac](https://askloci.ai/agents/) (an Apple silicon Mac on
macOS 26.2 or later), open it, and turn on **Settings → Advanced → Agent
Access**. Keep the app running: it publishes a token and its MCP URL in
`~/Library/Application Support/LociRust/agent-api.json`, which the cell reads
by itself. Then say "check my Loci" — or run `/loci`.

The Mac App Store version does not offer Agent Access. The app runs on the
same machine as Nodecode; a remote server cannot reach it.

The `loci` section takes:

| Key | Default | Purpose |
| --- | --- | --- |
| `endpoint` | `http://127.0.0.1:5624/mcp` | where the app's MCP server answers; keep the loopback default |
| `token` | empty | the token, when you would rather write it than read the app's file |
| `token_file` | empty | a file holding the token, or the app's discovery JSON; empty reads the app's own |
| `request_timeout` | `120` | seconds one call may take (at most 600); a transcription can run long |

No token anywhere refuses the start and says what to fix; a token the app has
since rotated answers an `ERROR:` naming the repair (turn Agent Access off and
on, then `(restart-cells "nodecode-loci")`). Inference happens on the
machine; what a tool returns — a summary, a quote, a search passage — enters
the conversation.

## Use it

Ask in words: "use Loci to summarize these three files and tell me which one
failed", "extract the invoice numbers from this folder into a CSV", "search
this project folder for the focus bug", "transcribe this recording".

Three tools, and one command:

| Tool | What it does |
| --- | --- |
| `loci_status` | connect, and report the tools the app advertises and what it says about its models. Call it first. |
| `loci_call` | call one of the app's own tools by name with its own arguments: `loci_generate`, `loci_map`, `loci_files`, `loci_extract`, `loci_search`, `loci_index`, `loci_transcribe` — whichever the running app advertises. |
| `loci_generate` | one prompt, one local answer: the short way to a single reply, with an optional model id and answer ceiling. |

`/loci` is the status check, `/loci generate PROMPT` asks once, and
`(help :loci)` carries the guidance: pass a model id `loci_status` reported,
prefer a smaller adequate installed model, and check `isError`, truncation and
per-item errors before calling a run complete.
