# nodecode-insight

Insight: select a term and get a short, cited definition from your own model. A Nodecode rewrite of [BrokeSkill/Hermes-Insight](https://github.com/BrokeSkill/Hermes-Insight) by [BrokeSkill](https://github.com/BrokeSkill).

The original is a Hermes Desktop panel: select any text, and a definition appears in a popup, streaming, with sources under it. The popup is Hermes's browser-side UI and this rewrite has no popup. What it keeps is what the panel asked its backend for: one stateless completion, in your own format template, with up to two sources cited by URL and nothing else sent.

The cell offers that as two surfaces:

| Surface | What it does |
| --- | --- |
| `define_term` (tool) | The model defines a term: the answer is JSON with the definition, the model that wrote it, and the sources (title, URL, snippet). |
| `/insight <term>` (command) | You type the term; the definition comes back with its source URLs after it. |

Both reach the same function. Ask "what does idempotent mean?", or type `/insight idempotent`.

## Get it

Tell the agent "add the insight cell", or run `nodecode add nc://insight`.

## Set it up

The cell talks to any OpenAI-compatible endpoint you name — your own provider, a local server, a gateway. Two things are required, one is optional:

```jsonc
"insight": {
  "base_url": "https://api.example.com/v1",   // the cell POSTs <base_url>/chat/completions
  "model": "gpt-4.1-mini",
  "api_key_env": "OPENAI_API_KEY",           // or a file: "api_key_file": "~/.config/insight-key"
  "searxng_url": "http://localhost:8888",    // optional: what gives a definition its sources
  "template": "",                              // optional: your format template
  "search": true,
  "timeout": 120
}
```

The key is named, never written into the config: `api_key_env` holds the name of the environment variable that carries it (export it in the shell that starts Nodecode), and `api_key_file` a file only you can read. Set at most one; a local server may take neither. Without `base_url` and `model` the cell refuses at start and says which one is missing.

`searxng_url` is a [SearXNG](https://docs.searxng.org/) instance with `format=json` enabled. With one set, a definition searches it first and is written with two sources cited by URL. With none, nothing is fetched and the definition stands on the model's own knowledge.

## Use it

The model calls the tool when the ask is a definition; people type the command. From eval, the same verb:

```lisp
(insight:define "idempotent")            ; => the text, the sources, the model
(insight:run-slash "idempotent" nil)     ; => what /insight prints
```

`(help :insight)` is the model's manual for the verb, kept while the cell runs.

### The format template

`template` is the whole system instruction, so the definition comes out in a shape you chose. It is the original panel's default unless you set one, and `{term}` in it is replaced by the term:

```
**{term}** - one-sentence definition.
Then, only when useful: 2-3 short "Examples" bullets and one "Also known as" line.
```

The built-in template also carries the original's writing rules — cite only the supplied URLs, never invent one, no filler, under ~180 words — and the tool accepts a per-call `template` so the model can ask for a different shape for one definition.

### What is sent, and to whom

Only the term, your template and the two source titles and URLs go to the provider whose `base_url` you set, with the key it needs. Snippets and the rest of the source rows stay on this machine. When `searxng_url` is set, the term is the only thing sent to that instance. Nothing of the conversation travels, and nothing is remembered: every definition is one stateless request.

### What the rewrite leaves out

The popup: selection watching, the streaming render, the provider and model pickers, and the per-result thumbnail fetched from each image host and inlined as a data URI. Those are Hermes Desktop's surfaces; the cell answers with text. Also left out: Hermes's own provider inventory and its web-search tool (the cell is pointed at one endpoint by `base_url`, and takes sources from SearXNG), the follow-up extraction round when the model answers with a tool call, and the provider-qualified model ids the plugin resolved against that inventory.

## License

MIT. The backend, its format template and its citation rule are BrokeSkill's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
