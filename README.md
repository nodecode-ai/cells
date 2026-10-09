# Nodecode cells

Every cell of [Nodecode](https://nodecode.ai), one folder each, under the
kind it is: a provider (a model subscription, a sign-in, a wire), a tool (a
verb the model or the operator calls), a room (a chat surface).

Two sorts live side by side. The ones marked **ships** are built into every
Nodecode release: the release reads them from here at the one commit its
`src/cells/bundle.json` pins, so they are never index entries. The rest are
in [`index.json`](index.json), which `nodecode add nc://NAME`, `/cells` and
the setup walk read and list after the shipped ones. [`template/`](template)
is what a new cell is copied from.


## Providers (`providers/`)

| Folder | What it does |
| --- | --- |
| [`nodecode-alibaba-coding-plan`](providers/nodecode-alibaba-coding-plan) | Alibaba Coding Plan: Model Studio's coding subscription as a Nodecode provider. |
| [`nodecode-alibaba-token-plan`](providers/nodecode-alibaba-token-plan) | QwenCloud Token Plan: Alibaba's regional token subscription as a Nodecode provider. |
| [`nodecode-amazon-bedrock`](providers/nodecode-amazon-bedrock) | Amazon Bedrock: the Converse Stream wire, SigV4 signing and the AWS event stream, on a lane of its own. |
| [`nodecode-anthropic`](providers/nodecode-anthropic) | Anthropic (Claude Pro/Max): sign in, and serve the anthropic provider on the subscription when no key is set. |
| [`nodecode-apple`](providers/nodecode-apple) | Apple Foundation Models: the on-device model, through omp's Swift bridge, as a provider lane (macOS only). |
| [`nodecode-azure`](providers/nodecode-azure) | Azure OpenAI: the Responses API on an Azure resource, with its api-version and api-key. |
| [`nodecode-claude-code`](providers/nodecode-claude-code) | Claude models through your own logged-in Claude Code CLI. Linux and macOS. |
| [`nodecode-cline`](providers/nodecode-cline) | Cline's API: the client header its free models need, and its model feed. |
| [`nodecode-cline-pass`](providers/nodecode-cline-pass) | ClinePass: Cline's model subscription as a Nodecode provider. |
| [`nodecode-cloudflare-ai-gateway`](providers/nodecode-cloudflare-ai-gateway) | Cloudflare AI Gateway: Anthropic, OpenAI and Workers AI models through one gateway. |
| [`nodecode-codex-auth`](providers/nodecode-codex-auth) | A ChatGPT subscription's login on OpenAI-family lanes. |
| [`nodecode-cursor`](providers/nodecode-cursor) | Cursor: a browser sign-in, and Cursor's Agent protocol (Connect over HTTP, protobuf) as a lane of its own. |
| [`nodecode-devin`](providers/nodecode-devin) | Devin: a browser sign-in, and Codeium's Cascade wire (Connect over protobuf) as a lane of its own. |
| [`nodecode-factory-droid`](providers/nodecode-factory-droid) | Factory Droid: Factory's model subscription as a Nodecode provider, signed in with a WorkOS device code. |
| [`nodecode-github-copilot`](providers/nodecode-github-copilot) | GitHub Copilot: a GitHub device sign-in, and Copilot's Messages, chat and Responses wires as one provider. |
| [`nodecode-gitlab-duo`](providers/nodecode-gitlab-duo) | GitLab Duo Non-Agentic: Duo's chat models through GitLab's AI gateway, as a Nodecode provider. |
| [`nodecode-gitlab-duo-agent`](providers/nodecode-gitlab-duo-agent) | GitLab Duo Agent: the Duo Workflow Service as a Nodecode provider lane. |
| [`nodecode-google-antigravity`](providers/nodecode-google-antigravity) | Antigravity: a Google sign-in, and Antigravity's Cloud Code Assist wire as a lane of its own. |
| [`nodecode-google-gemini-cli`](providers/nodecode-google-gemini-cli) | Google Cloud Code Assist: a Google sign-in, and the Gemini CLI's wire as a lane of its own. |
| [`nodecode-google-vertex`](providers/nodecode-google-vertex) | Google Vertex AI: Gemini, Claude and partner models on a Google Cloud project, with an API key or Application Default Credentials. |
| [`nodecode-kilo`](providers/nodecode-kilo) | Kilo Gateway: sign in with a device code, and serve its models. |
| [`nodecode-kimi-code`](providers/nodecode-kimi-code) | Kimi Code: Moonshot's coding subscription as a Nodecode provider, signed in with a device code. |
| [`nodecode-local`](providers/nodecode-local) | Local models: omp's tiny-model workers (ONNX, MLX) as a Nodecode provider lane. |
| [`nodecode-muse-code`](providers/nodecode-muse-code) | Muse Code: Meta's Muse subscription as a Nodecode provider, signed in with a device code. |
| [`nodecode-ollama-cloud`](providers/nodecode-ollama-cloud) | Ollama Cloud: Ollama's native /api/chat wire as a Nodecode provider lane. |
| [`nodecode-openai-codex`](providers/nodecode-openai-codex) | ChatGPT Plus/Pro (Codex subscription): sign in, and serve the Codex models. |
| [`nodecode-openai-codex-device`](providers/nodecode-openai-codex-device) | ChatGPT Plus/Pro (Codex, headless/device): sign in with a device code, and serve the Codex models. |
| [`nodecode-openrouter`](providers/nodecode-openrouter) | OpenRouter, with its browser sign-in, as a Nodecode provider. |
| [`nodecode-snowflake`](providers/nodecode-snowflake) | Snowflake Cortex: Claude and GPT models on a Snowflake account, signed in or with a PAT. |
| [`nodecode-xai-oauth`](providers/nodecode-xai-oauth) | XAI Grok through a SuperGrok or X Premium+ sign-in, as a Nodecode provider. |
| [`nodecode-xiaomi`](providers/nodecode-xiaomi) | Xiaomi MiMo: its models, on a pay-as-you-go key or a regional Token Plan key. |
| [`nodecode-zai-coding-plan`](providers/nodecode-zai-coding-plan) | Z.AI GLM Coding Plan, signed in from the browser, as a Nodecode provider. |

## Tools (`tools/`)

| Folder | What it does |
| --- | --- |
| [`nodecode-chrome`](tools/nodecode-chrome) | **ships** Drive Chrome; needs the companion extension. |
| [`nodecode-cron`](tools/nodecode-cron) | **ships** Prompts that run on a schedule. |
| [`nodecode-experience`](tools/nodecode-experience) | **ships** Learns from each turn: reflection and recaps. |
| [`nodecode-guard`](tools/nodecode-guard) | **ships** Refuses risky shell commands before they run. |
| [`nodecode-import-kit`](tools/nodecode-import-kit) | **ships** Bring another coding agent's home into this one. |
| [`nodecode-lsp`](tools/nodecode-lsp) | Language servers for Nodecode: diagnostics on write, navigation and rename verbs. |
| [`nodecode-mcp`](tools/nodecode-mcp) | **ships** Tools from MCP servers you list in the config. |
| [`nodecode-perplexity`](tools/nodecode-perplexity) | Perplexity web search: sign in with a Pro/Max account, or use a key, and search from eval. |
| [`nodecode-prs`](tools/nodecode-prs) | **ships** Pull requests ranked and reviewed minutes after each push. |
| [`nodecode-qa`](tools/nodecode-qa) | **ships** Anonymous usage counts, one page a week, sent only if you say so. |
| [`nodecode-team`](tools/nodecode-team) | **ships** Several sessions work one task together in a shared folder. |
| [`nodecode-typesafe`](tools/nodecode-typesafe) | TypeSafe: typed judgments over a state (System One), called through eval. |
| [`nodecode-web-provider`](tools/nodecode-web-provider) | Web search engines: Google, Startpage, DuckDuckGo, Ecosia, Mojeek, SearXNG and their merge, keyless. |
| [`nodecode-websearch`](tools/nodecode-websearch) | **ships** Web search and page fetch; no key needed. |

## Rooms (`rooms/`)

| Folder | What it does |
| --- | --- |
| [`nodecode-channel-discord`](rooms/nodecode-channel-discord) | **ships** Talk to nodecode through a Discord bot. |
| [`nodecode-channel-kit`](rooms/nodecode-channel-kit) | **ships** Shared plumbing for the chat bots; comes with Discord or Telegram. |
| [`nodecode-channel-slack`](rooms/nodecode-channel-slack) | Talk to Nodecode through a Slack app. |
| [`nodecode-channel-telegram`](rooms/nodecode-channel-telegram) | **ships** Talk to nodecode through a Telegram bot. |
| [`nodecode-link`](rooms/nodecode-link) | **ships** Open this machine's page from anywhere; `/link on` turns it on. |
| [`nodecode-web`](rooms/nodecode-web) | **ships** Use nodecode in a browser tab; `nodecode web` opens it. |

**What an entry is.** A folder in a public git repository, pinned at one
commit that a person here read before merging. The folder is the whole
repository, or one folder of it named by `path`, the way this repository's
own cells are listed. Installing one clones that
commit and nothing moves it afterwards. An cell runs inside Nodecode with
full access to your files and keys, so every entry is pinned and every
change to one is a new pull request that gets read the same way.

## Add yours

1. Put your cell in a public repository with its `.asd` at the top, or at
   the top of one folder of it. The
   folder contract is in Nodecode's `src/CELLS.md`; start from
   [`template/`](template). Vendor anything beyond `nodecode` and the shipped
   cells under `vendor/`. An entry never depends on another entry here.
2. Open a pull request that adds one entry to `index.json`, keeping the list
   sorted by name:

```json
{
  "name": "nodecode-weather",
  "git": "https://github.com/someone/nodecode-weather",
  "commit": "3f9a1c7e2b8d4f60a5e19c0b7d2e4a6f8c1b3d5e",
  "description": "Current weather and a 3-day forecast, as a tool the model can call.",
  "license": "MIT",
  "depends_on": ["nodecode"],
  "platforms": ["linux", "macos", "windows"]
}
```

| Field | Rule |
| --- | --- |
| `name` | Your primary system's name, which is your `.asd` file's name. a-z, 0-9, `.`, `_`, `-`. It can't be a name Nodecode ships. |
| `git` | An `https://` URL. |
| `commit` | The full 40-character commit id. A branch or a short id is refused. |
| `path` | Optional. The folder inside the repository the cell is, as `a/b`, when it isn't the whole repository. This repository's own are `providers/nodecode-x`, `tools/nodecode-x`, `rooms/nodecode-x`. |
| `description` | Exactly your primary system's `:description`. |
| `license` | Your `.asd`'s `:license`. Required. |
| `depends_on` | Exactly the systems your `.asd` depends on from outside your folder. |
| `platforms` | Optional. Leave it out when the cell runs on Linux, macOS and Windows. |

3. The `check` workflow installs every entry into a scratch home with the
   latest Nodecode release and loads it. Nodecode itself refuses an entry
   whose clone doesn't match its line: a different `HEAD`, a different
   primary system, a description or `depends_on` that isn't the `.asd`'s, or
   a symbolic link anywhere in the cell's folder.
4. A maintainer reads the code at that commit and merges.

To update your cell, open a pull request that changes `commit` (and
`description` or `depends_on` if they changed).

## Run the check yourself

```sh
.github/check.sh              # nodecode on PATH
.github/check.sh ./nodecode   # or a binary you name
```

It works in a scratch home and never touches your own `~/.nodecode`.

One cell's own tests run against a Nodecode source tree (the commit
`.github/nodecode-rev` names, read from `~/nodecode/nodecode` or
`NODECODE_SRC`), offline, in a scratch home:

```sh
.github/test-cell.sh tools/nodecode-guard
```
