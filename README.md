# Nodecode add-ons

The index of add-ons for [Nodecode](https://nodecode.ai) that don't ship with
it. `/setup` → **Choose** reads [`index.json`](index.json) and lists these
after the add-ons Nodecode ships.

Nodecode's own add-ons of this kind live here, one folder each:

| Folder | What it does |
| --- | --- |
| [`nodecode-alibaba-coding-plan`](nodecode-alibaba-coding-plan) | Alibaba Coding Plan: Model Studio's coding subscription as a Nodecode provider. |
| [`nodecode-alibaba-token-plan`](nodecode-alibaba-token-plan) | QwenCloud Token Plan: Alibaba's regional token subscription as a Nodecode provider. |
| [`nodecode-amazon-bedrock`](nodecode-amazon-bedrock) | Amazon Bedrock: the Converse Stream wire, SigV4 signing and the AWS event stream, on a lane of its own. |
| [`nodecode-anthropic`](nodecode-anthropic) | Anthropic (Claude Pro/Max): sign in, and serve the anthropic provider on the subscription when no key is set. |
| [`nodecode-apple`](nodecode-apple) | Apple Foundation Models: the on-device model, through omp's Swift bridge, as a provider lane (macOS only). |
| [`nodecode-azure`](nodecode-azure) | Azure OpenAI: the Responses API on an Azure resource, with its api-version and api-key. |
| [`nodecode-channel-slack`](nodecode-channel-slack) | Talk to Nodecode through a Slack app. |
| [`nodecode-claude-code`](nodecode-claude-code) | Claude models through your own logged-in Claude Code CLI. Linux and macOS. |
| [`nodecode-cline`](nodecode-cline) | Cline's API: the client header its free models need, and its model feed. |
| [`nodecode-cline-pass`](nodecode-cline-pass) | ClinePass: Cline's model subscription as a Nodecode provider. |
| [`nodecode-cloudflare-ai-gateway`](nodecode-cloudflare-ai-gateway) | Cloudflare AI Gateway: Anthropic, OpenAI and Workers AI models through one gateway. |
| [`nodecode-codex-auth`](nodecode-codex-auth) | A ChatGPT subscription's login on OpenAI-family lanes. |
| [`nodecode-cursor`](nodecode-cursor) | Cursor: a browser sign-in, and Cursor's Agent protocol (Connect over HTTP, protobuf) as a lane of its own. |
| [`nodecode-devin`](nodecode-devin) | Devin: a browser sign-in, and Codeium's Cascade wire (Connect over protobuf) as a lane of its own. |
| [`nodecode-factory-droid`](nodecode-factory-droid) | Factory Droid: Factory's model subscription as a Nodecode provider, signed in with a WorkOS device code. |
| [`nodecode-github-copilot`](nodecode-github-copilot) | GitHub Copilot: a GitHub device sign-in, and Copilot's Messages, chat and Responses wires as one provider. |
| [`nodecode-gitlab-duo`](nodecode-gitlab-duo) | GitLab Duo Non-Agentic: Duo's chat models through GitLab's AI gateway, as a Nodecode provider. |
| [`nodecode-gitlab-duo-agent`](nodecode-gitlab-duo-agent) | GitLab Duo Agent: the Duo Workflow Service as a Nodecode provider lane. |
| [`nodecode-google-antigravity`](nodecode-google-antigravity) | Antigravity: a Google sign-in, and Antigravity's Cloud Code Assist wire as a lane of its own. |
| [`nodecode-google-gemini-cli`](nodecode-google-gemini-cli) | Google Cloud Code Assist: a Google sign-in, and the Gemini CLI's wire as a lane of its own. |
| [`nodecode-google-vertex`](nodecode-google-vertex) | Google Vertex AI: Gemini, Claude and partner models on a Google Cloud project, with an API key or Application Default Credentials. |
| [`nodecode-kilo`](nodecode-kilo) | Kilo Gateway: sign in with a device code, and serve its models. |
| [`nodecode-kimi-code`](nodecode-kimi-code) | Kimi Code: Moonshot's coding subscription as a Nodecode provider, signed in with a device code. |
| [`nodecode-local`](nodecode-local) | Local models: omp's tiny-model workers (ONNX, MLX) as a Nodecode provider lane. |
| [`nodecode-lsp`](nodecode-lsp) | Language servers for Nodecode: diagnostics on write, navigation and rename verbs. |
| [`nodecode-muse-code`](nodecode-muse-code) | Muse Code: Meta's Muse subscription as a Nodecode provider, signed in with a device code. |
| [`nodecode-ollama-cloud`](nodecode-ollama-cloud) | Ollama Cloud: Ollama's native /api/chat wire as a Nodecode provider lane. |
| [`nodecode-openai-codex`](nodecode-openai-codex) | ChatGPT Plus/Pro (Codex subscription): sign in, and serve the Codex models. |
| [`nodecode-openai-codex-device`](nodecode-openai-codex-device) | ChatGPT Plus/Pro (Codex, headless/device): sign in with a device code, and serve the Codex models. |
| [`nodecode-openrouter`](nodecode-openrouter) | OpenRouter, with its browser sign-in, as a Nodecode provider. |
| [`nodecode-perplexity`](nodecode-perplexity) | Perplexity web search: sign in with a Pro/Max account, or use a key, and search from eval. |
| [`nodecode-snowflake`](nodecode-snowflake) | Snowflake Cortex: Claude and GPT models on a Snowflake account, signed in or with a PAT. |
| [`nodecode-typesafe`](nodecode-typesafe) | TypeSafe: typed judgments over a state (System One), called through eval. |
| [`nodecode-web-provider`](nodecode-web-provider) | Web search engines: Google, Startpage, DuckDuckGo, Ecosia, Mojeek, SearXNG and their merge, keyless. |
| [`nodecode-xai-oauth`](nodecode-xai-oauth) | XAI Grok through a SuperGrok or X Premium+ sign-in, as a Nodecode provider. |
| [`nodecode-xiaomi`](nodecode-xiaomi) | Xiaomi MiMo: its models, on a pay-as-you-go key or a regional Token Plan key. |
| [`nodecode-zai-coding-plan`](nodecode-zai-coding-plan) | Z.AI GLM Coding Plan, signed in from the browser, as a Nodecode provider. |

**What an entry is.** A folder in a public git repository, pinned at one
commit that a person here read before merging. The folder is the whole
repository, or one folder of it named by `path`, the way this repository's
own add-ons are listed. Installing one clones that
commit and nothing moves it afterwards. An add-on runs inside Nodecode with
full access to your files and keys, so every entry is pinned and every
change to one is a new pull request that gets read the same way.

## Add yours

1. Put your add-on in a public repository with its `.asd` at the top, or at
   the top of one folder of it. The
   folder contract is in Nodecode's `src/ADDONS.md`; start from
   `src/addons/template/`. Vendor anything beyond `nodecode` and the shipped
   add-ons under `vendor/`. An entry never depends on another entry here.
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
| `path` | Optional. The folder inside the repository the add-on is, as `a/b`, when it isn't the whole repository. |
| `description` | Exactly your primary system's `:description`. |
| `license` | Your `.asd`'s `:license`. Required. |
| `depends_on` | Exactly the systems your `.asd` depends on from outside your folder. |
| `platforms` | Optional. Leave it out when the add-on runs on Linux, macOS and Windows. |

3. The `check` workflow installs every entry into a scratch home with the
   latest Nodecode release and loads it. Nodecode itself refuses an entry
   whose clone doesn't match its line: a different `HEAD`, a different
   primary system, a description or `depends_on` that isn't the `.asd`'s, or
   a symbolic link anywhere in the add-on's folder.
4. A maintainer reads the code at that commit and merges.

To update your add-on, open a pull request that changes `commit` (and
`description` or `depends_on` if they changed).

## Run the check yourself

```sh
tools/check.sh              # nodecode on PATH
tools/check.sh ./nodecode   # or a binary you name
```

It works in a scratch home and never touches your own `~/.nodecode`.
