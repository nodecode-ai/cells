# nodecode-google-vertex

A Nodecode cell for [Google Vertex AI](https://cloud.google.com/vertex-ai).
With it, `/connect` offers Google Vertex AI, `/models` lists the models
oh-my-pi bundles for it (Gemini, Claude, and partner models such as gpt-oss,
Grok, Llama and GLM), and a turn on `google-vertex/<model>` goes to your
project the way oh-my-pi sends it:

| Models | Wire | Address |
| --- | --- | --- |
| Gemini | GenAI | `.../publishers/google/models/<model>:streamGenerateContent?alt=sse` |
| Claude | Messages | `.../publishers/anthropic/models/<model@version>:streamRawPredict` |
| partners | chat | `.../endpoints/openapi/chat/completions` |

each under `https://<host>/v1/projects/<project>/locations/<location>`. The
host is the location's: `aiplatform.googleapis.com` for `global`,
`aiplatform.eu.rep.googleapis.com` and `aiplatform.us.rep.googleapis.com` for
the multi-regions, `<location>-aiplatform.googleapis.com` otherwise.

- every round carries the Application Default Credentials bearer, except a
  Gemini round sent with an API key, which goes to the project-less express
  address (`https://<host>/v1/publishers/google/models/...`, the global host
  unless a location is named) with `x-goog-api-key`
- a Gemini body turns the four harm categories `OFF` when it names none
- a Claude body names no model (the address does), says
  `anthropic_version: vertex-2023-10-16`, and sends no `output_config`
  (Vertex refuses an output effort)
- a partner body names its cap `max_completion_tokens` and sends no
  `prompt_cache_key`

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s
`google-vertex` provider. See [NOTICE](NOTICE).

## A lane of its own?

No. Gemini on Vertex is the GenAI wire the core's `google` lane speaks, at
another address with another credential; Claude on Vertex is the Messages
wire; the partners are the chat wire. `walk-provider-stream` takes the
address and the headers, and each lane's body hook the few fields Vertex
wants otherwise, so this cell rides the core's three lanes.

## Credentials

omp's Vertex login is no login. A round's credential is found in this order:

1. an API key `/connect` saved (the core reads `api_keys` first)
2. `GOOGLE_CLOUD_API_KEY`
3. Application Default Credentials

An API key serves Gemini only, as in omp; Claude and the partners always
ride ADC. ADC is found as omp's `google-auth.ts` finds it, with no Google
library:

1. `GOOGLE_CLOUD_ACCESS_TOKEN` or `CLOUDSDK_AUTH_ACCESS_TOKEN`, as it is
   (`gcloud auth print-access-token`)
2. the file `GOOGLE_APPLICATION_CREDENTIALS` names, else gcloud's user ADC
   (`~/.config/gcloud/application_default_credentials.json`, or under
   `%APPDATA%\gcloud` on Windows):
   - `service_account`: an RS256 JWT assertion, signed here in Lisp, traded
     at `oauth2.googleapis.com/token`
   - `authorized_user`: its refresh token traded there
   - `impersonated_service_account`: its source's token, then IAM
     Credentials' `generateAccessToken` for the target
3. the GCE or Cloud Run metadata server

A token is kept in memory and replaced a minute before it expires
(`GOOGLE_VERTEX_REFRESH_SKEW_MS` sets the margin). Nothing is written to disk.
The `GOOGLE_API_KEY` variable, the Gemini API's, never reaches Vertex.

`/connect` does not check a key: the core's listing would ask the template
host, so the picker is answered from the bundled roster without a request.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-google-vertex/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

```jsonc
{
  "google-vertex": {
    "project": "my-project",   // else GOOGLE_CLOUD_PROJECT, GCP_PROJECT, GCLOUD_PROJECT
    "location": "us-central1"  // else GOOGLE_VERTEX_LOCATION, GOOGLE_CLOUD_LOCATION, VERTEX_LOCATION
  }
}
```

An ADC round with no project or no location is refused before anything is
sent, in omp's words.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-google-vertex
```

The RS256 signature is checked against a known answer `openssl dgst -sha256
-sign` made with a throwaway key the test carries. Every Google endpoint is a
stubbed `dex:request` or `dex:post`, every ADC file a temp file.

## Not ported

- The express address's fallback to the global host when a location read
  from the environment answers 404.
- `serviceTier: priority` as the `X-Vertex-AI-LLM-Shared-Request-Type`
  header, and context caching (`cachedContent`): Nodecode asks for neither.
- Gemini's thinking budgets for the `*latest` aliases (`thinking-mode
  budget`): the core's GenAI lane sends a thinking level.

MIT licensed.
