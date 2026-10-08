# nodecode-azure

A Nodecode cell for [Azure OpenAI](https://learn.microsoft.com/azure/ai-foundry/openai/).
With it, `/connect` offers Azure OpenAI, `/models` lists the OpenAI models
oh-my-pi bundles for it (GPT-4 through GPT-6, the o-series, Codex), and a turn
on `azure/<model>` goes to your resource's Responses API the way oh-my-pi's
`azure-openai-responses` transport, which mirrors the AzureOpenAI SDK client,
sends it:

- the address is `<base>/responses?api-version=<version>`, the base
  `https://<resource>.openai.azure.com/openai/v1` and the version `v1` unless
  set otherwise
- the key rides as a single `api-key` header, never as a bearer
- the body names the deployment: the model id, unless a deployment map
  names another
- every function tool says `strict: false`
- GPT-6 Astra beside function tools asks for `reasoning.effort: none`, the
  provider rule omp keeps for it

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `azure`
provider. See [NOTICE](NOTICE).

## A lane of its own?

No. The wire is the Responses API's, and what Azure changes is the address,
one header and the model name: `walk-provider-stream` takes the endpoint and
the headers, `responses-request-body` the body. So this cell rides the core's
`openai-responses` lane through those hooks, and the catalog row names that
lane's package (`@ai-sdk/openai`). models.dev's own `azure` row names
`@ai-sdk/azure`, which no lane speaks; its models are kept under the cell's.

## Key

omp's Azure login is a plain key. Save it with `/connect`, or set
`AZURE_OPENAI_API_KEY` (omp's variable); the core's own `AZURE_API_KEY` is read
after it. A saved key wins over both. `/connect` does not check the key: the
core's listing would send it as a bearer, which Azure refuses, so the picker
is answered from the bundled roster without a request.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-azure/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

```jsonc
{
  "azure": {
    // https://<resource_name>.openai.azure.com/openai/v1
    "resource_name": "my-resource",
    // or any other base; outranks resource_name
    // "base_url": "https://my-gateway.example/openai/v1",
    "api_version": "v1",
    "deployment_map": "gpt-5.5=prod-gpt55,gpt-6-sol=sol-east"
  }
}
```

Each setting falls back to omp's variable when the section leaves it out:
`AZURE_OPENAI_BASE_URL`, `AZURE_OPENAI_RESOURCE_NAME`,
`AZURE_OPENAI_API_VERSION`, `AZURE_OPENAI_DEPLOYMENT_NAME_MAP`. The base is
found in omp's order: the section's `base_url`, `AZURE_OPENAI_BASE_URL`, then
a resource name, the section's or the variable's. A round with no base is
refused before anything is sent, in omp's words.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-azure
```

## Not ported

- The computer-use tool (`{type: "computer"}`): Nodecode has none.
- omp's reasoning-effort fallback, which retries a request whose effort the
  deployment refused at a lower one.
- The first-event watchdog and its `X-Stainless-Timeout` header; the core's
  idle deadline stands in.

MIT licensed.
