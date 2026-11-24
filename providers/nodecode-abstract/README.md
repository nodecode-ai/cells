# nodecode-abstract

Abstract: GLM-5.3, DeepSeek-V4-Flash and Kimi-K3 over the Responses API, as a Nodecode model provider. A Nodecode rewrite of [abstractpbc/hermes-abstract-provider](https://github.com/abstractpbc/hermes-abstract-provider) by [abstractpbc](https://github.com/abstractpbc).

## Get it

Tell the agent "add the abstract cell", or run `nodecode add nc://abstract`.

## Set it up

Make a key at <https://platform.abstract.inc/>, then save it with `/connect`
(provider `abstract`) or set `ABSTRACT_API_KEY`. A saved key wins over the
variable.

```sh
export ABSTRACT_API_KEY="your-api-key"
```

Optional: the section names another base when a relay serves the API, on the
section's `base_url` (`https://api.abstract.inc/v1` by default).

## Use it

`/models` lists the three models and a turn runs on the one picked:

| Model | Input $/M | Cached input $/M | Output $/M |
|---|---|---|---|
| `glm-5.3:superfast` | 1.19 | 0.26 | 4.40 |
| `deepseek-v4-flash:superfast` | 0.10 | 0.05 | 0.25 |
| `kimi-k3:superfast` | 3.00 | 0.30 | 12.75 |

Prices are per million tokens, as the original's README publishes them. Input
is text only, tools are taken, and one response stops at 65,536 output tokens.

The roster is static, as it is in the original: Abstract's own `/models` also
names background lanes this provider does not offer, so `/models` asks the
provider nothing and lists these three. No context window is published for
any of them, so the cell leaves it unknown rather than inventing a number;
Nodecode learns the real window from the provider's own overflow reply.

A round with no key refuses before it goes out, naming `ABSTRACT_API_KEY`.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-abstract
```

MIT licensed. The original's licence and what the rewrite left out are in
[NOTICE](NOTICE).
