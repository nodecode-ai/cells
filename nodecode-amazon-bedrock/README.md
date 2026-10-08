# nodecode-amazon-bedrock

A Nodecode cell for [Amazon Bedrock](https://aws.amazon.com/bedrock/). With
it, `/connect` offers Amazon Bedrock, `/models` lists the 212 models oh-my-pi
bundles for it (Claude, Nova, Llama, Mistral, DeepSeek, gpt-oss, Qwen, Kimi,
GLM, Grok and the GPT SKUs, in their cross-region inference profiles), and a
turn on `amazon-bedrock/<model>` goes to the Converse Stream API the way
oh-my-pi sends it, with no AWS SDK:

- the address is `https://bedrock-runtime.<region>.amazonaws.com/model/<model>/converse-stream`,
  the model id escaped in the path (`:` is `%3A`, an ARN's `/` is `%2F`)
- the request is signed with AWS Signature Version 4 (service `bedrock`,
  the payload's SHA-256 signed too), or carries a Bedrock API key as
  `Authorization: Bearer`
- the answer is AWS's binary event stream
  (`application/vnd.amazon.eventstream`), decoded frame by frame with both
  CRC-32s checked
- the request is Converse's: messages with text, images, signed
  `reasoningContent`, `toolUse` and `toolResult` blocks (consecutive results
  in one user message); the system prompt; `inferenceConfig`; `toolConfig`;
  `guardrailConfig`; and the thinking each model's row asks for in
  `additionalModelRequestFields`

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s
`amazon-bedrock` provider (`bedrock-converse-stream`). See [NOTICE](NOTICE).

## A lane of its own

None of the core's four lanes speaks Converse, and the core's stream walk
reads server-sent event lines, not binary frames. So this cell registers a
lane, `amazon-bedrock` (family `:amazon-bedrock`, package
`nodecode-amazon-bedrock`), on start and takes it out on stop. Its stream
function (`call-bedrock-streaming`, wire.lisp) answers the lane contract
`nle::define-provider-lane` documents: `(values MESSAGE USAGE FINISH-REASON
REQUEST-JSON)`, MESSAGE chat-shaped, every part streamed on the way. It keeps
the core's transport bracket by calling it: the cancellable POST and its
failure classes (`nle::cancellable-post`), the idle deadline
(`nle::with-idle-cut`), the socket closed on every exit
(`nle::close-provider-stream-body`).

What the wire does, per omp:

- **thinking**, by the model's row: adaptive Claude gets `thinking: {type:
  adaptive, display: summarized}` (display where the model takes it) and
  `output_config.effort`; the GPT and Grok SKUs `reasoning.effort`; every
  other reasoning model a token budget (`minimal` 1024 up to `xhigh`/`max`
  32768) with `display: summarized`, the cap raised past it when it would
  leave the answer no room. Opus 5.5 and the other prefix-bound models bind
  their thinking with `prefix_mismatch_behavior: drop_block` and the
  `thinking-binding-controls-2026-08-01` beta, so a history whose prefix
  changed (an eviction) drops a bound thought instead of failing; a 400 that
  says a thought is bound to a different conversation is retried once with
  the thoughts taken out.
- **replay**: a signed thought replays as `reasoningContent` with its
  signature; an unsigned one (DeepSeek, gpt-oss) replays as text, bare for
  Claude and in a `<think>` or `<thinking>` block for the rest, never as
  reasoning, which Bedrock refuses.
- **cache points** where the row says caching is explicit: one after the
  final user message, then one after the system prompt
  (`AWS_BEDROCK_FORCE_CACHE` places them for any model).
- **tools**: each a `toolSpec` (no empty description); `auto` and
  `required` as Converse's choices; a model that refuses forced tool use, or
  thinking beside it, gets `auto`; a request with tool blocks in its history
  but no tools carries omp's `__no_tools__` placeholder, whose calls are
  never surfaced.
- **sampling** controls only where the model takes them.
- **stops**: `end_turn` and `stop_sequence` are stop, `max_tokens` and
  `model_context_window_exceeded` length, `tool_use` tool calls; a guardrail
  or content filter stop fails the round in omp's words. An in-stream
  exception fails it with the HTTP status of its shape in the bedrock-runtime
  service model (`throttlingException` 429, `internalServerException` 500,
  ...), so the retry policy reads it; a 401 or 403 drops the cached AWS
  credentials so the next attempt resolves them afresh.
- **usage** from the `metadata` event: input (cache-exclusive), output,
  cache reads and writes.

## Credentials

omp's Bedrock login is no login. A round's credential is found in this
order:

1. a Bedrock API key `/connect` saved (the core reads `api_keys` first), sent
   as a bearer
2. `AWS_BEARER_TOKEN_BEDROCK`, sent as a bearer
3. the AWS credential chain, as omp's `aws-credentials.ts` resolves it, signed
   with SigV4:
   1. `AWS_ACCESS_KEY_ID` + `AWS_SECRET_ACCESS_KEY` (+ `AWS_SESSION_TOKEN`)
   2. web identity: `AWS_WEB_IDENTITY_TOKEN_FILE` + `AWS_ROLE_ARN`, traded at
      STS `AssumeRoleWithWebIdentity`
   3. the profile (the section's, else `AWS_PROFILE`, else `default`) in
      `~/.aws/credentials` and `~/.aws/config` (their paths from
      `AWS_SHARED_CREDENTIALS_FILE` and `AWS_CONFIG_FILE`): static keys; SSO,
      from the AWS CLI's cached token in `~/.aws/sso/cache`, refreshed through
      SSO OIDC and written back when it is due, then `GetRoleCredentials`;
      `credential_process`; or `role_arn` chained through `source_profile`,
      `web_identity_token_file` or `credential_source`, traded at STS
      `AssumeRole` signed with the base credentials. MFA-gated roles are
      refused, as non-interactive resolution cannot supply a code.
   4. ECS / container credentials (`AWS_CONTAINER_CREDENTIALS_RELATIVE_URI` or
      `_FULL_URI`, with `AWS_CONTAINER_AUTHORIZATION_TOKEN[_FILE]`)
   5. EC2 IMDSv2, unless `AWS_EC2_METADATA_DISABLED` is `true`

Resolved credentials are kept in memory per profile and region and replaced
a minute before they expire; session keys read from the credentials file are
kept five minutes, since tools rotate them in place. `AWS_BEDROCK_SKIP_AUTH`
signs with dummy keys, for a gateway that does its own auth.

`/connect` asks no network: a key is not checked (the picker is answered from
the bundled roster, Bedrock's own listing being a signed control-plane
call), and the status reads `aws` when the environment or the profile files
name a credential source.

## Region

The region is the section's, else the one an ARN model id names. Otherwise a
cross-region inference profile (`us.`, `us-gov.`, `eu.`, `apac.`, `au.`,
`jp.`) takes the ambient region (`AWS_REGION`, `AWS_DEFAULT_REGION`, the
profile's `region` when the config file is read) only when that region can
serve its geo, else a guardrail ARN's region of that geo, else the geo's
default (`us-east-1`, `us-gov-west-1`, `eu-west-1`, `ap-southeast-1`,
`ap-southeast-2`, `ap-northeast-1`). Any other model takes the ambient
region, else the guardrail's, else `us-east-1`. A `base_url` of the section's
is used verbatim (its path and query kept); AWS's own regional host follows
the region.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-amazon-bedrock/` and
restart Nodecode, or run `(restart-cells)`.

## Configure

```jsonc
{
  "amazon-bedrock": {
    "region": "us-west-2",
    "profile": "work",
    // "base_url": "https://vpce-0123.bedrock-runtime.us-west-2.vpce.amazonaws.com",
    // "guardrail_identifier": "arn:aws:bedrock:us-west-2:123456789012:guardrail/abc",
    // "guardrail_version": "DRAFT",
    // "guardrail_trace": "enabled"
  }
}
```

## The roster

`models.json` is omp's bundled `amazon-bedrock` rows with the facts the
Converse request is shaped by, which the repository's `tools/omp-models.py`
does not carry: each model's thinking mode, whether it takes a thinking
display and binds its thinking to the prefix, its prompt-cache policy,
whether it takes a forced tool choice and sampling controls, and whether its
tool results carry images. `tools/bedrock-models.py` in this folder wrote it:

```sh
python3 -I tools/bedrock-models.py ~/.cache/nodecode-cells/omp-src models.json
```

The cell never runs the script.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-amazon-bedrock
```

SigV4 is checked against AWS's documented signing keys and its documented
S3 `GET Object` example (whose canonical request signs
`x-amz-content-sha256`, as omp's signer does), HMAC against RFC 4231, SHA-1
against FIPS 180. The event-stream decoder reads a frame Python's `zlib` and
`struct` built from the spec, every header type, and refuses a bad CRC and a
cut frame. Every AWS endpoint is a stubbed `dex:post` or `dex:request`, every
`~/.aws` file a temp file.

## Not ported

- `requestMetadata` (invocation-log tags): Nodecode sends none.
- The 1-hour cache TTL (`PI_CACHE_RETENTION=long`): every cache point is the
  default 5-minute one.
- `redactedContent` reasoning blocks: omp neither keeps nor replays them.
- The EC2 hardware probe (`/sys` DMI files) in the "is a source configured"
  check: an instance is assumed only when `AWS_EC2_METADATA_SERVICE_ENDPOINT`
  names its service. A round still asks IMDS.
- omp's dialect table for demoting an unsigned thought is approximated by
  model family (bare for Claude, `<think>` for DeepSeek, gpt-oss, Gemma,
  Qwen and Kimi, `<thinking>` otherwise).
- The first-event watchdog: the core's idle deadline stands in.

## Gaps

- **A binary stream walk.** `nle::walk-provider-stream` reads only
  server-sent event lines, so a lane whose wire is framed otherwise cannot
  hand it a fold; this lane re-assembles the walk's bracket from internals
  (`nle::cancellable-post`, `nle::with-idle-cut`,
  `nle::close-provider-stream-body`, the `lane-assembly` helpers), and no
  `walk-provider-stream` hook (another cell's headers, say) sees its rounds.
  The seam would be a `:frames` keyword on `walk-provider-stream`: a function
  of the response stream that answers the next decoded frame (a hash table)
  or NIL at its end, used in place of the SSE line reader, so a binary wire
  reuses the whole bracket, its evidence capture and its hooks.
- **Signing over the final body.** SigV4 signs the exact bytes sent, so the
  signature has to be computed after the body is final. The core's
  `walk-provider-stream` builds the request (and may swap in a body delta) after every
  hook has run; a hook there cannot sign it. With the seam above the lane
  would still need a `:sign` keyword (a function of the URL, the headers
  and the body octets that answers the headers to add) called last, just
  before the POST.

MIT licensed.
