# nodecode-perplexity

A Nodecode cell for [Perplexity](https://www.perplexity.ai) search. It gives
the model one verb, `(perplexity:search "query" ...)`, which asks Perplexity
to search the web live and answers with Perplexity's answer, the sources it
read and related questions. `/perplexity login` signs you in to your Pro or
Max account, so the search runs on your account's models.

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `perplexity`
sign-in ("Perplexity (Pro/Max)") and the web search provider it serves. See
[NOTICE](NOTICE).

## Why a search verb, not a provider

omp bundles no chat model under `perplexity`, and has no provider rule for
it. Perplexity is one of the engines behind omp's web search tool: the `web`
provider's `perplexity` model, alongside Exa, Brave, Kagi and others. The
models omp names for it are search models, not chat models:

- the subscription model a signed-in search asks the ask endpoint for:
  `experimental` (Perplexity's Sonar) unless `PI_PERPLEXITY_MODEL` names
  another; here the section's `model`
- the API model a key search asks `api.perplexity.ai` for: `sonar-pro` unless
  `PI_PERPLEXITY_API_MODEL` names another; here the section's `api_model`

So this cell is what omp's engine is: a search the model calls, through
`eval`. `(help :perplexity)` teaches the model the verb while the cell runs.

## Sign in

```
/perplexity login you@example.com
/perplexity code 123456
```

omp's login is a custom one with three ways in, tried in order:

1. **The desktop app.** On macOS, the legacy `ai.perplexity.mac` app keeps
   its session token in its defaults, and the login borrows it
   (`defaults read ai.perplexity.mac authToken`). So does `/perplexity login`
   here, unless the section says `"borrow_app_session": false` (omp's
   `PI_AUTH_NO_BORROW`). Newer Mac apps keep theirs in a restricted Keychain,
   out of reach.
2. **Browser SSO.** omp opens a browser window its host manages and captures
   the session cookie once you sign in. Nodecode has no such window; see
   Gaps.
3. **An email code.** Perplexity mails a code to your address. The command
   fetches a CSRF token, asks Perplexity to send the code, and answers. Type
   the code with `/perplexity code CODE`. If your account has an
   authenticator, Perplexity then asks for its code too, and the command
   says so: type that one with `/perplexity code CODE` as well. The cookies
   Perplexity sets along the way ride in one jar, as omp's do, and when the
   authenticator's answer carries no token the session is read from them.

omp prompts for each code inside one login. Here each code is a command of
its own, because it arrives somewhere else (your mail, your phone), so
nothing waits on a thread in between. A code Perplexity refuses can be typed
again.

The session token goes into Nodecode's shared `auth.json` under
`oauth_tokens.perplexity`:

```json
{"provider": "perplexity", "access_token": "...", "refresh_token": "", "email": "you@example.com"}
```

omp's rule is `expiry "jwt-or-never"` and `refresh "none"`: Perplexity's
session tokens usually carry no `exp`, and never expire from here. When one
does carry an `exp`, `expires_at` is that less five minutes, as omp keeps it,
and after it the session is not sent. There is no refresh; sign in again.
Every other field of `auth.json` is kept, and the file stays mode 0600.
`/perplexity status` says who is signed in, and `/perplexity logout` takes
the entry out.

## Other credentials

A search tries what omp's engine tries, in its order, and the first that
answers wins:

1. `PERPLEXITY_COOKIES`, a browser's whole `Cookie` header, sent to the ask
   endpoint.
2. The sign-in, sent to the ask endpoint as the session cookie
   (`__Secure-next-auth.session-token`). The ask endpoint ignores a bearer
   and falls back to its anonymous model, so the token never rides as one.
3. An API key, sent to `https://api.perplexity.ai/chat/completions`: a key
   `/connect` saved for `perplexity`, else `PERPLEXITY_API_KEY`. As in omp,
   a key is not tried while a sign-in is kept.
4. Nobody: the ask endpoint anonymously, only when none of the above is
   there. An anonymous answer with no sources is Perplexity's signup wall or
   an exhausted anonymous quota, and is refused as such.

## What a search sends

- **Ask endpoint** (cookies, sign-in, anonymous):
  `POST https://www.perplexity.ai/rest/sse/perplexity_ask` with the macOS
  app's identity (a browser's for an anonymous ask), the bare query (the ask
  endpoint has no system slot), `model_preference` from the section, web
  sources, incognito, and retrieval forced on. The answer streams as
  snapshots whose blocks merge by their use, a markdown block's chunks
  splicing in at their offset, until the final one.
- **API** (a key): the chat request with `sonar-pro`, 8192 output tokens,
  temperature 0.2, and Perplexity's search fields: web mode, 20 results, a
  pro search with high context, the search classifier, medium reasoning,
  English, related questions. The answer streams as chat deltas; its
  citations, matched with the search results, are the sources.
- **Filters**, keywords of `perplexity:search`: `:recency` (hour, day, week,
  month, year), `:domains` (hosts to keep, `-host` to drop, at most 20),
  `:after` and `:before` (`YYYY-MM-DD`, sent as `M/D/YYYY`, outranking
  recency), `:language` (a two-letter code). omp reads these off directives
  in the query (`site:`, `after:`); here they are keywords.
- A socket dropped before any answer is tried once more. An HTTP answer, a
  refusal included, never is.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-perplexity/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "perplexity": {
    // the subscription model a signed-in or cookie search asks for
    "model": "experimental",
    // the API model a key search asks for
    "api_model": "sonar-pro",
    // on macOS, whether /perplexity login takes the legacy app's session first
    "borrow_app_session": true
  }
}
```

## Not ported

- The API's Responses path (`PI_PERPLEXITY_RESPONSES=1` sends a key search
  to `/v1/responses`); the chat path, omp's default, is the one carried.
- The query directive parser. The filters are keywords instead.
- The local timezone in an ask: it is sent as UTC.

## Gaps

- **Browser SSO.** omp's SSO login calls its host's `onBrowserSession({url,
  cookieNames})`: open a browser window at
  `https://www.perplexity.ai/auth/signin`, let the operator sign in, answer
  the value of the first named cookie once it is set. The core has no such
  primitive; it would be one function that opens a URL in a browser the
  organism controls and answers the named cookie (`NLE:BROWSER-SESSION URL
  COOKIE-NAMES`), which the chrome cell could provide but no cell may depend
  on another.
- **One search tool.** In omp, Perplexity is one engine of one web search
  tool that falls through to the next engine. `nodecode-websearch`'s engines
  are a fixed list (`+PROVIDER-NAMES+` and `ASK` in its `search.lisp`), with
  no way for a cell to add one, so Perplexity is a verb of its own here. The
  seam would be an engine registration in `nodecode-websearch`: a name and a
  function `(query n) -> (values ANSWER HITS)`.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-perplexity
```

Every Perplexity endpoint is a stubbed `dex:post` or `dex:get`, and the
macOS app a stubbed `uiop:run-program`, so nothing reaches Perplexity.

MIT licensed.
