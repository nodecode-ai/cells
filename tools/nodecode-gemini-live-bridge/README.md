# nodecode-gemini-live-bridge

Gemini Live: one duplex session with the Gemini Live API, its model catalog and its token meter. A Nodecode rewrite of [cruzlxyz/gemini-live-bridge](https://github.com/cruzlxyz/gemini-live-bridge) by [cruzlxyz](https://github.com/cruzlxyz).

The original was a minibar in a desktop app: press play, talk, hear Gemini answer, watch the tokens. A terminal has no microphone and no speaker, so what this cell keeps is the session behind it — a Live session that stays open across tool calls, in text, with the catalog, the thinking level, the resumption handle and Google's own token count.

## Get it

Tell the agent "add the gemini live bridge cell", or run `nodecode add nc://gemini-live-bridge`.

## Set it up

A Google AI Studio key with Live API access, from <https://aistudio.google.com/apikey>.

1. **api_key_env** — the name of the environment variable holding the key, usually `GOOGLE_API_KEY`. The key itself is read from the environment at call time; nothing is written where the cell can read it back.
2. **model** — optional. Empty asks the key's own catalog and dials the first candidate; naming one, `models/gemini-3.8-live` say, pins it.
3. **thinking_level** — `low` or `high`; it is sent only to `*-extended-thinking` models, which require it.

## Use it

- `gemini_live_bridge_talk` — the tool. One turn: the prompt goes into the open session and the answer comes back with the session's token count. The session survives between calls, so a second turn has the first in context. A session that drops is redialled once with Google's session-resumption handle; `new_session: true` starts clean.
- `gemini_live_bridge_models` — every Live-capable model on the key, in the original's fixed order: `gemini-3.8-live` first, then extended-thinking, then native-audio-latest, then the newest native-audio preview, the rest by name. Transcribe and translate models are left out, as the original left them out.
- `/gemini-live-bridge` — `status`, `models`, `say <text>`, `stop`.

Text goes to `generativelanguage.googleapis.com` and nowhere else: ListModels, one single-use ephemeral token per dial, and the constrained Live socket. The session's handle is kept in memory only, so a restart starts a fresh session.
