# nodecode-gemini-live

Gemini Live voice: one spoken turn per call, the reply saved as a WAV, and the work the voice model delegates named back to you. A Nodecode rewrite of [skappafrost/hermes-gemini-live](https://github.com/skappafrost/hermes-gemini-live) by [skappafrost](https://github.com/skappafrost).

## Get it

Tell the agent "add the gemini-live cell", or run `nodecode add nc://gemini-live`.

## Set it up

A Gemini API key from [aistudio.google.com/apikey](https://aistudio.google.com/apikey); the free tier works. Two ways to give it one:

- paste it into the `gemini-live` section's `api_key`, or
- set `GEMINI_API_KEY` in the environment and leave `api_key_env` at its default.

`voice` is one of `Puck`, `Charon`, `Kore`, `Fenrir`, `Aoede`; `model` defaults to `gemini-3.8-live`, the original plugin's choice after measuring that the extended-thinking model accepts a function declaration and then delegates in only about two turns out of five. `timeout_seconds` bounds one turn; `directory` says where the reply WAVs are saved, empty for the profile's own folder.

## Use it

`gemini_live(text, audio_path?, delegate?)` runs ONE spoken turn. The text goes to the Gemini Live model, which answers out loud; the answer comes back as its spoken transcript, the path of the saved 24 kHz mono WAV, and any task the voice model delegated. The three verbs the original gave the voice model are declared unchanged (`hermes_task`, `hermes_tasks`, `hermes_task_update`), so the model still hands real work over instead of guessing. `audio_path` sends a 16-bit PCM WAV before the text, so a recorded clip is answered too. `/live` shows the configured model and voice and the calls saved so far; `/live <what to say>` runs one turn.

Each call is billed to your Google AI Studio key and holds a metered, listening socket while it runs, so make one call per thing you want said aloud.

## What the rewrite leaves out

The original is a full-duplex call inside Hermes Desktop: a browser holds the microphone, a backend owns the upstream socket so the key never reaches the renderer, the user can mute without hanging up, and a task that stops for permission reaches them as a spoken prompt with Approve and Deny buttons. A cell has no microphone and no browser control, so this one runs a turn at a time: text (or a recorded WAV) in, spoken reply out, saved as a file. Delegated tasks are named back to the agent that called the tool rather than run and read aloud inside the same call, and there is no approve button: the voice model can deny, steer or stop a task but never approves one, exactly as the original arranged it.

The key stays in the cell's own process, as the original's stayed in its backend; every error it builds is redacted, because the credential rides the socket URL.

## Test

`.github/test-cell.sh tools/nodecode-gemini-live` from the cells repository. The tests stub the socket and replay canned frames: a turn is transcribed and written, a delegation is named and answered, a call with no key refuses before anything is sent, and the cell starts and stops cleanly.
