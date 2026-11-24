# nodecode-emotion-chip

Emotion Chip: renders the delivery an agent writes into its own text — a bracket score such as `[sigh]`, `[whisper: it passed.]`, `[pause]` or `[warm, amused]` — for Fish Audio or ElevenLabs, each in its own dialect, or strips it to the words alone. A Nodecode rewrite of [100yenadmin/ai-emotion-chip](https://github.com/100yenadmin/ai-emotion-chip) by [100yenadmin](https://github.com/100yenadmin).

## Get it

Tell the agent "add the emotion-chip cell", or run `nodecode add nc://emotion-chip`.

## Set it up

Two settings, in the `emotion-chip` section of the config:

- `backend` — `fish` (the default) or `eleven`.
- `model` — that service's model id, which is what picks the dialect: `s2.1-pro`, `s2-pro` or `drama-3` on Fish (Drama 3 also reads `<soft>`-style spans), `eleven_v4`, `eleven_v4_turbo` or `eleven_v3` on ElevenLabs. A model the cell does not know renders tagless, so the words still read correctly.

`sounds` (on by default) is the operator's switch: off keeps cues, spans and pauses and drops the laughs and sighs. `config.example.jsonc` is the section written out.

## Use it

Write the delivery into the text you would speak, then ask the cell to render it:

- `[sigh] I read the whole log. [pause] It is the migration.`
- `[[register: warm, steady]] Here is what I found.` — the register of a whole reply, never spoken.
- `[soft: There is one thing I am not sure of.]` — a span; on Fish s2 it covers its sentence.
- `It [stress: never] retried.` — emphasis, capitals on ElevenLabs.

The tool `emotion_chip` answers the vendor-ready text, the voice settings the opening cue asks for (a Fish rate, or an ElevenLabs stability) and a line naming every token it dropped. Hand the text and the settings to whatever you speak through; nothing here dials a network, holds a key or bills an account.

- `emotion_chip_guide` reads the grammar for a dialect.
- `/emotion-chip` reports the dialect and the switches, with the grammar.
- `/emotion-chip guide` reads the delivery guide.
- `/emotion-chip sounds off` (then `sounds on`) turns the affect sounds off in the config.
- `/emotion-chip <score>` renders one score for this home's voice.
- `(help :emotion-chip)` is the same guide in the manual.
