# nodecode-anki

Anki Bridge: make Anki flashcards from notes, search the collection, and quiz
due cards. A Nodecode rewrite of
[suhas-svg/hermes_anki](https://github.com/suhas-svg/hermes_anki) by
[suhas-svg](https://github.com/suhas-svg).

## Get it

Tell the agent "add the anki cell", or run `nodecode add nc://nodecode-anki`.

## Set it up

Install [AnkiConnect](https://ankiweb.net/shared/info/2055492159) (add-on code
`2055492159`) in Anki desktop and leave Anki open: it listens on
`http://127.0.0.1:8765`, which is where the cell points by default. Nothing
else is required. The `anki` section takes:

| Key | Default | Purpose |
| --- | --- | --- |
| `anki_url` | `http://127.0.0.1:8765` | the AnkiConnect endpoint; keep loopback unless Anki runs elsewhere |
| `default_deck` | `Default` | the deck a call that names none adds to, created when it does not exist |
| `default_model` | `Basic` | the note model a call that names none uses |
| `request_timeout` | `10` | seconds one request may take (at most 120) |
| `max_cards` | `50` | the most notes or cards one call adds or returns (at most 100) |
| `api_key` | empty | AnkiConnect's own API key, when the add-on asks for one |
| `api_key_env` | `ANKI_BRIDGE_API_KEY` | the variable holding that key, read when `api_key` is empty |

A blank `anki_url`, or one that is not an `http://` or `https://` address,
refuses the start and says so.

## Use it

Ask in ordinary words - "turn these notes into five cards in my Spanish deck",
"what is due today", "quiz me on the due cards, one card at a time" - or use
the command:

```
/anki status
/anki due Spanish
```

The model gets five tools: `anki_decks` (decks and counts), `anki_add_cards`
(notes in a deck, creating the deck if needed, duplicates reported not
forced), `anki_search` (Anki's own query syntax), `anki_due` (the cards ready
to review, front and back as plain text) and `anki_answer` (grade a card and
reschedule it). Card fields arrive as plain text: Anki's HTML, its style
blocks and its entities are folded away. Nothing leaves the machine except
the request to the endpoint you name.

`(help :anki)` in eval explains the same, and `(anki:strip-html "<b>a</b>")`
is the field cleaner on its own.

Every request is one JSON POST to AnkiConnect, the same action and the same
arguments the original sent; the tools answer the same JSON a model read
there. The test system stubs that one HTTP seam, so the suite runs offline.

## Credit

A rewrite of suhas-svg's MIT-licensed Hermes Agent plugin `hermes_anki`, at
commit `bc89df834fb104b8d69c6444f35a5ae6b3764dd7`. See NOTICE for what was
rewritten and what was left out; LICENSE is the original's, unchanged.
