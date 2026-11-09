# nodecode-ja-writing-guard

JA Writing Guard: scores Japanese text for AI-style phrasing, with line, column and a fix hint for each. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-ja-writing-guard](https://github.com/TakeshiTGAL/hermes-plugin-ja-writing-guard) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

## Get it

Tell the agent "add the ja-writing-guard cell", or run `nodecode add nc://ja-writing-guard`.

## Set it up

Nothing is required: the cell scores every Japanese answer on its own, offline, and adds one italic line under it. All of it is in the `ja-writing-guard` section of `/settings` or `config.jsonc`; `config.example.jsonc` is that section written out.

- `mode` — `report` (the line under the answer) or `off`.
- `surface` — `chat` (default), `business_email`, `external`, `sns` or `tech_doc`: where the answers go. It scales each rule's weight, sets the threshold, and `business_email` and `external` also report 常体 where です・ます is needed.
- `threshold` — the 0-100 score at which a text counts as AI-like; `0` uses the surface's own (chat 40, business_email 35, external 40, sns 35, tech_doc 45).
- `min_chars`, `footer`, `max_findings` — how short an answer is skipped, whether the line is shown, and how many findings the tool returns.

## Use it

Ask for a Japanese mail, notice, post or page and read the answer. Where it carries the phrasing that reads as unedited AI output, one line says so:

> *ja-writing-guard: AI的な言い回し 72/100（chat の基準 40）。指摘 3 件（定型句・常套句・誇張・空虚な言葉）*

What it finds: stock closing phrases (〜と言えるでしょう, いかがでしたか), inflated words (シームレス, 重要な役割を果たします), English-calque grammar (〜することができます, 〜を行う), template shapes (3つのポイント, お役に立てれば幸いです), em dashes and Markdown where prose belongs, monotonous sentence endings, and 常体 where the surface needs です・ます. It checks wording, not authorship.

- `/ja-writing-guard TEXT` checks a text now; `/ja-writing-guard business_email TEXT` judges it as an outside email; bare, it shows the settings and the last report; `off`, `on`, `reset` control it.
- `ja_writing_check` — the tool: a whole draft in, the score, the threshold and every finding with line, column, excerpt and a fix hint out. Use it before sending a Japanese text and again after rewriting, to confirm the score went down.
- `(help :ja-writing-guard)` — the manual, for the model.

Code fences, inline code, URLs, quoted lines, HTML and text inside 「」 are masked out first, so a quotation or a code sample is never counted against the writer.

## What is in it

| File | What it is |
| --- | --- |
| `check.lisp` | the rule catalog (ids, labels, fix hints and weights), the masking, the surface profiles, the sentence and register checks, the score and the report |
| `cell.lisp` | the section, the line under the answer, the `ja_writing_check` tool, `/ja-writing-guard` and `(help :ja-writing-guard)` |
| `test/` | the offline tests: the catalog, the masking, the surfaces, the score, and the cell's own wiring |
