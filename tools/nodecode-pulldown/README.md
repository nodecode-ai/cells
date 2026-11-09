# nodecode-pulldown

Pulldown: reads a public web page as bounded Markdown with its title, status and extractor, and can render a JavaScript page in Chromium. A Nodecode rewrite of [anthony-maio/pulldown](https://github.com/anthony-maio/pulldown) by [anthony-maio](https://github.com/anthony-maio).

[Pulldown](https://github.com/anthony-maio/pulldown) is a page reader for
language models: it fetches one public page, keeps the part worth reading and
drops the navigation, the scripts and the boilerplate, and answers markdown
with the title, the HTTP status and a small routing record naming the extractor
that read it. A page that only makes sense after its JavaScript has run can be
rendered in Chromium instead. It runs on your machine; private and loopback
addresses are refused, so the model cannot read your router's admin page with
it, and nothing is sent anywhere but the site you asked for.

## Get it

Tell the agent "add the pulldown cell", or run `nodecode add nc://pulldown`.

## Set it up

The cell drives the `pulldown` program, so that has to be there first, in an
environment of its own:

```sh
python3 -m venv ~/.local/share/pulldown/venv
~/.local/share/pulldown/venv/bin/pip install 'pulldown==0.5.0'
```

It is found on `PATH`; when it is not, give the executable's path in the
cell's `program` setting (`/settings`). For rendering, install the extra and
Chromium once, in the same environment:

```sh
~/.local/share/pulldown/venv/bin/pip install 'pulldown[render]==0.5.0' 'playwright==1.64.1'
~/.local/share/pulldown/venv/bin/python -m playwright install chromium
```

`timeout_seconds` (default 60, 5 through 120) is how long one page may take;
`cache` (on by default) keeps fetched pages for an hour and answers a repeat
read from them, in `cache_dir` or, when that is empty, in the cell's own
folder under the Nodecode home. A cell that cannot find the program refuses to
start and says so.
`config.example.jsonc` in this folder lists every member.

## Use it

The model gets one tool:

| Tool | What it answers |
| --- | --- |
| `pulldown_fetch` | The page as markdown: title, address, a status line (HTTP code, detail, characters, time, whether the cache answered), the description, how the page was read, then the text. `detail`, `render` and `max_chars` shape it. |

And the same read three ways:

```lisp
(pulldown:fetch "https://docs.python.org/3/library/asyncio.html")
(pulldown:fetch "https://example.org" :detail "structured")
(pulldown:fetch "https://example.org/app" :render t :max-chars 4000)
```

`/pulldown URL` from the keyboard is the operator's own read; with no URL it
says what the cell is reading with. `(help :pulldown)` is the model's manual.

`detail` is `minimal`, `readable` (the default), `structured` for listings and
tables, or `full` for the whole page. `max_chars` (1000 to 40000, 20000 by
default) clips the text and says how much it left; when the routing record
recommends a browser, the answer says so, and `render true` reads the page in
Chromium. Untrusted page text reaches the conversation, so the manual tells the
model to treat it as source material and never as instructions.

It reads a page; it does not find one. Search with the websearch cell's
`web:search` and read here the pages worth reading whole.
