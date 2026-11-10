# nodecode-meme-studio

Meme Studio: the agent finds the meme template that fits a joke, captions its slots, and answers the finished picture (a GIF for an animated template) plus a link to edit it in the browser. A Nodecode rewrite of [MrHydeDev/meme-studio-mcp-plugin](https://github.com/MrHydeDev/meme-studio-mcp-plugin) by [MrHydeDev](https://github.com/MrHydeDev).

## Get it

Tell the agent "add the meme-studio cell", or run `nodecode add nc://meme-studio`.

## Set it up

Nothing to set up: Meme Studio asks for no account and no key. Five settings exist if you want them, in the `meme-studio` section of `config.jsonc`:

| Setting | Default | What it is |
| --- | --- | --- |
| `endpoint` | `"https://memes.mrhyde.dev/mcp"` | the Meme Studio MCP endpoint every call goes to; it must be an https address |
| `language` | `"en"` | the studio's language (`en` or `es`), used when a meme names none |
| `output_dir` | `"~/.nodecode/memes"` | where the pictures a meme makes are written |
| `timeout_seconds` | `90` | how long one call may wait for Meme Studio |
| `max_image_bytes` | `8388608` | the largest picture one answer may carry |

An `endpoint` that is not https refuses the cell at start, with the refusal naming the setting.

## Use it

Ask for a meme in your own words -- "make a meme about my code working locally but not in production", "an animated GIF of someone losing it because the wifi dropped", or "hazme un meme sobre los lunes por la mañana" -- and the model does the four steps the original plugin's skill taught:

| Tool | What it does |
| --- | --- |
| `suggest_templates` | The templates that fit the joke or situation, in any language: what each one is used for, whether it is animated, and its slots -- what goes in each one and about how many characters fit. |
| `search_templates` | The catalog by name, keyword or what a meme is used for. |
| `get_template` | One template in detail, its slots, and with `image: true` the picture with the slots numbered. |
| `popular_templates` | What people choose most this week, most used overall, or the newest additions. |
| `create_meme` | The meme itself: a template or your own photo, one caption per slot, and back comes the finished picture (a GIF for an animated template), an `edit_url` that opens it in the studio, and the server's warnings. |

`/meme my code works locally but not in production` asks for the same suggestions in the conversation, and `/meme` alone offers a row to type the joke in.

The picture is written under `output_dir` and named in the answer, so the model can show it with `![meme](path)` or hand you the file; `edit_url` is the server's own link, carried exactly as it came, and it holds the meme (the server stores none).

Meme Studio is a hosted service: every call goes to the endpoint above, which is the only peer. Your joke, the captions and any photo you give it go there to be rendered; nothing else is read, written or spawned. Usage limits apply per person and across the service, and a `busy` or `rate_limited` answer comes back as one line of text saying so.

## What is in it

| File | What it is |
| --- | --- |
| `mcp.lisp` | The one POST to the endpoint, the JSON-RPC envelope, the content items, and the picture saved to a file. |
| `cell.lisp` | The five tools, the `/meme` command, the settings and the one declaration. |
| `test/` | Offline tests over a scripted endpoint. |

## License

MIT, the original's: see `LICENSE` and `NOTICE`. The templates belong to their owners, and Meme Studio and its server are not open source.
