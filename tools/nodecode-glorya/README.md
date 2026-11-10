# nodecode-glorya

Glorya: image ads, video ads, landing pages and websites, made on your own Glorya account and brought back into the conversation. A Nodecode rewrite of [Glorya-AI/hermes-plugin](https://github.com/Glorya-AI/hermes-plugin) by [Glorya-AI](https://github.com/Glorya-AI).

## Get it

Tell the agent "add the glorya cell", or run `nodecode add nc://glorya`.

## Set it up

Glorya keeps the brand (logo, colours, fonts, products) on its side, so the work happens on your Glorya account, and this cell needs a token for it:

1. Sign in at https://glorya.ai and copy the API token the platform shows.
2. Put it in the environment variable `GLORYA_TOKEN`, or save it in a file and point the section's `token_file` at that file. A token pasted anywhere is never written by this cell.

The section is `glorya` in `~/.nodecode/config.jsonc`; `config.example.jsonc` shows every member. The endpoint defaults to `https://platform.glorya.ai/mcp`, and nothing else is contacted.

## Use it

Ask for what you want: "an image ad for the new serum, summer sale", "a 15-second video ad", "a landing page for the launch". The model calls `glorya_create`, tells you the job started, and gives you the Glorya link while Glorya works.

- `glorya_job` - where a job stands: status, progress, the question it is waiting on, and the links a finished job made.
- `glorya_answer` - answer the question a job asked, with the exact choice when it offers choices.
- `glorya_refine` - change a result: another headline, another colour, a shorter cut.
- `glorya_publish` - put a finished landing page or website live.
- `glorya_attach` - download the finished files into `~/.nodecode/glorya/` (or the section's `download_dir`) and answer their paths, so a picture can be shown in the reply.
- `glorya_jobs` - the jobs this conversation follows, with their open questions.

The cell follows every job it starts: a background pass asks Glorya how it is going, and when a job needs a decision or finishes, that lands in the chat as a notice and at the next request as a reminder - so a job does not need the model to poll it.

`/glorya` does the same from your side of the keyboard, without spending a model turn:

```
/glorya image <brief>            /glorya jobs [all]        /glorya status [job]
/glorya video <brief>            /glorya answer [job] <answer>
/glorya page <brief>             /glorya refine [job] <change>
/glorya site <brief>             /glorya publish [job] [slug]
/glorya campaign <brief>         /glorya cancel [job]
/glorya brand | credits | campaigns | sites | formats
```

Jobs spend your Glorya credits, the same as on the platform, and a campaign spends real ad money on Meta, Google Ads or TikTok.
