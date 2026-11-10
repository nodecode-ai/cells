# nodecode-quegly

Quegly: schedule, publish and measure social posts on ten networks from one
Quegly account. A Nodecode rewrite of [quegly/agent](https://github.com/quegly/agent)
by [quegly](https://github.com/quegly).

## Get it

Tell the agent "add the quegly cell", or run `nodecode add nc://quegly`.

## Set it up

A [Quegly](https://quegly.com) account with an active subscription, and an API
key: on quegly.com open **Settings**, **API** and create one; it starts with
`qgl_`. Pick **Full access (all workspaces)**, or bind the key to one
workspace.

Put the key in the cell's section, `api_key` (or export `QUEGLY_API_KEY`), in
`~/.nodecode/config.jsonc`:

```jsonc
{
  "quegly": {
    "api_key": "qgl_...",
    // only when a full-access key reaches several: the workspace every call
    // acts on. A key bound to one workspace needs none.
    "workspace_id": "b3f1c2d4-5e6a-7b8c-9d0e-1f2a3b4c5d6e"
  }
}
```

Never paste the key into the conversation: it is a setting, not a message.
`/quegly` says what the cell is connected to without printing it.

## Use it

Ask in words: "schedule these three photos to Instagram and X for Friday at
9am", "post this video to TikTok and YouTube now, with the title 'Behind the
scenes'", "what's scheduled for this week", "why did my LinkedIn post fail,
retry it", "what were my five most viewed posts last month". The model works
through eight tools, which `(help :quegly)` describes:

| Tool | What it does |
| --- | --- |
| `quegly_workspaces` | the workspaces the key reaches, with their ids |
| `quegly_accounts` | the connected accounts of a workspace, by id and platform |
| `quegly_upload` | a photo or video from a public url, answering a media id |
| `quegly_create_post` | one post to every account at once: schedule, now, or draft |
| `quegly_posts` | the posts of a date range, with their status |
| `quegly_post` | one post, each platform's status, link and error |
| `quegly_change_post` | publish now, retry the failed platforms, or delete |
| `quegly_analytics` | one post's numbers, or every published post of a range |

A post names accounts by id, never by platform, because one workspace can hold
two accounts of one platform. It is created in one call carrying every
account, with per-platform (or per-account) settings where a platform needs
them: a Pinterest `board_id`, a TikTok `privacy_level`, a caption of its own.
Media goes in first, by public url. Scheduling runs up to 90 days ahead, and
publishing is asynchronous: after a publish or a retry, the post is read again
until each platform has ended posted, or failed with its own error.

`/quegly` reports the endpoint, the key and the tool count; `/quegly accounts`
lists the connected accounts and sends nothing else.

## What it does not do

- It never connects a social account, adds a team member, or touches billing:
  those happen on quegly.com.
- It does not upload a local file: media goes in by a url Quegly can fetch.
- It does not set or remove a video thumbnail, create a Pinterest board, or
  update a scheduled post: the rest of the seventeen MCP tools, left for a
  later version. See [NOTICE](NOTICE).

Every tool call goes to Quegly's public API at `https://api.quegly.com/v1`,
with the key as a bearer header, and nowhere else. Nothing runs on your
machine, and nothing is sent anywhere unless the model calls one of the tools.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-quegly
```

Every Quegly endpoint is a stubbed `nlk:http`, so nothing reaches the network
and no key is spent: 17 tests, 0 failed.

MIT licensed, as the original is. Copyright (c) 2026 Quegly ([LICENSE](LICENSE)).
