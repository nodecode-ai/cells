# nodecode-markdown-new

Markdown New: reads, searches, crawls and converts public web pages to Markdown, as one tool the model calls. A Nodecode rewrite of [nosliwhtes/hermes-markdown-new](https://github.com/nosliwhtes/hermes-markdown-new) by [nosliwhtes](https://github.com/nosliwhtes).

## Get it

Tell the agent "add the markdown-new cell", or run `nodecode add nc://markdown-new`.

## Set it up

Nothing. The [Markdown.new](https://markdown.new) service is public and keyless,
so the cell works as installed; a page, a query or a site is sent to it and
its Markdown comes back.

The section is optional, and every member has a default:

| Member | Default | What it bounds |
| --- | --- | --- |
| `base_url` | `https://markdown.new` | the service address; another instance if you run one |
| `timeout_seconds` | 60 | one request |
| `poll_interval_seconds` | 2 | how often `crawl_run` asks a job for its status |
| `poll_timeout_seconds` | 300 | how long `crawl_run` waits before handing the job id back |
| `max_chars` | 20000 | how much of one document an answer carries |
| `max_upload_bytes` | 10485760 | the largest local file an upload may send |

`/markdown-new` with no argument prints what the cell is connected to and
what it can be asked, and `(help :markdown-new)` is the model's guide.

## Use it

Ask for what you want; the model picks the operation:

| Operation | Use |
| --- | --- |
| `url_convert` | a page you already have the address of, as Markdown |
| `search_extract` | search the web, with the extracted content of the results and their urls |
| `crawl_start` | begin a bounded crawl of a site; answers a job id |
| `crawl_status` | how far a job got, and how many pages it read |
| `crawl_download` | a finished job's Markdown |
| `crawl_run` | start, wait and download in one call |
| `crawl_cancel` | ask the service to stop a job |
| `file_convert` | a public PDF's address, or a local document, as Markdown |

From the keyboard, `/markdown-new <url>` reads one page and shows it.

### What it does not send

The service reads public pages only, so a URL that points inside your network
(`localhost`, `192.168.…`, a bare hostname) and a URL carrying a credential
in a query parameter (`?access_token=…`) are refused before anything leaves.
An upload must be a regular file that is not a secret store — no `.env`, no
`.ssh`, `.gnupg` or `.aws` path, no `.pem`, `.key` or wallet file — and no
larger than `max_upload_bytes`.

What comes back is untrusted text: the tool's own description says so, and a
page's instructions are the page's, not yours.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-markdown-new
```

Every request is a stubbed `nlk:http`, so nothing reaches Markdown.new and
no network is needed.

MIT licensed, as the original is.
