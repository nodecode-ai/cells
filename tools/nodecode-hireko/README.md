# nodecode-hireko

Hireko: sign in as a recruiter, create candidate screening interview links, and
read candidate match scores. A Nodecode rewrite of
[HireKoAI/HirekoAiPluginPublicRepo](https://github.com/HireKoAI/HirekoAiPluginPublicRepo)
by [HireKoAI](https://github.com/HireKoAI).

The model gets seven tools — `hireko_auth_status`, `hireko_login`,
`hireko_logout`, `hireko_create_interview_link`, `hireko_get_interview_link`,
`hireko_get_search_score` and `hireko_health` — that sign in to Hireko's own
Cognito user pool and call Hireko's hosted recruiter endpoint. `/hireko` is
the operator's way in and out, and `(help :hireko)` is the model's page.

## Get it

Tell the agent "add the hireko cell", or run `nodecode add nc://hireko`.

## Set it up

Hireko needs a recruiter account and the organization slug (the *tenant*)
Hireko gave you. Put the recruiter's email and password in the environment,
so the password never has to be typed into a conversation:

```sh
echo 'HIREKO_USERNAME=…' >> ~/.nodecode/.env
echo 'HIREKO_PASSWORD=…' >> ~/.nodecode/.env
```

and name the tenant in the cell's section, which `/settings` writes:

```jsonc
{
  "hireko": {
    "tenant": "acme",
    "stage": "beta"        // or "prod"
  }
}
```

Then `/hireko login` signs in once and saves the session; every tool reuses it
until it expires, and re-signs in by itself while the password is set. The
session is one file a tenant under the Nodecode home, readable by its owner
only. `/hireko` says where it stands and `/hireko out` deletes it.

| Setting | Default | What it does |
| --- | --- | --- |
| `tenant` | `test` | the organization slug every call names |
| `stage` | `beta` | `beta`, or `prod` when Hireko runs you there |
| `username_env` / `password_env` | none | the variables holding the recruiter's email and password (`HIREKO_USERNAME` and `HIREKO_PASSWORD` are read either way) |
| `facade_url` | Hireko's hosted endpoint | where every tool call goes, for a proxy of your own |
| `client_id` / `region` | read from the tenant | the Cognito app client and region, when the tenant's configuration page should not be read for them |
| `timeout_seconds` | `60` | one sign-in's or tool call's ceiling |
| `poll_timeout_seconds` | `150` | how long an interview call waits for its link when the call names no `wait_seconds` |
| `poll_interval_seconds` | `6` | how often it asks again while it waits |

## Use it

- **`hireko_auth_status`** — whether a session is saved, for which tenant, and
  how many minutes the token has left. It reads the saved file and makes no
  request.
- **`hireko_login`** / **`hireko_logout`** — sign in with the environment's
  email and password, or forget the saved session.
- **`hireko_create_interview_link`** — `{title, description?, company_name?,
  location?, interview_length?, start_date?, end_date?, company_culture?,
  employment_type?, language?, recruiter_email?, wait_seconds?}`. Hireko writes
  the questions asynchronously, so the call polls until the candidate's session
  exists (150 seconds by default) and answers the interview link:

  ```json
  {"status": "COMPLETED", "job_id": "job_…",
   "interview_link": "https://interview-test.beta.hireko.ai/anonymousInterviewSession/job_…?settings=…"}
  ```

  If it is still generating it answers `status: "STARTED"` with the
  `external_job_group_id`.
- **`hireko_get_interview_link`** — `{external_job_group_id, wait_seconds?}`:
  keep polling one that was still generating.
- **`hireko_get_search_score`** — `{job_id}`: the candidate match scores and
  search relevance Hireko computed for a job posting.
- **`hireko_health`** — whether Hireko's downstream recruiter services answer.

The cell carries the original's interviewing guidance in its manual page: ask
for one detail at a time (title, duration, location, company, dates), never
reuse an earlier job's details, and write the description from the title.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the section, the session file, the Cognito sign-in, the facade call, the poll, the seven tools, `/hireko` and `(help :hireko)` |
| `test/cell-test.lisp` | lifecycle, sign-in, the interview tools, the refusals and `/hireko`, every request a stubbed `nlk:http` |
| `config.example.jsonc` | the section, commented |

## Why a sign-in and not a browser window

In Hermes the plugin also offered a browser device login: it opened a page and
listened on a fixed local port for the token to arrive. Nodecode's own way in
is a command, so `/hireko login` signs in with the same Cognito call headlessly,
and the password is read from the environment rather than a tool argument — as
the original's own headless path did. The browser flow and its loopback port
were left out; nothing else of the tool surface was.

## Test

From the cells repository's root, offline:

```sh
.github/test-cell.sh tools/nodecode-hireko
```

## License

MIT. Copyright (c) 2026 Hireko: see `LICENSE` and `NOTICE`.
