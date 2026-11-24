# nodecode-gogogoogle

GogoGoogle: hand bulk work to the Antigravity CLI, then have a fresh model check it. A Nodecode rewrite of [josefacero/hermes-gogogoogle](https://github.com/josefacero/hermes-gogogoogle) by [josefacero](https://github.com/josefacero).

The session stays on its own model. The bulk of the work — mass edits, file generation, broad reading, a first draft of research, a second-opinion review — goes to the `agy` CLI (Google Antigravity), which runs on the user's Antigravity plan. A second, fresh `agy` conversation then checks the reply against the working folder, and the answer says whether the tree changed and what the diff was.

## Get it

Tell the agent "add the gogogoogle cell", or run `nodecode add nc://gogogoogle`.

## Set it up

Install the Antigravity CLI and sign it in once, outside Nodecode. Then name the model that should do the work, in the `gogogoogle` section of `~/.nodecode/config.jsonc` (`/settings`, or the file; `config.example.jsonc` has the shape):

| Member | What it is |
| --- | --- |
| `worker_model` | The Antigravity model that does the work, as `agy models` names it. Required. |
| `verifier_models` | Models that check a result, tried in order until one answers. Empty uses `claude-sonnet-5-5-low`, `gemini-4-argon-high`, `gemini-3.8-flash-high`. |
| `blocked_roots` | Folders a run may never use as its workdir, besides a drive root, your home folder and the credential folders (`.ssh`, `.aws`, `.gnupg`, `.azure`, `.kube`, `.docker`, `.config/gcloud`, `.gemini`). |
| `browser_mode` | `headless` (default), `visible`, or `off` for agy's browser in research, review and verifier passes. |
| `agy` | The CLI: its name on PATH, or a path to it. |

## Use it

`antigravity_delegate(task, workdir, role, minutes, verify, background, resume)` is the verb:

```
antigravity_delegate(task: "Write the tests for src/parse.lisp and list every file you changed.",
                     workdir: "/work/project", role: "do", minutes: 10)
```

| Role | What it may do |
| --- | --- |
| `do` | Edit files, run commands: agy approves its own permissions, so it can change or delete files in the folder. |
| `research` | Read only, and told to cite what it opened and list what it could not. |
| `review` | Read only, and told to report problems ranked worst first with file and line. |

The answer is JSON: the run's status and reply, its conversation id, the seconds it took, whether git says the working tree changed and the diff, and — with `verify` on — the verdict. A `research` or `review` run that changed the tree is a `READ-ONLY VIOLATION` and cannot pass; a resumed run, a blocked action or a truncated input makes a pass `VERIFY INCOMPLETE`.

- `background: true` answers a job id at once; `antigravity_job(id)` gives its phase, its elapsed time, or its whole result.
- `resume` continues a conversation a timeout cut short; the answer names the id to use.
- `antigravity_stats` counts the runs, the failures by reason, the average worker time, the tokens agy spent, and which verifier models are out of quota until when.
- `/gogogoogle` prints the same numbers to the operator.

A read-only role's rules are agy's permission rules, not an OS sandbox: `agy` can still read a file by absolute path outside the working folder, and in `do` it may write there. Keep the work in a git repository with a clean tree, and keep personal folders in `blocked_roots`.
