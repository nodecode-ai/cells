# nodecode-model-drift-watch

Model Drift Watch: catch a model that quietly got worse. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-model-drift](https://github.com/TakeshiTGAL/hermes-plugin-model-drift) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

Ask the same fourteen questions every time, grade each answer by a rule rather
than by a judge, keep every run, and compare a run with the same model's own
baseline by an exact test. A drop is reported only when it is big enough to
matter, when the test passes, when more questions fell than rose, and -- with
`confirm` on -- when a second run right after the first shows it too.

## Get it

Tell the agent "add the model-drift-watch cell", or run `nodecode add nc://model-drift-watch`.

## Set it up

Nothing is required. The questions are asked of the organism's auxiliary target
(the model its side calls run on); `provider` and `model` name another target
if you want one watched specifically.

| Setting | Default | What it does |
| --- | --- | --- |
| `provider`, `model` | empty | the watched target; empty watches the auxiliary one |
| `state_dir` | empty | where `runs.jsonl` lives; empty keeps it under `model-drift-watch/` in the home |
| `categories` | empty | categories to ask (`reasoning`, `instruction`, `japanese`); empty asks all |
| `max_tokens` | 400 | one answer's ceiling; raise it for a model that thinks at length |
| `baseline_runs` | 5 | clean runs that make the baseline before comparisons start |
| `min_drop_points` | 10 | the smallest accuracy drop worth reporting |
| `alpha` | 0.05 | the significance the exact test must pass |
| `confirm` | on | ask once more before calling a drop drift |
| `keep_runs` | 40 | how many recorded runs the file keeps |

## Use it

```
/model-drift-watch            the last check, the baseline and the verdict (free)
/model-drift-watch run        ask the questions now (spends tokens)
/model-drift-watch estimate   tokens a check would spend, before it spends them (free)
/model-drift-watch questions  the question set, by category, and how each is graded
/model-drift-watch reset      forget the runs and start a new baseline
```

The model can call the same thing as the `model_drift` tool, with
`action` one of `status`, `run`, `estimate`, `reset`. From eval:

```lisp
(model-drift-watch:status)
(model-drift-watch:check)
(model-drift-watch:estimate)
(model-drift-watch:questions)
(model-drift-watch:reset)
```

**Building a baseline.** The first `baseline_runs` clean runs are the
baseline; nothing is called drift before they exist, and a run that is mostly
provider errors is a failed measurement that never joins them. A baseline is
per target: change `model` and the baseline starts again.

**A scheduled check.** With the cron cell on, schedule a prompt that says
`/model-drift-watch run`; the report it answers is what the schedule delivers.

**What a report looks like.**

```
DRIFT: 9 of 14 correct (64%) against a baseline of 68 of 70 (97%), -33 points lower, p=0.0001: 9 question(s) fell against 0 that rose. Regressed: r01-arith, r03-letters, r05-percent.
```

**What it asks.** Reasoning (arithmetic, dates, letters, weekdays, averages,
primes), instruction following (a JSON array, an exact word count, lowercase
only, an exact shape) and two Japanese items. An answer is cut at its last
`ANSWER:` line, or read whole where the instruction says what to output, and
then graded by a rule: a number, an accepted spelling, a shape, a word count,
or a JSON array. No judge model is involved, so a score change is the model's.

**What it costs.** Fourteen short questions a run, each capped by
`max_tokens`; `estimate` prints the ceiling in tokens before you spend it. A
confirmation run happens only on a day that looks worse.

Run `.github/test-cell.sh tools/nodecode-model-drift-watch` from the cells
repository. The tests are offline: the one model call goes through a scripted
answerer, so grading, the baseline, the confirmation and the reports are all
facts a test asserts.
