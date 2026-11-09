# nodecode-good-student

Good Student: wrong questions a parent confirms, analysed into weak points and a week of study actions. A Nodecode rewrite of [harrylabsj/hermes-plugin-good-student](https://github.com/harrylabsj/hermes-plugin-good-student) by [harrylabsj](https://github.com/harrylabsj).

The model gets twelve `good_student_*` tools that keep one child's learning
record in a single JSON file under this home: profiles, exam and homework
scores, wrong questions waiting for a parent, the analysis they support, this
week's actions, reassessments and a weekly brief. Plus `/good-student` to see
the profiles, what waits, or one child's week. Nothing leaves the machine and
there is no key.

The gate the original is built on is kept. Recognition output is a
**candidate**: it sits in the waiting area, expires unconfirmed, and never
touches the profile until a parent confirms it. One wrong question is
*suspected* and never mastery; only an independent reassessment — a new
variant, no hints — moves a knowledge point, past the pass line.

## Get it

Tell the agent "add the good-student cell", or run `nodecode add nc://good-student`.

## Set it up

Nothing is required. The records live in `good-student/records.json` under
this home unless `data_dir` names another folder.

```jsonc
{
  "good-student": {
    "data_dir": "",          // empty: this home's good-student folder
    "pass_percent": 80,      // what an independent reassessment must reach
    "review_days": 14,       // how long a mastered point stands
    "candidate_hours": 24    // how long an unconfirmed candidate waits
  }
}
```

| Member | Default | What it does |
| --- | --- | --- |
| `data_dir` | none (this home) | the folder `records.json` is kept in |
| `pass_percent` | `80` | the accuracy an independent reassessment must reach |
| `review_days` | `14` | how long a mastered point stands before its review is due |
| `candidate_hours` | `24` | how long an unconfirmed candidate waits |

## Use it

> Record Sam's maths test: 18 out of 20, and reading 8 out of 10.

> Here are three wrong questions from this week's worksheet — put them in for me to confirm.

> What should we work on this week?

> He redid the fraction exercise with no hints: 5 out of 5.

The tools, as the model calls them:

| Tool | Argument | What it does |
| --- | --- | --- |
| `good_student_list_students` | — | every profile, with counts |
| `good_student_create_student` | `display_name`, `grade?`, `active_subjects?`, `goals?` | a new profile |
| `good_student_record_scores` | `student_id`, `records` | a batch of scores, whole or not at all |
| `good_student_list_scores` | `student_id`, filters | scores with a per-subject summary |
| `good_student_ingest_candidates` | `student_id`, `batch` | recognized wrong questions into the waiting area |
| `good_student_list_pending` | `student_id` | what waits for a parent |
| `good_student_confirm_questions` | `student_id`, `items` | a parent's yes or no, all or none |
| `good_student_analyze` | `student_id` | weak points with status, risk, confidence, evidence |
| `good_student_create_plan` | `student_id` | this week's actions, each with its reassessment day |
| `good_student_record_reassessment` | `student_id`, `kc_id`, `correct_count`, `total_count` | one reassessment, and the status it moves |
| `good_student_weekly_brief` | `student_id` | the parent's week, read-only |
| `good_student_delete_student` | `student_id`, `confirm_phrase?` | the scope, then the delete once the display name is given |

`/good-student` shows the profiles and what waits; `/good-student pending`
lists every waiting candidate; `/good-student Ada` is one child's week.

A knowledge point is named by its `kc_id` — the knowledge point's name
lowercased with dashes — and that is what a reassessment is recorded against.
A mastered point whose review date has passed goes back to `review_due`, and
the plan gives it an action again.

The records are one `records.json` in `data_dir`, written atomically under
the folder's lock, so two Nodecode processes can share one folder. Nothing
else is written there.
