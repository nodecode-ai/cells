# nodecode-agenda-guardian

Agenda Guardian: a day planner whose blocks say why they are there. A Nodecode rewrite of [Moodtuner997/hermes-agenda-guardian](https://github.com/Moodtuner997/hermes-agenda-guardian) by [Moodtuner997](https://github.com/Moodtuner997).

You write your rules once (work hours, protected windows, the sharpest hours, the longest block, the tasks a day), give each task an estimate, and report the real time when it is done: from the third finished task of a kind the planner corrects that kind's estimates by the median of your own errors and says so. A plan places the day's tasks in the free time around your calendar's events, splits one into blocks no longer than your longest block, prefers your focus window, and names what does not fit instead of overbooking. It is deterministic: no model call, and the same tasks, rules and calendar give the same plan.

## Get it

Tell the agent "add the agenda-guardian cell", or run `nodecode add nc://agenda-guardian`.

## Set it up

The cell keeps its tasks in one file, so it needs a folder:

```jsonc
// ~/.nodecode/config.jsonc
{
  "agenda-guardian": {
    "data_dir": "~/agenda/",
    "calendar": "~/agenda/calendar.ics",     // empty plans around no events
    "work_hours": "09:00-18:00",
    "workdays": "mon-fri",
    "protected": "lunch 12:30-13:30",
    "focus": "09:00-11:30",
    "buffer_min": 10,
    "max_block_min": 50,
    "max_tasks_per_day": 3
  }
}
```

`calendar` is an `.ics` file or an `https` address (an Outlook, Apple or Google export). Nothing else is read, and nothing reaches the network unless `calendar` is an address. No key and no sign-in are needed.

## Use it

In the chat, the model has five tools:

| Tool | What it does |
| --- | --- |
| `agenda_add_task` | A task with its estimate, a kind (writing, admin, sport) and a priority. |
| `agenda_tasks` | The open tasks in the order the planner takes them. |
| `agenda_done` | The real time a task took; from the third of a kind the estimates are corrected. |
| `agenda_plan` | The day: the blocks placed with their reasons, and what does not fit. |
| `agenda_rules` | The rules in force, and any of them changed in the chat. |

Without the model, `/agenda-guardian` is the same planner:

```
/agenda-guardian plan                  the day, each block with its reason
/agenda-guardian plan 2026-11-10       another day
/agenda-guardian tasks                 what is open
/agenda-guardian add 30m write the report
/agenda-guardian done t1 45m           the real time it took
/agenda-guardian rules                 the rules, and where each came from
/agenda-guardian set max_tasks_per_day=2
```

A rule changed in the chat is yours: it outranks the section's value, is kept in the planner's file, and every answer says where each rule came from.

## Gaps

The rewrite keeps the original's planner, its estimate correction and its calendar reading; it leaves out what was the other harness's and not the planner's: the Google Workspace write path (this cell reads its calendar and never writes one), the JSON and command calendar sources (an `.ics` file or address is read), the week and activity proposals with their eight weeks of history, the dashboard tab, the two cron reminders, and the model-facing prompt section. The event-reading covers the ordinary VEVENT shapes (start and end as a wall clock or with a `Z`); a recurring event is read at its first occurrence only.
