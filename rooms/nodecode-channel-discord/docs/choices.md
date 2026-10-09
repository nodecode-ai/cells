# Buttons and pickers

Some of the bot's messages carry buttons or menus. Pressing one is the same
as typing a line: the bot treats the press as that person saying the line in
the room, as a reply to the message pressed, and admits and answers it the
way it would a typed message. Two features use this: clarifying questions
and the `/models` picker.

The Stop and Details buttons on a turn's card are different: they act at
once and say nothing in the room. See [turns.md](turns.md#the-card).

## Clarifying questions

When the bot has to ask before it can answer, and the answer is one of a few
options, it can put the options on its question as buttons:

```
---

Which matters most for the first release?

[ Zero setup ] [ Fast startup ] [ Small binary ]
```

The model does this by calling, during its turn:

```lisp
(nck:answer-choices '("Zero setup" "Fast startup" "Small binary")
                    :platform "discord" :channel "<channel id>" :thread "<thread id, or omit>")
```

then ending the turn with the question as its answer. The model is told about
this in its Discord instructions; you do not need to configure anything.

- Two to five labels, each 1 to 80 characters, all different. Anything else
  is refused with an error the model sees.
- The labels ride the answer's last message as one row of blurple buttons.
- A second call in the same turn replaces the first set.
- It works only while a turn is running in that room.

### Pressing a button

A press is the presser's reply to the question, with the label as its words.

- The conversation that asked the question takes the label as its next
  turn, with everything it did before still in context. If that
  conversation is mid-turn, the press steers it.
- The question is edited in place: its words stay, its buttons go, and a
  small line under it says which answer it took:

  ```
  Which matters most for the first release?

  -# Answered: Zero setup
  ```

- Anyone the room admits may press, not only the person who asked. The press
  passes the same checks as a typed message from that person.
- Typing a reply instead still works: the buttons are a shortcut.

### A question that is no longer open

The buttons work while the conversation that asked is open: until 30 minutes
after its turn ended with no further reply, and not across a restart of the
bot. A press after that tells the presser privately "this question is no
longer open", and nothing runs. The question keeps its buttons. Type a reply
or ask again.

## The /models picker

On Discord, `/models` with no arguments answers with a picker card instead of
text.

### The providers view

```
## Model Picker
Current model: deepseek/deepseek-flash
Select a provider (3 available).

[ Select provider          v ]
```

The menu lists every configured provider with its model count, the
provider of the current model shown as selected. "Current model" is what
this room's next ask runs on: the room's `/models` pick, its channel's (for a
thread), the model of the agent the room runs as, or the organism's default.

### The models view

Picking a provider edits the card in place:

```
## Model Picker
Current model: deepseek/deepseek-flash
Default: deepseek/deepseek-flash
Select a openrouter model.

[ Select openrouter model  v ]
[ Providers ] [ Reset to default ]
```

- The menu lists that provider's models. The room's current model is shown
  as selected.
- **Providers** goes back to the first view.
- **Reset to default** runs `/models default`, which clears the pick of the
  room you are in. It is greyed out unless that room picked its own: a
  thread running its channel's pick has nothing to reset.
- If the provider's model list could not be fetched, the card says so on a
  `listing:` line.

Picking a model picks it for this room. The card is replaced with the answer:
"this channel runs PROVIDER/MODEL from its next ask; the organism's default
is unchanged" (or "this thread runs ...").

### Long lists

A menu shows at most 25 entries, Discord's limit. A longer list gets a row of
**Prev**, **Page N/M** and **Next** buttons under the menu.

### What each step is

Every press is a `/models` line, run exactly as if you had typed it:

| Press | Line |
| --- | --- |
| a provider | `/models PROVIDER` |
| a model | `/models PROVIDER MODEL` |
| Providers | `/models` |
| Reset to default | `/models default` |
| Prev / Next | `/models page N` or `/models PROVIDER page N` |

So the same rules apply: with operators declared, only they can press. Anyone
else is refused privately ("/models is the operator's; /help lists what you
can run") and the card stays as it was.

### Who sees the card

Run from the slash menu, `/models` answers privately: only you see the card,
and your presses edit it. Typed as a message (`@YourBot /models`), the card is
a normal reply that the room can see, and any operator can work it.

See [commands.md](commands.md#models-in-a-room) for the text forms of
`/models`.
