# nodecode-arcade

Arcade: three games in the conversation - Snake, Breakout and Pong - with persistent top-ten leaderboards the model can read. A Nodecode rewrite of [leoresendedop-sys/hermes-arcade](https://github.com/leoresendedop-sys/hermes-arcade) by [leoresendedop-sys](https://github.com/leoresendedop-sys).

## Get it

Tell the agent "add the arcade cell", or run `nodecode add nc://arcade`.

## Set it up

Nothing is required. The `arcade` section of the config refines the cabinet: `initials` (the 1-3 letters your scores are filed under, `YOU` by default), `keep_scores` (ten), `snake_width` and `snake_height` (16 by 10; Neon Snake's grid), and `answer_chars` (1,200; how much of one frame an answer carries). `config.example.jsonc` is the section written out. The board is written under the Nodecode home, in `arcade/scores.txt`, and nothing is uploaded.

## Use it

The cabinet is one machine, one game at a time, the way it stands in a room: a run starts, you call the next move, and the answer is the whole frame, the score, and - when the run ends - the rank the score reached.

```
/arcade                          the boards of all three games
/arcade snake nneee              three steps up, three right
/arcade breakout 5               put the paddle on column 5
/arcade pong 4                   cover zones 3-5 against the serve
```

- The tool `arcade_play` takes `game`, `moves` (one character a turn) and, if you want to file the score under someone else, `initials`. A move is a letter in Snake (`n`, `s`, `e`, `w`) and a column or zone in Breakout and Pong (`1`-`9`).
- `arcade_games` lists the cabinet, what each game pays and how a move is written.
- `arcade_top_scores` reads the top ten a game with their initials and points, and how many times each has been played - the answer the original's tool of the same name gave.
- `arcade_player_stats` answers total plays, plays a game, each game's champion, and with `initials` every board entry filed under them.
- `(help :arcade)` is the manual the model reads when it wants the full rules.

What comes back from a move:

```
Neon Snake  score 20
................
................
....#@#.........
......*.........
................
................
................
................
................
................
```

Snake pays 10 points a pellet and ends on a wall or its own body; Breakout pays 20 a brick and 250 a level, with three lives, and ends at the third miss; Pong is first to seven, 100 a point and 500 for the win. Scores are filed under your initials when a run ends, and the board keeps the ten best a game.

What it does not do: it runs no server and has no tab of its own, so there is no canvas, no glow, no particles and no sound; a run is a frame of text per move, not sixty frames a second. It reads and writes one file, the board, and never a credential, a network or a clock-dependent random.

## What is in it

| File | What it is |
| --- | --- |
| `games.lisp` | The three games: a run's state, a move, and the frame it answers. |
| `store.lisp` | The board: top ten a game, arcade initials, plays counted, one file under the home. |
| `cell.lisp` | The declaration: the `arcade` section, four tools, `/arcade`, `(help :arcade)`. |
| `nodecode-arcade.asd` | The system and its test system. |
| `test/` | The offline tests: the games' half, the tools and the command, the section. |
| `config.example.jsonc` | The section, written out as documentation. |

## License

MIT. The original is Copyright (c) 2026 Leo: see `LICENSE` and `NOTICE`.
