# nodecode-unity

Unity: the project open in your editor — its tools, a call, and a whole change as one approved plan. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-csharp-game-editor](https://github.com/TakeshiTGAL/hermes-plugin-csharp-game-editor) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

## Get it

Tell the agent "add the unity cell", or run `nodecode add nc://unity`.

## Set it up

The cell talks to [MCP for Unity](https://github.com/CoplayDev/unity-mcp), the
package that connects an open Unity editor to MCP clients. In the project:

1. Add the package in Unity: **Window > Package Manager > + > Add package from
   git URL**, and paste
   `https://github.com/CoplayDev/unity-mcp.git?path=/MCPForUnity#v10.3.0`.
2. Keep its transport on HTTP (its default) and press **Start** in **Window >
   MCP for Unity**. That window shows the address it listens on, usually
   `http://127.0.0.1:8080/mcp`.
3. Put that address in the `unity` section of `/settings` (or the page's
   Settings tab):

```jsonc
"unity": {
  "url": "http://127.0.0.1:8080/mcp",
  "timeout_seconds": 120,
  "plan_ttl_minutes": 10,
  "max_chars": 6000
}
```

`url` is the only required member. The endpoint has to be on this machine:
MCP for Unity listens on loopback (the `Start` button in its window is what
opens it), so a `url` that names another host is refused at start.

Nothing else is needed: no Unity Hub, no `uv`, no `uvx`, no Python. `/unity`
with no argument shows which endpoint the cell is on and what the editor
answers; `(help :unity)` is the model's guide to the tools.

## Use it

Ask for what you want in the project — "what is in my scene?", "add a coin
the player can collect", "run the EditMode tests" — and the model picks the
tool. A change is planned first and runs on the second call:

| Tool | Use |
| --- | --- |
| `unity_tools` | the editor's own tools and what each does |
| `unity_call` | one of them, with its arguments, right away |
| `unity_plan` | a whole change as steps; checks the paths and answers the plan with an id |
| `unity_apply` | runs the newest plan once, step by step, and stops at the first failure |

```lisp
;; the model does this, not you
(unity_plan "{\"steps\": [{\"tool\": \"manage_script\", \"arguments\": {\"path\": \"Assets/Scripts/Coin.cs\"}}, {\"tool\": \"manage_scene\", \"arguments\": {\"path\": \"Assets/Scenes/Main.unity\"}}]}")
```

The plan comes back in words, every step and its arguments, and nothing has
run yet; `unity_apply` with its `plan_id` runs it. A plan is checked before
it is kept: a path with `..`, a hidden file, a key or credential file, an
absolute path, a delete of `Assets`, `Packages` or `ProjectSettings`, and the
editor's own `File/Quit` are refused, with nothing sent to the editor. Only
the newest plan can run, once, and it expires after `plan_ttl_minutes`.

From the keyboard, `/unity` shows the connection and `/unity <word>` lists
the editor tools that word names.
