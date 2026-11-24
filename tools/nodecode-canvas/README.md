# nodecode-canvas

Board: a diagram the agent draws with tool calls and you open as one page. A Nodecode rewrite of [skk2010/hermes-board-plugin](https://github.com/skk2010/hermes-board-plugin) by [skk2010](https://github.com/skk2010).

A board is one JSON document under the folder you point it at. The model
draws on it with the original's six tools and its op vocabulary — nodes,
edges, free lines, groups, notes, captions, a title, a grid, and the layered
`layout` placement — and `board_render` writes the same drawing as a
standalone SVG or as one HTML page that opens in any browser. Nothing has to
be installed for the picture: no browser, no Python, no service.

## Get it

Tell the agent "add the canvas cell", or run:

```sh
nodecode add nc://nodecode-canvas
```

## Set it up

One setting, in `/settings` (or the page's Settings tab):

```jsonc
"canvas": { "dir": "~/boards", "max_nodes": 400 }
```

`dir` is where boards live: `boards/<id>.json` under it, and `render/` for
the pictures. `max_nodes` (default 400) caps how big one board may grow.
Until `dir` is set the cell still runs; a tool that needs it says so.

## Use it

Ask for a diagram — "draw the checkout flow" — and the model calls
`board_apply` with a list of ops:

```text
board_new  {id: "checkout", title: "Checkout"}
board_apply {id: "checkout", ops: [
  {op: "add_node", id: "cart", text: "Cart"},
  {op: "add_node", id: "pay",  text: "Payment", x: 360, y: 60},
  {op: "add_edge", from: "cart", to: "pay", label: "submits"},
  {op: "add_note", text: "idempotency key here", x: 60, y: 320},
  {op: "layout", rows: [["cart"], ["pay"]], direction: "down"}
]}
board_render {id: "checkout", format: "html"}
```

The tools, the same six the Hermes plugin registered:

- `board_list` — the boards in the folder, with a one-line shape of each.
- `board_get` — one board whole: the JSON document and its summary. Read it
  before editing, so the ids are the ones on the board.
- `board_new`, `board_delete` — one file each; `board_new` refuses an id
  that exists.
- `board_apply` — an ordered op list applied in one call, then saved. An op
  that refuses (an unknown op, an edge naming a node that is not there, a
  tone outside the palette, a full board) fails the call and saves nothing,
  naming the op by its index.
- `board_render` — `format: "svg"` for the drawing alone, `"html"` for the
  same drawing on a page; answers the file's path.

The operator's side is `/canvas`: `list`, `show ID`, `new ID [TITLE]`,
`delete ID`, `render ID [svg|html]`.

Ops: `add_node`, `update_node`, `move_node`, `delete_node`, `add_edge`,
`update_edge`, `delete_edge`, `add_line`, `update_line`, `delete_line`,
`add_group`, `update_group`, `delete_group`, `add_caption`, `add_note`,
`update_note`, `delete_note`, `set_title`, `set_grid`, `layout`. A node
carries `text`, an optional `sub`, a tone (`default`, `accent`, `ok`,
`warn`, `danger`, `muted`), a family (`box`, `pill`, `diamond`, `note`,
`caption`, `cloud`, `cylinder`, `document`, `hexagon`, `area`) and its own
size and position; a node with no `x`/`y` lands in the next free slot.

## What the rewrite leaves out

The Hermes plugin also docked an editor pane beside the conversation and
woke the session when you edited the board there (`board_watch`), rendered
PNG through a headless Chrome on `PATH`, and carried `bus` (shared-trunk
connections), `graph` (ranks derived from the edges) and the table node.
A cell has no editor pane, and a page that opens in your browser needs no
browser to be run by the cell — so the picture is SVG and HTML, `layout`
places what `graph` would have, and `bus` and tables are not rewritten. The
NOTICE says the same in one paragraph.

## Test it

```sh
.github/test-cell.sh tools/nodecode-canvas
```
