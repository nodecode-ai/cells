# nodecode-photofresco

PhotoFresco: drives the browser image editor from Nodecode - layers, masks, adjustments, previews and PNG/PSD export. A Nodecode rewrite of [photofresco/photofresco-agent](https://github.com/photofresco/photofresco-agent) by [photofresco](https://github.com/photofresco).

## Get it

Tell the agent "add the photofresco cell", or run `nodecode add nc://photofresco`.

## Set it up

Nothing is required to start. The cell opens a Chrome, Edge or Chromium
window of its own when it connects; name one in `browser` if none is on the
PATH. `read_roots` and `write_roots` are the only folders the editor may
take a local image from and put an export into, and they default to none.

```jsonc
{
  "photofresco": {
    "enabled": true,
    "read_roots": ["/home/me/Pictures"],
    "write_roots": ["/home/me/Pictures/exports"]
  }
}
```

## Use it

Say what you want done to the image, and the trio does the rest:

- `photofresco_connect` opens https://photofresco.com/app/ in the cell's own
  browser window and waits for the person there to allow the agent; it can
  ask for `read`, `edit`, `export`, `external-write` or `spend` and a credit
  budget, which the editor shows before anything is granted.
- `photofresco_ops` reads the session (`status`, `documents`), the operations
  the current grants allow (`operations`), and runs one (`execute`) by its
  canonical name - `layer.add`, `mask.add`, `adjust.add`, `render.preview`,
  `crop.apply`, `export.artifact` and the rest; `outcome` reconciles a
  request whose answer was lost.
- `photofresco_file` opens a local image in a tab (`open`) and exports the
  document to a file (`export`, png/psd/pfd).

`/photofresco status` and `/photofresco layer.add` do the same from the
prompt, and `(help :photofresco)` carries the manual the model reads. From
eval, the same calls answer as `photofresco:connect`, `photofresco:status`,
`photofresco:execute` and so on.

The editor enforces every grant on every call; this cell adds no authority of
its own. Nothing of the user's own browser is touched: the profile is the
cell's, under the Nodecode home.

## What the original does that this does not

The original lists one MCP tool per editor operation; a cell offers one
executor over the same catalog, which the editor answers live from
`operations`, so the surface stays a few dozen tools whatever the editor
grows. The Node binding of the original is replaced by a WebSocket to the
same DevTools port, and the page protocol (one fixed function, data never
code) is spoken as it is written.
