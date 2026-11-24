# nodecode-icon-index

Icon Index: 40 free CC0 3D icons in one matching style - search them by meaning, get ready-to-paste embed code, or save the files into a project. A Nodecode rewrite of [minutechreview/hermes-icon-index](https://github.com/minutechreview/hermes-icon-index) by [minutechreview](https://github.com/minutechreview).

The library is one glossy set - rocket, robot, padlock, wallet, chart, chat bubble, trophy and more - in eight accent colours, made with Meshy from original concept art. The icons are CC0 1.0: free for any use, commercial included, with no credit needed. The cell carries the catalog, so search, metadata and embed code work offline and cost nothing; only `icon_save` fetches, from the library's public files.

| Surface | What it does |
| --- | --- |
| `icon_search` | Icons by what they should mean - "security", "payments", "launch" - filtered by category (Tech, Money, Social, Work) and capped by limit. Answers each icon's id, title, category, meaning and a preview PNG. |
| `icon_get` | One icon's download links in a chosen colour, ready-to-paste embed code (iframe viewer, HTML with `<model-viewer>`, React, three.js, a plain `<img>`, and markdown for the chat) and the icons' licence. |
| `icon_save` | The same files written into a folder of the project, created if missing, never overwriting one that is there. |
| `/icon-index [words]` | The same search from your side of the keyboard, as a list. Typed alone it lists the library. |

Every format is per icon: `model.glb` (web-ready, meshopt and WebP, about 300 KB) for a site, three.js and `model-viewer`; `model-full.glb` uncompressed for Blender, Unity and Unreal; `icon.png` a transparent 1024 px render for slides and docs; `turntable.mp4` a 4-second seamless spin for video. Orange has all four; the seven other preset colours have the GLB and the PNG.

## Get it

Tell the agent "add the icon-index cell", or run `nodecode add nc://icon-index`.

## Set it up

Nothing is required: the section defaults point at the library's own files and its viewer, and search works before you touch anything. To move the files or the viewer - a fork of the icon repository, a local copy - set them in `/settings` or in the `icon-index` section of `config.jsonc`:

```jsonc
"icon-index": {
  "asset_base": "https://cdn.jsdelivr.net/gh/minutechreview/3d-icons@aee7c0abd462cb20202df81a2cc171e5ab2d3f06",
  "viewer_url": "https://motion.minutechbuilds.com/icons/viewer.html",
  "timeout": 60
}
```

| Setting | What it does |
| --- | --- |
| `asset_base` | The https base the icon files are fetched from, at the library's pinned commit. The bundled catalog is the same revision. |
| `viewer_url` | The https URL of the 3D viewer the iframe and the markdown embed point at. |
| `timeout` | How long one file download may take, in seconds. |

Both URLs must be https: they are the library's own public files, and the cell carries no key to protect. A wrong shape refuses at start and names the setting.

## Use it

You do not call the tools; the model does. "Put a 3D rocket in blue in the hero section of my landing page", "find an icon for payments", "save the trophy icon into public/icons". A person can call the same three from eval, in the `icon-index` package:

```lisp
(icon-index:icon-search "security")
(icon-index:icon-get "rocket" :color "blue")
(icon-index:icon-save "trophy" "public/icons" :formats '("glb" "png"))
```

`(help :icon-index)` is the model's manual for the tools, kept while the cell runs.

### What it sends, and what it will not

Search, every URL and every embed snippet are arithmetic over the catalog bundled with the cell. `icon_save` is the one verb that reaches the network: a plain GET to `<asset_base>/icons/<id>/…` for the icon the user asked for, with no key, no cookie and nothing about the user or the conversation in it. The saved file lands in the folder the call names - absolute, or relative to the working directory - and a file already there is left untouched. The 3D viewer is a page of its own, in the user's browser, never code this image runs. Nothing else is read: no other app's tokens, no browser profile, no environment.

The save is capped at 40 MB a file. A fetch that fails says the URL, the HTTP status and, when there is one, the reason the CDN gave. On a landing page use one to three spinning icons and plain PNGs for the rest.

## Test

```sh
.github/test-cell.sh tools/nodecode-icon-index
```

The run is offline: the catalog tests need no network, and `icon_save` is proved against a stubbed `nlk:http` writing into a scratch folder.
