;;;; contracts.lisp --- what the three tools take, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for each tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value each body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the body
;;;; sees it.

(in-package #:nodecode-icon-index)

(nlk:define-contract icon-search-request
  (:doc "Search Icon Index, a free library of 40 matching 3D icons (rocket, lock, wallet, chart, chat bubble, trophy and more) in one glossy style - orange by default, and in eight accent colours. Use it when the user wants 3D icons, illustrations or hero objects for a website, landing page, app, slide deck or video. Search reads the bundled catalog, so it works offline and costs nothing. The answer carries each icon's id, title, category, what it means and a preview PNG; call icon_get for embed code and download links, or icon_save to put the files in the project.")
  ("query" :string :default ""
   :doc "what the icon should show or mean: 'security', 'payments', 'launch'. Empty lists every icon")
  ("category" :string :default ""
   :doc "an optional filter: Tech, Money, Social or Work. Empty means every category")
  ("limit" :integer :default 8
   :doc "the most icons to answer, 1 to 40 (default 8)"))

(nlk:define-contract icon-get-request
  (:doc "One Icon Index icon by id: its download links in the chosen colour (web GLB, full GLB, transparent PNG, spinning MP4), ready-to-paste embed code (iframe viewer, HTML with <model-viewer>, React, three.js, a plain <img>, and markdown for this chat) and the icons' licence. Say where the files are; the markdown field shows the icon inline.")
  ("id" :text :required t
   :doc "the icon's id from icon_search, e.g. 'rocket'")
  ("color" :string :default "orange"
   :doc "an accent colour: orange (default), blue, purple, green, pink, red, teal, yellow, or a hex like #0ea5e9. A hex works in the viewer embed only; the preset colours have real files"))

(nlk:define-contract icon-save-request
  (:doc "Download one Icon Index icon's files into a folder in the user's project (for example 'public/icons'), so a site or app can use them offline. Never overwrites an existing file. Use it when the user asks to add an icon to their project, then reference the saved GLB with <model-viewer> and the local path.")
  ("id" :text :required t
   :doc "the icon's id, e.g. 'rocket'")
  ("folder" :text :required t
   :doc "the folder to save into, created if missing: absolute, or relative to the working directory, e.g. 'public/icons'")
  ("formats" :array :of :string
   :doc "which files to save: glb and png by default; glb_full and mp4 exist in orange only")
  ("color" :string :default "orange"
   :doc "a preset accent colour: orange (default), blue, purple, green, pink, red, teal or yellow. A non-orange colour has the GLB and the PNG only"))
