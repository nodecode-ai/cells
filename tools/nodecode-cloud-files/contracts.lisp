;;;; contracts.lisp --- the five tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-cloud-files)

(defparameter +root-doc+
  "which exposed folder to work in, by its id (r0, r1, ...): omit it for the first one, and call files_list with no root to see them all")

(defparameter +sort-orders+ '("name" "mtime")
  "What files_list's sort accepts: by name, or by mtime, newest first.")

(nlk:define-contract file-listing
  (:doc "List one folder of an exposed folder, or with no root at all the exposed folders themselves. Directories come first, then names, case-insensitively; the list is capped and says so. Answer a page at a time with offset when a folder is large, sort by mtime with sort \"mtime\" to find what changed recently, and use files_search to find a file by name anywhere below a folder.")
  ("root" :string :default "" :doc +root-doc+)
  ("path" :string :default ""
   :doc "the folder to list, relative to the root (\"docs\", \"projects/api\"). Omit or use \"\" for the root's own top level. Parent traversal (..) and absolute paths are refused")
  ("sort" :string :slot sort-by :default "name" :options +sort-orders+
   :doc "order the answer by name, or by mtime with the newest first")
  ("offset" :integer :default 0
   :doc "how many entries to skip, for a folder too large for one answer")
  ("limit" :integer :default 200
   :doc "the most entries to answer; over the cell's own cap, the cap wins"))

(nlk:define-contract file-search
  (:doc "Find files and folders by name anywhere below a folder of an exposed folder. The query is a substring (case-insensitive) or a glob with * and ?, matched against the name alone, not the contents. Heavy folders (.git, node_modules, __pycache__, virtualenvs, caches) are not descended unless the query starts with a dot. An answer cut by the cap says whether visits, time or the result count stopped it.")
  ("root" :string :default "" :doc +root-doc+)
  ("path" :string :default "" :doc "the folder to search below, relative to the root; \"\" for the whole root")
  ("query" :text :required t
   :doc "the name to look for: a substring (\"notes\", \"report.md\") or a glob (\"*.md\", \"build-?\")")
  ("limit" :integer :default 50 :doc "the most results to answer; over the cell's own cap, the cap wins"))

(nlk:define-contract file-read
  (:doc "Read one file of an exposed folder as text, with the sha256 that files_save needs to write it back without overwriting someone else's change. Answer the sha256 first from a read, then save with it. A binary file, a file over the size cap or a path that leaves the root is refused, and says which.")
  ("root" :string :default "" :doc +root-doc+)
  ("path" :text :required t :doc "the file to read, relative to the root (\"docs/README.md\")"))

(nlk:define-contract file-save
  (:doc "Save text into a Markdown or plain-text file (.md, .markdown, .txt) of an exposed folder, the way the file manager's editor does: read the file with files_read first, then save with the sha256 it answered as base_sha256. If the file changed in between, the save is refused as a conflict and the answer carries the file as it is now, so nothing is overwritten blind. A new file is created with no base_sha256 when the name does not exist yet. The write is atomic: a reader never sees half of it.")
  ("root" :string :default "" :doc +root-doc+)
  ("path" :text :required t :doc "the file to write, relative to the root; it must end in .md, .markdown or .txt")
  ("text" :text :required t :doc "the whole file, as it should read afterwards")
  ("base_sha256" :string :default ""
   :doc "the sha256 files_read answered for the file as it is now; empty only when the file does not exist yet"))

(nlk:define-contract folder-create
  (:doc "Create a folder, and any missing folder above it, inside an exposed folder. It answers the path created; a folder that already exists is not an error and says so.")
  ("root" :string :default "" :doc +root-doc+)
  ("path" :text :required t :doc "the folder to create, relative to the root (\"docs/2026\", \"inbox\")"))
