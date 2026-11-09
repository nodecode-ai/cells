;;;; cell.lisp --- the section, the five tools, /mediarag, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What the original registered through a Hermes plugin's ctx.register_tool is
;;;; here one NLE:DEFINE-CELL: five tools the model calls, a command the
;;;; operator types, a topic on the manual, and the `mediarag' section they run
;;;; on. The corpus lives in one file under corpus_root; nothing is uploaded.

(in-package #:nodecode-mediarag)

(defparameter +primer+
  "mediarag: one local index over your own documents, notes, images and audio.
  mediarag_search      ranked rows: file, kind, span (seconds for audio), snippet
  mediarag_add_note    store a note; searchable at once
  mediarag_ingest_dir  index a folder of md/txt/html/csv/rst, srt/vtt subtitles,
                        images, and audio
  mediarag_add_source  one file, one folder, or an http(s) URL
  mediarag_status      what the corpus holds
Reading stays local: the corpus is one file under corpus_root. Ranking fuses a
keyword lane with a fuzzy one over character 4-grams -- there is no embedding
model in this rewrite, so a phrase the document actually contains is the surest
way to find it, and a misspelling or a different ending still matches.
From eval the same five are (mediarag:search-text \"...\"), (mediarag:add-note
\"...\"), (mediarag:ingest-dir \"path\"), (mediarag:add-source \"...\") and
(mediarag:status-text). (help :mediarag) has this while the cell runs; the
section is in ~/.nodecode/config.jsonc under mediarag."
  "What (help :mediarag) answers while the cell runs.")

;;; --- the section, read once --------------------------------------------------

(defun read-settings (values table)
  "The section's members, with the corpus root normalized. A required member
left out refuses at the start -- the refusal is the wizard -- and so does a
value the declaration cannot hold."
  (declare (ignore table))
  (unless (getf values :corpus-root)
    (nlk:config-error "mediarag.corpus_root is unset: name the directory the corpus lives in"))
  (setf (getf values :corpus-root)
        (namestring (uiop:ensure-directory-pathname
                     (expand-path (getf values :corpus-root)))))
  values)

;;; --- the arguments each tool is held to --------------------------------------

(defun parameters (&rest entries)
  "A JSON-Schema parameters object from (NAME TYPE REQUIRED-P DESCRIPTION) entries."
  (let ((properties (nlk:json-object)) (required '()))
    (loop for (name type required-p description) on entries by #'cddddr
          do (setf (gethash name properties)
                   (nlk:json-object "type" type "description" description))
             (when required-p (push name required)))
    (nlk:json-object "type" "object"
                     "properties" properties
                     "required" (coerce (nreverse required) 'vector))))

(defun search-schema ()
  (list :description
        "Search the corpus (documents, notes, image and audio names) and answer the ranked rows: file, kind, span and snippet. A query of words the documents contain finds them; the fuzzy lane also survives a typo."
        :parameters (parameters "query" "string" t "natural-language query, or the words you expect the file to contain"
                                "k" "integer" nil "how many rows to answer; the section's default_results when left out"
                                "kind" "string" nil "narrow to one kind of row: text, image or audio")))

(defun note-schema ()
  (list :description "Store a text note in the corpus, searchable at once."
        :parameters (parameters "text" "string" t "the note's text")))

(defun ingest-schema ()
  (list :description
        "Bulk-index a folder: markdown, text, html, csv, subtitles, images and audio. Unreadable formats are counted as skipped, never fatal."
        :parameters (parameters "path" "string" t "the folder to index, its subfolders included")))

(defun source-schema ()
  (list :description
        "Index one source: a local file, a local folder, or an http(s) URL (downloaded into the corpus cache first)."
        :parameters (parameters "source" "string" t "a file path, a folder path, or an http(s) URL")))

(defun status-schema ()
  (list :description "What the corpus holds: rows, sources and the corpus root."
        :parameters (parameters)))

;;; --- the five tools -----------------------------------------------------------

(defun search-tool (arguments)
  (search-text (nlk:json-value arguments :string "query")
               :k (nlk:json-value arguments :integer "k")
               :kind (nlk:json-value arguments :string "kind")))

(defun note-tool (arguments)
  (add-note (nlk:json-value arguments :string "text")))

(defun ingest-tool (arguments)
  (ingest-dir (nlk:json-value arguments :string "path")))

(defun source-tool (arguments)
  (add-source (nlk:json-value arguments :string "source")))

(defun status-tool (arguments)
  (declare (ignore arguments))
  (status-text))

;;; --- /mediarag ------------------------------------------------------------------

(defun run-command (args session-id)
  "(/mediarag) says what the corpus holds; (/mediarag ferry times) searches it."
  (declare (ignore session-id))
  (let ((args (string-trim +space+ (or args ""))))
    (if (zerop (length args))
        (status-text)
        (search-text args))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell mediarag
  ;; corpus_root is the one required member: an unanswered section refuses at
  ;; the start and names it, which is the whole of the setup.
  (:section ("mediarag")
    (:guide "corpus_root is the one step: the directory the corpus lives in, so a folder you keep beside your documents; max_chunk_chars is how long one indexed row may be; default_results is how many rows a search answers when the call names none")
    ("corpus_root" :path :required t
                   :doc "the directory the corpus lives in")
    ("max_chunk_chars" :integer :default 1500 :min 100
                        :doc "longest indexed row, in characters")
    ("default_results" :integer :default 5 :min 1
                        :doc "rows one search answers when the call names no k"))
  (:settings 'read-settings)
  (:tool "mediarag_search" (search-schema) 'search-tool)
  (:tool "mediarag_add_note" (note-schema) 'note-tool)
  (:tool "mediarag_ingest_dir" (ingest-schema) 'ingest-tool)
  (:tool "mediarag_add_source" (source-schema) 'source-tool)
  (:tool "mediarag_status" (status-schema) 'status-tool)
  (:command "mediarag" 'run-command
            :description "What the corpus holds, or a search over it")
  (:help :mediarag "MediaRAG: local search over your own documents, notes, images and audio" +primer+)
  (:start (lambda ()
            (ensure-directories-exist (store-path))
            nil)))
