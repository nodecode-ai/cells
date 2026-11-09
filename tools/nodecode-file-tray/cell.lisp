;;;; cell.lisp --- the paths this session read or wrote, kept in one list.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of aydnOktay's hermes-file-tray (MIT) against Nodecode's
;;;; seams. The original watched Hermes's post_tool_call for the tool names
;;;; read_file, write_file and patch and kept the paths it saw in
;;;; state.json, one file for the whole install, capped at 40. Here:
;;;;
;;;;   * the file verbs are Lisp functions the eval tool calls, not tools of
;;;;     their own, so the record is kept by advising them -- NLE:EDIT,
;;;;     NLE:WRITE-FILE and NLE:LOOK -- which is the same seam a layer uses;
;;;;   * a tool whose arguments name a path (an MCP server's read_file, say)
;;;;     is caught at the :TOOL point, after it ran, the way the original's
;;;;     argument scan did;
;;;;   * the list is per session, under the session state key
;;;;     "file-tray.items", so two sessions never share one tray and a
;;;;     restart keeps it;
;;;;   * both tools keep their original names and answers, and /files is the
;;;;     same three words: bare or "list", and "clear".
;;;;
;;;; Nothing here reads or writes a user file of its own: a path is a string
;;;; in the session's state, and the actions the list shows are the verbs
;;;; that ran.

(defpackage #:nodecode-file-tray
  (:use #:cl)
  (:nicknames #:file-tray)
  (:export #:tray-items #:record-paths #:list-paths #:clear-paths
           #:normalize-path #:display-name #:file-tray-list #:file-tray-clear
           #:run-files #:*primer* #:*usage*))

(in-package #:nodecode-file-tray)

(nlk:define-peripheral file-tray :not-running t)

;;; --- the words ---------------------------------------------------------------

(defparameter *primer*
  "The file tray is a list of the paths this session read or wrote, kept by
the nodecode-file-tray cell: the file verbs (edit, write-file, look) and any
tool call whose arguments name a path are recorded as they run, and nothing
here reads or writes a user file of its own.
  file_tray_list   the list as JSON, with a markdown bullet list: the tool to
      call when the operator asks which files were opened or edited. Use only
      the paths it answers; never invent one.
  file_tray_clear  empty the list; call it when the operator wants a fresh
      one, not on your own.
  /files [list]    the same list for the operator; typed alone it also opens
      a panel with the one row that clears it.
  /files clear     empty the list.
A path appears once: touching it again updates its action and time in place,
and the list keeps at most file-tray.max_items of them, the oldest leaving
first. URLs and paths past 500 characters are ignored."
  "What (help :files) answers while the cell runs.")

(defparameter *usage*
  "Usage:
  /files          the paths this session read or wrote
  /files list     the same
  /files clear    empty the tray"
  "What /files answers for a word it does not know.")

(defparameter *state-key* "file-tray.items"
  "The session-state key the tray's JSON array sits under: one list a session.")

(defparameter *max-path* 500
  "The longest path kept, characters; the original's own limit.")

(defparameter *max-name* 160
  "The longest display name kept, characters.")

(defparameter *fallback-max-items* 40
  "The cap when the cell is not running: the original's default.")

(defparameter *path-argument-keys* '("path" "file" "filepath" "file_path" "paths")
  "The argument names a tool call's paths are read from -- the original's
scan, so an MCP server's read_file is recorded without naming its server.")

(defparameter *action-limit* 32
  "How long the action a tool call's own name becomes may be.")

;;; --- a path, as the original cleaned it ---------------------------------------

(defun url-p (text)
  "Whether TEXT is a URL rather than a file path."
  (loop with lower = (string-downcase text)
        for scheme in '("http://" "https://" "ftp://" "file://")
          thereis (and (<= (length scheme) (length lower))
                       (string= scheme lower :end2 (length scheme)))))

(defun collapse-separators (text)
  "TEXT with every run of / folded to one, so one path is one spelling."
  (with-output-to-string (out)
    (let ((previous nil))
      (loop for char across text
            unless (and previous (char= char #\/) (char= previous #\/))
              do (write-char char out)
            do (setf previous char)))))

(defun normalize-path (raw)
  "RAW as a path the tray keeps, or NIL: trimmed of quotes and spaces, one
separator, no URL, no line break, at most 500 characters."
  (let ((text (and (stringp raw)
                   (string-trim '(#\Space #\Tab #\Newline #\Return #\Null) raw))))
    (when text
      (setf text (string-trim '(#\" #\') text))
      (when (and (plusp (length text))
                 (<= (length text) *max-path*)
                 (not (find-if (lambda (char)
                                 (member char '(#\Newline #\Return #\Null)))
                               text))
                 (not (url-p text)))
        (let ((clean (collapse-separators (substitute #\/ #\\ text))))
          ;; a drive letter is a drive letter whatever case it was typed in
          (when (and (>= (length clean) 2) (char= (char clean 1) #\:))
            (setf (char clean 0) (char-upcase (char clean 0))))
          (subseq clean 0 (min (length clean) *max-path*)))))))

(defun display-name (path)
  "PATH's last segment, at most 160 characters: what a pane would label it."
  (let* ((trimmed (string-right-trim "/" path))
         (slash (position #\/ trimmed :from-end t))
         (base (if slash (subseq trimmed (1+ slash)) trimmed))
         (base (if (plusp (length base)) base path)))
    (subseq base 0 (min (length base) *max-name*))))

;;; --- the tray ------------------------------------------------------------------

(defun max-items ()
  "The tray's cap: the running cell's max_items, the original's 40 when the
cell is not running."
  (if (running-settings) (setting :max-items) *fallback-max-items*))

(defun tray-session ()
  "The session this call belongs to, or NIL: the eval's own, else the turn's."
  (or nlk:*scribe-session-id*
      (getf (nle:turn) :session-id)))

(defun tray-items (session)
  "The tray SESSION holds, oldest first, each (:path :action :at)."
  (let ((json (and session (nlk:store-open-p)
                    (ignore-errors (nlk:session-state-get session *state-key*)))))
    (loop for item across (or (and json
                                    (nlk:json-value (nlk:decode-json json) :array))
                               #())
          for path = (nlk:json-value item :string "path")
          when path
            collect (list :path path
                          :action (or (nlk:json-value item :string "action") "")
                          :at (or (nlk:json-value item :string "at") "")))))

(defun (setf tray-items) (items session)
  "Write ITEMS as SESSION's tray."
  (when (and session (nlk:store-open-p))
    (nlk:session-state-put
     session *state-key*
     (nlk:encode-json-object
      (coerce (loop for item in items
                    collect (nlk:make-json-object
                             "path" (getf item :path)
                             "action" (or (getf item :action) "")
                             "at" (or (getf item :at) "")))
              'vector))))
  items)

(defun record-paths (session action paths)
  "Record PATHS under ACTION in SESSION's tray: a path already on it keeps
its place and takes the newer ACTION, and past max_items the oldest leaves.
=> the tray."
  (let ((items (tray-items session))
        (at (nlk:iso-now)))
    (dolist (raw paths)
      (let ((path (normalize-path raw)))
        (when path
          (let ((entry (find path items
                             :key (lambda (item) (getf item :path)) :test #'equal)))
            (if entry
                (setf (getf entry :action) action
                      (getf entry :at) at)
                (setf items (append items (list (list :path path :action action :at at)))))))))
    (let ((limit (max-items)))
      (when (> (length items) limit)
        (setf items (last items limit))))
    (setf (tray-items session) items)))

(defun action-for (tool)
  "The action a tool call named TOOL leaves on a path."
  (let ((name (string-downcase (or tool "tool"))))
    (subseq name 0 (min (length name) *action-limit*))))

(defun clear-paths (session)
  "Empty SESSION's tray. => how many paths it held."
  (let ((count (length (tray-items session))))
    (setf (tray-items session) '())
    count))

(defun tray-markdown (items)
  "ITEMS as the list the operator and the model are shown."
  (if (null items)
      "(the file tray is empty)"
      (format nil "~{~a~^~%~}"
              (loop for item in items
                    for index from 1
                    collect (format nil "~d. `~a` (~a)"
                                    index (getf item :path) (getf item :action))))))

(defun item-json (item)
  "One tray entry as JSON."
  (nlk:make-json-object "path" (getf item :path)
                         "name" (display-name (getf item :path))
                         "action" (or (getf item :action) "")
                         "at" (or (getf item :at) "")))

(defun tray-json (items &key markdown)
  "The tray as the JSON string both tools answer."
  (nlk:encode-json-object
   (apply #'nlk:make-json-object
          "ok" t
          "count" (length items)
          "items" (coerce (loop for item in items collect (item-json item)) 'vector)
          (when markdown (list "markdown" (tray-markdown items))))))

;;; --- the record, at the two seams ----------------------------------------------

(defun record-verb (action paths)
  "Record PATHS under ACTION for the session this call belongs to, if any."
  (let ((session (tray-session)))
    (when session (record-paths session action paths))))

(defun observe-edit (next path old new)
  "(next PATH OLD NEW): NLE:EDIT, advised -- the file it changed joins the
tray once the edit landed."
  (multiple-value-prog1 (funcall next path old new)
    (record-verb "edit" (list path))))

(defun observe-write-file (next path text)
  "(next PATH TEXT): NLE:WRITE-FILE, advised -- as above."
  (multiple-value-prog1 (funcall next path text)
    (record-verb "write" (list path))))

(defun observe-look (next path)
  "(next PATH): NLE:LOOK, advised -- the image read joins the tray."
  (multiple-value-prog1 (funcall next path)
    (record-verb "read" (list path))))

(defun argument-paths (arguments)
  "The path strings ARGUMENTS, a decoded tool call's object, names."
  (let ((paths '()))
    (when (hash-table-p arguments)
      (dolist (key *path-argument-keys*)
        (let ((value (gethash key arguments)))
          (cond ((stringp value) (push value paths))
                ((and (vectorp value) (not (stringp value)))
                 (dolist (item (coerce value 'list))
                   (when (stringp item) (push item paths))))))))
    (nreverse paths)))

(defun observe-tool (op next)
  "(next OP): the :TOOL point -- a call whose arguments name paths adds
them to the tray after it ran; every other call, this cell's own included,
passes through untouched."
  (multiple-value-prog1 (funcall next op)
    (let ((paths (argument-paths (getf op :arguments))))
      (when paths
        (record-verb (action-for (getf op :name)) paths)))))

;;; --- the tools the model calls --------------------------------------------------

(defparameter +markdown-default+ t
  "Whether file_tray_list answers a markdown list unless told otherwise.")

(defun markdown-wanted-p (arguments)
  "Whether a file_tray_list call asked for the markdown list."
  (multiple-value-bind (value present)
      (if (hash-table-p arguments)
          (gethash "markdown" arguments)
          (values nil nil))
    (if present (not (null value)) +markdown-default+)))

(defun file-tray-list (arguments)
  "The file tray as JSON: ok, count, items, and a markdown list unless the
call said \"markdown\": false."
  (let ((items (tray-items (tray-session))))
    (tray-json items :markdown (markdown-wanted-p arguments))))

(defun file-tray-clear (arguments)
  "Empty the file tray: the JSON answer file_tray_clear gives."
  (declare (ignore arguments))
  (clear-paths (tray-session))
  "{\"ok\":true,\"count\":0,\"items\":[]}")

(defparameter +list-schema+
  (list :description
        (concatenate 'string
                     "List the file paths this session read or wrote, in the order they were "
                     "first touched, each with the verb it last saw (edit, write, read) and "
                     "when. Use it when the operator asks which files were opened or edited. "
                     "It reads no file and writes none: a path on it is a string the session's "
                     "verbs left there. Past the cell's max_items the oldest have left the "
                     "list; do not invent paths that are not on it.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "markdown" (nlk:make-json-object
                                   "type" "boolean"
                                   "description" "Also include a markdown bullet list. Default true."
                                   "default" t))
         "required" #()))
  "What the provider is shown for file_tray_list.")

(defparameter +clear-schema+
  (list :description
        "Empty the session's file tray. Use it when the operator wants a fresh path list, not on your own."
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object)
                                           "required" #()))
  "What the provider is shown for file_tray_clear.")

;;; --- /files --------------------------------------------------------------------

(defun run-files (args session-id)
  "/files: the tray as the original's command answered it -- bare or \"list\"
the list (bare also opens the panel whose one row clears it), \"clear\" to
empty it, anything else the usage."
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (char) (member char '(#\Space #\Tab))) text))
         (head (string-downcase (subseq text 0 space)))
         (session (or session-id (tray-session)))
         (items (tray-items session)))
    (cond
      ((string= head "clear")
       (clear-paths session)
       (format nil "file tray cleared~@[ (~d path~:p went)~]" (and (plusp (length items))
                                                                   (length items))))
      ((member head '("" "list" "show" "md" "markdown") :test #'string=)
       (let ((list (tray-markdown items)))
         (if (string= head "")
             (values list
                     (nle:list-dialog
                      :title "File tray"
                      :summary (format nil "~d path~:p this session~:p"
                                       (length items))
                      :rows (list (nle:run-row "Clear the tray" "/files clear"
                                                :detail "the session's list starts again"))))
             list)))
      (t *usage*))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell file-tray
  (:section ("file-tray")
    (:guide "nothing here is required: max_items bounds the list; the tray records itself from edit, write-file, look and any tool call that names a path")
    ("max_items" :integer :default 40 :min 1
                 :doc "the most paths one session's tray keeps; past it the oldest leave"))
  (:tool "file_tray_list" +list-schema+ 'file-tray-list)
  (:tool "file_tray_clear" +clear-schema+ 'file-tray-clear)
  (:hook 'nle:edit #'observe-edit)
  (:hook 'nle:write-file #'observe-write-file)
  (:hook 'nle:look #'observe-look)
  (:hook :tool #'observe-tool)
  (:help :files "the file tray: the paths this session read or wrote" *primer*)
  (:command "files" 'run-files
            :description "Paths this session read or wrote (list | clear)"))
