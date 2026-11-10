;;;; cell.lisp --- the plan the agent draws, whose status comes from its real work.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of 0xbyt4/hermes-canvas (see NOTICE). A canvas is one plan of
;;;; boxes for one chat session, drawn by the model with the original's four
;;;; tools -- canvas_draw, canvas_update, canvas_get, canvas_next -- and kept
;;;; as one JSON document in the cell's folder.
;;;;
;;;; WHAT SETS A BOX'S STATUS. The model never says a box is done: a box
;;;; carries file globs and a check command, and the cell reads the work the
;;;; harness already reports. A tool result names the files its call wrote
;;;; (its workspace diff, or a write-file/edit receipt) and the shell command
;;;; it ran with its exit; a box whose globs own a written file is `doing',
;;;; a box whose exact check command exits 0 is `done' and exits otherwise
;;;; `failed'. A file written that no box owns becomes a drift box -- the one
;;;; the operator may adopt or ask to undo. A write into a removed box's own
;;;; files is refused before it happens, on the :tool point, with no disk read.
;;;;
;;;; The operator's side is /plan-canvas: it shows the plan, and its note,
;;;; remove, restore, answer and adopt words are the pane's own edits. Each is
;;;; kept on the document as a user edit, and the model reads them back in
;;;; canvas_get -- and hears them at the round's start -- as the operator's
;;;; instructions.

(in-package #:nodecode-plan-canvas)

(nlk:define-peripheral plan-canvas :not-running t :lock "plan-canvas")

;;; --- what the original bounds ---------------------------------------------------

(defparameter +max-text+ 4000 "The longest one field may carry.")
(defparameter +max-list+ 50 "The most items one list field may carry.")
(defparameter +max-glob+ 256 "The longest file glob.")
(defparameter +max-edits+ 40 "The user edits kept on a canvas.")
(defparameter +coordinate-limit+ 100000 "The farthest a box may be pinned from the origin.")
(defparameter +edit-limit+ 160 "Characters of one user edit the round reminder repeats.")
(defparameter +blanks+ '(#\Space #\Tab #\Newline #\Return #\Page)
  "What a field is trimmed of.")
(defparameter +statuses+ '("todo" "doing" "done" "failed")
  "The four statuses the original has, in its order.")
(defparameter +kinds+ '("component" "question" "note" "drift")
  "The box kinds: planned work, a decision the operator must make, a note, a written
file no box planned.")
(defparameter +write-verbs+ '("write-file" "edit")
  "The eval verbs whose receipt names a file they wrote.")
(defparameter +user-only-ops+ '("note" "revert")
  "The ops the operator's own words make; the model may not send them.")

;;; --- one line, or a refusal -----------------------------------------------------

(defun trim (text) (string-trim +blanks+ text))

(defun control-p (char)
  "Whether CHAR is a line break or another control character: a label or an option
is read inside the operator's instructions, so it must stay on one line there."
  (let ((code (char-code char)))
    (or (< code 32) (= code 127) (= code 133) (= code 8232) (= code 8233))))

(defun one-line (text)
  "TEXT with every control character a space."
  (map 'string (lambda (char) (if (control-p char) #\Space char)) text))

(defun one-line-p (text) (notany #'control-p text))

(defun field (table key)
  "KEY's value on TABLE, a decoded JSON object, or NIL."
  (and (hash-table-p table) (gethash key table)))

(defun text-of (table key label &key (limit +max-text+) required)
  "KEY's value on TABLE as trimmed text, or a refusal naming LABEL."
  (let ((value (field table key)))
    (cond ((or (null value) (eq value :null))
           (when required (fail "~a is required" label))
           "")
          ((not (stringp value)) (fail "~a must be a string" label))
          (t (let ((text (trim value)))
               (when (and required (zerop (length text))) (fail "~a is required" label))
               (when (> (length text) limit)
                 (fail "~a is longer than ~d characters" label limit))
               text)))))

(defun line-of (table key label &key (limit 200) required)
  "TEXT-OF, refused when it carries a control character."
  (let ((text (text-of table key label :limit limit :required required)))
    (unless (one-line-p text) (fail "~a must be one line, without control characters" label))
    text))

(defun number-of (table key label &key required)
  "KEY's value on TABLE as a real number, or a refusal naming LABEL."
  (let ((value (field table key)))
    (cond ((or (null value) (eq value :null))
           (when required (fail "~a is required" label))
           nil)
          ((realp value) value)
          (t (fail "~a must be a number" label)))))

(defun list-of (table key label)
  "KEY's value on TABLE as a list of non-empty strings, or a refusal naming LABEL."
  (let ((value (field table key)))
    (let ((items (cond ((or (null value) (eq value :null)) nil)
                       ((stringp value) (list value))
                       ((vectorp value) (coerce value 'list))
                       (t (fail "~a must be a list of strings" label)))))
      (let ((strings (mapcar (lambda (item)
                               (unless (stringp item) (fail "~a must be a list of strings" label))
                               (trim item))
                             items)))
        (let ((kept (remove "" strings :test #'string=)))
          (when (> (length kept) +max-list+)
            (fail "~a has more than ~d items" label +max-list+))
          kept)))))

(defun globs-of (table key)
  "KEY's value on TABLE as file globs."
  (let ((globs (list-of table key "files")))
    (dolist (glob globs)
      (when (> (length glob) +max-glob+)
        (fail "file glob is longer than ~d characters: ~a..." +max-glob+ (subseq glob 0 (min 40 (length glob))))))
    globs))

(defun id-of (table key)
  "KEY's value on TABLE as a box id, or a refusal."
  (let ((id (text-of table key "id" :limit 64 :required t)))
    (unless (and (plusp (length id))
                 (alphanumericp (char id 0))
                 (every (lambda (char) (or (alphanumericp char) (member char '(#\_ #\. #\-)))) id))
      (fail "box id ~s must use letters, digits, '_', '.' or '-'" id))
    id))

;;; --- globs: the paths a box owns -------------------------------------------------

(defun glob-wild-p (pattern)
  "Whether PATTERN carries a wildcard."
  (find-if (lambda (char) (member char '(#\* #\? #\{ #\}))) pattern))

(defun split-segments (path)
  "PATH's segments without the empty ones."
  (remove "" (uiop:split-string path :separator "/") :test #'string=))

(defun segment-match-p (pattern name &optional (at 0) (from 0))
  "Whether one path segment NAME matches PATTERN, whose * and ? stay inside it."
  (cond ((>= at (length pattern)) (>= from (length name)))
        ((char= #\* (char pattern at))
         (or (segment-match-p pattern name (1+ at) from)
             (and (< from (length name)) (segment-match-p pattern name at (1+ from)))))
        ((char= #\? (char pattern at))
         (and (< from (length name)) (segment-match-p pattern name (1+ at) (1+ from))))
        (t (and (< from (length name))
                (char-equal (char pattern at) (char name from))
                (segment-match-p pattern name (1+ at) (1+ from))))))

(defun segments-match-p (patterns path)
  "Whether the segment list PATH matches PATTERNS, where ** spans directories."
  (cond ((null patterns) (null path))
        ((string= (first patterns) "**")
         (or (segments-match-p (rest patterns) path)
             (and path (segments-match-p patterns (rest path)))))
        ((null path) nil)
        ((segment-match-p (first patterns) (first path))
         (segments-match-p (rest patterns) (rest path)))
        (t nil)))

(defun brace-expand (pattern)
  "PATTERN with every {a,b} spelled out as one pattern each."
  (let ((open (position #\{ pattern)))
    (if (null open)
        (list pattern)
        (let ((close (position #\} pattern :start open)))
          (if (null close)
              (list pattern)
              (loop for option in (uiop:split-string (subseq pattern (1+ open) close) :separator ",")
                    append (brace-expand (concatenate 'string (subseq pattern 0 open) option
                                                      (subseq pattern (1+ close))))))))))

(defun glob-match-p (glob path)
  "Whether PATH, project-relative and in posix form, is a file GLOB covers.

** spans directories; * and ? stay inside one segment; {a,b} is either
alternative. A pattern with no wildcard, and one that ends with /, also covers
everything under it -- so src/payments covers src/payments/charge.ts."
  (let ((pattern (trim glob))
        (path (trim path)))
    (loop while (uiop:string-prefix-p "./" pattern) do (setf pattern (subseq pattern 2)))
    (cond ((zerop (length pattern)) nil)
          ((glob-wild-p pattern)
           (let ((trimmed (if (uiop:string-suffix-p pattern "/")
                              (concatenate 'string pattern "**")
                              pattern)))
             (some (lambda (one) (segments-match-p (split-segments one) (split-segments path)))
                   (brace-expand trimmed))))
          (t (or (string= pattern path)
                 (and (uiop:string-prefix-p pattern path)
                      (> (length path) (length pattern))
                      (char= #\/ (char path (length pattern))))
                 (string= pattern (concatenate 'string path "/")))))))

(defun expand-tilde (path)
  "PATH with a leading ~/ read as the account's home."
  (if (and (> (length path) 1) (char= #\~ (char path 0)) (char= #\/ (char path 1)))
      (namestring (nlk:home-path (subseq path 2)))
      path))

(defun absolute-path-p (path)
  "Whether PATH starts at the root."
  (and (plusp (length path)) (char= #\/ (char path 0))))

(defun normalize-segments (segments)
  "SEGMENTS with . dropped and .. folded; NIL when the walk climbs above its root."
  (let ((stack '()))
    (dolist (segment segments)
      (cond ((or (string= segment "") (string= segment ".")))
            ((string= segment "..")
             (if stack (pop stack) (return-from normalize-segments nil)))
            (t (push segment stack))))
    (nreverse stack)))

(defun join-segments (segments)
  "SEGMENTS as one posix path."
  (format nil "~{~a~^/~}" segments))

(defun relativize (path root)
  "PATH as a project-relative posix path under ROOT, or NIL when it lies outside it.

A relative PATH is taken as relative to ROOT, as the original's did."
  (let ((path (expand-tilde (trim path)))
        (root (expand-tilde (trim root))))
    (when (or (zerop (length path)) (zerop (length root))) (return-from relativize nil))
    (let ((root-segments (normalize-segments (split-segments root)))
          (path-segments (split-segments path)))
      (when (null root-segments) (return-from relativize nil))
      (if (absolute-path-p path)
          (when (and (<= (length root-segments) (length path-segments))
                     (equal root-segments (subseq path-segments 0 (length root-segments))))
            (let ((rest (normalize-segments (nthcdr (length root-segments) path-segments))))
              (and rest (plusp (length rest)) (join-segments rest))))
          (let ((rest (normalize-segments path-segments)))
            (and rest (plusp (length rest)) (join-segments rest)))))))

;;; --- the document ---------------------------------------------------------------

(defstruct (box (:constructor make-box))
  "One box: what it delivers, the files it writes, the command that proves it, and the
status the work itself set."
  (id "" :type string)
  (label "" :type string)
  (kind "component" :type string)
  (files nil)
  (check "" :type string)
  (after nil)
  (status "todo" :type string)
  (removed nil)
  (writes 0 :type integer)
  (written nil)
  (check-exit nil)
  (check-output "" :type string)
  (options nil)
  (answer "" :type string)
  (note "" :type string)
  (revert-asked nil)
  (x nil)
  (y nil))

(defstruct (doc (:constructor make-doc))
  "One chat's canvas: the plan's boxes, its revision, and the operator's edits the model
has not read yet."
  (id "" :type string)
  (session "" :type string)
  (title "" :type string)
  (root "" :type string)
  (boxes nil)
  (revision 0 :type integer)
  (created 0)
  (updated 0)
  (edits nil)
  (drift 0 :type integer))

(defun doc-box (doc id)
  "DOC's box ID, or NIL."
  (find id (doc-boxes doc) :key #'box-id :test #'string=))

(defun require-box (doc id)
  "DOC's box ID, or a refusal."
  (or (doc-box doc id) (fail "no box with id ~s" id)))

(defun active-boxes (doc)
  "DOC's boxes the operator has not removed."
  (remove-if #'box-removed (doc-boxes doc)))

(defun component-boxes (doc)
  "DOC's active planned work."
  (remove-if-not (lambda (box) (and (not (box-removed box)) (string= (box-kind box) "component")))
                 (doc-boxes doc)))

(defun box-from-plan (table)
  "A box from one entry of canvas_draw's plan, or a refusal."
  (unless (hash-table-p table) (fail "each box must be an object"))
  (let* ((options (list-of table "options" "options"))
         (given (text-of table "kind" "kind" :limit 32))
         (kind (if (plusp (length given)) given (if options "question" "component"))))
    (unless (member kind +kinds+ :test #'string=)
      (fail "box kind must be one of ~{~a~^, ~}" +kinds+))
    (when (string= kind "drift")
      (fail "drift boxes come from writes outside the plan; they cannot be drawn. ~
Leave them out: a redraw keeps them"))
    (dolist (option options)
      (unless (one-line-p option) (fail "options must be one line, without control characters")))
    (make-box :id (id-of table "id")
              :label (line-of table "label" "label" :required t)
              :kind kind
              :files (globs-of table "files")
              :check (text-of table "check" "check" :limit 500)
              :after (list-of table "after" "after")
              :options options)))

(defun validate-plan (boxes)
  "Refuse duplicate ids, unknown dependencies and dependency cycles."
  (let* ((ids (mapcar #'box-id boxes))
         (duplicates (remove-duplicates (remove-if-not (lambda (id) (> (count id ids :test #'string=) 1)) ids)
                                        :test #'string=)))
    (when duplicates (fail "duplicate box ids: ~{~a~^, ~}" duplicates))
    (dolist (box boxes)
      (let ((unknown (remove-if-not (lambda (after) (not (member after ids :test #'string=)))
                                    (box-after box))))
        (when unknown (fail "box ~s waits for unknown boxes: ~{~a~^, ~}" (box-id box) unknown))))
    (let ((state (make-hash-table :test #'equal)))
      (labels ((visit (id path)
                 (let ((mark (gethash id state)))
                   (cond ((eql mark :done) nil)
                         ((eql mark :open)
                          (let ((at (position id path :test #'string=)))
                            (fail "dependency cycle: ~{~a~^ -> ~}"
                                  (append (if at (nthcdr at path) path) (list id)))))
                         (t (setf (gethash id state) :open)
                            (dolist (after (box-after (doc-box (make-doc :boxes boxes) id)))
                              (visit after (append path (list id))))
                            (setf (gethash id state) :done))))))
        (dolist (box boxes) (visit (box-id box) nil))))))

(defun ready-boxes (doc)
  "Planned work that can start now: not removed, not done, and every box it waits for done.

A removed box does not block what waits for it. Order follows the document."
  (let ((done (remove-if-not (lambda (box) (or (string= (box-status box) "done") (box-removed box)))
                              (doc-boxes doc))))
    (remove-if-not (lambda (box)
                     (and (string= (box-kind box) "component")
                          (member (box-status box) '("todo" "failed") :test #'string=)
                          (every (lambda (after) (find after done :key #'box-id :test #'string=))
                                 (box-after box))))
                   (active-boxes doc))))

(defun without-cd-prefix (text)
  "TEXT without a leading `cd DIR &&`, as the original's normalize_command dropped it."
  (if (and (uiop:string-prefix-p "cd " text)
           (not (uiop:string-prefix-p "cd -" text)))
      (let ((amp (search "&&" text)))
        (if (and amp
                 (not (find-if (lambda (char) (member char '(#\; #\|))) (subseq text 0 amp))))
            (string-left-trim '(#\Space #\Tab) (subseq text (+ amp 2)))
            text))
      text))

(defun check-command (check)
  "CHECK in the one form the canvas compares commands in: a leading cd prefix and a
trailing 2>&1 dropped, quoting and whitespace collapsed."
  (let ((text (trim check)))
    (setf text (string-trim '(#\Space #\Tab) text))
    (when (uiop:string-suffix-p text "2>&1")
      (setf text (string-trim '(#\Space #\Tab) (subseq text 0 (- (length text) 4)))))
    (when (and (plusp (length text)) (char= #\; (char text (1- (length text)))))
      (setf text (string-trim '(#\Space #\Tab) (subseq text 0 (1- (length text))))))
    (let ((text (without-cd-prefix text)))
      (format nil "~{~a~^ ~}"
              (remove "" (uiop:split-string text :separator '(#\Space #\Tab #\Newline))
                      :test #'string=)))))

;;; --- the document on the wire ---------------------------------------------------

(defun box-json (box)
  "BOX as the JSON object the document file and canvas_get carry."
  (nlk:json-object
   "id" (box-id box)
   "label" (box-label box)
   "kind" (box-kind box)
   "files" (coerce (box-files box) 'vector)
   "check" (box-check box)
   "after" (coerce (box-after box) 'vector)
   "status" (box-status box)
   "removed" (if (box-removed box) t :false)
   "writes" (box-writes box)
   "written" (coerce (box-written box) 'vector)
   "check_exit" (or (box-check-exit box) :null)
   "check_output" (box-check-output box)
   "options" (coerce (box-options box) 'vector)
   "answer" (box-answer box)
   "note" (box-note box)
   "revert_asked" (if (box-revert-asked box) t :false)
   "x" (or (box-x box) :null)
   "y" (or (box-y box) :null)))

(defun doc-json (doc)
  "DOC as the one object its file holds."
  (nlk:json-object
   "id" (doc-id doc)
   "session" (doc-session doc)
   "title" (doc-title doc)
   "root" (doc-root doc)
   "boxes" (coerce (mapcar #'box-json (doc-boxes doc)) 'vector)
   "revision" (doc-revision doc)
   "created" (doc-created doc)
   "updated" (doc-updated doc)
   "edits" (coerce (doc-edits doc) 'vector)
   "drift" (doc-drift doc)))

(defun string-list (table key)
  "KEY's value on TABLE as a list of strings, whatever the file held."
  (let ((value (field table key)))
    (cond ((vectorp value) (remove-if-not #'stringp (coerce value 'list)))
          ((stringp value) (list value))
          (t nil))))

(defun box-from-json (table)
  "A box from the document file's own object; stored data is trusted, unknown keys dropped."
  (make-box :id (or (field table "id") "")
            :label (or (field table "label") "")
            :kind (or (field table "kind") "component")
            :files (string-list table "files")
            :check (or (field table "check") "")
            :after (string-list table "after")
            :status (or (field table "status") "todo")
            :removed (eq t (field table "removed"))
            :writes (or (nlk:json-value table :integer "writes") 0)
            :written (string-list table "written")
            :check-exit (nlk:json-value table :integer "check_exit")
            :check-output (or (field table "check_output") "")
            :options (string-list table "options")
            :answer (or (field table "answer") "")
            :note (or (field table "note") "")
            :revert-asked (eq t (field table "revert_asked"))
            :x (nlk:json-value table :number "x")
            :y (nlk:json-value table :number "y")))

(defun doc-from-json (table)
  "A canvas from the object its file holds."
  (let ((boxes (field table "boxes")))
    (make-doc :id (or (field table "id") "")
              :session (or (field table "session") "")
              :title (or (field table "title") "")
              :root (or (field table "root") "")
              :boxes (loop for entry across (if (vectorp boxes) boxes #())
                            when (hash-table-p entry) collect (box-from-json entry))
              :revision (or (nlk:json-value table :integer "revision") 0)
              :created (or (nlk:json-value table :number "created") 0)
              :updated (or (nlk:json-value table :number "updated") 0)
              :edits (string-list table "edits")
              :drift (or (nlk:json-value table :integer "drift") 0))))

;;; --- where a canvas lives ---------------------------------------------------------

(defparameter +default-folder+ "plan-canvas/"
  "The folder under the home canvases go in when the section names none.")

(defun folder ()
  "The folder the canvas files live in: the section's folder, else one under the home."
  (let* ((set (and *plan-canvas* (ignore-errors (setting :folder))))
         (name (and set (ignore-errors (namestring set))))
         (name (and name (trim name))))
    (uiop:ensure-directory-pathname (if (and name (plusp (length name))) name (nlk:home +default-folder+)))))

(defun session-key (session)
  "SESSION as a file name: its own characters in hex, so one chat is always one file."
  (with-output-to-string (out)
    (loop for char across session do (format out "~(~2,'0x~)" (logand (char-code char) #xff)))))

(defun doc-file (session)
  "The file SESSION's canvas is kept in."
  (merge-pathnames (format nil "~a.json" (session-key session)) (folder)))

(defun digest (text)
  "TEXT's FNV-1a in ten hex digits: a canvas id that a restart keeps."
  (let ((hash 2166136261))
    (loop for char across text
          do (setf hash (logand (* (logxor hash (logand (char-code char) #xff)) 16777619) #xffffffff)))
    (format nil "~(~10,'0x~)" (mod hash (expt 16 10)))))

(defun canvas-id (session)
  "The id a fresh canvas for SESSION takes: cv- and ten hex digits, as the original's."
  (format nil "cv-~a" (digest session)))

(defun save-doc (doc)
  "Write DOC whole to its file. => the pathname written."
  (let ((path (doc-file (doc-session doc))))
    (setf (doc-updated doc) (get-universal-time))
    (ensure-directories-exist (uiop:pathname-directory-pathname path))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string (nlk:encode-json-object (doc-json doc)) out))
    path))

(defun load-doc (session)
  "SESSION's canvas, or NIL when it has none (or its file is unreadable)."
  (let ((path (doc-file session)))
    (when (probe-file path)
      (let ((table (handler-case (nlk:decode-json (uiop:read-file-string path) :whole t)
                     (error () nil))))
        (when (hash-table-p table) (doc-from-json table))))))

(defun doc-of (session)
  "SESSION's canvas, or a refusal the model can read."
  (or (load-doc session) (fail "this chat has no canvas yet; call canvas_draw first")))

;;; --- what the operator's words say --------------------------------------------------

(defun note-edit (doc text)
  "Keep TEXT, one operator edit, on DOC until the model reads it."
  (setf (doc-edits doc)
        (last (append (doc-edits doc) (list (subseq text 0 (min (length text) 400))))
              +max-edits+))
  (setf (doc-revision doc) (1+ (doc-revision doc)))
  text)

;;; --- the ops ---------------------------------------------------------------------------

(defparameter +op-names+ '("add" "remove" "restore" "rename" "set_files" "set_check"
                           "set_after" "set_status" "answer" "move" "adopt" "note"
                           "link" "unlink" "revert")
  "Every op, as the original's _OPS names them.")

(defun op-add (doc op)
  (let ((box (box-from-plan (field op "box"))))
    (when (doc-box doc (box-id box)) (fail "box ~s already exists" (box-id box)))
    (validate-plan (append (doc-boxes doc) (list box)))
    (setf (doc-boxes doc) (append (doc-boxes doc) (list box)))
    (format nil "added ~a" (box-id box))))

(defun op-remove (doc op)
  (let ((box (require-box doc (id-of op "id"))))
    (setf (box-removed box) t)
    (format nil "removed ~a" (box-id box))))

(defun op-restore (doc op)
  (let ((box (require-box doc (id-of op "id"))))
    (setf (box-removed box) nil
          (box-revert-asked box) nil)
    (format nil "restored ~a" (box-id box))))

(defun op-rename (doc op)
  (let ((box (require-box doc (id-of op "id"))))
    (setf (box-label box) (line-of op "label" "label" :required t))
    (format nil "renamed ~a" (box-id box))))

(defun op-set-files (doc op)
  (let ((box (require-box doc (id-of op "id"))))
    (setf (box-files box) (globs-of op "files"))
    (format nil "set files of ~a" (box-id box))))

(defun op-set-check (doc op)
  (let* ((box (require-box doc (id-of op "id")))
         (check (text-of op "check" "check" :limit 500)))
    (when (and (plusp (length (box-check box))) (zerop (length check)))
      (fail "box ~s has a check; change it instead of removing it" (box-id box)))
    (when (and (not (string= check (box-check box)))
               (member (box-status box) '("done" "failed") :test #'string=))
      ;; A new check has not run yet: the old result does not count for it.
      (setf (box-status box) (if (plusp (box-writes box)) "doing" "todo")
            (box-check-exit box) nil
            (box-check-output box) ""))
    (setf (box-check box) check)
    (format nil "set check of ~a" (box-id box))))

(defun op-set-after (doc op)
  (let* ((box (require-box doc (id-of op "id")))
         (previous (box-after box)))
    (setf (box-after box) (list-of op "after" "after"))
    (handler-case (validate-plan (doc-boxes doc))
      (plan-canvas-error (condition) (setf (box-after box) previous) (error condition)))
    (format nil "set dependencies of ~a" (box-id box))))

(defun op-set-status (doc op)
  (let* ((box (require-box doc (id-of op "id")))
         (status (text-of op "status" "status" :limit 32)))
    (unless (member status +statuses+ :test #'string=)
      (fail "status must be one of ~{~a~^, ~}" +statuses+))
    (when (and (plusp (length (box-check box))) (member status '("done" "failed") :test #'string=))
      ;; Only the check's exit code, read by a hook, makes a checked box done or failed.
      (fail "box ~s has a check: run `~a` with (sh ...) inside eval; its exit code sets done or failed"
            (box-id box) (box-check box)))
    (setf (box-status box) status)
    (format nil "set status of ~a to ~a" (box-id box) status)))

(defun op-answer (doc op)
  (let ((box (require-box doc (id-of op "id"))))
    (unless (string= (box-kind box) "question") (fail "box ~s is not a question" (box-id box)))
    (let ((answer (text-of op "answer" "answer" :limit 500 :required t)))
      (setf (box-answer box) answer
            (box-status box) "done")
      (format nil "answered ~a: ~a" (box-id box) answer))))

(defun op-adopt (doc op)
  "Make a drift box -- a file written outside every box -- a planned box that owns it."
  (let ((box (require-box doc (id-of op "id"))))
    (unless (string= (box-kind box) "drift") (fail "box ~s is not a drift box" (box-id box)))
    (when (box-removed box) (fail "box ~s was removed; restore it first" (box-id box)))
    (when (field op "label")
      (setf (box-label box) (line-of op "label" "label" :required t)))
    (setf (box-kind box) "component"
          (box-status box) "done"
          (box-revert-asked box) nil)
    (format nil "added drift file ~{~a~^, ~} to the plan as ~a" (box-files box) (box-id box))))

(defun op-link (doc op)
  (let* ((box (require-box doc (id-of op "id")))
         (dep (require-box doc (id-of op "after"))))
    (unless (string= (box-kind box) "component")
      (fail "box ~s cannot wait for other boxes: only planned work boxes can" (box-id box)))
    (unless (and (member (box-kind dep) '("component" "question") :test #'string=)
                 (not (string= (box-id dep) (box-id box))))
      (fail "box ~s cannot wait for box ~s" (box-id box) (box-id dep)))
    (when (or (box-removed box) (box-removed dep))
      (fail "box ~s was removed; restore it first" (box-id (if (box-removed box) box dep))))
    (when (member (box-id dep) (box-after box) :test #'string=)
      (fail "box ~s already waits for box ~s" (box-id box) (box-id dep)))
    (setf (box-after box) (append (box-after box) (list (box-id dep))))
    (handler-case (validate-plan (doc-boxes doc))
      (plan-canvas-error (condition)
        (setf (box-after box) (butlast (box-after box)))
        (error condition)))
    (format nil "made ~a wait for ~a" (box-id box) (box-id dep))))

(defun op-unlink (doc op)
  (let* ((box (require-box doc (id-of op "id")))
         (dep (id-of op "after")))
    (unless (member dep (box-after box) :test #'string=)
      (fail "box ~s does not wait for box ~s" (box-id box) dep))
    (setf (box-after box) (remove dep (box-after box) :test #'string=))
    (format nil "~a no longer waits for ~a" (box-id box) dep)))

(defun op-revert (doc op)
  "Ask the model to undo a drift box's change; the cell never writes project files itself."
  (let ((box (require-box doc (id-of op "id"))))
    (unless (string= (box-kind box) "drift") (fail "box ~s is not a drift box" (box-id box)))
    (when (box-removed box) (fail "box ~s was removed; restore it first" (box-id box)))
    (setf (box-revert-asked box) t)
    (format nil "asked to undo ~{~a~^, ~}" (box-files box))))

(defun op-note (doc op)
  "Set the operator's note on a box; empty text clears it."
  (let* ((box (require-box doc (id-of op "id")))
         (note (text-of op "text" "text" :limit 500)))
    (setf (box-note box) note)
    (format nil "~a the note on ~a" (if (plusp (length note)) "set" "cleared") (box-id box))))

(defun op-move (doc op)
  (let ((box (require-box doc (id-of op "id"))))
    (dolist (axis '("x" "y"))
      (let ((value (number-of op axis axis :required t)))
        (unless (<= (abs value) +coordinate-limit+)
          (fail "~a must be a finite number within +-~d" axis +coordinate-limit+))
        (if (string= axis "x") (setf (box-x box) (float value)) (setf (box-y box) (float value)))))
    (format nil "moved ~a" (box-id box))))

(defun apply-op (doc op)
  "Apply one edit to DOC in place. => a short description of the change."
  (unless (hash-table-p op) (fail "each op must be an object"))
  (let ((name (field op "op")))
    (unless (and (stringp name) (member name +op-names+ :test #'string=))
      (fail "unknown op ~s; use one of ~{~a~^, ~}" name +op-names+))
    (cond ((string= name "add") (op-add doc op))
          ((string= name "remove") (op-remove doc op))
          ((string= name "restore") (op-restore doc op))
          ((string= name "rename") (op-rename doc op))
          ((string= name "set_files") (op-set-files doc op))
          ((string= name "set_check") (op-set-check doc op))
          ((string= name "set_after") (op-set-after doc op))
          ((string= name "set_status") (op-set-status doc op))
          ((string= name "answer") (op-answer doc op))
          ((string= name "move") (op-move doc op))
          ((string= name "adopt") (op-adopt doc op))
          ((string= name "note") (op-note doc op))
          ((string= name "link") (op-link doc op))
          ((string= name "unlink") (op-unlink doc op))
          ((string= name "revert") (op-revert doc op))
          (t (fail "unknown op ~s" name)))))

;;; --- a box as the model and the operator read it ------------------------------------

(defun box-view (box)
  "BOX as canvas_get shows it: every field the model needs, and only those that are set."
  (let ((view (nlk:json-object
               "id" (box-id box)
               "label" (box-label box)
               "kind" (box-kind box)
               "status" (box-status box)
               "files" (coerce (box-files box) 'vector)
               "check" (box-check box)
               "after" (coerce (box-after box) 'vector)
               "writes" (box-writes box))))
    (when (box-removed box)
      (setf (gethash "removed" view) t))
    (when (box-check-exit box)
      (setf (gethash "check_exit" view) (box-check-exit box)
            (gethash "check_output_tail" view)
            (let ((output (box-check-output box)))
              (subseq output (max 0 (- (length output) 600))))))
    (when (string= (box-kind box) "question")
      (setf (gethash "options" view) (coerce (box-options box) 'vector)
            (gethash "answer" view) (box-answer box)))
    (when (plusp (length (box-note box))) (setf (gethash "note" view) (box-note box)))
    (when (box-revert-asked box) (setf (gethash "revert_asked" view) t))
    view))

(defun card-line (doc)
  "The line the model puts in its reply so the operator sees the canvas."
  (format nil "::plan-canvas{id=\"~a\"}" (doc-id doc)))

(defun goal-tag (doc box)
  "The tag that links a delegate_task goal to its box."
  (format nil "[plan-canvas:~a/~a]" (doc-id doc) (box-id box)))

(defun delegate-goal (doc box)
  "BOX as a goal a delegate_task can be handed."
  (let ((parts (list (format nil "~a ~a." (goal-tag doc box) (box-label box))
                     (format nil "Project root: ~a." (doc-root doc)))))
    (when (box-files box)
      (push (format nil "Write only: ~{~a~^, ~}." (box-files box)) parts))
    (when (plusp (length (box-check box)))
      (push (format nil "Done when this passes: ~a" (box-check box)) parts))
    (push "Write files with the eval tool's write-file or edit, not through a shell, so the canvas sees each write." parts)
    (when (plusp (length (box-note box)))
      (push (format nil "Operator note: ~a" (box-note box)) parts))
    (format nil "~{~a~^ ~}" (nreverse parts))))

;;; --- the session a call belongs to ---------------------------------------------------

(defun session-of ()
  "The session the turn this call runs in belongs to, or NIL."
  (or (getf (nle:turn) :session-id) nlk:*scribe-session-id*))

(defun call-session ()
  "SESSION-OF, or a refusal saying no chat is in hand."
  (or (session-of) (fail "no chat to keep a canvas for")))

(defun default-root (session)
  "The folder a plan is drawn against when the call names none."
  (or (ignore-errors (nlk:find-session-cwd session))
      (namestring (uiop:getcwd))))

(defun root-of (arguments session)
  "The absolute project root a drawn plan's globs are relative to."
  (let ((given (text-of arguments "root" "root" :limit 1000)))
    (let ((root (if (plusp (length given)) (expand-tilde given) (or (default-root session) ""))))
      (unless (absolute-path-p root) (fail "root must be an absolute path"))
      (string-right-trim "/" root))))

;;; --- the four tools --------------------------------------------------------------------

(defun respond (thunk)
  "THUNK's object as JSON; a refusal comes back as {"error": ...} so the model can fix it."
  (handler-case (nlk:encode-json-object (funcall thunk))
    (plan-canvas-error (condition)
      (nlk:encode-json-object (nlk:json-object "error" (plan-canvas-error-detail condition))))
    (error (condition)
      (nlk:encode-json-object
       (nlk:json-object "error" (format nil "~a: ~a" (type-of condition) condition))))))

(defun ops-of (arguments)
  "The ops list a canvas_update call carries."
  (let ((value (field arguments "ops")))
    (let ((ops (cond ((vectorp value) (coerce value 'list))
                     ((hash-table-p value) (list value))
                     (t nil))))
      (unless ops (fail "ops must be a non-empty list"))
      ops)))

(defun entries-of (arguments key label)
  "KEY's value on ARGUMENTS as a list of objects."
  (let ((value (field arguments key)))
    (let ((entries (cond ((vectorp value) (coerce value 'list))
                         ((hash-table-p value) (list value))
                         (t nil))))
      (unless entries (fail "~a must be a non-empty list" label))
      entries)))

(defun tool-draw (arguments)
  "canvas_draw: draw the plan, keeping the state of every box that keeps its id."
  (respond
   (lambda ()
     (running-settings)
     (let* ((session (call-session))
            (title (text-of arguments "title" "title" :limit 200 :required t))
            (root (root-of arguments session))
            (boxes (mapcar #'box-from-plan (entries-of arguments "boxes" "boxes")))
            (old (load-doc session))
            (kept (remove-if-not (lambda (box) (string= (box-kind box) "drift"))
                                  (and old (doc-boxes old)))))
       (validate-plan boxes)
       (dolist (box boxes)
         (let ((previous (and old (doc-box old (box-id box)))))
           (when previous
             (setf (box-status box) (box-status previous)
                   (box-writes box) (box-writes previous)
                   (box-written box) (box-written previous)
                   (box-check-exit box) (box-check-exit previous)
                   (box-check-output box) (box-check-output previous)
                   (box-answer box) (box-answer previous)
                   (box-removed box) (box-removed previous)
                   (box-revert-asked box) (box-revert-asked previous)
                   (box-note box) (box-note previous)
                   (box-x box) (box-x previous)
                   (box-y box) (box-y previous)))))
       (let ((doc (or old (make-doc :id (canvas-id session) :session session
                                     :created (get-universal-time)))))
         (setf (doc-title doc) title
               (doc-root doc) root
               (doc-boxes doc) (append boxes kept)
               (doc-revision doc) (1+ (doc-revision doc)))
         (save-doc doc)
         (nlk:json-object
          "canvas_id" (doc-id doc)
          "revision" (doc-revision doc)
          "card" (card-line doc)
          "boxes" (coerce (mapcar #'box-json (active-boxes doc)) 'vector)
          "ready" (coerce (mapcar #'box-id (ready-boxes doc)) 'vector)))))))

(defun tool-update (arguments)
  "canvas_update: change the plan with ops applied together, all or nothing."
  (respond
   (lambda ()
     (running-settings)
     (let* ((session (call-session))
            (ops (ops-of arguments)))
       (dolist (op ops)
         (when (member (field op "op") +user-only-ops+ :test #'string=)
           (fail "op ~s is for the operator only; the canvas shows it as their words" (field op "op"))))
       (let ((doc (doc-of session)))
         (let ((reasons (mapcar (lambda (op) (apply-op doc op)) ops)))
           (setf (doc-revision doc) (1+ (doc-revision doc)))
           (save-doc doc)
           (nlk:json-object
            "canvas_id" (doc-id doc)
            "revision" (doc-revision doc)
            "applied" (length ops)
            "changes" (coerce reasons 'vector))))))))

(defun tool-get (arguments)
  "canvas_get: the whole canvas, and the operator's edits the model has not read."
  (declare (ignore arguments))
  (respond
   (lambda ()
     (running-settings)
     (let* ((session (call-session))
            (doc (doc-of session))
            (edits (doc-edits doc)))
       (when edits
         (setf (doc-edits doc) nil
               (doc-revision doc) (1+ (doc-revision doc)))
         (save-doc doc))
       (nlk:json-object
        "canvas_id" (doc-id doc)
        "title" (doc-title doc)
        "root" (doc-root doc)
        "revision" (doc-revision doc)
        "card" (card-line doc)
        "boxes" (coerce (mapcar #'box-view (doc-boxes doc)) 'vector)
        "user_edits" (coerce edits 'vector))))))

(defun tool-next (arguments)
  "canvas_next: the boxes that can start now, each with a ready delegate_task goal."
  (declare (ignore arguments))
  (respond
   (lambda ()
     (running-settings)
     (let ((doc (doc-of (call-session))))
       (nlk:json-object
        "canvas_id" (doc-id doc)
        "ready" (coerce (loop for box in (ready-boxes doc)
                               collect (nlk:json-object "id" (box-id box)
                                                         "label" (box-label box)
                                                         "goal" (delegate-goal doc box)))
                         'vector)
        "open_questions" (coerce (loop for box in (active-boxes doc)
                                        when (and (string= (box-kind box) "question")
                                                  (zerop (length (box-answer box))))
                                          collect (box-id box))
                                  'vector))))))

;;; --- the paths a form writes -------------------------------------------------------

(defun quotation-at (form from)
  "The string literal that opens at or after FROM in FORM, or NIL."
  (let ((open (position #\" form :start from)))
    (when open
      (let ((end (loop for index from (1+ open) below (length form)
                        when (and (char= #\" (char form index))
                                  (not (and (plusp index) (char= #\\ (char form (1- index))))))
                          return index)))
        (and end (subseq form (1+ open) end))))))

(defun written-paths (form)
  "The literal paths FORM names to write-file or edit, in the order they appear.

A path that is not a literal cannot be known before the call runs; the workspace
diff the result carries is what marks its box, so only the refusal needs this."
  (when (stringp form)
    (let ((paths '()))
      (dolist (verb '("(write-file" "(edit"))
        (let ((from 0))
          (loop for found = (search verb form :start2 from)
                while found
                do (let ((path (quotation-at form (+ found (length verb)))))
                     (when path (pushnew path paths :test #'string=)))
                   (setf from (+ found (length verb))))))
      (nreverse paths))))

(defun payload-metadata (payload)
  "PAYLOAD's own metadata object, or an empty one."
  (or (nlk:json-value payload :object "metadata") (make-hash-table :test #'equal)))

(defun path-of (entry)
  "One workspace-diff entry's path."
  (cond ((stringp entry) entry)
        ((hash-table-p entry) (or (nlk:json-value entry :text "path")
                                   (nlk:json-value entry :text "file")))
        (t nil)))

(defun changed-paths (payload)
  "The files PAYLOAD, one tool result, changed: its workspace diff's files, and the
source of each write-file or edit receipt it carries."
  (let* ((metadata (payload-metadata payload))
         (files (nlk:json-value metadata :array "workspace_diff" "files"))
         (calls (nlk:json-value metadata :array "calls"))
         (paths (loop for entry across (if (vectorp files) files #())
                      for path = (path-of entry)
                      when path collect path)))
    (loop for stamp across (if (vectorp calls) calls #())
          for verb = (nlk:json-value stamp :string "verb")
          for source = (nlk:json-value stamp :string "source")
          when (and verb source (member verb +write-verbs+ :test #'string=)
                    (plusp (length source)))
            do (pushnew source paths :test #'string=))
    (nreverse paths)))

(defun text-exit (text)
  "The exit status the (sh ...) verb wrote at the end of TEXT, or NIL."
  (when (stringp text)
    (let ((at (search "[exit " text)))
      (when at
        (let ((end (position #\] text :start at)))
          (when end (ignore-errors (parse-integer text :start (+ at 6) :end end :junk-allowed t))))))))

(defun commands-run (payload)
  "The shell commands this result ran, as (COMMAND . EXIT) pairs."
  (let* ((metadata (payload-metadata payload))
         (calls (nlk:json-value metadata :array "calls"))
         (text (nlk:json-value payload :string "text"))
         (out '()))
    (loop for stamp across (if (vectorp calls) calls #())
          for verb = (nlk:json-value stamp :string "verb")
          for source = (nlk:json-value stamp :string "source")
          when (and (stringp verb) (stringp source)
                    (member verb '("sh" "eval") :test #'string=))
            do (let ((exit (or (nlk:json-value stamp :integer "exit")
                               (nlk:json-value stamp :integer "shell_exit")
                               (nlk:json-value stamp :integer "status")
                               (text-exit text))))
                 (when exit (push (cons source exit) out))))
    (nreverse out)))

;;; --- who owns a written path -----------------------------------------------------

(defun owned-by (doc relative)
  "DOC's active planned boxes whose globs cover RELATIVE."
  (remove-if-not (lambda (box)
                   (some (lambda (glob) (glob-match-p glob relative)) (box-files box)))
                 (component-boxes doc)))

(defun removed-owner (doc relative)
  "The removed box that owns RELATIVE, when no active box does."
  (when (null (owned-by doc relative))
    (loop for box in (doc-boxes doc)
          when (and (box-removed box)
                    (string= (box-kind box) "component")
                    (some (lambda (glob) (glob-match-p glob relative)) (box-files box)))
            return (box-id box))))

(defun drift-box (doc relative)
  "A drift box for RELATIVE: a written file no box planned."
  (setf (doc-drift doc) (1+ (doc-drift doc)))
  (make-box :id (format nil "drift-~d" (doc-drift doc))
            :label (let ((label (format nil "wrote ~a" relative)))
                     (subseq label 0 (min (length label) 200)))
            :kind "drift"
            :files (list relative)
            :status "done"))

(defun note-write (doc path)
  "Mark, for one written file, the box that owns it -- or record it as drift."
  (let ((relative (relativize path (doc-root doc))))
    (when relative
      (let ((owners (owned-by doc relative)))
        (if owners
            (dolist (box owners)
              (incf (box-writes box))
              (pushnew relative (box-written box) :test #'string=)
              (unless (string= (box-status box) "done")
                (setf (box-status box) "doing")))
            (unless (loop for box in (doc-boxes doc)
                          thereis (and (string= (box-kind box) "drift")
                                       (member relative (box-files box) :test #'string=)))
              (setf (doc-boxes doc) (append (doc-boxes doc) (list (drift-box doc relative))))))))
    relative))

(defun note-command (doc command exit payload)
  "Set the box whose check is exactly COMMAND from its EXIT."
  (let ((normalized (check-command command))
        (text (nlk:json-value payload :string "text"))
        (ran nil))
    (dolist (box (active-boxes doc))
      (when (and (plusp (length (box-check box)))
                 (string= normalized (check-command (box-check box))))
        (setf (box-check-exit box) exit
              (box-status box) (if (zerop exit) "done" "failed")
              (box-check-output box)
              (if (stringp text) (subseq text (max 0 (- (length text) 600))) ""))
        (setf ran t)))
    ran))

(defun note-work (session payload)
  "Fold the work one tool result reports into SESSION's canvas."
  (let ((doc (load-doc session)))
    (when doc
      (let ((changed nil))
        (dolist (path (changed-paths payload))
          (when (note-write doc path) (setf changed t)))
        (dolist (pair (commands-run payload))
          (when (note-command doc (car pair) (cdr pair) payload) (setf changed t)))
        (when changed
          (setf (doc-revision doc) (1+ (doc-revision doc)))
          (save-doc doc))))))

(defun observe-frame (op)
  "The :FRAME observer: a tool's result marks the boxes its work touched."
  (multiple-value-bind (kind payload metadata turn-id) (nlk:frame-fact op)
    (declare (ignore metadata turn-id))
    (when (and kind (string= kind "turn.tool_result"))
      (let ((session (getf op :session-id)))
        (when (and (stringp session) (plusp (length session)))
          (with-plan-canvas-lock
            (ignore-errors (note-work session payload))))))))

(defun write-refusal (op)
  "The text that refuses OP when it writes a removed box's file, or NIL."
  (let ((name (getf op :name))
        (arguments (getf op :arguments)))
    (when (and (stringp name) (string= name "eval"))
      (let* ((session (session-of))
             (form (nlk:json-value arguments :string "form"))
             (doc (and session form (ignore-errors (load-doc session)))))
        (when doc
          (loop for path in (written-paths form)
                for relative = (relativize path (doc-root doc))
                for box = (and relative (removed-owner doc relative))
                when box
                  return (format nil "ERROR: ~a belongs to box ~s, which the operator removed from the plan. ~
Leave it alone, or restore the box with canvas_update's restore op first." relative box)))))))

(defun veto-write (op next)
  "The :TOOL hook: refuse a write into a removed box's files before the call runs.

Everything it reads is the call's own arguments, as the original's pre_tool_call
did: no store, no disk, so a slow canvas can never hold a turn."
  (let ((refusal (handler-case (write-refusal op) (error () nil))))
    (if refusal (values refusal nil) (funcall next op))))

;;; --- the round's reminder ------------------------------------------------------------

(defun reminder-text (doc)
  "The operator's unseen edits of DOC as one reminder for the round about to be asked."
  (format nil "[plan-canvas ~a] the operator edited your plan: ~{~a~^ | ~} ~
Call canvas_get to read the whole canvas and treat these as instructions."
          (doc-id doc)
          (loop for edit in (doc-edits doc)
                collect (subseq edit 0 (min (length edit) +edit-limit+)))))

(defun deliver (op)
  "The :ROUND observer: the operator's edits, as one reminder at a model request."
  (let* ((session (or (getf op :session-id) (session-of)))
         (turn (getf op :turn))
         (doc (and session turn (ignore-errors (load-doc session)))))
    (when (and doc (doc-edits doc))
      (nlk:record-turn-context-prompt turn :text (reminder-text doc)))))

;;; --- /plan-canvas --------------------------------------------------------------------

(defparameter +primer+
  "plan-canvas: the plan the agent draws, and the work itself keeps

The agent draws its plan as boxes with canvas_draw: each box names the files
it writes and the command that proves it. A box's status then comes from the
work, not from the agent -- a write into a box's files makes it doing, and the
exact check command's exit code makes it done or failed. A file written that
no box planned shows up as a drift box here.

/plan-canvas                  the plan: every box, its status, its check
/plan-canvas next             the boxes that can start now
/plan-canvas note ID TEXT     leave the agent a note on a box
/plan-canvas answer ID TEXT   answer a box that asked the operator a question
/plan-canvas adopt ID         keep a drift file as planned work
/plan-canvas remove ID        remove a box; a write into its files is refused
/plan-canvas restore ID       put a removed box back
/plan-canvas drop             delete this chat's canvas

A note, an answer, an adopt and a removal reach the agent as your own words:
it reads them in canvas_get, and hears them at its next request."
  "What /plan-canvas with no argument prints, and the manual's page.")

(defparameter +command-words+ '("next" "note" "answer" "adopt" "remove" "restore" "drop")
  "The words /plan-canvas takes after its name.")

(defun box-line (box)
  "One box as /plan-canvas shows it."
  (format nil "  [~a] ~a (~a)~@[  check: ~a~]~@[  files: ~a~]~@[  note: ~a~]~@[  -- removed~]"
          (box-status box)
          (box-label box)
          (box-id box)
          (and (plusp (length (box-check box))) (box-check box))
          (and (box-files box) (format nil "~{~a~^, ~}" (box-files box)))
          (and (plusp (length (box-note box))) (box-note box))
          (box-removed box)))

(defun plan-text (doc)
  "DOC as /plan-canvas with no argument prints it."
  (if (null (doc-boxes doc))
      (format nil "~a: no boxes yet" (doc-title doc))
      (format nil "~a  (revision ~d, root ~a)~%~{~a~^~%~}"
              (doc-title doc) (doc-revision doc) (doc-root doc)
              (mapcar #'box-line (doc-boxes doc)))))

(defun edit-words (verb box detail)
  "One operator edit, as the model reads it."
  (cond ((string= verb "remove")
         (format nil "Removed box ~s (~a) from the plan." (box-label box) (box-id box)))
        ((string= verb "restore")
         (format nil "Restored box ~s (~a)." (box-label box) (box-id box)))
        ((string= verb "note")
         (format nil "Left a note on box ~s: ~s. Follow it when you build this box."
                 (box-label box) detail))
        ((string= verb "answer")
         (format nil "Answered box ~s: ~s." (box-label box) detail))
        ((string= verb "adopt")
         (format nil "Adopted box ~s (~a): the file it wrote is planned work after all."
                 (box-label box) (box-id box)))
        (t (format nil "Edited box ~s." (box-label box)))))

(defun edit-and-keep (session verb id detail)
  "Run VERB on box ID as the operator, keep the edit for the model, answer what happened."
  (let ((doc (doc-of session)))
    (let ((op (nlk:json-object "op" verb "id" id)))
      (when detail
        (setf (gethash "text" op) detail)
        (setf (gethash "answer" op) detail))
      (let ((box (require-box doc id)))
        (apply-op doc op)
        (let ((words (edit-words verb box detail)))
          (note-edit doc words)
          (save-doc doc)
          (format nil "~a~%~%~a" words (plan-text doc)))))))

(defun run-command (args session-id)
  "/plan-canvas: the plan the agent drew, and the operator's own edits of it."
  (running-settings)
  (let* ((session (or session-id (call-session)))
         (text (trim (or args "")))
         (space (position #\Space text))
         (word (if space (subseq text 0 space) text))
         (rest (if space (trim (subseq text (1+ space))) "")))
    (let ((doc (load-doc session)))
      (when (null doc)
        (return-from run-command
          (values (if (string= word "drop")
                      "no canvas to drop"
                      (format nil "~a~%No canvas in this chat yet: ask the agent to draw one with canvas_draw." +primer+))
                  nil)))
      (cond ((member word '("" "boxes") :test #'string=)
             (values (format nil "~a~%~%More: /plan-canvas ~{~a~^ | ~}" (plan-text doc) +command-words+) nil))
            ((string= word "next")
             (let ((ready (ready-boxes doc)))
               (values (if ready
                           (format nil "Ready now:~%~{~a~^~%~}" (mapcar #'box-line ready))
                           "Nothing is ready: draw more boxes, or the rest wait on work not done yet.")
                       nil)))
            ((member word '("remove" "restore" "adopt") :test #'string=)
             (let ((id (trim rest)))
               (when (zerop (length id)) (fail "/plan-canvas ~a needs a box id" word))
               (values (edit-and-keep session word id nil) nil)))
            ((member word '("note" "answer") :test #'string=)
             (let* ((split (position #\Space rest))
                    (id (if split (subseq rest 0 split) rest))
                    (detail (if split (trim (subseq rest (1+ split))) "")))
               (when (or (zerop (length id)) (zerop (length detail)))
                 (fail "/plan-canvas ~a needs a box id and text" word))
               (values (edit-and-keep session word id (one-line detail)) nil)))
            ((string= word "drop")
             (let ((path (doc-file session)))
               (when (probe-file path) (ignore-errors (delete-file path)))
               (values "dropped this chat's canvas" nil)))
            (t (fail "/plan-canvas does not know ~s; try ~{~a~^, ~}" word +command-words+))))))

;;; --- what the tools are shown ------------------------------------------------------

(defparameter *box-schema*
  (nlk:json-object
   "type" "object"
   "properties" (nlk:json-object
                 "id" (nlk:json-object "type" "string"
                                        "description" "Short stable id, e.g. 'api'. Letters, digits, _ . -")
                 "label" (nlk:json-object "type" "string"
                                           "description" "What this box delivers, in a few words.")
                 "files" (nlk:json-object "type" "array"
                                           "items" (nlk:json-object "type" "string")
                                           "description" "Globs relative to root for the files this box writes, e.g. 'src/payments/**'.")
                 "check" (nlk:json-object "type" "string"
                                           "description" "Shell command that proves the box works, e.g. 'npm test -- payments'. Run exactly this command with (sh \"...\") inside eval; its exit code sets the box status.")
                 "after" (nlk:json-object "type" "array"
                                           "items" (nlk:json-object "type" "string")
                                           "description" "Ids of boxes to finish first.")
                 "options" (nlk:json-object "type" "array"
                                             "items" (nlk:json-object "type" "string")
                                             "description" "Only for a decision the operator must make: the choices. The box becomes a question."))
   "required" (vector "id" "label"))
  "One plan box, as canvas_draw is shown it.")

(defparameter *draw-schema*
  (list :description
        "Draw your plan for a multi-step coding task on the canvas, before you write code. The operator can remove, annotate or answer boxes with /plan-canvas. Bind every box to the files it writes and to a check command: the hooks then set a box's status from your real writes and the check's exit code. Calling it again replaces the plan and keeps the state of the boxes that keep their id. After drawing, put the returned card line on its own line in your reply."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                       "title" (nlk:json-object "type" "string" "description" "Short plan title.")
                       "root" (nlk:json-object "type" "string"
                                                "description" "Absolute path of the project root. Globs are relative to it; empty uses this chat's folder.")
                       "boxes" (nlk:json-object "type" "array"
                                                 "items" *box-schema*
                                                 "description" "Plan boxes in build order."))
         "required" (vector "title" "boxes")))
  "canvas_draw's schema.")

(defparameter *update-schema*
  (list :description
        "Change the canvas plan with a list of ops, applied together (all or nothing). Ops: add {box}, remove {id}, restore {id}, rename {id, label}, set_files {id, files}, set_check {id, check}, set_after {id, after}, link {id, after} (box id also waits for box after), unlink {id, after}, set_status {id, status} (only when no write or check can show it; never done or failed on a box with a check), answer {id, answer}, move {id, x, y}, adopt {id, label?} (make a drift box planned work that owns its file)."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                       "ops" (nlk:json-object "type" "array"
                                               "items" (nlk:json-object
                                                        "type" "object"
                                                        "properties" (nlk:json-object
                                                                      "op" (nlk:json-object "type" "string"
                                                                                             "enum" (coerce (remove-if (lambda (name) (member name +user-only-ops+ :test #'string=)) +op-names+) 'vector)))
                                                        "required" (vector "op"))
                                               "description" "The edits, in order."))
         "required" (vector "ops")))
  "canvas_update's schema.")

(defparameter *get-schema*
  (list :description
        "Read the current canvas: every box with its status, files, check result, and the operator's edits of the plan that you have not seen yet. Treat those edits as instructions from the operator."
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)))
  "canvas_get's schema.")

(defparameter *next-schema*
  (list :description
        "List the boxes that can start now (every box they wait for is done), each with a ready delegate_task goal. Keep the [plan-canvas:...] tag at the start of the goal: it links the subagent to its box."
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)))
  "canvas_next's schema.")

;;; --- the cell -------------------------------------------------------------------

(defun install ()
  "Make the folder canvases are kept in, once the cell is running."
  (ensure-directories-exist (folder))
  t)

(nle:define-cell plan-canvas
  (:section ("plan-canvas")
    (:guide "nothing has to be set: canvases live under the home; folder keeps them elsewhere, keep_edits caps the operator edits one canvas holds")
    ("folder" :path
     :doc "the folder canvas files go in; empty keeps them under the home")
    ("keep_edits" :integer :default 40 :min 1
     :doc "the operator edits a canvas keeps before the oldest go"))
  (:help :plan-canvas
   "plan-canvas: the plan the agent draws, whose status the work itself sets - /plan-canvas"
   +primer+)
  (:command "plan-canvas" 'run-command
            :catches 'plan-canvas-error
            :description "The plan the agent drew: its boxes, and the operator's own edits"
            :argument-hint "[next|note ID TEXT|answer ID TEXT|adopt ID|remove ID|restore ID|drop]")
  (:hook :frame (nle:observer #'observe-frame "nodecode-plan-canvas"))
  (:hook :tool #'veto-write)
  (:hook :round (nle:observer #'deliver "nodecode-plan-canvas"))
  (:tool "canvas_draw" *draw-schema* 'tool-draw)
  (:tool "canvas_update" *update-schema* 'tool-update)
  (:tool "canvas_get" *get-schema* 'tool-get)
  (:tool "canvas_next" *next-schema* 'tool-next)
  (:start #'install))
