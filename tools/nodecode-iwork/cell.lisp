;;;; cell.lisp --- Numbers cells, Keynote slides and Pages text, edited
;;;; through the apps, with a backup, a read-back and an undo.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every write goes the same way: the fence (the file must live in a folder
;;;; the operator gave the cell) -> a byte-exact backup -> the app's own edit,
;;;; through osascript -> the file read back and compared with what was asked
;;;; -> a receipt. A write the app did not keep is taken back from the backup
;;;; before the error is answered, so a refusal leaves the file as it was.
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :iwork) answers the primer below, and every request's help section
;;;; carries one line naming it.
;;;;
;;;; Rewritten from Arkanji's Hermes Agent plugin iwork-studio; see NOTICE.
;;;; The scripts the apps run live in jxa.lisp, which depends on nothing but
;;;; the osascript macOS ships.

(in-package #:nodecode-iwork)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Apple Numbers, Keynote and Pages files are available through the nodecode-iwork cell:
five tools, which a person can also call from eval in the iwork package. A file must
live in a folder the cell was given (its folders setting); anything else is refused
before it is touched, and every write leaves a backup and a receipt behind.
  iwork_read      {path, sheet, table, range, max_rows}
      What a document says. A .numbers file: its sheet and table names and the cells
      of one range (A1:J20 unless range names another). A .key deck: every slide
      with its title and body. A .pages document: its body text. Read before you
      write, so the sheet, table and cells you name are the ones the file holds.
  iwork_set_cell  {path, sheet, table, ref, value, dry_run}
      Set one Numbers cell (ref like \"B2\"). The app makes the edit and the cell is
      read back; when the app did not keep what you wrote, the file is put back and
      the tool says so. dry_run answers what would be written and touches nothing.
  iwork_slides    {path, action, slide, title, body}
      Keynote slides: list, add (a new slide, at the end), set_text (a slide's title
      and body), delete (never the last slide). slide numbers count from 1.
  iwork_export    {path, format, output, overwrite}
      Export to PDF, and to xlsx from Numbers, pptx from Keynote, docx from Pages. A
      new file is never overwritten unless overwrite says so.
  iwork_backups   {path, action, id}
      Every write, newest last, each with its file and backup; action=restore puts
      that file back from its backup.
The apps are driven through osascript, so a Mac with Numbers, Keynote or Pages
installed is needed; reading and writing happen in the real app, which opens the
document and closes it again without saving when it was not already open."
  "What (help :iwork) answers while the cell runs.")

;;; --- answers, and the one condition ---------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (iwork-error (condition)
       (nle:failure (format nil "ERROR: ~a" (iwork-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: iwork: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defvar *running* nil
  "Whether the cell is started: its verbs refuse until it is.")

(defun require-running ()
  "Refuse a verb called with nothing started."
  (unless *running*
    (fail "iwork is not running: start the cell, whose iwork section names the folders it may touch")))

;;; --- the tools' arguments ----------------------------------------------------------

(nlk:define-contract iwork-read-request
  (:doc "Read an Apple Numbers (.numbers), Keynote (.key) or Pages (.pages) file: a table's cells, a deck's slides with their titles and bodies, or a document's body text. Read before writing, so the sheet, table and cells you name are the ones the file holds. The file must be inside a folder the cell was given.")
  ("path" :text :required t :doc "the file to read, absolute or ~/...")
  ("sheet" :string :default "" :doc "the Numbers sheet's name; the first sheet when empty")
  ("table" :string :default "" :doc "the table's name on that sheet; the first table when empty")
  ("range" :string :default "" :doc "the cells to read, like A1 or A1:D9; A1:J20 when empty")
  ("max_rows" :integer :default 20 :doc "the most rows to answer"))

(nlk:define-contract iwork-set-cell-request
  (:doc "Set one cell of a Numbers table, in the app itself. The cell is read back afterwards; when the app did not keep what was written the file is put back from its backup and the tool says so. The file must be in a folder the cell was given, and a backup and a receipt are left behind.")
  ("path" :text :required t :doc "the .numbers file to edit")
  ("sheet" :string :default "" :doc "the sheet's name; the first sheet when empty")
  ("table" :string :default "" :doc "the table's name; the first table when empty")
  ("ref" :text :required t :doc "one cell, like B2")
  ("value" :any :required t :doc "what to put in it: a number, or text")
  ("dry_run" :boolean :default nil :doc "true answers what would be written and touches nothing"))

(nlk:define-contract iwork-slides-request
  (:doc "Keynote slides: list them with their titles and bodies, add a slide at the end, set one slide's title and body, or delete one (never the last). Slide numbers count from 1. The deck must be in a folder the cell was given; a write leaves a backup and a receipt, and iwork_backups puts a deck back.")
  ("path" :text :required t :doc "the .key deck")
  ("action" :string :default "list" :options '("list" "add" "set_text" "delete")
   :doc "list, add a slide, set_text on one slide, or delete one")
  ("slide" :integer :doc "which slide, 1-based; the first when omitted")
  ("title" :string :default "" :doc "the slide's title, for add and set_text")
  ("body" :string :default "" :doc "the slide's body text, for add and set_text"))

(nlk:define-contract iwork-export-request
  (:doc "Export a Numbers, Keynote or Pages file through its app: to PDF, and to xlsx from Numbers, pptx from Keynote, docx from Pages. The source must be in a folder the cell was given; the output too. A new file is never overwritten unless overwrite says so.")
  ("path" :text :required t :doc "the file to export")
  ("format" :string :slot export-format :default "pdf" :options '("pdf" "xlsx" "pptx" "docx")
   :doc "pdf; or the Office format the app itself writes")
  ("output" :string :default "" :doc "where to write it; beside the source with the new extension when empty")
  ("overwrite" :boolean :default nil :doc "true replaces an output that is already there"))

(nlk:define-contract iwork-backups-request
  (:doc "List the writes this cell made, newest last, each with its file, digest and backup; or restore one, putting its file back from the backup taken before that write.")
  ("path" :string :default "" :doc "only the receipts of this file, when set")
  ("action" :string :default "list" :options '("list" "restore") :doc "list the receipts, or restore one")
  ("id" :integer :doc "the receipt to restore, as list answers it"))

;;; --- the fence: which folders may be read and written -------------------------------

(defun expand-path (text)
  "TEXT as an absolute pathname, ~ resolved."
  (let ((text (string-trim " " (princ-to-string text))))
    (when (zerop (length text)) (fail "a file path is required"))
    (if (char= #\~ (char text 0))
        (merge-pathnames (subseq text (min 2 (length text))) (user-homedir-pathname))
        (uiop:ensure-pathname text :want-absolute t))))

(defun fence-folders ()
  (mapcar (lambda (entry) (princ-to-string entry)) (setting :folders)))

(defun desk-file (path)
  "PATH's namestring when it sits inside a folder the cell was given, else a refusal."
  (let* ((pathname (expand-path path))
         (probe (or (uiop:truename* pathname)
                    (uiop:truename* (uiop:pathname-directory-pathname pathname))
                    (uiop:pathname-directory-pathname pathname)))
         (folders (fence-folders)))
    (unless folders
      (fail "no folder is set for this cell: add one (folders: [\"~~/Documents\"] in ~/.nodecode/config.jsonc, or Settings) before touching ~a"
            (namestring pathname)))
    (or (loop for folder in folders
              for root = (uiop:truename* (expand-path folder))
              when (and root (uiop:subpathp probe root))
                return (namestring pathname))
        (fail "~a is not inside a folder this cell may touch (~{~a~^, ~}), and it reads and writes only there"
              (namestring pathname) folders))))

(defun document-kind (path)
  "PATH's kind, from its extension: :numbers, :keynote or :pages."
  (let ((type (string-downcase (or (pathname-type path) ""))))
    (cond ((equal type "numbers") :numbers)
          ((member type '("key" "keynote") :test #'equal) :keynote)
          ((equal type "pages") :pages)
          (t (fail "~a is not an iWork file: this cell reads and writes .numbers, .key and .pages"
                   (file-namestring path))))))

;;; --- octets, digests, backups, receipts ---------------------------------------------

(defun read-octets (path)
  (when (probe-file path)
    (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
      (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
        (read-sequence bytes in)
        bytes))))

(defun write-octets (path octets)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
    (write-sequence octets out))
  (namestring path))

(defun digest-of (path)
  (let ((octets (read-octets path)))
    (and octets (nlk:sha256-text octets))))

(defun data-file (relative)
  (namestring (nlk:home (concatenate 'string "iwork/" relative))))

(defun read-receipts ()
  (let ((path (data-file "receipts.json")))
    (if (probe-file path)
        (or (ignore-errors (nlk:json-value (nlk:decode-json (uiop:read-file-string path))
                                           :array "receipts"))
            #())
        #())))

(defun write-receipts (receipts)
  (let ((path (data-file "receipts.json")))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
      (write-string (nlk:encode-json-object
                     (nlk:json-object "receipts" (coerce receipts 'vector)))
                    stream))))

(defun next-receipt-id (receipts)
  (1+ (loop for receipt in receipts
            maximize (or (nlk:json-value receipt :integer "id") 0))))

(defun backup-file (path id)
  "PATH copied where no other write reuses it; answers the copy's namestring."
  (let ((target (data-file (format nil "backups/~d/~a" id (file-namestring path)))))
    (write-octets target (or (read-octets path)
                             (make-array 0 :element-type '(unsigned-byte 8))))
    (namestring (or (uiop:truename* target) target))))

(defun record-receipt (tool path before after backup summary)
  "One receipt for one finished write, appended to the receipt file."
  (let* ((receipts (coerce (read-receipts) 'list))
         (id (next-receipt-id receipts))
         (receipt (nlk:json-object "id" id
                                    "tool" tool
                                    "path" (namestring path)
                                    "before" (or before "")
                                    "after" (or after "")
                                    "backup" (or backup "")
                                    "change" (or summary ""))))
    (write-receipts (append receipts (list receipt)))
    (prune-backups (namestring path))
    receipt))

(defun prune-backups (path)
  "Delete this cell's backups of PATH beyond the newest (setting :backups), so
the folder does not grow without bound. Only files under the cell's own data
folder are ever deleted."
  (let* ((rows (remove-if-not (lambda (row)
                                 (equal path (nlk:json-value row :string "path")))
                               (coerce (read-receipts) 'list)))
         (keep (max 1 (or (ignore-errors (setting :backups)) 10)))
         (old (butlast rows keep))
         (root (data-file "backups/")))
    (dolist (row old)
      (let ((backup (nlk:json-value row :string "backup")))
        (when (and backup (plusp (length backup))
                   (uiop:subpathp (uiop:ensure-pathname backup :want-absolute t)
                                  (uiop:ensure-pathname root :want-absolute t))
                   (probe-file backup))
          (ignore-errors (delete-file backup)))))))

(defun receipt-of (id)
  (or (find id (coerce (read-receipts) 'list)
            :key (lambda (receipt) (nlk:json-value receipt :integer "id")))
      (fail "receipt ~a not found: iwork_backups action=list answers them" id)))

(defun restore-from (backup path)
  "Put BACKUP's bytes back over PATH."
  (let ((octets (read-octets backup)))
    (unless octets (fail "the backup ~a is unreadable" backup))
    (write-octets path octets)))

(defun guarded-write (tool path summary thunk)
  "Run THUNK, which edits PATH in its app: back PATH up first, read it back
after, and leave a receipt; a THUNK that signals, or a file that did not keep
the change, puts the backup back before the error goes up."
  (let ((before (digest-of path))
        (backup (and (probe-file path) (backup-file path (next-receipt-id (coerce (read-receipts) 'list))))))
    (handler-case
        (let ((value (funcall thunk)))
          (record-receipt tool path before (digest-of path) backup summary)
          value)
      (error (condition)
        (when (and backup before (not (equal (digest-of path) before)))
          (ignore-errors (restore-from backup path)))
        (error condition)))))

;;; --- iwork_read ---------------------------------------------------------------------

(defun request-field (call reader &optional (fallback ""))
  (or (funcall reader call) fallback))

(defun read-file (call)
  "The iwork_read tool: what a document says."
  (answering
    (require-running)
    (let* ((path (desk-file (iwork-read-request-path call)))
           (kind (document-kind path))
           (answer
             (ecase kind
               (:numbers
                (run-op :read-numbers
                        :path (namestring path)
                        :sheet (request-field call #'iwork-read-request-sheet)
                        :table (request-field call #'iwork-read-request-table)
                        :range (request-field call #'iwork-read-request-range)
                        :max_rows (or (iwork-read-request-max-rows call) 20)))
               (:keynote (run-op :read-keynote :path (namestring path)))
               (:pages (run-op :read-pages :path (namestring path))))))
      (nlk:encode-json-object answer))))

;;; --- iwork_set_cell ------------------------------------------------------------------

(defun first-cell (answer)
  "The top-left cell of a read-numbers answer, as text."
  (let* ((rows (nlk:json-value answer :array "rows"))
         (row (and rows (plusp (length rows)) (aref rows 0))))
    (if (and row (plusp (length row))) (princ-to-string (aref row 0)) "")))

(defun same-value-p (wrote value)
  "Whether the app's WROTE is VALUE: text exactly, a number by its digits."
  (cond ((stringp value) (string= (string-trim " " wrote) (string-trim " " value)))
        ((numberp value)
         (or (string= (string-trim " " wrote) (princ-to-string value))
             (let ((read (ignore-errors (read-from-string (string-trim " " wrote)))))
               (and (numberp read) (= read value)))))
        (t (string= (princ-to-string wrote) (princ-to-string value)))))

(defun set-cell (call)
  "The iwork_set_cell tool: one cell, in the app, read back and receipted."
  (answering
    (require-running)
    (let* ((path (desk-file (iwork-set-cell-request-path call)))
           (kind (document-kind path))
           (ref (iwork-set-cell-request-ref call))
           (value (iwork-set-cell-request-value call))
           (sheet (request-field call #'iwork-set-cell-request-sheet))
           (table (request-field call #'iwork-set-cell-request-table)))
      (unless (eq kind :numbers)
        (fail "iwork_set_cell edits .numbers files; ~a is a .~(~a~) file"
              (file-namestring path) (symbol-name kind)))
      (unless (probe-file path)
        (fail "~a is not there" (namestring path)))
      (when (iwork-set-cell-request-dry-run call)
        (return-from set-cell
          (nlk:encode-json-object
           (nlk:json-object "ok" t "dry_run" t "path" (namestring path)
                             "ref" ref "value" (princ-to-string value)
                             "change" (format nil "would set ~a to ~a" ref value)))))
      (let ((answer
              (guarded-write "iwork_set_cell" path (format nil "Set ~a to ~a" ref value)
                             (lambda ()
                               (run-op :set-cell :path (namestring path) :sheet sheet
                                                 :table table :ref ref :value value)))))
        ;; The app is the truth: what it kept, not what we asked, is the write.
        (let ((again (run-op :read-numbers :path (namestring path) :sheet sheet
                                          :table table :range ref :max_rows 1)))
          (let ((wrote (first-cell again)))
            (unless (same-value-p wrote value)
              (let ((receipt (car (last (coerce (read-receipts) 'list)))))
                (ignore-errors (restore-from (nlk:json-value receipt :string "backup") path))
                (record-receipt "iwork_set_cell" path (nlk:json-value receipt :string "after") (digest-of path)
                                (nlk:json-value receipt :string "backup")
                                (format nil "Took back the write of ~a: the app kept ~s" ref wrote))
                (fail "the app kept ~s where ~s was written, so ~a is back as it was"
                      wrote (princ-to-string value) (file-namestring path)))))
          (nlk:encode-json-object
           (nlk:json-object "ok" t
                             "path" (namestring path)
                             "ref" ref
                             "value" (princ-to-string value)
                             "kept" (nlk:json-value answer :string "wrote")
                             "receipt" (nlk:json-value (car (last (coerce (read-receipts) 'list)))
                                                        :integer "id"))))))))

;;; --- iwork_slides --------------------------------------------------------------------

(defun slide-op (call)
  "The iwork_slides tool: a deck's slides, on the app's own terms."
  (answering
    (require-running)
    (let* ((path (desk-file (iwork-slides-request-path call)))
           (kind (document-kind path))
           (action (or (iwork-slides-request-action call) "list"))
           (slide (iwork-slides-request-slide call))
           (title (or (iwork-slides-request-title call) ""))
           (body (or (iwork-slides-request-body call) "")))
      (unless (eq kind :keynote)
        (fail "iwork_slides edits .key decks; ~a is a .~(~a~) file"
              (file-namestring path) (symbol-name kind)))
      (if (equal action "list")
          (nlk:encode-json-object (run-op :read-keynote :path (namestring path)))
          (progn
            (when (and (member action '("set_text" "delete") :test #'equal) (null slide))
              (fail "~a needs the slide's number, counted from 1 (action=list answers them)" action))
            (when (and (member action '("add" "set_text") :test #'equal)
                       (zerop (length title)) (zerop (length body)))
              (fail "~a needs a title, a body, or both" action))
            (let ((answer (guarded-write "iwork_slides" path
                                         (format nil "~a slide ~@[~a~]" action slide)
                                         (lambda ()
                                           (run-op :slide :path (namestring path) :action action
                                                           :slide (or slide 0) :title title :body body)))))
              (nlk:encode-json-object answer)))))))

;;; --- iwork_export --------------------------------------------------------------------

(defun export-formats (kind)
  "The formats KIND's own app writes."
  (ecase kind
    (:numbers '("pdf" "xlsx"))
    (:keynote '("pdf" "pptx"))
    (:pages '("pdf" "docx"))))

(defun export-application (kind)
  (ecase kind (:numbers "Numbers") (:keynote "Keynote") (:pages "Pages")))

(defun export-as (kind format)
  "The name the app's exporter knows FORMAT by."
  (declare (ignore kind))
  (cond ((equal format "pdf") "PDF")
        ((equal format "xlsx") "Microsoft Excel")
        ((equal format "pptx") "Microsoft PowerPoint")
        ((equal format "docx") "Microsoft Word")
        (t (fail "~a is no export format: pdf, xlsx, pptx and docx are" format))))

(defun export-document (call)
  "The iwork_export tool: a PDF, or the Office file the app itself writes."
  (answering
    (require-running)
    (let* ((path (desk-file (iwork-export-request-path call)))
           (kind (document-kind path))
           (format (or (iwork-export-request-export-format call) "pdf"))
           (named (iwork-export-request-output call))
           (overwrite (iwork-export-request-overwrite call)))
      (unless (member format (export-formats kind) :test #'equal)
        (fail "~a exports ~{~a~^, ~}; ask for one of those"
              (file-namestring path) (export-formats kind)))
      (let* ((source (expand-path path))
             (output (desk-file (if (and named (plusp (length named)))
                                    named
                                    (namestring (make-pathname
                                                 :type format
                                                 :defaults source))))))
        (when (and (probe-file output) (not overwrite))
          (fail "~a is already there: ask for overwrite, or another output"
                (file-namestring output)))
        (let ((answer (run-op :export :path (namestring source) :output (namestring output)
                                      :as (export-as kind format) :app (export-application kind))))
          (unless (probe-file output)
            (fail "the app answered but ~a is not there" (namestring output)))
          (let ((receipt (record-receipt "iwork_export" output nil (digest-of output) nil
                                         (format nil "Exported to ~a" (file-namestring output)))))
            (nlk:encode-json-object
             (nlk:json-object "ok" t "path" (namestring source) "output" (namestring output)
                               "format" format "as" (nlk:json-value answer :string "as")
                               "receipt" (nlk:json-value receipt :integer "id")))))))))

;;; --- iwork_backups -------------------------------------------------------------------

(defun backups (call)
  "The iwork_backups tool: every write, and the one that puts a file back."
  (answering
    (require-running)
    (let* ((action (or (iwork-backups-request-action call) "list"))
           (filter (or (iwork-backups-request-path call) ""))
           (id (iwork-backups-request-id call)))
      (if (equal action "restore")
          (progn
            (unless id
              (fail "restore needs the id of a receipt: iwork_backups action=list answers them"))
            (let* ((receipt (receipt-of id))
                   (path (nlk:json-value receipt :string "path"))
                   (backup (nlk:json-value receipt :string "backup")))
              (unless (and backup (plusp (length backup)) (probe-file backup))
                (fail "receipt ~d has no backup to put back: that write made its file" id))
              (desk-file path)
              (let ((before (digest-of path)))
                (restore-from backup path)
                (let ((new (record-receipt "iwork_backups" (expand-path path) before (digest-of path) backup
                                            (format nil "Restored receipt ~d" id))))
                  (nlk:encode-json-object
                   (nlk:json-object "ok" t "restored" id "path" path
                                     "receipt" (nlk:json-value new :integer "id")))))))
          (let* ((all (coerce (read-receipts) 'list))
                 (shown (if (plusp (length filter))
                            (remove-if-not (lambda (receipt)
                                             (equal (namestring (expand-path filter))
                                                    (nlk:json-value receipt :string "path")))
                                           all)
                            all))
                 (newest (last shown (min 20 (length shown)))))
            (nlk:encode-json-object
             (nlk:json-object "ok" t "count" (length shown)
                               "folders" (coerce (fence-folders) 'vector)
                               "receipts" (coerce newest 'vector))))))))

;;; --- /iwork -------------------------------------------------------------------------

(defun installed-apps ()
  "Which of the three apps this Mac has, by their usual place in /Applications."
  (loop for (kind . path) in '(("Numbers" . "/Applications/Numbers.app/")
                               ("Keynote" . "/Applications/Keynote.app/")
                               ("Pages" . "/Applications/Pages.app/"))
        when (probe-file path) collect kind))

(defun run-slash (args session-id)
  "The /iwork command: what this machine can do, and the last writes."
  (declare (ignore session-id))
  (let* ((argument (nlk:trimmed (or args "")))
         (folders (ignore-errors (fence-folders)))
         (all (coerce (read-receipts) 'list)))
    (with-output-to-string (out)
      (format out "iwork: ~a~%"
              (if (macos-p)
                  (let ((apps (installed-apps)))
                    (if apps
                        (format nil "macOS, with ~{~a~^, ~} installed" apps)
                        "macOS, but none of Numbers, Keynote or Pages is in /Applications"))
                  "not a Mac: the tools need osascript and the apps"))
      (format out "folders: ~{~a~^, ~}~%"
              (if folders folders '("none yet: add one in Settings > iwork > folders")))
      (when (plusp (length argument))
        (format out "argument: ~a~%" argument))
      (if (null all)
          (format out "no write yet~%")
          (progn
            (format out "~d write~:p, newest last:~%" (length all))
            (dolist (receipt (last all (min 10 (length all))))
              (format out "  #~a ~a ~a  (restore: iwork_backups action=restore id=~a)~%"
                      (nlk:json-value receipt :integer "id")
                      (nlk:json-value receipt :string "tool")
                      (file-namestring (or (nlk:json-value receipt :string "path") ""))
                      (nlk:json-value receipt :integer "id"))))))))

;;; --- the cell ----------------------------------------------------------------------

(nle:define-cell iwork
  (:section ("iwork")
    (:guide "add the folders this cell may read and write in (absolute paths, ~ accepted); the tools then drive Numbers, Keynote and Pages on this Mac; next step: ask what a .numbers, .key or .pages file says")
    ("folders" :list :required t
     :doc "the folders this cell reads and writes inside; everything else is refused")
    ("backups" :integer :default 10 :min 1
     :doc "how many backups of one file to keep")
    ("timeout" :integer :default 180 :min 1
     :doc "how long one script sent to an app may take, in seconds")
    ("osascript" :string :default "/usr/bin/osascript"
     :doc "the osascript to run the app scripts with"))
  (:help :iwork
         "iwork: iwork_read, iwork_set_cell, iwork_slides, iwork_export and iwork_backups edit Numbers, Keynote and Pages files through the apps, backed up and checked"
         *manual*)
  (:tool "iwork_read" (nlk:contract 'iwork-read-request) #'read-file)
  (:tool "iwork_set_cell" (nlk:contract 'iwork-set-cell-request) #'set-cell)
  (:tool "iwork_slides" (nlk:contract 'iwork-slides-request) #'slide-op)
  (:tool "iwork_export" (nlk:contract 'iwork-export-request) #'export-document)
  (:tool "iwork_backups" (nlk:contract 'iwork-backups-request) #'backups)
  (:start (lambda ()
            ;; The fence is the whole policy: no folder, nothing to edit. A start
            ;; with none refuses, which the gateway shows as the folder's REFUSED
            ;; line, and (restart-cells) tries again once one is set.
            (unless (ignore-errors (fence-folders))
              (nlk:config-error "folders is empty: add the folders this cell may touch (folders: [\"~~/Documents\"]) and restart the cell"))
            (setf *running* t)
            (nle:on-stop (lambda () (setf *running* nil)))))
  (:command "iwork" 'run-slash
            :description "What this Mac can do with Numbers, Keynote and Pages, and the last writes"
            :argument-hint "list | restore ID"))
