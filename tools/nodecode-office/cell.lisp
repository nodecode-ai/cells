;;;; cell.lisp --- guarded Word, PowerPoint and Excel edits: inspect, search,
;;;; replace, leave a tracked change, and take one back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every write goes the same way: the policy check (inside a folder the
;;;; operator put on the desk) -> a byte-exact backup -> the edit, written to
;;;; a temporary file and renamed -> a receipt holding both digests and the
;;;; backup. A refusal touches nothing, and office_receipt brings a file back.
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :office) answers the primer below, and every request's help
;;;; section carries one line naming it.
;;;;
;;;; Rewritten from DECRUX9812's Hermes Agent plugin office-desk; see NOTICE.
;;;; What a Word, PowerPoint and Excel file is (a ZIP of XML parts) and how its
;;;; text is found and changed is in zip.lisp and office.lisp, which depend on
;;;; nothing beyond nodecode's own image.

(in-package #:nodecode-office)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Office files are available through the nodecode-office cell: five tools, which a
person can also call from eval in the office package. A file must live in a folder
on the desk (the cell's folders setting); anything else is refused before it is
touched, and every write leaves a backup and a receipt behind.
  office_inspect      {path, detail, limit}
      What a .docx, .pptx or .xlsx says: its paragraphs, slides or cells, with the
      Word revision count when there is one. Call it before editing anything, so
      the text you change is the text the file holds.
  office_search       {path, query, limit}
      Where a phrase appears, by paragraph, slide or cell reference - the cheapest
      way to find the one place to change in a long document.
  office_replace      {path, find, replace, nth, output}
      Replace text in the parts a reader sees: Word paragraphs, PowerPoint text
      frames, Excel shared strings. It matches inside one run (one formatted
      stretch), so search the exact phrase first; when it does not match, a shorter
      phrase usually will. Every occurrence goes unless nth names one, and output
      writes a new file instead of changing this one.
  docx_tracked_edit   {path, find, replace, nth, author}
      Replace the first matching stretch of a Word document as a tracked change:
      the old text is a deletion and the new text an insertion, both under your
      author name, so a reviewer accepts or rejects them in Word. Use it when
      someone will read the change; office_replace when they will not.
  office_receipt      {action, id}
      The receipts of every write, newest last, each with its tool, file, digests
      and backup; action revert puts that file back byte for byte.
The formats are plain OOXML: .docx, .pptx and .xlsx only. Legacy .doc, .ppt and
.xls, macro-enabled variants, and a file open in Office right now, are not edited."
  "What (help :office) answers while the cell runs.")

;;; --- answers, and the one condition ---------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (office-error (condition)
       (nle:failure (format nil "ERROR: ~a" (office-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: office: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defvar *running* nil
  "Whether the cell is started: its verbs refuse until it is.")

(defun require-running ()
  "Refuse a verb called with nothing started."
  (unless *running*
    (fail "office is not running: start the cell, whose office section names the folders on the desk")))

;;; --- the tools' arguments ----------------------------------------------------------

(nlk:define-contract office-inspect-request
  (:doc "Inspect a Word (.docx), PowerPoint (.pptx) or Excel (.xlsx) file: its paragraphs, slides or cells, with the Word revision count. Call it before editing so the text you change is the text the file holds. The file must be inside a folder on the desk.")
  ("path" :text :required t :doc "the file to inspect, absolute or ~/...")
  ("detail" :string :default "summary" :options '("summary" "full")
   :doc "summary previews a few blocks; full answers every block")
  ("limit" :integer :doc "how many blocks to show in summary (defaults to the cell's preview setting)"))

(nlk:define-contract office-search-request
  (:doc "Find where a phrase appears in a Word, PowerPoint or Excel file, by paragraph, slide or cell reference. The cheapest way to find the one place to change in a long document.")
  ("path" :text :required t :doc "the file to search")
  ("query" :text :required t :doc "the exact phrase to look for")
  ("limit" :integer :default 20 :doc "the most hits to answer"))

(nlk:define-contract office-replace-request
  (:doc "Replace text in a Word, PowerPoint or Excel file. It matches inside one formatting run, so search the exact phrase with office_search first and use a shorter one when it does not match. Every occurrence is replaced unless nth names one. The file must be on the desk; the write leaves a backup and a receipt, and office_receipt can put it back.")
  ("path" :text :required t :doc "the file to edit")
  ("find" :text :slot find-text :required t :doc "the text to replace")
  ("replace" :string :slot replacement-text :default "" :doc "what to put in its place; empty deletes it")
  ("nth" :integer :slot which :doc "replace only the nth occurrence, 1-based; every occurrence when omitted")
  ("output" :string :default "" :doc "write a new file at this path instead of changing the original"))

(nlk:define-contract office-tracked-request
  (:doc "Replace a stretch of a Word document as a tracked change: the old text becomes a deletion and the new text an insertion, both under the given author, so a reviewer accepts or rejects them in Word. Use office_replace when the change should simply be made. The file must be on the desk.")
  ("path" :text :required t :doc "the .docx file to edit")
  ("find" :text :slot find-text :required t :doc "the text to mark deleted")
  ("replace" :string :slot replacement-text :default "" :doc "the text to mark inserted; empty deletes it")
  ("nth" :integer :slot which :doc "the nth occurrence, 1-based; the first when omitted")
  ("author" :string :default "" :doc "the name the change carries; the cell's author setting when omitted"))

(nlk:define-contract office-receipt-request
  (:doc "List the receipts of every write on the desk, each with its tool, file, digests and backup; or revert one, putting its file back byte for byte.")
  ("action" :string :default "list" :options '("list" "revert") :doc "list the receipts, or revert one")
  ("id" :integer :doc "the receipt to revert, as list answers it"))

;;; --- the desk: which folders may be read and written -------------------------------

(defun expand-path (text)
  "TEXT as an absolute pathname, ~ resolved."
  (let ((text (string-trim " " (princ-to-string text))))
    (when (zerop (length text)) (fail "a file path is required"))
    (if (char= #\~ (char text 0))
        (merge-pathnames (subseq text (min 2 (length text))) (user-homedir-pathname))
        (uiop:ensure-pathname text :want-absolute t))))

(defun desk-folders ()
  (mapcar (lambda (entry) (princ-to-string entry)) (setting :folders)))

(defun desk-file (path)
  "PATH's namestring when it sits inside a folder on the desk, else a refusal."
  (let* ((pathname (expand-path path))
         (probe (or (uiop:truename* pathname)
                    (uiop:truename* (uiop:pathname-directory-pathname pathname))
                    (uiop:pathname-directory-pathname pathname)))
         (folders (desk-folders)))
    (unless folders
      (fail "no folder is on the desk: set the cell's folders (folders: [\"~~/Documents\"] in ~/.nodecode/config.jsonc, or Settings) before touching ~a"
            (namestring pathname)))
    (or (loop for folder in folders
              for root = (uiop:truename* (expand-path folder))
              when (and root (uiop:subpathp probe root))
                return (namestring pathname))
        (fail "~a is not inside a folder on the desk (~{~a~^, ~}), and this cell reads and writes only there"
              (namestring pathname) folders))))

;;; --- receipts and backups ---------------------------------------------------------

(defun data-file (relative)
  (namestring (nlk:home (concatenate 'string "office/" relative))))

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
    (ensure-directories-exist target)
    (write-octets target (read-octets path))
    (namestring (or (uiop:truename* target) target))))

(defun digest-of (path)
  (let ((octets (read-octets path)))
    (and octets (nlk:sha256-text octets))))

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
    receipt))

(defun record-write (tool path octets summary)
  "Write OCTETS over PATH, with a backup and a receipt. => the receipt, or
NIL when the file did not change."
  (let* ((path (expand-path path))
         (exists (probe-file path))
         (before (and exists (digest-of path)))
         (after (nlk:sha256-text octets)))
    (when (and before (equal before after)) (return-from record-write nil))
    (let ((backup (and exists
                        (backup-file path (next-receipt-id (coerce (read-receipts) 'list))))))
      (write-octets path octets)
      (record-receipt tool path before after backup summary))))

(defun receipt-of (id)
  (or (find id (coerce (read-receipts) 'list)
            :key (lambda (receipt) (nlk:json-value receipt :integer "id")))
      (fail "receipt ~a not found" id)))

(defun request-kind (path)
  (or (office-kind path)
      (fail "~a is not a Word, PowerPoint or Excel file: this cell edits .docx, .pptx and .xlsx (the legacy and macro-enabled formats are refused)"
            path)))

(defun revision-count (entries)
  (let ((entry (find-part entries "word/document.xml")))
    (and entry (count-find (zip-entry-text entry) "<w:ins "))))

;;; --- office_inspect ---------------------------------------------------------------

(defun slot-summary (entries kind)
  "What KIND's container holds beyond its text: parts, slides, sheets, revisions."
  (ecase kind
    (:docx (nlk:json-object "parts" (length entries)
                             "revisions" (revision-count entries)))
    (:pptx (nlk:json-object "parts" (length entries)
                             "slides" (count-if (lambda (entry)
                                                  (slide-number (zip-entry-name entry)))
                                                entries)))
    (:xlsx (let ((sheets (xlsx-sheets entries)))
             (nlk:json-object "parts" (length entries)
                              "sheets" (length sheets)
                              "sheet_names" (coerce (mapcar #'car sheets) 'vector))))))

(defun inspect-file (call)
  "The office_inspect tool: what a document says, block by block."
  (answering
    (require-running)
    (let* ((path (office-inspect-request-path call))
           (detail (or (office-inspect-request-detail call) "summary"))
           (limit (or (office-inspect-request-limit call) (setting :preview)))
           (kind (progn (desk-file path) (request-kind path)))
           (entries (archive path))
           (blocks (document-blocks entries kind))
           (shown (if (equal detail "full") blocks (subseq blocks 0 (min limit (length blocks)))))
           (answer (nlk:json-object
                    "ok" t
                    "path" (namestring (expand-path path))
                    "kind" (string-downcase (symbol-name kind))
                    "blocks" (length blocks)
                    "digest" (or (digest-of path) "")
                    "preview" (coerce shown 'vector))))
      (let ((slots (slot-summary entries kind)))
        (loop for key being the hash-keys of slots
              do (setf (gethash key answer) (gethash key slots))))
      (nlk:encode-json-object answer))))

;;; --- office_search ----------------------------------------------------------------

(defun search-file (call)
  "The office_search tool: where a phrase appears."
  (answering
    (require-running)
    (let* ((path (office-search-request-path call))
           (query (office-search-request-query call))
           (limit (or (office-search-request-limit call) 20))
           (kind (progn (desk-file path) (request-kind path)))
           (entries (archive path))
           (hits '()))
      (when (zerop (length query)) (fail "query is empty"))
      (dolist (block (document-blocks entries kind))
        (let ((text (getf block :text)))
          (when (search query text)
            (push (nlk:json-object "where" (getf block :where)
                                    "text" (nlk:clip text 240))
                  hits))))
      (setf hits (nreverse hits))
      (nlk:encode-json-object
       (nlk:json-object "ok" t "query" query "count" (length hits)
                        "hits" (coerce (subseq hits 0 (min limit (length hits))) 'vector))))))

;;; --- office_replace ---------------------------------------------------------------

(defun edit-parts (entries kind find replacement nth)
  "ENTRIES with FIND replaced by REPLACEMENT in the parts a reader sees.
=> (values ENTRIES COUNT)."
  (let ((count 0)
        (targets (ecase kind
                   (:docx (list (cons "word/document.xml" '("w:t"))))
                   (:pptx (loop for entry in entries
                                for number = (slide-number (zip-entry-name entry))
                                when number
                                  collect (cons (zip-entry-name entry) '("a:t"))))
                   (:xlsx (list (cons "xl/sharedStrings.xml" '("t"))))))
        (edited (copy-list entries)))
    (loop for (name . tags) in targets
          for position = (position name edited :key #'zip-entry-name :test #'string=)
          when position
            do (let ((entry (nth position edited)))
                 (multiple-value-bind (xml replaced)
                     (replace-in-part (zip-entry-text entry) tags find replacement nth)
                   (when (plusp replaced)
                     (incf count replaced)
                     (setf (nth position edited) (set-entry-text entry xml))))))
    (values edited count)))

(defun replace-text (call)
  "The office_replace tool: plain text, in place or into a new file."
  (answering
    (require-running)
    (let* ((path (office-replace-request-path call))
           (find (office-replace-request-find-text call))
           (replacement (or (office-replace-request-replacement-text call) ""))
           (nth (office-replace-request-which call))
           (output (office-replace-request-output call))
           (kind (progn (desk-file path) (request-kind path)))
           (entries (archive path)))
      (when (zerop (length find)) (fail "find is empty"))
      (multiple-value-bind (edited count) (edit-parts entries kind find replacement nth)
        (when (zerop count)
          (fail "~s is not in ~a as one stretch of text. Search a shorter phrase with office_search first: a phrase split across two formatting runs does not match."
                find (file-namestring path)))
        (let* ((target (if (and output (plusp (length output)))
                           (progn (desk-file output) output)
                           path))
               (receipt (record-write "office_replace" target (repack edited)
                                       (format nil "Replaced ~s with ~s (~d)" find replacement count))))
          (nlk:encode-json-object
           (nlk:json-object "ok" t "path" (namestring (expand-path target))
                            "replaced" count
                            "receipt" (and receipt (nlk:json-value receipt :integer "id"))
                            "backup" (or (and receipt (nlk:json-value receipt :string "backup")) ""))))))))

;;; --- docx_tracked_edit ------------------------------------------------------------

(defun substitute-entry (entries entry)
  "ENTRIES with the entry of the same name as ENTRY replaced by it."
  (mapcar (lambda (existing)
            (if (string= (zip-entry-name existing) (zip-entry-name entry)) entry existing))
          entries))

(defun tracked-edit-file (call)
  "The docx_tracked_edit tool: a Word change a reviewer accepts or rejects."
  (answering
    (require-running)
    (let* ((path (office-tracked-request-path call))
           (find (office-tracked-request-find-text call))
           (replacement (or (office-tracked-request-replacement-text call) ""))
           (nth (office-tracked-request-which call))
           (author (let ((named (office-tracked-request-author call)))
                     (if (and named (plusp (length named))) named (setting :author))))
           (kind (progn (desk-file path) (request-kind path)))
           (entries (archive path)))
      (unless (eq kind :docx)
        (fail "docx_tracked_edit edits .docx files; ~a is a .~(~a~) file, so use office_replace"
              (file-namestring path) (symbol-name kind)))
      (when (zerop (length find)) (fail "find is empty"))
      (multiple-value-bind (xml count) (tracked-edit entries find replacement author nth)
        (when (zerop count)
          (fail "~s is not in ~a as one stretch of text. Search a shorter phrase with office_search first, then edit that."
                find (file-namestring path)))
        (let* ((entry (find-part entries "word/document.xml"))
               (edited (substitute-entry entries (set-entry-text entry xml)))
               (receipt (record-write "docx_tracked_edit" path (repack edited)
                                       (format nil "Tracked ~s -> ~s by ~a"
                                               find replacement author))))
          (nlk:encode-json-object
           (nlk:json-object "ok" t "path" (namestring (expand-path path))
                            "revisions" (revision-count edited)
                            "author" author
                            "receipt" (and receipt (nlk:json-value receipt :integer "id"))
                            "backup" (or (and receipt (nlk:json-value receipt :string "backup")) ""))))))))

;;; --- office_receipt ---------------------------------------------------------------

(defun receipts (call)
  "The office_receipt tool: every write, and the one that takes a file back."
  (answering
    (require-running)
    (let ((action (or (office-receipt-request-action call) "list"))
          (id (office-receipt-request-id call)))
      (if (equal action "revert")
          (progn
            (unless id (fail "revert needs the id of a receipt: office_receipt action=list answers them"))
            (let* ((receipt (receipt-of id))
                   (path (nlk:json-value receipt :string "path"))
                   (backup (nlk:json-value receipt :string "backup")))
              (unless (and backup (plusp (length backup)) (probe-file backup))
                (fail "receipt ~d has no backup to restore: that write created its file, so there is nothing to put back" id))
              (desk-file path)
              (let ((bytes (read-octets backup)))
                (unless bytes (fail "the backup of receipt ~d is unreadable" id))
                (let ((new (record-write "office_receipt" path bytes
                                          (format nil "Reverted receipt ~d" id))))
                  (nlk:encode-json-object
                   (nlk:json-object "ok" t "reverted" id "path" path
                                    "receipt" (and new (nlk:json-value new :integer "id"))))))))
          (let* ((all (coerce (read-receipts) 'list))
                 (shown (last all (min 20 (length all)))))
            (nlk:encode-json-object
             (nlk:json-object "ok" t "count" (length all)
                              "folders" (coerce (desk-folders) 'vector)
                              "receipts" (coerce shown 'vector))))))))

;;; --- /office ----------------------------------------------------------------------

(defun run-slash (args session-id)
  "The /office command: the folders on the desk and the last writes."
  (declare (ignore session-id))
  (let* ((argument (nlk:trimmed (or args "")))
         (folders (desk-folders))
         (all (coerce (read-receipts) 'list)))
    (with-output-to-string (out)
      (format out "office desk: ~{~a~^, ~}~%"
              (if folders folders '("no folder yet: add one in Settings > office > folders")))
      (when (plusp (length argument))
        (format out "argument: ~a~%" argument))
      (if (null all)
          (format out "no write yet~%")
          (progn
            (format out "~d write~:p, newest last:~%" (length all))
            (dolist (receipt (last all (min 10 (length all))))
              (format out "  #~a ~a ~a  (revert: office_receipt action=revert id=~a)~%"
                      (nlk:json-value receipt :integer "id")
                      (nlk:json-value receipt :string "tool")
                      (file-namestring (or (nlk:json-value receipt :string "path") ""))
                      (nlk:json-value receipt :integer "id"))))))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell office
  (:section ("office")
    (:guide "add the folders you let this cell edit (absolute paths, ~ accepted); the author is the name Word tracked changes carry; next step: ask for a file's paragraphs, or to change one word in it")
    ("folders" :list :required t
     :doc "the folders on the desk: the cell reads and writes inside these and refuses everything else")
    ("author" :string :default "Nodecode"
     :doc "the author name on Word tracked changes and deletions")
    ("preview" :integer :default 12 :min 1
     :doc "how many blocks office_inspect previews by default"))
  (:help :office
         "office: office_inspect, office_search, office_replace, docx_tracked_edit and office_receipt edit Word, PowerPoint and Excel files, backed up and receipted"
         *manual*)
  (:tool "office_inspect" (nlk:contract 'office-inspect-request) #'inspect-file)
  (:tool "office_search" (nlk:contract 'office-search-request) #'search-file)
  (:tool "office_replace" (nlk:contract 'office-replace-request) #'replace-text)
  (:tool "docx_tracked_edit" (nlk:contract 'office-tracked-request) #'tracked-edit-file)
  (:tool "office_receipt" (nlk:contract 'office-receipt-request) #'receipts)
  (:start (lambda ()
            ;; The desk is the whole policy: no folder, nothing to edit. A start
            ;; with none refuses, which the gateway shows as the folder's REFUSED
            ;; line, and (restart-cells) tries again once one is set.
            (unless (desk-folders)
              (nlk:config-error "folders is empty: add the folders on the desk (folders: [\"~~/Documents\"]) and restart the cell"))
            (setf *running* t)
            (nle:on-stop (lambda () (setf *running* nil)))))
  (:command "office" 'run-slash
            :description "The folders on the office desk and the last writes"
            :argument-hint "list | revert ID"))
