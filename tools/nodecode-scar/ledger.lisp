;;;; ledger.lisp --- the scar ledger: the file, one row, and what covers what.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One JSON line per scar in this nodecode home, oldest first: the id, the
;;;; universal time it was recorded at, the token set, and the redacted
;;;; preview of the command that was denied. Nothing here knows what a cell
;;;; is: the file arrives as a path, so a test writes it under a scratch home.
;;;;
;;;; A missing file was looked for, an empty one holds no scars, and a file
;;;; that does not parse is UNREADABLE -- not empty. A caller decides what an
;;;; unreadable ledger means; for the block that answers here it means an rm
;;;; command is not run while a person can move the file aside.

(in-package #:nodecode-scar)

(defparameter +file-name+ "scar.json"
  "The ledger's name in the home, unless the section names another file.")

(defparameter +max-preview+ 500
  "How much of a denied command a scar keeps. The file is read on every call.")

;;; --- the preview, with secrets masked ----------------------------------------

(defparameter +secret-name+
  (cl-ppcre:create-scanner "(?i)(KEY|SECRET|TOKEN|PASS|PWD|CREDENTIAL|AUTH)")
  "The words an assignment's name carries when its value is a secret.")

(defparameter +known-prefix+
  (cl-ppcre:create-scanner "^(sk-|ghp_|gho_|github_pat_|xoxb-|xoxp-|hf_|glpat_|glpat-|AIza|AKIA|ya29\\.)")
  "What a token of a known service starts with.")

(defparameter +long-hex+ (cl-ppcre:create-scanner "^[0-9a-fA-F]{32,}$")
  "A run of hex long enough to be a key rather than a word.")

(defparameter +url-password+ (cl-ppcre:create-scanner "://[^/:@[:space:]]+:[^/@[:space:]]+@")
  "A password written into a URL.")

(defun redact-word (word)
  "WORD with a secret-shaped value masked: an assignment whose name says
secret, a known token prefix, a long hex run."
  (let ((equals (position #\= word)))
    (cond
      ((and equals (plusp equals)
            (cl-ppcre:scan +secret-name+ (subseq word 0 equals)))
       (concatenate 'string (subseq word 0 (1+ equals)) "****"))
      ((cl-ppcre:scan +known-prefix+ word) "****")
      ((cl-ppcre:scan +long-hex+ word) "****")
      (t word))))

(defun redact (text)
  "TEXT with secret-shaped words masked: a URL's password, an assignment
whose name says secret, a known token prefix, a long hex run."
  (if (not (stringp text))
      ""
      (let* ((clean (cl-ppcre:regex-replace-all "://([^/:@[:space:]]+):([^/@[:space:]]+)@"
                                                 text "://\\1:****@"))
             (words (remove "" (mapcar #'nlk:trimmed
                                       (nlk:lines (substitute #\Space #\Tab clean)))
                            :test #'equal)))
        (format nil "~{~a~^ ~}" (mapcar #'redact-word words)))))

(defun preview-of (text)
  "TEXT as a preview: secrets masked and folded onto one line, at most
+MAX-PREVIEW+ characters."
  (let ((line (redact text)))
    (subseq line 0 (min (length line) +max-preview+))))

;;; --- one row ------------------------------------------------------------------

(defun home-file ()
  "The ledger's path: the section's `file', else the name kept in this home."
  (let ((named (setting :file)))
    (nlk:home (if (and (stringp named) (plusp (length named))) named +file-name+))))

(defun row-id (row) (or (nlk:json-value row :number "id") 0))

(defun row-ts (row) (or (nlk:json-value row :number "ts") 0))

(defun row-tokens (row)
  "The token set ROW holds, as a list, or NIL when the row carries none."
  (let ((array (nlk:json-array row "danger_tokens")))
    (when (vectorp array) (coerce array 'list))))

(defun row-preview (row) (or (nlk:json-value row :string "preview") ""))

(defun row-p (row)
  "True when ROW is one this cell wrote: an id, a time, a sorted non-empty
token set drawn from the three rm words, and a short preview."
  (and (hash-table-p row)
       (let ((id (nlk:json-value row :number "id"))
             (tokens (row-tokens row))
             (preview (nlk:json-value row :string "preview"))
             (ts (nlk:json-value row :number "ts")))
         (and (integerp id) (plusp id)
              (numberp ts)
              (stringp preview) (<= (length preview) +max-preview+)
              (consp tokens)
              (every (lambda (token) (member token +allowed-tokens+ :test #'equal)) tokens)
              (equal tokens (sort (remove-duplicates tokens :test #'equal) #'string<))))))

;;; --- the file ------------------------------------------------------------------

(defun read-scars ()
  "The ledger. => (values STATUS ROWS), STATUS :absent, :ok or :unreadable.
An absent file was looked for; a line that does not parse makes the file
unreadable rather than quietly shorter."
  (let* ((path (home-file)) (exists (probe-file path)) (text nil) (lines 0) (rows nil))
    (unless exists (return-from read-scars (values :absent '())))
    (setf text (ignore-errors (nlk:read-text path)))
    (unless text (return-from read-scars (values :unreadable '())))
    (setf lines (count-if (lambda (line) (plusp (length (nlk:trimmed line))))
                          (nlk:lines text)))
    (when (zerop lines) (return-from read-scars (values :ok '())))
    (setf rows (ignore-errors (nlk:read-json-lines path :test #'row-p)))
    (if (and rows (= (length rows) lines))
        (values :ok rows)
        (values :unreadable '()))))

(defun write-scars (rows)
  "ROWS as the whole ledger, through a neighbour renamed over it."
  (let* ((path (home-file))
         (tmp (concatenate 'string (namestring path) ".tmp")))
    (ensure-directories-exist path)
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
      (dolist (row rows) (write-line (nlk:encode-json-object row) out)))
    (rename-file tmp path)
    rows))

;;; --- what covers what ----------------------------------------------------------

(defun covers-p (row tokens)
  "True when the call's TOKENS include every token ROW kept. `rm -r' does
not match a scar that also kept force."
  (let ((kept (row-tokens row)))
    (and kept (every (lambda (token) (member token tokens :test #'equal)) kept))))

(defun matching-scar (rows tokens)
  "The oldest scar every token of which the call carries, or NIL."
  (let ((matches (remove-if-not (lambda (row) (covers-p row tokens)) rows)))
    (and matches (first (sort (copy-list matches) #'< :key #'row-id)))))

(defun add-scar (tokens preview)
  "Store one scar and answer its row. The same token set is the scar that is
already there; a ledger at its cap refuses a new one rather than dropping an
old decision."
  (multiple-value-bind (status rows) (read-scars)
    (when (eq status :unreadable)
      (fail "the ledger file is present but could not be read. Nothing was written."))
    (or (find-if (lambda (row) (equal (row-tokens row) tokens)) rows)
        (let ((cap (setting :max-scars)))
          (when (>= (length rows) cap)
            (fail "the ledger is at its cap of ~d scars, so nothing was written; forget one first." cap))
          (let* ((id (1+ (reduce #'max rows :key #'row-id :initial-value 0)))
                 (row (nlk:make-json-object "id" id
                                             "ts" (get-universal-time)
                                             "danger_tokens" (coerce tokens 'vector)
                                             "preview" preview)))
            (write-scars (append rows (list row)))
            row)))))

(defun remove-scar (id)
  "Remove one scar by ID. => (values ROW COUNT-LEFT), or NIL when no scar
has that id. An absent or unreadable file is not created or replaced."
  (multiple-value-bind (status rows) (read-scars)
    (when (eq status :unreadable)
      (fail "the ledger file is present but could not be read. Nothing was written."))
    (let ((row (find id rows :key #'row-id :test #'=)))
      (when row
        (let ((left (remove row rows)))
          (write-scars left)
          (values row (length left)))))))
