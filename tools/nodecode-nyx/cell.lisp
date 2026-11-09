;;;; cell.lisp --- Nyx: the user's own memory, in a file on their disk.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of sixgodgit/nyx's Hermes memory-provider plugin (see NOTICE).
;;;; The original is local and single-user with no LLM on the write path: what
;;;; was said is logged verbatim and dated, secrets are replaced before
;;;; anything is stored, and a forgotten memory waits in a quarantine that can
;;;; be restored from until it expires. Those promises are what this cell
;;;; keeps, on the original's own journal line (STAMP | SOURCE | TEXT), so a
;;;; folder its command line already writes reads the same here.
;;;;
;;;; The harness seams are Nodecode's: the section is the config, the tools
;;;; are NLE:REGISTER-TOOL registrations, and /nyx is a slash command.

(defpackage #:nodecode-nyx
  (:use #:cl)
  (:export #:nyx-error #:nyx-error-detail))

(in-package #:nodecode-nyx)

(nlk:define-peripheral nyx :not-running t :lock "nyx")

(defparameter +journal-file+ "journal.txt"
  "One entry a line, in the original's shape: `2026-11-09 07:12:33 | user | text'.")

(defparameter +quarantine-file+ "quarantine.jsonl"
  "Forgotten entries, one JSON object a line, each with the time it expires.")

(defparameter +bar+ " | "
  "What separates an entry's stamp, its source and its text.")

(defparameter +stamp-length+ 19
  "YYYY-MM-DD HH:MM:SS, the width the original's journal reader checks.")

(defparameter +secret-mark+ "[hidden: "
  "What a replaced secret leaves behind, as the original's MARK does.")

(defparameter *secret-rules*
  ;; (LABEL PATTERN KEEP-TAIL). The original's secret_filter.RULES, less the
  ;; Chinese password phrasing; more specific patterns come first, so sk-ant-
  ;; is not read as sk-.
  '(("private key"
     "-----BEGIN (?:[A-Z0-9]+ )*PRIVATE KEY(?: BLOCK)?-----[\\s\\S]*?-----END (?:[A-Z0-9]+ )*PRIVATE KEY(?: BLOCK)?-----"
     nil)
    ("Anthropic API key" "\\bsk-ant-[A-Za-z0-9_\\-]{20,}" t)
    ("OpenRouter API key" "\\bsk-or-v1-[A-Za-z0-9]{32,}" t)
    ("OpenAI API key" "\\bsk-proj-[A-Za-z0-9_\\-]{20,}" t)
    ("API key" "\\bsk-[A-Za-z0-9]{20,}" t)
    ("HF token" "\\bhf_[A-Za-z0-9]{30,}" t)
    ("GitHub token" "\\b(?:gh[pousr]_[A-Za-z0-9]{36,}|github_pat_[A-Za-z0-9_]{40,})" t)
    ("GitLab token" "\\bglpat-[A-Za-z0-9_\\-]{20,}" t)
    ("PyPI token" "\\bpypi-[A-Za-z0-9_\\-]{40,}" t)
    ("npm token" "\\bnpm_[A-Za-z0-9]{36,}" t)
    ("AWS access key" "\\b(?:AKIA|ASIA)[0-9A-Z]{16}\\b" t)
    ("Google API key" "\\bAIza[0-9A-Za-z_\\-]{35}" t)
    ("Slack token" "\\bxox[abprs]-[A-Za-z0-9\\-]{10,}" t)
    ("Stripe key" "\\b(?:sk|rk)_(?:live|test)_[0-9A-Za-z]{16,}" t)
    ("SendGrid key" "\\bSG\\.[A-Za-z0-9_\\-]{16,}\\.[A-Za-z0-9_\\-]{16,}" t)
    ("JWT" "\\beyJ[A-Za-z0-9_\\-]{8,}\\.eyJ[A-Za-z0-9_\\-]{8,}\\.[A-Za-z0-9_\\-]{8,}" t))
  "What is replaced before an entry is written, and under which label.")

;;; --- the section, read back --------------------------------------------------

(defun now-stamp ()
  "The entry stamp: the operator's local time, as the original logs it."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(defun expand-folder (text)
  "TEXT -- a path as a person writes it, `~/' included -- as a directory pathname."
  (let ((text (string-trim '(#\Space #\Tab) text)))
    (uiop:ensure-directory-pathname
     (if (and (> (length text) 1) (char= #\~ (char text 0)) (char= #\/ (char text 1)))
         (merge-pathnames (subseq text 2) (user-homedir-pathname))
         (uiop:parse-native-namestring text)))))

(defun data-directory ()
  "Where the journal lives: the section's data_dir, else NEXSANDBASE_HOME,
else <home>/memory -- the original's NEXSANDBASE_HOME override, kept, so a
folder its command line already writes reads the same here."
  (let ((configured (setting :data-dir)))
    (cond ((typep configured 'pathname)
           (uiop:ensure-directory-pathname configured))
          ((and (stringp configured) (plusp (length (string-trim '(#\Space) configured))))
           (expand-folder configured))
          ((uiop:getenv "NEXSANDBASE_HOME")
           (expand-folder (uiop:getenv "NEXSANDBASE_HOME")))
          (t (uiop:ensure-directory-pathname (nlk:home "memory"))))))

(defun journal-path () (merge-pathnames +journal-file+ (data-directory)))
(defun quarantine-path () (merge-pathnames +quarantine-file+ (data-directory)))

(defun ensure-store ()
  "Make the data folder and prove it is writable, or refuse naming data_dir."
  (let ((directory (data-directory)))
    (handler-case
        (let ((probe (merge-pathnames "write-probe" directory)))
          (ensure-directories-exist probe)
          (with-open-file (out probe :direction :output :if-exists :supersede
                                     :if-does-not-exist :create :external-format :utf-8)
            (write-string "ok" out))
          (ignore-errors (delete-file probe)))
      (error (e)
        (error 'nlk:config-refusal
               :detail (format nil "nyx: data_dir ~a cannot be written (~a); point \"data_dir\" in the nyx section, or NEXSANDBASE_HOME, at a folder this account may write"
                               (namestring directory) e))))
    directory))

;;; --- the journal ---------------------------------------------------------------

(defun fenced (text)
  "TEXT on one line: an entry is a line of the journal, so its own breaks become spaces."
  (substitute #\Space #\Newline (substitute #\Space #\Return (string-trim '(#\Space #\Tab) text))))

(defun entry-id (stamp source text)
  "A stable id for an entry from its own content: m_ and twelve hex digits.
It survives a restart, so an id a tool answered yesterday restores today."
  (let ((hash 2166136261))
    (flet ((mix (string)
             (loop for character across string
                   do (setf hash (ldb (byte 32 0)
                                      (* (logxor hash (char-code character)) 16777619))))))
      (mix stamp)
      (mix +bar+)
      (mix source)
      (mix +bar+)
      (mix text))
    (format nil "m_~12,'0x" hash)))

(defun split-line (line)
  "LINE as (VALUES STAMP SOURCE TEXT), or NIL when it is not an entry."
  (let* ((first (search +bar+ line))
         (second (and first (search +bar+ line :start2 (+ first (length +bar+))))))
    (when (and first second (= +stamp-length+ first))
      (values (subseq line 0 first)
              (subseq line (+ first (length +bar+)) second)
              (subseq line (+ second (length +bar+)))))))

(defun entry-of-line (line)
  "LINE as an entry plist, or NIL."
  (multiple-value-bind (stamp source text) (split-line line)
    (when stamp
      (list :id (entry-id stamp source text) :stamp stamp :source source :text text))))

(defun read-entries ()
  "Every entry of the journal, oldest first."
  (let ((path (journal-path)))
    (when (probe-file path)
      (with-open-file (in path :external-format :utf-8)
        (loop for line = (read-line in nil nil)
              while line
              for entry = (entry-of-line line)
              when entry collect entry)))))

(defun entry-line (entry)
  (format nil "~a~a~a~a~a" (getf entry :stamp) +bar+ (getf entry :source) +bar+ (getf entry :text)))

(defun append-line (path line)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :append
                            :if-does-not-exist :create :external-format :utf-8)
    (write-string line out)
    (terpri out)))

(defun write-entries (entries)
  "Rewrite the journal as ENTRIES, atomically: a stop in the middle loses nothing."
  (nlk:write-file-atomically
   (journal-path)
   (with-output-to-string (out)
     (dolist (entry entries)
       (write-string (entry-line entry) out)
       (terpri out)))))

;;; --- secrets kept out of memory ---------------------------------------------------

(defun hidden (label)
  "The placeholder that replaces a secret, naming what it was."
  (format nil "~a~a]" +secret-mark+ label))

(defun filter-secrets (text)
  "TEXT with every token, key and password-shaped run replaced by its label.
The original's secret_filter on the way into memory."
  (if (not (setting :filter-secrets))
      text
      (reduce (lambda (string rule)
                (destructuring-bind (label pattern tail) rule
                  (cl-ppcre:regex-replace-all
                   pattern string
                   (lambda (&rest groups)
                     (declare (ignore groups))
                     (hidden label)))))
              *secret-rules* :initial-value text)))

;;; --- quarantine: two-stage forgetting ------------------------------------------------

(defun read-quarantine ()
  "The forgotten entries, newest first."
  (let ((path (quarantine-path)))
    (when (probe-file path)
      (with-open-file (in path :external-format :utf-8)
        (loop for line = (read-line in nil nil)
              while line
              for text = (string-trim '(#\Space #\Tab #\Return) line)
              when (plusp (length text))
                collect (handler-case (nlk:decode-json text) (error () nil))
              into rows
              finally (return (remove nil rows)))))))

(defun write-quarantine (rows)
  (nlk:write-file-atomically
   (quarantine-path)
   (with-output-to-string (out)
     (dolist (row rows)
       (write-string (nlk:encode-json-object row) out)
       (terpri out)))))

(defun row-value (row key) (nlk:json-value row :string key))
(defun row-expiry (row) (or (nlk:json-value row :integer "expires") 0))

(defun quarantine-row (entry days)
  (nlk:json-object "id" (getf entry :id)
                    "stamp" (getf entry :stamp)
                    "source" (getf entry :source)
                    "text" (getf entry :text)
                    "expires" (+ (get-universal-time) (* 86400 days))))

(defun sweep-quarantine ()
  "Erase the quarantine's expired entries: the second stage of forgetting."
  (let* ((rows (read-quarantine))
         (now (get-universal-time))
         (kept (remove-if (lambda (row) (<= (row-expiry row) now)) rows)))
    (when (/= (length kept) (length rows))
      (write-quarantine kept))
    (- (length rows) (length kept))))

;;; --- the verbs -------------------------------------------------------------------

(defun query-words (query)
  (when query
    (remove "" (uiop:split-string (string-downcase query)
                                   :separator '(#\Space #\Tab #\Newline))
            :test #'equal)))

(defun recall (&key query limit)
  "The entries every word of QUERY appears in, newest first, at most LIMIT.
A query with no words answers the newest entries."
  (let* ((entries (reverse (read-entries)))
         (words (query-words query))
         (hits (if words
                   (remove-if-not
                    (lambda (entry)
                      (let ((haystack (string-downcase
                                       (format nil "~a ~a" (getf entry :source) (getf entry :text)))))
                        (every (lambda (word) (search word haystack)) words)))
                    entries)
                   entries)))
    (subseq hits 0 (min limit (length hits)))))

(defun format-entry (entry)
  (format nil "[~a] ~a | ~a | ~a" (getf entry :id) (getf entry :stamp)
          (getf entry :source) (getf entry :text)))

(defun format-hits (query hits)
  "HITS as the lines the model reads, or one line saying nothing matched."
  (if (null hits)
      (format nil "No memory matched ~s." (or query ""))
      (with-output-to-string (out)
        (format out "~d ~a for ~s:" (length hits)
                (if (= 1 (length hits)) "memory" "memories") (or query ""))
        (dolist (entry hits)
          (format out "~% - ~a" (format-entry entry))))))

(defun day-of (universal-time)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun status-text ()
  "What /nyx answers with no argument: where memory is and what is in it."
  (let* ((directory (data-directory))
         (entries (read-entries))
         (rows (read-quarantine))
         (soonest (when rows (loop for row in rows minimize (row-expiry row)))))
    (format nil "Nyx memory: ~d ~a in ~a~@[; ~d forgotten, restorable until ~a~]"
            (length entries) (if (= 1 (length entries)) "entry" "entries")
            (namestring directory)
            (and rows (length rows))
            (and soonest (day-of soonest)))))

;;; --- the tools ------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: nyx: ~a" condition)))))

(defun argument-text (args name &optional (required t))
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (cond ((and (stringp value) (plusp (length (string-trim '(#\Space #\Tab) value))))
           (string-trim '(#\Space #\Tab) value))
          (required (fail "~a is required" (substitute #\_ #\- name)))
          (t nil))))

(defun argument-integer (args name default ceiling)
  (let ((value (gethash name args)))
    (if (integerp value) (max 1 (min value ceiling)) default)))

(defun tool-remember (args)
  "Store one thing the user said, dated and verbatim, secrets replaced first."
  (answering
   (let* ((text (argument-text args "text"))
          (source (or (argument-text args "source" nil) "user"))
          (entry (list :stamp (now-stamp)
                       :source (fenced source)
                       :text (fenced (filter-secrets text)))))
     (setf (getf entry :id) (entry-id (getf entry :stamp) (getf entry :source) (getf entry :text)))
     (append-line (journal-path) (entry-line entry))
     (format nil "Remembered [~a] at ~a~@[ (secrets written as a placeholder)~]"
             (getf entry :id) (getf entry :stamp)
             (and (search +secret-mark+ (getf entry :text)) t)))))

(defun tool-recall (args)
  "Answer what memory holds for a query, with the date of each entry."
  (answering
   (let ((query (argument-text args "query" nil)))
     (format-hits query
                  (recall :query query
                          :limit (argument-integer args "limit" (setting :recall-limit) 50))))))

(defun tool-forget (args)
  "Move what matches into the quarantine; it can be restored until it expires."
  (answering
   (let* ((id (argument-text args "mem_id" nil))
          (contains (argument-text args "contains" nil)))
     (when (and (null id) (null contains))
       (fail "give mem_id or contains: the entry to forget"))
     (let* ((entries (read-entries))
            (victims (remove-if-not
                      (lambda (entry)
                        (and (or (null id) (equal id (getf entry :id)))
                             (or (null contains)
                                 (search (string-downcase contains)
                                         (string-downcase (getf entry :text))))))
                      entries)))
       (when (null victims)
         (fail "no memory matched ~a" (or id contains)))
       (write-quarantine (append (mapcar (lambda (entry)
                                           (quarantine-row entry (setting :forget-days)))
                                         victims)
                                 (read-quarantine)))
       (write-entries (set-difference entries victims :test #'equal))
       (format nil "Forgotten ~d ~a, restorable for ~d days: ~{~a~^, ~}"
               (length victims) (if (= 1 (length victims)) "memory" "memories")
               (setting :forget-days) (mapcar (lambda (entry) (getf entry :id)) victims))))))

(defun tool-restore (args)
  "Put a quarantined entry back in memory, unchanged."
  (answering
   (let* ((id (argument-text args "mem_id"))
          (rows (read-quarantine))
          (row (find id rows :key (lambda (row) (row-value row "id")) :test #'equal)))
     (unless row (fail "~a is not in the quarantine" id))
     (append-line (journal-path)
                  (format nil "~a~a~a~a~a" (row-value row "stamp") +bar+
                          (or (row-value row "source") "user") +bar+ (or (row-value row "text") "")))
     (write-quarantine (remove row rows))
     (format nil "Restored ~a." id))))

(defun run-command (args session-id)
  "One /nyx invocation: a query answers what memory holds, no argument the store."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (format-hits query (recall :query query :limit (setting :recall-limit)))
        (status-text))))

;;; --- the cell -------------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type
                                                                     "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *remember-schema*
  (tool-schema
   "Store one durable thing the user said, verbatim and dated, in their own local memory: a preference, a decision, a fact about them or their work. Call it when they state something worth recalling in a later session -- don't wait to be asked to remember. Tokens, keys and passwords in the text are replaced before it is written."
   '(("text" "string" "What to remember, as the user said it." t)
     ("source" "string" "Where it came from: user (the default), a tool, a document." nil))))

(defparameter *recall-schema*
  (tool-schema
   "Search the user's local memory for what was said before; every hit carries the date it was said. Use it before answering anything that may depend on what you know about the user, and call it again with other words when the first answer is thin. No word of the query is required: an empty query answers the newest entries."
   '(("query" "string" "Words to look for; every word must appear." nil)
     ("limit" "integer" "Max entries (default: the section's recall_limit, at most 50)." nil))))

(defparameter *forget-schema*
  (tool-schema
   "Forget a memory. It moves to the quarantine, where it can still be restored, and is erased from the disk when the quarantine's window runs out. Use it only when the user asks to forget something."
   '(("mem_id" "string" "The entry's id, as recall answered it." nil)
     ("contains" "string" "Or an exact substring of the text to forget." nil))))

(defparameter *restore-schema*
  (tool-schema
   "Put a forgotten memory back, unchanged, while it is still in the quarantine."
   '(("mem_id" "string" "The quarantined entry's id." t))))

(nle:define-cell nyx
  (:section ("nyx")
    (:guide "memory is a folder on this disk; data_dir is where it lives (the original's ~/.neurobase can be named here to share it with the Nyx command line); recall_limit is how many entries one search answers; forget_days is how long a forgotten entry can be restored before it is erased; filter_secrets replaces tokens and keys before anything is written; next step: /nyx shows the store")
    ("data_dir" :path
     :doc "the folder the journal lives in; empty means <nodecode home>/memory, or NEXSANDBASE_HOME when it is set")
    ("recall_limit" :integer :default 8 :min 1
     :doc "how many entries one recall answers unless the call says otherwise")
    ("forget_days" :integer :default 7 :min 1
     :doc "how long a forgotten entry stays restorable before it is erased from the disk")
    ("filter_secrets" :boolean :default t
     :doc "replace tokens, keys and passwords with a placeholder before an entry is written"))
  (:help :nyx
   "nyx: local memory on your disk - nyx_remember, nyx_recall, nyx_forget and nyx_restore"
   "Nyx is a local, single-user memory: every entry is a line of a journal in its data folder, stamped with the time it was said and kept verbatim. Nothing is sent anywhere and no model runs on the write path. Text is filtered first, so a token, key or password is written as a placeholder naming what it was instead of its value. nyx_recall answers the entries every word of the query appears in, newest first, each with its date and its id; nyx_forget moves an entry to the quarantine, which it can be restored from until forget_days have passed, and then it is erased; /nyx with no argument shows the store, and /nyx with words searches it.")
  (:command "nyx" 'run-command
            :description "Show the memory store, or search it"
            :argument-hint "QUERY")
  (:tool "nyx_remember" *remember-schema* 'tool-remember)
  (:tool "nyx_recall" *recall-schema* 'tool-recall)
  (:tool "nyx_forget" *forget-schema* 'tool-forget)
  (:tool "nyx_restore" *restore-schema* 'tool-restore)
  (:start (lambda ()
            (ensure-store)
            (sweep-quarantine)
            (nle:on-stop (lambda () (sweep-quarantine))))))
