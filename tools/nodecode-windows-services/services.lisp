;;;; services.lisp --- the rows, the filters over them, and the text a model
;;;; reads back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A row is the one shape everything below agrees on: a plist built from
;;;; one decoded service, with the delayed flag folded into its start mode the
;;;; way the Services window shows it, its dependency edges spelled out, and
;;;; who waits for it folded out of everyone else's edges. Nothing here runs a
;;;; child process, so the filters, the numbers and the layout are provable
;;;; without Windows.

(in-package #:nodecode-windows-services)

(defparameter +binary-chars+ 160
  "How much of a registered binary path a row carries.")

(defparameter +message-chars+ 240
  "How much of one event-log message, or of a description, the detail carries.")

(defparameter +states+
  '("running" "stopped" "paused" "start pending" "stop pending"
    "continue pending" "pause pending" "unknown")
  "The states a filter may name, as Windows spells them.")

(defparameter +start-modes+ '("boot" "system" "auto" "manual" "disabled")
  "The start modes a filter may name; a delayed Automatic service matches auto.")

;;; --- rows out of the decoded listing -----------------------------------------

(defun json-text (object key)
  "KEY of OBJECT when it is a string."
  (nlk:json-value object :string key))

(defun dependencies-of (object)
  "The dependency edges OBJECT carries, a load-order group spelled out."
  (loop for item across (nlk:json-array object "depends_on")
        for text = (and (stringp item) (nlk:trimmed item))
        when (plusp (length text))
          collect (if (char= (char text 0) #\+)
                      (concatenate 'string (subseq text 1) " (group)")
                      text)))

(defun row-of (object)
  "One decoded service as the plist every renderer below reads."
  (let* ((state (string-downcase (or (json-text object "state") "unknown")))
         (start (string-downcase (or (json-text object "start") "unknown")))
         (pid (nlk:json-value object :integer "pid")))
    (list :name (json-text object "name")
          :display (json-text object "display")
          :state state
          :start (if (nlk:json-value object :boolean "delayed")
                     (concatenate 'string start ", delayed")
                     start)
          :pid (and pid (plusp pid) pid)
          :exit-code (nlk:json-value object :integer "exit_code")
          :account (json-text object "account")
          :binary (clip (json-text object "binary") +binary-chars+)
          :description (json-text object "description")
          :depends-on (dependencies-of object)
          :dependents nil
          :failure-actions (nlk:json-array object "failure_actions"))))

(defun edge-key (target)
  "TARGET as the name a dependency edge points at, a group suffix dropped."
  (let ((text (string-downcase (nlk:trimmed target)))
        (suffix " (group)"))
    (if (and (> (length text) (length suffix))
             (string= suffix text :start2 (- (length text) (length suffix))))
        (subseq text 0 (- (length text) (length suffix)))
        text)))

(defun rows-with-edges (&optional (objects (fetch-services)))
  "Every service as a row, each carrying who waits for it as well as whom it
waits for: the reverse edges are folded out of everyone else's edges."
  (let ((rows (mapcar (function row-of) objects))
        (waiters (make-hash-table :test (function equal))))
    (dolist (row rows)
      (dolist (target (getf row :depends-on))
        (push (getf row :name) (gethash (edge-key target) waiters))))
    (dolist (row rows)
      (setf (getf row :dependents)
            (reverse (gethash (string-downcase (or (getf row :name) "")) waiters))))
    rows))

;;; --- the filters --------------------------------------------------------------

(defun matching (rows &key query state start nonzero-exit)
  "ROWS the filters keep: a substring of the name or display, an exact state,
the start mode ROWS' own begins with, and failures only."
  (let ((query (string-downcase (nlk:trimmed (or query ""))))
        (state (string-downcase (nlk:trimmed (or state ""))))
        (start (string-downcase (nlk:trimmed (or start "")))))
    (remove-if-not
     (lambda (row)
       (and (or (zerop (length query))
                (search query (string-downcase (or (getf row :name) "")))
                (search query (string-downcase (or (getf row :display) ""))))
            (or (zerop (length state)) (equal state (getf row :state)))
            (or (zerop (length start)) (eql 0 (search start (getf row :start))))
            (or (not nonzero-exit) (failed-p row))))
     rows)))

(defun find-row (name rows)
  "The row whose name is NAME, case-insensitively."
  (find name rows :key (lambda (row) (getf row :name)) :test (function string-equal)))

(defun near-rows (name rows)
  "The names of services whose name or display contains NAME, up to five."
  (let ((name (string-downcase (nlk:trimmed (or name ""))))
        (found '()))
    (dolist (row rows)
      (when (and (< (length found) 5)
                 (or (search name (string-downcase (or (getf row :name) "")))
                     (search name (string-downcase (or (getf row :display) "")))))
        (push (getf row :name) found)))
    (nreverse found)))

;;; --- the binary a registration names ---------------------------------------

(defun expand-home (path)
  "PATH with a leading ~ as the home directory."
  (if (and (plusp (length path)) (char= (char path 0) #\~)
           (or (= (length path) 1) (find (char path 1) "/\\")))
      (let ((home (or (uiop:getenv "USERPROFILE") (uiop:getenv "HOME") "")))
        (concatenate 'string (string-right-trim "/\\" home) (subseq path 1)))
      path))

(defun expand-variables (path)
  "PATH with every %NAME% replaced by the environment's NAME; one the
environment does not set stays as it was."
  (with-output-to-string (out)
    (loop with start = 0
          for open = (position #\% path :start start)
          for close = (and open (position #\% path :start (1+ open)))
          do (cond ((null close)
                    (write-string (subseq path start) out)
                    (return))
                   (t (write-string (subseq path start open) out)
                      (let ((value (uiop:getenv (subseq path (1+ open) close))))
                        (write-string (or value (subseq path open (1+ close))) out))
                      (setf start (1+ close)))))))

(defun binary-path (binary)
  "The executable BINARY's command line names: a quoted path, else its first
token, with %VARIABLES% and ~ spelled out."
  (when binary
    (let* ((text (nlk:trimmed binary))
           (path (cond ((and (plusp (length text)) (char= (char text 0) #\"))
                        (let ((end (position #\" text :start 1)))
                          (if end (subseq text 1 end) (string-trim "\"" text))))
                       (t (let ((space (position #\Space text)))
                            (if space (subseq text 0 space) text))))))
      (expand-variables (expand-home path)))))

(defun absolute-pathname-p (path)
  "Whether PATH is a native absolute path: a drive letter, a UNC share, or a
leading slash."
  (and (plusp (length path))
       (or (char= (char path 0) #\/)
           (char= (char path 0) #\\)
           (and (> (length path) 2)
                (alpha-char-p (char path 0))
                (char= (char path 1) #\:)
                (find (char path 2) "\\/")))))

(defun binary-state (binary)
  "Whether the executable BINARY's registration names is still on disk, as
(:path PATH :present BOOL :note TEXT)."
  (let ((path (binary-path binary)))
    (when (and path (plusp (length path)))
      (if (absolute-pathname-p path)
          (let ((present (and (ignore-errors (probe-file (uiop:parse-native-namestring path))) t)))
            (list :path path :present present
                  :note (unless present "the registered binary is not on disk any more")))
          (list :path path :present nil :note "the path is not absolute; not checked")))))

;;; --- the listing a model reads ------------------------------------------------

(defun column (text width)
  "TEXT in a column WIDTH wide, cut when longer."
  (let ((text (or text "")))
    (if (<= (length text) (1- width))
        (concatenate 'string text (make-string (- width (length text)) :initial-element #\Space))
        (concatenate 'string (subseq text 0 (- width 2)) ". "))))

(defun row-annotation (row)
  "What ROW has to say beyond its columns, or NIL."
  (let ((notes '()))
    (when (failed-p row)
      (let ((exit (exit-of (getf row :exit-code))))
        (push (format nil "exit ~a (~a) - ~a" (getf exit :code) (getf exit :hex) (getf exit :meaning))
              notes)))
    (when (getf row :depends-on)
      (push (format nil "waits for ~{~a~^, ~}" (getf row :depends-on)) notes))
    (when (getf row :dependents)
      (push (format nil "waited on by ~{~a~^, ~}" (getf row :dependents)) notes))
    (when notes (format nil "    ~{~a~^; ~}" (nreverse notes)))))

(defun render-row (row)
  "ROW as one line, and a second only when there is something to add."
  (format nil "~a~a~a~a~a~a~@[~%~a~]"
          (column (getf row :state) 9)
          (column (getf row :start) 17)
          (column (getf row :name) 26)
          (column (if (getf row :pid) (princ-to-string (getf row :pid)) "-") 7)
          (column (getf row :account) 16)
          (or (getf row :binary) "")
          (row-annotation row)))

(defun table-notes (rows candidates listed limit state start failing running)
  "The lines above the table: what was asked, and what was left out."
  (let ((notes (list (format nil "~d of ~d matching service(s) listed, ~d registered, ~d running"
                             (length listed) (length candidates) (length rows) running))))
    (when (> (length candidates) (length listed))
      (push (format nil "stopped at limit=~d; raise it or narrow the query" limit) notes))
    (when (plusp failing)
      (push (format nil "~d service(s) carry a non-zero exit code; nonzero_exit=true shows them" failing)
            notes))
    (unless listed
      (push "nothing matched; drop the query or the filters" notes))
    (when (and (plusp (length state)) (not (member state +states+ :test (function equal))))
      (push (format nil "state is one of: ~{~a~^, ~}" +states+) notes))
    (when (and (plusp (length start)) (not (member start +start-modes+ :test (function equal))))
      (push (format nil "start is one of: ~{~a~^, ~}" +start-modes+) notes))
    (nreverse notes)))

(defun table-text (rows &key (limit 100) query state start nonzero-exit)
  "The listing over ROWS with the filters named, as the text the model reads."
  (let* ((candidates (matching rows :query query :state state :start start
                               :nonzero-exit nonzero-exit))
         (listed (if (> (length candidates) limit) (subseq candidates 0 limit) candidates))
         (running (count-if (lambda (row) (equal "running" (getf row :state))) rows))
         (failing (count-if (function failed-p) rows))
         (state (string-downcase (nlk:trimmed (or state ""))))
         (start (string-downcase (nlk:trimmed (or start "")))))
    (with-output-to-string (out)
      (dolist (line (table-notes rows candidates listed limit state start failing running))
        (format out "~a~%" line))
      (when listed
        (format out "~%~a~a~a~a~a~a~%"
                (column "state" 9) (column "start" 17) (column "name" 26)
                (column "pid" 7) (column "account" 16) "binary"))
      (dolist (row listed)
        (format out "~a~%" (render-row row))))))

;;; --- one service, in full ---------------------------------------------------

(defun event-line (object)
  "One Service Control Manager event as a line."
  (let ((id (nlk:json-value object :integer "id")))
    (format nil "~a ~a ~a: ~a"
            (or (nlk:json-value object :string "time") "unknown time")
            (or id "?")
            (sc-event-of id)
            (or (clip (nlk:json-value object :string "what") +message-chars+) ""))))

(defun detail-text (name &key (rows (rows-with-edges)) (events (fetch-events name)))
  "Service NAME in full: its state, its registered binary, the failure policy
Windows will apply, and the Service Control Manager's own recent record."
  (let ((row (find-row name rows)))
    (unless row
      (return-from detail-text
        (format nil "no service named ~a is registered~@[; closest: ~{~a~^, ~}~]. ~
                     (windows-services:table) lists every registered service."
                name (near-rows name rows))))
    (let* ((exit (exit-of (getf row :exit-code)))
           (binary (binary-state (getf row :binary)))
           (policy (decode-failure-actions (getf row :failure-actions)))
           (notes '()))
      (when (and binary (not (getf binary :present))
                 (search "not on disk" (or (getf binary :note) "")))
        (push (getf binary :note) notes))
      (unless policy
        (push "no failure action is configured: Windows leaves this service dead when it fails"
              notes))
      (with-output-to-string (out)
        (format out "~a~@[ - ~a~]~%" (getf row :name) (getf row :display))
        (format out "state: ~a   start: ~a~@[   pid: ~a~]~%"
                (getf row :state) (getf row :start) (getf row :pid))
        (format out "account: ~a~%" (or (getf row :account) "-"))
        (format out "binary: ~a~@[ (~a)~]~%"
                (or (getf row :binary) "-")
                (or (and binary (getf binary :path)) "not reported"))
        (format out "exit code: ~a~%"
                (if exit
                    (format nil "~a (~a) - ~a" (getf exit :code) (getf exit :hex)
                            (getf exit :meaning))
                    "not reported"))
        (format out "waits for: ~@[~{~a~^, ~}~]~%" (getf row :depends-on))
        (format out "waited on by: ~@[~{~a~^, ~}~]~%" (getf row :dependents))
        (format out "failure policy: ~a~%" (policy-text policy))
        (format out "~@[description: ~a~%~]" (clip (getf row :description) +message-chars+))
        (dolist (note (nreverse notes))
          (format out "note: ~a~%" note))
        (when events
          (format out "~&recent events, newest first:~%")
          (dolist (event events)
            (format out "  ~a~%" (event-line event))))))))
