;;;; oath.lisp --- the oath file: parsing, matching, fingerprints, the ledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The port of the plugin's oath.py. An oath is a plain line file the owner
;;;; writes, outside the agent:
;;;;
;;;;   # my agent's oath
;;;;   standing mission: keep the owner's work moving without being asked twice
;;;;   never delete: ~/.nodecode/cells/notes/**
;;;;   never modify: ~/.nodecode/skills/**
;;;;   always require backup before modify: ~/.nodecode/skills/**
;;;;
;;;; Verbs are delete, modify, create and any. A pattern ending in /** covers a
;;;; whole tree, a pattern carrying * ? or [ is a glob, and any other pattern
;;;; is the path itself or anything under it. ~, $HOME and $NODECODE_HOME are
;;;; expanded. Nothing here reads the config, touches the network or runs a
;;;; process: the cell passes its settings in.

(in-package #:nodecode-oathkeeper)

(defparameter +verbs+ '("delete" "modify" "create" "any")
  "What a rule may forbid or require evidence before.")

;;; --- small text helpers ------------------------------------------------------

(defun trim-blanks (text)
  "TEXT without leading and trailing spaces, tabs and returns."
  (string-trim '(#\Space #\Tab #\Return) text))

(defun prefixed-p (prefix text)
  "Does TEXT start with PREFIX, case ignored?"
  (let ((n (length prefix)))
    (and (>= (length text) n) (string-equal prefix text :end2 n))))

(defun suffix-p (suffix text &optional (end (length text)))
  "Does TEXT end with SUFFIX at END?"
  (let ((n (length suffix)))
    (and (>= end n) (string= suffix text :start2 (- end n) :end2 end))))

(defun replace-text (old new text)
  "TEXT with every OLD replaced by NEW."
  (let ((out (make-string-output-stream)) (start 0) (n (length old)))
    (loop for at = (search old text :start2 start)
          while at
          do (write-string text out :start start :end at)
             (write-string new out)
             (setf start (+ at n)))
    (write-string text out :start start)
    (get-output-stream-string out)))

(defun split-text (text separator)
  "TEXT cut at every SEPARATOR character, empty pieces kept."
  (let ((out '()) (start 0))
    (loop for at = (position separator text :start start)
          while at
          do (push (subseq text start at) out)
             (setf start (1+ at)))
    (push (subseq text start) out)
    (nreverse out)))

;;; --- paths ------------------------------------------------------------------------

(defun user-home-text ()
  "The operator's home directory as a namestring ending in a slash."
  (namestring (user-homedir-pathname)))

(defun organism-home-text ()
  "The organism's own home as a namestring ending in a slash."
  (let ((home (ignore-errors (namestring (nlk:home)))))
    (or home (user-home-text))))

(defun cwd-text ()
  "The working directory a relative path in a call is read against."
  (or (ignore-errors (namestring (uiop:getcwd))) "/"))

(defun expand-path-text (text)
  "TEXT with ~, $HOME and $NODECODE_HOME expanded at its head."
  (let ((out text))
    (setf out (replace-text "${NODECODE_HOME}" (organism-home-text) out))
    (setf out (replace-text "${HOME}" (user-home-text) out))
    (setf out (replace-text "$NODECODE_HOME" (organism-home-text) out))
    (setf out (replace-text "$HOME" (user-home-text) out))
    (cond ((string= out "~") (user-home-text))
          ((prefixed-p "~/" out)
           (concatenate 'string (user-home-text) (subseq out 2)))
          (t out))))

(defun normalize-segments (text)
  "TEXT with repeated slashes collapsed and . and .. resolved."
  (let ((absolute (and (plusp (length text)) (char= (char text 0) #\/))) (parts '()))
    (dolist (part (split-text text #\/))
      (cond ((or (string= part "") (string= part ".")) nil)
            ((string= part "..")
             (if (and parts (not (string= (first parts) "..")))
                 (pop parts)
                 (unless absolute (push ".." parts))))
            (t (push part parts))))
    (format nil "~a~{~a~^/~}" (if absolute "/" "") (nreverse parts))))

(defun normalize-path (value)
  "VALUE as an absolute path, ~ and $HOME expanded and . and .. resolved."
  (let* ((text (expand-path-text (trim-blanks (princ-to-string value))))
         (absolute (and (plusp (length text)) (char= (char text 0) #\/))))
    (normalize-segments (if absolute text (concatenate 'string (cwd-text) text)))))

;;; --- matching ----------------------------------------------------------------

(defun globby-p (pattern)
  "Does PATTERN carry a wildcard? A trailing /** alone names a tree, not a glob."
  (let ((body (if (suffix-p "/**" pattern) (subseq pattern 0 (- (length pattern) 3)) pattern)))
    (some (lambda (character) (find character body)) "*?[")))

(defun char-class (pattern open character)
  "Does CHARACTER match the [...] class opening at OPEN in PATTERN?
=> (values MATCHED NEXT), NEXT the index after the closing bracket."
  (let* ((n (length pattern))
         (start (1+ open))
         (negated (and (< start n) (member (char pattern start) '(#\! #\^))))
         (index (if negated (1+ start) start))
         (matched nil)
         (closed nil))
    (loop while (and (< index n) (not closed))
          do (let ((item (char pattern index)))
               (cond ((char= item #\]) (setf closed t))
                     ((and (< (+ index 2) n) (char= (char pattern (1+ index)) #\-))
                      (when (<= (char-code (char pattern index)) (char-code character)
                                (char-code (char pattern (+ index 2))))
                        (setf matched t))
                      (incf index 3))
                     (t (when (char= item character) (setf matched t))
                        (incf index)))))
    (when (and closed negated) (setf matched (not matched)))
    (values (and closed matched) (if closed (1+ index) n))))

(defun glob-match (pattern text &optional (pattern-index 0) (text-index 0))
  "Does TEXT match PATTERN, * standing for any run (a / included) and ? for one
character?"
  (loop
    (cond ((>= pattern-index (length pattern)) (return (= text-index (length text))))
          ((char= (char pattern pattern-index) #\*)
           (return (or (glob-match pattern text (1+ pattern-index) text-index)
                       (and (< text-index (length text))
                            (glob-match pattern text pattern-index (1+ text-index))))))
          ((>= text-index (length text)) (return nil))
          ((char= (char pattern pattern-index) #\?)
           (incf pattern-index) (incf text-index))
          ((char= (char pattern pattern-index) #\[)
           (multiple-value-bind (matched next) (char-class pattern pattern-index (char text text-index))
             (if matched
                 (setf pattern-index next)
                 (return nil))
             (incf text-index)))
          ((char= (char pattern pattern-index) (char text text-index))
           (incf pattern-index) (incf text-index))
          (t (return nil)))))

(defun pattern-matches-p (pattern target)
  "Does the oath PATTERN cover TARGET, an absolute path?"
  (let ((pat (cond ((globby-p pattern) pattern)
                   ((suffix-p "/**" pattern) pattern)
                   (t (normalize-path pattern))))
        (tgt (normalize-path target)))
    (cond ((suffix-p "/**" pat)
           (let ((base (subseq pat 0 (- (length pat) 3))))
             (or (string= tgt base) (prefixed-p (concatenate 'string base "/") tgt))))
          ((globby-p pat) (glob-match pat tgt))
          (t (or (string= tgt pat) (prefixed-p (concatenate 'string pat "/") tgt))))))

(defun rule-matches-p (rule verb target)
  "Does RULE, read at VERB on TARGET, apply?"
  (and (member (getf rule :verb) (list verb "any") :test #'string=)
       (pattern-matches-p (getf rule :pattern) target)))

;;; --- parsing ----------------------------------------------------------------

(defun parse-oath-text (text)
  "TEXT as an oath.
=> (values MISSIONS RULES ERRORS); a rule is (:kind :verb :pattern [:evidence]
:line :raw), and an unreadable line is one entry of ERRORS, ignored."
  (let ((missions '()) (rules '()) (errors '()) (lineno 0))
    (dolist (raw (split-text (or text "") #\Newline))
      (incf lineno)
      (let ((line (trim-blanks raw)))
        (cond
          ((zerop (length line)) nil)
          ((char= (char line 0) #\#) nil)
          ((prefixed-p "standing mission:" line)
           (push (trim-blanks (subseq line (length "standing mission:"))) missions))
          ((prefixed-p "never " line)
           (let* ((rest (subseq line (length "never ")))
                  (colon (position #\: rest))
                  (verb (and colon (string-downcase (trim-blanks (subseq rest 0 colon)))))
                  (pattern (and colon (trim-blanks (subseq rest (1+ colon))))))
             (if (and verb (member verb +verbs+ :test #'string=) pattern (plusp (length pattern)))
                 (push (list :kind "never" :verb verb :pattern (expand-path-text pattern)
                             :line lineno :raw line)
                       rules)
                 (push (format nil "line ~d: cannot parse never rule: ~s" lineno line) errors))))
          ((prefixed-p "always require " line)
           (let* ((rest (subseq line (length "always require ")))
                  (at (search " before " (string-downcase rest)))
                  (evidence (and at (string-downcase (trim-blanks (subseq rest 0 at)))))
                  (tail (and at (subseq rest (+ at (length " before ")))))
                  (colon (and tail (position #\: tail)))
                  (verb (and colon (string-downcase (trim-blanks (subseq tail 0 colon)))))
                  (pattern (and colon (trim-blanks (subseq tail (1+ colon))))))
             (if (and evidence (plusp (length evidence)) verb (member verb +verbs+ :test #'string=)
                      pattern (plusp (length pattern)))
                 (push (list :kind "require" :verb verb :pattern (expand-path-text pattern)
                             :evidence evidence :line lineno :raw line)
                       rules)
                 (push (format nil "line ~d: cannot parse require rule: ~s" lineno line)
                       errors))))
          (t (push (format nil "line ~d: unrecognized oath line: ~s" lineno line) errors)))))
    (values (nreverse missions) (nreverse rules) (nreverse errors))))

(defun load-oath (path)
  "PATH as an oath: a plist of :EXISTS, :MISSIONS, :RULES and :ERRORS. An oath
that cannot be read is present but unusable, which the gate treats as a freeze."
  (if (not (uiop:file-exists-p path))
      (list :exists nil :missions '() :rules '() :errors '())
    (let ((text (nlk:read-text path)))
      (if (null text)
          (list :exists t :missions '() :rules '()
                :errors (list (format nil "oath file unreadable: ~a" path)))
          (multiple-value-bind (missions rules errors) (parse-oath-text text)
            (list :exists t :missions missions :rules rules :errors errors))))))

;;; --- the ledger and evidence ------------------------------------------------

(defun ledger-file (data-dir)
  "The append-only ledger under DATA-DIR."
  (merge-pathnames "ledger.jsonl" data-dir))

(defun append-ledger (data-dir &rest pairs)
  "Append one line to the ledger under DATA-DIR: time, then PAIRS."
  (ignore-errors
    (nlk:append-json-line (ledger-file data-dir)
                           (apply #'nlk:make-json-object
                                  (append (list "ts" (get-universal-time)) pairs)))))

(defun read-ledger (data-dir &key (limit 500))
  "The last LIMIT rows of the ledger under DATA-DIR, oldest first."
  (let ((rows (ignore-errors (nlk:read-json-lines (ledger-file data-dir)))))
    (if (> (length rows) limit) (last rows limit) rows)))

(defun row-field (row key)
  "ROW's member KEY as a string, or NIL."
  (let ((value (and (hash-table-p row) (gethash key row))))
    (and (stringp value) value)))

(defun tree-related-p (one other)
  "Are ONE and OTHER the same path, or is either under the other's tree?"
  (or (string= one other)
      (prefixed-p (concatenate 'string other "/") one)
      (prefixed-p (concatenate 'string one "/") other)))

(defun evidence-satisfied-p (data-dir session kind target)
  "Was evidence of KIND recorded in SESSION for TARGET or its tree?"
  (let ((found nil))
    (dolist (row (read-ledger data-dir :limit 500) found)
      (when (and (equal "evidence" (row-field row "event"))
                 (equal session (row-field row "session_id"))
                 (equal kind (row-field row "kind")))
        (let ((recorded (row-field row "target")))
          (when (and recorded (tree-related-p target (normalize-path recorded)))
            (setf found t)))))))

;;; --- fingerprints ------------------------------------------------------------

(defparameter *fnv-offset* (load-time-value (expt 2 63)))
(defparameter *fnv-prime* 1099511628211)
(defparameter *fnv-mask* (load-time-value (1- (expt 2 64))))

(defun file-digest (path limit)
  "PATH's content digest over at most LIMIT octets, as hex, or NIL when it
cannot be read."
  (handler-case
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (let ((hash *fnv-offset*) (read 0) (buffer (make-array 8192 :element-type '(unsigned-byte 8))))
          (loop while (< read limit)
                do (let ((n (read-sequence buffer in :end (min 8192 (- limit read)))))
                     (when (zerop n) (return))
                     (incf read n)
                     (dotimes (index n)
                       (setf hash (logand (* *fnv-prime* (logxor hash (aref buffer index)))
                                          *fnv-mask*)))))
          (format nil "~16,'0x" hash)))
    (error () nil)))

(defun fingerprint-of (path limit)
  "PATH's fingerprint as (SIZE MTIME DIGEST), the digest only under LIMIT bytes."
  (handler-case
      (let ((size (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in))))
        (list size (or (ignore-errors (file-write-date path)) 0)
              (and (<= size limit) (file-digest path limit))))
    (error () nil)))

(defun directory-files-under (dir max)
  "Every file under DIR, depth first, at most MAX of them."
  (let ((out '())
        (dir (uiop:ensure-directory-pathname dir)))
    (labels ((walk (folder)
               (dolist (file (ignore-errors (uiop:directory-files folder)))
                 (when (< (length out) max) (push (namestring file) out)))
               (dolist (sub (ignore-errors (uiop:subdirectories (uiop:ensure-directory-pathname folder))))
                 (when (< (length out) max) (walk sub)))))
      (when (uiop:directory-exists-p dir) (walk dir)))
    (nreverse out)))

(defun covered-files (pattern max)
  "The files PATTERN covers, at most MAX of them."
  (cond
    ((suffix-p "/**" pattern)
     (let ((base (subseq pattern 0 (- (length pattern) 3))))
       (cond ((uiop:directory-exists-p base) (directory-files-under base max))
             ((uiop:file-exists-p base) (list base))
             (t '()))))
    ((globby-p pattern)
     (let* ((slash (position #\/ pattern :from-end t))
            (folder (if slash (subseq pattern 0 slash) "."))
            (name (if slash (subseq pattern (1+ slash)) pattern)))
       (when (uiop:directory-exists-p folder)
         (mapcar #'namestring
                 (remove-if-not (lambda (file) (glob-match name (file-namestring file)))
                                (ignore-errors
                                 (uiop:directory-files (uiop:ensure-directory-pathname folder))))))))
    ((uiop:file-exists-p pattern) (list pattern))
    ((uiop:directory-exists-p pattern) (directory-files-under pattern max))
    (t '())))

(defun build-snapshot (rules oath-path hash-limit max-files)
  "A snapshot of every file the oath's patterns cover: (values OATH-DIGEST
FILES), FILES a hash table from path to (SIZE MTIME DIGEST)."
  (let ((files (make-hash-table :test #'equal)) (patterns '()))
    (dolist (rule rules)
      (let ((pattern (getf rule :pattern)))
        (unless (member pattern patterns :test #'string=) (push pattern patterns))))
    (dolist (pattern (nreverse patterns))
      (dolist (file (covered-files pattern max-files))
        (unless (gethash file files)
          (let ((fingerprint (fingerprint-of file hash-limit)))
            (when fingerprint (setf (gethash file files) fingerprint))))))
    (values (and (uiop:file-exists-p oath-path) (file-digest oath-path hash-limit)) files)))

(defun snapshot-file (data-dir)
  "The snapshot beside the ledger under DATA-DIR."
  (merge-pathnames "snapshot.jsonl" data-dir))

(defun save-snapshot (data-dir oath-digest files)
  "Write FILES and OATH-DIGEST, the state the next diff is read against."
  (handler-case
      (let ((lines (list (nlk:encode-json-object (nlk:make-json-object "oath" (or oath-digest ""))))))
        (maphash (lambda (path fingerprint)
                   (push (nlk:encode-json-object
                          (nlk:make-json-object "path" path
                                                 "size" (first fingerprint)
                                                 "mtime" (second fingerprint)
                                                 "digest" (or (third fingerprint) "")))
                         lines))
                 files)
        (nlk:write-file-atomically (snapshot-file data-dir)
                                   (format nil "~{~a~%~}" (nreverse lines))))
    (error () nil)))

(defun load-snapshot (data-dir)
  "The snapshot under DATA-DIR as (values OATH-DIGEST FILES), or NIL when there
is none."
  (let ((rows (ignore-errors (nlk:read-json-lines (snapshot-file data-dir)))))
    (when rows
      (let ((files (make-hash-table :test #'equal))
            (oath nil))
        (dolist (row rows)
          (let ((path (row-field row "path")))
            (cond (path (setf (gethash path files)
                              (list (nlk:json-value row :integer "size")
                                    (nlk:json-value row :integer "mtime")
                                    (let ((digest (row-field row "digest")))
                                      (and digest (plusp (length digest)) digest)))))
                  (t (setf oath (let ((value (and (hash-table-p row) (gethash "oath" row))))
                                   (and (stringp value) value)))))))
        (list oath files)))))

(defun diff-snapshots (old new)
  "The changes between two snapshots, as a list of (CHANGE PATH): added,
removed, modified or oath_file_changed."
  (let ((changes '())
        (before (second old))
        (after (second new)))
    (maphash (lambda (path fingerprint)
               (declare (ignore fingerprint))
               (unless (gethash path before) (push (list "added" path) changes)))
             after)
    (maphash (lambda (path fingerprint)
               (declare (ignore fingerprint))
               (unless (gethash path after) (push (list "removed" path) changes)))
             before)
    (maphash (lambda (path fingerprint)
               (let ((was (gethash path before)))
                 (when (and was
                            (cond ((and (third was) (third fingerprint))
                                   (not (string= (third was) (third fingerprint))))
                                  (t (or (/= (first was) (first fingerprint))
                                         (/= (second was) (second fingerprint))))))
                   (push (list "modified" path) changes))))
             after)
    (unless (equal (first old) (first new))
      (push (list "oath_file_changed" "oath") changes))
    (nreverse changes)))
