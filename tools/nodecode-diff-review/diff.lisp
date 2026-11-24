;;;; diff.lisp --- the rules a line trips, and the parser that finds the lines.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here runs git or reads a directory: one string in, one COLLECTOR
;;;; out. The original plugin had the same split, and for the same reason --
;;;; the rules and the hunk accounting are what can be wrong, and a canned
;;;; diff is what proves them.

(in-package #:nodecode-diff-review)

;;; --- the rules ---------------------------------------------------------------

(defparameter +secret-names+
  "(?:password|passwd|secret|secret[_-]?key|api[_-]?key|access[_-]?key|private[_-]?key|token)"
  "The names that, given a value, are a credential.")

(defparameter +secret-quoted+
  (list (cl-ppcre:create-scanner
         (concatenate 'string
                      "(?i)(?<![\\w-])[\"']?[\\w.-]*?" +secret-names+ "(?![\\w-])[\"']?\\s*"
                      "(?::\\s*[\\w\\[\\]., |]+?\\s*)?(?:=|:=|=>|:)\\s*\"(?!\")[^\"\\n]+\""))
        (cl-ppcre:create-scanner
         (concatenate 'string
                      "(?i)(?<![\\w-])[\"']?[\\w.-]*?" +secret-names+ "(?![\\w-])[\"']?\\s*"
                      "(?::\\s*[\\w\\[\\]., |]+?\\s*)?(?:=|:=|=>|:)\\s*'(?!')[^'\\n]+'")))
  "A credential name assigned a quoted value, in either kind of quote:
token = \"...\", api_key: '...'.")

(defparameter +secret-unquoted+
  (cl-ppcre:create-scanner
   (concatenate 'string
                "(?i)^\\s*(?:export\\s+)?[\\w.-]*?" +secret-names+ "(?![\\w-])"
                "(?:=(?=\\S)|\\s*:\\s+)"
                "(?!(?:null|none|true|false|yes|no|~)\\s*$)"
                "([^\\s\"'#$<{(\\[][^\\s#()\\[\\]{}]*)\\s*(?:#.*)?$"))
  "A .env or YAML line: SECRET=value, TOKEN: value, with the value as its one
register.")

(defparameter +todo+ (cl-ppcre:create-scanner "\\b(?:TODO|FIXME|HACK|XXX)\\b")
  "An unresolved marker.")

(defparameter +bare-except+ (cl-ppcre:create-scanner "\\bexcept\\s*:")
  "A Python except with no type: it swallows everything, KeyboardInterrupt too.")

(defparameter +broad-except+
  (cl-ppcre:create-scanner
   "\\bexcept\\*?\\s*\\(?\\s*(?:[\\w.]+\\s*,\\s*)*(?:Base)?Exception\\b")
  "A Python except that catches everything Exception covers.")

(defparameter +debug-print+ (cl-ppcre:create-scanner "(?<![\\w.])p?print\\s*\\(")
  "A print( left in the code. The lookbehind keeps out pprint, sprint and a
method named print on an object.")

(defparameter +console+ (cl-ppcre:create-scanner "\\bconsole\\.(log|debug|info|warn|error|trace)\\s*\\(")
  "A JavaScript console call, its method as the one register.")

(defun looks-like-credential-p (value)
  "Whether VALUE, the right side of an unquoted assignment, is long or mixed
enough to be a credential rather than a word: the original's test."
  (and (stringp value)
       (>= (length value) 4)
       (or (some #'digit-char-p value) (>= (length value) 16))))

(defun unquoted-secret-p (text)
  "Whether TEXT is a .env or YAML line assigning a credential."
  (multiple-value-bind (match registers)
      (cl-ppcre:scan-to-strings +secret-unquoted+ text)
    (and match (looks-like-credential-p (and registers (aref registers 0))))))

(defun console-issue (text)
  "The warn label for a JavaScript console call in TEXT, or NIL."
  (cl-ppcre:register-groups-bind (method) (+console+ text)
    (format nil "console.~a statement" method)))

(defun line-issues (text)
  "Every label TEXT trips, in the order the original plugin reported them."
  (remove nil
          (list (and (cl-ppcre:scan +debug-print+ text) "debug print statement")
                (console-issue text)
                (and (or (some (lambda (scanner) (cl-ppcre:scan scanner text)) +secret-quoted+)
                         (unquoted-secret-p text))
                     "possible hardcoded secret")
                (and (cl-ppcre:scan +bare-except+ text) "bare except clause")
                (and (cl-ppcre:scan +broad-except+ text) "broad Exception catch")
                (and (cl-ppcre:scan +todo+ text) "unresolved TODO/FIXME"))))

;;; --- what a message is allowed to carry ----------------------------------------

(defparameter +redactions+
  '(("\\bgh[pousr]_[A-Za-z0-9]{8,}" . "***")
    ("\\bgithub_pat_[A-Za-z0-9_]{8,}" . "***")
    ("\\bsk-[A-Za-z0-9_-]{8,}" . "***")
    ("\\bxox[abprs]-[A-Za-z0-9-]{8,}" . "***")
    ("\\bAKIA[0-9A-Z]{16}\\b" . "***")
    ("\\bAIza[0-9A-Za-z_-]{16,}" . "***")
    ("(?i)\\bbearer\\s+[A-Za-z0-9._~+/=-]+" . "Bearer ***"))
  "What a credential-shaped run of text becomes in a message this cell answers:
the original's redactions, so a refused ref cannot spill a token.")

(defun redact (text)
  "TEXT with a URL's userinfo and every credential-shaped run masked."
  (let ((text (cl-ppcre:regex-replace-all
               "(://)[^/\\s@]+@" text
               (lambda (match &rest registers)
                 (declare (ignore match registers))
                 "://***@"))))
    (dolist (rule +redactions+ text)
      (setf text (cl-ppcre:regex-replace-all (car rule) text (cdr rule))))))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying nothing about the cut."
  (if (<= (length text) limit) text (concatenate 'string (subseq text 0 (1- limit)) "…")))

(defun shown (value)
  "VALUE as a message shows it: clipped and redacted."
  (clip (redact (if (stringp value) value (princ-to-string value))) 80))

;;; --- a file in the diff ----------------------------------------------------------

(defstruct (file-state (:constructor make-file-state))
  "One file's entry of a diff, as its header lines and hunks add up."
  (hdr-old nil) (hdr-new nil) (old nil) (new nil)
  (seen-old nil) (seen-new nil)
  (rename-from nil) (rename-to nil)
  (copied nil) (deleted nil) (created nil) (mode-change nil) (binary nil)
  (hunks 0) (added 0) (removed 0))

(defun strip-prefix (path prefix)
  "PATH without a leading PREFIX."
  (if (and path (uiop:string-prefix-p prefix path)) (subseq path (length prefix)) path))

(defun file-sides (state)
  "STATE's old and new paths, the a/ and b/ prefixes taken off when both
sides carry them."
  (let ((old (if (file-state-seen-old state) (file-state-old state) (file-state-hdr-old state)))
        (new (if (file-state-seen-new state) (file-state-new state) (file-state-hdr-new state))))
    (if (and (or old new)
             (or (null old) (uiop:string-prefix-p "a/" old))
             (or (null new) (uiop:string-prefix-p "b/" new)))
        (values (strip-prefix old "a/") (strip-prefix new "b/"))
        (values old new))))

(defun file-path (state)
  "The path STATE is reported under: the new one where the file still exists."
  (multiple-value-bind (old new) (file-sides state)
    (or new (file-state-rename-to state) old (file-state-rename-from state) "unknown")))

(defun file-status (state)
  "How STATE's file changed, in one word."
  (cond ((or (file-state-deleted state)
             (and (file-state-seen-new state) (null (file-state-new state))))
         "deleted")
        ((or (file-state-created state)
             (and (file-state-seen-old state) (null (file-state-old state))))
         "added")
        ((file-state-rename-to state)
         (if (file-state-copied state) "copied" "renamed"))
        ((file-state-binary state) "binary")
        ((and (file-state-mode-change state) (zerop (file-state-hunks state))) "mode_changed")
        (t "modified")))

(defun file-entry (state)
  "STATE as one JSON object of the answer."
  (let ((status (file-status state)))
    (nlk:json-object
     "path" (file-path state)
     "added" (file-state-added state)
     "removed" (file-state-removed state)
     "status" status
     :opt "old_path" (and (member status '("renamed" "copied") :test #'equal)
                          (or (file-state-rename-from state)
                              (nth-value 0 (file-sides state))))
     :when (file-state-binary state) "binary" t)))

;;; --- paths as git writes them ----------------------------------------------------

(defparameter +c-escapes+
  '((#\a . 7) (#\b . 8) (#\t . 9) (#\n . 10) (#\v . 11) (#\f . 12) (#\r . 13)
    (#\" . 34) (#\\ . 92))
  "The one-character escapes git's C-quoting uses.")

(defun unquote-path (text)
  "TEXT, a git C-quoted path, read back => (values PATH REST), REST what
follows the closing quote."
  (let ((out (make-string-output-stream))
        (index 1)
        (length (length text)))
    (loop while (< index length)
          do (let ((char (char text index)))
               (cond ((char= char #\")
                      (return-from unquote-path (values (get-output-stream-string out)
                                                        (subseq text (1+ index)))))
                     ((and (char= char #\\) (< (1+ index) length))
                      (let ((next (char text (1+ index))))
                        (if (and (<= (+ index 3) length)
                                 (every (lambda (c) (digit-char-p c 8))
                                        (subseq text (1+ index) (+ index 4))))
                            (progn (write-char (code-char (parse-integer (subseq text (1+ index) (+ index 4))
                                                                         :radix 8))
                                               out)
                                   (incf index 4))
                            (let ((escape (cdr (assoc next +c-escapes+))))
                              (if escape
                                  (progn (write-char (code-char escape) out) (incf index 2))
                                  (progn (write-char next out) (incf index 2)))))))
                     (t (write-char char out) (incf index)))))
    (values (get-output-stream-string out) "")))

(defun plain-path (text)
  "TEXT as a path, unquoted when git quoted it."
  (if (and (plusp (length text)) (char= #\" (char text 0)))
      (unquote-path text)
      text))

(defun header-path (text)
  "The path of a --- or +++ line body, or NIL for /dev/null."
  (let ((path (if (and (plusp (length text)) (char= #\" (char text 0)))
                  (unquote-path text)
                  (subseq text 0 (or (position #\Tab text) (length text))))))
    (unless (equal path "/dev/null") path)))

(defun git-header-paths (text)
  "The old and new paths of a `diff --git' line's body => (values OLD NEW)."
  (if (and (plusp (length text)) (char= #\" (char text 0)))
      (multiple-value-bind (old rest) (unquote-path text)
        (let ((rest (if (and (plusp (length rest)) (char= #\Space (char rest 0)))
                        (subseq rest 1)
                        rest)))
          (values old (and (plusp (length rest)) (plain-path rest)))))
      (let ((at (search " b/" text)))
        (if at
            (values (subseq text 0 at) (subseq text (1+ at)))
            (values text nil)))))

;;; --- the collector: files, warnings, counts ---------------------------------------

(defparameter *max-warnings* 200
  "How many warnings one answer lists; the count still says how many were found.")
(defparameter *max-files* 500
  "How many files one answer lists; the count still says how many were seen.")

(defstruct (collector (:constructor make-collector))
  "What reading a diff adds up to: the files and warnings it lists, and the
counts past what it lists."
  (files '())
  (warnings '())
  (warning-count 0)
  (total-added 0)
  (total-removed 0)
  (notes '()))

(defun added-line (out path number text)
  "Account for one added line of PATH at NUMBER, and every rule TEXT trips."
  (incf (collector-total-added out))
  (dolist (issue (line-issues text))
    (incf (collector-warning-count out))
    (when (< (length (collector-warnings out)) *max-warnings*)
      (push (nlk:json-object "file" path "line" number "issue" issue)
            (collector-warnings out)))))

;;; --- one hunk ---------------------------------------------------------------------

(defstruct (hunk (:constructor make-hunk (width old-left new-left new-line)))
  "A hunk in progress: WIDTH columns of prefix (1 for a plain diff, one per
parent in a combined one), OLD-LEFT the lines each parent still has, NEW-LEFT
the lines the result still has, NEW-LINE the number the next added line gets."
  (width 1 :type (integer 1))
  (old-left nil)
  (new-left 0 :type integer)
  (new-line 1 :type integer))

(defun hunk-done-p (hunk)
  "Whether every side of HUNK has been accounted for."
  (and (<= (hunk-new-left hunk) 0) (every (lambda (n) (<= n 0)) (hunk-old-left hunk))))

(defun hunk-consume (hunk line state out)
  "Account for LINE in HUNK => T when LINE belongs to the hunk."
  (when (and (plusp (length line)) (char= #\\ (char line 0)))
    (return-from hunk-consume t))
  (when (hunk-done-p hunk)
    (return-from hunk-consume nil))
  (let* ((width (hunk-width hunk))
         (prefix (if (>= (length line) width)
                     (subseq line 0 width)
                     (concatenate 'string line (make-string (- width (length line))
                                                            :initial-element #\Space)))))
    (unless (every (lambda (c) (member c '(#\Space #\+ #\-))) prefix)
      (return-from hunk-consume nil))
    (let ((present (if (find #\- prefix) #\- #\Space)))
      (loop for column from 0 below width
            when (char= (char prefix column) present)
              do (decf (aref (hunk-old-left hunk) column)))
      (cond ((char= present #\-)
             (incf (file-state-removed state))
             (incf (collector-total-removed out))
             t)
            (t
             (when (find #\+ prefix)
               (incf (file-state-added state))
               (added-line out (file-path state) (hunk-new-line hunk)
                           (if (>= (length line) width) (subseq line width) "")))
             (decf (hunk-new-left hunk))
             (incf (hunk-new-line hunk))
             t)))))

(defun start-hunk (line state)
  "The hunk LINE's header opens on STATE, or NIL when it opens none."
  (declare (ignore state))
  (multiple-value-bind (match registers)
      (cl-ppcre:scan-to-strings "^@@ -(\\d+)(?:,(\\d+))? \\+(\\d+)(?:,(\\d+))? @@" line)
    (when match
      (return-from start-hunk
        (make-hunk 1
                   (vector (if (aref registers 1) (parse-integer (aref registers 1)) 1))
                   (if (aref registers 3) (parse-integer (aref registers 3)) 1)
                   (parse-integer (aref registers 2))))))
  (multiple-value-bind (match registers)
      (cl-ppcre:scan-to-strings "^(@{3,}) (.*?) \\1" line)
    (when match
      (let* ((ranges (cl-ppcre:split "\\s+" (aref registers 1)))
             (olds (remove-if-not (lambda (r) (and (plusp (length r)) (char= #\- (char r 0)))) ranges))
             (news (remove-if-not (lambda (r) (and (plusp (length r)) (char= #\+ (char r 0)))) ranges)))
        (when (and (= (length olds) (1- (length (aref registers 0))))
                   (= (length news) 1)
                   (plusp (length (aref registers 1))))
          (flet ((count-of (range)
                   (let ((comma (position #\, range)))
                     (if comma (parse-integer range :start (1+ comma)) 1)))
                 (start-of (range)
                   (parse-integer range :start 1 :end (or (position #\, range) (length range)))))
            (make-hunk (length olds)
                       (map 'vector #'count-of olds)
                       (count-of (first news))
                       (start-of (first news)))))))))

;;; --- the parser ---------------------------------------------------------------------

(defun split-lines (text)
  "TEXT's lines: a trailing newline ends the last one rather than adding an
empty one, and a CR before a newline is not part of the line."
  (let ((lines '())
        (start 0)
        (length (length text)))
    (loop
      (let ((at (position #\Newline text :start start)))
        (push (subseq text start at) lines)
        (if at
            (setf start (1+ at))
            (return (nreverse lines)))))))

(defun strip-cr (line)
  "LINE without the CR of a CRLF ending."
  (if (and (plusp (length line)) (char= #\Return (char line (1- (length line)))))
      (subseq line 0 (1- (length line)))
      line))

(defun parse-diff (text out)
  "Read TEXT, a unified, git or combined diff, into OUT => how many files it
held. An empty result is zero files, which the caller reports as no diff or
as text that was never a diff."
  (let ((state nil)
        (hunk nil)
        (seen 0))
    (labels ((finish ()
               (when state
                 (push (file-entry state) (collector-files out))
                 (incf seen)
                 (setf state nil)))
             (step-line (line)
               (when hunk
                 (if (hunk-consume hunk line state out)
                     (return-from step-line t)
                     (setf hunk nil)))
               (cond
                 ((uiop:string-prefix-p "diff --git " line)
                  (finish)
                  (setf state (make-file-state))
                  (multiple-value-bind (old new) (git-header-paths (subseq line 11))
                    (setf (file-state-hdr-old state) old
                          (file-state-hdr-new state) new)))
                 ((or (uiop:string-prefix-p "diff --cc " line)
                      (uiop:string-prefix-p "diff --combined " line))
                  (finish)
                  (setf state (make-file-state)
                        (file-state-hdr-new state)
                        (plain-path (subseq line (if (uiop:string-prefix-p "diff --cc " line) 10 16)))))
                 ((uiop:string-prefix-p "@@" line)
                  (unless state (setf state (make-file-state)))
                  (let ((started (start-hunk line state)))
                    (when started
                      (incf (file-state-hunks state))
                      (setf hunk started))))
                 ((uiop:string-prefix-p "--- " line)
                  (when (or (null state) (plusp (file-state-hunks state))
                            (file-state-seen-old state))
                    (finish)
                    (setf state (make-file-state)))
                  (setf (file-state-old state) (header-path (subseq line 4))
                        (file-state-seen-old state) t))
                 ((uiop:string-prefix-p "+++ " line)
                  (unless state (setf state (make-file-state)))
                  (setf (file-state-new state) (header-path (subseq line 4))
                        (file-state-seen-new state) t))
                 ((null state) nil)
                 ((or (uiop:string-prefix-p "old mode " line)
                      (uiop:string-prefix-p "new mode " line))
                  (setf (file-state-mode-change state) t))
                 ((uiop:string-prefix-p "deleted file mode " line)
                  (setf (file-state-deleted state) t))
                 ((uiop:string-prefix-p "new file mode " line)
                  (setf (file-state-created state) t))
                 ((or (uiop:string-prefix-p "rename from " line)
                      (uiop:string-prefix-p "copy from " line))
                  (setf (file-state-rename-from state) (plain-path (subseq line 12))
                        (file-state-copied state) (uiop:string-prefix-p "copy from " line)))
                 ((or (uiop:string-prefix-p "rename to " line)
                      (uiop:string-prefix-p "copy to " line))
                  (setf (file-state-rename-to state) (plain-path (subseq line 10))))
                 ((or (uiop:string-prefix-p "Binary files " line)
                      (uiop:string-prefix-p "GIT binary patch" line))
                  (setf (file-state-binary state) t)))))
      (dolist (line (split-lines text))
        (step-line (strip-cr line)))
      (finish))
    seen))
