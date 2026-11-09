;;;; spec.lisp --- the bounded receipt spec, and the digest over it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A spec names what would count as done: a workspace, and one to twenty
;;;; checks of three kinds. It is validated once, on the way in, and what
;;;; comes out is the canonical plist every later step reads -- the same
;;;; one whose canonical text the criteria digest hashes, so a receipt can
;;;; say which criteria it judged.

(in-package #:nodecode-receipts)

(defparameter +spec-keys+ '("schema_version" "task_id" "workspace_root" "checks" "allow_loopback_http")
  "The only members a spec may carry.")

(defun check-id-p (value)
  "Whether VALUE is a short validated id, never a path."
  (and (stringp value)
       (<= 1 (length value) 64)
       (not (search ".." value))
       (let ((first (char value 0)))
         (and (alphanumericp first)
              (< (char-code first) 128)
              (every (lambda (character)
                       (and (< (char-code character) 128)
                            (or (alphanumericp character)
                                (find character "._-"))))
                     value)))))

(defun require-check-id (value label)
  "VALUE as a validated id, or a refusal naming LABEL."
  (unless (check-id-p value)
    (fail "~a must be a short validated id, not a path: ~s" label value))
  value)

(defun bounded-string (object key label limit &key (required t))
  "KEY's value in OBJECT, a string of at most LIMIT characters."
  (let ((value (jget object key)))
    (cond ((and (null value) (not required)) nil)
          ((not (stringp value)) (fail "~a must be a string" label))
          ((> (length value) limit) (fail "~a exceeds ~d characters" label limit))
          ((and required (zerop (length value))) (fail "~a is required" label))
          (t value))))

(defun bounded-integer (object key label low high &optional (default nil default-p))
  "KEY's value in OBJECT as an integer between LOW and HIGH, DEFAULT when absent."
  (let ((value (jget object key)))
    (when (and (null value) default-p) (setf value default))
    (unless (and (integerp value) (<= low value high))
      (fail "~a must be an integer between ~d and ~d" label low high))
    value))

(defun spec-unknown-members (object allowed label)
  "Refuse OBJECT carrying a member outside ALLOWED."
  (when (hash-table-p object)
    (let ((unknown (sort (set-difference (jkeys object) allowed :test #'equal) #'string<)))
      (when unknown
        (fail "~a has unknown fields: ~{~a~^, ~}" label unknown)))))

(defun normalize-check (object seen tests)
  "OBJECT as the canonical plist of one check. => (values CHECK SEEN TESTS),
the id list and the command/workdir pairs it joins."
  (unless (hash-table-p object) (fail "each check must be an object"))
  (let ((kind (jstring object "kind")))
    (unless (member kind '("file" "test" "website") :test #'equal)
      (fail "check kind must be file, test, or website"))
    (spec-unknown-members object
                           (cond ((equal kind "file")
                                  '("id" "kind" "path" "contains" "json_keys"))
                                 ((equal kind "test")
                                  '("id" "kind" "command" "workdir" "expected_exit"
                                    "contains" "timeout"))
                                 (t '("id" "kind" "url" "expected_status" "contains")))
                           "the check")
    (let ((id (require-check-id (jget object "id") "check id")))
      (when (member id seen :test #'equal) (fail "duplicate check id ~a" id))
      (let ((seen (cons id seen)))
        (cond ((equal kind "file") (values (normalize-file-check object id) seen tests))
              ((equal kind "test")
               (multiple-value-bind (check tests) (normalize-test-check object id tests)
                 (values check seen tests)))
              (t (values (normalize-website-check object id) seen tests)))))))

(defun normalize-file-check (object id)
  "A file check: a workspace-relative path, and either text it must hold
or the keys of the JSON object it must be."
  (let* ((label (format nil "~a.path" id))
         (path (bounded-string object "path" label +max-path+)))
    (when (find (code-char 0) path) (fail "~a must not contain a NUL character" label))
    (when (or (uiop:absolute-pathname-p (uiop:parse-unix-namestring path))
              (find #\\ path)
              (member (first (uiop:split-string path :separator "/")) '("" "." "..") :test #'equal)
              (some (lambda (part) (member part '("" "." "..") :test #'equal))
                    (uiop:split-string path :separator "/")))
      (fail "~a must be workspace-relative and must not traverse" label))
    (let ((has-contains (nth-value 1 (gethash "contains" object)))
          (has-keys (nth-value 1 (gethash "json_keys" object))))
      (unless (or has-contains has-keys)
        (fail "~a needs exactly one of contains or json_keys" id))
      (when (and has-contains has-keys)
        (fail "~a needs exactly one of contains or json_keys" id))
      (if has-contains
          (list :id id :kind "file" :path path
                :contains (bounded-string object "contains" (format nil "~a.contains" id)
                                           +max-expectation+))
          (let ((keys (jitems (jget object "json_keys"))))
            (unless (and keys (<= (length keys) +max-json-keys+))
              (fail "~a.json_keys must be a short non-empty list" id))
            (dolist (key keys)
              (unless (and (stringp key) (<= 1 (length key) 128))
                (fail "~a.json_keys entries must be short strings" id)))
            (unless (= (length keys) (length (remove-duplicates keys :test #'equal)))
              (fail "~a.json_keys has a duplicate" id))
            (list :id id :kind "file" :path path :json-keys keys))))))

(defun normalize-test-check (object id tests)
  "A test check: an exact command and workdir, the exit code it must end
with, and optional output it must hold. => (values CHECK TESTS)."
  (let ((command (bounded-string object "command" (format nil "~a.command" id) +max-command+))
        (workdir (bounded-string object "workdir" (format nil "~a.workdir" id) +max-path+))
        (expected (bounded-integer object "expected_exit" (format nil "~a.expected_exit" id)
                                   -255 255 0))
        (timeout (bounded-integer object "timeout" (format nil "~a.timeout" id) 1 120 30)))
    (when (find (code-char 0) workdir) (fail "~a.workdir must not contain a NUL character" id))
    (unless (uiop:absolute-pathname-p (uiop:parse-unix-namestring workdir))
      (fail "~a.workdir must be an absolute path" id))
    (let* ((pair (cons command workdir))
           (same (assoc pair tests :test #'equal)))
      (when same
        (fail "~a duplicates test command/workdir from ~a; use one test check per exact command/workdir pair"
              id (cdr same)))
      (values (list :id id :kind "test" :command command :workdir workdir
                    :expected-exit expected :timeout timeout
                    :contains (bounded-string object "contains" (format nil "~a.contains" id)
                                               +max-expectation+ :required nil))
              (cons (cons pair id) tests)))))

(defun normalize-website-check (object id)
  "A website check: one url, the status it must answer, and text it must hold."
  (let ((url (bounded-string object "url" (format nil "~a.url" id) +max-url+)))
    (list :id id :kind "website" :url url
          :expected-status (bounded-integer object "expected_status"
                                            (format nil "~a.expected_status" id) 100 599)
          :contains (bounded-string object "contains" (format nil "~a.contains" id)
                                     +max-expectation+))))

(defun normalize-spec (raw)
  "RAW, the spec a model passed, as the canonical plist every later step
reads; a member outside the bounded contract refuses with RECEIPTS-ERROR,
and nothing here touches the filesystem."
  (unless (hash-table-p raw) (fail "spec must be an object"))
  (spec-unknown-members raw +spec-keys+ "the spec")
  (let ((version (jget raw "schema_version")))
    (unless (eql version +schema-version+) (fail "schema_version must be 1"))
    (let* ((task-id (require-check-id (jget raw "task_id") "task_id"))
           (workspace (bounded-string raw "workspace_root" "workspace_root" +max-path+)))
      (when (find (code-char 0) workspace)
        (fail "workspace_root must not contain NUL characters"))
      (unless (uiop:absolute-pathname-p (uiop:parse-unix-namestring workspace))
        (fail "workspace_root must be an absolute path"))
      (let ((checks (jitems (jget raw "checks"))))
        (unless checks (fail "checks must be a list"))
        (unless (<= 1 (length checks) +max-checks+)
          (fail "checks must contain 1 to ~d items" +max-checks+))
        (let ((seen '()) (tests '())(normalized '()))
          (dolist (check checks)
            (multiple-value-bind (normalized-check seen-now tests-now)
                (normalize-check check seen tests)
              (push normalized-check normalized)
              (setf seen seen-now tests tests-now)))
          (list :schema-version +schema-version+
                :task-id task-id
                :workspace-root workspace
                :allow-loopback-http (and (jget raw "allow_loopback_http") t)
                :checks (nreverse normalized)))))))

;;; --- the digest over the criteria -----------------------------------------------

(defun check-json (check)
  "CHECK's canonical JSON object: its members in one fixed order."
  (cond
    ((equal (getf check :kind) "file")
     (if (getf check :json-keys)
                 (jobj "id" (getf check :id) "kind" "file" "path" (getf check :path)
                       "json_keys" (jarr (getf check :json-keys)))
                 (jobj "id" (getf check :id) "kind" "file" "path" (getf check :path)
                       "contains" (getf check :contains))))
    ((equal (getf check :kind) "test")
     (jobj "id" (getf check :id) "kind" "test"
                   "command" (getf check :command) "workdir" (getf check :workdir)
                   "expected_exit" (getf check :expected-exit)
                   "timeout" (getf check :timeout)
                   "contains" (or (getf check :contains) +null+)))
    (t (jobj "id" (getf check :id) "kind" "website" "url" (getf check :url)
                      "expected_status" (getf check :expected-status)
                      "contains" (getf check :contains)))))

(defun spec-json (spec)
  "SPEC's canonical JSON object: the text the criteria digest hashes."
  (jobj "schema_version" (getf spec :schema-version)
        "task_id" (getf spec :task-id)
        "workspace_root" (getf spec :workspace-root)
        "allow_loopback_http" (if (getf spec :allow-loopback-http) t :false)
        "checks" (jarr (mapcar #'check-json (getf spec :checks)))))

(defun criteria-digest (spec)
  "The SHA-256 of SPEC's canonical text: which criteria this attempt judges."
  (let ((digest (nlk:sha256-text (json-text (spec-json spec)))))
    (subseq digest 7)))
