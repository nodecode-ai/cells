;;;; review.lisp --- the request a person reviews, and the one file it writes.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A prepare writes one private JSON envelope into the review directory: the
;;;; exact workspace, path and content, a nonce, and a deadline. A commit reads
;;;; that envelope back by its nonce, hands it to the sidecar (sidecar.lisp),
;;;; and only then creates the one new file. Nothing here touches the network,
;;;; and nothing here reads a credential.
;;;;
;;;; A rewrite of writer.py in seanebones-lang/deadbolt-hermes-plugin (see
;;;; NOTICE): _relative, _create_file, _encoded and _load, kept check for
;;;; check, with the platform's O_EXCL and O_NOFOLLOW where it has them.

(in-package #:nodecode-deadbolt)

;;; --- the words ------------------------------------------------------------------

(defparameter +tool-id+ "hermes_artifact_write_v1"
  "The tool name the sidecar's policy is written against: the engine's own id
for one exact text-artifact write. Kept as it is so an operator's existing
`deadbolt policy --tools ...' admits this cell's actions unchanged.")

(defparameter +run-characters+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.:/-"
  "What a run ID may be made of, as the plugin's own pattern allows.")

(defparameter +hex-characters+ "0123456789abcdef"
  "What a request ID may be made of.")

;;; --- the creation flags ------------------------------------------------------------

#+sbcl
(defparameter +create-flags+
  (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl
          (if (find-symbol "O-NOFOLLOW" '#:sb-posix)
              (symbol-value (find-symbol "O-NOFOLLOW" '#:sb-posix))
              0))
  "What a new artifact is opened with: create it, fail if it is there, and
never follow a symbolic link into it.")

#+sbcl
(defun errno-word (errno)
  "ERRNO as the reason a person reads: which of the way the file is."
  (cond ((eql errno sb-posix:eexist) "the file already exists")
        ((eql errno sb-posix:eloop) "a symbolic link is in the way")
        ((eql errno sb-posix:enotdir) "a parent is not a directory")
        ((eql errno sb-posix:enoent) "the parent directory does not exist")
        ((member errno (list sb-posix:eacces sb-posix:eperm)) "permission denied")
        (t (format nil "the platform said errno ~d" errno))))

(defun open-new-file (path)
  "An output stream onto PATH, created fresh with mode 0600 and never through
a symbolic link or over an existing file: O_EXCL and O_NOFOLLOW where the
platform has them, an OPEN that refuses an existing file elsewhere."
  #+sbcl
  (let ((fd (handler-case (sb-posix:open (namestring path) +create-flags+ #o600)
              (sb-posix:syscall-error (condition)
                (fail "cannot create ~a: ~a" (file-namestring path)
                      (errno-word (sb-posix:syscall-errno condition)))))))
    (sb-sys:make-fd-stream fd :output t :element-type '(unsigned-byte 8)
                            :name (file-namestring path)))
  #-sbcl
  (or (open path :direction :output :if-exists nil :if-does-not-exist :create
            :element-type '(unsigned-byte 8))
      (fail "~a already exists: this cell only creates new files"
            (file-namestring path))))

(defun write-new-file (path text)
  "TEXT into the new file PATH, refusing an existing file or a symbolic link.
=> the bytes written."
  (let ((stream (open-new-file path)))
    (unwind-protect
         (progn (write-sequence (nlk:utf-8-octets text) stream)
                (finish-output stream))
      (close stream)))
  (nlk:utf-8-size text))

;;; --- the workspace ----------------------------------------------------------------

(defun workspace-root ()
  "The configured workspace, resolved to a directory pathname with a truename,
because it exists; a refusal naming the setting otherwise."
  (let ((workspace (setting :workspace)))
    (unless workspace
      (fail "deadbolt.workspace is not set: point it at the existing absolute ~
              directory new text files go in"))
    (unless (uiop:absolute-pathname-p workspace)
      (fail "deadbolt.workspace must be absolute, not ~a" (namestring workspace)))
    (let ((real (ignore-errors (uiop:directory-exists-p (probe-file workspace)))))
      (unless real
        (fail "deadbolt.workspace is not an existing directory: ~a" (namestring workspace)))
      (uiop:ensure-directory-pathname real))))

(defun directory-text (directory)
  "DIRECTORY as the string the envelope carries: its resolved namestring, no
trailing slash except at the root."
  (let ((text (namestring (uiop:ensure-directory-pathname directory))))
    (if (string= text "/") text (string-right-trim "/" text))))

(defun relative-parts (path)
  "PATH's parts, or a refusal: a non-empty relative path of plain names, the
one thing a new artifact may be named by."
  (let ((path (and path (princ-to-string path))))
    (unless (and (stringp path) (plusp (length path)))
      (fail "the path is empty: name the new file inside the workspace"))
    (when (or (find #\\ path) (find (code-char 0) path) (char= #\/ (char path 0)))
      (fail "the path must be relative to the workspace, with / between its ~
              names: ~s" path))
    (let ((parts (uiop:split-string path :separator "/")))
      (when (some (lambda (part) (member part '("" "." "..") :test #'string=)) parts)
        (fail "the path has an empty, . or .. part: ~s" path))
      parts)))

(defun artifact-path (root parts)
  "PARTS under ROOT, each existing parent resolved: one that leaves ROOT once
its links are followed is refused, and a parent that is missing is named."
  (let ((directory root))
    (dolist (part (butlast parts))
      (let* ((next (merge-pathnames (format nil "~a/" part) directory))
             (real (uiop:directory-exists-p next)))
        (unless real
          (fail "the parent directory ~a does not exist: create it first"
                (namestring next)))
        (unless (uiop:subpathp real root)
          (fail "~a leaves the workspace once its links are followed"
                (namestring next)))
        (setf directory (uiop:ensure-directory-pathname real))))
    (merge-pathnames (car (last parts)) directory)))

;;; --- the run and the review directory -----------------------------------------------

(defun run-id ()
  "The configured DeadBolt run, checked against the engine's own alphabet."
  (let ((run (setting :run-id)))
    (unless (and (stringp run) (<= 1 (length run) 128)
                 (every (lambda (character) (find character +run-characters+)) run))
      (fail "deadbolt.run_id must be 1 to 128 letters, digits, _ . : / or -, not ~s"
            run))
    run))

(defun review-directory ()
  "The private directory pending requests are kept in, made when it is absent:
`review_dir' when the section names one, else under the nodecode home."
  (let* ((configured (setting :review-dir))
         (directory (or configured (nlk:home "deadbolt/requests/"))))
    (ensure-directories-exist (merge-pathnames "request" directory))
    #+sbcl (ignore-errors (sb-posix:chmod (namestring directory) #o700))
    directory))

(defun request-file (request-id)
  "Where the review request REQUEST-ID is kept."
  (merge-pathnames (format nil "~a.json" request-id) (review-directory)))

(defun valid-request-id-p (value)
  "Whether VALUE could be one of the 32-hex IDs prepare answers."
  (and (stringp value) (= 32 (length value))
       (every (lambda (character) (find character +hex-characters+)) value)))

(defun new-nonce ()
  "A fresh 32-hex request ID from the operating system's entropy source."
  (string-downcase
   (format nil "~{~2,'0x~}" (coerce (nlk:random-bytes 16) 'list))))

;;; --- the envelope ------------------------------------------------------------------

(defun action (run workspace path content)
  "The exact action a person is asked to approve, as the sidecar's envelope.
=> (values envelope nonce)."
  (let ((nonce (new-nonce)))
    (values (nlk:json-object
             "version" 1
             "agent_id" run
             "tool" +tool-id+
             "dest" :null
             "arguments" (nlk:json-object "workspace" workspace
                                           "path" path
                                           "content" content)
             "nonce" nonce
             "expires_at" (+ (nlk:unix-now) (setting :request-ttl)))
            nonce)))

(defun encoded (object)
  "OBJECT as the text the sidecar is asked about, within the section's cap."
  (let ((text (nlk:encode-json-object object))
        (cap (setting :max-bytes)))
    (when (> (nlk:utf-8-size text) cap)
      (fail "the request is longer than ~d bytes: shorten the content" cap))
    text))

(defun exact-keys-p (object keys)
  "Whether OBJECT is an object carrying exactly KEYS, no more and no fewer."
  (and (hash-table-p object)
       (= (hash-table-count object) (length keys))
       (every (lambda (key) (nth-value 1 (gethash key object))) keys)))

(defun read-capped (path limit)
  "PATH's bytes, refusing more than LIMIT of them."
  (unless (probe-file path)
    (fail "no review request at ~a: prepare it first" (namestring path)))
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((size (file-length in)))
      (when (> size limit)
        (fail "~a is larger than ~d bytes" (file-namestring path) limit))
      (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
        (read-sequence bytes in)
        bytes))))

(defun load-request (request-id)
  "The envelope REQUEST-ID names, checked against this run, this workspace and
this tool. => (values ENVELOPE TEXT), TEXT what the sidecar is sent."
  (unless (valid-request-id-p request-id)
    (fail "~s is not a request ID: 32 hex characters, as prepare answered" request-id))
  (let* ((path (request-file request-id))
         (bytes (read-capped path (1+ (setting :max-bytes))))
         (envelope (handler-case (nlk:decode-json bytes :whole t) (error () nil))))
    (unless (exact-keys-p envelope '("version" "agent_id" "tool" "dest"
                                     "arguments" "nonce" "expires_at"))
      (fail "~a is not a review request" (file-namestring path)))
    (unless (and (eql 1 (gethash "version" envelope))
                 (equal (run-id) (gethash "agent_id" envelope))
                 (equal +tool-id+ (gethash "tool" envelope))
                 (eq :null (gethash "dest" envelope))
                 (equal request-id (gethash "nonce" envelope)))
      (fail "~a is another run's request, or another tool's: run, tool, ~
              destination or nonce differ" (file-namestring path)))
    (let ((arguments (gethash "arguments" envelope)))
      (unless (exact-keys-p arguments '("workspace" "path" "content"))
        (fail "~a does not carry one workspace, path and content"
              (file-namestring path)))
      (unless (equal (directory-text (workspace-root))
                     (gethash "workspace" arguments))
        (fail "the request was prepared for workspace ~a, not ~a"
              (gethash "workspace" arguments) (directory-text (workspace-root))))
      (relative-parts (gethash "path" arguments))
      (unless (stringp (gethash "content" arguments))
        (fail "~a carries no content" (file-namestring path)))
      (values envelope (encoded envelope)))))

(defun create-artifact (envelope)
  "Create the one new file ENVELOPE names, refusing overwrite and symlink
traversal. => (values PATH BYTES)."
  (let* ((arguments (gethash "arguments" envelope))
         (path (gethash "path" arguments))
         (content (gethash "content" arguments))
         (target (artifact-path (workspace-root) (relative-parts path))))
    (values path (write-new-file target content))))

;;; --- what is still pending -----------------------------------------------------------

(defun pending (&key (limit 20))
  "The review requests still in the review directory, newest first: one
(ID FILE WRITTEN-AT) a row."
  (let* ((directory (review-directory))
         (files (sort (uiop:directory-files directory "*.json") #'> :key #'file-write-date))
         (rows '()))
    (dolist (file files (nreverse rows))
      (let ((name (pathname-name file)))
        (when (and (valid-request-id-p name) (< (length rows) limit))
          (push (list name file (file-write-date file)) rows))))))
