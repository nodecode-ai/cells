;;;; lineage.lisp --- the graph: artifacts, the sources behind them, receipts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the agent-side core of P0GL0L's hermes-lineage (see NOTICE):
;;;; one observer on every tool call, a store of artifacts with the files and
;;;; URLs each was made from, and a receipt that says what a file is and where
;;;; it came from. An edge's basis is kept, never blended: OBSERVED means the
;;;; producing call named the source, EVIDENCED means the artifact's own text
;;;; names it, INFERRED means the session had seen it earlier. Nothing here
;;;; reaches the network; nothing here opens a credential file.

(in-package #:nodecode-file-lineage)

(nlk:define-peripheral file-lineage :not-running t)

;;; --- what it never reads, and how much it reads --------------------------------

(defparameter +max-hash-bytes+ (* 32 1024 1024)
  "A file larger than this is recorded with its size and no hash.")

(defparameter +max-evidence-bytes+ (* 512 1024)
  "How much of an artifact's own text is scanned for the sources it names.")

(defparameter +artifact-cap+ 500
  "Artifacts the store keeps; the oldest fall away.")

(defparameter +source-cap+ 60
  "Sources one artifact lists.")

(defparameter +seen-cap+ 400
  "Locators a session's window remembers, newest first.")

(defparameter +version-cap+ 8
  "Earlier hashes one artifact keeps.")

(defparameter +credential-fragments+
  '("/.ssh/" "/.gnupg/" "/.aws/" "/.config/gcloud/" "/.docker/config.json"
    "/.netrc" "/.npmrc" "/.pypirc" "/.kube/" "/.password-store/"
    "id_rsa" "id_ed25519" "id_ecdsa" ".pem" ".key" ".p12" ".pfx" ".keystore")
  "Path fragments never opened on a guess and never recorded as a source.")

(defparameter +skip-suffixes+
  '(".fasl" ".pyc" ".lock" ".tmp" ".o" ".so" ".dylib" ".dll" ".class" ".receipt.json")
  "Files a call leaves behind that are not artifacts.")

(defparameter +skip-folders+
  '(".git" "node_modules" ".nodecode" "dist" "build" "target" ".venv" "venv"
    "__pycache__" ".cache" ".mypy_cache" ".pytest_cache" ".tox" "vendor")
  "Folders the working-tree walk does not enter.")

(defparameter +named-tools+ '("write-file" "edit")
  "Tools whose arguments name the file they write.")

(defparameter +snapshot-tools+ '("sh" "eval" "patch")
  "Tools whose writes are found by walking the session's folder.")

;;; --- hashing and files ----------------------------------------------------------

(defun digest-text (text)
  "TEXT's SHA-256, `sha256:` and lowercase hex."
  (nlk:sha256-text (or text "")))

(defun short-hash (sha &optional (digits 10))
  "DIGITS characters of SHA past its `sha256:' prefix, or the whole when it has none."
  (let ((hex (if (and (stringp sha) (>= (length sha) 7) (string= "sha256:" (subseq sha 0 7)))
                  (subseq sha 7)
                  (or sha ""))))
    (subseq hex 0 (min digits (length hex)))))

(defun regular-file-p (path)
  "Whether PATH names a readable regular file."
  (and (stringp path) (nlk:file-bytes path) t))

(defun file-digest (path)
  "=> (VALUES SHA256 SIZE) for PATH; SHA256 is NIL past the size ceiling."
  (let ((size (and (stringp path) (nlk:file-bytes path))))
    (when size
      (if (> size +max-hash-bytes+)
          (values nil size)
          (handler-case
              (with-open-file (in path :element-type '(unsigned-byte 8))
                (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
                  (read-sequence bytes in)
                  (values (nlk:sha256-text bytes) size)))
            (error () (values nil size)))))))

(defun file-text (path &optional (limit +max-evidence-bytes+))
  "At most LIMIT characters of PATH, or NIL when it cannot be read as text."
  (when (regular-file-p path)
    (handler-case
        (with-open-file (in path :element-type 'character)
          (let* ((size (or (ignore-errors (file-length in)) limit))
                 (text (make-string (max 0 (min limit size)))))
            (subseq text 0 (read-sequence text in))))
      (error () nil))))

(defun credential-path-p (path)
  "Whether PATH is a secret the walk must not open."
  (let ((text (string-downcase (or path ""))))
    (or (some (lambda (fragment) (search fragment text)) +credential-fragments+)
        (and (cl-ppcre:scan "(^|/)\\.env($|\\.)" text) t)
        (and (cl-ppcre:scan "(^|/)\\.git-credentials$" text) t))))

(defun absolute-path (token cwd)
  "TOKEN read against CWD as a namestring, or NIL for a token that is no path."
  (handler-case
      (let* ((expanded (cond ((string= token "~") (user-homedir-pathname))
                              ((and (> (length token) 1) (string= "~/" (subseq token 0 2)))
                               (merge-pathnames (subseq token 2) (user-homedir-pathname)))
                              (t (pathname token))))
             (full (if (uiop:absolute-pathname-p expanded)
                       expanded
                       (merge-pathnames expanded (or cwd (uiop:getcwd))))))
        (namestring full))
    (error () nil)))

(defun resolve-existing (token cwd)
  "The truename of the file TOKEN names, or NIL when no file is there."
  (let ((path (absolute-path token cwd)))
    (and path (regular-file-p path) (ignore-errors (namestring (truename path))))))

;;; --- the tokens a call's text yields ---------------------------------------------

(defparameter +token-breakers+ '(#\Space #\Tab #\Newline #\Return #\" #\' #\` #\( #\) #\[
                                  #\] #\{ #\} #\< #\> #\, #\; #\= #\|)
  "What ends a token in a command, a form or a result.")

(defparameter +token-trailing+ ".,;:!?)]}>'\"`")

(defun text-tokens (text)
  "TEXT split where a shell or a form would split it, trailing punctuation trimmed."
  (let ((out '()) (text (or text "")) (start 0) (length 0))
    (setf length (length text))
    (flet ((emit (end)
             (let ((token (string-trim +token-trailing+ (subseq text start end))))
               (when (>= (length token) 2) (push token out)))))
      (dotimes (i length)
        (when (member (char text i) +token-breakers+)
          (emit i)
          (setf start (1+ i))))
      (emit length))
    (nreverse out)))

(defun url-token-p (token)
  (or (uiop:string-prefix-p "http://" token) (uiop:string-prefix-p "https://" token)))

(defun namespace-url-p (url)
  "XML namespace names look like URLs but nobody fetches them."
  (and (search "w3.org/" url) t))

(defun normalize-url (url)
  "URL with scheme and host lowercased, userinfo, `www.' and the fragment dropped,
and a trailing slash on a bare path removed. The query stays: it names the page."
  (let* ((text (string-trim '(#\Space) (or url "")))
         (fragment (position #\# text))
         (base (if fragment (subseq text 0 fragment) text))
         (scheme-end (search "://" base))
         (scheme (string-downcase (if scheme-end (subseq base 0 scheme-end) "https")))
         (rest (if scheme-end (subseq base (+ scheme-end 3)) base))
         (slash (position #\/ rest))
         (authority (string-downcase (if slash (subseq rest 0 slash) rest)))
         (path (if slash (subseq rest slash) "/"))
         (at (position #\@ authority)))
    (when at (setf authority (subseq authority (1+ at))))
    (when (uiop:string-prefix-p "www." authority) (setf authority (subseq authority 4)))
    (when (and (> (length path) 1) (uiop:string-suffix-p "/" path))
      (setf path (subseq path 0 (1- (length path)))))
    (format nil "~a://~a~a" scheme authority path)))

(defun directory-of (path)
  "The folder PATH sits in, as a namestring."
  (or (ignore-errors (namestring (uiop:pathname-directory-pathname path))) (uiop:getcwd)))

(defun locators-in (text cwd &key (limit 200))
  "The URLs and the existing files TEXT names, as (:KIND :LOCATOR) plists."
  (let ((out '()) (urls '()) (files '()))
    (dolist (token (text-tokens text))
      (cond ((url-token-p token)
             (let ((url (normalize-url token)))
               (unless (namespace-url-p url) (pushnew url urls :test #'equal))))
            (t
             (let ((path (resolve-existing token cwd)))
               (when (and path (not (credential-path-p path)))
                 (pushnew path files :test #'equal))))))
    (dolist (url (reverse urls))
      (when (< (length out) limit) (push (list :kind "url" :locator url) out)))
    (dolist (path (reverse files))
      (when (< (length out) limit) (push (list :kind "file" :locator path) out)))
    (nreverse out)))

(defun flatten-text (value out)
  "Write every string inside VALUE — a tool's arguments, decoded — into OUT."
  (cond ((stringp value) (write-string value out) (write-char #\Space out))
        ((hash-table-p value)
         (maphash (lambda (key item) (declare (ignore key)) (flatten-text item out)) value))
        ((vectorp value) (loop for item across value do (flatten-text item out)))
        ((consp value) (dolist (item value) (flatten-text item out)))
        ((and value (not (eq value :null))) (format out "~a " value)))
  out)

(defun arguments-text (arguments)
  "A tool call's arguments as one text, for the locators it names."
  (with-output-to-string (out) (flatten-text arguments out)))

(defun json-string (table key)
  (and (hash-table-p table)
       (let ((value (gethash key table)))
         (and (stringp value) (plusp (length value)) value))))

;;; --- the working tree, bounded ---------------------------------------------------

(defun skipped-file-p (path)
  (let ((name (string-downcase (or (file-namestring path) ""))))
    (or (some (lambda (suffix) (uiop:string-suffix-p name suffix)) +skip-suffixes+)
        (uiop:hidden-pathname-p path))))

(defun tree-snapshot (root limit)
  "ROOT's files as an alist (PATH-NAMESTRING . (MTIME . SIZE)), at most LIMIT of them."
  (let ((out '()) (root (uiop:ensure-directory-pathname root)))
    (labels ((walk (dir)
               (when (< (length out) limit)
                 (dolist (file (ignore-errors (uiop:directory-files dir)))
                   (when (< (length out) limit)
                     (unless (or (skipped-file-p file) (credential-path-p (namestring file)))
                       (let ((size (nlk:file-bytes file))
                             (mtime (ignore-errors (file-write-date file))))
                         (when size
                           (push (cons (or (ignore-errors (namestring (truename file))) (namestring file))
                                       (cons mtime size))
                                 out))))))
                 (dolist (sub (ignore-errors (uiop:subdirectories dir)))
                   (unless (member (string-downcase (or (car (last (pathname-directory sub))) ""))
                                   +skip-folders+ :test #'equal)
                     (walk sub))))))
      (when (and root (ignore-errors (uiop:directory-exists-p root)))
        (walk root))
      out)))

(defun changed-paths (before after)
  "The files in AFTER that are new or whose time or size moved since BEFORE."
  (let ((out '()))
    (dolist (entry after)
      (let ((old (assoc (car entry) before :test #'equal)))
        (when (or (null old) (not (equal (cdr old) (cdr entry))))
          (push (car entry) out))))
    (nreverse out)))

;;; --- the store -------------------------------------------------------------------

(defvar *artifacts* (make-hash-table :test #'equal)
  "Absolute path -> artifact plist. The graph, in memory.")

(defvar *seen* (make-hash-table :test #'equal)
  "Session id -> newest-first (TIME KIND LOCATOR): what this session has looked at.")

(defvar *cwd* nil
  "The folder the hook reads when no session owns the call: bound in tests.")

(defun store-path ()
  (nlk:home "file-lineage/graph.json"))

(defun artifact-locators ()
  "Every path the store knows, as a list."
  (loop for path being the hash-keys of *artifacts* collect path))

(defun source-json (source)
  (nlk:json-object "kind" (getf source :kind)
                   "locator" (getf source :locator)
                   "basis" (getf source :basis)
                   "confidence" (getf source :confidence)
                   "sha256" (getf source :sha256)
                   "at" (getf source :at)))

(defun artifact-json (artifact)
  (nlk:json-object
   "path" (getf artifact :path)
   "label" (getf artifact :label)
   "sha256" (getf artifact :sha256)
   "size" (getf artifact :size)
   "tool" (getf artifact :tool)
   "call_id" (getf artifact :call-id)
   "session" (getf artifact :session)
   "first" (getf artifact :first)
   "updated" (getf artifact :updated)
   "versions" (coerce (mapcar (lambda (entry)
                                 (nlk:json-object "sha256" (getf entry :sha256)
                                                  "at" (getf entry :at)))
                               (getf artifact :versions))
                       'vector)
   "sources" (coerce (mapcar #'source-json (getf artifact :sources)) 'vector)))

(defun json-field (table key)
  (and (hash-table-p table) (let ((value (gethash key table))) (and (not (eq value :null)) value))))

(defun source-from-json (table)
  (list :kind (json-field table "kind")
        :locator (json-field table "locator")
        :basis (json-field table "basis")
        :confidence (json-field table "confidence")
        :sha256 (json-field table "sha256")
        :at (json-field table "at")))

(defun artifact-from-json (table)
  (let ((path (json-field table "path")))
    (when (stringp path)
      (list :path path
            :label (json-field table "label")
            :sha256 (json-field table "sha256")
            :size (json-field table "size")
            :tool (json-field table "tool")
            :call-id (json-field table "call_id")
            :session (json-field table "session")
            :first (json-field table "first")
            :updated (json-field table "updated")
            :versions (loop for entry across (nlk:json-array table "versions")
                            when (stringp (json-field entry "sha256"))
                              collect (list :sha256 (json-field entry "sha256")
                                            :at (json-field entry "at")))
            :sources (loop for entry across (nlk:json-array table "sources")
                           when (stringp (json-field entry "locator"))
                             collect (source-from-json entry))))))

(defun prune-artifacts ()
  "Keep the newest +ARTIFACT-CAP+ artifacts."
  (when (> (hash-table-count *artifacts*) +artifact-cap+)
    (let* ((rows (loop for artifact being the hash-values of *artifacts* collect artifact))
           (sorted (sort rows #'> :key (lambda (artifact) (or (getf artifact :updated) 0))))
           (kept (subseq sorted 0 +artifact-cap+)))
      (clrhash *artifacts*)
      (dolist (artifact kept) (setf (gethash (getf artifact :path) *artifacts*) artifact)))))

(defun save-store ()
  "Write the graph under the cell's home; a store that cannot be written is not an error."
  (handler-case
      (let ((path (store-path))
            (rows (loop for artifact being the hash-values of *artifacts* collect artifact)))
        (ensure-directories-exist path)
        (nlk:write-file-atomically
         path
         (nlk:encode-json-object
          (nlk:json-object "version" 1
                           "artifacts" (coerce (mapcar #'artifact-json rows) 'vector)))
         :mode #o600)
        path)
    (error () nil)))

(defun load-store ()
  "Read the graph back from the cell's home into memory."
  (clrhash *artifacts*)
  (let ((path (store-path)))
    (when (probe-file path)
      (handler-case
          (let ((data (nlk:decode-json (or (file-text path (* 8 1024 1024)) ""))))
            (loop for entry across (nlk:json-array data "artifacts")
                  for artifact = (artifact-from-json entry)
                  when artifact do (setf (gethash (getf artifact :path) *artifacts*) artifact)))
        (error () nil))))
  (hash-table-count *artifacts*))

;;; --- what the settings say, and what the call named ------------------------------

(defun setting-value (key default)
  "KEY's value while the cell runs, DEFAULT when it does not."
  (if *file-lineage* (setting key) default))

(defun sidecar-p () (setting-value :sidecar nil))
(defun track-shell-p () (setting-value :track-shell t))
(defun snapshot-limit () (setting-value :snapshot-max-files 2000))
(defun artifacts-per-call () (setting-value :max-artifacts-per-call 50))
(defun session-window () (setting-value :session-window-calls 40))

(defun call-cwd (session)
  (or *cwd*
      (and (stringp session) (plusp (length session))
           (ignore-errors (nlk:find-session-cwd session)))
      (ignore-errors (uiop:getcwd))))

(defun source-plist (locator basis confidence &optional sha)
  (list :kind (if (url-token-p locator) "url" "file")
        :locator locator
        :basis basis
        :confidence confidence
        :sha256 sha
        :at (get-universal-time)))

(defun basis-rank (basis)
  (cond ((equal basis "observed") 3) ((equal basis "evidenced") 2) ((equal basis "inferred") 1) (t 0)))

(defun merge-sources (lists)
  "One source per locator, the stronger basis winning, strongest first."
  (let ((by-locator (make-hash-table :test #'equal)))
    (dolist (source (apply #'append (remove nil lists)))
      (let* ((locator (getf source :locator))
             (known (and (stringp locator) (gethash locator by-locator))))
        (when (and (stringp locator)
                   (or (null known) (> (basis-rank (getf source :basis)) (basis-rank (getf known :basis)))))
          (setf (gethash locator by-locator) source))))
    (let ((out (loop for source being the hash-values of by-locator collect source)))
      (subseq (sort out #'> :key (lambda (s) (* 10 (or (getf s :confidence) 0))))
              0 (min +source-cap+ (length out))))))

(defun remember-seen (session sources)
  "Put this call's locators in the session's window, newest first."
  (when (and (stringp session) sources)
    (setf (gethash session *seen*)
          (let ((rows (gethash session *seen*)))
            (dolist (source sources)
              (push (list (get-universal-time) (getf source :kind) (getf source :locator)) rows))
            (subseq rows 0 (min +seen-cap+ (length rows)))))))

(defun session-locators (session)
  "Every locator this session has seen."
  (mapcar #'third (gethash session *seen*)))

(defun evidenced-sources (path session taken)
  "The sources the artifact's own text names, when the session already knows them."
  (let ((text (file-text path))
        (known (append (session-locators session) (artifact-locators)))
        (taken-locators (mapcar (lambda (source) (getf source :locator)) taken))
        (out '()))
    (when text
      (dolist (token (text-tokens text))
        (let ((locator (cond ((url-token-p token) (normalize-url token))
                             (t (resolve-existing token (directory-of path))))))
          (when (and locator (member locator known :test #'equal)
                     (not (member locator taken-locators :test #'equal))
                     (not (member locator (mapcar (lambda (s) (getf s :locator)) out) :test #'equal)))
            (push (source-plist locator "evidenced" 0.9 (source-sha locator)) out)))))
    (nreverse out)))

(defun source-sha (locator)
  "The hash of a file source, when it is small enough to hash for the record."
  (when (and (not (url-token-p locator)) (regular-file-p locator)
             (let ((size (nlk:file-bytes locator))) (and size (<= size +max-hash-bytes+))))
    (nth-value 0 (file-digest locator))))

(defun inferred-sources (session started taken)
  "What this session saw before the call, counted as an inferred source."
  (let ((window (session-window))
        (taken-locators (mapcar (lambda (source) (getf source :locator)) taken))
        (now (or started (get-universal-time)))
        (out '())
        (count 0))
    (when (and (integerp window) (plusp window))
      (dolist (row (gethash session *seen*))
        (when (and (< count window)
                   (<= (first row) now)
                   (not (member (third row) taken-locators :test #'equal)))
          (push (list :kind (second row) :locator (third row) :basis "inferred"
                      :confidence 0.35 :sha256 nil :at (first row))
                out)
          (incf count))))
    (nreverse out)))

(defun note-artifact (path &key tool call-id session sources started)
  "Record PATH as what TOOL produced: hash, size, the sources behind it. => the artifact, or NIL."
  (multiple-value-bind (sha size) (file-digest path)
    (when (and sha (not (credential-path-p path)))
      (let* ((now (get-universal-time))
             (old (gethash path *artifacts*))
             (observed (remove-if-not (lambda (source) (getf source :locator)) sources))
             (evidenced (evidenced-sources path session observed))
             (inferred (inferred-sources session started (append observed evidenced)))
             (merged (merge-sources (list observed evidenced inferred)))
             (artifact (list :path path
                             :label (file-namestring path)
                             :sha256 sha
                             :size size
                             :tool (or tool (getf old :tool))
                             :call-id (or call-id (getf old :call-id))
                             :session (or session (getf old :session))
                             :first (or (getf old :first) now)
                             :updated now
                             :versions (artifact-versions old sha now)
                             :sources merged)))
        (setf (gethash path *artifacts*) artifact)
        (prune-artifacts)
        (save-store)
        (when (sidecar-p) (write-sidecar path))
        artifact))))

(defun artifact-versions (old sha now)
  "OLD's versions, with the hash it had before this one, newest first."
  (let ((previous (and old (getf old :sha256)))
        (versions (and old (getf old :versions))))
    (if (and (stringp previous) (not (equal previous sha)))
        (subseq (cons (list :sha256 previous :at now) versions) 0 (min +version-cap+ (1+ (length versions))))
        (or versions '()))))

;;; --- the hook --------------------------------------------------------------------

(defun produced-paths (name arguments cwd before scanned)
  "The files this call produced: named by a file tool, or changed under CWD
since BEFORE, the walk SCANNED took (an empty folder gives NIL too)."
  (let ((named (when (member name +named-tools+ :test #'string-equal)
                 (let ((path (json-string arguments "path")))
                   (and path (let ((full (absolute-path path cwd)))
                               (and full (regular-file-p full)
                                    (ignore-errors (namestring (truename full)))))))))
        (changed (when (and scanned (member name +snapshot-tools+ :test #'string-equal))
                   (changed-paths before (tree-snapshot cwd (snapshot-limit))))))
    (remove-duplicates
     (remove-if (lambda (path) (or (null path) (credential-path-p path))) (append (and named (list named)) changed))
     :test #'equal)))

(defun observe-call (op next)
  "The :TOOL observer: run the call, then record what it wrote and what it drew on."
  (let* ((name (getf op :name))
         (arguments (getf op :arguments))
         (call-id (getf op :call-id))
         (session (or (getf (nle:turn) :session-id) ""))
         (cwd (call-cwd session))
         (snapshot-p (and cwd (track-shell-p) (member name +snapshot-tools+ :test #'string-equal)))
         (before (and snapshot-p (tree-snapshot cwd (snapshot-limit))))
         (started (get-universal-time))
         (result (funcall next op)))
    (handler-case
        (let* ((produced (produced-paths name arguments cwd before snapshot-p))
               (result-text (if (stringp result) result ""))
               (named (locators-in (concatenate 'string (arguments-text arguments) " " result-text) cwd))
               (sources (remove-if (lambda (source) (member (getf source :locator) produced :test #'equal))
                                   named))
               (taken (subseq produced 0 (min (max 1 (artifacts-per-call)) (length produced)))))
          (dolist (path taken)
            (note-artifact path
                           :tool name :call-id call-id :session session
                           :sources (mapcar (lambda (source)
                                              (source-plist (getf source :locator) "observed" 1.0
                                                            (source-sha (getf source :locator))))
                                            sources)
                           :started started))
          (remember-seen session named))
      (error () nil))
    result))

;;; --- reading it back ---------------------------------------------------------------

(defun iso-time (universal)
  "UNIVERSAL as `YYYY-MM-DD HH:MM', or `?' for none."
  (if (integerp universal)
      (multiple-value-bind (second minute hour day month year) (decode-universal-time universal)
        (declare (ignore second))
        (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute))
      "?"))

(defun shorten (text limit)
  (let ((one (nlk:one-line (or text ""))))
    (if (> (length one) limit) (concatenate 'string (subseq one 0 (max 0 (- limit 1))) "…") one)))

(defun find-artifact (ref &key cwd)
  "The artifact REF names: a path, a path suffix, or a label the list shows."
  (let* ((text (string-trim '(#\Space #\Tab) (or ref "")))
         (absolute (and (plusp (length text)) (resolve-existing text cwd))))
    (or (and absolute (gethash absolute *artifacts*))
        (and (plusp (length text)) (gethash text *artifacts*))
        (and (plusp (length text))
             (loop for path being the hash-keys of *artifacts* using (hash-value artifact)
                   when (or (equal text (getf artifact :label))
                            (path-suffix-p path text))
                     return artifact)))))

(defun path-suffix-p (path suffix)
  "Whether PATH ends with SUFFIX at a path boundary."
  (and (stringp suffix)
       (uiop:string-suffix-p path suffix)
       (let ((at (- (length path) (length suffix))))
         (or (zerop at) (char= #\/ (char path (1- at)))))))

(defun artifacts (&optional (limit 15) session)
  "The newest artifacts, LIMIT of them, one session's when SESSION is given."
  (let ((rows (loop for artifact being the hash-values of *artifacts* collect artifact)))
    (when (and session (plusp (length session)))
      (setf rows (remove-if-not (lambda (artifact) (equal session (getf artifact :session))) rows)))
    (subseq (sort rows #'> :key (lambda (artifact) (or (getf artifact :updated) 0))) 0 (min limit (length rows)))))

(defun artifact (ref &key cwd) (find-artifact ref :cwd cwd))

(defun list-text (session &optional (limit 15))
  (let ((rows (artifacts limit session)))
    (if (null rows)
        "file-lineage: nothing recorded yet. Ask the agent to write a file, and come back."
        (with-output-to-string (out)
          (format out "file-lineage — artifacts, ~a:~%" (if (and session (plusp (length session))) "this session" "recent"))
          (dolist (artifact rows)
            (format out "  ~a  ~a  ~2d src  ~a  ~a~%"
                    (iso-time (getf artifact :updated))
                    (shorten (getf artifact :label) 36)
                    (length (getf artifact :sources))
                    (short-hash (getf artifact :sha256))
                    (or (getf artifact :tool) "")))
          (format out "  /file-lineage <file> for the story behind one of them~%")))))

(defun explain-text (path)
  "Where PATH came from, as the operator reads it."
  (let ((artifact (gethash path *artifacts*)))
    (if (null artifact)
        (format nil "file-lineage: nothing recorded for ~a. Only a file a tool call wrote is tracked." path)
        (with-output-to-string (out)
          (format out "file-lineage: ~a~%  ~@[~d bytes, ~]~a~%  made by ~a at ~a~@[ (call ~a)~]~@[ in session ~a~]~%"
                  (getf artifact :path)
                  (getf artifact :size)
                  (or (getf artifact :sha256) "no hash")
                  (or (getf artifact :tool) "?")
                  (iso-time (getf artifact :updated))
                  (getf artifact :call-id)
                  (let ((session (getf artifact :session))) (and session (plusp (length session)) session)))
          (let ((sources (getf artifact :sources)))
            (format out "  ~d source~:p~%" (length sources))
            (dolist (source sources)
              (format out "    ~9a ~4a ~a~@[ ~a~]~%"
                      (or (getf source :basis) "?")
                      (or (getf source :kind) "?")
                      (shorten (getf source :locator) 96)
                      (and (getf source :sha256) (short-hash (getf source :sha256) 12)))))
          (format out "  /file-lineage verify <file> re-hashes it and its sources; /file-lineage mermaid <file> draws the graph.~%")))))

(defun source-state (source)
  "SOURCE checked against the disk now: fresh, modified, missing, unchecked or unhashed."
  (let ((locator (getf source :locator)))
    (cond ((equal (getf source :kind) "url") "unchecked")
          ((null (nlk:file-bytes locator)) "missing")
          ((null (getf source :sha256)) "unhashed")
          ((equal (getf source :sha256) (nth-value 0 (file-digest locator))) "fresh")
          (t "modified"))))

(defun verify-text (path)
  "Re-hash PATH and its file sources against what was recorded when it was made."
  (let ((artifact (gethash path *artifacts*)))
    (if (null artifact)
        (format nil "file-lineage: nothing recorded for ~a." path)
        (let* ((recorded (getf artifact :sha256))
               (current (nth-value 0 (file-digest path)))
               (status (cond ((null (nlk:file-bytes path)) "missing")
                             ((and recorded (equal recorded current)) "fresh")
                             (t "modified")))
               (rows (mapcar (lambda (source) (cons (source-state source) source))
                             (getf artifact :sources)))
               (stale (some (lambda (row) (member (car row) '("modified" "missing") :test #'equal)) rows))
               (verdict (cond ((string= status "missing") "missing")
                              ((string= status "modified") "modified")
                              (stale "stale")
                              (t "fresh"))))
          (with-output-to-string (out)
            (format out "file-lineage: ~a: ~a → verdict: ~a~%" (or (getf artifact :label) path) status verdict)
            (when (and recorded current (not (equal recorded current)))
              (format out "  recorded ~a now ~a~%" (short-hash recorded 12) (short-hash current 12)))
            (dolist (row rows)
              (format out "  ~9a ~4a ~a~%" (car row) (or (getf (cdr row) :kind) "?")
                      (shorten (getf (cdr row) :locator) 96)))
            (when (some (lambda (row) (equal (car row) "unchecked")) rows)
              (format out "  URLs are unchecked: nothing here reaches the network.~%")))))))

(defun mermaid-text (path)
  "PATH and its sources as a Mermaid graph."
  (let ((artifact (gethash path *artifacts*)))
    (if (null artifact)
        (format nil "file-lineage: nothing recorded for ~a." path)
        (with-output-to-string (out)
          (format out "graph LR~%  A[\"~a<br/>~a\"]~%"
                  (label-text (getf artifact :label))
                  (short-hash (getf artifact :sha256)))
          (loop for source in (getf artifact :sources)
                for index from 1
                do (format out "  S~d[\"~a<br/>~a\"]~%  S~d -->|~a| A~%"
                           index
                           (label-text (shorten (getf source :locator) 48))
                           (or (getf source :kind) "?")
                           index
                           (or (getf source :basis) "?")))))))

(defun label-text (text)
  "TEXT safe inside a Mermaid or JSON label."
  (with-output-to-string (out)
    (loop for character across (or text "")
          do (write-char (if (member character '(#\" #\[ #\] #\< #\> #\{ #\} #\|)) #\Space character) out))))

(defun receipt-json (path)
  "The receipt of the file at PATH: what it is, and what it was made from."
  (let ((artifact (gethash path *artifacts*)))
    (when artifact
      (let* ((body (nlk:json-object
                    "schema" "nodecode.file-lineage.v1"
                    "artifact" (nlk:json-object "path" (getf artifact :path)
                                                 "label" (getf artifact :label)
                                                 "sha256" (getf artifact :sha256)
                                                 "size" (getf artifact :size)
                                                 "recorded_at" (iso-time (getf artifact :updated)))
                    "producer" (nlk:json-object "tool" (getf artifact :tool)
                                                 "call_id" (or (getf artifact :call-id) "")
                                                 "session" (or (getf artifact :session) ""))
                    "sources" (coerce (mapcar #'source-json (getf artifact :sources)) 'vector)
                    "versions" (coerce (mapcar (lambda (entry)
                                                  (nlk:json-object "sha256" (getf entry :sha256)
                                                                   "at" (getf entry :at)))
                                                (getf artifact :versions))
                                        'vector)
                    "basis" (nlk:json-object
                             "observed" "the producing tool call named this source"
                             "evidenced" "the artifact's own text names this source"
                             "inferred" "this session had seen this source earlier")))
             (text (nlk:encode-json-object body)))
        (nlk:json-object "schema" "nodecode.file-lineage.v1"
                         "receipt_sha256" (digest-text text)
                         "body" body)))))

(defun write-sidecar (path)
  "Write <PATH>.receipt.json beside the file => its path, or NIL."
  (handler-case
      (let ((receipt (receipt-json path)))
        (when receipt
          (let ((sidecar (concatenate 'string path ".receipt.json")))
            (nlk:write-file-atomically sidecar (nlk:encode-json-object receipt) :mode #o644)
            sidecar)))
    (error () nil)))

(defun stats-text ()
  "What the store holds."
  (let ((sources 0) (sessions (make-hash-table :test #'equal)))
    (loop for artifact being the hash-values of *artifacts*
          do (incf sources (length (getf artifact :sources)))
             (let ((session (getf artifact :session)))
               (when (and (stringp session) (plusp (length session)))
                 (setf (gethash session sessions) t))))
    (format nil "file-lineage store: ~d artifacts, ~d sources, ~d sessions; ~@[~d KB at ~a~]"
            (hash-table-count *artifacts*) sources (hash-table-count sessions)
            (let ((size (and (probe-file (store-path)) (nlk:file-bytes (store-path)))))
              (and size (floor size 1024)))
            (namestring (store-path)))))

(defparameter +primer+
  "File-lineage is on: every file a tool call writes gets a receipt — its SHA-256, the call that made it, and the files and URLs it was made from. A source is OBSERVED when the producing call named it, EVIDENCED when the artifact's own text names it, INFERRED when this session had seen it earlier; the three are labelled apart. One command, and two tools:
  /file-lineage                 artifacts made this session
  /file-lineage <file>          where a file came from
  /file-lineage verify <file>   check the file and its sources against recorded hashes
  /file-lineage mermaid <file>  the provenance graph
  /file-lineage receipt <file>  write <file>.receipt.json beside it
  /file-lineage stats           what the store holds
  file_lineage_show(ref) and file_lineage_verify(ref) do the same for the model."
  "What (help :file-lineage) answers while the cell runs.")
