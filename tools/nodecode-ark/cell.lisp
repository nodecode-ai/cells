;;;; cell.lisp --- the brain kept encrypted and restorable: snapshots of config and cells.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of medomohamed22/hermes-ark (see NOTICE), which kept a whole
;;;; Hermes home in an encrypted, deduplicated repository and could hand one
;;;; item of it back. Here the same repository holds what a Nodecode home
;;;; cannot recompute: config.jsonc and the cells folder -- the layer, the
;;;; memory, the skills and the installed cells -- content addressed, sealed
;;;; with a passphrase that is never written down, and restorable one file at
;;;; a time.
;;;;
;;;; Repository, under ~/.nodecode/ark/ unless the section names a folder:
;;;;
;;;;   ark.json            salt, KDF cost, and a verifier: never the passphrase
;;;;   objects/xx/<id>     one blob per distinct file content, sealed (ARK1)
;;;;                       or plain (ARK0) when no passphrase is set
;;;;   snapshots/<id>.json what one snapshot held: path, object, size, mode
;;;;
;;;; A snapshot costs the changed bytes only: an object already in the store
;;;; is not written again, and an unchanged home takes no new snapshot at all
;;;; when the caller allows it. The object id is an HMAC of the content under
;;;; the passphrase's key, so the store does not even say which files are
;;;; equal to anyone who has the folder without the passphrase.
;;;;
;;;; The cipher is the image's own: a keyed HMAC-SHA-256 stream (counter
;;;; mode) with an encrypt-then-MAC tag, keys from PBKDF2-HMAC-SHA-256 over
;;;; the passphrase. AES would need a foreign library beyond the digest
;;;; OpenSSL hands the image; this is the same construction class, written
;;;; small, and the tag is checked before anything is written back.

(defpackage #:nodecode-ark
  (:use #:cl)
  (:export #:ark-error #:ark-error-detail #:snapshot-manifests #:create-snapshot
           #:restore-item #:prune-snapshots #:status-text #:verify-text))

(in-package #:nodecode-ark)

(nlk:define-peripheral ark :not-running t)

;;; --- the words the model reads -------------------------------------------------

(defparameter +manual+
  "nodecode-ark keeps encrypted, deduplicated snapshots of this home's brain -- config.jsonc and
the cells folder (the layer, memory, skills, installed cells) -- in a repository of your own,
and puts one file back when it is needed.
  /ark                       the repository, the snapshots, and what changed since the last one
  /ark snap [label]         take a snapshot now
  /ark list                 every snapshot, newest first
  /ark show [id]            what a snapshot holds: file count, size, and the biggest entries
  /ark diff [a] [b]         what changed between two snapshots (default: the last two)
  /ark verify               prove every snapshot's files are present and open
  /ark restore PATH [id] [yes]   put one file back from a snapshot (preview unless yes)
  /ark prune [dry]          apply the retention policy and drop unreferenced data
The model takes a snapshot with the ark_checkpoint tool before anything risky touches memory or
skills. Settings live under \"ark\" in the config: repository, passphrase_env, roots, exclude,
keep_last, keep_hourly, keep_daily, keep_monthly, kdf_iterations, max_file_mb."
  "What (help :ark) answers while the cell runs.")

(defparameter +default-roots+ '("config.jsonc" "cells/")
  "What a snapshot holds when the section names no roots: the config and the brain.")

(defparameter +skipped-directories+
  '(".git" "fasl" "node_modules" "__pycache__" "tmp" "backups" "profiles" ".cache")
  "Folder names a snapshot never walks into: caches, other homes, git's own storage.")

(defparameter +secret-names+
  '(".env" "auth.json" "vault.key" "secrets.json" "credentials.json" "pairing.json"
    ".netrc" ".git-credentials")
  "Files this home holds that are credentials rather than brain: never captured.")

(defparameter +skipped-suffixes+
  '(".key" ".pem" ".fasl" ".lock" ".db-wal" ".db-shm" ".tmp" ".swp" "~")
  "A file whose name ends in one of these is a secret, a cache or a lock, never content.")

(define-condition ark-error (error)
  ((detail :initarg :detail :reader ark-error-detail))
  (:report (lambda (condition stream) (write-string (ark-error-detail condition) stream)))
  (:documentation "Something the operator can fix: no repository, a wrong passphrase, a bad path."))

(defun refuse (format-string &rest arguments)
  (error 'ark-error :detail (apply #'format nil format-string arguments)))

;;; --- octets, digests, and the seal ----------------------------------------------

(defun octets (value)
  "VALUE as an octet vector: its UTF-8 bytes when it is a string."
  (if (stringp value)
      (sb-ext:string-to-octets value :external-format :utf-8)
      (coerce value '(vector (unsigned-byte 8)))))

(defun octets-hex (bytes)
  (let ((out (make-string (* 2 (length bytes)))))
    (loop for byte across bytes
          for at from 0 by 2
          do (setf (char out at) (char "0123456789abcdef" (ash byte -4))
                   (char out (1+ at)) (char "0123456789abcdef" (logand byte #xf))))
    out))

(defun hex-octets (text)
  "TEXT, an even run of hexadecimal digits, as the bytes it spells."
  (let ((bytes (make-array (floor (length text) 2) :element-type '(unsigned-byte 8))))
    (loop for at from 0 below (length bytes)
          do (setf (aref bytes at)
                   (logior (ash (digit-char-p (char text (* 2 at)) 16) 4)
                           (digit-char-p (char text (1+ (* 2 at))) 16))))
    bytes))

(defun digest (bytes)
  "SHA-256 of BYTES, as 32 octets, through the digest the image already carries."
  (hex-octets (subseq (nlk:sha256-text (coerce bytes '(vector (unsigned-byte 8)))) 7)))

(defun xor-octets (left right)
  (let ((out (make-array (length left) :element-type '(unsigned-byte 8))))
    (dotimes (index (length left) out)
      (setf (aref out index) (logxor (aref left index) (aref right index))))))

(defun constant-time= (left right)
  (and (= (length left) (length right))
       (zerop (loop with difference = 0
                    for index below (length left)
                    do (setf difference (logior difference
                                                (logxor (aref left index) (aref right index))))
                    finally (return difference)))))

(defun hmac (key message)
  "HMAC-SHA-256 of MESSAGE under KEY, as 32 octets."
  (let* ((key (if (> (length key) 64) (digest key) (coerce key '(vector (unsigned-byte 8)))))
         (block (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0))
         (inner (make-array 64 :element-type '(unsigned-byte 8)))
         (outer (make-array 64 :element-type '(unsigned-byte 8))))
    (replace block key)
    (dotimes (index 64)
      (setf (aref inner index) (logxor (aref block index) #x36)
            (aref outer index) (logxor (aref block index) #x5c)))
    (digest (concatenate '(vector (unsigned-byte 8)) outer
                          (digest (concatenate '(vector (unsigned-byte 8)) inner message))))))

(defun pbkdf2 (passphrase salt iterations)
  "PASSPHRASE stretched to 32 octets: PBKDF2-HMAC-SHA-256, one block."
  (let ((block (concatenate '(vector (unsigned-byte 8)) salt (make-array 4 :element-type '(unsigned-byte 8)
                                                                           :initial-element 0))))
    (setf (aref block (1- (length block))) 1)
    (let ((step (hmac passphrase block))
          (answer (hmac passphrase block)))
      (loop repeat (1- iterations)
            do (setf step (hmac passphrase step)
                     answer (xor-octets answer step)))
      answer)))

(defun counter-octets (counter)
  (let ((bytes (make-array 4 :element-type '(unsigned-byte 8))))
    (loop for index from 3 downto 0
          do (setf (aref bytes index) (logand counter #xff)
                   counter (ash counter -8)))
    bytes))

(defun keystream (key nonce length)
  "LENGTH octets of the keyed stream: HMAC-SHA-256(KEY, NONCE || counter), block by block."
  (let ((out (make-array length :element-type '(unsigned-byte 8)))
        (at 0)
        (counter 0))
    (loop while (< at length)
          do (let ((block (hmac key (concatenate '(vector (unsigned-byte 8)) nonce (counter-octets counter)))))
               (replace out block :start1 at :end1 (min length (+ at 32)))
               (incf at 32)
               (incf counter)))
    out))

(defun magic (text)
  (octets text))

(defun seal (content keys)
  "CONTENT sealed under KEYS, or stored plainly when KEYS is NIL."
  (if (null keys)
      (concatenate '(vector (unsigned-byte 8)) (magic "ARK0") content)
      (let* ((nonce (nlk:random-bytes 16))
             (cipher (xor-octets content (keystream (getf keys :enc) nonce (length content))))
             (tag (hmac (getf keys :mac)
                        (concatenate '(vector (unsigned-byte 8)) (magic "ARK1") nonce cipher))))
        (concatenate '(vector (unsigned-byte 8)) (magic "ARK1") nonce tag cipher))))

(defun unseal (blob keys)
  "BLOB opened: the content it was sealed from, or a refusal when the tag does not check."
  (let ((header (sb-ext:octets-to-string (subseq blob 0 4))))
    (cond
      ((string= header "ARK0") (subseq blob 4))
      ((string= header "ARK1")
       (unless keys
         (refuse "the repository is encrypted: set ~a before reading it" (setting :passphrase-env)))
       (let ((nonce (subseq blob 4 20))
             (tag (subseq blob 20 52))
             (cipher (subseq blob 52)))
         (unless (constant-time= tag (hmac (getf keys :mac)
                                           (concatenate '(vector (unsigned-byte 8))
                                                        (magic "ARK1") nonce cipher)))
           (refuse "an object failed its check: the passphrase is wrong or the repository is damaged"))
         (xor-octets cipher (keystream (getf keys :enc) nonce (length cipher)))))
      (t (refuse "unknown object format ~s" header)))))

;;; --- the repository ------------------------------------------------------------

(defun repository-root ()
  "The folder that holds this home's snapshots."
  (let ((named (setting :repository)))
    (uiop:ensure-directory-pathname
     (if (and named (plusp (length named)))
         (if (uiop:absolute-pathname-p (uiop:parse-native-namestring named))
             (uiop:parse-native-namestring named)
             (nlk:home named))
         (nlk:home "ark/")))))

(defun repository-file (name)
  (merge-pathnames name (repository-root)))

(defun objects-directory () (repository-file "objects/"))

(defun snapshot-directory () (repository-file "snapshots/"))

(defun object-path (id)
  (merge-pathnames (format nil "objects/~a/~a" (subseq id 0 2) id) (repository-root)))

(defun snapshot-path (id)
  (merge-pathnames (format nil "snapshots/~a.json" id) (repository-root)))

(defun read-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

(defun passphrase ()
  "The passphrase the environment holds, or NIL for an unencrypted repository."
  (let* ((name (setting :passphrase-env))
         (value (and name (plusp (length name)) (uiop:getenv name))))
    (and value (plusp (length value)) value)))

(defun keys-from (word salt iterations)
  (let ((master (pbkdf2 (octets word) salt iterations)))
    (list :enc (hmac master (octets "enc"))
          :mac (hmac master (octets "mac"))
          :name (hmac master (octets "name"))
          :check (hmac master (octets "check")))))

(defun create-repository ()
  (let* ((salt (nlk:random-bytes 16))
         (iterations (or (setting :kdf-iterations) 120000))
         (word (passphrase))
         (keys (and word (keys-from word salt iterations)))
         (meta (nlk:json-object "format" 1
                                 "created" (nlk:iso-now)
                                 "kdf" "pbkdf2-hmac-sha256"
                                 "iterations" iterations
                                 "salt" (octets-hex salt)
                                 "encrypted" (and keys t)
                                 :opt "verifier" (and keys (octets-hex (getf keys :check)))
                                 "id" (octets-hex (nlk:random-bytes 8)))))
    (ensure-directories-exist (repository-file "ark.json"))
    (nlk:write-file-atomically (repository-file "ark.json") (nlk:encode-json-object meta) :mode #o600)
    (values keys meta (not (null keys)))))

(defun open-repository ()
  "=> (values KEYS META CREATED-P); KEYS is NIL for an unencrypted repository."
  (let ((path (repository-file "ark.json")))
    (if (not (probe-file path))
        (create-repository)
        (let* ((meta (nlk:decode-json (uiop:read-file-string path)))
               (iterations (or (nlk:json-value meta :integer "iterations") 120000))
               (salt (hex-octets (or (nlk:json-value meta :string "salt") ""))))
          (if (not (nlk:json-value meta :boolean "encrypted"))
              (values nil meta nil)
              (let ((word (passphrase)))
                (unless word
                  (refuse "this repository is encrypted: set ~a to its passphrase"
                          (setting :passphrase-env)))
                (let ((keys (keys-from word salt iterations)))
                  (unless (equal (nlk:json-value meta :string "verifier") (octets-hex (getf keys :check)))
                    (refuse "that passphrase does not open this repository"))
                  (values keys meta nil))))))))

(defun repository-p ()
  (probe-file (repository-file "ark.json")))

;;; --- what a snapshot holds ------------------------------------------------------

(defun symlink-p (path)
  (let ((stat (ignore-errors (sb-posix:lstat (uiop:native-namestring path)))))
    (and stat (sb-posix:s-islnk (sb-posix:stat-mode stat)))))

(defun file-mode (path)
  (or (ignore-errors (logand (sb-posix:stat-mode (sb-posix:lstat (uiop:native-namestring path))) #o777))
      #o600))

(defun skipped-name-p (name)
  (or (member name +secret-names+ :test #'string-equal)
      (some (lambda (suffix) (uiop:string-suffix-p name suffix)) +skipped-suffixes+)
      (uiop:string-prefix-p ".#" name)))

(defun roots ()
  (let ((named (setting :roots)))
    (if named (coerce named 'list) +default-roots+)))

(defun extra-excludes ()
  (let ((named (setting :exclude)))
    (if named (coerce named 'list) '())))

(defun within-repository-p (path)
  (let ((root (uiop:native-namestring (repository-root)))
        (name (uiop:native-namestring path)))
    (uiop:string-prefix-p root name)))

(defun walk (directory parts &aux (found '()))
  "Every regular file under DIRECTORY, as (RELATIVE-NAME . PATHNAME)."
  (dolist (file (ignore-errors (uiop:directory-files directory)))
    (let ((name (file-namestring file)))
      (unless (or (skipped-name-p name) (symlink-p file) (within-repository-p file))
        (push (cons (format nil "~{~a~^/~}/~a" parts name) file) found))))
  (dolist (sub (ignore-errors (uiop:subdirectories directory)))
    (let ((name (car (last (pathname-directory (uiop:ensure-directory-pathname sub))))))
      (unless (or (member name +skipped-directories+ :test #'string-equal)
                  (member name (extra-excludes) :test #'string-equal)
                  (symlink-p sub)
                  (within-repository-p sub))
        (setf found (nconc (walk sub (append parts (list name))) found)))))
  found)

(defun scan-home ()
  "What a snapshot would hold now, as (RELATIVE-NAME . PATHNAME), oldest name first."
  (let ((found '()))
    (dolist (root (roots))
      (let* ((name (string-right-trim "/" root))
             (path (nlk:home root)))
        (cond ((uiop:directory-exists-p path)
               (unless (or (member name (extra-excludes) :test #'string-equal)
                           (member name +skipped-directories+ :test #'string-equal))
                 (setf found (nconc (walk path (list name)) found))))
              ((uiop:file-exists-p path)
               (unless (skipped-name-p name)
                 (push (cons name path) found)))
              (t nil))))
    (sort found #'string< :key #'car)))

(defun file-size (path)
  (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (and in (file-length in))))

;;; --- objects and snapshots -------------------------------------------------------

(defun object-id (content keys)
  "The content address of CONTENT: an HMAC under the passphrase's key when there is one."
  (octets-hex (if keys (hmac (getf keys :name) content) (digest content))))

(defun store-object (content keys)
  "CONTENT in the repository, unless that content is already there. => its id."
  (let* ((id (object-id content keys))
         (path (object-path id)))
    (unless (probe-file path)
      (nlk:write-file-atomically path (seal content keys) :mode #o600))
    id))

(defun object-content (id keys)
  (let ((path (object-path id)))
    (unless (probe-file path)
      (refuse "object ~a~a is missing from the repository" (subseq id 0 8) "..."))
    (unseal (read-octets path) keys)))

(defun file-entries (keys)
  "One json object per capturable file, and how many new objects that wrote."
  (let ((entries '())
        (written 0)
        (limit (* 1024 1024 (or (setting :max-file-mb) 64))))
    (dolist (pair (scan-home))
      (let ((size (file-size (cdr pair))))
        (when (and size (<= size limit))
          (let* ((content (read-octets (cdr pair)))
                 (id (object-id content keys)))
            (unless (probe-file (object-path id))
              (store-object content keys)
              (incf written))
            (push (nlk:json-object "path" (car pair) "object" id
                                   "size" size "mode" (file-mode (cdr pair)))
                  entries)))))
    (values (coerce (sort entries #'string< :key (lambda (entry)
                                                  (or (nlk:json-value entry :string "path") "")))
                    'vector)
            written)))

(defun snapshot-files (manifest)
  "MANIFEST's files as a list of json objects."
  (coerce (or (nlk:json-value manifest :array "files") #()) 'list))

(defun snapshot-id (manifest) (nlk:json-value manifest :string "id"))
(defun snapshot-created (manifest) (nlk:json-value manifest :string "created"))
(defun snapshot-label (manifest) (nlk:json-value manifest :string "label"))
(defun snapshot-total (manifest) (nlk:json-value manifest :integer "total_bytes"))
(defun snapshot-stored (manifest) (nlk:json-value manifest :integer "stored_bytes"))
(defun snapshot-count (manifest) (length (snapshot-files manifest)))

(defun stamp (&optional (universal (get-universal-time)))
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal)
    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" year month day hour minute second)))

(defun next-sequence ()
  "The next snapshot's ordinal: one past every snapshot already in the repository, so two
snapshots in one second still sort, and prune, in the order they were taken."
  (1+ (reduce #'max (snapshot-manifests)
              :key (lambda (manifest) (or (nlk:json-value manifest :integer "sequence") 0))
              :initial-value 0)))

(defun new-snapshot-id (sequence)
  (format nil "~a-~4,'0x" (stamp) sequence))

(defun write-snapshot (files written keys label trigger sequence)
  (let* ((id (new-snapshot-id sequence))
         (total (loop for entry across files
                      sum (or (nlk:json-value entry :integer "size") 0)))
         (manifest (nlk:json-object "format" 1
                                     "id" id
                                     "sequence" sequence
                                     "created" (nlk:iso-now)
                                     "label" label
                                     "trigger" trigger
                                     "profile" (nlk:profile-label)
                                     "encrypted" (and keys t)
                                     "files" files
                                     "total_bytes" total
                                     "new_objects" written)))
    (nlk:write-file-atomically (snapshot-path id) (nlk:encode-json-object manifest) :mode #o600)
    manifest))

(defun snapshot-manifests ()
  "Every snapshot in the repository, newest first."
  (let ((files (ignore-errors (uiop:directory-files (snapshot-directory) "*.json"))))
    (sort (loop for file in files
                for manifest = (ignore-errors (nlk:decode-json (uiop:read-file-string file)))
                when manifest collect manifest)
          #'string> :key #'snapshot-order)))

(defun snapshot-order (manifest)
  "What orders two snapshots: the ordinal they were taken in, then their id."
  (format nil "~12,'0d|~a"
          (or (nlk:json-value manifest :integer "sequence") 0)
          (or (snapshot-id manifest) "")))

(defun find-snapshot (id)
  "The manifest named ID, or the newest when ID is empty, \"latest\" or \"last\"."
  (let ((all (snapshot-manifests)))
    (if (or (null id) (string= id "") (string= id "latest") (string= id "last"))
        (first all)
        (or (find id all :test #'string= :key #'snapshot-id)
            (find-if (lambda (manifest) (uiop:string-prefix-p id (snapshot-id manifest))) all)
            (refuse "no snapshot ~a in the repository" id)))))

(defun create-snapshot (&key label (trigger "manual") (skip-unchanged t))
  "A snapshot of the home now. => (values MANIFEST UNCHANGED-P); MANIFEST is NIL when
the repository could not be opened, and UNCHANGED-P when the home had not moved."
  (multiple-value-bind (keys meta created) (open-repository)
    (declare (ignore meta))
    (multiple-value-bind (files written) (file-entries keys)
      (let ((newest (first (snapshot-manifests))))
        (when (and skip-unchanged (not created) newest (same-shape-p files newest))
          (return-from create-snapshot (values newest t)))
        (values (write-snapshot files written keys label trigger (next-sequence)) nil)))))

(defun same-shape-p (files manifest)
  "Whether FILES holds exactly what MANIFEST held: the same paths at the same objects."
  (and (= (length files) (snapshot-count manifest))
       (equal (loop for entry across files collect (cons (nlk:json-value entry :string "path")
                                                         (nlk:json-value entry :string "object")))
              (loop for entry in (snapshot-files manifest) collect (cons (nlk:json-value entry :string "path")
                                                                         (nlk:json-value entry :string "object")))
              )))

;;; --- putting one thing back ------------------------------------------------------

(defun safe-relative (path)
  "PATH as a home-relative name, or a refusal when it could leave the home."
  (let* ((path (string-trim '(#\Space #\Tab) (or path "")))
         (path (string-left-trim "/" path))
         (parts (and (plusp (length path)) (uiop:split-string path :separator "/"))))
    (when (or (some (lambda (part) (member part '("" "." "..") :test #'string=)) parts)
              (find #\: path)
              (find (code-char 0) path))
      (refuse "~s is not a file inside the home" path))
    (when (null parts) (refuse "restore needs a file, e.g. /ark restore cells/you/note.lisp"))
    (format nil "~{~a~^/~}" parts)))

(defun item-in (manifest path)
  (find path (snapshot-files manifest) :test #'string= :key (lambda (entry)
                                                               (nlk:json-value entry :string "path"))))

(defun restore-item (id path &key (apply t))
  "PATH from snapshot ID, into this home when APPLY. => a line to read."
  (multiple-value-bind (keys meta) (open-repository)
    (declare (ignore meta))
    (let* ((manifest (find-snapshot id))
           (relative (safe-relative path))
           (entry (item-in manifest relative)))
      (unless entry (refuse "snapshot ~a holds no ~a" (snapshot-id manifest) relative))
      (let ((size (nlk:json-value entry :integer "size"))
            (object (nlk:json-value entry :string "object"))
            (mode (nlk:json-value entry :integer "mode")))
        (if (not apply)
            (format nil "would restore ~a (~:d bytes) from ~a; add yes to write it"
                    relative (or size 0) (snapshot-id manifest))
            (let ((content (object-content object keys))
                  (target (merge-pathnames relative (nlk:home))))
              (nlk:write-file-atomically target content :mode (or mode #o600))
              (format nil "restored ~a (~:d bytes) from ~a" relative (length content) (snapshot-id manifest))))))))

;;; --- retention ------------------------------------------------------------------

(defun bucket-key (id kind)
  (case kind
    (:hour (subseq id 0 (min 11 (length id))))
    (:day (subseq id 0 (min 8 (length id))))
    (:month (subseq id 0 (min 6 (length id))))))

(defun protected-ids ()
  "The snapshots the retention policy keeps: the newest keep_last, and the newest of each
bucket (hour, day, month) while the policy has room for that bucket."
  (let* ((all (remove nil (mapcar #'snapshot-id (snapshot-manifests))))
         (policy (list (cons :last (or (setting :keep-last) 10))
                       (cons :hour (or (setting :keep-hourly) 24))
                       (cons :day (or (setting :keep-daily) 7))
                       (cons :month (or (setting :keep-monthly) 12))))
         (protected (subseq all 0 (min (length all) (max 1 (or (setting :keep-last) 10))))))
    (dolist (kind (list :hour :day :month))
      (let ((limit (cdr (assoc kind policy)))
            (seen '()))
        (when (plusp (or limit 0))
          (dolist (id all)
            (let ((key (bucket-key id kind)))
              (unless (member key seen :test #'string=)
                (push key seen)
                (when (<= (length seen) limit)
                  (pushnew id protected :test #'string=))))))))
    protected))

(defun prune-snapshots (&key (dry-run nil))
  "Drop the snapshots the policy does not keep, then the objects none of the rest refers to.
=> a line to read."
  (let* ((all (snapshot-manifests))
         (keep (protected-ids))
         (dropped (remove-if (lambda (manifest) (member (snapshot-id manifest) keep :test #'string=))
                             all)))
    (if (null dropped)
        (format nil "nothing to prune: ~d snapshot~:p kept, all inside the policy" (length all))
        (let ((freed 0)
              (live (make-hash-table :test #'equal)))
          (unless dry-run
            (dolist (manifest dropped)
              (let ((path (snapshot-path (snapshot-id manifest))))
                (incf freed (or (file-size path) 0))
                (ignore-errors (delete-file path))))
            (dolist (manifest (snapshot-manifests))
              (dolist (entry (snapshot-files manifest))
                (setf (gethash (nlk:json-value entry :string "object") live) t)))
            (when (probe-file (objects-directory))
              (dolist (fan (uiop:subdirectories (objects-directory)))
                (dolist (object (uiop:directory-files fan))
                  (unless (gethash (file-namestring object) live)
                    (incf freed (or (file-size object) 0))
                    (ignore-errors (delete-file object))))
                (ignore-errors (uiop:delete-empty-directory fan)))))
          (format nil "~@[dry run: ~]~d snapshot~:p beyond the policy (~d kept), ~:d bytes ~:[would be freed~;freed~]"
                  dry-run (length dropped) (length (if dry-run all (snapshot-manifests))) freed (not dry-run))))))

;;; --- what the operator reads -----------------------------------------------------

(defun human-bytes (bytes)
  (let ((bytes (or bytes 0)))
    (cond ((>= bytes (* 1024 1024)) (format nil "~,1f MiB" (/ bytes (* 1024.0 1024))))
          ((>= bytes 1024) (format nil "~,1f KiB" (/ bytes 1024.0)))
          (t (format nil "~d bytes" bytes)))))

(defun changed-since (manifest)
  "How the home stands against MANIFEST: (values ADDED CHANGED REMOVED BEFORE AFTER)."
  (let ((before (make-hash-table :test #'equal))
        (added 0) (changed 0) (removed 0) (after 0))
    (dolist (entry (snapshot-files manifest))
      (setf (gethash (nlk:json-value entry :string "path") before)
            (nlk:json-value entry :integer "size")))
    (dolist (pair (scan-home))
      (incf after)
      (let ((known (gethash (car pair) before :none)))
        (cond ((eq known :none) (incf added))
              ((not (equal known (file-size (cdr pair)))) (incf changed))))
      (remhash (car pair) before))
    (setf removed (hash-table-count before))
    (values added changed removed (snapshot-count manifest) after)))

(defun status-text ()
  (if (not (repository-p))
      (format nil "No repository yet at ~a. /ark snap takes the first one.~%~
                   ~:[Unencrypted~;Encrypted with the passphrase in ~a~]."
              (nlk:home-abbreviated (uiop:native-namestring (repository-root)))
              (passphrase) (setting :passphrase-env))
      (multiple-value-bind (keys meta created) (open-repository)
        (declare (ignore keys created))
        (let* ((all (snapshot-manifests))
               (newest (first all))
               (objects (loop for fan in (ignore-errors (uiop:subdirectories (objects-directory)))
                              sum (length (ignore-errors (uiop:directory-files fan))))))
          (with-output-to-string (out)
            (format out "Ark repository ~a~%~:[plain~;encrypted~], ~d snapshot~:p, ~a in ~d object~:p.~%"
                    (nlk:home-abbreviated (uiop:native-namestring (repository-root)))
                    (nlk:json-value meta :boolean "encrypted")
                    (length all)
                    (if newest (human-bytes (reduce #'+ (snapshot-files newest)
                                                    :key (lambda (entry)
                                                           (or (nlk:json-value entry :integer "size") 0))))
                        "0 bytes")
                    objects)
            (if (null newest)
                (format out "No snapshot yet: /ark snap takes the first.~%")
                (progn
                  (format out "Newest: ~a ~@[~s ~]~a, ~d file~:p, ~a.~%"
                          (snapshot-id newest) (snapshot-label newest)
                          (snapshot-created newest) (snapshot-count newest)
                          (human-bytes (snapshot-total newest)))
                  (multiple-value-bind (added changed removed before after) (changed-since newest)
                    (format out "Since it: ~d added, ~d changed, ~d removed.~%" added changed removed)
                    (when (and (plusp before) (< after (ceiling before 2)))
                      (format out "THE BRAIN SHRANK: ~d files then, ~d now -- check cells/ or restore one.~%"
                              before after)))))
            (format out "Policy: keep ~d recent, ~d hourly, ~d daily, ~d monthly.~%"
                    (setting :keep-last) (setting :keep-hourly)
                    (setting :keep-daily) (setting :keep-monthly)))))))

(defun list-text ()
  (let ((all (snapshot-manifests)))
    (if (null all)
        "No snapshots yet: /ark snap takes the first."
        (with-output-to-string (out)
          (format out "~d snapshot~:p, newest first:~%" (length all))
          (dolist (manifest all)
            (format out "  ~a  ~8a  ~3@a file~:p  ~10a  ~@[~a~]~%"
                    (snapshot-id manifest)
                    (human-bytes (snapshot-total manifest))
                    (snapshot-count manifest)
                    (or (snapshot-created manifest) "")
                    (snapshot-label manifest)))))))

(defun show-text (id)
  (let* ((manifest (find-snapshot id))
         (files (sort (snapshot-files manifest) #'string> :key (lambda (entry)
                                                                  (or (nlk:json-value entry :integer "size") 0)))))
    (with-output-to-string (out)
      (format out "Snapshot ~a, ~@[~s ~]~a~%  ~d file~:p, ~a, stored as ~d new object~:p.~%"
              (snapshot-id manifest) (snapshot-label manifest) (snapshot-created manifest)
              (length files) (human-bytes (snapshot-total manifest))
              (or (nlk:json-value manifest :integer "new_objects") 0))
      (loop for entry in files
            for index from 1
            while (<= index 40)
            do (format out "  ~10a  ~a~%" (human-bytes (nlk:json-value entry :integer "size"))
                        (nlk:json-value entry :string "path")))
      (when (> (length files) 40)
        (format out "  ... and ~d more~%" (- (length files) 40))))))

(defun diff-text (older-id newer-id)
  (let* ((newer (find-snapshot newer-id))
         (older (if (or (null older-id) (string= older-id ""))
                    (let ((all (snapshot-manifests)))
                      (find (snapshot-id newer) all :test #'string/= :key #'snapshot-id))
                    (find-snapshot older-id))))
    (unless older (refuse "there is only one snapshot: nothing to compare it with"))
    (let ((before (make-hash-table :test #'equal))
          (after (make-hash-table :test #'equal))
          (added '()) (changed '()) (removed '()))
      (dolist (entry (snapshot-files older))
        (setf (gethash (nlk:json-value entry :string "path") before)
              (nlk:json-value entry :string "object")))
      (dolist (entry (snapshot-files newer))
        (setf (gethash (nlk:json-value entry :string "path") after)
              (nlk:json-value entry :string "object")))
      (maphash (lambda (path object)
                 (let ((was (gethash path before)))
                   (cond ((null was) (push path added))
                         ((not (equal was object)) (push path changed)))))
               after)
      (maphash (lambda (path object)
                 (declare (ignore object))
                 (unless (gethash path after) (push path removed)))
               before)
      (with-output-to-string (out)
        (format out "~a -> ~a~%" (snapshot-id older) (snapshot-id newer))
        (dolist (pair (list (cons "added" (sort added #'string<))
                             (cons "changed" (sort changed #'string<))
                             (cons "removed" (sort removed #'string<))))
          (format out "  ~a: ~d~%" (car pair) (length (cdr pair)))
          (loop for path in (cdr pair) for index from 1 while (<= index 20)
                do (format out "    ~a~%" path))
          (when (> (length (cdr pair)) 20)
            (format out "    ... and ~d more~%" (- (length (cdr pair)) 20))))))))

(defun verify-text ()
  (let ((all (snapshot-manifests)))
    (when (null all) (refuse "no snapshots to verify"))
    (multiple-value-bind (keys meta) (open-repository)
    (declare (ignore meta))
    (let ((checked 0) (bad '()))
      (dolist (manifest all)
        (dolist (entry (snapshot-files manifest))
          (incf checked)
          (let ((object (nlk:json-value entry :string "object")))
            (handler-case (object-content object keys)
              (error (condition)
                (push (format nil "~a: ~a (~a)" (snapshot-id manifest)
                              (nlk:json-value entry :string "path") condition)
                      bad))))))
      (format nil "Verified ~d file~:p across ~d snapshot~:p~:[~; -- ~d BAMAGED~]: ~@[~%~{  ~a~^~%~}~]"
              checked (length all) (null bad) (length bad) (nreverse bad))))))

;;; --- the model's one verb -------------------------------------------------------

(nlk:define-contract ark-checkpoint
  (:doc "Save a restorable, encrypted snapshot of this home's brain -- config.jsonc and the
cells folder: the layer, memory, skills and every installed cell -- before anything risky
touches them: bulk-editing or deleting memory or skills, rewriting config, or when the user
asks for a backup. Returns the snapshot id; one file comes back with /ark restore PATH ID.
Project files are not captured, and neither are credentials such as .env or auth.json.")
  ("label" :text :required t
   :doc "one short reason, shown in /ark list, e.g. 'before merging duplicate skills'"))

(defun checkpoint-tool (request)
  "The ark_checkpoint call: LABEL, one line."
  (handler-case
      (let ((label (substitute #\Space #\Newline
                                (string-trim '(#\Space #\Tab)
                                             (subseq (ark-checkpoint-label request) 0
                                                     (min 120 (length (ark-checkpoint-label request))))))))
        (multiple-value-bind (manifest unchanged) (create-snapshot :label label :trigger "agent")
          (if (null manifest)
              (nlk:encode-json-object
               (nlk:json-object "ok" nil "error" "Ark could not take a snapshot: see /ark"))
              (nlk:encode-json-object
               (nlk:json-object "ok" t
                                 "snapshot_id" (snapshot-id manifest)
                                 "label" label
                                 "files" (snapshot-count manifest)
                                 "unchanged" (and unchanged t)
                                 "restore_hint" (format nil "/ark restore PATH ~a" (snapshot-id manifest)))))))
    (ark-error (condition)
      (nlk:encode-json-object
       (nlk:json-object "ok" nil "error" (ark-error-detail condition))))))

;;; --- /ark ----------------------------------------------------------------------

(defun split-words (text)
  (remove "" (uiop:split-string (or text "") :separator '(#\Space #\Tab)) :test #'string=))

(defun run-command (args session-id)
  "One /ark invocation => the text to read."
  (declare (ignore session-id))
  (let* ((words (split-words args))
         (verb (string-downcase (or (first words) "")))
         (rest (rest words)))
    (handler-case
        (cond
          ((member verb '("" "status") :test #'string=) (status-text))
          ((member verb '("snap" "snapshot" "backup") :test #'string=)
           (multiple-value-bind (manifest unchanged) (create-snapshot :label (format nil "~{~a~^ ~}" rest)
                                                                       :trigger "operator")
             (if (null manifest)
                 "Ark could not take a snapshot: see /ark status."
                 (format nil "~:[Snapshot ~a~;No change since ~a~]: ~d file~:p, ~a~@[ (~a)~]."
                         unchanged (snapshot-id manifest) (snapshot-count manifest)
                         (human-bytes (snapshot-total manifest)) (snapshot-label manifest)))))
          ((member verb '("list" "ls" "log") :test #'string=) (list-text))
          ((string= verb "show") (show-text (first rest)))
          ((string= verb "diff") (diff-text (first rest) (second rest)))
          ((string= verb "verify") (verify-text))
          ((string= verb "prune")
           (prune-snapshots :dry-run (member (string-downcase (or (first rest) "apply"))
                                             '("dry" "dry-run" "--dry-run") :test #'string=)))
          ((member verb '("restore" "rewind") :test #'string=)
           (let* ((yes (member (string-downcase (or (car (last rest)) ""))
                                '("yes" "--yes" "-y" "confirm") :test #'string=))
                  (args (if yes (butlast rest) rest)))
             (unless args (refuse "Usage: /ark restore PATH [snapshot] [yes]"))
             (restore-item (second args) (first args) :apply (and yes t))))
          (t (format nil "Unknown /ark command ~s.~%~a" verb +manual+)))
      (ark-error (condition) (ark-error-detail condition)))))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell ark
  (:section ("ark")
    (:guide "nothing here is required: with no passphrase the repository is a plain folder under the home; a passphrase encrypts everything it stores; next step: /ark snap")
    ("repository" :path
     :doc "folder that holds the snapshots; empty means ark/ under the home")
    ("passphrase_env" :env :default "ARK_PASSPHRASE"
     :doc "environment variable holding the passphrase; empty means an unencrypted repository")
    ("roots" :list
     :doc "what a snapshot holds, relative to the home: config.jsonc and cells/ by default")
    ("exclude" :list
     :doc "extra file or folder names never captured")
    ("keep_last" :integer :default 10 :min 1
     :doc "snapshots always kept, however old")
    ("keep_hourly" :integer :default 24 :min 0
     :doc "how many hours of history the policy keeps one snapshot of")
    ("keep_daily" :integer :default 7 :min 0
     :doc "how many days of history the policy keeps one snapshot of")
    ("keep_monthly" :integer :default 12 :min 0
     :doc "how many months of history the policy keeps one snapshot of")
    ("kdf_iterations" :integer :default 120000 :min 1000
     :doc "PBKDF2 rounds over the passphrase; higher is slower to open and guess")
    ("max_file_mb" :integer :default 64 :min 1
     :doc "a file bigger than this is left out of a snapshot"))
  (:help :ark
         "ark: encrypted, deduplicated snapshots of config and cells, and one file back - /ark snap, /ark restore PATH"
         +manual+)
  (:tool "ark_checkpoint" (nlk:contract 'ark-checkpoint) #'checkpoint-tool)
  (:command "ark" 'run-command
            :description "Encrypted snapshots of this home's brain, and one file back"
            :argument-hint "[status | snap [label] | list | show [id] | diff [a] [b] | verify | restore PATH [id] [yes] | prune [dry]]"))
