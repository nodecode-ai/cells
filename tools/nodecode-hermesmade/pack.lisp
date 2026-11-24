;;;; pack.lisp --- a bot folder as the shelf's package, and the record of what you shared.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin's share path built a package from an allowlist, never from "the
;;;; profile minus a deny-list", then pattern-scanned every file for keys and
;;;; aborted the whole share on one hit. This does the same from a folder: the
;;;; files a bot is made of (SOUL.md, config.yaml, skills/, cron/), scanned,
;;;; then tarred and gzipped as one .tar.gz and posted as multipart to
;;;; POST /api/bots with the @handle for credit. A first share is a listing;
;;;; sharing the same name again sends the listing's manage token, so it
;;;; updates that listing instead of duplicating it, exactly as the plugin did.

(in-package #:nodecode-hermesmade)

(defparameter +secret-patterns+
  '(("sk-[A-Za-z0-9_-]{16,}" . "an API key")
    ("gh[pousr]_[A-Za-z0-9]{20,}" . "a GitHub token")
    ("xox[abprs]-[A-Za-z0-9-]{10,}" . "a Slack token")
    ("AKIA[0-9A-Z]{16}" . "an AWS key")
    ("-----BEGIN [A-Z ]*PRIVATE KEY-----" . "a private key")
    ("(api[_-]?key|token|secret|password|passwd)[\"']?[ \t]*[:=][ \t]*[\"']?[A-Za-z0-9_./+-]{12,}"
     . "a secret-looking assignment")
    ("\\b[0-9a-f]{40,}\\b" . "a long hex string"))
  "What a shared file may not hold: one hit aborts the share, as the plugin's
own scan did, and nothing is uploaded.")
(defparameter +skip-dirs+ '(".git" ".svn" "__pycache__" "node_modules")
  "Never shipped, wherever they sit.")
(defparameter +replaceable+ '("distribution.yaml" "hermesmade-bot.json")
  "The two files this cell writes itself, so a folder's own copies never ship.")
(defparameter +first-version+ "1.0.0"
  "What a new listing starts at; a re-share bumps the recorded version.")

;;; --- reading and scanning ---------------------------------------------------------

(defun octets-file (path)
  "PATH's bytes."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((out (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence out in)
      out)))

(defun text-file (path)
  "PATH's bytes as UTF-8 text, or NIL when they are not text."
  (handler-case (sb-ext:octets-to-string (octets-file path) :external-format :utf-8)
    (error () nil)))

(defun secret-hit (text)
  "What in TEXT looks like a credential, or NIL."
  (loop for (pattern . what) in +secret-patterns+
        when (cl-ppcre:scan pattern (or text "")) return what))

(defun check-secrets (files)
  "FILES -- (RELATIVE . PATH) -- scanned; a hit is a refusal naming the file."
  (dolist (file files)
    (let ((hit (secret-hit (text-file (cdr file)))))
      (when hit
        (fail "hermesmade: ~a holds what looks like ~a; remove it and share again. Nothing was uploaded."
              (car file) hit)))))

(defun inside (root path)
  "PATH under ROOT, both namestrings of absolute directories and a file?"
  (eql 0 (search (namestring (uiop:ensure-directory-pathname root))
                  (namestring path))))

(defun bot-files (folder)
  "FOLDER as the (RELATIVE . PATH) list a bot is made of: SOUL.md, config.yaml,
skills/ and cron/, nothing else. A link that leaves the folder refuses the
share, so nothing outside it can be uploaded by accident."
  (let* ((root (uiop:ensure-directory-pathname (uiop:truename* folder)))
         (files '()))
    (labels ((keep (rel path)
               (unless (inside root path)
                 (fail "hermesmade: ~a points outside the bot's folder; not sharing" rel))
               (push (cons rel path) files))
             (tree (dir rel)
               (dolist (child (uiop:subdirectories dir))
                 (let ((name (car (last (pathname-directory child)))))
                   (unless (member name +skip-dirs+ :test #'string=)
                     (tree child (format nil "~a~a/" rel name)))))
               (dolist (child (uiop:directory-files dir))
                 (keep (format nil "~a~a" rel (file-namestring child))
                       (uiop:truename* child)))))
      (dolist (name '("SOUL.md" "config.yaml"))
        (let ((path (merge-pathnames name root)))
          (when (uiop:file-exists-p path)
            (keep name (uiop:truename* path)))))
      (dolist (name '("skills" "cron"))
        (let ((dir (merge-pathnames (format nil "~a/" name) root)))
          (when (uiop:directory-exists-p dir)
            (tree dir (format nil "~a/" name))))))
    (nreverse files)))

(defun souled-p (folder)
  "Whether FOLDER holds the SOUL.md a shared bot needs: the plugin refused a
profile whose personality file was under 20 characters."
  (let ((path (merge-pathnames "SOUL.md" (uiop:ensure-directory-pathname folder))))
    (and (uiop:file-exists-p path)
         (let ((text (text-file (uiop:truename* path))))
           (and text (>= (length (nlk:trimmed text)) 20))))))

;;; --- the package ------------------------------------------------------------------

(defun bot-slug (name)
  "NAME as a bot's name on the shelf: lowercased, dashes, at most 40."
  (let* ((lower (string-downcase (nlk:trimmed name)))
         (slug (string-trim "-" (cl-ppcre:regex-replace-all "[^a-z0-9]+" lower "-")))
         (slug (subseq slug 0 (min 40 (length slug)))))
    (if (< (length slug) 3) (string-trim "-" (concatenate 'string slug "-bot")) slug)))

(defun manifest-text (name title tagline handle version)
  "distribution.yaml for a bot, the way the shelf reads a listing's manifest."
  (format nil "name: ~a~%version: ~a~%description: ~a~%hermes_requires: \">=0.22.1\"~%~
author: ~a~%license: \"\"~%env_requires: []~%"
          name version (or (and (plusp (length (or tagline ""))) tagline) title) handle))

(defun meta-text (title tagline version)
  "The record the shelf keeps beside a bot's own files, hermesmade-bot.json."
  (format nil "{\"title\": ~s, \"tagline\": ~s, \"version\": ~s, \"model_hint\": \"\"}~%"
          title (or tagline "") version))

;;; --- tar and gzip ----------------------------------------------------------------

(defun octal (value width)
  "VALUE as WIDTH-1 octal digits and a NUL: a tar header field."
  (let ((digits (format nil "~o" value)))
    (concatenate 'string (make-string (max 0 (- width 1 (length digits))) :initial-element #\0)
                 digits (string #\Null))))

(defun field-bytes (text width)
  "TEXT as WIDTH bytes: NUL padded, cut when longer."
  (let ((bytes (sb-ext:string-to-octets text :external-format :utf-8)))
    (subseq (concatenate '(vector (unsigned-byte 8)) bytes
                          (make-array width :element-type '(unsigned-byte 8) :initial-element 0))
            0 width)))

(defun ustar-header (name size)
  "One 512-byte ustar header for NAME of SIZE bytes."
  (let ((header (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
    (labels ((put (text at width) (replace header (field-bytes text width) :start1 at)))
      (put name 0 100)
      (put (octal #o644 8) 100 8)
      (put (octal 0 8) 108 8)
      (put (octal 0 8) 116 8)
      (put (octal size 12) 124 12)
      (put (octal (get-universal-time) 12) 136 12)
      (put "        " 148 8)
      (put "0" 156 1)
      (put "ustar" 257 6)
      (put "00" 263 2)
      (put "root" 265 32)
      (put "root" 297 32)
      (put (octal 0 8) 329 8)
      (put (octal 0 8) 337 8)
      (put (concatenate 'string (format nil "~6,'0o" (loop for byte across header sum byte))
                        (string #\Null) " ")
           148 8))
    header))

(defun tar-bytes (entries prefix)
  "ENTRIES -- (RELATIVE . OCTETS) -- as a tar archive, each row under PREFIX,
which is the bot's own folder inside the archive."
  (let ((chunks '())
        (trailer (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
    (dolist (entry entries)
      (let* ((name (concatenate 'string prefix (car entry)))
             (data (cdr entry))
             (pad (mod (- 512 (mod (length data) 512)) 512)))
        (push (ustar-header name (length data)) chunks)
        (push data chunks)
        (push (make-array pad :element-type '(unsigned-byte 8) :initial-element 0) chunks)))
    (apply #'concatenate '(vector (unsigned-byte 8)) (append (nreverse chunks) (list trailer)))))

(defun gzip-bytes (octets)
  "OCTETS gzipped, the deflater this image already carries."
  (salza2:compress-data octets 'salza2:gzip-compressor))

;;; --- the multipart body ----------------------------------------------------------

(defun bytes-of (string)
  "STRING as UTF-8 octets."
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun multipart (fields filename package)
  "FIELDS -- (NAME . VALUE) -- and the package as one multipart body
=> (values BODY CONTENT-TYPE)."
  (let* ((boundary (format nil "----nodecode-hermesmade-~36r" (random most-positive-fixnum)))
         (head (with-output-to-string (out)
                 (dolist (field fields)
                   (format out "--~a~c~cContent-Disposition: form-data; name=\"~a\"~c~c~c~c~a~c~c"
                           boundary #\Return #\Newline (car field)
                           #\Return #\Newline #\Return #\Newline (cdr field) #\Return #\Newline))
                 (format out "--~a~c~cContent-Disposition: form-data; name=\"package\"; filename=\"~a\"~c~c~
Content-Type: application/gzip~c~c~c~c"
                         boundary #\Return #\Newline filename
                         #\Return #\Newline #\Return #\Newline #\Return #\Newline)))
         (tail (format nil "~c~c--~a--~c~c" #\Return #\Newline boundary #\Return #\Newline))
         (head-bytes (bytes-of head))
         (tail-bytes (bytes-of tail))
         (body (make-array (+ (length head-bytes) (length package) (length tail-bytes))
                            :element-type '(unsigned-byte 8))))
    (replace body head-bytes :start1 0)
    (replace body package :start1 (length head-bytes))
    (replace body tail-bytes :start1 (+ (length head-bytes) (length package)))
    (values body (format nil "multipart/form-data; boundary=~a" boundary))))

;;; --- what you shared -------------------------------------------------------------

(defun state-path ()
  "The record of this machine's listings, one a line, written mode 600."
  (let ((dir (nlk:home "hermesmade/")))
    (ensure-directories-exist (merge-pathnames "shared" dir))
    (merge-pathnames "shared.txt" dir)))

(defun read-state ()
  "The listing records as plists: :name :id :token :version :url :handle."
  (let ((path (state-path)))
    (when (uiop:file-exists-p path)
      (loop for line in (uiop:read-file-lines path)
            for fields = (uiop:split-string line :separator (string #\Tab))
            when (>= (length fields) 6)
              collect (list :name (first fields) :id (second fields) :token (third fields)
                            :version (fourth fields) :url (fifth fields) :handle (sixth fields))))))

(defun write-state (records)
  "RECORDS written back, readable by nobody else."
  (let ((path (state-path)))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (record records)
        (format out "~a~%"
                (with-output-to-string (line)
                  (loop for field in (list (getf record :name) (getf record :id)
                                           (getf record :token) (getf record :version)
                                           (getf record :url) (getf record :handle))
                        for first = t then nil
                        do (unless first (write-char #\Tab line))
                           (princ field line))))))
    (ignore-errors (sb-posix:chmod (namestring path) #o600))
    path))

(defun record-for (name)
  "This machine's record of the listing NAME, or NIL."
  (find name (read-state) :key (lambda (record) (getf record :name)) :test #'string=))

(defun remember (record)
  "RECORD put in place of the one under its name."
  (write-state (cons record (remove (getf record :name) (read-state)
                                    :key (lambda (one) (getf one :name)) :test #'string=))))

;;; --- sharing --------------------------------------------------------------------

(defun next-version (previous)
  "PREVIOUS's version with its last part raised, or the first version."
  (if (null previous)
      +first-version+
      (let ((parts (mapcar (lambda (one) (or (parse-integer one :junk-allowed t) 0))
                           (uiop:split-string (or (getf previous :version) +first-version+)
                                              :separator "."))))
        (format nil "~d.~d.~d" (or (first parts) 1) (or (second parts) 0)
                (1+ (or (third parts) 0))))))

(defun pack (folder &key title tagline)
  "FOLDER as a share package, nothing sent: => a plist with the archive's
bytes, its files, its name, title, tagline and version."
  (unless (souled-p folder)
    (fail "hermesmade: ~a has no SOUL.md of at least 20 characters; give the bot a personality first"
          folder))
  (let* ((root (uiop:ensure-directory-pathname (uiop:truename* folder)))
         (given (and title (nlk:trimmed title)))
         (name (bot-slug (or (and given (plusp (length given)) given)
                              (car (last (pathname-directory root))))))
         (title (if (and given (plusp (length given))) given (string-capitalize name)))
         (tagline (or tagline ""))
         (version (next-version (record-for name)))
         (files (bot-files folder)))
    (check-secrets files)
    (let* ((extra (list (cons "distribution.yaml"
                              (bytes-of (manifest-text name title tagline "" version)))
                         (cons +meta-file+ (bytes-of (meta-text title tagline version)))))
           (all (append (mapcar (lambda (file) (cons (car file) (octets-file (cdr file)))) files)
                        extra)))
      (list :name name :title title :tagline tagline :version version
            :files (mapcar #'car all)
            :filename (format nil "~a-~a.tar.gz" name version)
            :archive (gzip-bytes (tar-bytes all (format nil "~a/" name)))))))

(defun upload (plan handle)
  "PLAN uploaded to the shelf under HANDLE, updating its former listing when
this machine has one. => (values RECORD MESSAGE)."
  (let* ((handle (string-left-trim "@" (nlk:trimmed (or handle ""))))
         (previous (record-for (getf plan :name))))
    (unless (cl-ppcre:scan "^[A-Za-z0-9_]{1,30}$" handle)
      (fail "hermesmade: give your @handle for credit (letters, digits, underscore): the handle ~
setting, or /hermesmade share FOLDER @you"))
    (multiple-value-bind (body content-type)
        (multipart (append (list (cons "handle" handle))
                           (when previous (list (cons "bot_id" (getf previous :id)))))
                   (getf plan :filename) (getf plan :archive))
      (multiple-value-bind (answer status)
          (ask :post (api "/api/bots") :content body
               :headers (append (list (cons "content-type" content-type))
                                (when (and previous (getf previous :token))
                                  (list (cons "x-manage-token" (getf previous :token))))))
        (let ((object (body-object answer)))
          (when (and (integerp status) (>= status 400))
            (fail "hermesmade: the upload failed - HTTP ~d~@[ - ~a~]"
                  status (nlk:json-value object :text "error")))
          (unless (and object (nlk:json-value object :any "ok"))
            (fail "hermesmade: the upload was not accepted~@[ - ~a~]"
                  (nlk:json-value object :text "error")))
          (let ((record (list :name (getf plan :name)
                              :id (or (nlk:json-value object :string "id")
                                      (and previous (getf previous :id)))
                              :token (or (nlk:json-value object :string "manage_token")
                                         (and previous (getf previous :token)))
                              :version (getf plan :version)
                              :url (or (nlk:json-value object :string "url") "")
                              :handle handle)))
            (remember record)
            (values record (or (nlk:json-value object :string "message")
                               "Shared. A person reviews it before it is listed."))))))))

(defun unshare (name)
  "NAME's listing deleted from the shelf with the manage token kept here."
  (let ((record (record-for name)))
    (unless record
      (fail "hermesmade: this machine has no listing called ~s; mine lists them" name))
    (unless (getf record :token)
      (fail "hermesmade: no manage token was kept for ~s, so it cannot be deleted from here" name))
    (multiple-value-bind (answer status)
        (ask :post (api (format nil "/api/bots/~a/delete" (getf record :id)))
             :content (make-array 0 :element-type '(unsigned-byte 8))
             :headers (list (cons "x-manage-token" (getf record :token))))
      (let ((object (body-object answer)))
        (when (and (integerp status) (>= status 400))
          (fail "hermesmade: the shelf refused the delete - HTTP ~d~@[ - ~a~]"
                status (nlk:json-value object :text "error")))))
    (write-state (remove name (read-state) :key (lambda (one) (getf one :name)) :test #'string=))
    (format nil "Deleted the listing ~s from the shelf." name)))
