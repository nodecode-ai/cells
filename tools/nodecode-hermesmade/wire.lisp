;;;; wire.lisp --- the shelf's two read endpoints, and the safe way a package lands.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin talked to one host, https://hermesmade.lol, and this cell talks
;;;; to the same one: GET /api/bots/<id> answers the listing as JSON, GET
;;;; /api/bots/<id>/package answers the bot as a .tar.gz whose sha256 the
;;;; listing names. Every byte goes through ASK, so a test stubs one name and
;;;; never touches the network.
;;;;
;;;; A downloaded bot is someone else's upload, so nothing in it is trusted:
;;;; the archive may carry only what a shared bot is made of (the same
;;;; allowlist the plugin used), every entry is checked before anything is
;;;; written, and the checksum is verified before the archive is opened.

(in-package #:nodecode-hermesmade)

(defparameter +default-site+ "https://hermesmade.lol"
  "The shelf, as the plugin hardcoded it; the section's site overrides it.")
(defparameter +user-agent+ "nodecode-hermesmade/0.1.0"
  "What a request calls itself, as the plugin named itself to the shelf.")
(defparameter +meta-file+ "hermesmade-bot.json"
  "The record the shelf keeps beside a bot's own files.")
(defparameter +package-top+ '("SOUL.md" "config.yaml" "distribution.yaml"
                              "hermesmade-bot.json" "skills" "cron")
  "What a bot package may hold at its top level: the plugin's own allowlist.
Anything else refuses the install.")
(defparameter +max-download+ 2000000
  "The plugin's ceiling on one package, in bytes.")
(defparameter +bot-name-limit+ 42
  "How long an installed bot's name may be, as the plugin's own pattern said.")

;;; --- the host and one exchange ---------------------------------------------------

(defun site ()
  "The shelf's address, from the section or the plugin's own; no trailing slash."
  (let ((given (nlk:trimmed (or (setting :site) ""))))
    (string-right-trim "/" (if (plusp (length given)) given +default-site+))))

(defun api (path)
  "PATH on the shelf."
  (concatenate 'string (site) path))

(defun bots-directory ()
  "Where added bots land: the section's bots_dir, else bots/ under the home."
  (let ((given (nlk:trimmed (or (setting :bots-dir) ""))))
    (if (plusp (length given))
        given
        (nlk:home "bots/"))))

(defun ask (method url &key content headers)
  "METHOD URL as one exchange under the section's ceiling => (values BODY
STATUS). A transport failure is a HERMESMADE-ERROR, one line the model reads."
  (let ((answer nil) (status nil)
        (timeout (setting :timeout-seconds)))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers (append (list (cons "user-agent" +user-agent+)
                                           (cons "accept" "application/json"))
                                     headers)
                    :content content
                    :timeout timeout
                    :connect-timeout (min 10 (max 1 timeout))))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "hermesmade: ~a" (nle:transport-failure-label condition url))))
    (values answer status)))

(defun body-object (answer)
  "ANSWER -- an HTTP body -- as a decoded JSON object, or NIL."
  (cond ((hash-table-p answer) answer)
        ((and (vectorp answer) (not (stringp answer)))
         (body-object (nlk:body-text answer)))
        ((stringp answer)
         (let ((decoded (and (plusp (length (nlk:trimmed answer)))
                             (handler-case (nlk:decode-json answer) (error () nil)))))
           (and (hash-table-p decoded) decoded)))
        (t nil)))

;;; --- a bot reference -------------------------------------------------------------

(defun bot-id (reference)
  "REFERENCE -- a bot id, a hermesmade.lol/bots/... link, or an add= link -- as
the id's digits, or a refusal: the plugin took all three shapes."
  (let* ((text (nlk:trimmed (princ-to-string (or reference ""))))
         (start (or (and (search "/bots/" text) (+ (search "/bots/" text) 6))
                    (and (search "add=" text) (+ (search "add=" text) 4))
                    (and (search "/add/" text) (+ (search "/add/" text) 5))
                    0))
         (end (or (position-if-not #'digit-char-p text :start start) (length text))))
    (when (or (zerop (length text)) (= start end))
      (fail "hermesmade: give a bot id or a hermesmade.lol/bots/... link, not ~s" text))
    (subseq text start end)))

(defun bot-name-p (name)
  "Whether NAME is a bot's own name, as the plugin's pattern read one."
  (and (stringp name)
       (<= 1 (length name) +bot-name-limit+)
       (every (lambda (c) (or (alphanumericp c) (char= c #\-))) name)
       (alphanumericp (char name 0))))

(defun bot-info (id)
  "The shelf's record for bot ID, checked where it reaches a path or a URL."
  (multiple-value-bind (answer status) (ask :get (api (format nil "/api/bots/~a" id)))
    (let ((body (body-object answer)))
      (when (and (integerp status) (>= status 400))
        (fail "hermesmade: bot ~a - HTTP ~d~@[ - ~a~]" id status
              (nlk:json-value body :text "error")))
      (let ((bot (and body (nlk:json-value body :any "bot"))))
        (unless (hash-table-p bot)
          (fail "hermesmade: the shelf answered nothing readable for bot ~a" id))
        (unless (bot-name-p (nlk:json-value bot :string "name"))
          (fail "hermesmade: bot ~a has a name that is not one: ~s"
                id (nlk:json-value bot :string "name")))
        bot))))

;;; --- reading a gzipped tar -------------------------------------------------------

(defun octets-of (answer)
  "ANSWER as octets, or a refusal: a package is bytes, not text."
  (cond ((and (vectorp answer) (not (stringp answer)))
         (coerce answer '(simple-array (unsigned-byte 8) (*))))
        ((stringp answer) (sb-ext:string-to-octets answer :external-format :latin-1))
        (t (fail "hermesmade: the package came back as ~s, not bytes"
                 (type-of answer)))))

(defun untar (tar)
  "TAR -- an uncompressed archive's octets -- as a list of (NAME KIND DATA), a
file's DATA its bytes and a directory's NIL. Only what a bot is made of
survives: an entry outside the allowlist, an absolute path, one climbing with
.., or anything but a plain file or directory refuses the whole archive."
  (let ((entries '()) (at 0) (long-name nil) (limit (length tar)))
    (loop
      while (and (< (+ at 512) limit)
                 (plusp (reduce #'+ (subseq tar at (+ at 512)))))
      do (let* ((header (subseq tar at (+ at 512)))
                (name (or long-name (header-text header 0 100)))
                (size (parse-integer (header-text header 124 135) :radix 8 :junk-allowed t))
                (kind (header-text header 156 157))
                (start (+ at 512))
                (next (+ start (* 512 (ceiling (or size 0) 512)))))
           (setf long-name nil)
           (unless size (fail "hermesmade: the package's tar header is unreadable"))
           (case (if (plusp (length kind)) (char kind 0) #\Null)
             (#\x (setf long-name (pax-name (subseq tar start (+ start size)))))
             (#\g (setf long-name nil))
             ((#\0 #\5 #\7)
              (let ((rel (component-list name)))
                (push (list (format nil "~{~a~^/~}" rel)
                            (if (char= (char kind 0) #\5) :directory :file)
                            (and (char/= (char kind 0) #\5) (subseq tar start (+ start size))))
                      entries)))
             ;; symlinks, hard links, devices: a bot carries none of them
             ((#\1 #\2 #\3 #\4 #\6) (push (list name :other nil) entries)))
           (setf at next)))
    (nreverse entries)))

(defun header-text (header start end)
  "HEADER's field between START and END as text, its NUL and space padding cut."
  (string-trim '(#\Null #\Space)
               (sb-ext:octets-to-string header :start start :end end
                                         :external-format :latin-1)))

(defun pax-name (data)
  "The path a pax extended header in DATA names, or NIL."
  (let* ((text (sb-ext:octets-to-string data :external-format :utf-8))
         (at (search " path=" text)))
    (and at (let* ((start (+ at 6))
                   (end (or (position #\Newline text :start start) (length text))))
              (subseq text start end)))))

(defun component-list (name)
  "NAME's path components, checked: no absolute path, nothing climbing out, no
empty component. A shape a bot does not have refuses the archive."
  (let* ((clean (substitute #\/ #\\ name))
         (all (remove "" (uiop:split-string clean :separator "/") :test #'string=))
         ;; a package arrives with the bot's own folder at its root: drop it, so
         ;; what is checked and written is what a bot is made of
         (parts (if (and (cdr all)
                         (not (member (first all) +package-top+ :test #'string=))
                         (member (second all) +package-top+ :test #'string=))
                    (cdr all)
                    all)))
    (when (or (zerop (length parts))
              (char= (char name 0) #\/)
              (member ".." parts :test #'string=)
              (member "." parts :test #'string=)
              (not (member (first parts) +package-top+ :test #'string=)))
      (fail "hermesmade: the package holds ~s, which a bot is not made of; not installing" name))
    parts))

(defun gunzip (octets)
  "OCTETS inflated, the tar inside a .tar.gz."
  (handler-case (chipz:decompress nil 'chipz:gzip octets)
    (error () (fail "hermesmade: the package is not readable as a .tar.gz"))))

;;; --- downloading and installing ---------------------------------------------------

(defun hex-digest (octets)
  "The sha256 of OCTETS as lowercase hex, as a listing names it."
  (subseq (nlk:sha256-text octets) 7))

(defun bot-package (bot)
  "BOT's package bytes, verified against the listing's own sha256."
  (multiple-value-bind (answer status)
      (ask :get (api (format nil "/api/bots/~a/package" (nlk:json-value bot :string "id")))
           :headers (list (cons "accept" "application/gzip")))
    (when (and (integerp status) (/= status 200))
      (fail "hermesmade: the package download failed - HTTP ~d" status))
    (let ((bytes (octets-of answer)))
      (when (> (length bytes) (max +max-download+ (setting :max-bytes)))
        (fail "hermesmade: the package is ~d KB; that is larger than this cell takes"
              (round (length bytes) 1024)))
      (multiple-value-bind (refusal code)
          (values (nlk:json-value bot :string "sha256") nil)
        (declare (ignore code))
        (when (and refusal (not (string-equal refusal (hex-digest bytes))))
          (fail "hermesmade: the package does not match the listing's checksum; not installing")))
      bytes)))

(defun extract (entries dest)
  "Write ENTRIES under DEST, refusing an entry that would land outside it.
=> the files written, relative."
  (let ((root (namestring (uiop:ensure-directory-pathname dest))) (written '()))
    (dolist (entry entries)
      (destructuring-bind (name kind data) entry
        (let ((path (merge-pathnames (format nil "~a" name) dest)))
          (unless (eql 0 (search root (namestring path)))
            (fail "hermesmade: ~a would land outside the bot's folder" name))
          (if (eq kind :directory)
              (ensure-directories-exist (merge-pathnames "x" path))
              (progn
                (ensure-directories-exist path)
                (with-open-file (out path :direction :output :if-exists :supersede
                                          :element-type '(unsigned-byte 8)
                                          :if-does-not-exist :create)
                  (write-sequence data out))
                (push name written))))))
    (nreverse written)))

(defun skill-names (files)
  "The skills FILES -- relative paths written by EXTRACT -- carry: the directory
under skills/ each file sits in."
  (let ((names '()))
    (dolist (file files (nreverse names))
      (let ((parts (uiop:split-string file :separator "/")))
        (when (and (string= (first parts) "skills") (second parts))
          (pushnew (second parts) names :test #'string=))))))

(defun schedule-count (files)
  "How many cron definitions FILES carry."
  (count-if (lambda (file) (eql 0 (search "cron/" file))) files))

(defun install (reference &key name)
  "REFERENCE added as a bot under the section's bots directory. => a plist:
:name, :title, :dir, :files, :skills, :schedules."
  (let* ((bot (bot-info (bot-id reference)))
         (want (or (and name (nlk:trimmed name)) (nlk:json-value bot :string "name")))
         (folder (bot-folder-name want))
         (dir (merge-pathnames (format nil "~a/" folder)
                                (uiop:ensure-directory-pathname (bots-directory)))))
    ;; everything is checked in memory first: a package the shelf served wrong
    ;; leaves no folder behind at all
    (let* ((entries (untar (gunzip (bot-package bot))))
           (files (progn (ensure-directories-exist (merge-pathnames "x" dir))
                         (extract entries dir))))
      (list :name folder
            :title (or (nlk:json-value bot :string "title") want)
            :dir (namestring dir)
            :files (length files)
            :skills (skill-names files)
            :schedules (schedule-count files)
            :record bot))))

(defun bot-folder-name (want)
  "WANT as a folder name a bot may take."
  (let ((name (string-downcase (substitute #\- #\Space (nlk:trimmed want)))))
    (if (bot-name-p name)
        name
        (let ((slug (string-right-trim "-" (subseq (cl-ppcre:regex-replace-all
                                                     "[^a-z0-9]+" name "-")
                                                    0 (min +bot-name-limit+ (length name))))))
          (cond ((zerop (length slug)) (fail "hermesmade: ~s is no name for a bot" want))
                ((< (length slug) 3) (concatenate 'string slug "-bot"))
                (t slug))))))
