;;;; store.lisp --- the store's format, its crypto, the dotenv body, and the
;;;; process environment it loads into.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The format is the original's, byte for byte, so a store the original
;;;; wrote opens here and one written here opens there:
;;;;
;;;;   key file    32 raw bytes, mode 0600
;;;;   store file  "SEALED1" and a newline, then
;;;;               base64url(nonce(12) || AES-256-GCM ciphertext || tag(16))
;;;;
;;;; "SEALED1" and the newline after it are the AEAD's associated data, so a
;;;; store written by another format version fails authentication instead of
;;;; decrypting into garbage. The plaintext is a dotenv body, parsed and
;;;; dumped the way the original's store.py does.
;;;;
;;;; AES-256-GCM is ironclad's, the crypto system already in the image. The
;;;; environment is reached through three variables a test binds to a hash
;;;; table, so a run proves the load and the restore without touching the
;;;; environment of the image it runs in.

(in-package #:nodecode-sealed)

(defparameter +magic-text+ "SEALED1"
  "The format's magic, the AEAD's associated data and the store's first line.")

(defparameter +magic+ (sb-ext:string-to-octets (format nil "~a~%" +magic-text+)
                                               :external-format :ascii)
  "The magic as octets, the newline included: what a store file starts with.")

(defparameter +key-length+ 32)
(defparameter +nonce-length+ 12)
(defparameter +tag-length+ 16)

(defparameter +default-store+ "sealed/sealed.store"
  "The store, under the home, when the section does not name one.")

(defparameter +default-key-file+ "sealed/sealed.key"
  "The key file, under the home, when the section does not name one.")

(defparameter +env-name-characters+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
  "What an environment variable's name is made of, after its first character.")

(defparameter +quoting-characters+ " #\"'\\$"
  "A value holding any of these, a tab, a newline or nothing at all is written quoted.")

;;; --- octets ------------------------------------------------------------------------

(defun string->octets (text)
  "TEXT as its UTF-8 octets."
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun octets->string (octets)
  "OCTETS as the UTF-8 text they spell."
  (sb-ext:octets-to-string octets :external-format :utf-8))

(defun random-octets (count)
  "COUNT octets from the operating system's generator."
  (ironclad:random-data count))

(defun octets->hex (octets)
  "OCTETS as lowercase hex, two characters an octet."
  (string-downcase (format nil "~{~2,'0x~}" (coerce octets 'list))))

;;; --- base64url, the alphabet the original writes its payload in -------------

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

(defparameter +base64-values+
  (let ((table (make-hash-table)))
    (loop for index from 0 below 64
          do (setf (gethash (char-code (char +base64-alphabet+ index)) table) index))
    ;; the standard alphabet decodes too: the original's reader spends
    ;; urlsafe_b64decode, which takes either.
    (setf (gethash (char-code #\+) table) 62
          (gethash (char-code #\/) table) 63)
    table)
  "A character's code to its six bits.")

(defun base64url-encode (octets)
  "OCTETS as base64url with `=' padding, three octets to four characters."
  (with-output-to-string (out)
    (loop for start from 0 below (length octets) by 3
          for left = (- (length octets) start)
          for a = (aref octets start)
          for b = (if (> left 1) (aref octets (1+ start)) 0)
          for c = (if (> left 2) (aref octets (+ start 2)) 0)
          do (write-char (char +base64-alphabet+ (ldb (byte 6 2) a)) out)
             (write-char (char +base64-alphabet+
                               (logior (ash (ldb (byte 2 0) a) 4) (ldb (byte 4 4) b)))
                         out)
             (if (> left 1)
                 (write-char (char +base64-alphabet+
                                   (logior (ash (ldb (byte 4 0) b) 2) (ldb (byte 2 6) c)))
                             out)
                 (write-char #\= out))
             (if (> left 2)
                 (write-char (char +base64-alphabet+ (ldb (byte 6 0) c)) out)
                 (write-char #\= out)))))

(defun base64url-decode (text &key (where "the store payload"))
  "TEXT as octets; padding and whitespace are skipped, anything else refuses."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0))
        (buffer 0)
        (bits 0))
    (loop for character across text
          for value = (cond ((member character '(#\Space #\Tab #\Newline #\Return)) nil)
                            ((char= character #\=) nil)
                            (t (or (gethash (char-code character) +base64-values+)
                                   (fail "~a is not base64: it carries ~s" where character))))
          when value
            do (setf buffer (logior (ash buffer 6) value)
                     bits (+ bits 6))
               (when (>= bits 8)
                 (decf bits 8)
                 (vector-push-extend (ldb (byte 8 bits) buffer) octets)
                 (setf buffer (ldb (byte bits 0) buffer))))
    (coerce octets '(simple-array (unsigned-byte 8) (*)))))

;;; --- the seal ----------------------------------------------------------------------

(defun aes-gcm-seal (key nonce aad plaintext)
  "PLAINTEXT under AES-256-GCM: the ciphertext then its 16-octet tag."
  (let ((cipher (ironclad:make-authenticated-encryption-mode
                 :gcm :cipher-name :aes :key key :initialization-vector nonce)))
    (when (plusp (length aad)) (ironclad:process-associated-data cipher aad))
    (concatenate '(vector (unsigned-byte 8))
                 (ironclad:encrypt-message cipher plaintext)
                 (ironclad:produce-tag cipher))))

(defun aes-gcm-open (key nonce aad sealed)
  "The plaintext under SEALED when its tag verifies, NIL when it does not."
  (handler-case
      (let* ((size (length sealed))
             (cipher (ironclad:make-authenticated-encryption-mode
                      :gcm :cipher-name :aes :key key :initialization-vector nonce)))
        (when (< size +tag-length+) (return-from aes-gcm-open nil))
        (when (plusp (length aad)) (ironclad:process-associated-data cipher aad))
        (let ((plain (ironclad:decrypt-message cipher (subseq sealed 0 (- size +tag-length+)))))
          (and (equalp (ironclad:produce-tag cipher) (subseq sealed (- size +tag-length+)))
               plain)))
    (error () nil)))

;;; --- the dotenv body ---------------------------------------------------------------

(defun env-name-p (name)
  "NAME is a usable environment variable name, as ^[A-Za-z_][A-Za-z0-9_]*$ is."
  (and (stringp name)
       (plusp (length name))
       (or (find (char name 0) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")
           nil)
       (every (lambda (character) (find character +env-name-characters+)) name)
       t))

(defun check-name (name &key (what "a variable name"))
  "NAME when it is a variable name; a refusal naming WHAT otherwise."
  (unless (env-name-p name)
    (fail "~a must be letters, numbers and underscores, not starting with a number: ~s"
          what name))
  name)

(defun unescape-double-quoted (text)
  "TEXT with \\\\, \\n, \\t, \\r and \\\" read back, as the original's dump writes them."
  (with-output-to-string (out)
    (loop with index = 0
          while (< index (length text))
          for character = (char text index)
          do (if (and (char= character #\\)
                      (< (1+ index) (length text))
                      (find (char text (1+ index)) "ntr\"\\"))
                 (let ((next (char text (1+ index))))
                   (write-char (case next (#\n #\Newline) (#\t #\Tab) (#\r #\Return) (t next)) out)
                   (incf index 2))
                 (progn (write-char character out) (incf index))))))

(defun parse-dotenv (text)
  "TEXT's KEY=value lines as an alist (NAME . VALUE), in file order.

Comments and blank lines are skipped, an `export ' prefix is allowed, single
and double quotes are stripped, and escapes are read inside double quotes.
One line is one value: a value holding a newline is written escaped."
  (let ((values '()))
    (dolist (raw (uiop:split-string text :separator '(#\Newline)))
      (let ((line (string-trim '(#\Space #\Tab #\Return) raw)))
        (cond
          ((zerop (length line)))
          ((char= (char line 0) #\#))
          (t
           (when (uiop:string-prefix-p "export " line)
             (setf line (string-left-trim '(#\Space #\Tab) (subseq line 7))))
           (let ((equals (position #\= line)))
             (when equals
               (let ((name (string-trim '(#\Space #\Tab) (subseq line 0 equals)))
                     (value (string-trim '(#\Space #\Tab) (subseq line (1+ equals)))))
                 (when (env-name-p name)
                   (when (and (>= (length value) 2)
                              (find (char value 0) "'\"")
                              (char= (char value 0) (char value (1- (length value)))))
                     (let ((quote (char value 0)))
                       (setf value (subseq value 1 (1- (length value))))
                       (when (char= quote #\") (setf value (unescape-double-quoted value)))))
                   (setf values (acons name value (remove name values :key #'car :test #'string=)))))))))))
    (nreverse values)))

(defun escape-double-quoted (value)
  "VALUE with \\\\, \\\", \\n, \\t and \\r written, for a quoted line."
  (with-output-to-string (out)
    (loop for character across value
          do (case character
               (#\\ (write-string "\\\\" out))
               (#\" (write-string "\\\"" out))
               (#\Newline (write-string "\\n" out))
               (#\Tab (write-string "\\t" out))
               (#\Return (write-string "\\r" out))
               (t (write-char character out))))))

(defun quoted-value-p (value)
  "VALUE needs quotes to read back as itself."
  (or (zerop (length value))
      (some (lambda (character) (find character +quoting-characters+)) value)
      (find #\Tab value)
      (find #\Newline value)))

(defun dump-dotenv (values)
  "VALUES, an alist, as dotenv text: sorted by name, quoted only when needed."
  (with-output-to-string (out)
    (write-line "# sealed env store: encrypted at rest; /sealed list never prints a value" out)
    (dolist (name (sort (mapcar #'car values) #'string<))
      (let ((value (cdr (assoc name values :test #'string=))))
        (if (quoted-value-p value)
            (format out "~a=\"~a\"~%" name (escape-double-quoted value))
            (format out "~a=~a~%" name value))))))

;;; --- the store file -----------------------------------------------------------------

(defun store-text (key values &optional nonce)
  "VALUES under KEY as the store file's whole text, with NONCE or a fresh one."
  (let* ((nonce (or nonce (random-octets +nonce-length+)))
         (sealed (aes-gcm-seal key nonce +magic+ (string->octets (dump-dotenv values)))))
    (concatenate 'string
                 (format nil "~a~%" +magic-text+)
                 (base64url-encode (concatenate '(vector (unsigned-byte 8)) nonce sealed)))))

(defun open-store-text (key text &key (where "the store"))
  "TEXT's values under KEY, or a refusal saying why they do not open."
  (unless (uiop:string-prefix-p (format nil "~a~%" +magic-text+) text)
    (fail "~a is not a sealed store: it does not start with ~a" where +magic-text+))
  (let* ((payload (subseq text (1+ (length +magic-text+))))
         (raw (base64url-decode payload :where where)))
    (when (<= (length raw) +nonce-length+)
      (fail "~a is truncated: it holds no sealed body" where))
    (let ((plain (aes-gcm-open key (subseq raw 0 +nonce-length+) +magic+
                               (subseq raw +nonce-length+))))
      (unless plain
        (fail "~a did not open: the key is wrong or the file was changed" where))
      (parse-dotenv (octets->string plain)))))

;;; --- where things are ---------------------------------------------------------------

(defun as-path (value default)
  "VALUE as a pathname, DEFAULT when it is unset; a relative one is under the home."
  (let ((text (and (stringp value) (string-trim '(#\Space #\Tab) value))))
    (nlk:home (if (and text (plusp (length text))) text default))))

(defun store-path () (as-path (setting :store) +default-store+))
(defun key-path () (as-path (setting :key-file) +default-key-file+))

(defun file-mode (path)
  "PATH's permission bits, or NIL when it is not there."
  (ignore-errors (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring path))))))

(defun mode-private-p (mode)
  "MODE is readable and writable by its owner alone."
  #+win32 (declare (ignore mode))
  #+win32 t
  #-win32 (and mode (zerop (logand mode #o077)) t))

(defun read-bytes (path)
  "PATH's whole content as octets."
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun write-private-file (path text &key (if-exists :supersede))
  "TEXT to PATH, mode 0600, parents made."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists if-exists
                             :if-does-not-exist :create :external-format :utf-8)
    (write-string text out))
  (ignore-errors (sb-posix:chmod (namestring path) #o600))
  path)

(defun copy-private-file (from to)
  "FROM's bytes to TO, mode 0600."
  (ensure-directories-exist to)
  (with-open-file (in from :element-type '(unsigned-byte 8))
    (with-open-file (out to :direction :output :if-exists :supersede
                             :if-does-not-exist :create :element-type '(unsigned-byte 8))
      (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
        (loop for count = (read-sequence buffer in)
              while (plusp count)
              do (write-sequence buffer out :end count)))))
  (ignore-errors (sb-posix:chmod (namestring to) #o600))
  to)

(defun backup-of (path)
  "PATH's backup beside it, the name a person looks for: <path>.bak."
  (pathname (format nil "~a.bak" (namestring path))))

(defun write-store-file (path key values)
  "VALUES under KEY to PATH: one atomic rename, the file 0600."
  (let* ((directory (uiop:pathname-directory-pathname path))
         (tmp (merge-pathnames (format nil ".~a.tmp" (file-namestring path)) directory)))
    (write-private-file tmp (store-text key values))
    (uiop:rename-file-overwriting-target tmp path)
    (ignore-errors (sb-posix:chmod (namestring path) #o600))
    path))

(defun write-key-file (path &optional (key (random-octets +key-length+)))
  "KEY, 32 raw octets, to PATH, mode 0600."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create :element-type '(unsigned-byte 8))
    (write-sequence key out))
  (ignore-errors (sb-posix:chmod (namestring path) #o600))
  key)

(defun read-key-file (&optional (path (key-path)))
  "PATH's key, which must be exactly 32 raw octets."
  (unless (probe-file path)
    (fail "no sealed key at ~a: run /sealed init" path))
  (let ((raw (read-bytes path)))
    (unless (= +key-length+ (length raw))
      (fail "the key file ~a must hold exactly ~d raw bytes, not ~d"
            path +key-length+ (length raw)))
    raw))

(defun read-values (&optional (path (store-path)) (key (read-key-file)))
  "PATH's entries as an alist (NAME . VALUE); an empty store answers NIL."
  (unless (probe-file path)
    (fail "no sealed store at ~a: run /sealed init" path))
  (open-store-text key (uiop:read-file-string path) :where (format nil "~a" path)))

(defun permission-warnings ()
  "A sentence for each of the store and the key that others can read; a
refusal instead when the section's strict_permissions is on."
  (let ((open (loop for path in (list (key-path) (store-path))
                     for mode = (and (probe-file path) (file-mode path))
                     when (and mode (not (mode-private-p mode)))
                       collect (format nil "~a is mode ~o; expected 600" path mode))))
    (when (and open (setting :strict-permissions))
      (fail "~{~a~^; ~}, and the section's strict_permissions is on" open))
    open))

;;; --- the process environment ---------------------------------------------------------

(defvar *loaded* nil
  "The (NAME . WHAT-IT-HELD) pairs this cell put in the environment, newest first.")

(defvar *live* nil
  "T once this cell has read the store into the environment, NIL once it put it back.")

(defvar *getenv* (lambda (name) (uiop:getenv name))
  "How a variable is read; a test binds a hash table's reader.")

(defvar *setenv* (lambda (name value) (sb-posix:setenv name value 1))
  "How a variable is set; a test binds a hash table's writer.")

(defvar *unsetenv* (lambda (name) (sb-posix:unsetenv name))
  "How a variable is removed; a test binds a hash table's remover.")

(defun export-values (values &key (override (setting :override-existing)))
  "VALUES into this process's environment, keeping what each variable held so
UNLOAD-SECRETS can put it back. => the names set, in VALUES' order."
  (let ((names '()))
    (dolist (pair values)
      (let* ((name (car pair))
             (held (funcall *getenv* name)))
        (when (or override (null held))
          (unless (and held (equal held (cdr pair)))
            (funcall *setenv* name (cdr pair)))
          (push (cons name held) *loaded*)
          (push name names))))
    (nreverse names)))

(defun export-value (name value &key (override (setting :override-existing)))
  "One NAME=VALUE into the environment and onto *LOADED*, as EXPORT-VALUES does."
  (let ((held (funcall *getenv* name)))
    (if (or override (null held))
        (progn (unless (and held (equal held value)) (funcall *setenv* name value))
               (push (cons name held) *loaded*)
               t)
        nil)))

(defun unexport-value (name)
  "NAME back to what it held before this cell set it, and off *LOADED*."
  (let ((pair (assoc name *loaded* :test #'string=)))
    (when pair
      (if (cdr pair)
          (funcall *setenv* name (cdr pair))
          (funcall *unsetenv* name))
      (setf *loaded* (remove pair *loaded*))
      t)))

(defun load-secrets ()
  "Read the store and put its variables into this process's environment.
=> (values NAMES WARNINGS), NAMES the variables set."
  (let* ((warnings (permission-warnings))
         (values (read-values)))
    (setf *loaded* nil *live* t)
    (values (export-values values) warnings)))

(defun unload-secrets ()
  "Put back every variable LOAD-SECRETS set, then forget them."
  (let ((loaded *loaded*))
    (setf *loaded* nil *live* nil)
    (dolist (pair loaded)
      (if (cdr pair)
          (funcall *setenv* (car pair) (cdr pair))
          (funcall *unsetenv* (car pair))))
    (length loaded)))

;;; --- the verbs ------------------------------------------------------------------------

(defun init (&key force)
  "Make the key file and the store, keeping what an existing store holds.
With FORCE a new key is made and the old store and key are kept as .bak."
  (let ((store (store-path))
        (key-file (key-path)))
    (when (and (probe-file store) (not force))
      (fail "the store already exists at ~a: /sealed init force makes a new key and keeps its entries"
            store))
    (when (and (probe-file key-file) (not (probe-file store)) (not force))
      (fail "the key exists but the store is missing at ~a: /sealed init force makes a pair of them"
            store))
    (let ((existing '())
          (backups '()))
      (when (probe-file store)
        (let ((old (handler-case (read-key-file key-file) (sealed-error () nil))))
          (setf existing (and old (handler-case (read-values store old)
                                    (sealed-error () '()))))
          (push (copy-private-file store (backup-of store)) backups))
        (when (probe-file key-file)
          (push (copy-private-file key-file (backup-of key-file)) backups)))
      (let ((key (random-octets +key-length+)))
        ;; the store before the key: a crash between them leaves the old key
        ;; readable, never a store nobody can open.
        (write-store-file store key existing)
        (write-key-file key-file key)
        (format nil "sealed: store ~a (~d entries, 0600)~%key:   ~a (0600)~@[~%kept:  ~{~a~^, ~}~]~%next:  /sealed set NAME VALUE"
                store (length existing) key-file backups)))))

(defun set-value (name value)
  "Store NAME=VALUE, creating the store's entry or replacing it; answers a sentence."
  (check-name name)
  (when (or (null value) (zerop (length value)))
    (fail "the value is empty: /sealed unset ~a removes a variable" name))
  (let* ((key (read-key-file))
         (values (read-values (store-path) key)))
    (write-store-file (store-path) key
                      (acons name value (remove name values :key #'car :test #'string=)))
    ;; a variable the store already owned goes live at once, so a key set
    ;; during a session needs no restart to reach a tool.
    (when *live* (export-value name value))
    (format nil "set ~a (~d chars, stored encrypted)" name (length value))))

(defun unset (name)
  "Remove NAME from the store; answers a sentence."
  (check-name name)
  (let* ((key (read-key-file))
         (values (read-values (store-path) key)))
    (unless (assoc name values :test #'string=)
      (fail "~a is not in the store" name))
    (write-store-file (store-path) key (remove name values :key #'car :test #'string=))
    (unexport-value name)
    (format nil "removed ~a" name)))

(defun names ()
  "The store's variable names, sorted; never a value."
  (sort (mapcar #'car (read-values)) #'string<))

(defun value (name)
  "NAME's value in clear, or a refusal when the store does not hold it."
  (check-name name)
  (or (cdr (assoc name (read-values) :test #'string=))
      (fail "no ~a in the store" name)))

(defun rotate ()
  "Re-encrypt the store under a new key, keeping the previous key as .bak."
  (let* ((store (store-path))
         (key-file (key-path))
         (old (read-key-file key-file))
         (values (read-values store old))
         (backup (copy-private-file key-file (backup-of key-file)))
         (new (random-octets +key-length+)))
    (write-store-file store new values)
    (write-key-file key-file new)
    (format nil "rotated the key for ~d entries; the previous key is at ~a~%delete the backup once a start reads the store"
            (length values) backup)))

(defun status (&key (open t))
  "The paths, their modes, the entry count and what is loaded, as text a person reads.
With OPEN false the store is not opened, so no key is needed to read it."
  (with-output-to-string (out)
    (format out "store: ~a~%key:   ~a~%" (store-path) (key-path))
    (dolist (pair (list (cons "store" (store-path)) (cons "key" (key-path))))
      (let ((path (cdr pair)))
        (cond
          ((not (probe-file path))
           (format out "  ~a: missing~%" (car pair)))
          (t
           (let ((mode (file-mode path)))
             (format out "  ~a: mode ~o ~a~%" (car pair) (or mode 0)
                     (if (mode-private-p mode) "ok" "TOO OPEN (want 600)")))))))
    (if open
        (multiple-value-bind (count problem)
            (handler-case (values (length (read-values)) nil)
              (sealed-error (condition) (values nil (format nil "~a" condition))))
          (format out "entries: ~@[~d~]~@[~%  ~a~]~%" count problem))
        (format out "entries: (not read)~%"))
    (format out "loaded: ~d~@[ (~{~a~^, ~})~]~%"
            (length *loaded*) (sort (mapcar #'car *loaded*) #'string<))
    (let ((warnings (handler-case (permission-warnings) (sealed-error () nil))))
      (dolist (warning warnings) (format out "warning: ~a~%" warning)))))
