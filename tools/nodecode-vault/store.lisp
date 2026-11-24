;;;; store.lisp --- the encrypted store, its key custody, and the hv: refs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One file holds every item, sealed as a whole: AES-256-GCM under a
;;;; 32-byte key the store does not carry. The key is the environment
;;;; variable the section's key_env names, else the 64 hex characters in the
;;;; file key_file names (vault/master.key by default, written 0600 by
;;;; SETUP). Nothing is created by a read: a missing key or store is a
;;;; refusal that names the way back, never a prompt and never a new key.
;;;;
;;;; A ref is hv:<item>[:<field>], the same spelling the original uses.

(in-package #:nodecode-vault)

(defparameter +store-format+ 1)
(defparameter +key-length+ 32)
(defparameter +nonce-length+ 12)
(defparameter +tag-length+ 16)
(defparameter +id-characters+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-")
(defparameter +env-name-characters+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
(defparameter +default-store+ "vault/agent-vault.sexp")
(defparameter +default-key-file+ "vault/master.key")

(defvar *last-error* nil
  "The last refusal a verb answered, for /vault doctor.")

;;; --- octets -------------------------------------------------------------

(defun random-octets (count) (ironclad:random-data count))
(defun string->octets (text) (sb-ext:string-to-octets text :external-format :utf-8))
(defun octets->string (octets) (sb-ext:octets-to-string octets :external-format :utf-8))

(defun octets->hex (octets)
  "OCTETS as lowercase hex, two characters an octet."
  (format nil "~{~2,'0x~}" (coerce octets 'list)))

(defun hex->octets (text)
  "TEXT, an even run of hex characters, as octets."
  (unless (and (stringp text) (evenp (length text))
               (every (lambda (c) (digit-char-p c 16)) text))
    (fail "the store file is damaged: a sealed field is not hex"))
  (let ((octets (make-array (/ (length text) 2) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length text) by 2
          for j from 0
          do (setf (aref octets j) (parse-integer text :start i :end (+ i 2) :radix 16)))
    octets))

;;; --- the seal ------------------------------------------------------------

(defun seal (key nonce plaintext)
  "PLAINTEXT under AES-256-GCM: the ciphertext then its 16-octet tag."
  (let ((cipher (ironclad:make-authenticated-encryption-mode
                 :gcm :cipher-name :aes :key key :initialization-vector nonce)))
    (concatenate '(vector (unsigned-byte 8))
                 (ironclad:encrypt-message cipher plaintext)
                 (ironclad:produce-tag cipher))))

(defun unseal (key nonce sealed)
  "The plaintext under SEALED, or a refusal when the tag does not verify."
  (handler-case
      (let* ((size (length sealed))
             (cipher (ironclad:make-authenticated-encryption-mode
                      :gcm :cipher-name :aes :key key :initialization-vector nonce))
             (plain (ironclad:decrypt-message cipher (subseq sealed 0 (- size +tag-length+)))))
        (if (equalp (ironclad:produce-tag cipher) (subseq sealed (- size +tag-length+)))
            plain
            (fail "the store did not open: the key is wrong or the file was changed")))
    (error ()
      (fail "the store did not open: the key is wrong or the file was changed"))))

;;; --- where things are ----------------------------------------------------

(defun as-path (value default)
  "VALUE as a pathname, DEFAULT when it is unset; a relative one is under the home."
  (let ((text (and (stringp value) (string-trim " " value))))
    (nlk:home (if (and text (plusp (length text))) text default))))

(defun store-path () (as-path (setting :store) +default-store+))
(defun key-file-path () (as-path (setting :key-file) +default-key-file+))

(defun parse-key (text &key (where "the vault key"))
  "TEXT as a +KEY-LENGTH+ octet key, or a refusal naming WHERE."
  (let ((hex (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (unless (= (* 2 +key-length+) (length hex))
      (fail "~a must be ~d hex characters, not ~d" where (* 2 +key-length+) (length hex)))
    (hex->octets hex)))

(defun store-key ()
  "The key the section's custody names: key_env first, then the key file."
  (let ((name (setting :key-env)))
    (if (and (stringp name) (plusp (length name)))
        (let ((value (uiop:getenv name)))
          (unless (and value (plusp (length value)))
            (fail "no vault key: the environment variable ~a is not set" name))
          (parse-key value :where (format nil "~a" name)))
        (let ((path (key-file-path)))
          (unless (probe-file path)
            (fail "no vault key at ~a: run /vault setup once" path))
          (parse-key (uiop:read-file-string path)
                     :where (format nil "the key file ~a" path))))))

;;; --- the file ------------------------------------------------------------

(defun seal-text (key items)
  "The store file's whole text: the nonce and the sealed items, as hex."
  (let* ((plain (string->octets (let ((*print-readably* t)) (prin1-to-string items))))
         (nonce (random-octets +nonce-length+)))
    (format nil "(:format ~d :nonce ~s :sealed ~s)~%"
            +store-format+ (octets->hex nonce) (octets->hex (seal key nonce plain)))))

(defun open-text (key text &key (where "the store"))
  "The items TEXT seals under KEY; a refusal when anything is off."
  (let ((form (handler-case (let ((*read-eval* nil)) (read-from-string text))
                (error () (fail "~a is not readable: the file is damaged" where)))))
    (unless (and (consp form) (eql +store-format+ (getf form :format)))
      (fail "~a is not a vault store this cell knows (format ~s)" where (getf form :format)))
    (let ((plain (unseal key (hex->octets (getf form :nonce)) (hex->octets (getf form :sealed)))))
      (handler-case (let ((*read-eval* nil)) (read-from-string (octets->string plain)))
        (error () (fail "~a opened but its contents are not readable" where))))))

(defun read-store (&optional key)
  "Every item in the store, or NIL when there is none yet. Reads only."
  (let ((path (store-path)))
    (if (probe-file path)
        (open-text (or key (store-key)) (uiop:read-file-string path)
                   :where (format nil "~a" path))
        '())))

(defun write-store (key items)
  "Seal ITEMS under KEY into the store, one atomic rename, 0600."
  (let* ((path (store-path))
         (tmp (merge-pathnames (format nil "~a.tmp" (file-namestring path))
                                (uiop:pathname-directory-pathname path))))
    (ensure-directories-exist path)
    (with-open-file (out tmp :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string (seal-text key items) out))
    (uiop:rename-file-overwriting-target tmp path)
    (ignore-errors (sb-posix:chmod (namestring path) #o600))
    items))

(defun write-key (path)
  "A fresh key at PATH, 64 hex characters and a newline, readable by its owner only."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (format out "~a~%" (octets->hex (random-octets +key-length+))))
  (ignore-errors (sb-posix:chmod (namestring path) #o600))
  path)

;;; --- items and refs ------------------------------------------------------

(defun id-ok-p (id)
  (and (stringp id) (<= 1 (length id) 128)
       (every (lambda (c) (find c +id-characters+)) id)))

(defun check-id (id &key (what "item id"))
  (unless (id-ok-p id)
    (fail "~a must be 1-128 letters, numbers, dots, underscores or hyphens" what))
  id)

(defun env-name-ok-p (name)
  (and (stringp name) (plusp (length name))
       (every (lambda (c) (find c +env-name-characters+)) name)))

(defun item (items id) (find id items :key (lambda (one) (getf one :id)) :test #'equal))

(defun fields (one) (getf one :fields))

(defun field-value (one field)
  (cdr (assoc field (fields one) :test #'equal)))

(defun set-field (one field value)
  (let ((cell (assoc field (fields one) :test #'equal)))
    (if cell
        (setf (cdr cell) value)
        (setf (getf one :fields) (append (fields one) (list (cons field value)))))
    one))

(defun parse-ref (ref &key (what "ref"))
  "hv:<item>[:<field>] => (values ID FIELD-OR-NIL); a refusal otherwise."
  (unless (and (stringp ref) (uiop:string-prefix-p "hv:" ref))
    (fail "~a must be hv:<item>[:<field>], not ~s" what ref))
  (let* ((body (subseq ref 3))
         (colon (position #\: body)))
    (values (check-id (if colon (subseq body 0 colon) body))
            (and colon (let ((field (subseq body (1+ colon))))
                         (if (plusp (length field)) field
                             (fail "~a names an empty field" what)))))))

;;; --- the verbs -----------------------------------------------------------

(defun setup ()
  "Create the key file and an empty store when they are not there yet."
  (let ((key-path (key-file-path)) (path (store-path)))
    (unless (or (probe-file key-path)
                (let ((name (setting :key-env)))
                  (and (stringp name) (plusp (length name)) (uiop:getenv name))))
      (write-key key-path))
    (let ((key (store-key)))
      (unless (probe-file path)
        (write-store key '()))
      (format nil "vault ready: ~a~%key: ~a~%store: ~a"
              (if (probe-file path) "yes" "no") (key-file-path) path))))

(defun items ()
  "Every item as (:id :label :fields), values included."
  (read-store (store-key)))

(defun put (id field value &key label)
  "Set FIELD of item ID to VALUE, creating the item; answers a sentence."
  (check-id id)
  (check-id field :what "field name")
  (when (or (null value) (zerop (length value)))
    (fail "the value is empty"))
  (let* ((key (store-key))
         (all (read-store key))
         (one (or (item all id) (car (setf all (append all (list (list :id id :fields '()))))))))
    (when (and label (plusp (length label))) (setf (getf one :label) label))
    (set-field one field value)
    (write-store key all)
    (format nil "stored ~a.~a" id field)))

(defun secret (ref)
  "The value REF names; a refusal when there is no such item or field."
  (multiple-value-bind (id field) (parse-ref ref :what "the reference")
    (let* ((one (item (items) id)))
      (unless one (fail "no vault item ~a" id))
      (cond
        (field
         (let ((value (field-value one field)))
           (unless value (fail "vault item ~a has no field ~a" id field))
           value))
        ((field-value one "value") (field-value one "value"))
        ((= 1 (length (fields one))) (cdr (first (fields one))))
        (t (fail "vault item ~a holds ~d fields: name one with hv:~a:<field>"
                 id (length (fields one)) id))))))

(defun forget (id)
  "Remove item ID; answers a sentence."
  (check-id id)
  (let* ((key (store-key))
         (all (read-store key)))
    (unless (item all id) (fail "no vault item ~a" id))
    (write-store key (remove id all :key (lambda (one) (getf one :id)) :test #'equal))
    (format nil "removed ~a" id)))

(defun mapping ()
  "The section's NAME=hv:... pairs as (NAME . REF), each checked."
  (loop for pair in (setting :env)
        for text = (princ-to-string pair)
        for equals = (position #\= text)
        do (unless equals (fail "vault.env entry ~s is not NAME=hv:<item>[:<field>]" text))
           (unless (env-name-ok-p (subseq text 0 equals))
             (fail "vault.env entry ~s does not name an environment variable" text))
           (parse-ref (subseq text (1+ equals)) :what (format nil "vault.env ~a" (subseq text 0 equals)))
        collect (cons (subseq text 0 equals) (subseq text (1+ equals)))))

;;; --- a command under the vault's variables ---------------------------------

(defun process-environment ()
  "This process's environment, one NAME=VALUE string each; a bare PATH when
this SBCL cannot hand them over."
  (or (ignore-errors (sb-ext:posix-environ))
      (list "PATH=/usr/local/bin:/usr/bin:/bin")))

(defun shell-with-env (command env)
  "Run COMMAND through /bin/sh with ENV, an alist of (NAME . VALUE), laid over
this process's own environment. => the command's output as one string."
  (let* ((names (mapcar #'car env))
         (kept (remove-if (lambda (entry)
                            (member (subseq entry 0 (or (position #\= entry) (length entry)))
                                    names :test #'string=))
                          (process-environment)))
         (pairs (append kept (mapcar (lambda (one) (format nil "~a=~a" (car one) (cdr one))) env))))
    (uiop:run-program (list "/bin/sh" "-c" command)
                       :output :string :error-output :output
                       :environment pairs :ignore-error-status t)))

(defvar *shell-with-env* #'shell-with-env
  "What RUN calls; a test binds it to record the environment it was given.")

(defun run (command)
  "Run COMMAND with the section's mapped variables in its environment, so a
secret reaches the command without being printed. => its output."
  (let ((env (loop for (name . ref) in (mapping) collect (cons name (secret ref)))))
    (unless env
      (fail "the vault maps no variables: set vault.env to NAME=hv:<item>[:<field>] first"))
    (funcall *shell-with-env* command env)))
