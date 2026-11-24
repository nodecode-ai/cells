;;;; api.lisp --- the Proton wire, the session, and gpg for the private keys.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two seams, both replaceable so the tests never touch the machine:
;;;; *HTTP* is (METHOD URL HEADERS CONTENT TIMEOUT) => (values BODY STATUS),
;;;; *GPG* is (ARGS &key INPUT) => (values OUTPUT STATUS). Everything else is
;;;; the cell's own: the SRP login, the token refresh, and the request shapes
;;;; Proton's API answers.

(in-package #:nodecode-protonmail)

(defparameter *http* nil "The HTTP seam; NIL means NLK:HTTP.")
(defparameter *gpg* nil "The gpg seam; NIL means a real gpg on a temp homedir.")
(defparameter *session* nil "The live session, or NIL before the first login.")
(defparameter *gpg-home* nil "The temp GNUPGHOME the account's keys were imported into.")
(defparameter *imported* nil "The key IDs already imported into *GPG-HOME*.")
(defparameter *sending-address* nil "The address mail is sent as, once it is known.")

(defparameter +accept+ "application/vnd.protonmail.v1+json")

;;; --- the wire ------------------------------------------------------------------

(defun http (method url &key headers content binary)
  (let ((caller (or *http* #'default-http)))
    (funcall caller method url headers content (conf :timeout) binary)))

(defun default-http (method url headers content timeout binary)
  (multiple-value-bind (body status)
      (nlk:http method url
                :headers (append headers (list (cons "accept" +accept+)
                                               (cons "content-type" "application/json")))
                :content content
                :binary binary
                :timeout timeout
                :connect-timeout (min 10 timeout))
    (values (if (or binary (stringp body)) body (nlk:body-text body)) status)))

(defun base-url () (string-right-trim "/" (conf :host)))

(defun request (method path &key content authenticated-p)
  "PATH's decoded answer, or a refusal carrying Proton's own words."
  (let* ((url (concatenate 'string (base-url) path))
         (headers (append (list (cons "x-pm-appversion" (conf :app-version))
                                (cons "user-agent" "nodecode-protonmail/0.1"))
                          (when (and authenticated-p *session* (session-token *session*))
                            (list (cons "authorization"
                                        (format nil "Bearer ~a" (session-token *session*)))
                                  (cons "x-pm-uid" (session-uid *session*)))))))
    (multiple-value-bind (body status) (http method url :headers headers :content content)
      (let ((text (nlk:trimmed (or body ""))))
        (when (and (>= status 400) authenticated-p *session* (session-refresh *session*)
                   (search "\"Code\":10013" (remove #\Space text)))
          (refresh *session*)
          (return-from request (request method path :content content :authenticated-p t)))
        (if (< status 400)
            (if (zerop (length text)) (nlk:json-object) (or (ignore-errors (nlk:decode-json text)) text))
            (fail "Proton answered HTTP ~a on ~a ~a~@[: ~a~]" status method path
                  (nlk:one-line (or text "")))))) ))

;;; --- the session ------------------------------------------------------------------

(defstruct (session (:constructor make-session))
  uid token refresh username passphrase)

(defun login ()
  "Log in with the settings' address and password, and keep the session."
  (let* ((username (conf :username))
         (password (map '(vector (unsigned-byte 8)) #'char-code (password-text)))
         (info (request "POST" "/auth/v4/info"
                        :content (encode-object (list (cons "Username" username))))))
    (let* ((salt (cl-base64:base64-string-to-usb8-array (json info "Salt")))
           (modulus (cl-base64:base64-string-to-usb8-array (json info "Modulus")))
           (server (cl-base64:base64-string-to-usb8-array (json info "ServerEphemeral")))
           (proofs (srp-proofs :password password :salt salt
                                :server-ephemeral server :signed-modulus modulus))
           (payload (list (cons "Username" username)
                          (cons "ClientEphemeral" (cl-base64:usb8-array-to-base64-string
                                                   (proofs-client-ephemeral proofs)))
                          (cons "ClientProof" (cl-base64:usb8-array-to-base64-string
                                               (proofs-client-proof proofs)))
                          (cons "SRPSession" (json info "SRPSession"))
                          (cons "Version" (json info "Version"))))
           (answer (request "POST" "/auth/v4" :content (encode-object payload))))
      (setf *session* (make-session :uid (json answer "UID")
                                     :token (json answer "AccessToken")
                                     :refresh (json answer "RefreshToken")
                                     :username username))
      (setf *imported* nil)
      (setf (session-passphrase *session*) (key-passphrase-on-server password))
      *session*)))

(defun key-passphrase-on-server (password)
  "The one passphrase every key of this account opens with, from the primary key's salt."
  (let* ((salts (request "GET" "/core/v4/keys/salts" :authenticated-p t))
         (user (request "GET" "/core/v4/users" :authenticated-p t))
         (keys (jlist (jget user "User") "Keys"))
         (primary (and (plusp (length keys)) (aref keys 0)))
         (id (and primary (json primary "ID"))))
    (let ((salt (loop for entry across (jlist salts "KeySalts")
                      when (equal id (json entry "ID"))
                        return (json entry "KeySalt"))))
      (unless salt (fail "Proton did not return a salt for the primary key"))
      (map 'string #'code-char
           (key-passphrase password (cl-base64:base64-string-to-usb8-array salt))))))

(defun ensure-session ()
  (or *session* (login)))

(defun refresh (session)
  "Trade the refresh token for a new access token."
  (let ((answer (request "POST" "/auth/v4/refresh"
                         :content (nlk:encode-json-object
                                   (list (cons "UID" (session-uid session))
                                         (cons "RefreshToken" (session-refresh session))
                                         (cons "ResponseType" "token")
                                         (cons "GrantType" "refresh_token")
                                         (cons "RedirectURI" "https://protonmail.ch"))))))
    (setf (session-token session) (json answer "AccessToken"))
    (setf (session-refresh session) (json answer "RefreshToken"))
    session))

;;; --- addresses and keys -----------------------------------------------------------

(defun addresses ()
  (let ((answer (request "GET" "/core/v4/addresses" :authenticated-p t)))
    (coerce (jlist answer "Addresses") 'list)))

(defun address-keys (address)
  "The armoured private keys of one address, with the passphrase that opens them."
  (let ((passphrase (session-passphrase (ensure-session))))
    (loop for key across (jlist address "Keys")
          for armored = (json key "PrivateKey")
          when (and armored (plusp (length armored)))
            collect (cons armored passphrase))))

(defun request-bytes (path &key authenticated-p)
  "PATH's body as octets: an attachment is not JSON."
  (let* ((url (concatenate 'string (base-url) path))
         (headers (append (list (cons "x-pm-appversion" (conf :app-version)))
                          (when (and authenticated-p *session*)
                            (list (cons "authorization"
                                        (format nil "Bearer ~a" (session-token *session*)))
                                  (cons "x-pm-uid" (session-uid *session*)))))))
    (multiple-value-bind (body status) (http "GET" url :headers headers :binary t)
      (if (< status 400)
          (if (stringp body)
              (map '(vector (unsigned-byte 8)) #'char-code body)
              body)
          (fail "Proton answered HTTP ~a on ~a" status path)))))

;;; --- the address mail goes out as -------------------------------------------------

(defun sending-address ()
  "The address this account sends as: verified, allowed to send, and keyed."
  (or *sending-address*
      (let ((usable (remove-if-not
                     (lambda (address)
                       (and (eql 1 (json address "Status" :integer))
                            (eql 1 (json address "Send" :integer))
                            (plusp (length (jlist address "Keys")))))
                     (addresses))))
        (unless usable (fail "no address on this account can send mail"))
        (setf *sending-address* (first usable)))))

;;; --- gpg, on a throwaway homedir ------------------------------------------------

(defun run-gpg (args &key input password)
  "ARGS run through gpg, INPUT written to a temp file and named by \"--input\".
=> (values OUTPUT STATUS)."
  (if *gpg* (funcall *gpg* args :input input :password password)
      (let* ((home (or *gpg-home* (setf *gpg-home* (gpg-home))))
             (file (when input
                     (let ((path (merge-pathnames
                                  (format nil "pmail-~a.bin" (random 1000000))
                                  (uiop:temporary-directory))))
                       (with-open-file (out path :direction :output :if-exists :supersede
                                                 :element-type '(unsigned-byte 8))
                         (write-sequence input out))
                       path)))
             (pass (when password
                     (let ((path (merge-pathnames
                                  (format nil "pmail-~a.pass" (random 1000000))
                                  (uiop:temporary-directory))))
                       (with-open-file (out path :direction :output :if-exists :supersede)
                         (write-string password out))
                       path))))
        (unwind-protect
             (uiop:run-program
              (append (list (conf :gpg-program) "--homedir" (namestring home)
                            "--batch" "--yes" "--no-tty" "--pinentry-mode" "loopback")
                      (when pass (list "--passphrase-file" (namestring pass)))
                      (when file (list "--input" (namestring file)))
                      args)
              :output :string :error-output :string :ignore-error-status t)
          (when file (ignore-errors (delete-file file)))
          (when pass (ignore-errors (delete-file pass)))))))

(defun run-gpg-binary (args input password)
  "ARGS run through gpg over INPUT, the plaintext read back as octets."
  (when *gpg*
    (multiple-value-bind (out status) (funcall *gpg* args :input input :password password)
      (return-from run-gpg-binary
        (if (and (integerp status) (zerop status))
            (map '(vector (unsigned-byte 8)) #'char-code (or out ""))
            (fail "gpg answered ~a" status)))))
  (let* ((home (or *gpg-home* (setf *gpg-home* (gpg-home))))
         (in (merge-pathnames (format nil "pmail-~a.in" (random 1000000))
                              (uiop:temporary-directory)))
         (out (merge-pathnames (format nil "pmail-~a.out" (random 1000000))
                               (uiop:temporary-directory)))
         (pass (when password
                 (let ((path (merge-pathnames (format nil "pmail-~a.pass" (random 1000000))
                                               (uiop:temporary-directory))))
                   (with-open-file (s path :direction :output :if-exists :supersede)
                     (write-string password s))
                   path))))
    (with-open-file (s in :direction :output :if-exists :supersede
                          :element-type '(unsigned-byte 8))
      (write-sequence input s))
    (unwind-protect
         (let ((status
                 (nth-value 2
                            (uiop:run-program
                             (append (list (setting-program :gpg) "--homedir" (namestring home)
                                           "--batch" "--yes" "--no-tty" "--pinentry-mode" "loopback"
                                           "--output" (namestring out))
                                     (when pass (list "--passphrase-file" (namestring pass)))
                                     args)
                             :input in :output :string :error-output :string
                             :ignore-error-status t))))
           (if (and (integerp status) (zerop status))
               (with-open-file (s out :element-type '(unsigned-byte 8))
                 (let ((octets (make-array (file-length s) :element-type '(unsigned-byte 8))))
                   (read-sequence octets s)
                   octets))
               (fail "gpg could not decrypt the attachment")))
      (ignore-errors (delete-file in))
      (ignore-errors (delete-file out))
      (when pass (ignore-errors (delete-file pass))))))

(defun setting-program (key)
  "KEY of the section as a program name, so a test can name one."
  (or (conf key) (conf :gpg-program)))

(defun gpg-home ()
  (let ((path (merge-pathnames (format nil "pmail-gpg-~a/" (random 1000000))
                                (uiop:temporary-directory))))
    (ensure-directories-exist path)
    (sb-posix:chmod (namestring path) #o700)
    path))

(defun ensure-keys-imported ()
  "Import every address key once per session: gpg needs a keyring to decrypt with."
  (unless *imported*
    (dolist (address (addresses))
      (dolist (bundle (address-keys address))
        (run-gpg (list "--import") :input (map '(vector (unsigned-byte 8))
                                                #'char-code (car bundle)))))
    (setf *imported* t)))

(defun decrypt-armored-octets (blob)
  "BLOB (a key packet and a data packet, or one armoured message) decrypted.
=> the plaintext octets, which may be anything at all."
  (ensure-session)
  (ensure-keys-imported)
  (run-gpg-binary (list "--decrypt") blob (session-passphrase *session*)))

(defun decrypt-armored (armored)
  "ARMORED decrypted with the account's own keys => the plaintext octets."
  (ensure-session)
  (ensure-keys-imported)
  (babel:octets-to-string (decrypt-armored-octets
                           (map '(vector (unsigned-byte 8)) #'char-code armored))
                          :encoding :utf-8))

;;; --- JSON, text ------------------------------------------------------------------

(defun jget (object key)
  "KEY of OBJECT when OBJECT is a decoded JSON object, else NIL."
  (and (hash-table-p object) (gethash key object)))

(defun jlist (object key)
  "KEY of OBJECT as a simple vector, empty when it is not an array."
  (let ((value (jget object key)))
    (cond ((typep value '(simple-array t (*))) value)
          ((vectorp value) value)
          ((listp value) (coerce value 'vector))
          (t #()))))

(defun jsonify (value)
  "VALUE with every alist turned into a JSON object, and every vector walked.

An alist is what this file builds; shasht wants objects, so the conversion
happens once, here, instead of at every call site."
  (cond ((and (listp value) (plusp (length value)) (every #'consp value))
         (let ((object (nlk:json-object)))
           (dolist (pair value object)
             (setf (gethash (car pair) object) (jsonify (cdr pair))))))
        ((and (vectorp value) (not (stringp value))) (map 'vector #'jsonify value))
        (t value)))

(defun encode-object (alist)
  "ALIST (a list of (key . value)) as a JSON object string."
  (nlk:encode-json-object (jsonify alist)))

(defun json (object key &optional (type :string))
  "KEY of OBJECT as TYPE, or NIL."
  (let ((value (cond ((hash-table-p object) (gethash key object))
                     ((and (vectorp object) (not (stringp object))) nil)
                     (t nil))))
    (cond ((eq type :string) (and (stringp value) value))
          ((eq type :integer) (and (integerp value) value))
          ((eq type :boolean) (if value t nil))
          (t value))))

(defun replace-all (text old new)
  "TEXT with every OLD replaced by NEW."
  (let ((out (make-string-output-stream))
        (start 0))
    (loop for at = (search old text :start2 start)
          do (if at
                 (progn (write-string text out :start start :end at)
                        (write-string new out)
                        (setf start (+ at (length old))))
                 (progn (write-string text out :start start) (return))))
    (get-output-stream-string out)))

(defun collapse-space (text)
  "TEXT with every run of whitespace (a newline included) as one space, trimmed."
  (let ((out (make-string-output-stream))
        (space nil))
    (loop for char across (or text "")
          do (if (member char '(#\Space #\Tab #\Newline #\Return))
                 (setf space t)
                 (progn (when (and space (plusp (file-position out))) (write-char #\Space out))
                        (setf space nil)
                        (write-char char out))))
    (get-output-stream-string out)))

(defun html-to-text (text)

  "HTML as prose: tags dropped, the usual entities decoded, runs of blank space collapsed."
  (let ((out (make-string-output-stream))
        (in-tag nil))
    (loop for char across text
          do (cond ((char= char #\<) (setf in-tag t))
                   ((char= char #\>) (setf in-tag nil) (write-char #\Space out))
                   ((not in-tag) (write-char char out))))
    (let ((plain (get-output-stream-string out)))
      (loop for (entity . char) in '(("&nbsp;" . " ") ("&amp;" . "&") ("&lt;" . "<")
                                     ("&gt;" . ">") ("&quot;" . "\"") ("&#39;" . "'")
                                     ("&apos;" . "'"))
            do (setf plain (replace-all plain entity char)))
      (collapse-space plain))))

(defun body-text (plaintext)
  "The body out of what gpg decrypted: Proton stores headers, a blank line, then it."
  (let* ((text (or (ignore-errors (babel:octets-to-string plaintext :encoding :utf-8))
                    plaintext))
         (split (search (format nil "~c~c~c~c" #\Return #\Newline #\Return #\Newline) text)))
    (let ((body (if split (subseq text (+ split 4)) text)))
      (if (search "<" body) (html-to-text body) (collapse-space body)))))
