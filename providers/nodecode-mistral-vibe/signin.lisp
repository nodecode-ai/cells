;;;; signin.lisp --- the browser sign-in: the key it mints, and where it is kept.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from nebneo's hermes-mistral-vibe (see NOTICE):
;;;; vibe_provider.py's _login, auth_handler and _json_request.
;;;;
;;;; The console runs a PKCE flow with no redirect: one POST opens a process
;;;; and answers a sign-in address to show, an address to poll, and a process
;;;; id; the browser posts back to the console; the poll turns `completed' and
;;;; hands over an exchange token; the verifier behind the challenge buys the
;;;; API key at the process's exchange endpoint. Everything after the first
;;;; POST is on the console's own origin, which the plugin checks the way its
;;;; browser and poll addresses are checked here.
;;;;
;;;; The key is kept in auth.json's api_keys (NLE::SAVE-PROVIDER-API-KEY), so
;;;; a later turn reads it before the :CREDENTIAL hook runs, exactly as a key
;;;; saved with /connect is read. A MISTRAL_VIBE_API_KEY in the environment
;;;; still outranks a saved one (RESOLVE-PROVIDER-CREDENTIAL's order).

(in-package #:nodecode-mistral-vibe)

(defparameter +sign-in-seconds+ 300
  "How long a sign-in waits for the browser: the plugin's five-minute cap.")

(defparameter +poll-seconds+ 2
  "How long between two polls of the console.")

(defparameter +base64url-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

;;; --- the challenge ----------------------------------------------------------------

(defun base64url (octets)
  "OCTETS as base64url without its padding, the way PKCE spells a challenge."
  (with-output-to-string (out)
    (loop for start from 0 below (length octets) by 3
          for left = (- (length octets) start)
          for a = (aref octets start)
          for b = (if (>= left 2) (aref octets (1+ start)) 0)
          for c = (if (>= left 3) (aref octets (+ start 2)) 0)
          do (write-char (char +base64url-alphabet+ (ash a -2)) out)
             (write-char (char +base64url-alphabet+
                               (logior (ash (logand a 3) 4) (ash b -4)))
                         out)
             (when (>= left 2)
               (write-char (char +base64url-alphabet+
                                 (logior (ash (logand b 15) 2) (ash c -6)))
                           out))
             (when (>= left 3)
               (write-char (char +base64url-alphabet+ (logand c 63)) out)))))

(defun sha256-octets (text)
  "TEXT's SHA-256 digest as its 32 bytes."
  (let ((hex (nlk:sha256-text text)))
    (loop with digest = (make-array 32 :element-type '(unsigned-byte 8))
          for at from 0 below 32
          do (setf (aref digest at)
                   (parse-integer hex :start (+ 7 (* 2 at)) :end (+ 9 (* 2 at)) :radix 16))
          finally (return digest))))

(defun pkce-verifier ()
  "A fresh code verifier: 48 random bytes, base64url, 64 characters."
  (base64url (nlk:random-bytes 48)))

(defun pkce-challenge (verifier)
  "VERIFIER's S256 challenge."
  (base64url (sha256-octets verifier)))

;;; --- one JSON exchange -------------------------------------------------------------

(defun json-call (method url &key content token)
  "(values OBJECT STATUS) of one JSON exchange: OBJECT when the body parsed as
an object, STATUS the HTTP status (NIL when nothing answered)."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http method url
                    :headers (append '(("accept" . "application/json"))
                                     (when content '(("content-type" . "application/json")))
                                     (when token
                                       (list (cons "authorization" (format nil "Bearer ~a" token)))))
                    :content content :timeout 15 :connect-timeout 10)
        (values (let ((text (nlk:body-text body)))
                  (and (stringp text) (plusp (length text)) (ignore-errors (nlk:decode-json text))))
                status))
    (error () (values nil nil))))

(defun post-json (url object token)
  "(values OBJECT STATUS) of OBJECT, sent to URL as JSON."
  (json-call :post url :content (shasht:write-json object nil) :token token))

(defun ok-p (status)
  "Whether STATUS is a 2xx."
  (and (integerp status) (<= 200 status 299)))

;;; --- the checks the plugin makes on the console's answers --------------------------

(defun console-url-p (url)
  "Whether URL is an https address on the console's own origin: no fragment,
no control character, no backslash. What the plugin requires of both the
sign-in address and the poll address before it opens either."
  (and (stringp url)
       (plusp (length url))
       (every (lambda (char) (>= (char-code char) 33)) url)
       (null (find #\\ url))
       (let ((uri (ignore-errors (quri:uri url))))
         (and uri
              (equal "https" (quri:uri-scheme uri))
              (equal "console.mistral.ai" (quri:uri-host uri))
              (null (quri:uri-fragment uri))))))

(defun process-id-p (id)
  "Whether ID is one the console minted: 1 to 128 of [A-Za-z0-9_-]."
  (and (stringp id)
       (plusp (length id))
       (<= (length id) 128)
       (every (lambda (char)
                (find char "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-"))
              id)))

(defun started-process (object)
  "OBJECT, the console's answer to a sign-in request, as a plist (:sign-in-url
:poll-url :process-id :verifier); a hole in it signals."
  (let ((sign-in (nlk:json-value object :string "sign_in_url"))
        (poll (nlk:json-value object :string "poll_url"))
        (id (nlk:json-value object :string "process_id")))
    (unless (console-url-p sign-in) (error "The console's sign-in address is not its own: ~s" sign-in))
    (unless (console-url-p poll) (error "The console's poll address is not its own: ~s" poll))
    (unless (process-id-p id) (error "The console's process id is not one it could have minted: ~s" id))
    (list :sign-in-url sign-in :poll-url poll :process-id id)))

(defun start-process ()
  "Open a sign-in at the console: a plist (:sign-in-url :poll-url
:process-id :verifier)."
  (let ((verifier (pkce-verifier)))
    (multiple-value-bind (object status)
        (post-json (concatenate 'string +console+ "/api/vibe/sign-in")
                   (nlk:json-object "code_challenge" (pkce-challenge verifier)
                                    "code_challenge_method" "S256")
                   nil)
      (unless (ok-p status)
        (error "The console refused a sign-in: HTTP ~a" status))
      (let ((process (started-process object)))
        (setf (getf process :verifier) verifier)
        process))))

(defun exchange (process exchange-token)
  "The API key the console mints for PROCESS with EXCHANGE-TOKEN."
  (unless (and (stringp exchange-token) (plusp (length exchange-token)))
    (error "The sign-in carried no exchange token"))
  (multiple-value-bind (object status)
      (post-json (format nil "~a/api/vibe/sign-in/~a/exchange"
                         +console+ (getf process :process-id))
                 (nlk:json-object "exchange_token" exchange-token
                                  "code_verifier" (getf process :verifier))
                 nil)
    (unless (ok-p status)
      (error "The console refused the exchange: HTTP ~a" status))
    (let ((key (nlk:json-value object :string "api_key")))
      (unless (and (stringp key) (plusp (length (string-trim " " key))))
        (error "The exchange answered no API key"))
      key)))

(defun identity-of (key)
  "Who KEY belongs to, as the console tells it, or NIL."
  (multiple-value-bind (object status)
      (json-call :get (concatenate 'string +console+ "/api/vibe/whoami") :token key)
    (when (ok-p status)
      (or (nlk:json-value object :string "name")
          (nlk:json-value object :string "email")
          (nlk:json-value object :string "user" "name")))))

;;; --- the wait ---------------------------------------------------------------------

(defun await-key (process cancelled-p)
  "Poll PROCESS until the browser completes it: (values KEY IDENTITY), or a
condition when it is refused, expires or is cancelled."
  (loop with deadline = (+ (get-universal-time) +sign-in-seconds+)
        do (multiple-value-bind (object status)
               (json-call :get (getf process :poll-url))
             (unless (ok-p status)
               (error "The console's poll failed: HTTP ~a" status))
             (let ((state (nlk:json-value object :string "status")))
               (cond ((equal state "completed")
                      (let ((key (exchange process (nlk:json-value object :string "exchange_token"))))
                        (return (values key (identity-of key)))))
                     ((not (equal state "pending"))
                      (error "The browser sign-in was denied"))))
             (when (funcall cancelled-p)
               (error "The browser sign-in was cancelled"))
             (when (>= (get-universal-time) deadline)
               (error "The browser sign-in expired after ~d seconds" +sign-in-seconds+))
             (sleep +poll-seconds+))))

;;; --- keeping the key --------------------------------------------------------------

(defun announce-url (url)
  "Tell the operator the address to open, where the toast can carry it."
  (nle:notice (format nil "mistral-vibe: sign in at ~a (five minutes, then /mistral-vibe status)" url)
              :level :info))

(defun save-key (key auth-path)
  "Keep KEY as this provider's key at AUTH-PATH."
  (nle::save-provider-api-key +provider+ key :auth-path auth-path))

(defun sign-in (&key (auth-path nle::*auth-file-path*) (announce #'announce-url)
                     (cancelled-p (constantly nil)))
  "Run the whole sign-in: open a process, ANNOUNCE its address, wait for the
browser, keep the key at AUTH-PATH; => the identity, when the console names it."
  (let* ((process (start-process))
         (url (getf process :sign-in-url)))
    (funcall announce url)
    (multiple-value-bind (key identity) (await-key process cancelled-p)
      (save-key key auth-path)
      identity)))

;;; --- the command ------------------------------------------------------------------

(defvar *sign-in-thread* nil "The sign-in running in the background, or NIL.")

(defvar *sign-in-cancelled* nil "Set to end the sign-in in flight.")

(defun cancel-sign-in ()
  "End the sign-in in flight, saying nothing."
  (setf *sign-in-cancelled* t)
  (when (and *sign-in-thread* (bt2:thread-alive-p *sign-in-thread*))
    (ignore-errors (bt2:destroy-thread *sign-in-thread*)))
  (setf *sign-in-thread* nil))

(defun login (&key (auth-path nle::*auth-file-path*))
  "Open a sign-in and poll it behind this answer; => what the operator does next."
  (cancel-sign-in)
  (let* ((process (start-process))
         (url (getf process :sign-in-url)))
    (announce-url url)
    (setf *sign-in-cancelled* nil)
    (setf *sign-in-thread*
          (bt2:make-thread
           (lambda ()
             (handler-case
                 (multiple-value-bind (key identity) (await-key process (lambda () *sign-in-cancelled*))
                   (save-key key auth-path)
                   (nle:notice (format nil "mistral-vibe: signed in~@[ as ~a~]; pick a model with /models"
                                       identity)
                               :level :info))
               (error (condition)
                 (nle:notice (format nil "mistral-vibe: ~a" condition) :level :warning))))
           :name "nodecode-mistral-vibe sign-in"))
    (format nil "mistral-vibe: opened a browser sign-in; open ~a if the browser did not, then /mistral-vibe status" url)))

(defun current-key (auth-path)
  "The key a turn would send, or NIL."
  (let ((credential (ignore-errors (nle::resolve-provider-credential +provider+ :auth-path auth-path :probe t))))
    (and credential
         (not (eq :public (nle:credential-source credential)))
         (nle:credential-key credential))))

(defun status (&optional (auth-path nle::*auth-file-path*))
  "What this provider's key is, in the operator's words."
  (let ((key (current-key auth-path)))
    (if key
        (format nil "mistral-vibe: a key is set (~a...), models come from ~a/models; pick one with /models"
                (subseq key 0 (min 4 (length key))) (string-right-trim "/" (setting :base-url)))
        (format nil "mistral-vibe: no key yet — /mistral-vibe login signs in, or set ~a" (first +env+)))))

(defun logout (&optional (auth-path nle::*auth-file-path*))
  "Forget the key saved at AUTH-PATH; the environment variable is the operator's own."
  (cancel-sign-in)
  (save-key nil auth-path)
  (format nil "mistral-vibe: the saved key is gone; ~a in the environment (if set) still answers, and the console's key stays live there until you delete it"
          (first +env+)))

(defun run-command (args session-id)
  "/mistral-vibe: the key's state; `/mistral-vibe login', `/logout', `/status'."
  (declare (ignore session-id))
  (let* ((text (string-downcase (string-trim " " (or args ""))))
         (space (position #\Space text))
         (verb (subseq text 0 space))
         (rest (string-trim " " (subseq text (1+ (or space -1))))))
    (declare (ignore rest))
    (cond ((equal verb "") (status))
          ((search "login" verb) (login))
          ((search "logout" verb) (logout))
          ((search "status" verb) (status))
          (t (format nil "mistral-vibe: ~a is not one of its words — /mistral-vibe, login, logout, status" verb)))))
