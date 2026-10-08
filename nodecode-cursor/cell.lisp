;;;; cell.lisp --- the cell: Cursor's agent service as a lane of its own.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No lane of the organism speaks Cursor's agent protocol, so the cell
;;;; registers one, cursor, whose stream is a whole Cursor run (wire.lisp).
;;;; Three hooks, each declining for every other provider, and one slash
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Cursor's row: this cell's
;;;;                          lane package, this section's base and omp's
;;;;                          bundled models, so /models lists them and a
;;;;                          turn resolves this lane
;;;;   :CREDENTIAL            the kept sign-in's token, refreshed first when it
;;;;                          is due on a round, else CURSOR_ACCESS_TOKEN or
;;;;                          CURSOR_API_KEY; no other provider's key is lent
;;;;                          to Cursor
;;;;   LIST-PROVIDER-MODELS   /models asks the account's own roster
;;;;                          (GetUsableModels), which the bundled rows only
;;;;                          seed
;;;;   /cursor                login, logout, status
;;;;
;;;; Config, a sibling top-level key:
;;;;   "cursor": {"base_url": "https://api2.cursor.sh"}
;;;; A vetoed section ("enabled": false) installs nothing: no lane, no hook.

(in-package #:nodecode-cursor)

;;; --- the catalog ---------------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Cursor's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Cursor's row, made once per
catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

;;; --- the credential ------------------------------------------------------------

(defun credential (op next)
  "The :CREDENTIAL answer for cursor: the kept sign-in's token, else a token
from the environment, else nothing to send."
  ;; Never NEXT for cursor: the lane's family is its own, which the ladder
  ;; behind this point does not know. A question of where the credential
  ;; comes from (/connect, status) names no endpoint and is answered without
  ;; a refresh, a write or the network.
  (if (not (equal (getf op :provider) +provider+))
      (funcall next op)
      (let ((entry (stored-entry (getf op :auth)))
            (path (getf op :auth-path)))
        (cond
          (entry
           (let ((entry (if (getf op :endpoint)
                            (handler-case (fresh-entry entry path)
                              (signin-failed (refusal)
                                ;; stands until a sign-in succeeds: the model
                                ;; keeps seeing why its rounds cannot go
                                (nle:notice (format nil "~a: the sign-in could not be refreshed (~a); run /~a login"
                                                    +provider+ refusal +provider+)
                                            :level :warning :key +key+)
                                (nle::credential-fail "~a; run /~a login" refusal +provider+)))
                            entry)))
             (nle:make-credential (nlk:json-value entry :text "access_token") :oauth
                                  (list :auth-path path :email (nlk:json-value entry :text "email")))))
          ((env-token) (nle:make-credential (env-token) :env))
          (t (nle:make-credential "public" :public))))))

;;; --- the listing -----------------------------------------------------------------

(defun listing-rows (models)
  "The /models rows of the roster MODELS (normalizeCursorModels, the
GetUsableModels half): each id once, its bundled window when it has one, 1M
when its name says so; each model's max-mode marker kept for its rounds."
  (let ((rows '()))
    (dolist (model models)
      (let* ((id (getf model :id))
             (row (find-row +models+ id))
             (window (nlk:json-value row :integer "context")))
        (setf (gethash id *discovered*) (getf model :max-mode))
        (when (some (lambda (name) (ppcre:scan "(?i)\\b1m\\b" name)) (cons id (getf model :names)))
          (setf window (max (or window 0) 1000000)))
        (setf rows (cons (list :id id :display (nlk:trimmed (getf model :name)) :context-window (or window 200000))
                         (remove id rows :key (lambda (r) (getf r :id)) :test #'equal)))))
    (sort rows #'string< :key (lambda (row) (getf row :id)))))

(defun refusal-reason (status body)
  "The core's reason for a refused listing: `HTTP STATUS CODE', CODE the
Connect error code the body names when it names one."
  (let* ((parsed (ignore-errors (nlk:decode-json (nlk:body-text body))))
         (code (or (nlk:json-value parsed :text "code") (nlk:json-value parsed :text "error" "code"))))
    (format nil "HTTP ~a~@[ ~a~]" status code)))

(defun listing (next provider &rest keys &key key base &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: Cursor lists the account's roster over its
own RPC, a protobuf body, not a GET of /models. Called with KEY it is also
/connect's check of that key, so KEY is what is asked with, and a refusal
answers the core's `HTTP <status> <code>'."
  (if (not (equal provider +provider+))
      (apply next provider keys)
      (let ((token (or key (ignore-errors (nle:credential-key (nle::resolve-provider-credential provider)))))
            (url (format nil "~a~a" (string-right-trim "/" (or base (setting :base-url))) +usable-models-path+)))
        (if (or (null token) (equal token "public"))
            (values nil (format nil "not signed in: /~a login" +provider+))
            (handler-case
                (multiple-value-bind (body status)
                    (dex:post url
                              :headers (append (client-headers token)
                                               `(("connect-protocol-version" . "1")
                                                 ("x-request-id" . ,(uuid))))
                              :content (make-array 0 :element-type '(unsigned-byte 8))
                              :force-binary t :connect-timeout 5 :read-timeout 5)
                  (if (and (integerp status) (<= 200 status 299))
                      (let ((octets (coerce (if (stringp body) (utf8 body) body) 'octets)))
                        (values (listing-rows (usable-models (or (connect-unary-body octets) octets))) nil))
                      (values nil (refusal-reason status body))))
              (dex:http-request-failed (e)
                (values nil (refusal-reason (dex:response-status e) (ignore-errors (dex:response-body e)))))
              (error (e) (values nil (nle::transport-failure-label e url))))))))

;;; --- /cursor ------------------------------------------------------------------------

(defun status ()
  "Whether a sign-in is kept, in a line."
  (let ((entry (stored-entry (ignore-errors (nle::read-auth-file nle::*auth-file-path*)))))
    (cond (entry
           (format nil "~a: signed in~@[ as ~a~]~:[~;; a sign-in is in progress~]"
                   +provider+ (nlk:json-value entry :text "email") *flow*))
          (*flow* (format nil "~a: a sign-in is in progress" +provider+))
          ((env-token) (format nil "~a: not signed in; a token from the environment is used" +provider+))
          (t (format nil "~a: not signed in; /~a login signs in with Cursor" +provider+ +provider+)))))

(defun logout ()
  "Forget the kept sign-in."
  (cancel-flow)
  (save-entry nle::*auth-file-path* nil)
  (nle:notice nil :key +key+)
  (format nil "~a: signed out" +provider+))

(defun run-slash (args session-id)
  "/cursor login | logout | status"
  (declare (ignore session-id))
  (let* ((trimmed (nlk:trimmed (or args "")))
         (space (position-if (lambda (char) (member char '(#\Space #\Tab))) trimmed))
         (verb (string-downcase (if (plusp (length trimmed)) (subseq trimmed 0 space) "status"))))
    (cond ((equal verb "login") (login nle::*auth-file-path*))
          ((equal verb "logout") (logout))
          ((equal verb "status") (status))
          (t (format nil "usage: /~a login | logout | status" +provider+)))))

(defun complete-slash (text session-id)
  "The verbs /cursor takes, those that start with TEXT."
  (declare (ignore session-id))
  (loop for verb in '("login" "logout" "status")
        when (uiop:string-prefix-p (string-downcase (nlk:trimmed (or text ""))) verb)
          collect (list :name verb :value verb)))

(defun start ()
  "Register the lane; on the way down, take it out, end a sign-in in progress,
forget the conversations and clear what was said."
  (forget-catalog)
  (register-lane)
  (nle:on-stop #'unregister-lane)
  (nle:on-stop #'forget-catalog)
  (nle:on-stop #'forget-conversations)
  (nle:on-stop (lambda () (clrhash *discovered*) (clrhash *round-models*)))
  (nle:on-stop (lambda () (cancel-flow) (nle:notice nil :key +key+))))

(nle:define-cell cursor
  (:section ("cursor")
    (:guide "sign in with /cursor login (a Cursor account), or set CURSOR_ACCESS_TOKEN; base_url is where Cursor's agent service is served")
    ("base_url" :string :default +base+
     :doc "the Cursor API base the lane's RunSSE and BidiAppend calls go to"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::list-provider-models #'listing)
  (:command "cursor" 'run-slash
   :description "Sign in to Cursor, or out"
   :argument-hint "login | logout | status"
   :session nil
   :complete 'complete-slash))
