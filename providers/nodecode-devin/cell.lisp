;;;; cell.lisp --- the cell: Devin's Cascade as a lane of its own.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No lane of the organism speaks Cascade (protobuf in Connect envelopes,
;;;; not SSE), so the cell registers one, devin, whose stream is this cell's
;;;; own (wire.lisp): it makes the HTTP exchanges itself and folds the frames
;;;; into the organism's assistant message with the lane assembly every lane
;;;; uses. Three hooks, each declining for every other provider, and one
;;;; slash command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Devin's row: this cell's lane
;;;;                          package, this section's base, and the account's
;;;;                          roster once it was asked, omp's two seeds before
;;;;   :CREDENTIAL            the kept sign-in's session token while it has not
;;;;                          expired, else DEVIN_API_KEY; an expired sign-in
;;;;                          on a round asks for /devin login; no other
;;;;                          provider's key is lent to Devin
;;;;   LIST-PROVIDER-MODELS   /models (and /connect's key check) ask
;;;;                          GetCliModelConfigs, Cascade having no /models
;;;;   /devin                 login, code ADDRESS, logout, status
;;;;
;;;; Config, a sibling top-level key:
;;;;   "devin": {"base_url": "https://server.codeium.com"}
;;;; A vetoed section ("enabled": false) installs nothing: no lane, no hook.

(in-package #:nodecode-devin)

;;; --- the catalog ---------------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Devin's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Devin's row, made once per
catalog the core reads and per roster discovery answers."
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
  "Drop the merged catalog, so the next read builds it again: a restart with
another base, or a roster discovery just answered."
  (setf *catalog* (cons nil nil)))

;;; --- the credential ----------------------------------------------------------------

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun credential (op next)
  "The :CREDENTIAL answer for devin: the kept sign-in's token while it holds,
else DEVIN_API_KEY, else nothing to send."
  ;; Never NEXT for devin: the ladder behind this point knows only the four
  ;; core families. A question of where the credential comes from (no
  ;; :endpoint) touches no network and writes nothing.
  (if (not (equal (getf op :provider) +provider+))
      (funcall next op)
      (let ((entry (stored-entry (getf op :auth)))
            (env (env-key)))
        (cond ((and entry (not (expired-p entry)))
               (nle:make-credential (nlk:json-value entry :text "access_token") :oauth
                                    (list :auth-path (getf op :auth-path))))
              (env (nle:make-credential env :env))
              ((and entry (getf op :endpoint))
               ;; omp has no refresh for Devin: an expired sign-in is asked
               ;; for again, and the notice stands until one succeeds
               (nle:notice (format nil "devin: the sign-in expired at ~a; run /devin login"
                                   (expiry-text (nlk:json-value entry :number "expires_at")))
                           :level :warning :key +key+)
               (nle::credential-fail "the Devin sign-in expired; run /devin login"))
              (t (nle:make-credential "public" :public))))))

;;; --- the lane -------------------------------------------------------------------------

(defun register-lane ()
  "The devin lane: Cascade's GetChatMessage, folded by this cell."
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                            :stream-symbol 'stream-round
                            :family :devin
                            :reasoning-carry :text
                            :default-endpoint +base+
                            :path ""
                            :npm "nodecode-devin")))

(defun unregister-lane ()
  "Take the devin lane back out."
  (setf nle::*provider-lanes*
        (remove +provider+ nle::*provider-lanes* :key #'nle::provider-lane-name :test #'equal)))

;;; --- the listing ------------------------------------------------------------------------

(defun listing-rows (specs)
  "SPECS as the listing's row plists."
  (loop for spec in specs
        collect (list :id (getf spec :id) :display (getf spec :name) :context-window (getf spec :context))))

(defun listing (next provider &rest keys &key key base &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: Devin's listing is GetCliModelConfigs;
Cascade publishes no /models. Asked with KEY, it is /connect's key check:
Devin is asked with that key alone and the roster is not kept, a refusal
answering `HTTP <status> <code>'. Asked without, it lists the signed-in
account's roster and keeps it for the catalog."
  (if (not (equal provider +provider+))
      (apply next provider keys)
      (let ((base (string-right-trim "/" (or base (setting :base-url)))))
        (flet ((answer (thunk)
                 (multiple-value-bind (specs reason)
                     (handler-case (funcall thunk)
                       (error (e) (values nil (nle::transport-failure-label e base))))
                   (if specs (values (listing-rows specs) nil) (values nil reason)))))
          (if key
              (answer (lambda () (fetch-models key base)))
              (let ((key (ignore-errors (nle:credential-key (nle::resolve-provider-credential +provider+)))))
                (if (or (null key) (equal key "public"))
                    (values nil "not signed in: /devin login, or set DEVIN_API_KEY")
                    (answer (lambda () (discover key base))))))))))

;;; --- /devin ------------------------------------------------------------------------------

(defun status ()
  "Whether a sign-in is kept, in a line."
  (let ((entry (stored-entry (ignore-errors (nle::read-auth-file nle::*auth-file-path*))))
        (expires nil))
    (when entry (setf expires (nlk:json-value entry :number "expires_at")))
    (cond ((and entry (expired-p entry))
           (format nil "devin: the sign-in expired at ~a; /devin login signs in again" (expiry-text expires)))
          (entry
           (format nil "devin: signed in~@[ until ~a~]~:[~;; a sign-in is in progress~]"
                   (and expires (expiry-text expires)) *flow*))
          (*flow* "devin: a sign-in is in progress")
          ((env-key) "devin: DEVIN_API_KEY is set; /devin login signs in with the browser instead")
          (t "devin: not signed in; /devin login signs in with the browser"))))

(defun logout ()
  "Forget the kept sign-in."
  (cancel-flow)
  (save-entry nle::*auth-file-path* nil)
  (nle:notice nil :key +key+)
  "devin: signed out")

(defun run-slash (args session-id)
  "/devin login | code ADDRESS | logout | status"
  (declare (ignore session-id))
  (let* ((trimmed (nlk:trimmed (or args "")))
         (space (position-if (lambda (char) (member char '(#\Space #\Tab))) trimmed))
         (verb (string-downcase (if (plusp (length trimmed)) (subseq trimmed 0 space) "status")))
         (rest (if space (nlk:trimmed (subseq trimmed space)) "")))
    (cond ((equal verb "login") (login nle::*auth-file-path*))
          ((equal verb "code") (paste rest))
          ((equal verb "logout") (logout))
          ((equal verb "status") (status))
          (t "usage: /devin login | code ADDRESS | logout | status"))))

(defun complete-slash (text session-id)
  "The verbs /devin takes, those that start with TEXT."
  (declare (ignore session-id))
  (loop for verb in '("login" "code" "logout" "status")
        when (uiop:string-prefix-p (string-downcase (nlk:trimmed (or text ""))) verb)
          collect (list :name verb :value verb)))

(defun forget-roster ()
  "Forget the roster discovery answered, and which model wrote which
message: a start asks the account again."
  (setf *discovered* nil *discovery-tried* nil)
  (forget-authors))

(defun start ()
  "Register the lane; on the way down, take it out, end a sign-in in progress
and clear what it said."
  (forget-roster)
  (forget-catalog)
  (register-lane)
  (nle:on-stop #'unregister-lane)
  (nle:on-stop #'forget-catalog)
  (nle:on-stop #'forget-roster)
  (nle:on-stop (lambda () (cancel-flow) (nle:notice nil :key +key+))))

(nle:define-cell devin
  (:section ("devin")
    (:guide "sign in with /devin login (a Devin account in the browser), or set DEVIN_API_KEY; base_url is where Codeium's Cascade is served")
    ("base_url" :string :default +base+
     :doc "the Cascade base the devin lane calls GetUserJwt, AssignModel, GetChatMessage and GetCliModelConfigs at"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::list-provider-models #'listing)
  (:command "devin" 'run-slash
   :description "Sign in to Devin, or out"
   :argument-hint "login | code ADDRESS | logout | status"
   :session nil
   :complete 'complete-slash))
