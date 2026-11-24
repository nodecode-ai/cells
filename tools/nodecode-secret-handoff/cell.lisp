;;;; cell.lisp --- ask for a password, type it into the page, say only whether it landed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The value takes one road: the operator types it into a loopback form this
;;;; cell serves, the cell holds it in a struct in RAM, and one CDP websocket
;;;; types it into the focused password field of the tab whose origin the
;;;; operator was shown. It is never a tool argument, never in a tool result,
;;;; never in the transcript, and dropped when the handoff ends, whatever it
;;;; ended as.

(in-package #:nodecode-secret-handoff)

;;; --- small readers -----------------------------------------------------------

(defun jget (object key)
  "KEY's member of OBJECT, a decoded JSON object; NIL when it is not there."
  (when (hash-table-p object)
    (multiple-value-bind (value found) (gethash key object)
      (if found value (gethash (string-downcase key) object)))))

(defun jlist (value)
  "VALUE, a JSON array, as a list."
  (cond ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t nil)))

(defun url-host (url)
  "The lowercased host of URL, without userinfo or port; \"\" when it has none."
  (let* ((text (or url ""))
         (mark (search "://" text))
         (rest (if mark (subseq text (+ mark 3)) text))
         (cut (position-if (lambda (c) (member c '(#\/ #\? #\#))) rest))
         (authority (if cut (subseq rest 0 cut) rest))
         (at (position #\@ authority :from-end t))
         (authority (if at (subseq authority (1+ at)) authority)))
    (string-downcase
     (cond ((zerop (length authority)) "")
           ((char= #\[ (char authority 0))
            (subseq authority 0 (min (length authority)
                                     (1+ (or (position #\] authority) (1- (length authority)))))))
           (t (subseq authority 0 (or (position #\: authority) (length authority))))))))

(defun page-origin (url)
  "SCHEME://HOST[:PORT] of URL, or NIL when URL is not an http(s) page."
  (let* ((text (string-trim " " (or url "")))
         (mark (search "://" text)))
    (when mark
      (let ((scheme (string-downcase (subseq text 0 mark)))
            (rest (subseq text (+ mark 3))))
        (when (member scheme '("http" "https") :test #'string=)
          (let* ((cut (position-if (lambda (c) (member c '(#\/ #\? #\#))) rest))
                 (authority (if cut (subseq rest 0 cut) rest))
                 (at (position #\@ authority :from-end t))
                 (authority (if at (subseq authority (1+ at)) authority)))
            (when (plusp (length authority))
              (format nil "~a://~(~a~)" scheme (string-downcase authority)))))))))

(defun origin-matches-service (origin service)
  "True when ORIGIN's host is SERVICE's domain or one of its subdomains."
  (let ((host (url-host origin))
        (want (url-host (if (search "://" (or service ""))
                            service
                            (format nil "https://~a" service)))))
    (and (plusp (length host)) (plusp (length want))
         (or (string= host want)
             (and (> (length host) (length want))
                  (uiop:string-suffix-p host (concatenate 'string "." want)))))))

(defun loopback-origin-p (url)
  "True when URL's host is this machine."
  (let ((host (url-host url)))
    (or (string= host "localhost") (string= host "::1") (string= host "[::1]")
        (uiop:string-prefix-p "127." host))))

;;; --- stubbable seams -----------------------------------------------------------

(defvar *http* #'nlk:http
  "The one HTTP exchange: NLK:HTTP, or a test's answer.")

(defvar *cdp-run* nil
  "CDP-RUN, or the stub a test puts here: (WS-URL CALLS TIMEOUT) => results.")

;;; --- the CDP target list --------------------------------------------------------

(defun cdp-base ()
  "The endpoint the browser answers CDP on, from the section's `cdp_url'."
  (string-right-trim "/" (string-trim " " (or (setting :cdp-url) ""))))

(defun cdp-json (url)
  "URL's JSON answer, or NIL when it did not answer as one."
  (handler-case
      (multiple-value-bind (body status) (funcall *http* :get url :timeout (setting :cdp-seconds))
        (when (and (integerp status) (<= 200 status 299) (stringp body))
          (ignore-errors (nlk:decode-json body))))
    (error () nil)))

(defun pick-target (targets &optional target)
  "The tab the value would go into: TARGET's id when named, else the first
attached page, else the first page, else the first entry."
  (let ((items (remove-if-not #'hash-table-p (jlist targets))))
    (or (and target (find-if (lambda (item) (equal target (jget item "id"))) items))
        (find-if (lambda (item) (and (equal "page" (jget item "type")) (jget item "attached"))) items)
        (find-if (lambda (item) (equal "page" (jget item "type"))) items)
        (first items))))

(defun cdp-plan (&optional target)
  "The tab this handoff would type into, as (:ws-url URL :origin ORIGIN).
Signals HANDOFF-ERROR with the reason when there is none."
  (when (and (not (loopback-origin-p (cdp-base))) (not (setting :allow-remote-cdp)))
    (refuse "cdp endpoint is not loopback, and allow_remote_cdp is off"))
  (let* ((targets (cdp-json (concatenate 'string (cdp-base) "/json")))
         (tab (pick-target targets target)))
    (unless tab (refuse "the browser at the configured cdp endpoint has no target"))
    (let* ((url (str (jget tab "url")))
           (origin (page-origin url))
           (ws (str (jget tab "webSocketDebuggerUrl"))))
      (unless origin (refuse "that tab is not an http(s) page"))
      (unless (or (loopback-origin-p ws) (string= (url-host ws) (url-host (cdp-base))))
        (refuse "the websocket the endpoint handed back is another host"))
      (list :ws-url ws :origin origin))))

(defun str (value)
  (if (stringp value) (string-trim " " value) ""))

(defun refuse (detail)
  "The handoff, in one sentence, saying why it did not happen."
  (error 'secret-handoff-error :detail detail))

;;; --- typing it in -----------------------------------------------------------------

(defparameter +focus-expression+
  "(function(){var el=document.activeElement,ok=false;if(el&&el.tagName==='INPUT'){var t=(el.type||'').toLowerCase(),ac=(el.getAttribute('autocomplete')||'').toLowerCase();ok=t==='password'||/password|one-time-code/.test(ac);}if(ok&&typeof el.focus==='function')el.focus();return {origin:location.origin,field:ok};})()"
  "The guard the page answers before anything is typed: its live origin and
whether focus is on a password or one-time-code field.")

(defun cdp-rpc (ws-url calls timeout)
  "CALLS, each (METHOD PARAMS), over one CDP websocket; => the results, in order.
NIL for a call whose answer never came."
  (let* ((ws (make-instance 'websocket-driver.ws.client:client :url ws-url))
         (lock (sb-thread:make-mutex :name "secret-handoff-cdp"))
         (cv (sb-thread:make-waitqueue :name "secret-handoff-cdp"))
         (inbox '())
         (closed nil)
         (results '()))
    (labels
        ((take (id)
           (let ((deadline (+ (get-internal-real-time)
                               (* timeout internal-time-units-per-second))))
             (loop
               (sb-thread:with-mutex (lock)
                 (let ((hit (find-if (lambda (message)
                                       (and (hash-table-p message)
                                            (eql (jget message "id") id)))
                                     inbox)))
                   (when hit
                     (setf inbox (remove hit inbox))
                     (return-from take (jget hit "result"))))
                 (when closed (return-from take nil))
                 (let ((left (/ (- deadline (get-internal-real-time))
                                internal-time-units-per-second)))
                   (when (<= left 0) (return-from take nil))
                   (sb-thread:condition-wait cv lock :timeout left))))))
         (heard (message)
           (let ((data (if (stringp message)
                           (ignore-errors (nlk:decode-json message))
                           message)))
             (sb-thread:with-mutex (lock)
               (when (hash-table-p data) (push data inbox))
               (sb-thread:condition-broadcast cv)))))
      (wsd:on :message ws #'heard)
      (wsd:on :close ws (lambda (&key &allow-other-keys)
                          (sb-thread:with-mutex (lock)
                            (setf closed t)
                            (sb-thread:condition-broadcast cv))))
      (let ((lap (sb-thread:make-thread
                  (lambda () (ignore-errors (wsd:start-connection ws)))
                  :name "secret-handoff-cdp-lap")))
        (unwind-protect
                  (progn
                    (loop for (method params) in calls
                          for id from 1
                          do (wsd:send ws (nlk:encode-json-object
                                            (nlk:json-object "id" id
                                                             "method" method
                                                             "params" params)))
                             (push (take id) results))
                    (nreverse results))
               (nlk:sever-websocket ws)
               (when (sb-thread:thread-alive-p lap)
                 (ignore-errors (sb-thread:terminate-thread lap))))))))

(defun guard-value (result)
  "The {origin, field} the page answered in RESULT, or NIL."
  (let ((inner (jget result "result")))
    (jget inner "value")))

(defun inject-secret (secret ws-url origin)
  "Type SECRET into WS-URL's focused password field, once ORIGIN is proven live.
=> (values T \"injected\") or (values NIL detail)."
  (let* ((runner (or *cdp-run* #'cdp-rpc))
         (results (funcall runner
                           ws-url
                           (list (list "Runtime.evaluate"
                                       (nlk:json-object "expression" +focus-expression+
                                                        "returnByValue" t))
                                 (list "Input.insertText"
                                       (nlk:json-object "text" secret)))
                           (setting :cdp-seconds)))
         (guard (guard-value (first results))))
    (cond ((null results) (values nil "the cdp websocket did not answer"))
          ((not (hash-table-p guard)) (values nil "the page did not report its origin"))
          ((not (equal origin (str (jget guard "origin"))))
           (values nil "the page navigated away from the origin the operator was shown"))
          ((not (jget guard "field")) (values nil "focus is not on a password field"))
          ((null (second results)) (values nil "the page did not take the text"))
          (t (values t "injected")))))

;;; --- what waits for an answer ---------------------------------------------------

(defstruct (pending (:copier nil))
  (id "" :read-only t)
  (service "" :read-only t)
  (origin "" :read-only t)
  (ws-url nil :read-only t)
  (answer nil)
  (settled-p nil)
  (lock (sb-thread:make-mutex :name "secret-handoff-pending"))
  (cv (sb-thread:make-waitqueue :name "secret-handoff-pending")))

(defvar *pending* '()
  "The handoffs waiting for an answer, oldest first. Nothing here outlives
the call: START-CELL empties it and every outcome drops its own entry.")

(defvar *pending-lock* (sb-thread:make-mutex :name "secret-handoff-pending-list"))

(defun pending-list ()
  "The handoffs waiting for an answer, oldest first."
  (sb-thread:with-mutex (*pending-lock*)
    (copy-list *pending*)))

(defun new-pending (service origin ws-url)
  (let ((entry (make-pending :id (format nil "~(~36r~)-~(~36r~)"
                                         (random (expt 36 8)) (get-universal-time))
                              :service service :origin origin :ws-url ws-url)))
    (sb-thread:with-mutex (*pending-lock*)
      (setf *pending* (append *pending* (list entry))))
    entry))

(defun forget-pending (entry)
  (sb-thread:with-mutex (*pending-lock*)
    (setf *pending* (remove entry *pending*))))

(defun settle (id value)
  "Hand VALUE to the handoff ID. => T when one was waiting, NIL when none was."
  (let ((entry (find id (pending-list) :key #'pending-id :test #'string=)))
    (when entry
      (sb-thread:with-mutex ((pending-lock entry))
        (unless (pending-settled-p entry)
          (setf (pending-answer entry) value
                (pending-settled-p entry) t)
          (sb-thread:condition-broadcast (pending-cv entry))))
      t)))

(defun settle-service (service value)
  "Hand VALUE to the handoff waiting on SERVICE, newest first."
  (let ((entry (find (string-downcase (str service)) (reverse (pending-list))
                     :key (lambda (item) (string-downcase (pending-service item)))
                     :test #'string=)))
    (when entry
      (sb-thread:with-mutex ((pending-lock entry))
        (unless (pending-settled-p entry)
          (setf (pending-answer entry) value
                (pending-settled-p entry) t)
          (sb-thread:condition-broadcast (pending-cv entry))))
      t)))

(defun wait-for-answer (entry seconds)
  "ENTRY's answer, or NIL when SECONDS ran out first."
  (sb-thread:with-mutex ((pending-lock entry))
    (let ((deadline (+ (get-internal-real-time)
                        (* seconds internal-time-units-per-second))))
      (loop
        (when (pending-settled-p entry) (return (pending-answer entry)))
        (let ((left (/ (- deadline (get-internal-real-time))
                       internal-time-units-per-second)))
          (when (<= left 0) (return nil))
          (sb-thread:condition-wait (pending-cv entry) (pending-lock entry)
                                    :timeout left))))))

(defun cancel-all ()
  "Drop every handoff waiting for an answer; => how many were dropped."
  (let ((entries (pending-list)))
    (dolist (entry entries)
      (sb-thread:with-mutex ((pending-lock entry))
        (unless (pending-settled-p entry)
          (setf (pending-answer entry) nil
                (pending-settled-p entry) t)
          (sb-thread:condition-broadcast (pending-cv entry)))))
    (sb-thread:with-mutex (*pending-lock*)
      (setf *pending* '()))
    (length entries)))

;;; --- the page the operator types into ---------------------------------------------

(defun api-base ()
  "The gateway's own loopback address, where the form answers."
  (let ((port (and (boundp 'nle::*gateway-port*) nle::*gateway-port*)))
    (if (and (integerp port) (plusp port))
        (format nil "http://127.0.0.1:~a" port)
        "http://127.0.0.1:PORT")))

(defun handoff-address (id)
  (format nil "~a/secret-handoff/?id=~a" (api-base) id))

(defun html-page (title body)
  (format nil "<!doctype html><html><head><meta charset=\"utf-8\"><title>~a</title><style>body{font:16px system-ui;margin:3rem auto;max-width:26rem}input{width:100%;padding:.6rem;font-size:1rem;margin:.6rem 0}button{padding:.6rem 1.2rem;font-size:1rem}code{word-break:break-all}</style></head><body>~a</body></html>"
          title body))

(defun form-page (entry)
  (html-page
   "secret handoff"
   (format nil "<h2>Password for ~a</h2><p>It goes into <code>~a</code> and is not kept.</p><form method=\"post\" action=\"/secret-handoff/\"><input type=\"hidden\" name=\"id\" value=\"~a\"><input type=\"password\" name=\"value\" autofocus autocomplete=\"off\"><button type=\"submit\">Type it into the page</button></form>"
           (pending-service entry) (pending-origin entry) (pending-id entry))))

(defun done-page (text)
  (html-page "secret handoff" (format nil "<h2>~a</h2><p>You can close this tab.</p>" text)))

(defun url-decode (text)
  "TEXT with %XX and + undone: a form field's own text."
  (let ((out (make-string-output-stream)))
    (loop with string = (or text "")
          for index from 0 below (length string)
          for char = (char string index)
          do (cond ((char= char #\+) (write-char #\Space out))
                   ((and (char= char #\%) (< (+ index 2) (length string)))
                    (let ((code (ignore-errors
                                  (parse-integer string :start (1+ index) :end (+ index 3)
                                                        :radix 16))))
                      (if code
                          (progn (write-char (code-char code) out) (incf index 2))
                          (write-char char out))))
                   (t (write-char char out))))
    (get-output-stream-string out)))

(defun form-fields (text)
  "TEXT, a form body or query string, as an alist of (name . value)."
  (loop for piece in (uiop:split-string (or text "") :separator "&")
        for equals = (position #\= piece)
        when (plusp (length piece))
          collect (cons (url-decode (if equals (subseq piece 0 equals) piece))
                        (url-decode (if equals (subseq piece (1+ equals)) "")))))

(defun field (fields name)
  (cdr (assoc name fields :test #'string=)))

(defun request-body (env)
  (let ((body (getf env :body))
        (length (getf env :content-length)))
    (cond ((null body) "")
          ((stringp body) body)
          ((streamp body)
           (let ((text (handler-case (nlk:body-text body 8192) (error () ""))))
             (if (and (integerp length) (> (length text) length))
                 (subseq text 0 length)
                 text)))
          (t ""))))

(defun answer-page (env)
  "The loopback form: GET asks for the password, POST hands it to the handoff."
  (let* ((method (getf env :request-method))
         (fields (append (form-fields (getf env :query-string))
                         (form-fields (request-body env))))
         (id (field fields "id"))
         (entry (and id (find id (pending-list) :key #'pending-id :test #'string=))))
    (cond
      ((and entry (eq method :post) (field fields "value"))
       (if (settle (pending-id entry) (field fields "value"))
           (list 200 (list :content-type "text/html; charset=utf-8")
                 (list (done-page "Handed over.")))
           (list 410 (list :content-type "text/html; charset=utf-8")
                 (list (done-page "That handoff already ended.")))))
      (entry
       (list 200 (list :content-type "text/html; charset=utf-8") (list (form-page entry))))
      (t
       (list 404 (list :content-type "text/html; charset=utf-8")
             (list (done-page "No handoff is waiting under that id.")))))))

;;; --- the tool ----------------------------------------------------------------------

(nlk:define-contract secret-request
  (:doc "Ask the operator for a site password without it entering the conversation:
it is typed into the focused password field of the browser tab for that site, over
CDP, and the answer here is a status only. Focus the password field in the browser
first, then call this.")
  ("service" :text :required t
   :doc "the site's domain, for example example.com; the value is only typed into a tab whose host is that domain or a subdomain of it")
  ("target" :string :doc "a CDP tab id, when the tab to fill is not the focused one"))

(defun status-json (&rest pairs)
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun request-secret (report)
  "The model's call: one handoff, its status, and never the value."
  (let ((service (secret-request-service report))
        (target (secret-request-target report)))
    (handler-case
        (let* ((plan (cdp-plan target))
               (origin (getf plan :origin)))
          (unless (origin-matches-service origin service)
            (return-from request-secret
              (status-json "status" "failed" "service" service
                           "detail" "the tab's origin does not match the service"
                           "origin" origin)))
          (let ((entry (new-pending service origin (getf plan :ws-url))))
            (nle:notice (format nil "secret-handoff: a password for ~a is wanted — type it at ~a"
                                service (handoff-address (pending-id entry)))
                        :level :info :key "nodecode-secret-handoff")
            (unwind-protect
                 (let ((answer (wait-for-answer entry (setting :prompt-seconds))))
                   (cond
                     ((null answer)
                      (status-json "status" "failed" "service" service
                                   "detail" "no_response"
                                   "page" (handoff-address (pending-id entry))))
                     (t
                      (multiple-value-bind (ok detail)
                          (inject-secret answer (pending-ws-url entry) origin)
                        (status-json "status" (if ok "ok" "failed")
                                     "service" service "detail" detail)))))
              (setf (pending-answer entry) nil)
              (forget-pending entry)
              (nle:notice nil :key "nodecode-secret-handoff"))))
      (secret-handoff-error (condition)
        (status-json "status" "failed" "service" service
                     "detail" (secret-handoff-error-detail condition)))
      (error () (status-json "status" "failed" "service" service "detail" "inject failed")))))

;;; --- the command --------------------------------------------------------------------

(defun run-command (args session-id)
  "`/secret-handoff` lists what waits; `cancel` drops it; `SERVICE PASSWORD`
answers the handoff waiting on SERVICE."
  (declare (ignore session-id))
  (let* ((words (remove "" (uiop:split-string (or args "") :separator " ") :test #'string=))
         (waiting (pending-list)))
    (cond
      ((null words)
       (if (null waiting)
           "secret-handoff: nothing is waiting for an answer. The model asks with request_secret, and the password goes into the browser page it names."
           (format nil "secret-handoff: ~a waiting~{~%~a~}~%~a"
                   (length waiting)
                   (loop for entry in waiting
                         collect (format nil "  ~a at ~a — ~a"
                                         (pending-service entry) (pending-origin entry)
                                         (handoff-address (pending-id entry))))
                   "open that address to type it, or /secret-handoff cancel.")))
      ((string-equal (first words) "cancel")
       (format nil "secret-handoff: ~a handoff~:p dropped." (cancel-all)))
      ((= (length words) 2)
       (if (settle-service (first words) (second words))
           (format nil "secret-handoff: handed to the ~a handoff." (string-downcase (first words)))
           (format nil "secret-handoff: nothing is waiting on ~a." (first words))))
      (t
       (format nil "secret-handoff: /secret-handoff lists what waits, /secret-handoff cancel drops it, /secret-handoff SERVICE PASSWORD answers by hand.~%~a"
               (if (null waiting) "" (run-command "" session-id)))))))

;;; --- the cell -------------------------------------------------------------------------

(defparameter +primer+
  "secret-handoff asks the operator for a site password and types it into the browser tab
whose origin is that site's, over CDP, answering a status only — the value is never a
tool argument, a tool result, or part of the transcript. The operator types it into a
loopback form the cell serves, or hands it over with /secret-handoff SERVICE PASSWORD.

The browser must already answer CDP at `cdp_url` (http://127.0.0.1:9222 unless set):
Chrome or Chromium started with --remote-debugging-port=9222. The model focuses the
password field in that tab, then calls request_secret with the site's domain. Nothing is
typed unless the tab's live origin still matches and focus is on a password or
one-time-code field; a `no_response` status means nobody answered inside
`prompt_seconds`, and the page address in that status is where to type it.")

(defun start ()
  (cancel-all)
  (nle:route "/secret-handoff/" #'answer-page :public t)
  (nle:on-stop (lambda ()
                 (nle:route "/secret-handoff/" nil)
                 (cancel-all))))

(nle:define-cell secret-handoff
  (:section ("secret-handoff")
    (:guide "start Chrome with --remote-debugging-port=9222, focus the password field, and ask; nothing else is required")
    ("cdp_url" :string :default "http://127.0.0.1:9222"
               :doc "the endpoint the browser answers CDP on")
    ("prompt_seconds" :integer :default 90 :min 1
                      :doc "how long one handoff waits for the operator to type the value")
    ("cdp_seconds" :integer :default 8 :min 1
                   :doc "one CDP exchange's whole budget")
    ("allow_remote_cdp" :boolean :default nil
                        :doc "accept a cdp_url that is not this machine; off refuses one"))
  (:tool "request_secret" (nlk:contract 'secret-request) 'request-secret)
  (:help :secret-handoff
         "secret-handoff: ask for a password, type it into the browser tab, never into the transcript"
         +primer+)
  (:command "secret-handoff" 'run-command
            :description "What is waiting for a password; cancel it, or answer it by hand"
            :argument-hint "cancel")
  (:start #'start))
