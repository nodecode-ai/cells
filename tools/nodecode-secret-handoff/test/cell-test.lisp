;;;; cell-test.lisp --- the secret-handoff cell against its own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No browser, no network: the CDP target list is a stub answer to
;;;; NLK:HTTP, the CDP session is a stub runner, and the answer page is called
;;;; the way the gateway calls a route, with a clack environment built by the
;;;; fixture. What is under test is the judgement around the value — which tab
;;;; it may go into, what is checked before a keystroke, and that the value
;;;; never comes back in the status.

(in-package #:nodecode.test)

(define-test-slice "secret-handoff" "SECRET-HANDOFF-CELL-"
  :start nodecode-secret-handoff:start-cell)

(defvar *typed* '()
  "Every (WS-URL CALLS) a stubbed CDP session was handed, newest first.")

(defvar *targets* '()
  "The tabs the stubbed CDP target list answers with.")

(defun tab (id url &key attached)
  (nlk:json-object "id" id "type" "page" "attached" (and attached t)
                   "url" url "webSocketDebuggerUrl" (format nil "ws://127.0.0.1:9222/devtools/page/~a" id)))

(defun stub-http (method url &key &allow-other-keys)
  "The browser's answer to the CDP target list, and its version."
  (declare (ignore method))
  (cond ((uiop:string-suffix-p url "/json")
         (values (nlk:encode-json-object (coerce *targets* 'vector)) 200 '() nil))
        (t (values "{}" 404 '() nil))))

(defun stub-cdp (ws-url calls timeout)
  "A CDP session that proves the origin and types, without a browser."
  (declare (ignore timeout))
  (push (list ws-url calls) *typed*)
  (list (nlk:json-object "result"
                          (nlk:json-object "value"
                                           (nlk:json-object "origin" "https://check24.de"
                                                            "field" t)))
        (nlk:json-object "result" (nlk:json-object "frameId" "F1"))))

(defun answer-when-waiting (value &key (seconds 5))
  "A second thread that hands VALUE to the first handoff that appears."
  (let ((deadline (+ (get-universal-time) seconds)))
    (sb-thread:make-thread
     (lambda ()
       (loop
         (let ((entry (first (nodecode-secret-handoff:pending-list))))
           (when entry
             (nodecode-secret-handoff:settle
              (nodecode-secret-handoff:pending-id entry) value)
             (return)))
         (when (> (get-universal-time) deadline) (return))
         (sleep 0.01)))
     :name "secret-handoff-test-answer")))

(defmacro with-handoff ((&key (tabs '((tab "T1" "https://check24.de/login" :attached t)))
                              (prompt 5) (cdp-url nil))
                        &body body)
  "BODY with the cell started, both CDP seams stubbed, and a handoff that
nobody answers unless BODY does."
  `(let ((*typed* '())
         (*targets* (list ,@tabs))
         (nodecode-secret-handoff::*http* #'stub-http)
         (nodecode-secret-handoff::*cdp-run* #'stub-cdp))
     (with-cell-stop ((secret-handoff-start
                       ,@(append (when cdp-url (list "cdp_url" cdp-url))
                                 (list "prompt_seconds" prompt))))
       (nodecode-secret-handoff:cancel-all)
       ,@body)))

(defun call-tool (&rest pairs)
  "REQUEST_SECRET called the way the model calls it, as the status it answers."
  (nodecode-secret-handoff:request-secret
   (nodecode-secret-handoff:decode-secret-request (apply #'nlk:make-json-object pairs))))

(defun answer (text)
  "TEXT decoded, for a test that reads a status."
  (ignore-errors (nlk:decode-json text)))

;;; --- the cell's own life -------------------------------------------------------

(define-cell-lifecycle-tests "secret-handoff"
  (:help :secret-handoff)
  (:command "secret-handoff")
  (:running
   (is (equal "http://127.0.0.1:9222" (nodecode-secret-handoff::setting :cdp-url)) "the documented default endpoint")
   (is (nle::find-tool "request_secret") "the one tool is registered")
   (is (nodecode-secret-handoff:loopback-origin-p "http://127.0.0.1:9222") "the default is this machine")
   (is (not (nodecode-secret-handoff:loopback-origin-p "https://browser.example")) "and another host is not"))
  (:stopped
   (is (null (nle::find-tool "request_secret")) "stop takes the tool back")
   (is (null (nodecode-secret-handoff:pending-list)) "and nothing waits behind it"))
  (:refused ("prompt_seconds" 0)))

;;; --- what the value may be typed into ---------------------------------------------

(deftest secret-handoff-cell-origin-matches-the-service-and-its-subdomains ()
  (is (nodecode-secret-handoff:origin-matches-service "https://check24.de" "check24.de") "the domain itself")
  (is (nodecode-secret-handoff:origin-matches-service "https://login.check24.de" "check24.de") "a subdomain")
  (is (not (nodecode-secret-handoff:origin-matches-service "https://check24.de.evil.test" "check24.de")) "a lookalike that only ends in the name")
  (is (not (nodecode-secret-handoff:origin-matches-service "https://evil.test" "check24.de")) "another site")
  (is (equal "https://check24.de:8443" (nodecode-secret-handoff:page-origin "https://check24.de:8443/login?x=1")) "the origin keeps the port and drops the path"))

(deftest secret-handoff-cell-picks-the-attached-page ()
  (is (equal "T2" (nodecode-secret-handoff:jget
                    (nodecode-secret-handoff:pick-target
                     (vector (tab "T1" "https://a.test/")
                             (tab "T2" "https://b.test/" :attached t)))
                    "id")) "an attached page before a bare one")
  (is (equal "T2" (nodecode-secret-handoff:jget
                    (nodecode-secret-handoff:pick-target
                     (vector (tab "T1" "https://a.test/") (tab "T2" "https://b.test/")) "T2")
                    "id")) "and the id the caller named wins"))

;;; --- the tool ---------------------------------------------------------------------

(deftest secret-handoff-cell-tool-answers-a-status-and-not-the-value ()
  (with-handoff ()
    (answer-when-waiting "hunter2")
    (let* ((text (call-tool "service" "check24.de"))
           (status (answer text)))
      (is (equal "ok" (nodecode-secret-handoff:jget status "status")) "the handoff landed")
      (is (equal "injected" (nodecode-secret-handoff:jget status "detail")))
      (is (null (search "hunter2" text)) "the value is nowhere in what the model reads")
      (is (equal 1 (length *typed*)) "one CDP session")
      (let* ((calls (second (first *typed*)))
             (insert (second (second calls))))
        (is (equal "hunter2" (nodecode-secret-handoff:jget insert "text")) "and the value went to the page, as the text to insert")
        (is (equal "Runtime.evaluate" (first (first calls))) "after the origin guard")))))

(deftest secret-handoff-cell-a-tab-of-another-site-is-refused ()
  (with-handoff (:tabs ((tab "T1" "https://evil.test/login" :attached t)))
    (let ((status (answer (call-tool "service" "check24.de"))))
      (is (equal "failed" (nodecode-secret-handoff:jget status "status")) "nothing was asked of the operator")
      (is (search "does not match" (nodecode-secret-handoff:jget status "detail")))
      (is (null *typed*) "and nothing was typed")
      (is (null (nodecode-secret-handoff:pending-list)) "no handoff was left waiting"))))

(deftest secret-handoff-cell-no-answer-inside-the-budget ()
  (with-handoff (:prompt 1)
    (let* ((started (get-universal-time))
           (status (answer (call-tool "service" "check24.de"))))
      (is (equal "no_response" (nodecode-secret-handoff:jget status "detail")))
      (is (<= (get-universal-time) (+ started 4)) "it gives up on its own budget, not the host's")
      (is (search "/secret-handoff/" (nodecode-secret-handoff:jget status "page")) "and says where to type it")
      (is (null *typed*) "nothing was typed")
      (is (null (nodecode-secret-handoff:pending-list)) "and the handoff is dropped"))))

(deftest secret-handoff-cell-a-remote-endpoint-is-refused-until-allowed ()
  (with-handoff (:cdp-url "http://browser.example:9222")
    (let ((status (answer (call-tool "service" "check24.de"))))
      (is (equal "cdp endpoint is not loopback, and allow_remote_cdp is off"
                 (nodecode-secret-handoff:jget status "detail")) "a member the declaration does not carry is named, not silently used"))))

;;; --- the page, and the command ------------------------------------------------------

(deftest secret-handoff-cell-the-page-hands-the-value-over ()
  (with-handoff ()
    (let* ((entry (nodecode-secret-handoff::new-pending "check24.de" "https://check24.de" "ws://127.0.0.1:9222/x"))
           (form (nodecode-secret-handoff:answer-page
                  (list :request-method :get :query-string (format nil "id=~a" (nodecode-secret-handoff:pending-id entry)))))
           (body (third form)))
      (is (= 200 (first form)) "the form is served on loopback")
      (is (search "type=\"password\"" (first body)) "with a password field, so the value is not on screen"))
    (let* ((entry (nodecode-secret-handoff::new-pending "check24.de" "https://check24.de" "ws://127.0.0.1:9222/x"))
           (env (list :request-method :post
                      :body (make-string-input-stream
                             (format nil "id=~a&value=hunter2" (nodecode-secret-handoff:pending-id entry))))))
      (is (= 200 (first (nodecode-secret-handoff:answer-page env))) "the post is taken")
      (is (equal "hunter2" (nodecode-secret-handoff::wait-for-answer entry 1)) "and the value reaches the waiting handoff") )))

(deftest secret-handoff-cell-command-lists-and-drops-what-waits ()
  (with-handoff ()
    (is (search "nothing is waiting" (nodecode-secret-handoff:run-command "" "s1")) "an idle cell says so")
    (nodecode-secret-handoff::new-pending "check24.de" "https://check24.de" "ws://127.0.0.1:9222/x")
    (let ((listed (nodecode-secret-handoff:run-command "" "s1")))
      (is (search "check24.de" listed) "a waiting handoff is named")
      (is (search "/secret-handoff/?id=" listed) "with the address to answer it at")
      (is (null (search "hunter2" listed)) "and no value, because none was typed"))
    (is (search "1 handoff dropped" (nodecode-secret-handoff:run-command "cancel" "s1")))
    (is (null (nodecode-secret-handoff:pending-list)) "and it is gone")))
