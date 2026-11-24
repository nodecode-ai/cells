;;;; cell-test.lisp --- lifecycle, the six tools, and /stacktree.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, an integer field under its floor refused, a start with no key
;;;; refused), each tool through the seam the model calls it by -- its url,
;;;; its Bearer header, its body and its answer -- and the command. Every
;;;; request is a stubbed NLK:HTTP, so nothing reaches Stacktree.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '() "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-stacktree-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials Stacktree."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected stacktree request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defmacro with-stacktree-answers ((&rest routes) &body body)
  "BODY with the stubbed wire answering ROUTES: (FRAGMENT FORM) answers any
request whose url holds FRAGMENT, (METHOD FRAGMENT FORM) only that method's;
FORM's (values TEXT STATUS) is the answer, and a request no route names is an
error. The cell is started on a test key and stopped on unwind."
  `(with-stacktree-wire
     (let ((*answer-fn* (lambda (method url args)
                           (declare (ignore args))
                           (cond ,@(loop for route in routes
                                         collect (if (= 2 (length route))
                                                     `((search ,(first route) url) ,(second route))
                                                     `((and (eq ,(first route) method)
                                                            (search ,(second route) url))
                                                       ,(third route))))
                                 (t (error "unexpected request to ~a ~a" method url))))))
       (with-cell-stop ((stacktree-start "api_key" "stk-test"))
         ,@body))))

(defun stacktree-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun stacktree-request-body (request)
  "REQUEST's JSON body, decoded; NIL for a request that carries none."
  (let ((content (getf (cddr request) :content)))
    (and content (nlk:decode-json content))))

(defun stacktree-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun stacktree-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "stacktree" "STACKTREE-CELL-" :start nodecode-stacktree:start-cell)

(define-cell-lifecycle-tests "stacktree"
  (:fixture with-stacktree-wire)
  (:config "api_key" "stk-test")
  (:help :stacktree)
  (:command "stacktree")
  (:running (is (nle::find-tool "stacktree_publish") "stacktree_publish is registered")
            (is (nle::find-tool "stacktree_update"))
            (is (nle::find-tool "stacktree_pages"))
            (is (nle::find-tool "stacktree_activity"))
            (is (nle::find-tool "stacktree_delete"))
            (is (nle::find-tool "stacktree_restore"))
            (is (equal "https://api.stacktr.ee" (nodecode-stacktree::root)) "the public root unless the section answers another")
            (is (= 30 (nodecode-stacktree::setting :timeout-seconds))))
  (:stopped (is (null (nle::find-tool "stacktree_publish")) "stop unregisters the tools"))
  (:refused ("api_key" "stk-test" "timeout_seconds" 0))
  (:idle stacktree:stacktree-error (stacktree:pages)))

(deftest stacktree-cell-a-key-is-required ()
  (with-stacktree-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignorable name)) nil)
      (is-carrying (text (refusal-text nlk:config-refusal (stacktree-start)))
        "stacktree: no key" "app.stacktr.ee/api-keys" "STACKTREE_API_KEY")
      (is (null (nle::find-tool "stacktree_publish")) "a refused start installed nothing"))))

;;; --- stacktree_publish -------------------------------------------------------

(deftest stacktree-cell-publish-answers-the-link ()
  (with-stacktree-answers (("/sites" (values (concatenate 'string
                                                           "{\"url\":\"https://stacktr.ee/p/tok123/\",\"id\":\"site-1\","
                                                           "\"expires_at_iso\":\"2026-11-15T00:00:00Z\","
                                                           "\"warnings\":[\"the page looks like it holds an email address\"]}")
                                            200)))
    (is-carrying (text (stacktree-tool "stacktree_publish"
                                       (nlk:json-object "content" "<!doctype html><html></html>"
                                                        "password" "s3cret"
                                                        "expires_in_hours" 24)))
      "published: https://stacktr.ee/p/tok123/" "id: site-1" "expires: 2026-11-15T00:00:00Z"
      "warning: the page looks like it holds an email address")
    (let ((request (first (stacktree-asked))))
      (is (= 1 (length (stacktree-asked))) "one request")
      (is (eq :post (first request)))
      (is (equal "https://api.stacktr.ee/sites" (second request)))
      (is (equal "Bearer stk-test" (stacktree-request-header request "authorization")) "the key rides the header")
      (is (null (search "stk-test" (second request))) "not the url")
      (let ((body (stacktree-request-body request)))
        (is (equal "<!doctype html><html></html>" (gethash "html" body)) "the page rides as html")
        (is (equal "s3cret" (gethash "password" body)))
        (is (= 24 (gethash "expires_in_hours" body)))))))

(deftest stacktree-cell-publish-reports-a-plan-limit ()
  (with-stacktree-answers (("/sites" (values (concatenate 'string
                                                           "{\"error\":\"plan_lifetime_limit_exceeded\","
                                                           "\"message\":\"this plan has published 3 pages\"}")
                                            402)))
    (is-carrying (text (stacktree-tool "stacktree_publish" (nlk:json-object "content" "<html></html>")))
      "ERROR: stacktree:" "HTTP 402" "this plan has published 3 pages"
      "lifetime page count is spent")))

;;; --- stacktree_update --------------------------------------------------------

(deftest stacktree-cell-update-keeps-the-url ()
  (with-stacktree-answers
      ((:put "/sites/site-1" (values (concatenate 'string "{\"url\":\"https://stacktr.ee/p/tok123/\",\"id\":\"site-1\"}") 200))
       (:patch "/sites/site-1" (values "{\"password\":\"s3cret\"}" 200)))
    (is-carrying (text (stacktree-tool "stacktree_update"
                                       (nlk:json-object "id" "site-1" "content" "<html>new</html>"
                                                        "password" "s3cret")))
      "updated in place: https://stacktr.ee/p/tok123/" "settings applied: password")
    (let ((asked (stacktree-asked)))
      (is (= 2 (length asked)) "the page, then its settings")
      (is (equal '(:put :patch) (map 'list #'first asked)))
      (is (equal "<html>new</html>" (gethash "html" (stacktree-request-body (first asked)))) "the put carries the page")
      (is (equal "s3cret" (gethash "password" (stacktree-request-body (second asked)))) "and the patch the setting")
      (is (null (gethash "html" (stacktree-request-body (second asked)))) "the patch carries no page"))))

;;; --- stacktree_pages ---------------------------------------------------------

(deftest stacktree-cell-pages-lists-the-account ()
  (with-stacktree-answers (("/sites" (values (concatenate 'string
                                                           "{\"sites\":["
                                                           "{\"id\":\"site-1\",\"url\":\"https://stacktr.ee/p/one/\","
                                                           "\"title\":\"Q3 brief\",\"expires_at_iso\":\"2026-11-20T00:00:00Z\","
                                                           "\"state\":\"live\"},"
                                                           "{\"id\":\"site-2\",\"url\":\"https://stacktr.ee/p/two/\"}]}")
                                            200)))
    (is-carrying (text (stacktree-tool "stacktree_pages" (nlk:json-object)))
      "2 pages" "https://stacktr.ee/p/one/" "Q3 brief" "expires 2026-11-20T00:00:00Z" "[live]"
      "https://stacktr.ee/p/two/" "no expiry"))
  (with-stacktree-answers (("/sites" (values "{\"sites\":[]}" 200)))
    (is-carrying (text (stacktree-tool "stacktree_pages" (nlk:json-object)))
      "no pages on this account yet")))

;;; --- stacktree_activity ------------------------------------------------------

(deftest stacktree-cell-activity-reports-an-open ()
  (with-stacktree-answers (("/activity" (values (concatenate 'string
                                                              "{\"summary\":\"Opened once, on 12 November.\",\"opened\":true,"
                                                              "\"state\":\"live\",\"first_opened_at\":\"2026-11-12T09:00:00Z\","
                                                              "\"open_comments\":2}")
                                                 200)))
    (is-carrying (text (stacktree-tool "stacktree_activity" (nlk:json-object "id" "site-1")))
      "Opened once, on 12 November." "state: live" "first opened: 2026-11-12T09:00:00Z" "comments: 2")
    (is (equal "https://api.stacktr.ee/sites/site-1/activity" (second (first (stacktree-asked)))))))

;;; --- stacktree_delete and stacktree_restore ----------------------------------

(deftest stacktree-cell-delete-then-restore ()
  (with-stacktree-answers ((:delete "/sites/site-1" (values (concatenate 'string
                                                                         "{\"restorable_until\":\"2026-12-12T00:00:00Z\"}")
                                                            200)))
    (is-carrying (text (stacktree-tool "stacktree_delete" (nlk:json-object "id" "site-1")))
      "taken down" "stops working now" "2026-12-12T00:00:00Z" "stacktree_restore"))
  (with-stacktree-answers ((:post "/restore" (values (concatenate 'string
                                                                  "{\"url\":\"https://stacktr.ee/p/one/\","
                                                                  "\"expires_at_iso\":\"2026-11-17T00:00:00Z\","
                                                                  "\"restored_for\":\"grace\"}")
                                                     200)))
    (is-carrying (text (stacktree-tool "stacktree_restore" (nlk:json-object "id" "site-1")))
      "restored: https://stacktr.ee/p/one/" "expires: 2026-11-17T00:00:00Z" "grace")
    (is (equal "https://api.stacktr.ee/sites/site-1/restore" (second (first (stacktree-asked)))))))

;;; --- /stacktree --------------------------------------------------------------

(deftest stacktree-cell-command-lists-the-pages ()
  (with-stacktree-answers (("/sites" (values (concatenate 'string
                                                           "{\"sites\":[{\"id\":\"site-1\","
                                                           "\"url\":\"https://stacktr.ee/p/one/\"}]}")
                                            200)))
    (is-carrying (text (nodecode-stacktree::run-command "" "s1"))
      "1 page" "https://stacktr.ee/p/one/" "key from the stacktree section" "https://api.stacktr.ee")))
