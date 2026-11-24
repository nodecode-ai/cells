;;;; cell-test.lisp --- the two tools over a stubbed wire, the sign-in, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs a relay, a token or the network: every request goes
;;;; through NODECODE-ARTIFACT-RELAY::*HTTP*, a stub that records the call it
;;;; was handed and answers what the test chose, and the token and the saved
;;;; service live in a scratch folder under the run's own home. What is under
;;;; test is what this cell owns -- the multipart it composes, the id and URL
;;;; it accepts, the origin it refuses, the words a refusal carries, and the
;;;; device-code flow that ends with a token file at 0600.

(in-package #:nodecode.test)

(define-test-slice "artifact-relay" "ARTIFACT-RELAY-CELL-" :start nodecode-artifact-relay:start-cell)

(define-cell-lifecycle-tests "artifact-relay"
  (:config "base_url" "https://relay.example" "state_dir" (namestring (scratch-dir)))
  (:help :artifact-relay)
  (:command "artifact-relay")
  (:running
   (is (nle::find-tool "artifact_publish") "the publish tool is registered")
   (is (nle::find-tool "artifact_read") "the read tool too")
   (is (equal "https://relay.example"
              (nodecode-artifact-relay::origin-text (nodecode-artifact-relay::base-origin))) "the section's service is what a publish goes to"))
  (:stopped
   (is (null (nle::find-tool "artifact_publish")) "stop takes both tools back"))
  (:refused ("timeout_seconds" 0) ("timeout_seconds" "soon"))
  (:idle nodecode-artifact-relay:artifact-relay-error (nodecode-artifact-relay::status-line)))

;;; --- the fixture ------------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . OPTIONS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed *HTTP* answers: a function of METHOD and URL => (values TEXT STATUS).")

(defvar *scratch* 0
  "How many scratch folders this run has made, so each test's is its own.")

(defvar *dir* nil
  "The scratch folder of the fixture running now.")

(defun scratch-dir ()
  "A fresh folder under the run's home, holding a token file."
  (let* ((dir (uiop:ensure-directory-pathname
               (nlk:home (format nil "test-artifact-relay/~d/" (incf *scratch*))))))
    (ensure-directories-exist dir)
    (with-open-file (out (merge-pathnames "token" dir) :direction :output
                                                     :if-exists :supersede
                                                     :if-does-not-exist :create)
      (write-string "test-token" out))
    dir))

(defun scripted (&rest answers)
  "An *ANSWER-FN* answering ANSWERS -- (TEXT STATUS) -- in order."
  (lambda (method url)
    (declare (ignore method url))
    (let ((answer (pop answers)))
      (unless answer (error "the test's script has no answer left"))
      (values (first answer) (second answer)))))

(defmacro with-relay ((&rest pairs) &body body)
  "BODY with the cell started on a scratch folder and a stubbed relay. PAIRS
are section members the test adds or overrides; a token file holding
test-token is already there."
  `(let* ((*dir* (scratch-dir))
          (*requests* '())
          (*answer-fn* (lambda (method url)
                         (declare (ignore method url))
                         (error "unexpected request: no script")))
          (nodecode-artifact-relay::*http*
            (lambda (method url &rest options)
              (push (list* method url options) *requests*)
              (funcall *answer-fn* method url))))
     (with-cell-stop ((artifact-relay-start "state_dir" (namestring *dir*)
                                             "base_url" "https://relay.example"
                                             ,@pairs))
       ,@body)))

(defun asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun request-header (request name)
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun request-content-text (request)
  "A request's body as text, whatever the stub was handed."
  (let ((content (getf (cddr request) :content)))
    (typecase content
      (string content)
      ((vector (unsigned-byte 8)) (sb-ext:octets-to-string content :external-format :utf-8))
      (t ""))))

(defparameter +artifact-id+ "abcdefghijklmnopqrstuv"
  "One id of the relay's own shape, which every fixture uses.")

(defun viewer-url (&optional (origin "https://relay.example"))
  (format nil "~a/a/~a" origin +artifact-id+))

(defun args (&rest pairs)
  (apply #'nlk:make-json-object pairs))

;;; --- the two tools ---------------------------------------------------------------

(deftest artifact-relay-cell-publishes-one-multipart-artifact ()
  (with-relay ()
    (setf *answer-fn* (scripted (list (format nil "{\"id\":\"~a\",\"url\":\"~a\"}"
                                               +artifact-id+ (viewer-url))
                                      201)))
    (let ((answer (nodecode-artifact-relay::tool-publish
                   (args "title" "Quarterly report" "content" "# Hello" "summary" "the summary"))))
      (is (search (viewer-url) answer) "the tool answers the viewer URL")
      (is (search +artifact-id+ answer) "and the id the relay minted"))
    (let* ((requests (asked))
           (request (first requests)))
      (is (= 1 (length requests)) "one request, one artifact")
      (is (eq :post (first request)) "a POST")
      (is (equal "https://relay.example/api/artifacts" (second request)) "to the section's service")
      (is (equal "Bearer test-token" (request-header request "authorization")) "with the token file's token")
      (is (search "multipart/form-data; boundary=" (request-header request "content-type")) "as multipart/form-data")
      (let ((body (request-content-text request)))
        (is (search "name=\"title\"" body) "the title is one part")
        (is (search "Quarterly report" body) "with its text")
        (is (search "name=\"content\"; filename=\"artifact.md\"" body) "the source is the file part")
        (is (search "Content-Type: text/markdown" body) "typed as Markdown")
        (is (search "# Hello" body) "and carried whole")
        (is (search "name=\"expires_in_days\"" body) "the expiry the relay reads")
        (is (search "30" body) "defaulting to thirty days")))))

(deftest artifact-relay-cell-reads-the-source-back ()
  (with-relay ()
    (setf *answer-fn*
          (scripted (list (format nil "{\"id\":\"~a\",\"title\":\"Report\",\"format\":\"markdown\",\"content\":\"# Hello again\"}"
                                   +artifact-id+)
                          200)))
    (let ((answer (nodecode-artifact-relay::tool-read (args "url" (viewer-url)))))
      (is (search "# Hello again" answer) "the source comes back whole")
      (is (search "Report" answer) "with the title the relay kept"))
    (let ((request (first (asked))))
      (is (eq :get (first request)) "a GET")
      (is (equal (format nil "https://relay.example/api/artifacts/~a" +artifact-id+)
                 (second request)) "of the id the URL named, on the configured service"))
    (setf *requests* '()
          *answer-fn*
          (scripted (list (format nil "{\"content\":\"# Hello again\"}") 200)))
    (let ((answer (nodecode-artifact-relay::tool-read (args "url" +artifact-id+))))
      (is (search "# Hello again" answer) "a bare id reads the same artifact"))))

(deftest artifact-relay-cell-refuses-another-origin-before-the-token-leaves ()
  (with-relay ()
    (setf *answer-fn* (scripted))
    (let ((answer (nodecode-artifact-relay::tool-read
                   (args "url" (viewer-url "https://elsewhere.example")))))
      (is (search "ERROR:" answer) "a URL on another origin is a refusal")
      (is (search "relay.example" answer) "naming the service it is configured for")
      (is (null (asked)) "and nothing was sent"))
    (setf *answer-fn*
          (scripted (list (format nil "{\"url\":\"~a\"}" (viewer-url "https://elsewhere.example"))
                          201)))
    (let ((answer (nodecode-artifact-relay::tool-publish
                   (args "title" "T" "content" "# C"))))
      (is (search "ERROR:" answer) "a viewer URL off the service refuses the publish")
      (is (= 1 (length (asked))) "after the one request it took to learn that"))))

(deftest artifact-relay-cell-names-the-setting-that-is-missing ()
  (with-relay ("base_url" "")
    (setf *answer-fn* (scripted))
    (let ((answer (nodecode-artifact-relay::tool-publish (args "title" "T" "content" "# C"))))
      (is (search "ERROR:" answer) "a publish with no service is a refusal")
      (is (search "base_url" answer) "naming the member to set")
      (is (null (asked)) "and nothing was sent"))
    (is (search "not ready" (nodecode-artifact-relay::status-line)) "/artifact-relay status says so too"))
  (with-relay ()
    (setf *answer-fn* (scripted))
    (delete-file (merge-pathnames "token" *dir*))
    (let ((answer (nodecode-artifact-relay::tool-publish (args "title" "T" "content" "# C"))))
      (is (search "ERROR:" answer) "a publish with no token is a refusal")
      (is (search "token" answer) "naming the token")
      (is (null (asked)) "and nothing was sent"))))

;;; --- the sign-in -----------------------------------------------------------------

(deftest artifact-relay-cell-signs-in-with-a-device-code ()
  (with-relay ("base_url" "")
    (setf *answer-fn*
          (scripted (list "{\"device_code\":\"dev-code\",\"user_code\":\"ABCD-EFGH\",\"verification_uri_complete\":\"https://relay.lok-labs.com/activate/ABCD-EFGH\",\"interval\":5,\"expires_in\":600}"
                          200)
                    (list "{\"error\":\"authorization_pending\"}" 400)
                    (list "{\"api_token\":\"minted-token\",\"base_url\":\"https://mine.example\"}" 200)
                    (list (format nil "{\"id\":\"~a\",\"url\":\"~a\"}"
                                  +artifact-id+ (viewer-url "https://mine.example"))
                          201)))
    ;; the poll is driven by hand, one attempt at a time, so no thread races the script
    (with-stubbed-fdefinition (nlk:worker-start (name step &rest options)
                               (declare (ignore name step options))
                               nil)
      (let ((answer (nodecode-artifact-relay::begin-setup)))
        (is (search "https://relay.lok-labs.com/activate/ABCD-EFGH" answer) "the operator is sent to the control plane's own activation page")
        (is (search "ABCD-EFGH" answer) "with the code to type")
        (is (eq :post (first (first (asked)))) "the authorization is one POST"))
      (is (null (nodecode-artifact-relay::poll-attempt)) "a pending approval keeps waiting")
      (is (eq :stop (nodecode-artifact-relay::poll-attempt)) "an approval ends the poll")
      (is (search "minted-token" (uiop:read-file-string (merge-pathnames "token" *dir*))) "the token the approval bought is kept")
      (is (search "mine.example" (uiop:read-file-string (merge-pathnames "setup.json" *dir*))) "and the service it belongs to is saved beside it")
      (let ((publish (fourth (asked))))
        (is (equal "https://mine.example/api/artifacts" (second publish)) "the proof artifact goes to the service the sign-in learned, not the section's")
        (is (equal "Bearer minted-token" (request-header publish "authorization")) "with the token just minted")))))
