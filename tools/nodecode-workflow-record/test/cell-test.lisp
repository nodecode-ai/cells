;;;; cell-test.lisp --- lifecycle; start to stop over a stubbed page; the
;;;; recording model; the refusals; /record.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials anything. The one stub is the transport: *DISCOVER*
;;;; answers a page, *OPEN-LINK* answers a link whose SEND plays the part of a
;;;; browser -- a reply to each command, then the events a person would have
;;;; made in the tab. Everything above the socket is the shipping code: the
;;;; same recorder script, the same normalization, the same masking, the same
;;;; file.

(in-package #:nodecode.test)

(define-test-slice "workflow-record" "WORKFLOW-RECORD-CELL-" :start nodecode-workflow-record:start-cell)

;;; --- the stubbed page ----------------------------------------------------------------

(defvar *sent* '()
  "The CDP methods the fake page received, newest first.")

(defvar *page-events* '()
  "The binding payloads the fake page sends once the recorder is installed.")

(defun fake-json (object)
  (nlk:encode-json-object object))

(defun fake-send (json on-message)
  "Play the browser: answer every command, and after Runtime.evaluate send the
events a person would have made in the tab."
  (let* ((command (nlk:decode-json json))
         (method (nlk:json-value command :string "method"))
         (id (nlk:json-value command :integer "id")))
    (push method *sent*)
    (when (equal method "Runtime.evaluate")
      (funcall on-message (fake-json (nlk:json-object "id" id "result" (nlk:json-object))))
      (dolist (event *page-events*)
        (funcall on-message
                 (fake-json (nlk:json-object
                             "method" "Runtime.bindingCalled"
                             "params" (nlk:json-object
                                       "name" nodecode-workflow-record:+binding+
                                       "payload" (nlk:encode-json-object event))))))
      (funcall on-message
               (fake-json (nlk:json-object
                           "method" "Page.frameNavigated"
                           "params" (nlk:json-object
                                     "frame" (nlk:json-object
                                              "url" "https://example.test/home"))))))))

(defun fake-discover (endpoint)
  (declare (ignore endpoint))
  (list :ws-url "ws://stub/devtools/page/1" :url "https://example.test/start" :title "Example"))

(defun no-browser (endpoint)
  (declare (ignore endpoint))
  (error 'nodecode-workflow-record:workflow-record-error
         :detail "nothing listens on the debugging endpoint"))

(defmacro with-stubbed-cdp (&body body)
  "BODY over a page that answers like a browser and an endpoint that is there."
  `(progn
     (setf *sent* '())
     ;; a clean shelf: a slot's scratch home outlives one run of this file
     (dolist (old (uiop:directory-files (nlk:home "recordings/") "*.json"))
       (ignore-errors (delete-file old)))
     (let ((nodecode-workflow-record:*discover* 'fake-discover)
           (nodecode-workflow-record:*open-link*
             (lambda (ws-url on-message)
               (declare (ignore ws-url))
               (nodecode-workflow-record:make-link
                :send (lambda (json) (fake-send json on-message))
                :close (lambda () nil)))))
       ,@body)))

(defun page-script ()
  "The events a person makes in the tab: sign in as alice, type a password and
press Enter. A global, not a binding: the fake page reads it on the thread
that records."
  (let ((base 1763000000000))
    (setf *page-events*
          (list (nlk:make-json-object "type" "click" "selector" "#login > button"
                                       "text" "Sign in" "tag" "button" "t" (+ base 900))
                (nlk:make-json-object "type" "input" "selector" "input[name=\"user\"]"
                                       "value" "alice" "inputType" "text" "t" (+ base 1500))
                (nlk:make-json-object "type" "input" "selector" "input[name=\"pw\"]"
                                       "value" "hunter2" "inputType" "password" "t" (+ base 2200))
                (nlk:make-json-object "type" "enter" "selector" "input[name=\"pw\"]"
                                       "t" (+ base 2600))))))

(defun call-record (action &key slug note cdp-url)
  "The record tool as the model calls it: a contract payload in, a string out."
  (nodecode-workflow-record:record-tool
   (nodecode-workflow-record:decode-record-call
    (apply #'nlk:make-json-object
           (append (list "action" action)
                   (when slug (list "slug" slug))
                   (when note (list "note" note))
                   (when cdp-url (list "cdp_url" cdp-url)))))))

(defun saved-files ()
  (sort (uiop:directory-files (nlk:home "recordings/") "*.json") #'string< :key #'namestring))

;;; --- lifecycle ----------------------------------------------------------------------

(define-cell-lifecycle-tests "workflow-record"
  (:fixture with-stubbed-cdp)
  (:config "max_steps" 500)
  (:help :record)
  (:command "record")
  (:running (is (search "demonstrate a browser workflow once" (nle:help :record)))
            (is (= 500 (nodecode-workflow-record::setting :max-steps))))
  (:refused ("max_steps" 0) ("timeout_seconds" 0)))

;;; --- the tool over a stubbed page ------------------------------------------------------

(deftest workflow-record-cell-start-stop-saves-the-demonstration (with-stubbed-cdp)
  (page-script)
  (with-cell-stop ((workflow-record-start))
    (let ((started (call-record "start" :slug "Checkout Flow")))
      (is (search "recording \"checkout-flow\"" started)
          (format nil "start answered: ~a" started))
      (is (search "https://example.test/" started)
          (format nil "start names the page: ~a" started))
      (is (equal '("Runtime.enable" "Page.enable" "Runtime.addBinding"
                   "Page.addScriptToEvaluateOnNewDocument" "Runtime.evaluate")
                 (reverse *sent*))
          (format nil "five commands went out, in order: ~s" (reverse *sent*)))
      (let ((stopped (call-record "stop")))
        (is (search "5 steps recorded" stopped)
            (format nil "stop answered: ~a" stopped))
        (is (search "record action show" stopped) "the model is told how to read it back")
        (let* ((file (first (saved-files)))
               (json (and file (nlk:decode-json (uiop:read-file-string file))))
               (steps (and json (nlk:json-value json :array "steps"))))
          (is (stringp (namestring (or file (nlk:home "recordings/missing.json"))))
              (format nil "the recording is under recordings/: ~s" (saved-files)))
          (is (= 1 (nlk:json-value json :integer "version")))
          (is (equal "checkout-flow" (nlk:json-value json :string "slug")))
          (is (equal "https://example.test/home" (nlk:json-value json :string "url"))
              "the last navigation is the page the recording ends on")
          (is (= 5 (length steps)))
          (is (= 0.0 (coerce (nlk:json-value (aref steps 0) :number "t") 'single-float))
              "the first step is 0.0 seconds")
          (is (equal "click" (nlk:json-value (aref steps 0) :string "type")))
          (is (equal "Sign in" (nlk:json-value (aref steps 0) :string "text")))
          (is (equal "alice" (nlk:json-value (aref steps 1) :string "value")))
          (is (equal "{SECRET:pw}" (nlk:json-value (aref steps 2) :string "value"))
              "the password never reaches disk")
          (is (equal "navigate" (nlk:json-value (aref steps 4) :string "type")))
          (is (not (search "hunter2" (uiop:read-file-string file)))
              (format nil "not anywhere in the file: ~a" (uiop:read-file-string file)))
          (is (search "checkout-flow" (call-record "list")) "list names it")
          (is (search "{SECRET:pw}" (call-record "show" :slug "checkout-flow"))
              "show hands the model the same JSON"))))))

(deftest workflow-record-cell-a-manual-step-and-the-cap (with-stubbed-cdp)
  (page-script)
  (with-cell-stop ((workflow-record-start "max_steps" 3))
    (let ((started (call-record "start" :slug "capped")))
      (is (search "recording \"capped\"" started) (format nil "start answered: ~a" started)))
    (is (search "the login page" (call-record "step" :note "the login page"))
        "narration is a step of its own")
    (let ((stopped (call-record "stop")))
      (is (search "3 step" stopped) (format nil "stop answered: ~a" stopped))
      (is (search "older steps dropped at the cap" stopped)))))

(deftest workflow-record-cell-refuses-what-it-cannot-do (with-stubbed-cdp)
  (with-cell-stop ((workflow-record-start))
    (is (search "ERROR: WORKFLOW-RECORD-ERROR: no recording is running"
                (call-record "stop"))
        "stop with nothing recording")
    (is (search "needs a name" (call-record "start" :slug "!!!")
                ) "a slug from nothing")
    (is (handler-case (progn (call-record "dance") nil) (error () t))
        "the contract refuses an action outside its list")
    (let ((nodecode-workflow-record:*discover* 'no-browser))
      (is (search "ERROR: WORKFLOW-RECORD-ERROR: could not record: nothing listens"
                  (call-record "start" :slug "gone"))
          "a browser that is not there is one line, not a signal"))))

;;; --- the recording model, function by function ---------------------------------------

(deftest workflow-record-cell-the-model-is-a-function-of-its-input ()
  (is (equal "{SECRET:pw}" (nodecode-workflow-record:mask-value "hunter2" "password" "" "pw")))
  (is (equal "{SECRET:user}" (nodecode-workflow-record:mask-value "x" "" "username current-password" "user"))
      "a secret named among other autocomplete tokens")
  (is (equal "alice" (nodecode-workflow-record:mask-value "alice" "text" "" "")))
  (is (nodecode-workflow-record:secret-field-p "text" "one-time-code"))
  (is (not (nodecode-workflow-record:secret-field-p "text" "username")))
  (is (null (nodecode-workflow-record:normalize-event
             (nlk:make-json-object "type" "scroll" "t" 1)))
      "an unknown step type is dropped")
  (let* ((recording (nodecode-workflow-record:build-recording
                      "slug" "https://example.test/" "2026-11-08T00:00:00Z"
                      (list (list :t 3000 :type "enter" :selector "input")
                            (list :t 1000 :type "click" :selector "#go" :text "Go"))))
         (steps (nlk:json-value recording :array "steps")))
    (is (= 1 (nlk:json-value recording :integer "version")))
    (is (= 2 (length steps)))
    (is (equal "click" (nlk:json-value (aref steps 0) :string "type")) "sorted by the page's clock")
    (is (= 2.0 (coerce (nlk:json-value (aref steps 1) :number "t") 'single-float))
        (format nil "rebased to seconds: 3000ms after the first step is 2.0s, got ~s"
                (nlk:json-value (aref steps 1) :number "t"))))
  (let ((targets (nodecode-workflow-record:decode-targets
                  (nlk:decode-json "[{\"type\":\"page\",\"url\":\"devtools://x\",\"webSocketDebuggerUrl\":\"ws://a\"},
                                     {\"type\":\"page\",\"url\":\"https://example.test/\",\"webSocketDebuggerUrl\":\"ws://b\"},
                                     {\"type\":\"other\",\"url\":\"\"}]"))))
    (is (equal "ws://b" (getf (nodecode-workflow-record:pick-page targets) :ws-url))
        "the first web page, not a devtools tab")))

;;; --- /record ---------------------------------------------------------------------------

(deftest workflow-record-cell-slash-records-reads-and-refuses (with-stubbed-cdp)
  (page-script)
  (with-cell-stop ((workflow-record-start))
    (multiple-value-bind (text dialog) (nodecode-workflow-record:run-slash "" nil)
      (is (search "not recording" text))
      (is dialog "bare /record answers a panel"))
    (is (search "recording \"shop\"" (nodecode-workflow-record:run-slash "start shop" nil))
        "start names the recording")
    (is (search "recording shop" (nodecode-workflow-record:run-slash "" nil)) "the bare line says so")
    (is (search "5 steps recorded" (nodecode-workflow-record:run-slash "stop" nil)))
    (is (search "shop-" (nodecode-workflow-record:run-slash "list" nil))
        "the list reads the disk")
    (is (search "{SECRET:pw}" (nodecode-workflow-record:run-slash "show shop" nil))
        "show hands back the JSON it saved")
    (is (search "no recording is running" (nodecode-workflow-record:run-slash "stop" nil))
        (format nil "a refusal is a line, not a signal: ~a"
                (nodecode-workflow-record:run-slash "stop" nil)))))
