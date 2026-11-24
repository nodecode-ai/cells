;;;; cell-test.lisp --- the cell against a stubbed 0bull MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches 0bull, no token
;;;; is needed and no phone is touched. What is proved is the cell's own
;;;; wiring: the section (a healthy start, and a missing token refused), the
;;;; seven tools through the dispatch the model calls them by -- the MCP tool
;;;; each one names, the arguments it sends, the bearer token and the session
;;;; id on the wire -- and the /0bull command.

(in-package #:nodecode.test)

(define-test-slice "0bull" "0BULL-CELL-" :start nodecode-0bull:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defparameter *calls* nil
  "Every request the stub answered, newest first, as (METHOD PARAMS HEADERS).")

(defparameter *answer* nil
  "When set, (TEXT STATUS HEADERS): what the stub answers to tools/call.")

(defun envelope (id result)
  "One JSON-RPC answer carrying RESULT, as the wire spells it."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun text-result (text &key errorp)
  "One tools/call result carrying TEXT."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                   "isError" (and errorp t)))

(defun call-result (params)
  "What the stub's tools/call answers: the tool's name and the arguments back."
  (let ((tool (nlk:json-value params :string "name"))
        (arguments (nlk:json-value params :object "arguments")))
    (text-result (format nil "~a answered~@[ ~a~]" tool
                         (and arguments (nlk:encode-json-object arguments))))))

(defun session-headers ()
  (let ((headers (make-hash-table :test #'equal)))
    (setf (gethash "mcp-session-id" headers) "sess-1")
    headers))

(defun stub-wire (options)
  "NLK:HTTP, answering from the request's own method; nothing leaves the machine."
  (let* ((body (nlk:decode-json (or (getf options :content) "{}")))
         (name (nlk:json-value body :text "method"))
         (params (nlk:json-value body :object "params"))
         (id (nlk:json-value body :integer "id")))
    (push (list name params (getf options :headers)) *calls*)
    (cond ((equal name "initialize")
           (values (envelope id (nlk:json-object "protocolVersion" "2025-06-18"
                                                   "capabilities" (nlk:make-json-object)))
                   200 (session-headers) nil))
          ((equal name "notifications/initialized")
           (values "" 202 nil nil))
          ((equal name "tools/call")
           (if *answer*
               (values (first *answer*) (second *answer*) (third *answer*) nil)
               (values (envelope id (call-result params)) 200 nil nil)))
          (t (values "{}" 404 nil nil)))))

(defmacro with-bull-wire (&body body)
  "BODY with NLK:HTTP stubbed: every request is logged and answered here."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (stub-wire options))
       ,@body)))

(defmacro with-bull-running (&body body)
  "BODY with the cell started on a literal section, stopped on unwind."
  `(with-cell-stop ((0bull-start "token" "tok-1"))
     ,@body))

(defun 0bull-request (method)
  "The newest stubbed request of JSON-RPC METHOD, or NIL."
  (find method *calls* :test #'equal :key #'first))

(defun 0bull-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun 0bull-tool-call (&optional (index 0))
  "The INDEX'th newest tools/call the cell sent, as (METHOD PARAMS HEADERS)."
  (nth index (remove-if-not (lambda (call) (equal (first call) "tools/call")) *calls*)))

(defun 0bull-sent-tool (&optional (index 0))
  "The MCP tool name the INDEX'th newest tools/call named."
  (let ((call (0bull-tool-call index)))
    (and call (nlk:json-value (second call) :string "name"))))

(defun 0bull-sent-arguments (&optional (index 0))
  "The arguments object of the INDEX'th newest tools/call."
  (let ((call (0bull-tool-call index)))
    (and call (nlk:json-value (second call) :object "arguments"))))

(defun 0bull-called (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "0bull-call" name arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "0bull"
  (:fixture with-bull-wire)
  (:config "token" "tok-1")
  (:help :0bull)
  (:command "0bull")
  (:running
   (is (nle::find-tool "0bull_phones") "the phones tool is registered")
   (is (nle::find-tool "0bull_accounts") "and the accounts")
   (is (nle::find-tool "0bull_see") "and the screen")
   (is (nle::find-tool "0bull_act") "and one input")
   (is (nle::find-tool "0bull_run") "and queued work")
   (is (nle::find-tool "0bull_post") "and a publication")
   (is (nle::find-tool "0bull_status") "and the status that follows them")
   (is (= 7 (count-if (lambda (entry) (eql 0 (search "0bull_" (first entry))))
                      nle::*tools*)) "seven of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "0bull_phones")) "and stop takes every one back")
   (is (null (nle::find-tool "0bull_status"))))
  (:refused ("token" "tok-1" "timeout_seconds" 0)
            ("token" "tok-1" "max_chars" 10)))

(deftest 0bull-cell-a-start-with-no-token-refuses (with-bull-wire)
  (with-stubbed-fdefinition (nodecode-0bull::mcp-token (url)
                                (declare (ignore url))
                                nil)
    (let ((text (refusal-text nlk:config-refusal (0bull-start))))
      (is-carrying text "token" "/mcp")
      (is (null (nle::find-tool "0bull_phones")) "nothing was installed"))))

(deftest 0bull-cell-the-mcp-cells-sign-in-is-reused (with-bull-wire)
  ;; The shipped mcp cell keeps its OAuth sign-in under the home; this cell
  ;; reads that token rather than asking for one of its own.
  (let ((path (nlk:home "mcp/oauth.json")))
    (unwind-protect
         (progn
           (ensure-directories-exist path)
           (with-open-file (out path :direction :output :if-exists :supersede
                                      :if-does-not-exist :create)
             (write-string (nlk:encode-json-object
                            (nlk:json-object "https://0bull.net/mcp"
                                             (nlk:json-object "tokens"
                                                              (nlk:json-object "access_token" "tok-stored"))))
                           out))
           (with-cell-stop ((0bull-start))
             (0bull-called "0bull_phones" "{}")
             (is (equal "Bearer tok-stored"
                        (0bull-request-header (0bull-tool-call) "authorization")) "the token the mcp cell kept is the one sent")))
      (when (probe-file path) (ignore-errors (delete-file path))))))

;;; --- the wire ---------------------------------------------------------------------

(deftest 0bull-cell-one-session-serves-every-call (with-bull-wire)
  (with-bull-running
    (0bull-called "0bull_phones" "{}")
    (0bull-called "0bull_accounts" "{}")
    (is (= 1 (count-if (lambda (call) (equal (first call) "initialize")) *calls*)) "initialize happens once and is reused")
    (is (equal "Bearer tok-1" (0bull-request-header (0bull-tool-call) "authorization")) "with this cell's own token")
    (is (equal "sess-1" (0bull-request-header (0bull-tool-call) "mcp-session-id")) "and the session id the server handed is echoed")))

(deftest 0bull-cell-an-error-the-server-reports-fails-the-call (with-bull-wire)
  (with-bull-running
    (setf *answer* (list (envelope 1 (text-result "no phone on this account" :errorp t))
                         200 nil))
    (let ((answer (0bull-called "0bull_phones" "{}")))
      (is (search "no phone on this account" answer) "the server's words reach the model"))))

;;; --- the tools say what the skills say ---------------------------------------------

(deftest 0bull-cell-phones-asks-for-the-phone-list (with-bull-wire)
  (with-bull-running
    (0bull-called "0bull_phones" "{}")
    (is (equal "list-phones-tool" (0bull-sent-tool)) "list-phones-tool, as the skills name it")))

(deftest 0bull-cell-accounts-passes-the-platform-it-was-given (with-bull-wire)
  (with-bull-running
    (0bull-called "0bull_accounts" "{\"platform\": \"tiktok\"}")
    (is (equal "list-accounts-tool" (0bull-sent-tool)))
    (is (equal "tiktok" (nlk:json-value (0bull-sent-arguments) :text "platform")) "the filter goes through as the server names it")))

(deftest 0bull-cell-act-sends-one-input-with-its-coordinates (with-bull-wire)
  (with-bull-running
    (0bull-called "0bull_act" "{\"slot\": \"s-1\", \"op\": \"tap\", \"fx\": 0.5, \"fy\": 0.25}")
    (is (equal "phone-control-tool" (0bull-sent-tool)))
    (let ((sent (0bull-sent-arguments)))
      (is (equal "s-1" (nlk:json-value sent :text "slot")))
      (is (equal "tap" (nlk:json-value sent :text "op")))
      (is (= 0.5 (nlk:json-value sent :number "fx")) "a fraction of the screen, not pixels")
      (is (= 0.25 (nlk:json-value sent :number "fy"))))))

(deftest 0bull-cell-run-queues-a-macro-and-says-how-to-follow-it (with-bull-wire)
  (with-bull-running
    (let ((answer (0bull-called "0bull_run"
                                 "{\"slot\": \"s-1\", \"kind\": \"macro\", \"name\": \"morning\"}")))
      (is (equal "run-macro-tool" (0bull-sent-tool)))
      (is (equal "morning" (nlk:json-value (0bull-sent-arguments) :text "workflow")))
      (is (search "0bull_status" answer) "the model is told how to follow the run"))))

(deftest 0bull-cell-see-answers-the-screen-text (with-bull-wire)
  (with-bull-running
    (0bull-called "0bull_see" "{\"slot\": \"s-1\"}")
    (is (equal "phone-ocr-tool" (0bull-sent-tool)) "text is the default: what the model matches on")))

(deftest 0bull-cell-see-saves-a-snapshot-to-look-at (with-bull-wire)
  (setf *answer*
        (list (envelope 1 (nlk:json-object
                           "content" (vector (nlk:json-object "type" "image"
                                                                "data" "aGVsbG8="
                                                                "mimeType" "image/jpeg"))))
              200 nil))
  (with-bull-running
    (let ((answer (0bull-called "0bull_see" "{\"slot\": \"s-1\", \"mode\": \"image\"}")))
      (is (equal "phone-snapshot-tool" (0bull-sent-tool)))
      (is (search "0bull/screens" answer) "the picture is written under this home")
      (is (search "(look" answer) "and the model is told how to look at it"))))

(deftest 0bull-cell-post-queues-a-tiktok-with-its-caption (with-bull-wire)
  (with-bull-running
    (0bull-called "0bull_post"
                   "{\"platform\": \"tiktok\", \"account_id\": \"a-1\", \"video_url\": \"https://example.test/v.mp4\", \"caption\": \"a cat\"}")
    (is (equal "create-submission-tool" (0bull-sent-tool)))
    (let ((sent (0bull-sent-arguments)))
      (is (equal "tiktok" (nlk:json-value sent :text "platform")))
      (is (equal "a cat" (nlk:json-value sent :text "caption")))
      (is (equal "https://example.test/v.mp4" (nlk:json-value sent :text "video_url"))))))

(deftest 0bull-cell-a-youtube-short-needs-a-title-before-anything-is-sent (with-bull-wire)
  (with-bull-running
    (let ((answer (0bull-called "0bull_post"
                                 "{\"platform\": \"youtube\", \"account_id\": \"a-1\", \"video_url\": \"https://example.test/v.mp4\"}")))
      (is (search "title" answer) "the skills' rule, said as a refusal")
      (is (null (0bull-tool-call)) "and nothing was queued for it"))))

(deftest 0bull-cell-status-follows-a-run-or-a-submission (with-bull-wire)
  (with-bull-running
    (0bull-called "0bull_status" "{\"kind\": \"submission\", \"id\": \"sub-1\"}")
    (is (equal "get-submission-tool" (0bull-sent-tool)))
    (is (equal "sub-1" (nlk:json-value (0bull-sent-arguments) :text "submission_id")))
    (0bull-called "0bull_status" "{\"kind\": \"run\", \"id\": \"run-1\", \"slot\": \"s-1\"}")
    (is (equal "get-phone-run-tool" (0bull-sent-tool)))
    (is (equal "run-1" (nlk:json-value (0bull-sent-arguments) :text "run_id")))))

;;; --- the operator's command ---------------------------------------------------------

(deftest 0bull-cell-the-command-lists-the-phones (with-bull-wire)
  (with-bull-running
    (let ((answer (nodecode-0bull::run-command "" nil)))
      (is (search "list-phones-tool" answer) "no argument lists the phones"))
    (is (equal "list-phones-tool" (0bull-sent-tool)))))

(deftest 0bull-cell-the-command-says-where-the-token-came-from (with-bull-wire)
  (with-bull-running
    (let ((answer (nodecode-0bull::run-command "login" nil)))
      (is (search "token in use comes from" answer) "the operator is told, in words")
      (is (not (search "tok-1" answer)) "and never the token itself"))))
