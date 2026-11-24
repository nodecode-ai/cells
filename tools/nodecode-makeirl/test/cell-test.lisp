;;;; cell-test.lisp --- the cell's own wiring, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the config surface (a section that names no token
;;;; refuses with a sentence naming the three members, and an out-of-range
;;;; timeout refuses at start), the two tools against a stubbed MakeIRL (the
;;;; handshake, the bearer header, the arguments that reach the server, a
;;;; failed result, a server-sent answer), /makeirl, and the manual.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "makeirl"
  (:fixture with-stubbed-makeirl)
  (:config "access_token" "throwaway-token")
  (:help :makeirl)
  (:command "makeirl")
  (:running
   (is (equal '("makeirl_call" "makeirl_tools") (sort (registered-tools) #'string<)) "the two tools are registered")
   (is (null *requests*) "and the start asks the server nothing")
   (is (equal nodecode-makeirl::+manual+ (nle:help :makeirl)) "(help :makeirl) is the manual"))
  (:stopped
   (is (null (nle::find-tool "makeirl_call")) "stop unregisters the tools")
   (is (null nodecode-makeirl::*session*) "and forgets the session"))
  (:refused ("timeout_seconds" 1)))

;;; --- the tools, through the server ------------------------------------------------

(deftest makeirl-cell-tools-come-from-the-server (with-stubbed-makeirl)
  (with-cell-stop ((makeirl-start "access_token" "throwaway-token"))
    (let ((text (call-tool "makeirl_tools")))
      (is (search "release_create_upload" text) "a server tool is listed")
      (is (search "release_status" text) "and so is the one beside it")
      (is (search "read-only" text) "with the hint the server annotated")
      (is (search "runId:string*" text) "and the arguments it takes, the required one starred"))
    (is (equal '("initialize" "notifications/initialized" "tools/list")
               (reverse (mapcar #'first *requests*))) "one handshake, then the listing")
    (let ((first-request (first (last *requests*))))
      (is (equal "Bearer throwaway-token" (request-header (second first-request) "authorization")) "the token rode the handshake")
      (is (search "text/event-stream" (request-header (second first-request) "accept")) "and the client takes a streamed answer")
      (is (equal "session-1" nodecode-makeirl::*session*) "the session id is kept"))))

(deftest makeirl-cell-a-filter-keeps-only-the-matching-tools (with-stubbed-makeirl)
  (with-cell-stop ((makeirl-start "access_token" "throwaway-token"))
    (let ((text (call-tool "makeirl_tools" "filter" "upload")))
      (is (search "release_create_upload" text) "the matching tool is there")
      (is (not (search "release_status" text)) "and the others are not"))
    (is (search "no tool matching" (call-tool "makeirl_tools" "filter" "zzz")) "a word nothing carries says so")))

(deftest makeirl-cell-a-call-carries-its-tool-and-arguments (with-stubbed-makeirl)
  (with-cell-stop ((makeirl-start "access_token" "throwaway-token"))
    (let ((text (call-tool "makeirl_call"
                            "tool" "release_status"
                            "arguments" (nlk:json-object "runId" "run-7"))))
      (is (search "run run-7: REVIEW" text) "the answer is the server's own text"))
    (let* ((call (assoc "tools/call" (reverse *requests*) :test #'equal))
           (message (nlk:decode-json (third call))))
      (is (equal "release_status" (nlk:json-value message :text "params" "name")) "the tool named the model named")
      (is (equal "run-7" (nlk:json-value message :text "params" "arguments" "runId")) "and the argument object reached it unchanged")))
  (with-cell-stop ((makeirl-start "access_token" "throwaway-token"))
    (is (search "release_create_upload ok" (call-tool "makeirl_call"
                                                      "tool" "release_create_upload")) "a call with no arguments sends an empty object")))

(deftest makeirl-cell-a-failed-result-is-an-error (with-stubbed-makeirl)
  (with-cell-stop ((makeirl-start "access_token" "throwaway-token"))
    (setf *server*
          (lambda (message)
            (declare (ignore message))
            (json-answer (nlk:json-object
                          "jsonrpc" "2.0" "id" 1
                          "result" (nlk:json-object
                                    "isError" t
                                    "content" (vector (nlk:json-object
                                                       "type" "text"
                                                       "text" "no such run")))))))
    (let ((text (call-tool "makeirl_call" "tool" "release_status")))
      (is (search "ERROR:" text) "a failed call answers ERROR:")
      (is (search "no such run" text) "with the server's own words"))))

(deftest makeirl-cell-a-streamed-answer-is-read (with-stubbed-makeirl)
  (with-cell-stop ((makeirl-start "access_token" "throwaway-token"))
    (is (search "BLOCKED" (call-tool "makeirl_call" "tool" "release_list_findings"
                                                  "arguments" (nlk:json-object "files" #()))) "a server-sent frame answers the same as a JSON body")))

;;; --- the token ------------------------------------------------------------------

(deftest makeirl-cell-no-token-refuses-with-the-three-members (with-stubbed-makeirl)
  (with-cell-stop ((makeirl-start "endpoint" "https://makeirl.test/api/mcp"))
    (let ((text (refusal-text makeirl:makeirl-error (call-tool "makeirl_tools"))))
      (is (search "no MakeIRL token" text) "the refusal says what is missing")
      (is (search "access_token" text) "and names the member to paste it in")
      (is (search "token_env" text) "and the variable it may name")
      (is (search "token_file" text) "and the file it may name"))))

(deftest makeirl-cell-the-token-comes-from-the-file (with-stubbed-makeirl)
  (with-cell-stop ((makeirl-start "token_file" (token-file)))
    (call-tool "makeirl_tools")
    (let ((first-request (first (last *requests*))))
      (is (equal "Bearer throwaway-token" (request-header (second first-request) "authorization")) "the token the file holds rode the handshake"))))

(deftest makeirl-cell-a-401-names-the-sign-in (with-stubbed-makeirl)
  (with-cell-stop ((makeirl-start "access_token" "stale-token"))
    (setf *server* (lambda (message) (declare (ignore message)) (values "{}" 401 nil)))
    (let ((text (refusal-text makeirl:makeirl-error (call-tool "makeirl_tools"))))
      (is (search "refused the token (401)" text) "a refused token is named")
      (is (search "sign in at makeirl.com" text) "with where to sign in"))))

;;; --- /makeirl and the manual -------------------------------------------------------

(deftest makeirl-cell-the-command-lists-and-calls (with-stubbed-makeirl)
  (with-cell-stop ((makeirl-start "access_token" "throwaway-token"))
    (is (search "release_create_upload" (nodecode-makeirl:makeirl-command "" "s1")) "typed alone, the command lists what the server offers")
    (is (search "release_status" (nodecode-makeirl:makeirl-command "tools" "s1")) "`tools' does the same")
    (is (search "run run-3: REVIEW"
                (nodecode-makeirl:makeirl-command "release_status {\"runId\":\"run-3\"}" "s1")) "a tool name and a JSON object call it")))

(deftest makeirl-cell-the-manual-carries-the-release-gate ()
  (is (search "release_list_findings" nodecode-makeirl::+manual+) "the workflow names its steps")
  (is (search "does not prove that the" nodecode-makeirl::+manual+) "and what a report is not")
  (is (search "payment happens in the browser" nodecode-makeirl::+manual+) "and how a purchase goes"))
