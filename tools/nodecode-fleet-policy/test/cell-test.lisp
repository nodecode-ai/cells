;;;; cell-test.lisp --- the fleet-policy cell against a stubbed MCP endpoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches a network: N LK:HTTP is stubbed with a function that
;;;; answers the three requests one MCP session makes -- initialize, the
;;;; initialized notification, and the one tools/call -- and every state file
;;;; lands under the suite's own scratch home, because the section's default
;;;; state folder is a folder in the home. What is proved is the cell's own
;;;; wiring: a start installs the tool, the manual topic and the command and a
;;;; stop takes them back; the section's shape is the loader's refusal; and
;;;; the verb reports an id once, answers the gate line when there is nothing
;;;; new, reads an item out of a relay's embedded document, records nothing
;;;; when the read failed, and refuses without an endpoint.

(in-package #:nodecode.test)

(define-test-slice "fleet-policy" "FLEET-POLICY-CELL-"
  :start nodecode-fleet-policy:start-cell)

;;; --- the stub -----------------------------------------------------------------

(defvar *fleet-answer* "{}"
  "The JSON text the stubbed tools/call answers as its one content block.")

(defvar *fleet-asked* '()
  "Every request body the stub was handed, newest first.")

(defun fleet-tool-body (text)
  "One MCP tools/call answer (a server-sent event) carrying TEXT."
  (format nil "event: message~%data: {\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"content\":[{\"type\":\"text\",\"text\":~s}],\"isError\":false}}~%~%"
          text))

(defun fleet-http (method url &rest args)
  "The stub: one MCP session, answered in order."
  (declare (ignore method url))
  (let ((content (or (getf args :content) "")))
    (push content *fleet-asked*)
    (cond
      ((search "notifications/initialized" content) (values "" 202 '()))
      ((search "\"initialize\"" content)
       (values "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-06-18\"}}"
               200 '(("Mcp-Session-Id" . "s-1") ("Content-Type" . "application/json"))))
      ((search "\"tools/call\"" content) (values (fleet-tool-body *fleet-answer*) 200 '()))
      (t (error "unexpected MCP request: ~a" content)))))

(defmacro with-fleet-mcp (&body body)
  "BODY with N LK:HTTP answering the endpoint the section names."
  `(with-stubbed-fdefinition (nlk:http (method url &rest args)
                               (apply #'fleet-http method url args))
     ,@body))

(defun fleet-items (&rest items)
  "A tools/call answer carrying ITEMS, each a (FIELD . VALUE) alist, under items."
  (let ((rows (mapcar (lambda (item)
                         (format nil "{~{\"~a\":~s~^,~}}"
                                 (loop for (field . value) in item
                                       append (list field value))))
                       items)))
    (format nil "{\"items\":[~{~a~^,~}]}" rows)))

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "fleet-policy"
  (:config "server_url" "http://mcp.test/mcp")
  (:help :fleet-policy)
  (:command "fleet-policy")
  (:running (is (nle::find-tool "fleet_news") "the tool is registered")
            (is (nlk:contract-p (second (nle::find-tool "fleet_news"))) "against its boundary")
            (is (equal "http://mcp.test/mcp" (nodecode-fleet-policy::endpoint)))
            (is (search "fleet-policy" (namestring (nodecode-fleet-policy:state-directory))) "under the home"))
  (:stopped (is (null (nle::find-tool "fleet_news")) "stop unregisters the tool"))
  (:refused ("server_url" "http://mcp.test/mcp" "max_seen" 0)
            ("server_url" "not a url"))
  (:idle fleet-policy:fleet-policy-error (fleet-policy:watch-new-items :tool "LIST")))

;;; --- the verb -----------------------------------------------------------------

(deftest fleet-policy-cell-reports-an-id-once ()
  (with-cell-stop ((fleet-policy-start "server_url" "http://mcp.test/mcp"))
    (let ((*fleet-asked* '())
          (*fleet-answer* (fleet-items '(("id" . "a") ("title" . "first"))
                                        '(("id" . "b") ("title" . "second")))))
      (with-fleet-mcp
        ;; The first call is a baseline: the ids are recorded, nothing reported.
        (let ((first (fleet-policy:watch-new-items :tool "LIST_MAIL")))
          (is (search "baseline recorded: 2 id(s)" first))
          (is (search "{\"wakeAgent\": false}" first) "and the gate line it always carries"))
        (is (= 3 (length *fleet-asked*)) "one session per call: initialize, notification, tools/call")
        (is (search "notifications/initialized" (second *fleet-asked*)))
        ;; Nothing new: the stock gate line, and nothing else.
        (is (equal "{\"wakeAgent\": false}"
                   (fleet-policy:watch-new-items :tool "LIST_MAIL")))
        ;; One new item, reported with the fields asked for.
        (setf *fleet-answer* (fleet-items '(("id" . "a") ("title" . "first"))
                                           '(("id" . "b") ("title" . "second"))
                                           '(("id" . "c") ("title" . "third"))))
        (let ((third (fleet-policy:watch-new-items :tool "LIST_MAIL" :show-fields '("title"))))
          (is (search "1 new item(s) from LIST_MAIL" third))
          (is (search "- id=c | title: third" third))
          (is (not (search "id=a" third)) "a seen item is not repeated"))
        ;; The state remembers all three, and a fourth call reports nothing.
        (multiple-value-bind (seen have-state)
            (nodecode-fleet-policy::read-state (nodecode-fleet-policy::state-file "LIST_MAIL"))
          (is have-state "the state is on disk")
          (is (equal '("a" "b" "c") seen)))
        (is (equal "{\"wakeAgent\": false}"
                   (fleet-policy:watch-new-items :tool "LIST_MAIL")))
        ))))

(deftest fleet-policy-cell-reads-an-item-out-of-an-embedded-document ()
  ;; A relay often wraps the provider's answer in a string: the id is two
  ;; documents deep, and the walk finds it anyway.
  (with-cell-stop ((fleet-policy-start "server_url" "http://mcp.test/mcp"))
    (let ((*fleet-answer*
            (format nil "{\"result\":{\"payload\":~s}}"
                    "{\"rows\":[{\"id\":\"r1\",\"subject\":\"one\"},{\"id\":\"r2\"}]}")))
      (with-fleet-mcp
        (is (search "baseline recorded: 2 id(s)"
                    (fleet-policy:watch-new-items :tool "RELAY")))
        (setf *fleet-answer*
              (format nil "{\"result\":{\"payload\":~s}}"
                      "{\"rows\":[{\"id\":\"r1\"},{\"id\":\"r2\"},{\"id\":\"r3\",\"subject\":\"three\"}]}"))
        (let ((report (fleet-policy:watch-new-items :tool "RELAY" :show-fields '("subject"))))
          (is (search "1 new item(s)" report))
          (is (search "id=r3 | subject: three" report)))))))

(deftest fleet-policy-cell-a-failed-read-marks-nothing-seen ()
  (with-cell-stop ((fleet-policy-start "server_url" "http://mcp.test/mcp"))
    (let ((*fleet-answer* "{\"successful\":false,\"error\":\"upstream timeout\"}"))
      (with-fleet-mcp
        (is (search "reported a failure"
                    (handler-case (progn (fleet-policy:watch-new-items :tool "BROKEN") "")
                      (fleet-policy:fleet-policy-error (condition) (princ-to-string condition)))))
        (is (null (probe-file (nodecode-fleet-policy::state-file "BROKEN"))) "nothing recorded")
        ;; An answer that says successful: true is a good read, not a failure.
        (setf *fleet-answer* "{\"successful\":true,\"items\":[{\"id\":\"ok1\"}]}")
        (is (search "baseline recorded: 1 id(s)" (fleet-policy:watch-new-items :tool "BROKEN")))))))

(deftest fleet-policy-cell-refuses-without-an-endpoint ()
  (with-cell-stop ((fleet-policy-start))
    (with-fleet-mcp
      (is (search "no server_url is set"
                  (handler-case (progn (fleet-policy:watch-new-items :tool "LIST") "")
                    (fleet-policy:fleet-policy-error (condition) (princ-to-string condition))))))))

(deftest fleet-policy-cell-a-dry-run-records-nothing ()
  (with-cell-stop ((fleet-policy-start "server_url" "http://mcp.test/mcp"))
    (let ((*fleet-answer* (fleet-items '(("id" . "d1") ("title" . "only")))))
      (with-fleet-mcp
        (is (search "dry run" (fleet-policy:watch-new-items :tool "DRY" :dry-run t)))
        (is (null (probe-file (nodecode-fleet-policy::state-file "DRY"))) "nothing recorded")))))

;;; --- the tool, through the real dispatch ---------------------------------------

(deftest fleet-policy-cell-the-tool-answers-through-the-wire (with-session-store ("s1"))
  (with-cell-stop ((fleet-policy-start "server_url" "http://mcp.test/mcp"))
    (let ((*fleet-answer* (fleet-items '(("id" . "w1") ("title" . "wired")))))
      (with-fleet-mcp
        ;; The boundary carries the arguments; the endpoint is the section's.
        (let ((result (execute-wire-call "call-1" "fleet_news"
                                         "{\"tool\":\"WIRED\",\"dry_run\":true}")))
          (is (equal "tool" (gethash "role" result)))
          (is (search "dry run" (princ-to-string (nle::message-content result)))))
        ;; A call that leaves `tool' out is repaired by the boundary, not by the body.
        (let ((missing (execute-wire-call "call-2" "fleet_news" "{\"dry_run\":true}")))
          (is (search "tool must be present" (princ-to-string (nle::message-content missing)))))))))

;;; --- the command ---------------------------------------------------------------

(deftest fleet-policy-cell-the-command-lists-what-each-watch-remembers ()
  (with-cell-stop ((fleet-policy-start "server_url" "http://mcp.test/mcp"))
    (let ((*fleet-answer* (fleet-items '(("id" . "s1") ("title" . "seen")))))
      (with-fleet-mcp
        (fleet-policy:watch-new-items :tool "STATE_TOOL")
        (let ((listing (nodecode-fleet-policy::run-command "" "s1")))
          (is (search "endpoint http://mcp.test/mcp" listing))
          (is (search "STATE_TOOL" listing))
          (is (search "1 id(s)" listing)))
        (let ((dry (nodecode-fleet-policy::run-command "STATE_TOOL" "s1")))
          (is (search "dry run" dry) "one argument is a dry run"))))))
