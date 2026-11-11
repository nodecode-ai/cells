;;;; cell-test.lisp --- the t3mp3st cell, end to end over a scripted War Room.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one network call is NODECODE-T3MP3ST::HTTP; WITH-SCRIPTED-WAR-ROOM
;;;; stubs that name, records every (method . url) asked for, and answers a
;;;; fixture -- so no test needs a War Room, a port or a socket, and T3MP3ST
;;;; itself (AGPL, a separate program) is never installed by this slice. The
;;;; lifecycle tests are the machinery's own; the rest check what a person
;;;; gets: an idle server said to be idle, a mission read out as its phase and
;;;; progress, a launch that carries its targets and names the gate holding it,
;;;; a refusal that says what a launch needs, and the refusal that tells you
;;;; how to start the War Room.

(in-package #:nodecode.test)

(define-test-slice "t3mp3st" "T3MP3ST-CELL-" :start nodecode-t3mp3st:start-cell)

;;; --- the scripted seam ---------------------------------------------------------------

(defmacro with-scripted-war-room ((calls &key answer) &body body-forms)
  "Run BODY-FORMS with NODECODE-T3MP3ST::HTTP recording each (method . url)
into CALLS and answering (values BODY STATUS) from ANSWER, a function of the
method, the url, and the arguments the call carried -- so a test reads the
request it sent."
  (let ((method (gensym "METHOD")) (url (gensym "URL")))
    `(let ((,calls '()))
       (with-stubbed-fdefinition (nodecode-t3mp3st::http (,method ,url &rest arguments)
                                   (setf ,calls (append ,calls (list (cons ,method ,url))))
                                   (funcall ,answer ,method ,url arguments))
         ,@body-forms))))

(defun wr-json (object)
  "OBJECT as the JSON text the War Room answers with."
  (nlk:encode-json-object object))

(defun wr-object (&rest pairs)
  "A decoded JSON object from alternating key/value PAIRS."
  (apply #'nlk:make-json-object pairs))

(defun wr-array (&rest items)
  "A decoded JSON array of ITEMS."
  (coerce items 'vector))

(defun wr-idle-answer (method url arguments)
  "An idle War Room: the shape its status endpoint answers with no command."
  (declare (ignore method url arguments))
  (values (wr-json (wr-object "active" nil "paused" nil "progress" (wr-array) "tasks" (wr-array)))
          200))

(defun wr-answer-with (json &optional (status 200))
  "An answer function that always answers JSON at STATUS."
  (lambda (method url arguments)
    (declare (ignore method url arguments))
    (values (wr-json json) status)))

(defun wr-mission-answer (method url arguments)
  "A War Room running one mission on two targets."
  (declare (ignore method url arguments))
  (values (wr-json (wr-object
                     "active" t
                     "tickCount" 12
                     "mission" (wr-object "id" "m-42" "name" "Nightfall" "status" "running"
                                          "currentPhase" "exploitation" "progress" 0.4)
                     "targets" (wr-array "10.20.0.5" "app.example")
                     "operators" (wr-object "summary" (wr-object "total" 3 "available" 0 "busy" 1))))
          200))

;;; --- the lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "t3mp3st"
  (:help :t3mp3st)
  (:command "t3mp3st")
  (:refused ("timeout_seconds" 0))
  (:idle nodecode-t3mp3st:t3mp3st-error (t3mp3st:status)))

;;; --- what a person gets ----------------------------------------------------------------

(deftest t3mp3st-cell-an-idle-server-is-said-to-be-idle ()
  (with-cell-stop ((t3mp3st-start))
    (with-scripted-war-room (calls :answer #'wr-idle-answer)
      (let ((block (t3mp3st:status)))
        (is (cl:search "T3MP3ST War Room at http://127.0.0.1:3333" block) "the block names the War Room it asked")
        (is (cl:search "no mission: the server is up and idle" block) "and says the server is up with nothing running")
        (is (equal "GET" (car (first calls))) "one GET, to the status endpoint")
        (is (cl:search "/api/mission/status" (cdr (first calls))) "at the status path")
        (is (= 1 (length calls)) "and nothing else was asked")))))

(deftest t3mp3st-cell-a-mission-is-read-as-its-phase-and-progress ()
  (with-cell-stop ((t3mp3st-start))
    (with-scripted-war-room (calls :answer #'wr-mission-answer)
      (let ((block (t3mp3st:status)))
        (is (cl:search "mission: Nightfall (running)" block) "the mission and its state")
        (is (cl:search "phase: exploitation" block) "the phase it is in")
        (is (cl:search "progress: 40%" block) "a 0..1 progress read as a percentage")
        (is (cl:search "ticks: 12" block) "the ticks")
        (is (cl:search "targets: 10.20.0.5, app.example" block) "every target in scope")
        (is (cl:search "operators: 3 total, 0 available, 1 busy" block) "and the operator counts, read from its summary")))))

(deftest t3mp3st-cell-a-stopped-mission-is-not-called-active ()
  ;; Found against the live server: a stopped mission's object keeps status
  ;; 'active' while the server's own flag says otherwise. The flag wins.
  (with-cell-stop ((t3mp3st-start))
    (with-scripted-war-room (calls :answer
                                   (wr-answer-with
                                    (wr-object "active" nil "paused" nil
                                               "mission" (wr-object "id" "m-42" "name" "Nightfall"
                                                                     "status" "active"
                                                                     "currentPhase" "reconnaissance"))))
      (let ((block (t3mp3st:status)))
        (is (cl:search "mission: Nightfall (not running)" block) "the server's active flag is the truth, not the mission object's word")
        (is (cl:search "the War Room's last status for it: active" block) "and what that object still says is quoted, not trusted")))))

(deftest t3mp3st-cell-a-launch-carries-its-targets ()
  (with-cell-stop ((t3mp3st-start))
    (with-scripted-war-room (calls :answer
                                   (lambda (method url arguments)
                                     (declare (ignore url))
                                     (is (equal "POST" method) "a launch is a POST")
                                     (let ((body (or (getf arguments :content) "")))
                                       (is (cl:search "\"targets\"" body) "the body carries targets")
                                       (is (cl:search "10.20.0.5" body) "the first of them")
                                       (is (cl:search "app.example" body) "and the second"))
                                     (values (wr-json (wr-object "missionId" "m-42")) 200)))
      (let ((block (t3mp3st:launch "10.20.0.5, app.example" :name "Nightfall")))
        (is (cl:search "started mission m-42" block) "the mission it started is named")
        (is (cl:search "targets: 10.20.0.5, app.example" block) "with the targets it read")
        (is (cl:search "/api/mission/start" (cdr (first calls))) "at the start path")))))

(deftest t3mp3st-cell-a-launch-with-no-targets-refuses-in-words ()
  (with-cell-stop ((t3mp3st-start))
    (with-scripted-war-room (calls :answer #'wr-idle-answer)
      (let ((refusal (signals-error nodecode-t3mp3st:t3mp3st-error (t3mp3st:launch "  "))))
        (is (typep refusal 'nodecode-t3mp3st:t3mp3st-error) "a launch with nothing to test refuses")
        (is (cl:search "at least one target" (nodecode-t3mp3st::t3mp3st-error-detail refusal)) "saying what it needs")
        (is (null calls) "and asking the War Room nothing")))))

(deftest t3mp3st-cell-a-held-launch-names-the-gate ()
  ;; The shape is the one a live War Room answers with: HTTP 403, the error, and
  ;; the approval the launch is waiting on -- read from the program, not invented.
  (with-cell-stop ((t3mp3st-start))
    (with-scripted-war-room (calls :answer
                                   (wr-answer-with
                                    (wr-object "error" "Approval required before active execution"
                                               "approval" (wr-object "id" "approval_7ed0e24b"
                                                                     "action" "mission_execution"
                                                                     "target" "10.20.0.5"
                                                                     "reason" "Start mission live-check against 10.20.0.5"
                                                                     "status" "pending")
                                               "next" "POST /api/approvals/approval_7ed0e24b/approve")
                                    403))
      (let ((block (t3mp3st:launch "10.20.0.5")))
        (is (cl:search "the mission has NOT started" block) "a gated launch is said to have not started")
        (is (cl:search "approval_7ed0e24b" block) "the gate's id is given")
        (is (cl:search "mission_execution" block) "with what it asks for")
        (is (cl:search "Start mission live-check" block) "and the War Room's own reason")
        (is (cl:search "POST /api/approvals/approval_7ed0e24b/approve" block) "and the step it suggests")
        (is (cl:search "then launch again with approval_id=approval_7ed0e24b" block) "and says a cleared gate starts nothing by itself")))))

(deftest t3mp3st-cell-a-relaunch-carries-the-approval-it-cleared ()
  ;; The live contract: an approval is named by the start it clears, or the War
  ;; Room asks again. A cell that approved and re-launched bare would loop.
  (with-cell-stop ((t3mp3st-start))
    (with-scripted-war-room (calls :answer
                                   (lambda (method url arguments)
                                     (declare (ignore method url))
                                     (is (cl:search "\"approvalId\"" (or (getf arguments :content) "")) "the start names the gate it cleared")
                                     (is (cl:search "approval_7ed0e24b" (or (getf arguments :content) "")) "by that gate's id")
                                     (values (wr-json (wr-object "success" t
                                                                 "missionName" "live-check"
                                                                 "missionId" "m-99"))
                                             200)))
      (let ((block (t3mp3st:launch "10.20.0.5" :name "live-check" :provider "mock"
                                    :approval-id "approval_7ed0e24b")))
        (is (cl:search "started mission m-99" block) "and the mission starts")))))

(deftest t3mp3st-cell-a-control-with-nothing-running-is-a-state ()
  ;; The live War Room answers 404 'No active mission'; that is the run being
  ;; over, not a verb that failed.
  (with-cell-stop ((t3mp3st-start))
    (with-scripted-war-room (calls :answer
                                   (wr-answer-with (wr-object "error" "No active mission") 404))
      (let ((block (t3mp3st:control "stop")))
        (is (cl:search "no mission is running, so there was nothing to stop" block) "a stop with nothing running says so")))))

(deftest t3mp3st-cell-findings-read-the-vault-list ()
  (with-cell-stop ((t3mp3st-start))
    (with-scripted-war-room (calls :answer
                                   (wr-answer-with
                                    (wr-object "findings" (wr-array (wr-object "title" "exposed admin panel"
                                                                                "severity" "high")
                                                                     (wr-object "title" "TLS 1.0 accepted"
                                                                                "severity" "medium"))
                                               "credentials" (wr-array (wr-object "kind" "password")))))
      (let ((block (t3mp3st:findings)))
        (is (cl:search "2 findings in the mission's vault" block) "the count is the real one")
        (is (cl:search "exposed admin panel" block) "each finding is read")
        (is (cl:search "TLS 1.0 accepted" block) "and the next")
        (is (cl:search "1 credential record" block) "with the credentials counted, not shown")))))

(deftest t3mp3st-cell-approvals-list-then-clear-one ()
  (with-cell-stop ((t3mp3st-start))
    (with-scripted-war-room (calls :answer
                                   (lambda (method url arguments)
                                     (declare (ignore arguments))
                                     (if (search "/approve" url)
                                         (values (wr-json (wr-object "id" "a-7" "status" "approved")) 200)
                                         (values (wr-json (wr-object
                                                            "approvals" (wr-array
                                                                         (wr-object "id" "a-7"
                                                                                    "action" "mission_execution"
                                                                                    "target" "10.20.0.5"
                                                                                    "status" "pending"))))
                                                 200))))
      (let ((listed (t3mp3st:approvals)))
        (is (cl:search "1 approval request" listed) "the gate is listed")
        (is (cl:search "a-7  mission_execution -> 10.20.0.5  [pending]" listed) "with its id, what it asks, its target and its state"))
      (let ((cleared (t3mp3st:approvals :action "approve" :id "a-7")))
        (is (cl:search "approve:" cleared) "an approval answers what it did")
        (is (equal "POST" (car (second calls))) "as a POST")
        (is (cl:search "/api/approvals/a-7/approve" (cdr (second calls))) "to that gate's own path")))))

(deftest t3mp3st-cell-a-control-verb-is-one-of-three ()
  (with-cell-stop ((t3mp3st-start))
    (let ((refusal (signals-error nodecode-t3mp3st:t3mp3st-error (t3mp3st:control "detonate"))))
      (is (cl:search "stop, pause, resume" (nodecode-t3mp3st::t3mp3st-error-detail refusal)) "an unknown verb is refused with the three that exist"))))

(deftest t3mp3st-cell-an-unreachable-war-room-says-how-to-start-it ()
  (with-cell-stop ((t3mp3st-start))
    (with-scripted-war-room (calls :answer
                                   (lambda (method url arguments)
                                     (declare (ignore method url arguments))
                                     (error "connection refused")))
      (let ((refusal (signals-error nodecode-t3mp3st:t3mp3st-error (t3mp3st:status))))
        (is (cl:search "no T3MP3ST War Room answers at http://127.0.0.1:3333"
                       (nodecode-t3mp3st::t3mp3st-error-detail refusal)) "the refusal names the address that did not answer")
        (is (cl:search "npm run server" (nodecode-t3mp3st::t3mp3st-error-detail refusal)) "and the command that starts one")))))

(deftest t3mp3st-cell-the-command-answers-status-and-usage ()
  (with-cell-stop ((t3mp3st-start))
    (with-scripted-war-room (calls :answer #'wr-mission-answer)
      (let ((replied (cell-entry "nodecode-t3mp3st" "t3mp3st" "status" "t3mp3st-cell-command")))
        (is (cl:search "mission: Nightfall" replied) "/t3mp3st status reads the mission"))
      (let ((bare (cell-entry "nodecode-t3mp3st" "t3mp3st" "" "t3mp3st-cell-command")))
        (is (cl:search "T3MP3ST War Room at" bare) "typed alone it answers the status"))
      (let ((unknown (cell-entry "nodecode-t3mp3st" "t3mp3st" "detonate" "t3mp3st-cell-command")))
        (is (cl:search "usage: /t3mp3st" unknown) "and an unknown word says how to use it")))))
