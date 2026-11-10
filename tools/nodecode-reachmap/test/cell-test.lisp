;;;; cell-test.lisp --- lifecycle, the tool, the command, the prompt section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over stubbed probes: that
;;;; a start installs the tool, /reachmap, the manual topic and the frame
;;;; hook, and a stop takes every one of them back; that a section naming a
;;;; scanner or a tool it does not have refuses; that the tool maps a browser
;;;; whose DevTools port answers, a service whose port accepts, a CLI on
;;;; PATH, a listed MCP server and a mounted drive; that find answers a
;;;; name, that a find with nothing to look for refuses, that check catches
;;;; a denial the map contradicts; and that a committed turn leaves the
;;;; compact map in the session's harness block. Nothing leaves the image:
;;;; every exchange, port, lookup, sign-in probe, mount and config read goes
;;;; through a seam this file stubs.

(in-package #:nodecode.test)

(define-test-slice "reachmap" "REACHMAP-CELL-" :start nodecode-reachmap:start-cell)

;;; --- the seams --------------------------------------------------------------

(defvar *asked* ()
  "Every url the stubbed exchange was asked for, newest first.")

(defvar *answers* ()
  "(URL-SUBSTRING STATUS BODY) rows; the newest row whose substring the url
holds answers it, and a url no row holds answers 404.")

(defvar *open* ()
  "The ports the stubbed probe says accept a connection.")

(defvar *installed* ()
  "ALIST of binary -> path: what the stubbed PATH lookup finds.")

(defvar *probe* '("" 0)
  "(OUTPUT EXIT-CODE) the stubbed sign-in probe answers.")

(defun stub-http (method url &rest args)
  (declare (ignore method args))
  (push url *asked*)
  (let ((row (find-if (lambda (row) (search (first row) url)) *answers* :from-end t)))
    (if row
        (values (third row) (second row) '())
        (values "" 404 '()))))

(defun stub-open-p (host port &key timeout)
  (declare (ignore host timeout))
  (and (member port *open* :test #'=) t))

(defun stub-which (name)
  (cdr (assoc name *installed* :test #'equal)))

(defun stub-probe (path arguments seconds)
  (declare (ignore path arguments seconds))
  (values (first *probe*) (second *probe*)))

(defun stub-mounts ()
  '(("Archive" "/Volumes/Archive")))

(defun stub-config-get (path)
  (declare (ignore path))
  '("linear" "sentry"))

(defun answer (substring status body)
  (push (list substring status body) *answers*))

(defun a-browser (&optional (port 9222))
  "A Chrome answering on PORT with one signed-in Gmail tab."
  (answer (format nil ":~d/json/version" port) 200
          "{\"Browser\":\"Chrome/134.0.6998.89\"}")
  (answer (format nil ":~d/json/list" port) 200
          "[{\"type\":\"page\",\"url\":\"https://mail.google.com/mail/u/0/\",\"title\":\"Inbox - operator@example.test - Gmail\"}]"))

(defmacro with-reachmap (&body body)
  "Run BODY with every seam stubbed and every stub's record empty."
  `(let ((nodecode-reachmap::*http* #'stub-http)
         (nodecode-reachmap::*port-open-p* #'stub-open-p)
         (nodecode-reachmap::*which* #'stub-which)
         (nodecode-reachmap::*probe-runner* #'stub-probe)
         (nodecode-reachmap::*mounts* #'stub-mounts)
         (nodecode-reachmap::*config-get* #'stub-config-get)
         (*asked* ()) (*answers* ()) (*open* ()) (*installed* ()) (*probe* '("" 0)))
     ,@body))

;;; --- lifecycle --------------------------------------------------------------

(define-cell-lifecycle-tests "reachmap"
  (:fixture with-reachmap)
  (:config "cache_seconds" 0)
  (:hooks :frame)
  (:help :reachmap)
  (:command "reachmap")
  (:running (is (nle::find-tool "reachmap") "the tool is registered")
            (is (search "REACHMAP" (nodecode-reachmap:report)) "and answers the map"))
  (:refused ("auth_probes" #("gh" "nosuchtool")))
  (:stopped (is (null (nle::find-tool "reachmap")) "stop unregisters the tool")
            (is (null nodecode-reachmap::*snapshot*) "and forgets the last scan")))

;;; --- the tool ---------------------------------------------------------------

(deftest reachmap-cell-the-tool-maps-what-is-live ()
  (with-reachmap
    (setf *open* '(11434)
          *installed* '(("gh" . "/opt/bin/gh")))
    (a-browser 9222)
    (with-cell-stop ((reachmap-start "cache_seconds" 0))
      (let ((text (nle::message-content
                   (execute-wire-call "u1" "reachmap" "{\"action\":\"show\"}"))))
        (is (search "Chrome on CDP :9222" text) "the browser answers on its DevTools port")
        (is (search "Gmail" text) "the site a tab holds is named")
        (is (search "signed" text) "and reads as signed in")
        (is (search "operator@example.test" text) "with who it is signed in as")
        (is (search "Ollama on :11434" text) "the service whose port accepts is named")
        (is (search "GitHub CLI (gh)" text) "the CLI on PATH is named")
        (is (search "disk Archive" text) "the mounted drive is named")
        (is (search "MCP server linear" text) "the listed MCP server is named")))))

(deftest reachmap-cell-find-answers-what-a-name-reaches ()
  (with-reachmap
    (a-browser 9222)
    (with-cell-stop ((reachmap-start "cache_seconds" 0))
      (let ((text (nle::message-content
                   (execute-wire-call "u2" "reachmap" "{\"action\":\"find\",\"query\":\"gmail\"}"))))
        (is (search "live hit" text) "the answer counts the hits")
        (is (search "Gmail" text) "and names the thing that reaches it")))))

(deftest reachmap-cell-a-find-with-nothing-to-look-for-refuses ()
  (with-reachmap
    (with-cell-stop ((reachmap-start "cache_seconds" 0))
      (let ((text (nle::message-content
                   (execute-wire-call "u3" "reachmap" "{\"action\":\"find\"}"))))
        (is (search "ERROR: REACHMAP-ERROR" text) "the answer says it failed")
        (is (search "needs something to look for" text) "and says what find wants")))))

(deftest reachmap-cell-a-call-with-no-action-is-refused ()
  (with-reachmap
    (with-cell-stop ((reachmap-start "cache_seconds" 0))
      (let ((text (string-downcase
                   (nle::message-content (execute-wire-call "u4" "reachmap" "{}")))))
        (is (search "action" text) "the refusal names the member it wanted")))))

(deftest reachmap-cell-check-catches-a-denial-the-map-contradicts ()
  (with-reachmap
    (a-browser 9222)
    (with-cell-stop ((reachmap-start "cache_seconds" 0))
      (let ((text (nle::message-content
                   (execute-wire-call "u5" "reachmap"
                                      "{\"action\":\"check\",\"text\":\"I don't have access to Gmail.\"}"))))
        (is (search "reads as a denial" text) "the denial is named")
        (is (search "Gmail" text) "and the reach that contradicts it is named")))))

(deftest reachmap-cell-a-scan-is-reused-until-it-is-asked-again ()
  (with-reachmap
    (setf *open* '(11434))
    (with-cell-stop ((reachmap-start "cache_seconds" 600))
      (nodecode-reachmap:report)
      (let ((first (length *asked*)))
        (nodecode-reachmap:report)
        (is (= first (length *asked*)) "a second answer inside the cache dials nothing")
        (nodecode-reachmap:report :refresh t)
        (is (> (length *asked*) first) "and refresh scans again")))))

;;; --- the map in the prompt ---------------------------------------------------

(deftest reachmap-cell-a-committed-turn-leaves-the-map-in-the-prompt ()
  (with-reachmap
    (a-browser 9222)
    (with-cell-stop ((reachmap-start "cache_seconds" 0))
      (let ((op (cell-frame-op "turn.input_committed")))
        (nodecode-reachmap::observe-frame op)
        (let ((block (or (and (nlk:store-open-p) (nlk:get-harness-section "s1" "reachmap")) "")))
          (is (or (not (nlk:store-open-p)) (search "Gmail" block)) "the harness block carries the map"))
        (is (search "reachmap tool" (or (nodecode-reachmap::compact-map) "")) "the block says to check")))))
