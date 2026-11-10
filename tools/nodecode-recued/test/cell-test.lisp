;;;; cell-test.lisp --- the entry it writes, the lines /recued answers, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the wiring: that start puts the manual topic and
;;;; the command on and writes mcp.servers only when it is not there; that
;;;; stop takes every contribution back; that a server the user wrote itself is
;;;; left exactly as it was; that /recued adds, removes and reports, and says
;;;; when the command is not on this computer's PATH; and that a section with a
;;;; member of the wrong shape refuses with nothing installed.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "recued"
  (:fixture with-recued-seams)
  (:help :recued)
  (:command "recued")
  (:running
   (is (string= "recued" (nodecode-recued::setting :server-name)) "the server is named after the command")
   (is (equal '("--mcp") (nodecode-recued::setting :args)) "started the way the plugin declared it")
   (is (= 120 (nodecode-recued::setting :timeout-seconds)) "one call, two minutes by default")
   (is (equal "recued" (getf (recued-entry) :command)) "start wrote mcp.servers.recued")
   (is (equal '("--mcp") (getf (recued-entry) :args)) "with the arguments the plugin gave")
   (is (= 120000 (getf (recued-entry) :timeout_ms)) "and a ceiling the server can read"))
  (:refused ("timeout_seconds" 1))
  (:idle nodecode-recued:recued-error (nodecode-recued::run-slash "status" nil)))

;;; --- the entry ---------------------------------------------------------------------

(deftest recued-cell-a-server-the-user-wrote-is-left-as-it-was (with-recued-seams)
  ;; Someone may have written the entry by hand, or for another client, before
  ;; this folder was added: start must not overwrite it.
  (setf *recued-servers*
        (list (cons "recued" (list :command "recued" :args '("--mcp" "--app" "another")))))
  (with-cell-stop ((recued-start))
    (is (equal '("--mcp" "--app" "another") (getf (recued-entry) :args)) "the entry the user wrote is untouched")
    (is-carrying (text (nodecode-recued::status-line))
                 "mcp.servers.recued is set"
                 "recued --mcp --app another")))

(deftest recued-cell-connect-writes-what-the-plugin-declared (with-recued-seams)
  (with-cell-stop ((recued-start "auto_connect" nil))
    (is (null (recued-entry)) "auto_connect off writes nothing at start")
    (is-carrying (text (nodecode-recued::run-slash "connect" nil))
                 "mcp.servers.recued"
                 "restart-cells")
    (is (equal '("--mcp") (getf (recued-entry) :args)) "the entry the plugin's mcp.json declares")
    (is (= 120000 (getf (recued-entry) :timeout_ms)) "and one call's ceiling")))

(deftest recued-cell-forget-takes-the-entry-out (with-recued-seams)
  (with-cell-stop ((recued-start))
    (is-carrying (text (nodecode-recued::run-slash "forget" nil)) "took mcp.servers.recued out")
    (is (null (recued-entry)) "the member is gone")
    (is-carrying (text (nodecode-recued::status-line)) "not in mcp.servers")
    (is-carrying (text (nodecode-recued::run-slash "forget" nil)) "was not there")))

;;; --- what /recued says -------------------------------------------------------------

(deftest recued-cell-status-names-a-command-this-computer-lacks (with-recued-seams)
  (let ((*recued-on-path-p* nil))
    (with-cell-stop ((recued-start))
      (is-carrying (text (nodecode-recued::status-line))
                   "mcp.servers.recued is set"
                   "recued is not on this computer's PATH"))))

(deftest recued-cell-an-unknown-subcommand-answers-with-the-choices (with-recued-seams)
  (with-cell-stop ((recued-start))
    (is-carrying (text (nodecode-recued::run-slash "nap" nil))
                 "nap is not one of its choices"
                 "forget")))
