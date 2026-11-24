;;;; cell-test.lisp --- the agentplaybooks cell against its own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every apb call is stubbed: RUN-CLI answers a scripted text and every call
;;;; is recorded, so nothing spawns a process, reads a project or reaches
;;;; agentplaybooks.ai.

(in-package #:nodecode.test)

(define-test-slice "agentplaybooks" "AGENTPLAYBOOKS-CELL-"
  :start nodecode-agentplaybooks:start-cell)

(defparameter +playbook+ "3f6a1c2e-0000-4000-8000-00000000cafe"
  "The guid every test names, standing in for the operator's own.")

(defvar *calls* '()
  "Every (ARGV DIRECTORY SECONDS) a stubbed RUN-CLI was handed this test, newest first.")

(defmacro with-cli ((answer) &body body)
  "BODY with RUN-CLI stubbed: ANSWER, a form with ARGV bound, answers
 (values OUT ERR STATUS) and every call is recorded in *CALLS*."
  `(let ((*calls* '()))
     (with-stubbed-fdefinition (nodecode-agentplaybooks::run-cli (argv &key directory seconds)
                                  (push (list argv directory seconds) *calls*)
                                  ,answer)
       ,@body)))

(defmacro with-apb-cli (&body body)
  "BODY with the version probe and every other call answered: the fixture that
 lets the cell start without the CLI on the machine."
  `(with-cli ((if (member "--version" argv :test #'equal)
                  (values (format nil "~a 0.7.0" (first argv)) "" 0)
                  (values "plan: 2 files, 1 profile member" "" 0)))
     ,@body))

(defun argv-is (&rest expected)
  "Whether the newest stubbed call was made with EXPECTED, in order."
  (equal expected (first (first *calls*))))

(defun doctor-op (&rest pairs)
  "A DOCTOR-QUERY from PAIRS."
  (nodecode-agentplaybooks:decode-doctor-query (apply #'nlk:make-json-object pairs)))

(defun setup-op (&rest pairs)
  "A SETUP-QUERY from PAIRS."
  (nodecode-agentplaybooks:decode-setup-query (apply #'nlk:make-json-object pairs)))

(defun connect-op (&rest pairs)
  "A CONNECT-QUERY from PAIRS."
  (nodecode-agentplaybooks:decode-connect-query (apply #'nlk:make-json-object pairs)))

(defun project-op (&rest pairs)
  "A PROJECT-QUERY from PAIRS."
  (nodecode-agentplaybooks:decode-project-query (apply #'nlk:make-json-object pairs)))

;;; --- the cell's own life ---------------------------------------------------

(define-cell-lifecycle-tests "agentplaybooks"
  (:fixture with-apb-cli)
  (:config "playbook" "3f6a1c2e-0000-4000-8000-00000000cafe")
  (:help :agentplaybooks)
  (:command "apb")
  (:running
   (is (equal "apb" (nodecode-agentplaybooks::setting :binary)) "the default CLI")
   (is (equal "hermes" (nodecode-agentplaybooks::setting :target)) "the documented target")
   (is (equal 180 (nodecode-agentplaybooks::setting :timeout)) "the default bound"))
  (:refused ("playbook" "") ("timeout" 1)))

(deftest agentplaybooks-cell-a-section-without-a-playbook-is-a-refusal ()
  ;; The section is present and enabled but names no playbook: the one
  ;; setting the tools cannot invent.
  (let ((section (nlk:json-object "agentplaybooks" (nlk:make-json-object "enabled" t))))
    (is (signals-error nlk:config-refusal (nodecode-agentplaybooks:start-cell section)) "refused"))
  (is (null nodecode-agentplaybooks::*agentplaybooks*) "and leaves no settings behind"))

;;; --- apb_doctor ------------------------------------------------------------

(deftest agentplaybooks-cell-doctor-probes-then-checks ()
  (with-cli ((if (member "--version" argv :test #'equal)
                  (values "apb 0.7.0" "" 0)
                  (values "ok: node 20, writable, key present" "" 0)))
    (with-cell-stop ((agentplaybooks-start "playbook" +playbook+))
      (let ((answer (nodecode-agentplaybooks:apb-doctor (doctor-op))))
        (is (search "apb 0.7.0" answer) "the CLI's version is answered first")
        (is (search "writable" answer) "and the preflight's own words"))
      (is (argv-is "apb" "hermes" "doctor" +playbook+ ".") "the playbook and the project")
      (nodecode-agentplaybooks:apb-doctor (doctor-op "check" t "playbook" "other-guid"))
      (is (argv-is "apb" "hermes" "doctor" "other-guid" "." "--check") "check comes last"))))

(deftest agentplaybooks-cell-a-missing-cli-is-the-prerequisite ()
  (with-cli ((error "run-program: apb: No such file or directory"))
    (with-cell-stop ((agentplaybooks-start "playbook" +playbook+))
      (let ((answer (nodecode-agentplaybooks:apb-doctor (doctor-op))))
        (is (search "could not be started" answer) "the tool says so, quietly")
        (is (search "@agentplaybooks/cli" answer) "and names the package to install")))))

;;; --- apb_setup -------------------------------------------------------------

(deftest agentplaybooks-cell-setup-previews-then-applies ()
  (with-apb-cli
    (with-cell-stop ((agentplaybooks-start "playbook" +playbook+ "profile" "work"))
      (let ((preview (nodecode-agentplaybooks:apb-setup (setup-op))))
        (is (search "plan: 2 files" preview) "the plan the CLI printed")
        (is (search "preview only" preview) "and that it wrote nothing"))
      (is (argv-is "apb" "hermes" "setup" +playbook+ "." "--profile=work") "the selection rides")
      (nodecode-agentplaybooks:apb-setup (setup-op "tools_only" t "apply" t))
      (is (argv-is "apb" "hermes" "setup" +playbook+ "." "--profile=work" "--tools-only" "--apply") "apply is last")
      (let ((applied (nodecode-agentplaybooks:apb-setup (setup-op "apply" t))))
        (is (null (search "preview only" applied)) "an applied call says no such thing")))))

;;; --- apb_connect -----------------------------------------------------------

(deftest agentplaybooks-cell-connect-is-scoped-or-account-wide ()
  (with-apb-cli
    (with-cell-stop ((agentplaybooks-start "playbook" +playbook+))
      (nodecode-agentplaybooks:apb-connect (connect-op))
      (is (argv-is "apb" "connect" +playbook+ "--target=hermes") "one playbook, one target")
      (nodecode-agentplaybooks:apb-connect (connect-op "account" t "apply" t))
      (is (argv-is "apb" "connect" "--account" "--target=hermes" "--apply") "account-wide"))))

;;; --- apb_project -----------------------------------------------------------

(deftest agentplaybooks-cell-project-pulls-syncs-and-pushes ()
  (with-apb-cli
    (with-cell-stop ((agentplaybooks-start "playbook" +playbook+))
      (nodecode-agentplaybooks:apb-project (project-op "command" "pull"))
      (is (argv-is "apb" "pull" +playbook+ ".") "pull takes the playbook and the project")
      (nodecode-agentplaybooks:apb-project (project-op "command" "sync" "apply" t))
      (is (argv-is "apb" "sync" "." "--target=hermes" "--apply") "sync registers into the target")
      (nodecode-agentplaybooks:apb-project (project-op "command" "push"))
      (is (argv-is "apb" "push" ".") "push uploads the project, never its secrets"))))

(deftest agentplaybooks-cell-a-command-outside-the-set-refuses ()
  (let ((decoded (lambda (&rest pairs)
                   (nodecode-agentplaybooks:decode-project-query
                    (apply #'nlk:make-json-object pairs)))))
    (is (signals-error nlk:contract-refusal (funcall decoded "command" "install")) "three verbs")
    (is (signals-error nlk:contract-refusal (funcall decoded)) "and needs the command")))

;;; --- /apb and the bound ----------------------------------------------------

(deftest agentplaybooks-cell-command-probes-then-runs-apb ()
  (with-apb-cli
    (with-cell-stop ((agentplaybooks-start "playbook" +playbook+))
      (let ((status (nodecode-agentplaybooks::run-command "" "s1")))
        (is (search "apb: apb 0.7.0" status) "the CLI's version")
        (is (search "apb_doctor" status) "and the tools' names"))
      (let ((answer (nodecode-agentplaybooks::run-command "login" "s1")))
        (is (search "plan: 2 files" answer) "typed arguments go to the CLI itself"))
      (is (argv-is "apb" "login") "one argv, no shell in between"))))

(deftest agentplaybooks-cell-a-failing-cli-keeps-its-status ()
  (with-cli ((values "" "playbook key rejected" 2))
    (with-cell-stop ((agentplaybooks-start "playbook" +playbook+))
      (let ((answer (nodecode-agentplaybooks:apb-setup (setup-op "apply" t))))
        (is (search "exited 2" answer) "the status is the verdict")
        (is (search "playbook key rejected" answer) "and stderr is not swallowed")))))

(deftest agentplaybooks-cell-a-loud-plan-is-cut ()
  (with-cli ((values (make-string 4000 :initial-element #\x) "" 0))
    (with-cell-stop ((agentplaybooks-start "playbook" +playbook+ "max_chars" 1000))
      (let ((answer (nodecode-agentplaybooks:apb-connect (connect-op "apply" t))))
        (is (search "[cut at 1000 of 4000 characters]" answer) "the cut says where")))))
