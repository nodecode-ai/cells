;;;; cell-test.lisp --- a constraint pinned, delivered, logged, dropped.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the cell starts and stops cleanly, its three tools
;;;; and its command answer, a constraint reaches a request's standing section
;;;; verbatim and its delivery is logged with the block's hash, a constraint
;;;; outlives a restart, an unset store_file refuses, and a setting outside
;;;; its range refuses at start. The store is a file in the scratch home, so no
;;;; network is touched at all.

(in-package #:nodecode.test)

(define-test-slice "constraint-vault" "CONSTRAINT-VAULT-CELL-" :start nodecode-constraint-vault:start-cell)

;;; --- helpers -------------------------------------------------------------------

(defvar *cv-calls* 0
  "How many tool calls this test file made, for call ids.")

(defun cv-store ()
  "The store file this test's scratch home holds."
  (namestring (nlk:home "constraint-vault/vault.json")))

(defmacro with-cv ((&rest pairs) &body body)
  "BODY with a session store open and the cell started on its defaults, PAIRS
overriding the section."
  `(with-session-store ("constraint-vault-test")
     (with-cell-stop ((constraint-vault-start "store_file" (cv-store) ,@pairs))
       ,@body)))

(defun cv-json (&rest pairs)
  "PAIRS as the JSON body a tool call carries."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun cv-tool (name &rest arguments)
  "The text the tool called NAME answers, called as the provider's call is."
  (princ-to-string
   (nle::message-content
    (execute-wire-call (format nil "cv-call-~d" (incf *cv-calls*)) name
                        (if arguments (apply #'cv-json arguments) "{}")))))

(defun cv-section (session)
  "The vault's standing section for SESSION, or NIL."
  (cell-section session "constraint-vault"))

(defun cv-log ()
  "The delivery log's pathname beside this test's store."
  (nodecode-constraint-vault::log-pathname (cv-store)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "constraint-vault"
  (:config "store_file" (cv-store))
  (:help :constraints)
  (:command "constraints")
  (:running (is (string= (cv-store) (namestring (nodecode-constraint-vault::store-pathname))) "the section's file is where the constraints go")
            (is (= 50 (nodecode-constraint-vault::setting :max-constraints)) "fifty by default")
            (is (= 2000 (nodecode-constraint-vault::setting :max-chars)) "two thousand characters each")
            (is (nodecode-constraint-vault::setting :inject) "the block is delivered by default")
            (is (nodecode-constraint-vault::setting :log-injections) "and each delivery is logged by default"))
  (:refused ("max_constraints" 0) ("max_chars" 0))
  (:idle nodecode-constraint-vault::constraint-vault-error (nodecode-constraint-vault::listing)))

(deftest constraint-vault-cell-the-hook-installs-and-comes-off ()
  (with-session-store ("constraint-vault-hook")
    (let ((stop (constraint-vault-start "store_file" (cv-store))))
      (unwind-protect
           (progn
             (is (functionp (cell-hook "nodecode-constraint-vault" 'nle::read-harness-sections)) "the request-build hook is installed")
             (is (null (nodecode-constraint-vault::constrain-sections (lambda (id) (declare (ignore id)) '()) "cv-empty")) "an empty vault adds no section"))
        (funcall stop))
      (is (null (cell-hook "nodecode-constraint-vault" 'nle::read-harness-sections)) "stop unhooks it"))))

;;; --- the tools and the command ---------------------------------------------------

(deftest constraint-vault-cell-pin-list-drop ()
  (with-cv ()
    (is-carrying (pinned (cv-tool "constraint_pin" "text" "never spend more than 0.05 ETH per swap"))
                 "Pinned #1" ("never spend more than 0.05 ETH per swap"))
    (is-carrying (all (cv-tool "constraint_list")) ("1 pinned constraint") ("0.05 ETH"))
    (is-carrying (filtered (cv-tool "constraint_list" "text" "0.05")) ("0.05 ETH"))
    (is-carrying (none (cv-tool "constraint_list" "text" "banana")) ("constraint vault is empty"))
    (is-carrying (second-one (nodecode-constraint-vault::run-slash "add reply in French" nil))
                 "Pinned #2" ("reply in French"))
    (is-carrying (dropped (nodecode-constraint-vault::run-slash "drop 1" nil)) "Dropped #1" ("0.05 ETH"))
    (let ((listed (nodecode-constraint-vault::listing)))
      (is (not (search "0.05 ETH" listed)) "the dropped one is gone")
      (is-carrying listed ("reply in French") ("1 pinned constraint")))))

(deftest constraint-vault-cell-refusals-name-what-was-wrong ()
  (with-cv ("max_constraints" 2)
    (cv-tool "constraint_pin" "text" "one rule")
    (is-carrying (again (cv-tool "constraint_pin" "text" "one rule")) "Already pinned as #1" ("one rule"))
    (cv-tool "constraint_pin" "text" "two rule")
    (is-carrying (full (cv-tool "constraint_pin" "text" "three rule")) "ERROR" ("vault is full"))
    (is-carrying (missing (cv-tool "constraint_drop" "id" 99)) "ERROR" ("no constraint with id 99"))
    (is-carrying (blank (cv-tool "constraint_pin" "text" "   ")) "ERROR" ("needs text"))))

(deftest constraint-vault-cell-an-overlong-constraint-refuses ()
  (with-cv ("max_chars" 20)
    (is-carrying (long (cv-tool "constraint_pin" "text" "a rule far too long for twenty characters"))
                 "ERROR" ("too long"))
    (is (= 0 (length (nodecode-constraint-vault::read-constraints))) "and nothing was pinned")))

(deftest constraint-vault-cell-without-a-store-file-the-tool-names-it ()
  (with-session-store ("constraint-vault-nofile")
    (with-cell-stop ((constraint-vault-start))
      (is (nle::find-tool "constraint_pin") "the tool is there")
      (is-carrying (answer (cv-tool "constraint_pin" "text" "always be terse"))
                   "ERROR" ("constraint-vault.store_file is not set"))
      (let ((said (handler-case (nodecode-constraint-vault::run-slash "" nil)
                    (nodecode-constraint-vault::constraint-vault-error (condition)
                      (princ-to-string condition)))))
        (is-carrying said "constraint-vault.store_file is not set")))))

;;; --- every request ---------------------------------------------------------------

(deftest constraint-vault-cell-a-constraint-rides-every-request ()
  (with-cv ()
    (cv-tool "constraint_pin" "text" "never touch the main branch")
    (let ((section (cv-section "cv-request")))
      (is (search "[Constraint vault" section) "the section opens on the marker")
      (is (search "1. never touch the main branch" section) "the operator's words, verbatim, with their id"))
    (let ((prompt (nle::harness-overlay-prompt "" (nle::read-harness-sections "cv-request"))))
      (is (search "never touch the main branch" prompt) "the block reaches the prompt a request is built from"))
    (let ((sections (nle::read-harness-sections "cv-request")))
      (is (= 1 (count "constraint-vault" sections :key #'car :test #'equal)) "one section, read twice"))))

(deftest constraint-vault-cell-each-delivery-is-logged-with-its-hash ()
  (with-cv ()
    (cv-tool "constraint_pin" "text" "max 0.05 ETH")
    (let* ((section (cv-section "cv-log"))
           (lines (nlk:read-json-lines (cv-log)))
           (last (car (last lines))))
      (is (plusp (length lines)) "one line per delivery")
      (is (equal (nlk:sha256-text section) (nlk:json-value last :string "block_sha256")) "the logged hash is the hash of the block that went out")
      (is (equal "harness" (nlk:json-value last :string "via")))
      (is (= 1 (nlk:json-value last :integer "constraints"))))))

(deftest constraint-vault-cell-the-log-and-the-delivery-can-be-turned-off ()
  (with-cv ("log_injections" nil)
    (when (probe-file (cv-log)) (delete-file (cv-log)))
    (cv-tool "constraint_pin" "text" "answer in British English")
    (is (search "answer in British English" (cv-section "cv-nolog")) "still delivered")
    (is (null (probe-file (cv-log))) "and nothing logged"))
  (with-cv ("inject" nil)
    (cv-tool "constraint_pin" "text" "answer in British English")
    (is (null (cv-section "cv-noinject")) "not delivered")))

(deftest constraint-vault-cell-a-constraint-outlives-a-restart ()
  (with-session-store ("constraint-vault-restart")
    (let ((stop (constraint-vault-start "store_file" (cv-store))))
      (unwind-protect
           (cv-tool "constraint_pin" "text" "answer in British English")
        (funcall stop)))
    (with-cell-stop ((constraint-vault-start "store_file" (cv-store)))
      (is-carrying (listed (nodecode-constraint-vault::listing)) ("answer in British English"))
      (is-carrying (next (cv-tool "constraint_pin" "text" "a second rule")) "Pinned #2" ("a second rule")))))
