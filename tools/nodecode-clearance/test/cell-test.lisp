;;;; cell-test.lisp --- clearance's lifecycle, its tiers, its hook and its command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is this cell's own: which sender a session is taken to
;;;; be, which tools a tier's patterns let through, that a refusal travels the
;;;; path a tool result does and never runs the tool, that the audit log keeps
;;;; argument names and never their values, and that the command answers every
;;;; word of its usage to the person entitled to it.

(in-package #:nodecode.test)

(defmacro with-clearance-turn ((session-id) &body body)
  "Run BODY in a turn on SESSION-ID, the way a room's lane opens one: the
meter NLE:TURN reads, built here rather than opened, so no store is touched."
  `(let ((nle::*live-turn* (nle::%make-live-turn :session-id ,session-id
                                                 :turn-id "t-clearance")))
     ,@body))

(defun clearance-call (session-id tool &key arguments)
  "The (values ANSWER RAN-P) of the installed :TOOL hook for TOOL on
SESSION-ID: the refusal string and NIL, or :RAN and T when the tool ran."
  (with-clearance-turn (session-id)
    (let ((ran nil))
      (values (funcall (cell-hook "nodecode-clearance" :tool)
                       (list :name tool :arguments arguments :call-id "c")
                       (lambda (op) (declare (ignore op)) (setf ran t) :ran))
              ran))))

(defun clearance-tier-name (session-id)
  "The name of the tier SESSION-ID's turns run at, under the running settings."
  (nodecode-clearance::tier-name
   (nodecode-clearance::tier-for (nodecode-clearance::build-policy '())
                                  (nodecode-clearance::identity-of session-id))))

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "clearance"
  (:hooks :tool)
  (:command "clearance")
  (:running (is (string= "guest" (nodecode-clearance::setting :default-tier)) "the section's default tier")
            (is (hash-table-p nodecode-clearance::*assigned*) "and the assignment table is there"))
  (:refused ("default_tier" 42)))

;;; --- who is talking --------------------------------------------------------

(deftest clearance-cell-a-local-session-is-the-owner (with-cell-stop ((clearance-start))
  (multiple-value-bind (answer ran) (clearance-call "main" "eval")
    (is (eq :ran answer) "a session no channel claims is the operator's own")
    (is (eq t ran) "so the tool runs"))))

(deftest clearance-cell-a-room-sender-is-refused (with-cell-stop ((clearance-start))
  (multiple-value-bind (answer ran) (clearance-call "telegram-555-m1" "eval")
    (is (search "refused by clearance" answer) "a room sender at guest is refused eval")
    (is (null ran) "and the tool never runs"))))

(deftest clearance-cell-a-guest-may-still-talk (with-cell-stop ((clearance-start))
  (multiple-value-bind (answer ran) (clearance-call "telegram-555-m2" "todo_list")
    (is (eq :ran answer) "the @chat group is a guest's whole surface")
    (is (eq t ran) "and it runs"))))

(deftest clearance-cell-a-sender-nobody-named-gets-the-default-tier (with-cell-stop ((clearance-start))
  (let ((identity (nodecode-clearance::identity-of "telegram-555-m3")))
    (is (eq :unknown (getf identity :kind)) "a session no lane claims is unknown, never local")
    (is (equal "guest" (clearance-tier-name "telegram-555-m3")) "so it falls to the default tier"))))

;;; --- what a tier may call --------------------------------------------------

(deftest clearance-cell-deny-beats-allow-and-unnamed-is-denied ()
  (let ((tier (nodecode-clearance::tier-from-spec
               "t" '(:allow ("@web" "@shell") :deny ("web_private")))))
    (is (null (nodecode-clearance::decide tier "web_private")) "a deny beats an allow")
    (is (nodecode-clearance::decide tier "web_search") "a group allows the names it spans")
    (is (nodecode-clearance::decide tier "eval") "and a group named beside it")
    (is (null (nodecode-clearance::decide tier "weather-unknown")) "an unnamed tool is denied")))

(deftest clearance-cell-the-hourly-limit-holds ()
  (let ((nodecode-clearance::*hits* (make-hash-table :test #'equal)))
    (is (nodecode-clearance::rate-check "telegram:9" 1) "the first turn of the hour passes")
    (is (null (nodecode-clearance::rate-check "telegram:9" 1)) "the second is over the limit")
    (is (nodecode-clearance::rate-check "telegram:8" 1) "another sender has its own count")))

;;; --- the command -----------------------------------------------------------

(deftest clearance-cell-the-owner-assigns-a-sender (with-cell-stop ((clearance-start))
  (let ((answer (nodecode-clearance::run-clearance "set telegram:123 admin" "main")))
    (is (search "is now admin" answer) "the owner assigns a sender"))
  (let ((tier (nodecode-clearance::tier-for
               (nodecode-clearance::build-policy '())
               (nodecode-clearance::make-identity "telegram" "123"))))
    (is (equal "admin" (nodecode-clearance::tier-name tier)) "and that sender's tier follows"))
  (let ((answer (nodecode-clearance::run-clearance "users" "main")))
    (is (search "telegram:123 -> admin" answer) "and users lists it"))))

(deftest clearance-cell-only-the-owner-assigns (with-cell-stop ((clearance-start))
  (let ((answer (nodecode-clearance::run-clearance "set telegram:123 admin" "telegram-555-m4")))
    (is (search "Only the owner" answer) "a guest cannot promote itself"))
  (is (null (gethash "telegram:123" nodecode-clearance::*assigned*)) "and nothing was written")))

(deftest clearance-cell-the-command-answers-what-you-are (with-cell-stop ((clearance-start))
  (let ((guest (nodecode-clearance::run-clearance "me" "telegram-555-m5"))
        (owner (nodecode-clearance::run-clearance "me" "main"))
        (tiers (nodecode-clearance::run-clearance "tiers" "main"))
        (other (nodecode-clearance::run-clearance "nonsense" "main")))
    (is (search "Tier: guest" guest) "me names the tier")
    (is (search "Tier: owner" owner) "an operator session is the owner")
    (is (search "guest: @chat" tiers) "tiers lists what a tier may call")
    (is (search "Usage:" other) "and anything else is the usage"))))

;;; --- the audit log ---------------------------------------------------------

(deftest clearance-cell-the-audit-keeps-names-not-values (with-cell-stop ((clearance-start))
  (clearance-call "telegram-555-m6" "eval"
                  :arguments (nlk:json-object "form" "cat ~/.ssh/id_rsa"))
  (let ((log (nlk:read-text (nodecode-clearance::audit-path))))
    (is (search "tool_denied" log) "a refusal is recorded")
    (is (search "arg_keys" log) "with the names of its arguments")
    (is (null (search "id_rsa" log)) "and never their values"))))
