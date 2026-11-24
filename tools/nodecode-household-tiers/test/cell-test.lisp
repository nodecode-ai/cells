;;;; cell-test.lisp --- the household-tiers cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here touches a real session, a real clock or the operator's home:
;;;; the turn's session and the clock are stubbed, the home is a scratch
;;;; folder holding the policy document, and the tool dispatch is a function
;;;; that records whether it ran.

(in-package #:nodecode.test)

(define-test-slice "household-tiers" "HOUSEHOLD-TIERS-CELL-"
  :start nodecode-household-tiers:start-cell)

(define-cell-lifecycle-tests "household-tiers"
  (:hooks :tool)
  (:command "household-tiers")
  (:help :household-tiers)
  (:refused ("default_tier" "mayor")))

;;; --- the fixture ------------------------------------------------------------------------

(defparameter +ht-policy+
  "{\"sessions\": {\"s-owner\": \"owner\", \"s-kid\": \"kid\", \"s-guest\": \"guest\", \"chan-*\": \"user\"},
    \"hours\": {\"kid\": {\"allow\": \"08:00-21:00\"}},
    \"tiers\": {\"user\": [\"web_search\"]}}"
  "A household: an owner, a child with hours, a guest, and a lane prefix.")

(defmacro with-household ((session &key policy (now 720) config) &body body)
  "Run BODY with the cell started in a scratch home whose policy is POLICY
(+HT-POLICY+ when it is not given), the turn's session SESSION, the clock at
NOW minutes past midnight, and the section built from CONFIG when it is given."
  (let ((policy-form (if policy policy '+ht-policy+)))
    `(let* ((nlk:*home* (uiop:ensure-directory-pathname
                         (merge-pathnames (format nil "ht-~(~a~)/" (gensym "RUN"))
                                          (uiop:temporary-directory))))
            (document ,policy-form))
       (ensure-directories-exist (nlk:home "keep"))
       (when document (nlk:write-file-atomically (nlk:home "household-tiers.json") document))
       (with-stubbed-fdefinitions
           ((nodecode-household-tiers::current-session () ,session)
            (nodecode-household-tiers::now-minutes () ,now))
         (with-cell-stop ((apply #'household-tiers-start ',config))
           ,@body)))))

(defun ht-verdict (tool)
  "The installed :TOOL hook's answer for TOOL: :ALLOWED, or the veto text."
  (funcall (cell-hook "nodecode-household-tiers" :tool)
           (list :name tool :arguments (nlk:json-object) :call-id "c1")
           (lambda (op) (declare (ignore op)) :allowed)))

(defun ht-ledger ()
  "The audit ledger's lines, oldest first."
  (nlk:read-json-lines (nodecode-household-tiers::audit-path)))

;;; --- the tier decides the tool --------------------------------------------------------

(deftest household-tiers-cell-owner-calls-any-tool (with-household ("s-owner")
  (is (eq :allowed (ht-verdict "eval")) "the owner's tier is `*'")
  (is (eq :allowed (ht-verdict "web_search")) "and every other tool")))

(deftest household-tiers-cell-guest-is-refused-and-written-down (with-household ("s-guest")
  (let* ((before (length (ht-ledger)))
         (veto (ht-verdict "eval"))
         (ledger (ht-ledger))
         (line (car (last ledger))))
    (is (stringp veto) "a guest may not call eval")
    (is (search "s-guest (guest) may not use eval" veto) veto)
    (is (search "ERROR: " veto) "the veto reads as the call's failure")
    (is (> (length ledger) before) "the refusal is written down")
    (is (equal "s-guest" (nlk:json-value line :any "who")) "naming who")
    (is (equal "eval" (nlk:json-value line :any "tool")) "and which tool")
    (is (equal "block" (nlk:json-value line :any "decision")) "as a block"))))

(deftest household-tiers-cell-child-keeps-its-tools-inside-hours (with-household ("s-kid")
  (is (eq :allowed (ht-verdict "web_search")) "the child's own tool, at 12:00")
  (is (stringp (ht-verdict "eval")) "a tool outside the tier, at 12:00")))

(deftest household-tiers-cell-quiet-hours-refuse-every-tool (with-household ("s-kid" :now 1350)
  (let ((veto (ht-verdict "web_search")))
    (is (stringp veto) "22:30 is outside 08:00-21:00")
    (is (search "outside their hours" veto) veto)
    (is (search "08:00-21:00" veto) "the window is named back")))
  (is (equal "curfew" (nlk:json-value (first (ht-ledger)) :any "decision")) "the ledger says why"))

(deftest household-tiers-cell-a-prefix-names-a-lane (with-household ("chan-42")
  (is (eq :allowed (ht-verdict "web_search")) "chan-* is a `user', which names web_search")
  (is (stringp (ht-verdict "eval")) "and only that")))

(deftest household-tiers-cell-unlisted-session-falls-to-the-default (with-household ("nobody" :policy "{}")
  (is (stringp (ht-verdict "eval")) "the section's default tier is guest, which has no tools")))

(deftest household-tiers-cell-a-broken-policy-fails-closed (with-household ("s-owner" :policy "{")
  (let ((veto (ht-verdict "eval")))
    (is (stringp veto) "an unreadable document refuses the owner too")
    (is (search "could not be read" veto) veto))))

(deftest household-tiers-cell-a-silent-document-writes-no-ledger
    (with-household ("s-guest" :policy "{\"audit\": {\"enabled\": false}}")
  (is (stringp (ht-verdict "eval")) "the call is still refused")
  (is (notany (lambda (line) (equal "block" (nlk:json-value line :any "decision"))) (ht-ledger)) "and the document's audit switch writes no block")))

;;; --- the summary, the command -------------------------------------------------------------

(deftest household-tiers-cell-summary-is-the-owners (with-household ("s-guest")
  (let ((answer (nodecode-household-tiers:summary)))
    (is (search "Only the owner" answer) answer)))
  (with-stubbed-fdefinitions ((nodecode-household-tiers::current-session () "s-owner"))
    (let ((answer (nodecode-household-tiers:summary)))
      (is (search "Household summary (7 days)" answer) "the owner reads the summary")
      (is (search "Household summary (7 days)" answer) "under the window it was asked for"))))

(deftest household-tiers-cell-command-changes-a-tier-and-forgets (with-household ("s-guest" :now 720)
  (is (stringp (ht-verdict "eval")) "a guest cannot call eval yet")
  (let ((answer (nodecode-household-tiers::run-command "tier s-guest user" "s-owner")))
    (is (search "Done: s-guest is now user" answer) answer))
  (is (equal "user" (nodecode-household-tiers::session-tier
                      "s-guest" (nodecode-household-tiers::policy-document))) "and the document it saved says so")
  (is (eq :allowed (ht-verdict "web_search")) "the next call reads the new tier")
  (let ((summary (nodecode-household-tiers::run-command "" "s-owner")))
    (is (search "s-guest: blocked eval x" summary) summary))
  (let ((receipt (nodecode-household-tiers::run-command "forget s-guest" "s-owner")))
    (is (search "audit entries for s-guest" receipt) receipt))
  (let ((left (ht-ledger)))
    (is (notany (lambda (line) (equal "s-guest" (nlk:json-value line :any "who"))) left) "nothing naming that person is left")
    (is (equal "forget" (nlk:json-value (car (last left)) :any "decision")) "which says what was done"))))

(deftest household-tiers-cell-command-refuses-a-stranger (with-household ("nobody")
  (is (search "Only the owner" (nodecode-household-tiers::run-command "tier s-kid user" "s-guest")) "a guest changes no tier")))

(deftest household-tiers-cell-tool-answers-the-owner (with-household ("s-owner")
  (is (search "Household summary" (nodecode-household-tiers::household-summary (nlk:json-object "days" 2))) "the tool answers the owner's summary for the days it was given")))
