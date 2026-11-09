;;;; cell-test.lisp --- lifecycle, the three checks, the report, the tool, /checkup.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: that a start installs the
;;;; command, the manual topic and the tool and a stop takes them back; that
;;;; each check answers and a scope this cell does not know refuses in words;
;;;; that a report with nothing in it says so and points at /doctor; that a
;;;; standing notice becomes a finding with its fix; that the tool answers
;;;; through the real dispatch seam; and that a call missing its required
;;;; member is repaired by the declaration rather than the body.

(in-package #:nodecode.test)

(define-test-slice "checkup" "CHECKUP-CELL-" :start nodecode-checkup:start-cell)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "checkup"
  (:help :checkup)
  (:command "checkup")
  (:running (is (nle::find-tool "run_checkup") "the tool is registered")
            (is (equal '("sections" "cells" "notices") (nodecode-checkup::setting :scopes))
                "scopes defaults to the three checks")
            (is (= 20 (nodecode-checkup::setting :max-findings))))
  (:stopped (is (null (nle::find-tool "run_checkup")) "stop unregisters the tool"))
  (:refused ("max_findings" 0))
  (:idle nodecode-checkup:checkup-error (checkup:scan)))

;;; --- the checks and the report ---------------------------------------------------

(deftest checkup-cell-a-report-with-nothing-in-it-says-so ()
  (with-cell-stop ((checkup-start))
    (is-carrying (text (nodecode-checkup::format-report '() '("sections" "cells")))
      ("checkup: nothing to fix across sections, cells"))
    (is-carrying (text (nodecode-checkup::format-report '() '("notices")))
      ("/doctor reads the rest" "the half it does not read is named"))))

(deftest checkup-cell-each-check-answers-and-a-stranger-refuses ()
  (with-cell-stop ((checkup-start))
    (dolist (scope '("sections" "cells" "notices"))
      (let ((text (checkup:scan scope)))
        (is (stringp text) scope)
        (is (uiop:string-prefix-p "checkup: " text) scope)))
    (is (equal '("sections" "cells" "notices") (nodecode-checkup::checked-scopes nil))
        "bare means every check, in order")
    (is (equal '("cells") (nodecode-checkup::checked-scopes '("cells"))))
    (is (search "no check of this cell" (refusal-text nodecode-checkup:checkup-error
                                                      (checkup:scan "nonsense")))
        "a scope the cell does not know names the set it does")))

(deftest checkup-cell-a-standing-notice-is-a-finding-with-its-fix ()
  (with-cell-stop ((checkup-start))
    (nle:notice "the telegram bot is not running" :key "checkup-cell-voice" :level :error)
    (unwind-protect
         (let ((text (checkup:scan "notices")))
           (is-carrying (text text)
             ("[fix] notices · checkup-cell-voice" "the key names the part")
             ("telegram bot is not running" "and what it said")
             ("->" "every finding carries its fix")))
      (nle:notice nil :key "checkup-cell-voice"))))

(deftest checkup-cell-a-long-report-says-how-much-it-left-out ()
  (with-cell-stop ((checkup-start "max_findings" 1))
    (let ((text (nodecode-checkup::format-report
                 (list (nodecode-checkup::finding "cells" :fix "a" "one")
                       (nodecode-checkup::finding "cells" :fix "b" "two"))
                 '("cells") :cap 1)))
      (is-carrying (text text)
        ("2 findings across cells (the first 1 shown)")
        ("[fix] cells · a")))))

;;; --- the tool --------------------------------------------------------------------

(deftest checkup-cell-the-tool-answers-through-the-dispatch-seam (with-session-store ("s1"))
  (with-cell-stop ((checkup-start))
    (let ((result (execute-wire-call "call-1" "run_checkup" "{\"scope\":\"cells\"}")))
      (is (equal "tool" (gethash "role" result)))
      (is (uiop:string-prefix-p "checkup: "
                                (princ-to-string (nle::message-content result)))
          "the tool answers the report, not a summary of it"))))

(deftest checkup-cell-the-tool-is-held-to-the-shape-it-advertises (with-session-store ("s1"))
  (with-cell-stop ((checkup-start))
    (let* ((entry (nle::find-tool "run_checkup"))
           (contract (second entry)))
      (is (nlk:contract-p contract) "the tool is registered against its declaration")
      (is (equalp (nlk:contract-schema contract)
                  (nlk:json-value (aref (nle::tools-payload (list entry)) 0)
                                  :object "function" "parameters"))))
    (loop for (arguments needle) in '(("{}" "scope must be present")
                                      ("{\"scope\":\"meh\"}" "one of all"))
          do (is-carrying (text (princ-to-string
                                 (nle::message-content
                                  (execute-wire-call "call-bad" "run_checkup" arguments))))
               ("contract_refused" arguments)
               (is (search needle text) needle)))))
