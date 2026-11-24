;;;; cell-test.lisp --- the standing section, the table, the two renderings, the
;;;; owner's additions, and the command's verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: that a start installs the section hook, the command
;;;; and the manual topic and a stop takes every one back; that the nine
;;;; articles parse out of the shipped file and that a form outside the two the
;;;; declaration allows refuses the start; that the compact form rides a request
;;;; and is capped, that the full form carries the whole text and one state
;;;; line an article, and that a stopped cell leaves the request alone; that an
;;;; article reads ENFORCED exactly when the cell carrying one of the enforcers
;;;; it names sits in the cells folder; that the command answers each of its
;;;; verbs and its usage line; and that the owner's additions are appended
;;;; under their own cap. Everything is offline: one file in this folder and
;;;; one scratch folder are the whole of what the cell reads.

(in-package #:nodecode.test)

(define-test-slice "doctrine" "DOCTRINE-CELL-" :start nodecode-doctrine:start-cell)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "doctrine"
  (:hooks 'nle::read-harness-sections)
  (:help :doctrine)
  (:command "doctrine")
  (:running (is (equal "compact" (nodecode-doctrine::setting :form)) "compact by default")
            (is (eq t (nodecode-doctrine::setting :inject)) "the doctrine rides a request by default")
            (is (= 9 (length (nodecode-doctrine::doctrine-articles))) "the file's nine articles parse"))
  (:refused ("form" "medium"))
  (:idle doctrine:doctrine-error
         (doctrine:status-text)
         (doctrine:plugins-text)
         (doctrine:show-text)))

;;; --- the standing section ------------------------------------------------------------

(deftest doctrine-cell-the-compact-form-rides-a-request-and-comes-off-at-stop ()
  (with-cell-stop ((doctrine-start))
    (is-carrying (section (cell-section nil "doctrine"))
      "Nothing sends, spends, or spins without the one who holds the gate."
      "IV. Proof over claims"
      "ADVISED"
      (:absent "*Enforced by:" "the compact form carries no article's markers")
      (is (<= (length section) 2600) "the section stays inside the compact cap"))
    (funcall stop)
    (is (null (cell-section nil "doctrine")) "a stop takes the section off the request")))

(deftest doctrine-cell-the-full-form-carries-the-whole-text-and-one-state-a-line ()
  (with-cell-stop ((doctrine-start "form" "full"))
    (is-carrying (section (cell-section nil "doctrine"))
      "*Enforced by: standing."
      "Install state:"
      "IV: ADVISED"
      "IX: ADVISED"
      ("This cell enforces nothing itself" "the footer rides the full form too"))))

(deftest doctrine-cell-no-inject-leaves-the-request-alone-and-keeps-the-command ()
  (with-cell-stop ((doctrine-start "inject" nil))
    (is (null (cell-section nil "doctrine")) "no section when the section says so")
    (is-present (entry (nle::find-registered-command "doctrine")) "and /doctrine is still there"
      (is (equal "nodecode-doctrine" (nle::slash-command-owner entry))))))

;;; --- the report over installed cells -----------------------------------------------

(deftest doctrine-cell-an-article-reads-enforced-only-when-its-cell-is-installed ()
  (with-temp-workspace (root "nodecode-axioma/plugin.yaml" "nodecode-tripwire/plugin.yaml")
    (with-cell-stop ((doctrine-start "cells_dir" (namestring root)))
      (is-carrying (table (doctrine:status-text))
        "ENFORCED (hermes-axioma)"
        "ENFORCED (tripwire)"
        "IV | Proof over claims | standing | ADVISED"
        "2 of 9 enforced on this install"
        (:absent "ENFORCED (handydandy)" "the cell that is not installed stays ADVISED"))
      (is-carrying (report (doctrine:plugins-text))
        "hermes-axioma | nodecode-axioma | yes"
        "standing | none yet | no cell carries it"
        "handydandy | nodecode-handydandy | not found"))))

(deftest doctrine-cell-a-home-with-no-cells-enforces-none ()
  (with-temp-workspace (root)
    (with-cell-stop ((doctrine-start "cells_dir" (namestring root)))
      (is-carrying (table (doctrine:status-text))
        "0 of 9 enforced on this install"
        "IX | Secrets stay secret | none (doctrine only) | ADVISED"))))

;;; --- the command ------------------------------------------------------------------

(deftest doctrine-cell-the-command-answers-each-verb-and-its-usage-line ()
  (with-temp-workspace (root "nodecode-handydandy/plugin.yaml")
    (with-cell-stop ((doctrine-start "cells_dir" (namestring root)))
      (is-carrying (bare (cell-entry "nodecode-doctrine" "doctrine" nil))
        "No. | Article | Enforcer(s) | State"
        "VII | Work is handed off, not dropped | handydandy | ENFORCED (handydandy)")
      (is-carrying (whole (cell-entry "nodecode-doctrine" "doctrine" "show"))
        "## IV. Proof over claims"
        "## IX. Secrets stay secret")
      (is-carrying (one (cell-entry "nodecode-doctrine" "doctrine" "show III"))
        "## III. The gate"
        "send"
        (:absent "## IV. Proof over claims" "one article is one article"))
      (is-carrying (roman (cell-entry "nodecode-doctrine" "doctrine" "show 4"))
        "## IV. Proof over claims")
      (is-carrying (plugins (cell-entry "nodecode-doctrine" "doctrine" "plugins"))
        "handydandy | nodecode-handydandy | yes"
        "presence is enabled here")
      (is-carrying (bogus (cell-entry "nodecode-doctrine" "doctrine" "everything"))
        "Usage: /doctrine")
      (is-carrying (missing (cell-entry "nodecode-doctrine" "doctrine" "show X"))
        "Usage: /doctrine"))))

;;; --- the owner's additions ---------------------------------------------------------

(deftest doctrine-cell-owner-additions-ride-under-their-own-cap ()
  (with-temp-file (additions :type "md"
                             :contents "## X. Local review\nShow evidence before sharing a result.")
    (with-cell-stop ((doctrine-start "additions" additions))
      (is-carrying (section (cell-section nil "doctrine"))
        "Owner additions:"
        "Show evidence before sharing a result."
        ("IX. Secrets stay secret" "and the nine articles still lead"))))
  (with-temp-file (long :type "md" :contents (make-string 900 :initial-element #\x))
    (with-cell-stop ((doctrine-start "additions" long))
      (is-carrying (section (cell-section nil "doctrine"))
        ("[Owner additions truncated.]" "the additions cap holds a request"))
      (let ((whole (doctrine:show-text)))
        (is (search (make-string 900 :initial-element #\x) whole) "show carries the additions whole")
        (is (null (search "[Owner additions truncated.]" whole)) "and drops the truncation note")))))
