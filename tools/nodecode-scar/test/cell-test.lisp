;;;; cell-test.lisp --- a deny keeps a shape, the shape stops a call, a person forgets it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test runs in the scratch home the cell lane gives the run, so the
;;;; ledger file is that home's and never the operator's: nothing here dials a
;;;; provider, nothing needs the network, and the seam a question arrives on
;;;; is a stub that answers at once.

(in-package #:nodecode.test)

(define-test-slice "scar" "SCAR-CELL-"
  :start nodecode-scar:start-cell)

(define-cell-lifecycle-tests "scar"
  (:hooks :tool 'nle:ask-operator)
  (:help :scar)
  (:command "scar")
  (:running
   (is (= 200 (nodecode-scar::setting :max-scars)) "max_scars defaults to 200")
   (is (= 120 (nodecode-scar::setting :ask-seconds)) "a forget question waits two minutes")
   (is (eq t (nodecode-scar::setting :record)) "a deny is kept"))
  (:refused ("max_scars" 0) ("ask_seconds" 1))
  (:idle scar:scar-error
         (scar:list-scars)
         (scar:check "rm -rf x")
         (scar:forget 1)))

;;; --- fixtures ---------------------------------------------------------------------

(defun scar-emptied ()
  "Delete the ledger this home holds, so a test starts with none."
  (let ((path (nodecode-scar::home-file)))
    (when (probe-file path) (delete-file path)))
  t)

(defmacro with-scar (&body body)
  "The cell started on its defaults, with an emptied ledger."
  `(with-cell-stop ((scar-start))
     (scar-emptied)
     ,@body))

(defmacro with-scar-section (pairs &body body)
  "The cell started on PAIRS of section overrides, with an emptied ledger."
  `(with-cell-stop ((scar-start ,@pairs))
     (scar-emptied)
     ,@body))

(defun scar-ask (answer why)
  "A stub for the question seam: it answers ANSWER and WHY at once."
  (lambda (&rest args)
    (declare (ignore args))
    (values answer why)))

(defun scar-rows ()
  "The rows the ledger holds, oldest first."
  (nth-value 1 (nodecode-scar::read-scars)))

(defun row-tokens-of (row)
  "The token set a ledger row keeps."
  (nodecode-scar::row-tokens row))

(defun scar-hook (form)
  "The cell's :TOOL hook run by hand on an eval call carrying FORM."
  (funcall (cell-hook "nodecode-scar" :tool)
           (list :name "eval" :call-id "c1"
                 :arguments (nlk:make-json-object "form" form))
           (lambda (op) (declare (ignore op)) "ran")))

;;; --- a deny keeps one shape ---------------------------------------------------------

(deftest scar-cell-a-deny-keeps-the-shape ()
  (with-scar
    (let ((answer (nodecode-scar::observe-question
                   (scar-ask "Deny" :answered)
                   "The guard stopped a call: recursive delete"
                   :detail "rm -rf ./build"
                   :choices '("Allow once" "Deny")
                   :seconds 5
                   :session-id "s1")))
      (is (equal "Deny" answer) "the advice answers what the question answered"))
    (let ((rows (scar-rows)))
      (is (= 1 (length rows)) "one deny, one scar")
      (is (equal '("force" "recursive" "rm") (row-tokens-of (first rows))) "the token set is kept, and no path")
      (is (equal "rm -rf ./build" (nodecode-scar::row-preview (first rows)))))
    (let ((text (scar:list-scars)))
      (is (search "scar #1" text))
      (is (search "(force, recursive, rm)" text) "the token set is what a person reads")
      (is (search "it holds 1 scar" text) "one stored scar is reported as 1 scar"))))

(deftest scar-cell-only-a-deny-is-kept ()
  (with-scar
    (dolist (case '(("Allow once" . :answered) ("Allow for this session" . :answered)
                    (nil . :timeout) (nil . :nobody)))
      (nodecode-scar::observe-question (scar-ask (car case) (cdr case))
                                        "A block pattern stopped a call" :detail "rm -rf x"))
    (is (null (scar-rows)) "allow once, allow always, a timeout and nobody there keep nothing")
    (nodecode-scar::observe-question (scar-ask "Deny" :answered)
                                      "A block pattern stopped a call"
                                      :detail "echo rm -rf is dangerous")
    (is (null (scar-rows)) "a denied command with no rm shape is not stored")))

(deftest scar-cell-a-preview-masks-secrets ()
  (with-scar
    (scar:deny "AWS_SECRET_ACCESS_KEY=abc123xyz rm -rf ./build")
    (let ((preview (nodecode-scar::row-preview (first (scar-rows)))))
      (is (search "AWS_SECRET_ACCESS_KEY=****" preview) "an assignment whose name says secret is masked")
      (is (null (search "abc123xyz" preview)) "the value is gone from the file"))))

;;; --- what the shape stops --------------------------------------------------------------

(deftest scar-cell-a-covered-call-is-refused ()
  (with-scar
    (scar:deny "rm -rf ./build")
    (let ((blocked (scar-hook "(sh \"rm -fr ./out\")"))
          (weaker (scar-hook "(sh \"rm -r ./out\")"))
          (plain (scar-hook "(sh \"ls -l\")"))
          (sentence (scar-hook "(sh \"echo rm -rf is dangerous\")")))
      (is (search "blocked by nodecode-scar" blocked))
      (is (search "pressed deny" blocked) "the block says a person pressed deny")
      (is (search "do not run it again" blocked))
      (is (equal "ran" weaker) "rm -r does not carry force, so this scar does not cover it")
      (is (equal "ran" plain) "a call with no rm shape runs")
      (is (equal "ran" sentence) "a command that only prints the words runs"))))

(deftest scar-cell-an-unreadable-ledger-blocks-only-rm ()
  (with-scar
    (let ((path (nodecode-scar::home-file)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
        (write-line "this is not json" out)))
    (is (search "could not be read" (scar-hook "(sh \"rm -rf ./out\")")) "an rm call is not run")
    (is (equal "ran" (scar-hook "(sh \"ls -l\")")) "every other call still runs")
    (is (search "could not be read" (scar:check "rm -rf y")) "check says the same")))

(deftest scar-cell-a-full-ledger-refuses-a-new-scar ()
  (with-scar-section ("max_scars" 1)
    (scar:deny "rm -r x")
    (is (search "at its cap" (scar:deny "rm -rf y")) "a full ledger refuses rather than drops one")
    (is (= 1 (length (scar-rows))) "the stored scar is still there")))

;;; --- the operator's hand ---------------------------------------------------------------

(deftest scar-cell-check-and-the-slash-command ()
  (with-scar
    (is (search "carries no rm danger token" (scar:check "ls -l")))
    (is (search "the ledger file is absent" (scar:check "rm -rf x")))
    (scar:deny "rm -rf ./build")
    (is (search "would be blocked" (scar:check "rm -rf ./dist")))
    (is (search "none covers" (scar:check "rm x")) "a bare rm is not this shape")
    (is (search "scar #1" (nodecode-scar::run-slash "list" nil)))
    (is (search "WHAT MATCHES" (nodecode-scar::run-slash "help" nil)) "help is the manual")
    (is (search "Unknown option: nosuch" (nodecode-scar::run-slash "nosuch" nil)))
    (is (search "forgotten" (nodecode-scar::run-slash "forget 1" nil)) "/scar forget is the answer")
    (is (null (scar-rows)))))

(deftest scar-cell-forget-needs-a-yes ()
  (with-scar
    (scar:deny "rm -rf x")
    (is (search "was not forgotten" (scar:forget 1 :ask (scar-ask "Keep it" :answered))) "Keep it leaves it")
    (is (= 1 (length (scar-rows))))
    (is (search "was not forgotten" (scar:forget 1 :ask (scar-ask nil :nobody))) "nobody there leaves it")
    (is (search "forgotten" (scar:forget 1 :ask (scar-ask "Forget it" :answered))) "a yes forgets it")
    (is (null (scar-rows)))))
