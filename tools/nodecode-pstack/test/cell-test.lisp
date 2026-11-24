;;;; cell-test.lisp --- the catalogue, the reader, its refusals and the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials anything: the cell reads its own prose/ folder, and one
;;;; test stubs that read to prove the path and the limit it hands on.

(in-package #:nodecode.test)

(define-test-slice "pstack" "PSTACK-CELL-" :start nodecode-pstack:start-cell)

(defun pstack-tool ()
  "The registered `pstack' tool's function, or NIL."
  (third (nle::find-tool "pstack")))

(defun pstack-answer (&rest pairs)
  "The `pstack' tool over PAIRS, as the model calls it."
  (funcall (pstack-tool) (apply #'nlk:make-json-object pairs)))

(defun pstack-refusal (&rest pairs)
  "The refusal PAIRS earn from the tool, as text."
  (refusal-text nodecode-pstack::pstack-error (apply #'pstack-answer pairs)))

(defun pstack-every-piece ()
  (append (nodecode-pstack::playbooks) (nodecode-pstack::skills)))

;;; --- the catalogue -------------------------------------------------------------------

(deftest pstack-cell-the-catalogue-names-every-playbook-and-every-skill ()
  (with-cell-stop ((pstack-start))
    (let ((shown (pstack-answer)))
      (is (search "pstack: Poteto's engineering rigor stack." shown))
      (is (search "Start with pstack(name=\"poteto-mode\")" shown) "the router is named first")
      (dolist (piece (pstack-every-piece))
        (is (search (nodecode-pstack::piece-name piece) shown)
            (format nil "~a is in the catalogue" (nodecode-pstack::piece-name piece))))
      (is (>= (length (nodecode-pstack::skills)) 50) "every skill of the original is here")
      (is (>= (length (nodecode-pstack::playbooks)) 15) "and every playbook")
      (is (search "investigation" shown))
      (is (search "principle-prove-it-works" shown)))))

(deftest pstack-cell-every-piece-sits-where-the-catalogue-names-it ()
  (with-cell-stop ((pstack-start))
    (dolist (piece (pstack-every-piece))
      (is (probe-file (nodecode-pstack::piece-path piece)) (nodecode-pstack::piece-name piece)))
    (is (every (lambda (piece) (plusp (length (nodecode-pstack::piece-summary piece)))) (nodecode-pstack::playbooks)) "every playbook opens with a line")))

;;; --- reading one -----------------------------------------------------------------------

(deftest pstack-cell-a-playbook-comes-back-whole-under-its-header ()
  (with-cell-stop ((pstack-start))
    (let ((text (pstack-answer "name" "feature")))
      (is (search ";; pstack: feature (playbook), prose/skills/poteto-mode/playbooks/feature.md" text))
      (is (search "Where it says skill_view" text) "the header says how the original's names read here")
      (is (search "# Feature Playbook" text))
      (is (search "**Trigger:**" text) "the original's words, unchanged"))))

(deftest pstack-cell-a-skill-and-the-router-are-read-the-same-way ()
  (with-cell-stop ((pstack-start))
    (is (search "Design types and module structure before writing code"
                (pstack-answer "name" "architect")))
    (is (search "PLAYBOOK ROUTING" (pstack-answer "name" "poteto-mode")))
    (is (search "declaring it done" (pstack-answer "name" "principle-prove-it-works")))
    (is (search "declaring it done" (pstack-answer "name" "Principle-Prove-It-Works")) "a name is read without regard to case")))

(deftest pstack-cell-a-reference-file-is-read-by-its-path ()
  (with-cell-stop ((pstack-start))
    (let ((text (pstack-answer "name" "reflect/judgment-reviewer")))
      (is (search "prose/skills/reflect/references/judgment-reviewer.md" text))
      (is (> (length text) 200) "the file itself, not only its header"))))

(deftest pstack-cell-the-reader-gets-the-file-and-the-limit ()
  (with-cell-stop ((pstack-start "max_chars" 5000))
    (let* ((seen '())
           (nodecode-pstack::*read-text*
             (lambda (path limit)
               (push (list (nodecode-pstack::path-relative path) limit) seen)
               (values "STUBBED TEXT" nil)))
           (text (pstack-answer "name" "feature")))
      (is (search "STUBBED TEXT" text) "what the reader answers is what the model reads")
      (is (equal '(("skills/poteto-mode/playbooks/feature.md" 5000)) seen) "the file and the limit it was given")
      (is (search ";; pstack: feature (playbook)" text) "the header stands above it"))))

(deftest pstack-cell-a-long-file-is-cut-and-says-where-the-rest-is ()
  (with-cell-stop ((pstack-start "max_chars" 1200))
    (let ((text (pstack-answer "name" "orchestrate")))
      (is (< (length text) 1800) "the answer stops at the limit")
      (is (search "cut at 1200 characters" text))
      (is (search "prose/skills/poteto-mode/playbooks/orchestrate.md" text) "and names the whole file")
      (is (search "raise pstack.max_chars" text) "and the setting that raises it"))))

(deftest pstack-cell-a-file-under-the-limit-is-not-announced-as-cut ()
  (with-cell-stop ((pstack-start "max_chars" 24000))
    (is (not (search "cut at" (pstack-answer "name" "feature"))) "19K of playbook fits")))

;;; --- refusals ----------------------------------------------------------------------------

(deftest pstack-cell-a-name-outside-the-stack-is-refused-with-the-names ()
  (with-cell-stop ((pstack-start))
    (let ((text (pstack-refusal "name" "ghost")))
      (is (search "ghost is not a pstack playbook or skill" text))
      (is (search "investigation" text) "the refusal names what can be read"))))

(deftest pstack-cell-a-path-may-not-leave-the-prose ()
  (with-cell-stop ((pstack-start))
    (is (search "is not a pstack playbook or skill" (pstack-refusal "name" "../../README")))
    (is (search "is not a pstack playbook or skill" (pstack-refusal "name" "/etc/passwd")))
    (is (search "is not a pstack playbook or skill"
                (pstack-refusal "name" "poteto-mode/../../../README")))))

(deftest pstack-cell-a-setting-out-of-its-shape-refuses ()
  (is (signals-error nlk:config-refusal (pstack-start "max_chars" 5)) "under the minimum")
  (is (signals-error nlk:config-refusal (pstack-start "max_chars" "many")) "not an integer")
  (is (null (nle::find-tool "pstack")) "and nothing was left registered"))

;;; --- the command --------------------------------------------------------------------------

(deftest pstack-cell-the-command-answers-the-catalogue-and-one-file ()
  (with-cell-stop ((pstack-start))
    (is (search "Playbooks:" (nle:slash "/pstack")))
    (is (search "Skills (read with pstack(name=\"how\"))" (nle:slash "/pstack")))
    (is (search "# Feature Playbook" (nle:slash "/pstack feature")))
    (is (search "is not a pstack playbook or skill" (nle:slash "/pstack ghost")))))

;;; --- lifecycle -----------------------------------------------------------------------------

(define-cell-lifecycle-tests "pstack"
  (:config)
  (:help :pstack)
  (:command "pstack")
  (:running
   (is (functionp (pstack-tool)) "the tool is registered")
   (is (= 24000 (nodecode-pstack::setting :max-chars)) "max_chars defaults to 24000")
   (is (>= (length (nodecode-pstack::skills)) 50)))
  (:refused ("max_chars" 10) ("max_chars" "wide"))
  (:idle nodecode-pstack::pstack-error (nodecode-pstack::serve "feature")))
