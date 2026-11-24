;;;; cell-test.lisp --- lifecycle; new versus refine; /soul; the guide.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here touches the network or a real home: the SOUL.md is a
;;;; fixture file under the scratch home, and NLE:SUBMIT is stubbed so no
;;;; session is needed.

(in-package #:nodecode.test)

(define-test-slice "soul-finder" "SOUL-FINDER-CELL-"
  :start nodecode-soul-finder:start-cell)

(define-cell-lifecycle-tests "soul-finder"
  (:help :soul-finder)
  (:command "soul")
  (:running (is (search "\"mode\"" (nodecode-soul-finder:soul-finder-status)) "the status verb answers"))
  (:refused ("min_lines" 0))
  (:idle nodecode-soul-finder:soul-finder-error
         (nodecode-soul-finder:soul-finder-status)
         (nodecode-soul-finder:soul-mode)))

(defun sf-fixture-path ()
  "A SOUL.md of this test's own, under the scratch home."
  (namestring (nlk:home "soul-finder-fixture/SOUL.md")))

(defun sf-write (text)
  (let ((path (sf-fixture-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
      (write-string text out))
    path))

(defparameter *lean-soul*
  (format nil "# Identity~%~%One teammate: direct, warm, allergic to filler.~%~%## Style~%~%- Lead with the point, then the context.~%- Punchy and plain; short sentences.~%- Say the whole thing once; no throat clearing.~%~%## Avoid~%~%- Hedging, passive voice, politeness theater.~%- Corporate cadence, and three clauses where one will do.~%- Claims nobody verified.~%~%## The Standard~%~%Finish the whole thing, then say it once and stop.~%"))

(deftest soul-finder-cell-new-then-refine ()
  (let ((path (sf-write "short seed")))
    (with-cell-stop ((soul-finder-start "soul_file" path))
      (is (string= "new" (nodecode-soul-finder:soul-mode)) "a tiny seed is not a real soul")
      (is (search "\"new\"" (nodecode-soul-finder:soul-finder-status)))
      (sf-write *lean-soul*)
      (is (nodecode-soul-finder:real-soul-p) "a lean soul is real")
      (is (string= "refine" (nodecode-soul-finder:soul-mode)) "and the mode turns to refine")
      (is (search "\"refine\"" (nodecode-soul-finder:soul-finder-status)))
      (is (search path (nodecode-soul-finder:soul-finder-status)) "status names the file it read")
      (is (search "\"voice\"" (nodecode-soul-finder:soul-finder-status)) "and calls a lean file a voice"))
    (is (probe-file (sf-fixture-path)) "the cell writes nothing over the file it read")))

(deftest soul-finder-cell-absent-soul-is-new ()
  (when (probe-file (sf-fixture-path)) (delete-file (sf-fixture-path)))
  (with-cell-stop ((soul-finder-start "soul_file" (sf-fixture-path)))
    (is (null (nodecode-soul-finder:soul-text)) "no file, no text")
    (is (string= "new" (nodecode-soul-finder:soul-mode)))
    (is (search "\"soul_exists\":false" (nodecode-soul-finder:soul-finder-status)))))

(deftest soul-finder-cell-complete-marks-the-flow ()
  (with-cell-stop ((soul-finder-start "soul_file" (sf-fixture-path)))
    (is (not (nodecode-soul-finder::flag "complete")) "a fresh home is unfinished")
    (is (search "\"success\"" (nodecode-soul-finder:soul-finder-complete)))
    (is (nodecode-soul-finder::flag "complete") "the marker is kept")
    (is (probe-file (nodecode-soul-finder::marker-path "complete")) "as a file of the cell's own beside the home")
    (is (search "\"flow_complete\":true" (nodecode-soul-finder:soul-finder-status)))))

(deftest soul-finder-cell-slash-submits-the-kickoff ()
  (with-cell-stop ((soul-finder-start "soul_file" (sf-fixture-path)))
    (let ((submitted '()))
      (with-stubbed-fdefinition (nle:submit (session prompt &key command-id source source-id images steer &allow-other-keys)
                                             (declare (ignore command-id source source-id images steer))
                                             (push (list session prompt) submitted)
                                             t)
        (multiple-value-bind (text dialog) (nodecode-soul-finder:run-slash "new" "s1")
          (is (null dialog) "a kickoff opens no picker")
          (is (search "SOUL.md" text))
          (is (equal "s1" (first (first submitted))) "it lands in the running session")
          (is (search "walk me through it" (second (first submitted)))))
        (setf submitted '())
        (is (search "Let's build" (nodecode-soul-finder:run-slash "" "s1")) "with no argument a home without a soul picks the new flow")
        (sf-write *lean-soul*)
        (setf submitted '())
        (is (search "audit" (nodecode-soul-finder:run-slash "" "s1")) "and a real soul picks the refine flow")
        (setf submitted '())
        (is (search "\"mode\"" (nodecode-soul-finder:run-slash "status" "s1")) "status answers instead of submitting")
        (is (null submitted) "and submits nothing")))))

(deftest soul-finder-cell-guide-reads-the-skill ()
  (with-cell-stop ((soul-finder-start "soul_file" (sf-fixture-path)))
    (let ((guide (nodecode-soul-finder:soul-finder-guide)))
      (is (search "SOUL.md Refinement Workflow" guide))
      (is (search "Cheeky" guide) "the tone trap is kept, whole")
      (is (< 2000 (length guide)) "the workflow, not a summary of it"))))
