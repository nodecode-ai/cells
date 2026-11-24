;;;; cell-test.lisp --- the plan, the work that sets its status, the operator's words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is what the original's plugin gave a user: that the
;;;; four tools draw and read one plan per chat; that a box's status comes from
;;;; the work -- a write in its files, a check's exit code -- and never from the
;;;; model saying so; that a file no box planned shows up as drift and can be
;;;; adopted; that the operator's note reaches the model and is read once; that
;;;; a write into a removed box is refused before the call runs; and that a plan
;;;; the document cannot keep is refused with a reason the model can act on.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "plan-canvas"
  (:hooks :tool)
  (:help :plan-canvas)
  (:command "plan-canvas")
  (:running (is (= 40 (nodecode-plan-canvas:setting :keep-edits)))
            (is (string= nodecode-plan-canvas:+primer+ (nle:help :plan-canvas))))
  (:refused ("keep_edits" 0))
  (:idle nodecode-plan-canvas:plan-canvas-error (plan-canvas:run-command nil "s1")))

;;; --- drawing -----------------------------------------------------------------------

(deftest plan-canvas-cell-draws-one-plan-per-chat ()
  (with-canvas ()
    (as-chat ("s1")
      (let ((answer (answer-of (draw-plan
                                 :boxes (list (box-args "api" "Build the API"
                                                        :files '("src/api/**")
                                                        :check "npm test -- api"))))))
        (is (search "cv-" (gethash "canvas_id" answer)))
        (is (search "::plan-canvas{id=" (gethash "card" answer)) "the model gets the card line")
        (is (equal '("api") (coerce (gethash "ready" answer) 'list)))
        (let ((view (view-of "s1")))
          (is (equal "Ship the payments API" (gethash "title" view)))
          (is (equal "/work/project" (gethash "root" view)))
          (is (equal "todo" (gethash "status" (box-of "s1" "api"))))
          (is (= 0 (length (gethash "user_edits" view))) "nothing to read yet"))))
    (as-chat ("s2")
      (is (search "no canvas" (error-of (plan-canvas:tool-get (args)))) "another chat has its own canvas"))))

(deftest plan-canvas-cell-a-redraw-keeps-the-state-of-a-box-that-stays ()
  (with-canvas ()
    (as-chat ("s1")
      (draw-plan :boxes (list (box-args "api" "Build the API" :files '("src/api/**"))))
      (work-seen (result-paths '("/work/project/src/api/charge.ts")))
      (is (equal "doing" (gethash "status" (box-of "s1" "api"))))
      (draw-plan :title "Ship it, again"
                 :boxes (list (box-args "api" "Build the API" :files '("src/api/**"))
                              (box-args "ui" "Build the UI" :files '("src/ui/**"))))
      (is (equal "doing" (gethash "status" (box-of "s1" "api"))) "a kept id keeps its work")
      (is (equal "todo" (gethash "status" (box-of "s1" "ui")))))))

(deftest plan-canvas-cell-refuses-a-plan-it-cannot-keep ()
  (with-canvas ()
    (as-chat ("s1")
      (is (search "drift" (error-of (draw-plan :boxes (list (box-args "d" "A drift" :kind "drift"))))))
      (is (search "unknown boxes"
                  (error-of (draw-plan :boxes (list (box-args "a" "One" :after '("b")))))))
      (is (search "cycle"
                  (error-of (draw-plan :boxes (list (box-args "a" "One" :after '("b"))
                                                    (box-args "b" "Two" :after '("a"))))))))))

;;; --- the work sets the status -------------------------------------------------------

(deftest plan-canvas-cell-a-write-sets-its-box-doing ()
  (with-canvas ()
    (as-chat ("s1")
      (draw-plan :boxes (list (box-args "api" "Build the API" :files '("src/api/**"))
                              (box-args "ui" "Build the UI" :files '("src/ui/**"))))
      (work-seen (result-paths '("/work/project/src/api/charge.ts")))
      (let ((box (box-of "s1" "api")))
        (is (equal "doing" (gethash "status" box)))
        (is (= 1 (gethash "writes" box)) "the write counts"))
      (is (equal "todo" (gethash "status" (box-of "s1" "ui"))) "another box's file is not its own"))))

(deftest plan-canvas-cell-a-check-sets-done-or-failed ()
  (with-canvas ()
    (as-chat ("s1")
      (draw-plan :boxes (list (box-args "api" "Build the API" :check "npm test -- api")))
      (work-seen (check-result "npm test -- api" 0))
      (let ((box (box-of "s1" "api")))
        (is (equal "done" (gethash "status" box)) "exit 0 is done")
        (is (= 0 (gethash "check_exit" box))))
      (work-seen (result-paths '("/work/project/src/api/charge.ts")))
      (work-seen (check-result "cd ./src && npm test -- api 2>&1" 2))
      (let ((box (box-of "s1" "api")))
        (is (equal "failed" (gethash "status" box)) "a non-zero exit is failed")
        (is (= 2 (gethash "check_exit" box)))
        (is (search "[exit 2]" (gethash "check_output_tail" box)) "the output is kept"))
      (work-seen (check-result "npm test -- api --verbose" 0))
      (is (equal "failed" (gethash "status" (box-of "s1" "api"))) "a command that only starts with the check is not the check"))))

(deftest plan-canvas-cell-a-check-owns-its-box-status ()
  (with-canvas ()
    (as-chat ("s1")
      (draw-plan :boxes (list (box-args "api" "Build the API" :check "make test")
                              (box-args "docs" "Write the docs")))
      (let ((answer (update-plan "s1" (nlk:make-json-object "op" "set_status"
                                                            "id" "api" "status" "done"))))
        (is (search "exit code sets done or failed" (gethash "error" answer))))
      (let ((answer (update-plan "s1" (nlk:make-json-object "op" "set_status"
                                                            "id" "docs" "status" "done"))))
        (is (null (gethash "error" answer)))
        (is (equal "done" (gethash "status" (box-of "s1" "docs"))) "an unchecked box may be set")))))

(deftest plan-canvas-cell-a-written-file-no-box-owns-becomes-drift ()
  (with-canvas ()
    (as-chat ("s1")
      (draw-plan :boxes (list (box-args "api" "Build the API" :files '("src/api/**"))))
      (work-seen (result-paths '("/work/project/notes.md")))
      (let ((drift (find-if (lambda (box) (equal "drift" (gethash "kind" box)))
                             (boxes-of "s1"))))
        (is (not (null drift)) "a written file no box planned is drift")
        (let ((id (gethash "id" drift)))
          (let ((answer (update-plan "s1" (nlk:make-json-object "op" "adopt" "id" id))))
            (is (null (gethash "error" answer)))
            (is (equal "component" (gethash "kind" (box-of "s1" id))) "adopting makes it planned work")))))))

;;; --- next -------------------------------------------------------------------------

(deftest plan-canvas-cell-next-lists-only-what-can-start ()
  (with-canvas ()
    (as-chat ("s1")
      (draw-plan :boxes (list (box-args "api" "Build the API" :check "make api")
                              (box-args "ui" "Build the UI" :files '("src/ui/**") :after '("api"))))
      (let ((answer (answer-of (plan-canvas:tool-next (args)))))
        (is (equal '("api") (loop for entry across (gethash "ready" answer)
                                   collect (gethash "id" entry))) "the box that waits is not ready")
        (is (search "[plan-canvas:" (gethash "goal" (aref (gethash "ready" answer) 0))) "each goal carries the tag that links it to its box"))
      (work-seen (check-result "make api" 0))
      (let ((answer (answer-of (plan-canvas:tool-next (args)))))
        (is (equal '("ui") (loop for entry across (gethash "ready" answer)
                                  collect (gethash "id" entry))) "the work alone makes the next box ready")))))

;;; --- the operator's words ----------------------------------------------------------

(deftest plan-canvas-cell-the-operator-s-edits-reach-the-model-once ()
  (with-canvas ()
    (as-chat ("s1")
      (draw-plan :boxes (list (box-args "api" "Build the API")))
      (multiple-value-bind (text dialog) (plan-canvas:run-command "note api keep it small" "s1")
        (declare (ignore dialog))
        (is (search "Left a note on box" text)))
      (let ((view (view-of "s1")))
        (is (search "keep it small" (aref (gethash "user_edits" view) 0)) "the model reads the operator's own words"))
      (is (= 0 (length (gethash "user_edits" (view-of "s1")))) "and reads them once")
      (plan-canvas:run-command "note api keep it small" "s1")
      (is (search "keep it small" (plan-canvas:reminder-text
                                    (plan-canvas:load-doc "s1"))) "the round's reminder carries them too"))))

(deftest plan-canvas-cell-the-operator-removes-and-restores-a-box ()
  (with-canvas ()
    (as-chat ("s1")
      (draw-plan :boxes (list (box-args "api" "Build the API" :files '("src/api/**"))))
      (plan-canvas:run-command "remove api" "s1")
      (is (eq t (gethash "removed" (box-of "s1" "api"))))
      (let ((called nil))
        (let ((refused (plan-canvas:veto-write
                        (list :name "eval"
                              :arguments (args "form" "(write-file \"src/api/charge.ts\" \"x\")"))
                        (lambda (op) (declare (ignore op)) (setf called t) "wrote"))))
          (is (search "removed from the plan" refused) "the write is refused")
          (is (null called) "and the call never ran")))
      (plan-canvas:run-command "restore api" "s1")
      (is (null (gethash "removed" (box-of "s1" "api"))))
      (is (equal "wrote"
                 (plan-canvas:veto-write
                  (list :name "eval"
                        :arguments (args "form" "(write-file \"src/api/charge.ts\" \"x\")"))
                  (lambda (op) (declare (ignore op)) "wrote"))) "a restored box's file is written again"))))

(deftest plan-canvas-cell-a-glob-owns-what-the-plan-says ()
  (is (plan-canvas:glob-match-p "src/api/**" "src/api/charge.ts"))
  (is (plan-canvas:glob-match-p "src/payments" "src/payments/charge.ts") "a bare folder covers it")
  (is (plan-canvas:glob-match-p "src/{api,ui}/x.ts" "src/ui/x.ts") "either alternative")
  (is (not (plan-canvas:glob-match-p "src/api/*.ts" "src/api/deep/x.ts")) "* stays in one segment")
  (is (equal "src/api/x.ts" (plan-canvas:relativize "/work/project/src/api/x.ts" "/work/project")))
  (is (null (plan-canvas:relativize "/elsewhere/x.ts" "/work/project")) "outside the root is outside"))

