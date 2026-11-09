;;;; cell-test.lisp --- what a scaffold writes, what it refuses, what a patch adds.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: that the folder a scaffold writes is one this
;;;; image's own ASDF loads and this organism starts, and that the tool it
;;;; declares answers through its handler; that a name which would leave the
;;;; cells folder is refused, and so is a folder already there, and one tool
;;;; too many; that a tool and a command spliced into a scaffolded cell are
;;;; in the file and in the running cell, and that a folder without the
;;;; scaffold's markers refuses by name instead of being half-patched.

(in-package #:nodecode.test)

;;; --- lifecycle --------------------------------------------------------------------

(define-cell-lifecycle-tests "cell-generator"
  (:help :cell-generator)
  (:command "cell-generator")
  (:running (is (= 8 (nodecode-cell-generator::setting :max-tools)) "max_tools defaults to eight"))
  (:refused ("max_tools" 0))
  (:idle nodecode-cell-generator::cell-generator-error
         (cell-generator:scaffold (nlk:json-object "name" "idle-sample"))))

;;; --- a scaffolded folder -------------------------------------------------------------

(deftest cell-generator-scaffold-writes-a-cell-that-loads-and-answers ()
  (with-cells ()
    (let ((answer (cell-generator:scaffold
                   (nlk:json-object
                    "name" "smoke-sample"
                    "description" "a cell a test scaffolded"
                    "tools" (vector (nlk:json-object
                                     "name" "ping"
                                     "description" "answer a greeting"
                                     "params" (vector (nlk:json-object
                                                       "name" "who"
                                                       "type" "string"
                                                       "description" "who to greet"
                                                       "required" t))))))))
      (is (string= "created" (answer-member answer "status")))
      (is (stringp (answer-member answer "path")))
      (is (probe-file (generated-file "smoke-sample" "cell.lisp")) "cell.lisp")
      (is (probe-file (generated-file "smoke-sample" "nodecode-smoke-sample.asd")) "the system")
      (is (probe-file (generated-file "smoke-sample" "README.md")) "the README")
      (is (probe-file (generated-file "smoke-sample" "test/support.lisp")) "the slice")
      (is (probe-file (generated-file "smoke-sample" "test/cell-test.lisp")) "the lifecycle tests")
      (is (search "\"ping\"" (uiop:read-file-string (generated-file "smoke-sample" "cell.lisp"))) "the tool is declared")
      ;; the folder is a cell: it loads, it starts, and its tool answers
      (load-generated-cell "smoke-sample" :test-system t)
      (let ((stop (start-generated "smoke-sample")))
        (unwind-protect
             (let ((ping (find-symbol "PING" (find-package "NODECODE-SMOKE-SAMPLE"))))
               (is (fboundp ping) "the handler is defined")
               (is (stringp (funcall ping (nlk:json-object "who" "there"))) "the handler answers a string")
               (is (assoc "ping" nle::*tools* :test #'string=) "the tool is registered"))
          (funcall stop)
          (is (null (assoc "ping" nle::*tools* :test #'string=)) "and stopping takes it back"))))))

;;; --- refusals --------------------------------------------------------------------------

(deftest cell-generator-refuses-a-name-that-would-leave-the-cells-folder ()
  (with-cells ()
    (signals-error nodecode-cell-generator::cell-generator-error
      (cell-generator:scaffold (nlk:json-object "name" "../evil")))
    (is (null (probe-file (merge-pathnames "evil/" (nlk:home)))) "nothing escaped the cells folder")
    (signals-error nodecode-cell-generator::cell-generator-error
      (cell-generator:scaffold (nlk:json-object "name" "   ")))
    (cell-generator:scaffold (nlk:json-object "name" "twice-sample"))
    (signals-error nodecode-cell-generator::cell-generator-error
      (cell-generator:scaffold (nlk:json-object "name" "twice-sample")))))

(deftest cell-generator-refuses-more-tools-than-the-section-allows ()
  (with-cells ("max_tools" 1)
    (signals-error nodecode-cell-generator::cell-generator-error
      (cell-generator:scaffold
       (nlk:json-object
        "name" "greedy-sample"
        "tools" (vector (nlk:json-object "name" "one")
                         (nlk:json-object "name" "two")))))
    (is (null (probe-file (generated-folder "greedy-sample"))) "nothing was half-written")))

;;; --- a patch ---------------------------------------------------------------------------

(deftest cell-generator-adds-a-tool-and-a-command-to-a-cell-it-wrote ()
  (with-cells ()
    (cell-generator:scaffold (nlk:json-object "name" "growth-sample"
                                                "description" "a cell that grows"
                                                "with_command" :false))
    (let ((answer (cell-generator:add-tool
                   (nlk:json-object
                    "cell" "growth-sample"
                    "name" "shout"
                    "description" "say it loud"
                    "params" (vector (nlk:json-object "name" "word" "type" "string"))))))
      (is (string= "tool_added" (answer-member answer "status")))
      (is (search "(:tool \"shout\"" (uiop:read-file-string (generated-file "growth-sample" "cell.lisp"))) "the clause is in the file"))
    (let ((answer (cell-generator:add-command
                   (nlk:json-object "cell" "growth-sample" "name" "grow"
                                     "description" "grow the cell"))))
      (is (string= "command_added" (answer-member answer "status"))))
    (let ((stop (start-generated "growth-sample")))
      (unwind-protect
           (progn
             (is (assoc "shout" nle::*tools* :test #'string=) "the added tool runs")
             (let ((grow (find-symbol "RUN-GROW" (find-package "NODECODE-GROWTH-SAMPLE"))))
               (is (fboundp grow) "the added command is defined")
               (is (search "grow the cell" (funcall grow "" nil)) "and answers")))
        (funcall stop)))))

(deftest cell-generator-refuses-to-patch-a-folder-without-its-markers ()
  (with-cells ()
    (let ((folder (merge-pathnames "nodecode-handmade/" (nodecode-cell-generator::cell-directory))))
      (ensure-directories-exist (merge-pathnames "cell.lisp" folder))
      (with-open-file (out (merge-pathnames "cell.lisp" folder) :direction :output
                                                             :if-exists :supersede
                                                             :if-does-not-exist :create)
        (write-string ";; a cell nobody scaffolded here: no markers.
" out))
      (signals-error nodecode-cell-generator::cell-generator-error
        (cell-generator:add-tool (nlk:json-object "cell" "handmade" "name" "added")))
      (signals-error nodecode-cell-generator::cell-generator-error
        (cell-generator:add-command (nlk:json-object "cell" "handmade" "name" "added")))
      (is (string= ";; a cell nobody scaffolded here: no markers.
"
                   (uiop:read-file-string (merge-pathnames "cell.lisp" folder))) "the folder is untouched"))))
