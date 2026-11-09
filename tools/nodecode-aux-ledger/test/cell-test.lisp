;;;; cell-test.lisp --- one row per side call, the reports, the tool, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring over a scratch ledger file: that
;;;; start installs both advices and /aux and stop takes them back; that a
;;;; side call answering and one failing each leave exactly one row, the
;;;; second carrying the failure's kind and the first the provider's counts;
;;;; that a task name travels from WITH-TASK into the row; that /aux and the
;;;; aux_ledger tool read those rows back as a table and clear them; and that
;;;; a section with no file refuses.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "aux-ledger"
  (:config "file" (uiop:native-namestring (aux-ledger-scratch-file)))
  (:hooks 'nle:complete 'nle::call-provider)
  (:help :aux)
  (:command "aux")
  (:running
   (is (= 5000 (nodecode-aux-ledger::setting :max-rows)) "max_rows defaults to 5000")
   (is (eq t (nodecode-aux-ledger::setting :record)) "recording is on unless told otherwise"))
  (:refused ("max_rows" 1))
  (:idle nodecode-aux-ledger:aux-ledger-error
         (aux-ledger:report "all")
         (aux-ledger:clear)))

;;; --- the refusal --------------------------------------------------------------

(deftest aux-ledger-cell-a-section-with-no-file-refuses ()
  (is (signals-error nlk:config-refusal (aux-ledger-start)) "no file named")
  (is (signals-error nlk:config-refusal (aux-ledger-start "file" "  ")) "a blank file is no file")
  (is (null nodecode-aux-ledger::*aux-ledger*) "and no settings are left behind"))

;;; --- recording ----------------------------------------------------------------

(deftest aux-ledger-cell-a-side-call-leaves-one-row-with-its-counts ()
  (let ((file (aux-ledger-scratch-file)))
    (with-cell-stop ((aux-ledger-start "file" (uiop:native-namestring file)))
      (let ((answer (aux-ledger-side-call
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (nodecode-aux-ledger::observe-provider-call
                        (aux-ledger-provider-answer
                         "a title"
                         (nodecode.evolved::make-provider-usage
                          :input-tokens 40 :cached-input-tokens 2 :output-tokens 5))
                        :context))
                     "the instruction" "the message" :session-id "s1")))
        (is (equal "a title" answer) "the call's answer comes back untouched")
        (let* ((rows (aux-ledger-rows file))
               (row (first rows)))
          (is (= 1 (length rows)) "one side call, one row")
          (is (equal "s1" (getf row :session-id)) "the session is kept")
          (is (equal "side" (getf row :task)) "unnamed side work reads as side")
          (is (= 40 (getf row :input-tokens)) "the provider's input count is kept")
          (is (= 2 (getf row :cached-input-tokens)) "and its cache reads")
          (is (= 5 (getf row :output-tokens)) "and its output")
          (is (getf row :has-usage) "the row knows usage was reported")
          (is (null (getf row :error-type)) "a clean call carries no failure")
          (is (= 0 (getf row :retry-count)) "one attempt, no retry")
          (is (stringp (getf row :model)) "the model is a string even when none resolved")
          (is (not (search "the message" (format nil "~{~a~%~}" (uiop:read-file-lines file)))) "no message text reaches the file"))))))

(deftest aux-ledger-cell-a-failing-side-call-leaves-its-kind-and-still-signals ()
  (let ((file (aux-ledger-scratch-file)))
    (with-cell-stop ((aux-ledger-start "file" (uiop:native-namestring file)))
      (is (handler-case
              (progn (aux-ledger-side-call
                      (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (error "the target is down"))
                      "the instruction" "the message")
                     nil)
            (error () t)) "the caller still sees the failure")
      (let ((row (first (aux-ledger-rows file))))
        (is (equal "simple-error" (getf row :error-type)) "the row keeps the kind, not the message")
        (is (not (getf row :has-usage)) "a failed attempt reported no usage"))
      (aux-ledger:with-task
          "title"
        (aux-ledger-side-call
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (nodecode-aux-ledger::observe-provider-call
            (aux-ledger-provider-answer "titled" nil) :context))
         "the instruction" "the message"))
      (is (equal "title" (getf (car (last (aux-ledger-rows file))) :task)) "WITH-TASK names the rows its body leaves"))))

;;; --- /aux ----------------------------------------------------------------------

(defun aux-ledger-sample (file)
  "A ledger FILE holding one titling call and one failed note."
  (aux-ledger-write
   file
   (aux-ledger-a-row :task "title" :provider "alpha" :model "m1" :duration-s 1.1
                     :usage-p t :tokens '(:input-tokens 321 :output-tokens 12))
   (aux-ledger-a-row :task "note" :provider "beta" :model "m2" :error-type "provider-error")
   (aux-ledger-a-row :task "note" :provider "beta" :model "m2" :duration-s 0.2
                     :usage-p t :tokens '(:input-tokens 1000 :output-tokens 50))))

(deftest aux-ledger-cell-the-command-reads-the-ledger-and-clears-it ()
  (let ((file (aux-ledger-scratch-file)))
    (is (= 3 (aux-ledger-sample file)) "three rows written")
    (with-cell-stop ((aux-ledger-start "file" (uiop:native-namestring file)))
      (let ((summary (cell-entry "nodecode-aux-ledger" "aux" "all providers" nil)))
        (is (search "total" summary) "a summary ends in a total line")
        (is (search "alpha" summary) "each provider is a row")
        (is (search "1,321" summary) "input is the provider's whole read, separated by thousands")
        (is (search "1" summary) "failures are counted"))
      (let ((recent (cell-entry "nodecode-aux-ledger" "aux" "recent 2" nil)))
        (is (search "newest 2 of 3" recent) "recent N lists the newest rows instead")
        (is (search "m2" recent) "newest first"))
      (is (search "Unknown option: 9z" (cell-entry "nodecode-aux-ledger" "aux" "9z" nil)) "an option the ledger does not know says so")
      (is (search "deleted 3" (cell-entry "nodecode-aux-ledger" "aux" "clear" nil)) "clear answers how many rows it dropped")
      (is (null (probe-file file)) "and the file is gone"))))

(deftest aux-ledger-cell-the-tool-answers-the-same-report ()
  (let ((file (aux-ledger-scratch-file)))
    (aux-ledger-sample file)
    (with-cell-stop ((aux-ledger-start "file" (uiop:native-namestring file)))
      (let* ((contract (nlk:contract 'nodecode-aux-ledger::aux-ledger-query))
             (call (nlk:contract-decode
                    contract
                    (cell-json "{\"window\":\"all\",\"group_by\":\"model\",\"recent\":0}")))
             (answer (nodecode-aux-ledger::query-tool call)))
        (is (search "model" answer) "the tool groups the way it was asked")
        (is (search "m1" answer) "and names the models that ran")
        (is (search "m2" answer)))
      (let* ((contract (nlk:contract 'nodecode-aux-ledger::aux-ledger-query))
             (call (nlk:contract-decode contract (cell-json "{\"recent\":2}")))
             (answer (nodecode-aux-ledger::query-tool call)))
        (is (search "newest 2 of 3" answer) "a recent count lists the newest calls")))))
