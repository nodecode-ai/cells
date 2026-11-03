;;;; repro-test.lisp --- the repro: a failed turn becomes a sealed, locked test.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A temp store holds a real failed turn; the repro reads it back, writes
;;;; the test file, and maps the test's exit to the finding. The test run is
;;;; stubbed through *RUN-TEST*: the replay itself (a sealed child organism)
;;;; is the selffix spec's to drive end to end.

(in-package #:nodecode.test)

(defun selffix-failed-turn (input fault)
  "A failed turn in the temp store => its evidence occurrence."
  (nlk:create-session :id "s-work")
  (let ((turn (nlk:admit-turn "s-work" (nlk::make-durable-id "command") input)))
    (nlk:fail-turn turn (make-condition 'simple-error :format-control fault))))

(defun selffix-turn-of-failure ()
  (second (first (nlk:events :kind "turn.failed" :as :rows :columns '("session_id" "turn_id")))))

(defun selffix-test-json (path)
  "The recorded turn a test file at PATH carries."
  (let* ((text (uiop:read-file-string path))
         (start (+ (search "<<'SELFFIX_REPLAY_JSON'" text) (length "<<'SELFFIX_REPLAY_JSON'") 1))
         (end (search (format nil "~%SELFFIX_REPLAY_JSON~%") text :start2 start)))
    (nlk:decode-json (sb-ext:octets-to-string
                      (cl-base64:base64-string-to-usb8-array (remove #\Newline (subseq text start end)))
                      :external-format :utf-8))))

(defun selffix-turn-item (input fault)
  (selffix-failed-turn input fault)
  (list :source :turn-failed :key (format nil "turn-failed SIMPLE-ERROR: ~a" fault) :count 2
        :last "2026-11-03T01:00:00Z"
        :evidence (list (list :at "2026-11-03T01:00:00Z" :session "s-work"
                              :turn (selffix-turn-of-failure) :text fault))))

(deftest selffix-cell-repro-reads-the-turn ()
  (with-temp-store ()
    (selffix-turn-item "do the café thing, twice" "planted fault 7")
    (let ((replay (nodecode-selffix::turn-replay "s-work" (selffix-turn-of-failure))))
      (is (equal "do the café thing, twice" (getf replay :input)) "the input as the person gave it")
      (is (equal "SIMPLE-ERROR" (getf replay :type)))
      (is (equal "planted fault 7" (getf replay :detail)))
      (is (null (getf replay :messages)) "a turn that failed before the model answered replays no answer"))))

(deftest selffix-cell-repro-writes-a-sealed-test ()
  (with-temp-store ()
    (with-selffix-root ()
      (let* ((item (selffix-turn-item "do the café thing, twice" "planted fault 7"))
             (ran (list nil))
             (nodecode-selffix::*run-test* (lambda (path) (setf (car ran) path) (values 1 "REPRODUCED: the turn fails as recorded")))
             (result (nodecode-selffix::repro item :home (nodecode-selffix::selffix-path "scratch/x/"))))
        (is (getf result :fails) "exit 1 is a test that fails as recorded")
        (is (equal (car ran) (getf result :test)) "the file that ran is the file answered")
        (is (equal (getf result :sha256) (nodecode-selffix::file-sha256 (getf result :test))) "the hash is the file's")
        (let ((text (uiop:read-file-string (getf result :test))))
          (dolist (flag '("--unshare-all" "--clearenv" "--ro-bind / /" "--tmpfs /home" "--tmpfs /tmp" "--die-with-parent"))
            (is (search flag text) (format nil "the sandbox carries ~a" flag)))
          (is (search "def normalize" text) "replay.py rides inside the file"))
        (let ((json (selffix-test-json (getf result :test))))
          (is (equal "do the café thing, twice" (nlk:json-value json :string "input")) "the input survives the file, UTF-8 included")
          (is (equal "SIMPLE-ERROR" (nlk:json-value json :string "expect" "condition_type")))
          (let ((normal (nlk:json-value json :string "expect" "detail_normal")))
            (is (equal "planted fault N" normal) "the expected detail is normalized as the backlog normalizes")))))))

(deftest selffix-cell-repro-exit-codes ()
  (with-temp-store ()
    (with-selffix-root ()
      (let ((item (selffix-turn-item "go" "planted fault 8")))
        (let* ((nodecode-selffix::*run-test* (lambda (path) (declare (ignore path)) (values 0 "NOT REPRODUCED")))
               (result (nodecode-selffix::repro item :home (nodecode-selffix::selffix-path "scratch/y/"))))
          (is (null (getf result :fails)) "exit 0: the failure did not happen again")
          (is (getf result :test) "a test that passes is still answered, for the ledger"))
        (let ((nodecode-selffix::*run-test* (lambda (path) (declare (ignore path)) (values 2 "could not run: bwrap is missing"))))
          (let ((refused (signals-error error (nodecode-selffix::repro item :home (nodecode-selffix::selffix-path "scratch/z/")))))
            (is refused "exit 2 is a cycle that failed, never a finding")))))))

(deftest selffix-cell-repro-other-sources-do-not-replay ()
  (with-temp-store ()
    (let ((nodecode-selffix::*run-test* (lambda (path) (error "ran ~a" path))))
      (let ((result (nodecode-selffix::repro (list :source :qa :key "qa eval wrong_output" :evidence (list (list :text "x")))
                                             :home "/nonexistent/")))
        (is (null (getf result :fails)))
        (is (null (getf result :test)) "no test is written")
        (is (search "not replayable" (getf result :output))))
      (let ((result (nodecode-selffix::repro (list :source :turn-failed :key "turn-failed X: y"
                                                   :evidence (list (list :session "s-gone" :turn "turn-gone")))
                                             :home "/nonexistent/")))
        (is (null (getf result :fails)) "a turn the store no longer holds is not replayable")
        (is (search "no longer holds" (getf result :output)))))))
