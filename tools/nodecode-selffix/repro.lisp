;;;; repro.lisp --- prove an item: a test that fails today.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; (REPRO ITEM &key HOME) answers (:TEST PATH :SHA256 HEX :FAILS BOOL
;;;; :OUTPUT STRING). For a failed turn the test is one self-contained shell
;;;; file, written under HOME: it replays the recorded turn (the input, then
;;;; the model's recorded answers in order, from a loopback provider) in a
;;;; throwaway home inside a bwrap sandbox -- read-only filesystem, /home and
;;;; /tmp hidden, no environment, no network -- and exits 1 when the turn
;;;; fails again with the recorded condition type and normalized detail, 0
;;;; when it does not, 2 when it could not run. The file carries everything
;;;; it needs (replay.py and the recorded turn), so the hash the ledger keeps
;;;; covers the whole test, and a fix is checked against it unchanged.
;;;;
;;;; Only turns replay. An item from another source, or a turn the store no
;;;; longer holds, answers :FAILS NIL with the reason, never a test.

(in-package #:nodecode-selffix)

(defparameter *repro-seconds* 900
  "The most a test may run, boot and turn included; past it the test is killed.")

(defvar *run-test* 'run-test
  "Runs a test file => (values EXIT-CODE OUTPUT); a test binds a stub.")

(defun cell-file (name)
  (asdf:system-relative-pathname "nodecode-selffix" name))

(defun payload (row)
  (ignore-errors (nlk:decode-json (first row))))

(defun turn-facts (session turn kind)
  "The payloads of TURN's facts of KIND, oldest first."
  (remove nil (mapcar #'payload (nlk:events :kind kind :session-id session :turn-id turn
                                            :as :rows :columns '("payload")))))

(defun json-text (object &rest path)
  (ignore-errors (apply #'nlk:json-value object :string path)))

(defun replay-message (fact)
  "The model's answer a recorded assistant message carries, without its reasoning."
  (let ((message (ignore-errors (nlk:json-value fact :object "message"))))
    (when message
      (let ((calls (ignore-errors (nlk:json-value message :array "tool_calls"))))
        (nlk:json-object "role" "assistant"
                         "content" (or (json-text message "content") "")
                         "tool_calls" (or calls #()))))))

(defun turn-replay (session turn)
  "What replaying TURN needs => (:INPUT :MESSAGES :TYPE :DETAIL), or NIL when
the store no longer holds its input or its failure."
  (let ((started (first (turn-facts session turn "turn.started")))
        (failed (first (turn-facts session turn "turn.failed"))))
    (when (and started failed (json-text started "input"))
      (list :input (json-text started "input")
            :messages (remove nil (mapcar #'replay-message
                                          (turn-facts session turn "turn.assistant_message_completed")))
            :type (or (json-text failed "condition-type") "ERROR")
            :detail (or (json-text failed "detail") "")))))

(defun replayable-evidence (item)
  "ITEM's newest occurrence that names a turn the store can replay => (values
OCCURRENCE REPLAY), or NIL."
  (dolist (occurrence (getf item :evidence))
    (when (and (listp occurrence) (getf occurrence :session) (getf occurrence :turn))
      (nlk:when-let (replay (turn-replay (getf occurrence :session) (getf occurrence :turn)))
        (return (values occurrence replay))))))

(defun base64-lines (text)
  (let ((encoded (cl-base64:usb8-array-to-base64-string
                  (sb-ext:string-to-octets text :external-format :utf-8))))
    (with-output-to-string (out)
      (loop for start from 0 below (length encoded) by 76
            do (write-line (subseq encoded start (min (length encoded) (+ start 76))) out)))))

(defun shell-quote (text)
  (format nil "'~a'" (cl-ppcre:regex-replace-all "'" text "'\\\\''")))

(defun comment-lines (text)
  (format nil "~{# ~a~^~%~}" (uiop:split-string (nlk:one-line text :cap 400) :separator '(#\Newline))))

(defun fill-template (template &rest pairs)
  (loop for (key value) on pairs by #'cddr
        do (setf template (cl-ppcre:regex-replace-all (cl-ppcre:quote-meta-chars key) template
                                                      (lambda (&rest _) (declare (ignore _)) value))))
  template)

(defun write-test (path item occurrence replay)
  "Write the self-contained test for ITEM's REPLAY at PATH."
  (let ((json (nlk:encode-json-object
               (nlk:json-object "session" (getf occurrence :session) "turn" (getf occurrence :turn)
                                "input" (getf replay :input) "messages" (coerce (getf replay :messages) 'vector)
                                "expect" (nlk:json-object "condition_type" (getf replay :type)
                                                          "detail" (nlk:one-line (getf replay :detail) :cap 300)
                                                          "detail_normal" (normalize-text (getf replay :detail)))))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string
       (fill-template (uiop:read-file-string (cell-file "repro.sh.in"))
                      "@HEADER@" (format nil "~a~%~a"
                                         (comment-lines (format nil "Recorded failure: ~a: ~a" (getf replay :type) (getf replay :detail)))
                                         (comment-lines (format nil "Turn ~a of session ~a, ~d time~:p in the backlog~@[, last ~a~]."
                                                                (getf occurrence :turn) (getf occurrence :session)
                                                                (or (getf item :count) 1) (getf item :last))))
                      "@FOUND_HOME@" (shell-quote (string-right-trim "/" (namestring (nlk:home ""))))
                      "@REPLAY_JSON@" (string-right-trim '(#\Newline) (base64-lines json))
                      "@REPLAY_PY@" (string-right-trim '(#\Newline) (uiop:read-file-string (cell-file "replay.py"))))
       out))
    path))

(defun run-test (path)
  "Run the test at PATH => (values EXIT-CODE OUTPUT)."
  (multiple-value-bind (output error-output code)
      (uiop:run-program (list "timeout" "-k" "10" (princ-to-string *repro-seconds*) "sh" (namestring path))
                        :output :string :error-output :output :ignore-error-status t)
    (declare (ignore error-output))
    (values code output)))

(defun repro (item &key home)
  "Prove ITEM with a test that fails today => (:TEST PATH :SHA256 HEX :FAILS
BOOL :OUTPUT STRING). The test is written under HOME; it makes its own
throwaway home when it runs and removes it after."
  (multiple-value-bind (occurrence replay) (replayable-evidence item)
    (unless replay
      (return-from repro
        (list :test nil :sha256 nil :fails nil
              :output (if (eq (getf item :source) :turn-failed)
                          "not replayable: the store no longer holds the failed turn's input and failure"
                          (format nil "not replayable: a ~(~a~) item names no failed turn to replay"
                                  (getf item :source))))))
    (let ((test (write-test (merge-pathnames "repro.sh" (uiop:ensure-directory-pathname home))
                            item occurrence replay)))
      (multiple-value-bind (code output) (funcall *run-test* test)
        (case code
          (1 (list :test test :sha256 (file-sha256 test) :fails t :output output))
          (0 (list :test test :sha256 (file-sha256 test) :fails nil :output output))
          (t (fail "the test could not run (exit ~a): ~a" code
                   (nlk:one-line (princ-to-string output) :cap 400))))))))
