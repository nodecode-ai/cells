;;;; bridge.lisp --- the child that speaks to X.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; X has no bot API a cookie opens: the web client signs each request with
;;;; a transaction id derived from x.com's own page and script, which
;;;; tweetkit-x (https://github.com/nsozturk/tweetkit-x, MIT) computes. Rather
;;;; than port that, the cell keeps one child, bridge.py, run under uvx with
;;;; tweetkit-x pinned, and speaks JSON lines to it: one request, one answer,
;;;; in order, under one lock. The session cookie goes down the pipe once, at
;;;; login; it rides no argv, no environment and no answer. A child that dies
;;;; is started and logged in again by the next call.

(in-package #:nodecode-channel-x)

(nlk:access (plan request-plan))

(defun bridge-script-path ()
  "bridge.py, beside the system's own source (a compiled file's path would name
its fasl's folder)."
  (or (ignore-errors (asdf:system-relative-pathname "nodecode-channel-x" "bridge.py"))
      (merge-pathnames "bridge.py" (or *load-truename* *default-pathname-defaults*))))

(defparameter +default-bridge-command+
  '("uvx" "--quiet" "--from" "tweetkit-x==0.3.0" "python")
  "The interpreter the bridge runs under, tweetkit-x pinned; the script is appended.")

(defstruct (bridge (:copier nil) (:constructor %make-bridge))
  (argv nil)
  (cookie "" :type string)
  (pid nil)
  (process nil)
  (in nil)
  (out nil)
  (lines nil)
  (reader nil)
  (user-id nil)
  (lock (sb-thread:make-mutex :name "x bridge")))

(nlk:access (bridge bridge))

(defmethod print-object ((bridge bridge) stream)
  ;; never the slots: they hold the session cookie
  (print-unreadable-object (bridge stream :type t :identity t)
    (format stream "pid ~a" (bridge-pid bridge))))

(define-condition bridge-failure (error)
  ((text :initarg :text :reader bridge-failure-text)
   (status :initarg :status :initform 0 :reader bridge-failure-status))
  (:report (lambda (condition stream) (write-string (bridge-failure-text condition) stream))))

(defun bridge-log-path ()
  (nlk:home "channels/x/bridge.log"))

(defun spawn-bridge (bridge)
  "Start BRIDGE's child and the thread that reads its answers."
  (let ((log (bridge-log-path)))
    (ensure-directories-exist log)
    (multiple-value-bind (pid out err process in)
        (nlk:spawn-program (append bridge.argv (list (namestring (bridge-script-path))))
                           :input :stream :output :stream :error-output log)
      (declare (ignore err))
      (let ((lines (sb-concurrency:make-mailbox :name "x bridge lines")))
        (setf bridge.pid pid bridge.process process bridge.in in bridge.out out bridge.lines lines
              bridge.reader (nlk:spawn "channel-x-bridge-reader"
                              (handler-case
                                  (loop for line = (read-line out nil)
                                        while line
                                        do (sb-concurrency:send-message lines line))
                                (error () nil))
                              (sb-concurrency:send-message lines :eof))))))
  bridge)

(defun kill-bridge (bridge)
  "End BRIDGE's child, if it runs."
  (when bridge.pid
    (ignore-errors (close bridge.in :abort t))
    (ignore-errors (nlk:kill-tree bridge.pid))
    (ignore-errors (close bridge.out :abort t))
    (when bridge.process (ignore-errors (sb-ext:process-close bridge.process)))
    (setf bridge.pid nil bridge.process nil bridge.in nil bridge.out nil)))

(defun bridge-exchange (bridge request timeout)
  "One REQUEST down BRIDGE's pipe and its answer back, decoded."
  (handler-case (progn (write-line (nlk:encode-json-object request) bridge.in)
                       (finish-output bridge.in))
    (error ()
      (kill-bridge bridge)
      (error 'bridge-failure :text "the X bridge is not running")))
  (let ((line (sb-concurrency:receive-message bridge.lines :timeout timeout)))
    (cond ((null line)
           (kill-bridge bridge)
           (error 'bridge-failure :text (format nil "the X bridge did not answer within ~as" timeout)))
          ((eq line :eof)
           (kill-bridge bridge)
           (error 'bridge-failure :text (format nil "the X bridge exited; its log is ~a" (bridge-log-path))))
          (t (nlk:decode-json line)))))

(defun bridge-login (bridge)
  "Hand BRIDGE's child the cookie; refuse in X's words when it is not taken."
  (let ((answer (bridge-exchange bridge (nlk:json-object "op" "login" "cookie" bridge.cookie) 90)))
    (unless (nlk:json-value answer :boolean "ok")
      (kill-bridge bridge)
      (error 'bridge-failure :text (or (nlk:json-value answer :text "error") "login refused")
                             :status (or (nlk:json-value answer :integer "status") 0)))
    (setf bridge.user-id (nlk:json-value answer :string "user_id"))
    bridge))

(defun start-bridge (argv cookie)
  "A logged-in bridge: ARGV, the interpreter, runs bridge.py; COOKIE is the
X session it speaks as."
  (let ((bridge (%make-bridge :argv argv :cookie cookie)))
    (sb-thread:with-mutex (bridge.lock)
      (bridge-login (spawn-bridge bridge)))))

(defun stop-bridge (bridge)
  (when bridge
    (sb-thread:with-mutex (bridge.lock)
      (kill-bridge bridge))))

(defun bridge-call (bridge request &key (timeout 60))
  "REQUEST's answer from BRIDGE; a child that died is started and logged in
again first."
  (sb-thread:with-mutex (bridge.lock)
    (unless bridge.pid
      (bridge-login (spawn-bridge bridge)))
    (bridge-exchange bridge request timeout)))

(defun bridge-executor (bridge)
  "The executor that runs BRIDGE plans through BRIDGE's child."
  (make-plan-executor
   :run (lambda (plan)
          (handler-case
              (let* ((answer (bridge-call bridge plan.body :timeout plan.timeout-seconds))
                     (ok (nlk:json-value answer :boolean "ok")))
                (make-execution :ok-p ok :attempts 1 :body answer
                                :status (if ok 200 (or (nlk:json-value answer :integer "status") 0))
                                :error (and (not ok)
                                            (format nil "x ~a failed: ~a" plan.audit-label
                                                    (nlk:json-value answer :text "error")))))
            (bridge-failure (condition)
              (make-execution :ok-p nil :attempts 1 :status (bridge-failure-status condition)
                              :error (format nil "x ~a failed: ~a" plan.audit-label condition)))))))
