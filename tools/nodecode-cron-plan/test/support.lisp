;;;; support.lisp --- the slice runner, and a cron folder a test writes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; These tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a CRON-PLAN-CELL-
;;;; name prefix; RUN-CRON-PLAN-TESTS runs exactly that slice. Nothing of the
;;;; cell is stubbed and nothing needs a stub: the cell reads files, and a
;;;; Hermes-shaped jobs.json is a file a test writes, so every test here is
;;;; offline and deterministic.

(in-package #:nodecode.test)

(define-test-slice "cron-plan" "CRON-PLAN-CELL-" :start cron-plan:start-cell)

(defvar *fixture-number* 0
  "How many cron folders this slice has written; each test gets its own.")

(defun fixture-cron (&optional (tag "fixture"))
  "An empty cron folder this test owns, under the scratch home, as a namestring."
  (let ((dir (namestring (nlk:home (format nil "cron-plan-~a-~d/cron/"
                                           tag (incf *fixture-number*))))))
    (ensure-directories-exist (format nil "~a" dir))
    (string-right-trim "/" dir)))

(defun fixture-home (&optional (tag "fixture"))
  "The home around a fresh cron folder: what the cell's `home' setting names."
  (nodecode-cron-plan::directory-of (fixture-cron tag)))

(defun utc-iso (universal)
  "UNIVERSAL as the ISO-8601 stamp usage_audit.jsonl writes."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun write-json-file (path value)
  "VALUE as compact JSON at PATH."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out "~a" (nlk:encode-json-object value))))

(defun job-row (id name prompt expression &key skills deliver (enabled t) streak error
                                        unverified script (created "2026-01-01T00:00:00Z")
                                        (kind "cron"))
  "One jobs.json row, the shape a Hermes cron registry writes."
  (nlk:json-object
   "id" id
   "name" name
   "prompt" prompt
   "schedule" (nlk:json-object "kind" kind "expr" (if (string= kind "cron") expression ""))
   "schedule_display" expression
   "skills" (coerce (or skills '()) 'vector)
   "deliver" (or deliver "local")
   "enabled" (and enabled t)
   "failure_streak" (or streak 0)
   "last_error" (or error "")
   "last_delivery_unverified" (and unverified t)
   "script" (or script "")
   "created_at" created))

(defun usage-row (id prompt-tokens universal &key (completion-tokens 100))
  "One usage_audit.jsonl record: one fire of ID at UNIVERSAL."
  (nlk:json-object "ts" (utc-iso universal) "job_id" id
                    "prompt_tokens" prompt-tokens
                    "completion_tokens" completion-tokens
                    "response_silent" nil))

(defun write-cron (jobs &key audit)
  "A home whose cron/ folder holds JOBS rows and AUDIT records; answers the home."
  (let* ((cron (fixture-cron))
         (home (nodecode-cron-plan::directory-of cron)))
    (write-json-file (format nil "~a/jobs.json" cron)
                     (nlk:json-object "jobs" (coerce jobs 'vector)))
    (when audit
      (with-open-file (out (format nil "~a/usage_audit.jsonl" cron)
                            :direction :output :if-exists :supersede)
        (dolist (record audit)
          (format out "~a~%" (nlk:encode-json-object record)))))
    home))

(defmacro with-consolidator (home &body body)
  "BODY with the cell started over HOME, and stopped on unwind."
  `(with-cell-stop ((cron-plan-start "home" ,home "days" 14))
     ,@body))
