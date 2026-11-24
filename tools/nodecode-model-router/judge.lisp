;;;; judge.lisp --- the optional remote judge, and the task it is allowed to see.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ranking is local and stays local. What the original's provider=jev did
;;;; on top of it was ask a second opinion: POST the task and the candidate
;;;; rows to a decision endpoint, take back a score a row, and blend it in.
;;;; Here that is one optional member of the section (judge_url), the variable
;;;; holding its key (judge_key_env) and the weight its opinion carries.
;;;;
;;;; The judge never sees the task as it was typed: known shapes of secret are
;;;; replaced first, exactly as the original's redaction.py did, and only the
;;;; two are sent - the task's text and the rows' numbers. No key, no vendor
;;;; address, nothing else.

(in-package #:nodecode-model-router)

;;; --- what a judge may see ---------------------------------------------------------

(defparameter *redactions*
  (list (cons (ppcre:create-scanner
               "-----BEGIN [A-Z0-9 ]*PRIVATE KEY-----.*?-----END [A-Z0-9 ]*PRIVATE KEY-----"
               :single-line-mode t)
              "[REDACTED_PRIVATE_KEY]")
        (cons (ppcre:create-scanner "sk-ant-[A-Za-z0-9_-]+") "[REDACTED_API_KEY]")
        (cons (ppcre:create-scanner "sk-[A-Za-z0-9]{20,}") "[REDACTED_API_KEY]")
        (cons (ppcre:create-scanner "jev_[A-Za-z0-9_-]{8,}") "[REDACTED_API_KEY]")
        (cons (ppcre:create-scanner "Bearer\\s+[A-Za-z0-9._\\-+=/]+") "[REDACTED_BEARER]")
        (cons (ppcre:create-scanner "(?i)(password|passwd|secret)\\s*[:=]\\s*\\S+")
              "[REDACTED_PASSWORD]"))
  "The shapes of secret the original masked before any remote call, and the
marker each becomes.")

(defun redact (text)
  "TEXT with every known shape of secret replaced by a stable marker."
  (let ((redacted (or text "")))
    (dolist (rule *redactions* redacted)
      (setf redacted (ppcre:regex-replace-all (car rule) redacted (cdr rule))))))

(defun judge-payload (task entries)
  "The one object a judge is sent: the redacted task, and the rows it ranks."
  (nlk:make-json-object
   "task" (redact task)
   "models" (coerce (loop for entry in entries
                          collect (nlk:make-json-object
                                   "id" (entry-id entry)
                                   "name" (entry-name entry)
                                   "provider" (entry-provider entry)
                                   "model" (entry-model entry)
                                   "quality" (entry-quality entry)
                                   "cost" (entry-cost entry)
                                   "latency" (entry-latency entry)
                                   "capabilities" (coerce (entry-capabilities entry) 'vector)))
                  'vector)))

(defun parse-judge-scores (document)
  "The scores a judge answered: {\"scores\": [{id, score}]}, a bare array, or
the list under one of the keys the original also accepted."
  (let ((rows (if (vectorp document)
                  document
                  (or (nlk:json-value document :array "scores")
                      (nlk:json-value document :array "selected")
                      (nlk:json-value document :array "models")))))
    (unless rows (fail "judge answered no scores: expected an array under scores"))
    (let ((scores '()))
      (loop for row across rows
            for id = (nlk:json-value row :string "id")
            for raw = (or (nlk:json-value row :number "score")
                          (nlk:json-value row :number "confidence"))
            when (and id raw)
              do (push (cons id (min 1.0 (max 0.0 (coerce raw 'single-float)))) scores))
      (when (null scores) (fail "judge scored none of the models it was sent"))
      scores)))

;;; --- the judge itself --------------------------------------------------------------

(defun http-judge (url key-env task entries timeout)
  "One POST to a command-line-decisions endpoint, as the original's JevClient
sent it. => the scores, or MODEL-ROUTER-ERROR naming what the peer said."
  (let* ((key (and key-env (env-set-p key-env) (uiop:getenv key-env)))
         (headers (if key (list (cons "Authorization" (format nil "Bearer ~a" key))) '())))
    (multiple-value-bind (body status)
        (handler-case (nlk:http :post url :headers headers
                                     :content (nlk:encode-json-object
                                               (judge-payload task entries))
                                     :timeout timeout)
          (error (condition) (fail "judge unreachable at ~a: ~a" url condition)))
      (unless (and (integerp status) (<= 200 status 299))
        (fail "judge answered HTTP ~a at ~a" status url))
      (parse-judge-scores (handler-case (nlk:decode-json (princ-to-string body))
                            (error (condition)
                              (fail "judge answered something that is not JSON: ~a"
                                    condition)))))))

(defvar *judge* #'http-judge
  "The one seam out of this cell: a function of (URL KEY-ENV TASK ENTRIES TIMEOUT)
answering an alist of (ID . SCORE). A test replaces it; nothing else does.")

(defun judge-verdict (url key-env task entries timeout)
  "The judge's scores, or (values NIL NOTE) with the line a decision carries
when the local ranking stood alone."
  (handler-case (values (funcall *judge* url key-env task entries timeout) nil)
    (model-router-error (condition)
      (values nil (format nil "judge_unavailable: ~a" (model-router-error-detail condition))))))
