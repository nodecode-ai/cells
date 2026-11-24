;;;; support.lisp --- the slice runner, a fixture home, and the tool answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Doctorpack tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a DOCTORPACK-CELL-
;;;; name prefix; RUN-DOCTORPACK-TESTS runs exactly that slice.
;;;;
;;;; The cell's I/O is the file system, so the stub is a home: every test that
;;;; reads builds a fresh folder under the scratch home, plants a config, a
;;;; cells folder and log lines in it, and points the section's `home' at it.
;;;; Nothing reads or writes the operator's own home.

(in-package #:nodecode.test)

(define-test-slice "doctorpack" "DOCTORPACK-CELL-" :start nodecode-doctorpack:start-cell)

(defvar *fixture-count* 0
  "How many fixture homes this run has made, so two never share one.")

(defun doctorpack-stamp (&optional (seconds-ago 0))
  "An ISO-8601 UTC stamp SECONDS-AGO seconds before now."
  (nlk:iso-time (- (get-universal-time) 2208988800 seconds-ago)))

(defun doctorpack-fixture-home (&rest files)
  "A fresh folder under the scratch home holding FILES ((RELATIVE . TEXT) ...):
(doctorpack-fixture-home '(\"config.jsonc\" . \"{}\")). => its namestring."
  (let ((root (nlk:home (format nil "doctorpack-test/~d-~d/"
                                 (get-universal-time) (incf *fixture-count*)))))
    (ensure-directories-exist (merge-pathnames "keep" root))
    (dolist (file files)
      (nlk:write-file-atomically (merge-pathnames (car file) root) (cdr file)))
    (namestring root)))

(defun doctorpack-raw (name &rest pairs)
  "NAME called through the wire, as a provider sends it: the answer's text."
  (let ((call (nle::execute-tool-call
               (wire-tool-call "call-1" name (apply #'nlk:make-json-object pairs))
               (compiled-context '()))))
    (princ-to-string (nle::message-content call))))

(defun doctorpack-answer (name &rest pairs)
  "NAME's answer, decoded: a JSON object of findings and counts."
  (nlk:decode-json (apply #'doctorpack-raw name pairs)))

(defun doctorpack-find (findings id)
  "The finding ID among FINDINGS, an array, or NIL."
  (loop for finding across findings
        when (equal id (nlk:json-value finding :string "id"))
          return finding))

(defun doctorpack-finding (payload id)
  "The finding ID in PAYLOAD, or NIL when the audit did not make one."
  (doctorpack-find (nlk:json-array payload "findings") id))

(defun doctorpack-status (payload id)
  "The status of finding ID, or NIL."
  (nlk:json-value (doctorpack-finding payload id) :string "status"))

(defun doctorpack-config-text ()
  "A config.jsonc with two sections and one credential pasted inline."
  (format nil "{~%  \"providers\": {~%    \"openai\": {\"api_key\": \"sk-abcdefghijklmnopqrstuvwxyz012345\"}~%  },~%  \"cells\": {},~%  \"doctorpack\": {\"lookback_hours\": 24}~%}~%"))

(defun doctorpack-log-text ()
  "A log tail: one plain line, one cell load error, one warning."
  (format nil "~a INFO started~%~a ERROR cell nodecode-example failed to load: asdf could not find it~%~a WARNING retrying the gateway~%"
          (doctorpack-stamp 600) (doctorpack-stamp 300) (doctorpack-stamp 120)))

(defun doctorpack-old-log-text ()
  "One error line old enough that a 24 hour lookback does not count it."
  (format nil "~a ERROR cell stale failed an hour and a week ago~%" (doctorpack-stamp (* 8 24 3600))))
