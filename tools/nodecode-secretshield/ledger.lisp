;;;; ledger.lisp --- an append-only incident ledger, metadata only.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; <home>/secretshield/incidents.jsonl, one JSON object a line: when, on
;;;; which surface, which tool, the kinds found with their lengths and masked
;;;; previews, and the action taken. The secret value itself is never written
;;;; anywhere. The original kept its ledger under the harness's own folder; in
;;;; Nodecode that folder is the organism's home, so a profile carries its own.

(in-package #:nodecode-secretshield)

(defun incident-path ()
  "The ledger file: <home>/secretshield/incidents.jsonl."
  (nlk:home "secretshield/incidents.jsonl"))

(defun json-object* (&rest pairs)
  "A JSON object from alternating KEY VALUE pairs."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (when key (setf (gethash key object) value)))
    object))

(defun findings-json (findings)
  "FINDINGS as a JSON array of objects, each stripped of every span."
  (coerce (mapcar (lambda (finding)
                    (json-object* "kind" (finding-kind finding)
                                  "length" (finding-length finding)
                                  "preview" (finding-preview finding)
                                  "path" (or (finding-path finding) "")))
                  findings)
          'vector))

(defun record-incident (surface action findings &key tool session)
  "Append one incident: metadata only, and a ledger that cannot be written
never takes the call down with it."
  (ignore-errors
    (nlk:append-json-line
     (incident-path)
     (json-object* "ts" (get-universal-time)
                   "surface" surface
                   "action" action
                   "tool" (or tool "")
                   "session" (or session "")
                   "findings" (findings-json findings)))))

(defun recent-incidents (&optional (limit 100))
  "The newest LIMIT incidents, oldest first."
  (let ((rows (nlk:read-json-lines (incident-path))))
    (if (> (length rows) limit) (last rows limit) rows)))

(defun incident-field (row key)
  "ROW's KEY as a string, or NIL."
  (let ((value (and (hash-table-p row) (gethash key row))))
    (and value (stringp value) (plusp (length value)) value)))

(defun incident-kinds (row)
  "The kinds ROW's findings name, as one comma-separated line."
  (let ((kinds '()))
    (map nil (lambda (finding)
               (let ((kind (and (hash-table-p finding) (gethash "kind" finding))))
                 (when (stringp kind) (pushnew kind kinds :test #'string=))))
         (nlk:json-array row "findings"))
    (if kinds (format nil "~{~a~^, ~}" (nreverse kinds)) "-")))

(defun incident-text (&optional (limit 100))
  "The ledger as a person reads it: the counts, then the newest five."
  (let ((rows (recent-incidents limit)))
    (if (null rows)
        "The SecretShield ledger is empty: no credential has been detected yet."
        (let ((counts (make-hash-table :test #'equal)))
          (dolist (row rows)
            (let ((action (or (incident-field row "action") "?")))
              (setf (gethash action counts) (1+ (gethash action counts 0)))))
          (with-output-to-string (out)
            (format out "SecretShield incidents (most recent last):")
            (format out "~%Counts: ~{~a=~d~^, ~}"
                    (loop for key being the hash-keys of counts using (hash-value n)
                          append (list key n)))
            (dolist (row (last rows (min 5 (length rows))))
              (format out "~% - ~a on ~a | ~a~@[ | tool=~a~]~@[ | session=~a~]"
                      (or (incident-field row "action") "?")
                      (or (incident-field row "surface") "?")
                      (incident-kinds row)
                      (incident-field row "tool")
                      (incident-field row "session"))))))))
