;;;; rows.lisp --- a results file read into trials, exactly as gonogo reads it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two shapes of file are read: JSONL, one object per case, which a domain
;;;; expert can build in a spreadsheet; and a report JSON this cell saved,
;;;; whose cases are the per-case record. A row is turned into a trial
;;;; (CONFIDENCE . PASSED) the way gonogo's own evaluate does it -- an agent
;;;; error is a failed case, a plain abstention is a failure at zero
;;;; confidence, every other action keeps its confidence and a hold-back case
;;;; passes exactly when the action matches -- and grouped cases fold to one
;;;; trial per group, passing only if every case in it passed.

(in-package #:nodecode-gonogo)

(defparameter +answer+ "answer"
  "The action that produces an output for the scorer.")

(defparameter +abstain+ "abstain"
  "The one non-answer that is a deferral rather than a decision.")

(defun json-member (object key)
  "MEMBER KEY of decoded JSON OBJECT => (values VALUE PRESENT). JSON null
reads as NIL."
  (if (hash-table-p object)
      (multiple-value-bind (value present) (gethash key object)
        (values (unless (eq value :null) value) present))
      (let ((pair (and (consp object) (assoc key object :test #'equal))))
        (values (and pair (unless (eq (cdr pair) :null) (cdr pair)))
                (and pair t)))))

(defun path-string (path)
  "PATH as the string NLK:EXPAND-HOME reads: a pathname names itself."
  (typecase path
    (pathname (namestring path))
    (string (string-trim '(#\Space #\Tab) path))
    (t (princ-to-string path))))

(defun read-text (path)
  "PATH's whole text, refusing when it is not a file."
  (let* ((name (path-string path))
         (truename (nlk:expand-home name)))
    (unless (probe-file truename)
      (fail "~a is not a file" name))
    (uiop:read-file-string truename)))

(defun lines->rows (text name)
  "TEXT as JSONL rows: blank lines and // comments skipped, a bad line refused."
  (let ((rows '())
        (lineno 0))
    (with-input-from-string (in text)
      (loop for line = (read-line in nil nil)
            while line
            do (incf lineno)
               (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
                 (unless (or (zerop (length trimmed))
                             (uiop:string-prefix-p "//" trimmed))
                   (let ((row (handler-case (nlk:decode-json trimmed)
                                (error (condition)
                                  (fail "~a:~d: invalid JSON: ~a" name lineno condition)))))
                     (unless (hash-table-p row)
                       (fail "~a:~d: each line must be a JSON object" name lineno))
                     (push row rows))))))
    (nreverse rows)))

(defun read-rows (path)
  "PATH as (values ROWS PAYLOAD): ROWS decoded objects, PAYLOAD the saved
report object when the file was one."
  (let* ((text (read-text path))
         (document (handler-case (nlk:decode-json text) (error () nil)))
         (cases (and (hash-table-p document) (json-member document "cases"))))
    (cond ((and (vectorp cases) (plusp (length cases)))
           (values (coerce cases 'list) document))
          (t (values (lines->rows text path) nil)))))

;;; --- one row --------------------------------------------------------------------

(defun row-name (row index)
  "The id of ROW, or a generated one, and whether it was explicit."
  (multiple-value-bind (id present) (json-member row "id")
    (if (and present (stringp id) (plusp (length (string-trim '(#\Space) id))))
        (values id t)
        (values (format nil "case-~d" index) nil))))

(defun row-action (row id)
  "ROW's (values EXPECTED-ACTION ACTION), as gonogo reads them."
  (multiple-value-bind (expected given) (json-member row "expected_action")
    (multiple-value-bind (action acted) (json-member row "action")
      (multiple-value-bind (abstained) (json-member row "abstained")
        (let ((expected (if given expected +answer+))
              (action (cond (acted action)
                            (abstained +abstain+)
                            (t +answer+))))
          (dolist (pair (list (cons "expected_action" expected) (cons "action" action)))
            (unless (and (stringp (cdr pair)) (plusp (length (cdr pair))))
              (fail "case ~s: ~a must be a non-empty string" id (car pair))))
          (values expected action))))))

(defun row->record (row index)
  "ROW as a record: a plist of :ID :GROUP :PASSED :CONFIDENCE :EXPECTED-ACTION
:ACTION :ERROR :DEFERRAL, every refusal named."
  (multiple-value-bind (id explicit) (row-name row index)
    (multiple-value-bind (passed present) (json-member row "passed")
      (unless present
        (fail "case ~s: missing required key 'passed'" id))
      (unless (or (eq passed t) (null passed))
        (fail "case ~s: passed must be a boolean, got ~s" id passed))
      (multiple-value-bind (expected action) (row-action row id)
        (multiple-value-bind (group) (json-member row "group")
          (multiple-value-bind (error) (json-member row "error")
            (multiple-value-bind (confidence supplied) (json-member row "confidence")
              (when supplied
                (unless (and (numberp confidence) (<= 0 confidence 1))
                  (fail "case ~s: confidence must be in [0, 1], got ~s" id confidence)))
              (when (and error (not (stringp error)))
                (setf error (princ-to-string error)))
              (when (and error (plusp (length error)))
                (return-from row->record
                  (list :id id :explicit explicit :group group :passed nil
                        :confidence nil :expected-action expected :action action
                        :error error :deferral nil)))
              (when (and (/= 0 (length expected))
                         (not (string= expected +answer+))
                         (not (eq passed (string= action expected))))
                (fail "case ~s: passed=~a disagrees with expected_action ~s and action ~s; ~
a hold-back case passes exactly when the action matches"
                      id (if passed "true" "false") expected action))
              (when (and (string= expected +answer+) (/= 0 (length action))
                         (not (string= action +answer+)) passed)
                (fail "case ~s: passed=true but action ~s on a case that expects an answer"
                      id action))
              (let ((deferral (and (string= action +abstain+) (string= expected +answer+))))
                (list :id id :explicit explicit :group group
                      :passed (if deferral nil passed)
                      :confidence (if deferral 0d0 (and supplied (coerce confidence 'double-float)))
                      :expected-action expected :action action
                      :error nil :deferral deferral)))))))))

;;; --- rows to trials -------------------------------------------------------------

(defun actions-table (records)
  "The two failures a pass rate hides, when any case uses an action other
than answer or abstain; NIL otherwise."
  (let ((uses nil)
        (acted 0)
        (held 0))
    (dolist (record records)
      (let ((expected (getf record :expected-action))
            (action (getf record :action)))
        (unless (or (string= expected +answer+) (string= expected +abstain+))
          (setf uses t))
        (unless (or (string= action +answer+)
                    (and (string= action +abstain+) (string= expected +answer+)))
          (setf uses t))
        (when (and (not (getf record :error))
                   (not (string= expected +answer+))
                   (string= action +answer+))
          (incf acted))
        (when (and (not (getf record :error))
                   (string= expected +answer+)
                   (not (string= action +answer+)))
          (incf held))))
    (when uses
      (list :acted-when-should-hold-back acted
            :held-back-when-should-act held))))

(defun fold-groups (records)
  "One trial per group, passing only if every case in it passed, at the
group's lowest confidence."
  (let ((groups (make-hash-table :test #'equal))
        (order '()))
    (dolist (record records)
      (let ((key (getf record :group)))
        (unless (gethash key groups)
          (push key order)
          (setf (gethash key groups) (cons 1d0 t)))
        (let ((trial (gethash key groups)))
          (setf (gethash key groups)
                (cons (min (car trial) (or (getf record :confidence) 1d0))
                      (and (cdr trial) (getf record :passed)))))))
    (values (loop for key in (nreverse order)
                  collect (gethash key groups))
            (length order))))

(defun rows->trials (rows)
  "ROWS as the trials a decision is measured on.
=> (values TRIALS UNIT N-GROUPS RECORDS)."
  (let ((records (loop for row in rows
                       for i from 1
                       collect (row->record row i))))
    (let* ((answered (remove-if (lambda (record)
                                  (or (getf record :error) (getf record :deferral)))
                                records))
           (supplied (some (lambda (record) (getf record :confidence)) answered))
           (missing (some (lambda (record) (null (getf record :confidence))) answered)))
      (when (and supplied missing)
        (fail "mixed missing and present confidence; supply confidence for every answered ~
case or none"))
      (dolist (record records)
        (unless (getf record :confidence)
          (setf (getf record :confidence) 1d0))))
    (let* ((grouped (remove-if-not (lambda (record) (getf record :group)) records))
           (unit (cond ((= (length grouped) (length records)) "groups")
                       ((null grouped) "cases")
                       (t (fail "some cases carry a group and some do not; put a group on ~
every case, or on none")))))
      (if (equal unit "groups")
          (multiple-value-bind (trials n-groups) (fold-groups records)
            (values trials "groups" n-groups records))
          (values (mapcar (lambda (record)
                            (cons (getf record :confidence) (getf record :passed)))
                          records)
                  "cases" nil records)))))
