;;;; json.lisp --- one JSON writer, and the readers over shasht's tables.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A receipt is a document with an order and a digest over it, so it is
;;;; written by one small writer here rather than through a hash table whose
;;;; order no one chose: (JOBJ "id" "a" "state" "passed") is an object,
;;;; (JARR list) an array, +NULL+ null, T true, :FALSE false, and (JRAW
;;;; text) a document spliced in whole. DECODED-JSON-TEXT writes a value
;;;; NLK:DECODE-JSON produced (hash tables, vectors) with its keys sorted,
;;;; which is what an export reads back.

(in-package #:nodecode-receipts)

(defparameter +null+ :null
  "JSON null, as the writer takes it.")

(defun jobj (&rest pairs)
  "A JSON object of PAIRS, alternating key and value, in this order."
  (cons :object pairs))

(defun jarr (list)
  "A JSON array of LIST, in this order."
  (cons :array (coerce list 'list)))

(defun jraw (text)
  "TEXT, already JSON, spliced into the document as it stands."
  (cons :raw text))

(defun emit-json-string (text stream)
  "TEXT as a JSON string on STREAM: the two quotes and the controls escaped."
  (write-char #\" stream)
  (loop for character across text
        for code = (char-code character)
        do (cond ((char= character #\") (write-string "\\\"" stream))
                 ((char= character #\\) (write-string "\\\\" stream))
                 ((< code 32)
                  (format stream "\\u~4,'0x" code))
                 (t (write-char character stream))))
  (write-char #\" stream))

(defun emit-json (value stream)
  "VALUE on STREAM as JSON, in the shape JOBJ, JARR and JRAW give it."
  (cond ((eq value +null+) (write-string "null" stream))
        ((eq value t) (write-string "true" stream))
        ((eq value :false) (write-string "false" stream))
        ((stringp value) (emit-json-string value stream))
        ((integerp value) (format stream "~d" value))
        ((and (consp value) (eq (car value) :raw)) (write-string (cdr value) stream))
        ((and (consp value) (eq (car value) :array))
         (write-char #\[ stream)
         (loop for (item . rest) on (cdr value)
               do (emit-json item stream)
               when rest do (write-char #\, stream))
         (write-char #\] stream))
        ((and (consp value) (eq (car value) :object))
         (write-char #\{ stream)
         (loop for (key item . rest) on (cdr value) by #'cddr
               do (emit-json-string key stream)
                  (write-char #\: stream)
                  (emit-json item stream)
               when rest do (write-char #\, stream))
         (write-char #\} stream))
        (t (error "not a JSON value: ~s" value))))

(defun json-text (value)
  "VALUE as one JSON document."
  (with-output-to-string (stream) (emit-json value stream)))

;;; --- what NLK:DECODE-JSON answers, written back --------------------------------

(defun emit-decoded (value stream)
  "VALUE -- a hash table, a vector, a string, a number -- on STREAM as JSON,
objects with their keys sorted so one receipt always exports the same bytes."
  (cond ((hash-table-p value)
         (write-char #\{ stream)
         (let ((first t))
           (dolist (key (sort (loop for key being the hash-keys of value collect key)
                              #'string<))
             (unless first (write-char #\, stream))
             (setf first nil)
             (emit-json-string key stream)
             (write-char #\: stream)
             (emit-decoded (gethash key value) stream)))
         (write-char #\} stream))
        ((stringp value) (emit-json-string value stream))
        ((integerp value) (format stream "~d" value))
        ((eq value t) (write-string "true" stream))
        ((null value) (write-string "null" stream))
        ((vectorp value)
         (write-char #\[ stream)
         (loop for index below (length value)
               do (when (plusp index) (write-char #\, stream))
                  (emit-decoded (aref value index) stream))
         (write-char #\] stream))
        ((consp value)
         (write-char #\[ stream)
         (loop for (item . rest) on value
               do (emit-decoded item stream)
               when rest do (write-char #\, stream))
         (write-char #\] stream))
        (t (error "not a decoded JSON value: ~s" value))))

(defun decoded-json-text (value)
  "VALUE as one JSON document, keys in order: the export of a receipt read back."
  (with-output-to-string (stream) (emit-decoded value stream)))

;;; --- reading the tables DECODE-JSON answers -------------------------------------

(defun jget (object key)
  "KEY's value in OBJECT, a hash table of string keys, or NIL."
  (and (hash-table-p object) (gethash key object)))

(defun jstring (object key)
  "KEY's value in OBJECT when it is a string, else NIL."
  (let ((value (jget object key))) (and (stringp value) value)))

(defun jinteger (object key)
  "KEY's value in OBJECT when it is an integer, else NIL."
  (let ((value (jget object key))) (and (integerp value) value)))

(defun jboolean (object key)
  "KEY's value in OBJECT as a boolean: JSON false and an absent key both read NIL."
  (and (jget object key) t))

(defun jitems (value)
  "VALUE as a list of its elements when it is an array, as shasht or the
contract decodes one, else NIL."
  (cond ((vectorp value) (coerce value 'list))
        ((and (listp value) value) (copy-list value))
        (t nil)))

(defun copy-table (table)
  "A shallow copy of TABLE, a hash table: one export's own view of a receipt."
  (let ((copy (make-hash-table :test (hash-table-test table) :size (hash-table-size table))))
    (maphash (lambda (key value) (setf (gethash key copy) value)) table)
    copy))

(defun jkeys (object)
  "OBJECT's keys, sorted, or NIL when it is not an object."
  (and (hash-table-p object)
       (sort (loop for key being the hash-keys of object collect key) #'string<)))
