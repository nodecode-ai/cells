;;;; store.lisp --- reviews on disk: one folder a review, one review.json.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A review is a title, a brief, and the rounds put to the operator. A
;;;; round is the decisions it asks, the feedback that came back, and whether
;;;; the model has acknowledged it. The board server reads the same file the
;;;; tools write, so one review is one document and no cache can disagree
;;;; with it.
;;;;
;;;; Everything here is data, not code: a review.json is read with
;;;; NLK:DECODE-JSON and written with NLK:ENCODE-JSON-OBJECT, and every read
;;;; of it is lenient (NLK:JSON-VALUE: a wrong type is NIL) because a
;;;; half-written or older file must not take a cell down.

(in-package #:nodecode-mock-review)

(defparameter +id-length+ 64 "The longest review id a path is built from.")

(defun id-character-p (char)
  (or (alphanumericp char) (char= char #\-)))

(defun cap (text limit)
  (subseq text 0 (min limit (length text))))

(defun clean-line (text limit)
  "TEXT as one line, control characters folded to spaces, at most LIMIT characters."
  (if (stringp text)
      (let ((folded (map 'string (lambda (c) (if (< (char-code c) 32) #\Space c)) text)))
        (cap (format nil "~{~a~^ ~}" (remove "" (uiop:split-string folded :separator '(#\Space #\Tab)))) limit))
      ""))

(defun clean-block (text limit)
  "TEXT with CRLF folded, control characters dropped and runs of blank lines collapsed, at most LIMIT."
  (if (stringp text)
      (let* ((folded (substitute #\Newline #\Return text))
             (kept (map 'string (lambda (c) (if (and (< (char-code c) 32) (/= (char-code c) 10)) #\Space c)) folded))
             (trimmed (string-trim '(#\Space #\Tab #\Newline) kept)))
        (subseq trimmed 0 (min limit (length trimmed))))
      ""))

(defun now-iso (&aux (universal (get-universal-time)))
  "The time as 2026-11-08T09:30:00Z, so a review reads the same in every locale."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" year month day hour minute second)))

(defun reviews-directory ()
  "Where reviews live: the section's directory, made on first use."
  (let ((directory (setting :directory)))
    (unless directory
      (fail "the mock-review section names no directory"))
    (ensure-directories-exist (merge-pathnames "probe" directory))
    directory))

(defun review-folder (id)
  "ID's folder. ID is a name this cell made; a path is refused before one is built."
  (unless (and (stringp id) (plusp (length id)) (<= (length id) +id-length+)
               (every #'id-character-p id))
    (fail "~s is not a review id" id))
  (merge-pathnames (format nil "~a/" id) (reviews-directory)))

(defun review-file (id)
  (merge-pathnames "review.json" (review-folder id)))

(defun read-json-file (path)
  (let ((text (ignore-errors (uiop:read-file-string path))))
    (and text (ignore-errors (nlk:decode-json text)))))

(defun write-json-file (path object)
  (ensure-directories-exist path)
  (nlk:write-file-atomically path (nlk:encode-json-object object)))

(defun review-ids ()
  "Every review this directory holds, newest folder first."
  (let ((directory (reviews-directory)))
    (sort (loop for folder in (uiop:subdirectories directory)
                for name = (car (last (pathname-directory folder)))
                when (and (stringp name) (probe-file (merge-pathnames "review.json" folder)))
                  collect name)
          #'string>)))

(defun load-review (id)
  "ID's review as decoded JSON, or a refusal naming it."
  (let ((data (read-json-file (review-file id))))
    (unless (hash-table-p data)
      (fail "no review ~a here: (mock_review_list) says which there are" id))
    data))

(defun save-review (data)
  (write-json-file (review-file (nlk:json-value data :string "id")) data)
  data)

(defun make-review (title brief decisions)
  "A new review at round 1, saved and answered."
  (let* ((id (nlk:make-durable-id "r"))
         (stamp (now-iso))
         (data (nlk:json-object "id" id
                                 "title" title
                                 "brief" brief
                                 "created_at" stamp
                                 "status" "open"
                                 "token" (format nil "~(~36r~36r~)" (get-universal-time)
                                                  (random (expt 36 8)))
                                 "rounds" (vector (make-round 1 stamp decisions)))))
    (write-json-file (review-file id) data)
    data))

(defun make-round (n stamp decisions)
  "Round N: the decisions asked, at STAMP, nothing answered yet."
  (nlk:json-object "n" n "created_at" stamp "decisions" (coerce decisions 'vector)
                    "feedback" nil "acked_at" nil))

(defun rounds (data)
  (coerce (nlk:json-array data "rounds") 'list))

(defun newest-round (data)
  (car (last (rounds data))))

(defun round-by-number (data n)
  "The round numbered N, or the newest for a NIL N, or a refusal naming it."
  (or (if n (find n (rounds data) :key (lambda (round) (nlk:json-value round :integer "n")))
          (newest-round data))
      (if n
          (fail "review ~a has no round ~a" (nlk:json-value data :string "id") n)
          (fail "review ~a has no rounds" (nlk:json-value data :string "id")))))

(defun add-round-to (data decisions)
  "Append the next round to DATA, saved and answered."
  (let* ((head (newest-round data))
         (n (1+ (nlk:json-value head :integer "n")))
         (new (make-round n (now-iso) decisions)))
    (setf (gethash "rounds" data) (concatenate 'vector (nlk:json-array data "rounds") (vector new)))
    (save-review data)
    new))

(defun review-open-p (data)
  (equal "open" (nlk:json-value data :string "status")))

(defun close-review (data)
  (setf (gethash "status" data) "closed"
        (gethash "closed_at" data) (now-iso))
  (save-review data))
