;;;; render.lisp --- a tools/call answer as the model reads it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; FlashData answers JSON: a structured payload, or text parts that are JSON
;;;; themselves. The original left the reading to the model, with a Skill per
;;;; operation saying what to preserve. Here one renderer does it: a result's
;;;; lists become numbered items with their title, link, snippet and small
;;;; facts, the object's own scalars a line each, and nothing beyond the
;;;; section's max_items and max_chars, so a 700-result extended search or a
;;;; two-hour transcript does not fill the turn.

(in-package #:nodecode-flashdata)

(defparameter +title-keys+ '("title" "name" "video_title" "channel_title" "headline" "question")
  "A result's name, in the order a result usually carries one.")

(defparameter +link-keys+ '("link" "url" "video_url" "channel_url" "website" "source_url"
                            "displayed_link")
  "Where a result points, in the same order.")

(defparameter +body-keys+ '("snippet" "description" "summary" "answer" "text" "content"
                            "abstract" "transcript" "caption")
  "What a result says, in the same order.")

(defparameter +meta-keys+ '("position" "rank" "price" "price_raw" "rating" "reviews" "address"
                            "date" "published" "published_at" "channel" "author" "duration"
                            "views" "start" "end" "seconds" "video_id" "handle" "status"
                            "job_id" "result_id" "next_offset" "credits" "count" "total")
  "The small facts one result line carries after its text.")

(defun items-limit ()
  "How many results of one list a reader sees."
  (if *flashdata* (setting :max-items) 10))

(defun characters-limit ()
  "How many characters of one answer a reader sees, or NIL for all of it."
  (if *flashdata* (setting :max-chars) 6000))

(defun scalar-p (value)
  "Whether VALUE is a fact a line can carry (JSON null and false read as absent)."
  (or (stringp value) (numberp value) (eq value t)))

(defun scalar-text (value)
  "VALUE as one line of text."
  (cond ((stringp value) value)
        ((eq value t) "yes")
        ((numberp value) (princ-to-string value))
        ((null value) "")
        (t (princ-to-string value))))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how many it left, or NIL."
  (cond ((null text) nil)
        ((or (null limit) (<= (length text) limit)) text)
        (t (format nil "~a [... ~d more characters]" (subseq text 0 limit)
                   (- (length text) limit)))))

(defun trimmed-text (value)
  "VALUE as non-empty trimmed text, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun members (object)
  "OBJECT's members as (KEY . VALUE) pairs, ordered by key."
  (let ((pairs '()))
    (maphash (lambda (key value)
               (push (cons (if (stringp key) key (princ-to-string key)) value) pairs))
             object)
    (sort pairs #'string< :key #'car)))

(defun pick-text (object keys)
  "The first of KEYS OBJECT carries as non-empty text, or NIL."
  (loop for key in keys
        for text = (trimmed-text (nlk:json-value object :any key))
        when text return text))

(defun meta-line (object)
  "OBJECT's small facts -- position, price, rating, date -- as one parenthesised line, or NIL."
  (let ((facts (loop for key in +meta-keys+
                      for value = (nlk:json-value object :any key)
                      when (and value (or (stringp value) (numberp value)))
                        collect (format nil "~(~a~) ~a" key value))))
    (and facts (format nil "(~{~a~^, ~})" (subseq facts 0 (min 6 (length facts)))))))

(defun item-lines (object limit)
  "OBJECT -- one result -- as the lines that name it, point at it and say what it says."
  (let ((title (pick-text object +title-keys+))
        (link (pick-text object +link-keys+))
        (body (pick-text object +body-keys+)))
    (remove nil (list title link (and body (clip body (min (or limit 400) 400))) (meta-line object)))))

(defun render-scalars (vector out)
  "VECTOR -- an array of facts -- as one bullet each."
  (loop for item across vector
        for index from 0
        when (< index (items-limit))
          do (format out "~&- ~a~%" (scalar-text item)))
  (when (> (length vector) (items-limit))
    (format out "~&[... ~d more]~%" (- (length vector) (items-limit)))))

(defun render-array (vector out limit)
  "VECTOR -- a result list -- as numbered items and their lines."
  (let ((shown (min (length vector) (items-limit))))
    (loop for item across vector
          for index from 1
          while (<= index shown)
          do (cond ((hash-table-p item)
                    (let ((lines (item-lines item limit)))
                      (format out "~&~d. ~a~%" index (or (first lines) "result"))
                      (dolist (line (rest lines))
                        (format out "   ~a~%" line))))
                   ((scalar-p item) (format out "~&- ~a~%" (scalar-text item)))))
    (when (> (length vector) shown)
      (format out "~&[... ~d more]~%" (- (length vector) shown)))))

(defun render-value (value out depth limit)
  "VALUE -- a JSON value -- written to OUT, DEPTH levels under the answer's own object."
  (cond ((hash-table-p value)
         (let* ((pairs (members value))
                (scalars (remove-if-not (lambda (pair) (scalar-p (cdr pair))) pairs))
                (nested (remove-if (lambda (pair) (scalar-p (cdr pair))) pairs)))
           (dolist (pair scalars)
             (format out "~&~@[~a: ~]~a~%"
                     (and (plusp depth) (car pair))
                     (clip (scalar-text (cdr pair)) (min (or limit 400) 400))))
           (loop for (key . member) in nested
                 do (cond ((and (vectorp member) (plusp (length member)))
                           (format out "~&~@[~a:~]~%" (and (plusp depth) key))
                           (render-array member out limit))
                          ((hash-table-p member)
                           (format out "~&~@[~a:~]~%" (and (plusp depth) key))
                           (render-value member out (1+ depth) limit))))))
        ((and (vectorp value) (plusp (length value)))
         (if (hash-table-p (aref value 0))
             (render-array value out limit)
             (render-scalars value out)))
        ((scalar-p value) (format out "~&- ~a~%" (scalar-text value)))))

(defun render-object (object out limit)
  "OBJECT -- one answer -- its scalars' lines, then one heading per list."
  (let* ((pairs (members object))
         (scalars (remove-if-not (lambda (pair) (scalar-p (cdr pair))) pairs))
         (nested (remove-if (lambda (pair) (scalar-p (cdr pair))) pairs)))
    (dolist (pair scalars)
      (format out "~&~a: ~a~%" (car pair)
              (clip (scalar-text (cdr pair)) (min (or limit 400) 400))))
    (when (and scalars nested) (terpri out))
    (loop for (key . value) in nested
          do (format out "~&## ~a~%" key)
             (render-value value out 1 limit))))

(defun content-text (content)
  "CONTENT -- a result's content array -- as the text parts it carries."
  (let ((parts (loop for item across content
                     for text = (and (hash-table-p item) (nlk:json-value item :text "text"))
                     when text collect text)))
    (and parts (format nil "~{~a~^~%~}" parts))))

(defun json-object-or-nil (text)
  "TEXT as a JSON object, or NIL when it is not one."
  (handler-case (let ((value (nlk:decode-json text :whole t)))
                  (and (hash-table-p value) value))
    (error () nil)))

(defun render-result (operation result limit)
  "One tools/call RESULT as text, its lists and facts in reading order, the key never in it."
  (let* ((is-error (nlk:json-value result :boolean "isError"))
         (structured (nlk:json-value result :any "structuredContent"))
         (content (nlk:json-value result :array "content"))
         (text (and content (plusp (length content)) (content-text content)))
         (payload (if (hash-table-p structured)
                      structured
                      (and text (trimmed-text text) (json-object-or-nil text))))
         (body (with-output-to-string (out)
                 (cond ((hash-table-p payload)
                        (when (plusp (length (members payload)))
                          (format out "~a~%~%" operation))
                        (render-object payload out limit))
                       ((trimmed-text text) (write-string (trimmed-text text) out))
                       (t (format out "~a returned nothing to read" operation)))))
         (body (nlk:trimmed (string-right-trim '(#\Newline #\Space) body))))
    (when (zerop (length body))
      (setf body (if is-error "the call failed" "the call returned nothing to read")))
    (when is-error
      (setf body (format nil "ERROR: ~a" body)))
    (redact (if (and limit (> (length body) limit))
                (format nil "~a~%[... ~d more characters; ask for the whole answer to read it all]"
                        (subseq body 0 limit) (- (length body) limit))
                body))))
