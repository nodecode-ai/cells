;;;; suite.lisp --- the fixed questions, how an answer is cut out of a reply,
;;;; and the exact grading each question gets.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every item is one question with one answer that can be graded by a rule,
;;;; never by a judge: a number, one of a few accepted spellings, a regular
;;;; expression, a word count, or a JSON array. That is the whole reason a
;;;; drop means something -- the questions never change, so a lower score is
;;;; the model, not the exam.
;;;;
;;;; The items are this rewrite's own, in the shape of the default suite the
;;;; original plugin ships (see NOTICE).

(in-package #:nodecode-model-drift-watch)

(defparameter +answer-system+
  "Answer the question. Work briefly, in at most a few short lines. Then end
with one final line of exactly this form and nothing after it:
ANSWER: <the answer>"
  "The instruction an item extracted as `answer_line' is asked under.")

(defparameter +full-system+
  "Follow the instruction exactly. Output only what it asks for: no preamble,
no explanation, no code fences."
  "The instruction an item extracted whole is asked under.")

(defun item (id category extract prompt grader)
  "One question: ID, its CATEGORY, how its answer is EXTRACTED from the reply,
its PROMPT, and the GRADER rule."
  (list :id id :category category :extract extract :prompt prompt :grader grader))

(defparameter *suite*
  (list
   (item "r01-arith" "reasoning" :answer-line
         "Compute 4,837 × 29 − 6,251." '(:number 134022))
   (item "r02-days" "reasoning" :answer-line
         "How many days are there from 2024-02-15 to 2024-03-15? Count the end date but not the start date."
         '(:number 29))
   (item "r03-letters" "reasoning" :answer-line
         "How many times does the letter r appear in the phrase \"strawberry raspberry recurring\"?"
         '(:number 9))
   (item "r04-weekday" "reasoning" :answer-line
         "Today is a Wednesday. What day of the week will it be 100 days from today? Give the English name of the day."
         '(:exact "Friday"))
   (item "r05-percent" "reasoning" :answer-line
         "A jacket costs 84 dollars after a 30 percent discount. What did it cost before the discount, in dollars?"
         '(:number 120))
   (item "r06-average" "reasoning" :answer-line
         "What is the average of 17, 23, 41 and 55?" '(:number 34))
   (item "r07-primes" "reasoning" :answer-line
         "How many prime numbers are greater than 20 and less than 40?" '(:number 4))
   (item "r08-words" "reasoning" :answer-line
         "How many words are in this sentence: \"the quick brown fox jumps over the lazy dog\"?"
         '(:number 9))
   (item "i01-colors" "instruction" :full
         "List the three primary colors of light as a JSON array of lowercase English strings. Output only the JSON array, with no code fences and no other text."
         '(:json ("red" "green" "blue")))
   (item "i02-three-words" "instruction" :full
         "Answer in exactly three words: what is the capital city of France?"
         '(:words 3 ("paris")))
   (item "i03-lowercase" "instruction" :full
         "In lowercase letters only, and with no other word: what is the capital city of Japan?"
         '(:exact "tokyo"))
   (item "i04-commas" "instruction" :full
         "Write the numbers 1 through 5 in one line, separated by commas, with no spaces and nothing else."
         '(:regex "^1,2,3,4,5$"))
   (item "f01-japanese-arith" "japanese" :answer-line
         "次の計算をしてください: 37 × 12 − 4" '(:number 440))
   (item "f02-japanese-roman" "japanese" :answer-line
         "「さくら」をローマ字で書いてください。" '(:exact "sakura")))
  "The fixed question set: fourteen items in four categories, each graded by a rule.")

(defun nv (text)
  "TEXT without its surrounding whitespace, \"\" when it is NIL."
  (nlk:trimmed (or text "")))

(defun answer-line (raw)
  "What RAW answers: the text after its last `ANSWER:' marker, else its last
non-empty line, else the whole reply."
  (let* ((text (or raw ""))
         (found nil)
         (at 0))
    (loop for start = 0 then (+ end 1)
          for end = (position #\Newline text :start start)
          for line = (subseq text start (or end (length text)))
          do (let ((marker (search "ANSWER:" (string-upcase line))))
               (when marker
                 (setf found (subseq line (+ marker (length "ANSWER:")))
                       at t)))
          while end)
    (cond (at (dequote (nv found)))
          (t (or (loop for line in (uiop:split-string text :separator '(#\Newline))
                        for trimmed = (nv line)
                        when (plusp (length trimmed)) do (return trimmed))
                 (dequote (nv text)))))))

(defun dequote (text)
  "TEXT without the whitespace, backticks, quotes or code fences a reply wraps an answer in."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return #\`) text)))
    (loop while (and (>= (length text) 2)
                     (or (and (char= (char text 0) #\") (char= (char text (1- (length text))) #\"))
                         (and (char= (char text 0) #\') (char= (char text (1- (length text))) #\'))))
          do (setf text (string-trim '(#\Space #\Tab #\Newline #\Return)
                                      (subseq text 1 (1- (length text))))))
    text))

(defun extracted (question raw)
  "RAW's answer to QUESTION, as the text grading reads."
  (if (eq :full (getf question :extract))
      (dequote (nv raw))
      (answer-line raw)))

;;; --- grading ------------------------------------------------------------------------

(defun number-in (text)
  "The first number TEXT holds, as a rational, or NIL for none."
  (let ((found (ppcre:scan-to-strings "-?[0-9][0-9,]*(?:\\.[0-9]+)?" text)))
    (when found
      (ignore-errors
       (let ((digits (remove #\, found)))
         (if (find #\. digits)
             (let ((dot (position #\. digits)))
               (+ (parse-integer digits :end dot)
                  (/ (parse-integer digits :start (1+ dot))
                     (expt 10 (- (length digits) dot 1)))))
             (parse-integer digits)))))))

(defun grade-number (text expected)
  "TEXT graded against the EXPECTED number."
  (let ((got (number-in text)))
    (cond ((null got) (values nil "no number in the answer"))
          ((= got expected) (values t nil))
          (t (values nil (format nil "expected ~a, got ~a" expected got))))))

(defun grade-exact (text answers)
  "TEXT graded against ANSWERS, any of which is accepted, case ignored."
  (let ((normalized (string-downcase (string-trim ".!'\" " text))))
    (if (member normalized (mapcar (lambda (answer) (string-downcase (nv answer))) answers)
                :test #'string=)
        (values t nil)
        (values nil (format nil "expected ~{~a~^ or ~}, got \"~a\"" answers
                            (nlk:one-line text :cap 60))))))

(defun grade-regex (text pattern)
  "TEXT graded by PATTERN, which must match all of it."
  (if (ppcre:scan (concatenate 'string "\\A" pattern "\\z") text)
      (values t nil)
      (values nil (format nil "did not match the required shape, got \"~a\""
                          (nlk:one-line text :cap 60)))))

(defun grade-words (text count wanted)
  "TEXT graded as exactly COUNT words, one of which holds each of WANTED."
  (let* ((words (remove "" (uiop:split-string text :separator '(#\Space #\Tab #\Newline #\Return))
                        :test #'string=))
         (lowered (mapcar #'string-downcase words)))
    (cond ((/= (length words) count)
           (values nil (format nil "expected ~d words, got ~d" count (length words))))
          ((every (lambda (word) (some (lambda (w) (search word w)) lowered)) wanted)
           (values t nil))
          (t (values nil (format nil "a word is missing: ~{~a~^, ~}" wanted))))))

(defun grade-json (text wanted)
  "TEXT graded as a JSON array whose strings are WANTED, order ignored."
  (let ((decoded (ignore-errors (nlk:decode-json text :whole t))))
    (cond ((not (vectorp decoded)) (values nil "not a JSON array"))
          ;; decode-json answers a general vector; the elements are strings.
          ((equal (sort (loop for value across decoded collect (princ-to-string value)) #'string<)
                  (sort (copy-list wanted) #'string<))
           (values t nil))
          (t (values nil (format nil "expected ~{~a~^, ~}" wanted))))))

(defun grade (question text)
  "TEXT graded for QUESTION. => (values PASSED-P DETAIL)."
  (destructuring-bind (type &rest arguments) (getf question :grader)
    (ecase type
      (:number (grade-number text (first arguments)))
      (:exact (grade-exact text arguments))
      (:regex (grade-regex text (first arguments)))
      (:words (grade-words text (first arguments) (second arguments)))
      (:json (grade-json text (first arguments))))))

(defun ask-for (question)
  "The system text, then the user text, QUESTION is asked with."
  (if (eq :full (getf question :extract))
      (values +full-system+ (getf question :prompt))
      (values +answer-system+ (getf question :prompt))))
