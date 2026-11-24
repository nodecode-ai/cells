;;;; detector.lisp --- the loop judgements, pure: no I/O, no state, no model.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of loop_detector.py. Two jobs, kept apart, as there:
;;;;
;;;; - CHECK-LINES / CHECK-TEXT judge streamed model text: identical
;;;;   consecutive lines, a short block repeated many times, or a window of
;;;;   short lines with almost no variety.
;;;; - JUDGE-ARGUMENT judges tool-call arguments. An argument is data, not
;;;;   prose: a table, a matrix, a CSV export may repeat on purpose, so the
;;;;   rule first asks whether the payload has a data shape at all, and only
;;;;   a repetition that reads as sentences earns a question.
;;;;
;;;; Thresholds are frozen: they were set before the first run, and every
;;;; number is backed by the corpus the original committed. They travel as an
;;;; alist so a test can measure a rule at another setting without editing it.

(in-package #:nodecode-degeneracy-guard)

;;; --- the frozen thresholds -------------------------------------------------------------

(defparameter *output-thresholds*
  '((:max-identical-run . 12)
    (:max-period . 20)
    (:min-repeats . 8)
    (:short-line-chars . 40)
    (:short-window-lines . 24)
    (:short-max-distinct . 4))
  "How model output is judged. A change here is a visible change of policy.")

(defparameter *argument-thresholds*
  '((:min-chars . 200)
    (:min-lines . 12)
    (:min-reps . 18)
    (:min-identical-run . 18)
    (:min-share . 3/5)
    (:max-period . 20)
    (:scan-cap-chars . 1000000))
  "How a tool argument is judged. Below MIN-CHARS a repetition cannot carry a
loop far enough to matter; MIN-SHARE is the part of the payload that must sit
inside the repeated block, so a document that quotes one line many times and
then goes on is not a loop.")

(defun th (thresholds key)
  "KEY's value in the threshold alist THRESHOLDS."
  (cdr (assoc key thresholds)))

(defun normalize (line)
  "LINE as a judgement sees it."
  (nlk:trimmed line))

;;; --- criteria 1 to 3: model output ------------------------------------------------

(defun all-blank-p (lines)
  (every (lambda (line) (zerop (length line))) lines))

(defun periodic-block (norm thresholds &aux (mp (th thresholds :max-period))
                                            (least (th thresholds :min-repeats)))
  "The first period of at most MP lines repeated at least LEAST times.
=> (values PERIOD REPS END-INDEX BLOCK), or NIL."
  (let ((n (length norm)))
    (loop for p from 1 to mp
          do (let ((i 0))
               (loop while (<= (+ i (* p least)) n)
                     do (let ((block (subseq norm i (+ i p))))
                          (if (all-blank-p block)
                              (incf i)
                              (let ((reps 1) (j (+ i p)))
                                (loop while (and (<= (+ j p) n)
                                                 (equal (subseq norm j (+ j p)) block))
                                      do (incf reps) (incf j p))
                                (when (>= reps least)
                                  (return-from periodic-block
                                    (values p reps (1- j) block)))
                                (setf i (if (> reps 1) j (1+ i))))))))))
  nil)

(defun short-line-window (norm thresholds)
  "The end index of the first window of short lines with almost no variety."
  (let ((size (th thresholds :short-window-lines))
        (chars (th thresholds :short-line-chars))
        (most (th thresholds :short-max-distinct))
        (n (length norm)))
    (loop for end from size to n
          do (let ((window (remove "" (subseq norm (- end size) end) :test #'string=)))
               (when (and (= (length window) size)
                          (every (lambda (line) (<= (length line) chars)) window)
                          (<= (length (nlk:distinct window)) most))
                 (return-from short-line-window
                   (values (1- end) (length (nlk:distinct window))))))))
  nil)

(defun verdict (triggered reason detail line-index metric)
  "One judgement, as a plist, so a caller can encode it or read it."
  (list :triggered triggered :reason reason :detail detail
        :line-index line-index :metric metric))

(defun check-lines (lines &key (thresholds *output-thresholds*))
  "Judge LINES: identical consecutive lines, a repeated short block, or a
window of short lines with almost no variety.
=> (:triggered BOOL :reason STRING :detail STRING :line-index N :metric N).
   REASON is \"\", \"identical_run\", \"periodic_repeat\" or \"short_loop\"."
  (let ((norm (mapcar #'normalize lines)))
    (let ((run (loop with best = 1 and at = -1 and next = 1
                     for i from 1 below (length norm)
                     do (if (and (plusp (length (nth i norm)))
                                 (string= (nth i norm) (nth (1- i) norm)))
                            (progn (incf next)
                                   (when (> next best) (setf best next at i)))
                            (setf next 1))
                     finally (return (and (>= best (th thresholds :max-identical-run))
                                          (cons at best))))))
      (when run
        (return-from check-lines
          (verdict t "identical_run"
                   (format nil "~d consecutive identical lines: ~s"
                           (cdr run) (nlk:clip (nth (car run) norm) 60))
                   (car run) (cdr run)))))
    (multiple-value-bind (period reps end block)
        (periodic-block norm thresholds)
      (when period
        (return-from check-lines
          (verdict t "periodic_repeat"
                   (format nil "period of ~d lines repeated ~d times: ~s"
                           period reps (subseq block 0 (min 3 (length block))))
                   end reps))))
    (multiple-value-bind (end distinct)
        (short-line-window norm thresholds)
      (when end
        (return-from check-lines
          (verdict t "short_loop"
                   (format nil "~d consecutive short lines with only ~d distinct"
                           (th thresholds :short-window-lines) distinct)
                   end distinct))))
    (verdict nil "" "no degeneracy detected" -1 0)))

(defun check-text (text &key (thresholds *output-thresholds*))
  "CHECK-LINES over TEXT's own lines."
  (check-lines (nlk:lines text) :thresholds thresholds))

;;; --- what a payload's shape says about its repetition --------------------------------

(defparameter +separator-chars+
  (concatenate 'string "-=_*+#" (string (code-char 183)) (string (code-char 8212)))
  "The characters a separator line is made of.")

(defparameter +patterns+
  `((:markdown-table . ,(cl-ppcre:create-scanner "^\\|.*\\|$"))
    (:checklist . ,(cl-ppcre:create-scanner "^[-*+]\\s+\\[[ xX-]\\]"))
    (:xml . ,(cl-ppcre:create-scanner
              "^(</?[A-Za-z][\\w:.-]*(\\s[^<>]*)?/?>(</?[A-Za-z][\\w:.-]*(\\s[^<>]*)?/?>)*|<!--.*-->)$"))
    (:sql . ,(cl-ppcre:create-scanner
              "^(insert|update|delete|select|create|alter|drop|with|values|replace|commit)\\b"
              :case-insensitive-mode t))
    (:toml-table . ,(cl-ppcre:create-scanner "^\\[\\[?[A-Za-z0-9_.\"-]+\\]?\\]$"))
    (:toml-kv . ,(cl-ppcre:create-scanner
                  "^[A-Za-z0-9_.\\-]+\\s*=\\s*(\"[^\"]*\"|'[^']*'|\\[[^\\]]*\\]|\\{[^}]*\\}|[^\\s+\\-*/=]{0,120})$"))
    (:yaml-item . ,(cl-ppcre:create-scanner "^-\\s|^-\\s*$"))
    (:yaml-kv . ,(cl-ppcre:create-scanner
                  "^[^\\s:#{}\\[\\]\"';,][^:#{}\\[\\]\"';,]*:\\s*(\\S.*)?$"))
    (:log-prefix . ,(cl-ppcre:create-scanner
                     "^(\\[?\\d{4}-\\d{2}-\\d{2}[T ]\\d{2}:\\d{2}|\\[?\\d{1,2}:\\d{2}:\\d{2}|[A-Z][a-z]{2}\\s+\\d{1,2}\\s+\\d{2}:\\d{2})"))
    (:log-level . ,(cl-ppcre:create-scanner
                    "\\b(TRACE|DEBUG|INFO|NOTICE|WARN|WARNING|ERROR|FATAL|CRITICAL)\\b"))
    (:numeric-row . ,(cl-ppcre:create-scanner "^[\\[\\]{}()0-9.,;:\\-+eE\\s*/%=]+$"))
    (:code-element . ,(cl-ppcre:create-scanner "^[\\[\\]{}(),;]*\\s*$|,\\s*$")))
  "The shapes a payload can have. Anything recognised here repeats on
purpose, so the guard leaves it alone: never blocked, never asked about.")

(defparameter +separator+ (cl-ppcre:create-scanner
                           (format nil "^[~a~c-]+$" +separator-chars+ #\Space))
  "A line made only of separator characters and blanks.")

(defparameter +prose-end+
  (coerce (list #\. #\! #\? (code-char #x3002) (code-char #xFF01) (code-char #xFF1F)
                (code-char #x2026) (code-char #xFF1B) #\; #\:)
          'string)
  "What a sentence ends with, in the scripts the original saw loops in.")

(defparameter +prose-floor+ #x2E80
  "A character above this code is a letter for the prose test: CJK text has
no case and no ASCII alphabet to count.")

(defparameter +grammar+ (cl-ppcre:create-scanner "[;,{}()\\[\\]=<>|@$\\\\]")
  "A delimiter, tag or terminator: what makes a line a record rather than a
sentence.")

(defun json-payload-p (text)
  "Whether TEXT is one JSON object or array. The reader is lenient about what
follows a value -- `1,1,ok' decodes as 1 -- so the text must open as a
container for the answer to be about the whole payload."
  (let ((text (nlk:trimmed text)))
    (and (plusp (length text))
         (find (char text 0) "{[")
         (handler-case (progn (nlk:decode-json text) t)
           (error () nil)))))

(defun json-line-p (line)
  (and (<= 2 (length line))
       (char= (char line 0) #\{)
       (char= (char line (1- (length line))) #\})
       (json-payload-p line)))

(defun prose-line-p (line)
  "Whether LINE reads like a sentence rather than a data record. Data has
grammar -- delimiters, tags, terminators -- and prose has sentences; this is
the one signal the rule trusts when it decides whether repetition is worth a
person's attention."
  (let ((text (nlk:trimmed line)))
    (declare (type string text))
    (cond ((zerop (length text)) nil)
          ((find (char text (1- (length text))) +prose-end+) t)
          ((cl-ppcre:scan +grammar+ text) nil)
          (t (let* ((words (nlk:split-words text))
                    (letters (count-if (lambda (char)
                                         (or (alpha-char-p char)
                                             (> (char-code char) +prose-floor+)))
                                       text)))
               (and (<= (length words) 4)
                    (>= letters (* (length text) 3/5))))))))

(defun shared-delimiter (body)
  "The one delimiter most of BODY's lines carry the same number of times, with
rows that differ from each other: a CSV or TSV export, not prose with a comma."
  (let ((n (length body)))
    (loop for delim across (format nil ",;~c|" #\Tab)
          do (let* ((rows (remove-if-not (lambda (line) (find delim line)) body))
                    (counts (mapcar (lambda (line) (count delim line)) rows)))
               (when (and (<= 3 (length rows))
                          (<= (* n 3/5) (length rows))
                          (= 1 (length (remove-duplicates counts)))
                          (<= 2 (length (nlk:distinct rows))))
                 (return-from shared-delimiter t))))
    nil))

(defun shape-name (keyword)
  "KEYWORD as the name the metrics report, matching the original's."
  (substitute #\_ #\- (string-downcase (string keyword))))

(defun detect-structure (lines)
  "The name of the data shape LINES have, or \"\" when they have none.
Deliberately generous: anything named here is left alone."
  (let* ((body (remove "" (mapcar #'normalize lines) :test #'string=)))
    (when (< (length body) 3)
      (return-from detect-structure ""))
    (let ((text (format nil "~{~a~^~%~}" lines))
          (n (length body)))
      (when (json-payload-p text)
        (return-from detect-structure "json"))
      (let ((head (subseq body 0 (min 20 (length body)))))
        (when (every #'json-line-p head)
          (return-from detect-structure "jsonl")))
      (dolist (key '(:markdown-table :checklist :xml :sql :toml-table :toml-kv))
        (let* ((scanner (cdr (assoc key +patterns+)))
               (hits (count-if (lambda (line) (cl-ppcre:scan scanner line)) body)))
          (when (and (<= 3 hits) (<= (* n 3/5) hits))
            (return-from detect-structure (shape-name key)))))
      (let ((hits (count-if (lambda (line)
                              (or (cl-ppcre:scan (cdr (assoc :yaml-item +patterns+)) line)
                                  (cl-ppcre:scan (cdr (assoc :yaml-kv +patterns+)) line)))
                            body)))
        (when (and (<= 3 hits) (<= (* n 3/5) hits))
          (return-from detect-structure "yaml")))
      (let ((hits (count-if (lambda (line)
                              (or (cl-ppcre:scan (cdr (assoc :log-prefix +patterns+)) line)
                                  (cl-ppcre:scan (cdr (assoc :log-level +patterns+)) line)))
                            body)))
        (when (and (<= 3 hits) (<= (* n 3/5) hits))
          (return-from detect-structure "log")))
      (let ((hits (count-if (lambda (line)
                              (and (cl-ppcre:scan (cdr (assoc :numeric-row +patterns+)) line)
                                   (find-if #'digit-char-p line)))
                            body)))
        (when (and (<= 3 hits) (<= (* n 3/5) hits))
          (return-from detect-structure "numeric_rows")))
      (let ((hits (count-if (lambda (line) (cl-ppcre:scan +separator+ line)) body)))
        (when (and (<= 3 hits) (<= (* n 3/5) hits))
          (return-from detect-structure "separator")))
      (when (shared-delimiter body)
        (return-from detect-structure "delimited"))
      (let ((hits (count-if (lambda (line)
                              (cl-ppcre:scan (cdr (assoc :code-element +patterns+)) line))
                            body)))
        (when (and (<= 3 hits) (<= (* n 3/5) hits))
          (return-from detect-structure "code_elements")))
      "")))

;;; --- the tool-argument rule -----------------------------------------------------------

(defun longest-identical-run (body)
  "The longest run of identical consecutive lines in BODY."
  (let ((run 1) (best 1))
    (loop for i from 1 below (length body)
          do (setf run (if (string= (nth i body) (nth (1- i) body)) (1+ run) 1))
             (setf best (max best run)))
    best))

(defun largest-repetition (body max-period)
  "The largest periodic block in BODY, by line count.
=> (values PERIOD REPS LINES DISTINCT BLOCK); a payload with no repeat
answers period 0 and one rep."
  (let ((best (list 0 1 0 0 nil))
        (n (length body)))
    (loop for p from 1 to max-period
          do (let ((i 0))
               (loop while (<= (+ i p) n)
                     do (let ((block (subseq body i (+ i p))))
                          (if (all-blank-p block)
                              (incf i)
                              (let ((reps 1) (j (+ i p)))
                                (loop while (and (<= (+ j p) n)
                                                 (equal (subseq body j (+ j p)) block))
                                      do (incf reps) (incf j p))
                                (when (< (third best) (* reps p))
                                  (setf best (list p reps (* reps p)
                                                   (length (nlk:distinct block)) block)))
                                (setf i (if (> reps 1) j (1+ i)))))))))
    (values (first best) (second best) (third best)
            (fourth best) (fifth best))))

(defun judge-argument (text &key (thresholds *argument-thresholds*))
  "Whether TEXT, one tool argument, deserves a person's attention before the
call runs. The rule, in order:
  1. too small or too few lines to matter -> allow;
  2. a data shape (table, matrix, SQL, YAML/TOML, XML, log, checklist,
     delimited records, code literals) -> allow, repetition there is expected;
  3. no repetition worth mentioning -> allow;
  4. the repeating unit reads as prose -> ask.
=> (:ask BOOL :reason STRING :detail STRING :metrics PLIST)."
  (let* ((chars (length (or text "")))
         (cap (th thresholds :scan-cap-chars))
         (scanned (if (<= chars cap) (or text "")
                      (concatenate 'string
                                   (subseq text 0 (floor cap 2))
                                   (subseq text (- chars (floor cap 2))))))
         (norm (mapcar #'normalize (nlk:lines scanned)))
         (body (remove "" norm :test #'string=)))
    (flet ((answer (ask reason detail metrics)
             (list :ask ask :reason reason :detail detail :metrics metrics)))
      (when (< chars (th thresholds :min-chars))
        (return-from judge-argument
          (answer nil "" "below min_chars" (list :chars chars))))
      (when (< (length body) (th thresholds :min-lines))
        (return-from judge-argument
          (answer nil "" "too few lines" (list :chars chars :lines (length body)))))
      (let* ((run (longest-identical-run body))
             (max-period (th thresholds :max-period)))
        (multiple-value-bind (period reps lines distinct block)
            (largest-repetition body max-period)
          (let* ((share (/ lines (length body)))
                 (shape (detect-structure (nlk:lines scanned)))
                 (run-hit (<= (th thresholds :min-identical-run) run))
                 (unit (if run-hit (list (car (last body))) block))
                 (prose (/ (count-if #'prose-line-p unit) (max 1 (length unit))))
                 (metrics (list :chars chars :lines (length body) :distinct (length (nlk:distinct body))
                                :max-identical-run run :period period :reps reps
                                :rep-lines lines :share (float (/ (round (* 100 share)) 100))
                                :structure (if (zerop (length shape)) "none" shape)
                                :prose-share (float (/ (round (* 100 prose)) 100)))))
            (cond ((plusp (length shape))
                   (answer nil "" (format nil "data-shaped payload (~a)" shape) metrics))
                  ((and (not run-hit)
                        (not (and (<= (th thresholds :min-reps) reps)
                                  (<= (th thresholds :min-share) share))))
                   (answer nil "" "repetition below the ask threshold" metrics))
                  (run-hit
                   (answer t "identical_run"
                           (format nil "~d consecutive identical sentences in a ~d-line argument: ~s"
                                   run (length body) (nlk:clip (car (last body)) 60))
                           metrics))
                  (t
                   (answer t "periodic_repeat"
                           (format nil "~d-line block repeated ~d times (~d% of a ~d-line argument): ~s"
                                   period reps (round (* 100 share)) (length body)
                                   (nlk:clip (first unit) 60))
                           metrics)))))))))
