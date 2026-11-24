;;;; claims.lisp --- which sentences state a decision, and whether a quote backs them.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Rewritten from the original plugin's claims.py and verdict.py: detect a
;;;; claim on the sentence as written, look every quoted span up in what the
;;;; user typed, blank the spans that verify and detect again (words inside a
;;;; verified quote are the user's, not a claim), and pass a claim only when a
;;;; verified quote shares two content tokens with the rest of the sentence.

(in-package #:nodecode-provenance)

(defstruct (claim (:constructor make-claim (sentence pattern kind lang start end)))
  sentence pattern kind lang start end)

(defparameter +scopes+ '("decisions" "decisions+preferences")) 

(defun sentence-ends-in-question-p (sentence)
  (let* ((text (string-right-trim '(#\Space #\Tab) sentence))
         (len (length text))
         (tail (if (and (plusp len) (find (char text (1- len)) ")）"))
                   (string-right-trim '(#\Space #\Tab) (subseq text 0 (1- len)))
                   text)))
    (and (plusp (length tail)) (find (char tail (1- (length tail))) "?？"))))

;;; --- exemptions ---------------------------------------------------------------

(defun words-before (text index &optional (window 80))
  "The words before INDEX, lower-cased and stripped of a trailing full stop or quote mark."
  (let ((from (max 0 (- index window))))
    (mapcar (lambda (word)
              (string-downcase (string-right-trim ".'’" word)))
            (cl-ppcre:all-matches-as-strings "[^\\W\\d_]+" (subseq text from index)))))

(defun text-before (text index &optional (window 80))
  (subseq text (max 0 (- index window)) index))

(defun clause-words-before (text index)
  "The words before INDEX back to the last contrast or clause break."
  (let ((before (string-downcase (text-before text index))))
    (let ((cut (loop for break in +en-clause-breaks+
                     maximize (let ((at (search break before :from-end t)))
                                (if at (+ at (length break)) 0)))))
      (mapcar (lambda (word) (string-right-trim ".'’" word))
              (cl-ppcre:all-matches-as-strings "[^\\W\\d_]+" (subseq before cut))))))

(defparameter +spec-label-re+
  (format nil "(?i)^\\W*(?:~a)(?:\\s*#?\\s*[\\w.\\-]{1,20})?\\s*(?::|\\s[-–—]\\s)"
          (format nil "~{~a~^|~}"
                  (sort (mapcar #'cl-ppcre:quote-meta-chars +en-spec-labels+) #'> :key #'length)))
  "A sentence that starts like a QA or specification line.")

(defun sentence-exempt-p (sentence)
  (let ((lowered (string-downcase (string-left-trim " *_>" sentence))))
    (or (cl-ppcre:scan +spec-label-re+ sentence)
        (some (lambda (start) (cl-ppcre:scan (format nil "^~a\\s" (cl-ppcre:quote-meta-chars start)) lowered))
              +en-spec-starts+))))

(defun key-value-exempt-p (sentence start)
  "A key/value line describes data, not a decision: user confirmed: false."
  (multiple-value-bind (whole parts)
      (cl-ppcre:scan-to-strings "^\\s*([^:]{1,60}):\\s*(.*)$" sentence)
    (when whole
      (let* ((key (string-trim " `*_" (aref parts 0)))
             (value (string-trim " .;," (aref parts 1)))
             (key-end (+ (search key sentence) (length key)))
             (words (cl-ppcre:all-matches-as-strings "[^\\W_]+" key))
             (scalar (cl-ppcre:scan "^[a-z][\\w \"'.,-]{0,40}$" value)))
        (and (<= start key-end)
             (<= (length words) 4)
             (not (find-if (lambda (ch) (find ch ",-")) key))
             scalar
             (plusp (length value)))))))

(defun subject-context-ok-p (sentence start end subject &key phrase)
  (let ((words (words-before sentence start))
        (clause (clause-words-before sentence start))
        (after (cl-ppcre:all-matches-as-strings "[^\\W\\d_]+"
                                                 (subseq sentence (min (length sentence) (+ end 40))))))
    (cond ((sentence-exempt-p sentence) nil)
          ((key-value-exempt-p sentence start) nil)
          ((some (lambda (word) (member word +en-conditionals+ :test #'string=))
                 (last words (min 3 (length words)))) nil)
          ((some (lambda (word) (member word +en-generic-context+ :test #'string=)) clause) nil)
          ((some (lambda (word) (member word +en-reported-verbs+ :test #'string=)) (rest clause)) nil)
          ((and after (member (string-downcase (string-right-trim "." (first after)))
                               +en-field-nouns+ :test #'string=)) nil)
          ((or phrase (cl-ppcre:scan "^the\\s" subject :start 0) (string-equal subject "the user")
               (string-equal subject "the owner"))
           (or phrase (cl-ppcre:scan "(?i)^the\\b" subject) t))
          (t (let* ((before (string-right-trim " \t*_" (text-before sentence start))))
               (cond ((zerop (length before)) t)
                     ((find (char before (1- (length before))) ",;:(—–-|>\"'“”‘’") t)
                     (t (let ((last-word (first (last words))))
                          (and last-word
                               (member last-word +en-bare-subject-after+ :test #'string=)
                               (cl-ppcre:scan (format nil "~a\\s*$" (cl-ppcre:quote-meta-chars last-word))
                                               (string-downcase before)))))))))))

(defun zh-subject-context-ok-p (sentence start end)
  (let* ((before (text-before sentence start))
         (after (subseq sentence (min (length sentence) end) (min (length sentence) (+ end 60))))
         (rest-of-sentence (subseq sentence start))
         (trimmed-after (string-left-trim '(#\Space) after)))
    (cond ((some (lambda (word) (cl-ppcre:scan (format nil "~a$" (cl-ppcre:quote-meta-chars word)) before))
                 +zh-third-party-before+) nil)
          ((some (lambda (word) (cl-ppcre:scan (format nil "~a$" (cl-ppcre:quote-meta-chars word))
                                                (string-right-trim '(#\Space) before)))
                 +zh-open-before+) nil)
          ((some (lambda (word) (cl-ppcre:scan (cl-ppcre:quote-meta-chars word) (text-before sentence start 40)))
                 +zh-generic-context+) nil)
          ((and (plusp (length trimmed-after)) (char= (char trimmed-after 0) (code-char #x7684))) nil)
          ((some (lambda (word) (cl-ppcre:scan (cl-ppcre:quote-meta-chars word) rest-of-sentence))
                 +zh-condition-markers+) nil)
          ((and (plusp (length trimmed-after))
                (let ((pair (assoc (char trimmed-after 0) +zh-open-after+)))
                  (and pair
                       (let ((next (and (> (length trimmed-after) 1) (char trimmed-after 1))))
                         (or (zerop (length (second pair)))
                             (and next (not (find next (second pair)))))))))
           nil)
          (t t))))

;;; --- detection ----------------------------------------------------------------

(defun verb-claims (sentence start end table lang)
  "Every verb of TABLE that follows the subject at (START END), as claims."
  (let* ((region (subseq sentence end))
         (prefix (concatenate 'string "\\A(?i)"
                              (if (string= lang "zh") +zh-adverbs+ +en-subject-paren+)
                              (if (string= lang "zh") "" +en-aux+)
                              (if (string= lang "zh") "" +en-adverbs+)
                              (if (string= lang "zh") "\\s*" "\\s+")))
         (claims '()))
    (dolist (entry table)
      (let ((pattern (cdr entry)))
        (when (cl-ppcre:scan (concatenate 'string prefix "(?:" pattern (if (string= lang "zh") ")" ")(?![\\w-])"))
                              region)
          (push (make-claim sentence (car entry) :decision lang start end) claims))))
    (nreverse claims)))

(defun phrase-claims (sentence table)
  (let ((claims '()))
    (dolist (entry table)
      (loop for (start end) on (cl-ppcre:all-matches (concatenate 'string "(?i)" (cdr entry)) sentence) by #'cddr
            when (subject-context-ok-p sentence start end (if (string= (car entry) "the user's decision")
                                                              "the user"
                                                              "user")
                                        :phrase t)
              do (push (make-claim sentence (car entry) :decision "en-phrase" start end) claims)))
    (nreverse claims)))

(defun claims-in (text &key (scope "decisions"))
  "Every decision claim in TEXT, one per pattern match, in text order."
  (let ((preferences (string= scope "decisions+preferences"))
        (claims '()))
    (dolist (sentence (sentence-list text))
      (unless (sentence-ends-in-question-p sentence)
        (loop for (start end) on (cl-ppcre:all-matches (concatenate 'string "(?i)" +en-subject+) sentence) by #'cddr
              do (let ((subject (subseq sentence start end)))
                   (when (subject-context-ok-p sentence start end subject)
                     (setf claims (append claims (verb-claims sentence start end
                                                               (if preferences
                                                                   (append +en-decision-verbs+ +en-preference-verbs+)
                                                                   +en-decision-verbs+)
                                                               "en"))))))
        (loop for (start end) on (cl-ppcre:all-matches +zh-subject+ sentence) by #'cddr
              do (when (zh-subject-context-ok-p sentence start end)
                   (setf claims (append claims (verb-claims sentence start end
                                                             (if preferences
                                                                 (append +zh-decision-verbs+ +zh-preference-verbs+)
                                                                 +zh-decision-verbs+)
                                                             "zh")))))
        (setf claims (append claims (phrase-claims sentence +en-decision-phrases+)))))
    (sort claims #'< :key #'claim-start)))

(defun mask-spans (sentence spans)
  "SENTENCE with SPANS blanked out, keeping every offset."
  (let ((chars (copy-seq sentence)))
    (dolist (span spans chars)
      (loop for i from (first span) below (min (second span) (length chars))
            do (setf (char chars i) #\Space)))))

;;; --- the user's own words ------------------------------------------------------

(defstruct (said (:constructor make-said (texts norm norm-whole last))) texts norm
  norm-whole
  last)

(defun make-evidence (texts)
  "TEXTS, the user's own messages, prepared once per check."
  (let* ((clean (remove-if-not (lambda (text) (and (stringp text) (plusp (length (string-trim '(#\Space) text)))))
                                texts))
         (recent (last clean 3)))
    (make-said clean
                (mapcar #'normalize clean)
                (mapcar (lambda (text) (cl-ppcre:regex-replace "\\s*[.,!?;。，！？；]+$"
                                                                (normalize text) ""))
                        clean)
                recent)))

(defun lookup-quote (evidence quote)
  "=> (VALUES STATUS MESSAGE): found, negated, question, too_short or not_found."
  (let* ((q (normalize-quote quote))
         (whole (and (plusp (length q))
                     (some (lambda (text) (string= q text)) (said-norm-whole evidence))))
         (norm (said-norm evidence))
         (found nil) (negated nil) (question nil))
    (when (plusp (length q))
      (when (short-quote-p quote)
        (return-from lookup-quote (if whole (values "found" quote) (values "too_short" nil))))
      (loop for text in norm
            for original in (said-texts evidence)
            do (let ((starts (remove-if (lambda (start) (in-question-p text (+ start (length q))))
                                        (occurrences q text))))
                 (cond ((null starts)
                        (when (and (null question) (occurs-in-p q text)) (setf question original)))
                       ((has-negation-p q) (return-from lookup-quote (values "found" original)))
                       ((some (lambda (start) (not (negated-at-p text start))) starts)
                        (return-from lookup-quote (values "found" original)))
                       ((null negated) (setf negated original))))))
    (cond (negated (values "negated" negated))
          (question (values "question" question))
          (whole (values "found" quote))
          (t (values "not_found" nil)))))

(defparameter +reason-order+ '("quote_negated" "quote_from_question" "quote_unrelated"
                               "quote_too_short" "quote_not_found"))

(defun check-one-sentence (sentence evidence scope)
  "One check per claim in SENTENCE => a list of plists, empty when it claims nothing."
  (when (claims-in sentence :scope scope)
    (let* ((spans (quote-spans sentence))
           (looked (mapcar (lambda (span)
                              (multiple-value-bind (status message) (lookup-quote evidence (third span))
                                (list span status message)))
                            spans))
           (verified (mapcar #'first (remove-if-not (lambda (row) (string= (second row) "found")) looked)))
           (claims (claims-in (mask-spans sentence verified) :scope scope)))
      (when claims
        (loop for claim in claims
              for index from 0
              for lo = (if (zerop index) 0 (claim-start (nth (1- index) claims)))
              for hi = (if (< (1+ index) (length claims))
                            (claim-start (nth (1+ index) claims))
                            (length sentence))
              collect (check-one-claim sentence claim lo hi looked))))))

(defun check-one-claim (sentence claim lo hi looked)
  (let* ((mine (remove-if-not (lambda (row) (<= lo (first (first row)) (1- hi))) looked))
         (segment (subseq sentence lo hi))
         (reasons '()) (passed nil))
    (dolist (row mine)
      (destructuring-bind (span status message) row
        (let ((inner (third span)))
          (cond ((string= status "found")
                 (let ((rest (remove-spans segment (list (list (- (first span) lo) (- (second span) lo) inner)))))
                   (cond ((and (member (claim-pattern claim) +approval-patterns+ :test #'string=)
                               (leads-with-negation-p inner)
                               (not (has-negation-p (normalize rest))))
                          (pushnew "quote_negated" reasons :test #'string=))
                         ((or (short-quote-p inner) (related-p inner rest))
                          (setf passed (list :claim (claim-sentence claim) :pattern (claim-pattern claim)
                                             :kind (claim-kind claim) :ok t :quote inner
                                             :matched-in "this-session")))
                         (t (pushnew "quote_unrelated" reasons :test #'string=)))))
                ((string= status "too_short") (pushnew "quote_too_short" reasons :test #'string=))
                ((string= status "negated") (pushnew "quote_negated" reasons :test #'string=))
                ((string= status "question") (pushnew "quote_from_question" reasons :test #'string=))
                (t (pushnew "quote_not_found" reasons :test #'string=)))
          )))
    (or passed
        (list :claim (claim-sentence claim) :pattern (claim-pattern claim) :kind (claim-kind claim)
              :ok nil
              :reason (or (loop for reason in +reason-order+ when (member reason reasons :test #'string=)
                                 return reason)
                          "no_quote")
              :quote (loop for row in mine
                           for status = (second row)
                           when (and (stringp (third row)) (not (string= status "found")))
                             return (third row))))))

(defun evaluate-text (text texts &key (scope "decisions"))
  "Every check for TEXT against TEXTS, the user's own words => a list of plists."
  (let ((evidence (make-evidence texts))
        (checks '()))
    (dolist (sentence (sentence-list (detection-form (or text ""))))
      (when (cl-ppcre:scan "(?i)user|owner|用户|用戶|主人|老板|老闆" sentence)
        (setf checks (append checks (check-one-sentence sentence evidence scope)))))
    checks))
