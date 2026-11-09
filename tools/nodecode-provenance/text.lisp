;;;; text.lisp --- sentences, quoted spans, and verbatim matching.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Rewritten from the original plugin's claims.py (sentence splitting) and
;;;; quotes.py (quoted spans, matching, content tokens). Quotes match the
;;;; user's own words verbatim: NFKC-ish folding, collapsed whitespace, case
;;;; folded, straight and curly quotes alike, the quote's trailing
;;;; punctuation ignored. Nothing else is forgiven.

(in-package #:nodecode-provenance)

;;; --- detection form -----------------------------------------------------------

(defun invisible-char-p (ch)
  "Whether CH is a default-ignorable character, which only ever hides a word."
  (let ((c (char-code ch)))
    (or (= c #x00AD) (= c #x034F) (= c #x061C) (= c #x115F) (= c #x1160)
        (<= #x17B4 c #x17B5) (<= #x200B c #x200F) (<= #x202A c #x202E)
        (<= #x2060 c #x206F) (= c #xFEFF) (= c #x3164) (<= #xFE00 c #xFE0F)
        (<= #x1D173 c #x1D17A))))

(defun detection-form (text)
  "TEXT as claim detection sees it: full-width letters folded, invisibles dropped."
  (with-output-to-string (out)
    (loop for ch across (or text "")
          for c = (char-code ch)
          unless (invisible-char-p ch)
            do (write-char (cond ((<= #xFF01 c #xFF5E) (code-char (- c #xFEE0)))
                                 ((= c #x3000) #\Space)
                                 (t ch))
                           out))))

;;; --- sentences -----------------------------------------------------------------

(defparameter +sentence-enders+ ".!?;。！？；"
  "Characters a sentence ends on, outside a quotation.")

(defun quoted-state-step (ch in-straight closers)
  "The quote state after CH => (VALUES IN-STRAIGHT CLOSERS)."
  (cond ((char= ch #\") (values (not in-straight) closers))
        ((find ch "“「『")
         (values in-straight (cons (case ch ((code-char #x201C) (code-char #x201D)) ((code-char #x300C) (code-char #x300D)) (t (code-char #x300F))) closers)))
        ((and closers (char= ch (first closers))) (values in-straight (rest closers)))
        (t (values in-straight closers))))

(defun split-segment (text)
  "TEXT split on sentence ends that are not inside a quotation."
  (let ((sentences '()) (start 0) (in-straight nil) (closers '()))
    (loop for i from 0 below (length text)
          for ch = (char text i)
          do (if (find ch "\"“「『")
                 (multiple-value-setq (in-straight closers)
                   (quoted-state-step ch in-straight closers))
                 (when (and (not in-straight) (null closers) (find ch +sentence-enders+))
                   (push (subseq text start (1+ i)) sentences)
                   (setf start (1+ i)))))
    (push (subseq text start) sentences)
    (loop for sentence in (nreverse sentences)
          for trimmed = (string-trim '(#\Space #\Tab #\Newline #\Return) sentence)
          when (plusp (length trimmed)) collect trimmed)))

(defun line-text (line)
  "LINE without its Markdown list, heading or quote marker."
  (string-trim '(#\Space #\Tab)
               (cl-ppcre:regex-replace "^\\s*(?:[-*+]|\\d+[.)]|#{1,6}|>)\\s+" line "")))

(defun sentence-list (text)
  "TEXT split into prose sentences: fenced and indented code and table rows are
left out, one paragraph's hard-wrapped lines are joined, and each sentence is
split on its own ends. A list of strings."
  (let ((sentences '()) (buffer '()) (in-fence nil))
    (labels ((flush ()
               (when buffer
                 (dolist (sentence (split-segment (format nil "~{~a~^ ~}" (nreverse buffer))))
                   (push sentence sentences))
                 (setf buffer '()))))
      (dolist (raw (cl-ppcre:split "\\n" (cl-ppcre:regex-replace-all "\\r" (or text "") "")))
        (cond ((cl-ppcre:scan "^\\s*```" raw) (flush) (setf in-fence (not in-fence)))
              ((cl-ppcre:scan "^\\s*$" raw) (flush))
              (in-fence nil)
              ((cl-ppcre:scan "^\\s{4,}\\S" raw) (flush))
              ((cl-ppcre:scan "^\\s*\\|" raw) (flush))
              (t (push (line-text raw) buffer))))
      (flush))
    (nreverse sentences)))

;;; --- quoted spans --------------------------------------------------------------

(defun span-found-p (text start end)
  "Whether (START END) may be kept: it holds something, and overlaps no kept span."
  (and (> end (1+ start)) (plusp (length (string-trim '(#\Space) (subseq text (1+ start) (1- end)))))))

(defun find-pairs (text openers closers)
  "((START END INNER) ...) for OPENERS paired with the next CLOSER, no newline inside."
  (let ((spans '()) (start 0))
    (loop
      (let ((a (position-if (lambda (ch) (find ch openers)) text :start start)))
        (unless a (return))
        (let ((b (position-if (lambda (ch) (find ch closers)) text :start (1+ a))))
          (unless b (return))
          (let ((inner (subseq text (1+ a) b)))
            (when (and (plusp (length (string-trim '(#\Space #\Newline) inner)))
                       (not (find #\Newline inner)))
              (push (list a (1+ b) inner) spans)))
          (setf start (1+ b)))))
    (nreverse spans)))

(defun double-quote-spans (text)
  "Every pair of double quote marks around something, taking them in order."
  (let ((positions (loop for i from 0 below (length text)
                         when (find (char text i) "\"“”„") collect i))
        (spans '()))
    (loop for (a b) on positions by #'cddr
          when b
            do (let ((inner (subseq text (1+ a) b)))
                 (when (and (plusp (length (string-trim '(#\Space #\Newline) inner)))
                            (null (find #\Newline inner)))
                   (push (list a (1+ b) inner) spans))))
    (nreverse spans)))

(defun single-quote-spans (text)
  "Single quotes around two or more words, never an apostrophe inside a word."
  (let ((positions (loop for i from 0 below (length text)
                         when (and (find (char text i) "'‘’")
                                   (not (and (plusp i) (< i (1- (length text)))
                                             (alphanumericp (char text (1- i)))
                                             (alphanumericp (char text (1+ i))))))
                           collect i))
        (spans '()))
    (loop for (a b) on positions by #'cddr
          when b
            do (let ((inner (subseq text (1+ a) b)))
                 (when (and (find #\Space (string-trim '(#\Space) inner))
                            (null (find #\Newline inner)))
                   (push (list a (1+ b) inner) spans))))
    (nreverse spans)))

(defun quote-spans (text)
  "((START END INNER) ...) for every quoted span, in order; overlaps dropped."
  (let ((found (append (double-quote-spans text)
                       (single-quote-spans text)
                       (find-pairs text "「" "」")
                       (find-pairs text "『" "』"))))
    (loop with spans = '()
          for span in (sort found #'< :key #'first)
          when (and (plusp (length (string-trim '(#\Space) (third span))))
                    (or (null spans) (>= (first span) (second (first spans)))))
            do (push span spans)
          finally (return (nreverse spans)))))

(defun extract-quotes (text)
  (mapcar #'third (quote-spans text)))

(defun remove-spans (text spans)
  "TEXT with SPANS replaced by a neutral placeholder word."
  (let ((out '()) (last 0))
    (dolist (span (sort (copy-list spans) #'< :key #'first))
      (push (subseq text last (first span)) out)
      (push " Q " out)
      (setf last (second span)))
    (push (subseq text last) out)
    (apply #'concatenate 'string (nreverse out))))

;;; --- matching -----------------------------------------------------------------

(defun normalize (text)
  "The matching form of TEXT: folded, whitespace collapsed, case folded."
  (string-downcase
   (string-trim '(#\Space)
                (cl-ppcre:regex-replace-all "\\s+" 
                  (cl-ppcre:regex-replace-all "[“”„]" 
                    (cl-ppcre:regex-replace-all "[‘’]" (detection-form text) "'")
                    "\"")
                  " "))))

(defun normalize-quote (text)
  (cl-ppcre:regex-replace "\\s*[.,!?;:。，！？；：、…]+$" (normalize text) ""))

(defun cjk-char-p (ch)
  (let ((c (char-code ch)))
    (or (<= #x3400 c #x9FFF) (<= #xF900 c #xFAFF) (<= #x3040 c #x30FF) (<= #xAC00 c #xD7AF))))

(defun latin-word-char-p (ch)
  (and (alphanumericp ch) (not (cjk-char-p ch))))

(defparameter +joiners+ "-_./'")

(defun word-continues-p (text index step)
  "The Latin word at the edge of a match continues at TEXT[INDEX]."
  (when (and (<= 0 index) (< index (length text)))
    (if (latin-word-char-p (char text index))
        t
        (let ((next (+ index step)))
          (and (find (char text index) +joiners+)
               (<= 0 next) (< next (length text))
               (latin-word-char-p (char text next)))))))

(defun occurrences (quote-norm message-norm)
  "Start offsets where QUOTE-NORM occurs in MESSAGE-NORM without cutting a word."
  (let ((found '()) (start 0))
    (when (plusp (length quote-norm))
      (loop
        (let ((at (search quote-norm message-norm :start2 start)))
          (unless at (return))
          (let ((end (+ at (length quote-norm))))
            (unless (or (and (latin-word-char-p (char quote-norm 0))
                             (word-continues-p message-norm (1- at) -1))
                        (and (latin-word-char-p (char quote-norm (1- (length quote-norm))))
                             (word-continues-p message-norm end 1)))
              (push at found)))
          (setf start (1+ at)))))
    (nreverse found)))

(defun occurs-in-p (quote-norm message-norm)
  (and (occurrences quote-norm message-norm) t))

(defun quote-size (text)
  "CJK characters plus Latin words: a pure-Latin quote counts words, a CJK one characters."
  (let ((cjk 0) (latin 0) (in-word nil))
    (loop for ch across text
          do (if (cjk-char-p ch)
                 (progn (incf cjk) (setf in-word nil))
                 (if (alphanumericp ch)
                     (unless in-word (incf latin) (setf in-word t))
                     (setf in-word nil))))
    (+ cjk latin)))

(defun short-quote-p (quote)
  (< (quote-size quote) +short-quote-limit+))

;;; --- negation, questions -------------------------------------------------------

(defun negation-word-p (word)
  (or (member word +en-negations+ :test #'string=)
      (cl-ppcre:scan "(?:n['’]t)$" word)))

(defun has-negation-p (text-norm)
  (or (some (lambda (ch) (member ch +zh-negation-chars+ :test #'char=)) text-norm)
      (some #'negation-word-p (cl-ppcre:all-matches-as-strings "[a-z]+(?:['’][a-z]+)?" text-norm))))

(defparameter +sentence-end-char-p+ ".!?;。！？；")

(defun negated-at-p (message-norm start)
  "A negation word earlier in the same sentence, before START: \"Never delete the prod
database\" does not back a quote of \"delete the prod database\"."
  (let* ((from (max 0 (- start 200)))
         (window (subseq message-norm from start))
         (cut (loop for i from (1- (length window)) downto 0
                    when (find (char window i) +sentence-end-char-p+)
                      return (1+ i)))
         (before (if cut (subseq window cut) window)))
    (or (some (lambda (ch) (member ch +zh-negation-chars+ :test #'char=))
              (let ((n (length before))) (subseq before (max 0 (- n 6)))))
        (some #'negation-word-p
              (last (cl-ppcre:all-matches-as-strings "[a-z]+(?:['’][a-z]+)?" before)
                    +quote-leading-words+)))))

(defun leads-with-negation-p (quote)
  "The quote's first few words (or characters) hold a negation."
  (let ((norm (normalize quote)))
    (or (some #'negation-word-p
              (subseq (cl-ppcre:all-matches-as-strings "[a-z]+(?:['’][a-z]+)?" norm)
                      0 (min +quote-leading-words+
                             (length (cl-ppcre:all-matches-as-strings "[a-z]+(?:['’][a-z]+)?" norm)))))
        (some (lambda (ch) (member ch +zh-negation-chars+ :test #'char=))
              (subseq norm 0 (min 4 (length norm)))))))

(defun in-question-p (message-norm end)
  "The sentence holding the span that ends at END asks a question."
  (let* ((rest (subseq message-norm end))
         (stop (position-if (lambda (ch) (find ch +sentence-end-char-p+)) rest))
         (tail (if stop (subseq rest 0 (1+ stop)) rest))
         (trimmed (string-right-trim '(#\Space) tail)))
    (and (plusp (length trimmed)) (find (char trimmed (1- (length trimmed))) "?？"))))

;;; --- relatedness ---------------------------------------------------------------

(defun cjk-runs (text)
  "The runs of CJK characters in TEXT."
  (let ((runs '()) (current '()))
    (flet ((flush () (when current (push (coerce (nreverse current) 'string) runs) (setf current '()))))
      (loop for ch across text
            do (if (cjk-char-p ch) (push ch current) (flush)))
      (flush))
    (nreverse runs)))

(defun content-tokens (text)
  "Latin words of 3+ letters minus stopwords, and CJK character bigrams."
  (let ((norm (normalize text)) (tokens '()))
    (dolist (word (cl-ppcre:all-matches-as-strings "[^\\W\\d_]+"
                     (cl-ppcre:regex-replace-all "[぀-ヿ㐀-鿿가-힯豈-﫿]" norm " ")))
      (when (and (>= (length word) 3) (not (gethash word *stopwords*)))
        (pushnew word tokens :test #'string=)))
    (dolist (run (cjk-runs norm))
      (loop for i from 0 below (1- (length run))
            for bigram = (subseq run i (+ i 2))
            unless (member bigram +zh-stop-bigrams+ :test #'string=)
              do (pushnew bigram tokens :test #'string=)))
    tokens))

(defun related-p (quote claim-without-quote)
  (>= (length (intersection (content-tokens quote) (content-tokens claim-without-quote)
                            :test #'string=))
      +min-shared-tokens+))
