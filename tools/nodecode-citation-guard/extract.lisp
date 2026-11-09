;;;; extract.lisp --- references out of text: ids, entries, BibTeX.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Deterministic and LLM-free, as the original was: a reference is found by
;;;; its identifiers (a DOI, a PMID, an arXiv id) or by its shape (a line with
;;;; an author, a year and a title). Nothing here talks to anything.

(in-package #:nodecode-citation-guard)

;;; --- words -------------------------------------------------------------------

(defparameter +stop+
  '("a" "an" "the" "of" "and" "in" "on" "for" "to" "with" "at" "by"
    "from" "is" "are" "as" "its" "into" "via" "using" "we" "our" "de"
    "la" "le" "et" "al" "this" "that" "these" "those" "not" "be" "was")
  "Words that carry no title: dropped before two titles are compared.")

(defun ascii-fold (char)
  "CHAR lowercased, with the accented Latin letters folded to their base."
  (let ((code (char-code char)))
    (cond ((< code 128) (char-downcase char))
          ((<= #xC0 code #xC5) #\a)
          ((<= #xC8 code #xCB) #\e)
          ((<= #xCC code #xCF) #\i)
          ((<= #xD2 code #xD6) #\o)
          ((<= #xD9 code #xDC) #\u)
          ((= code #xC7) #\c)
          ((= code #xD1) #\n)
          ((= code #xDF) #\s)
          (t (char-downcase char)))))

(defun words (text)
  "TEXT's words, folded and lowercased: letters and digits only."
  (let ((word (make-string-output-stream)) (out '()))
    (flet ((flush () (let ((w (get-output-stream-string word)))
                       (when (plusp (length w)) (push w out)))))
      (loop for char across (or text "")
            do (if (or (alphanumericp char) (> (char-code char) 127))
                   (write-char (ascii-fold char) word)
                   (flush)))
      (flush))
    (nreverse out)))

(defun content-words (text)
  "TEXT's words that a title is made of: no stop words, nothing shorter than
three letters unless it is a number."
  (remove-if (lambda (word)
               (or (member word +stop+ :test #'string=)
                   (and (< (length word) 3)
                        (not (every #'digit-char-p word)))))
             (words text)))

(defun coverage (of in)
  "The share of OF's distinct words that also stand in IN's, 0 to 1."
  (let ((want (remove-duplicates of :test #'string=))
        (have (remove-duplicates in :test #'string=)))
    (if (null want)
        0.0
        (/ (float (count-if (lambda (word) (member word have :test #'string=)) want))
           (length want)))))

;;; --- identifiers ---------------------------------------------------------------

(defun doi-char-p (char)
  (or (alphanumericp char) (find char "._/;:()<>-+*'")))

(defun find-dois (text)
  "Every DOI in TEXT, in the order it stands, without the resolver in front."
  (let ((found '()) (start 0))
    (loop
      (let ((at (search "10." text :start2 start)))
        (unless at (return (nreverse found)))
        (let ((after (+ at 3)))
          ;; four to nine digits, then a slash
          (let ((digits (loop for i from after below (min (length text) (+ after 9))
                              while (digit-char-p (char text i)) count 1))
                (end nil))
            (if (and (<= 4 digits) (< (+ after digits) (length text))
                     (char= (char text (+ after digits)) #\/))
                (progn
                  (setf end (+ after digits 1))
                  (loop while (and (< end (length text)) (doi-char-p (char text end)))
                        do (incf end))
                  (let ((doi (subseq text at end)))
                    ;; a trailing period or comma is the sentence's, not the DOI's
                    (setf doi (string-right-trim ".,;:" doi))
                    (when (> (length doi) 7) (push doi found)))
                  (setf start (max end (+ at 3))))
                (setf start (+ at 3)))))))))

(defun digits-after (text markers)
  "The first run of seven to nine digits standing after one of MARKERS."
  (loop for marker in markers
        for at = (search marker text :test #'char-equal)
        do (when at
             (let ((i at))
               (loop while (and (< i (length text))
                                (not (digit-char-p (char text i)))
                                (< (- i at) 12))
                     do (incf i))
               (let ((end (loop for j from i below (length text)
                                while (digit-char-p (char text j)) count 1)))
                 (when (<= 7 end 9)
                   (return (subseq text i (+ i end)))))))))

(defun find-pmids (text) (remove-duplicates (remove nil (list (digits-after text '("PMID" "PubMed")))) :test #'string=))

(defun arxiv-id-p (text)
  "Whether TEXT looks like an arXiv id: 2401.12345, or the older math.GT/0309136."
  (or (and (>= (length text) 9)
           (loop for i from 0 below 4 always (digit-char-p (char text i)))
           (char= (char text 4) #\.)
           (loop for i from 5 below (length text)
                 always (or (digit-char-p (char text i)) (char= (char text i) #\v))))
      (and (find #\/ text)
           (loop for char across text
                 always (or (alphanumericp char) (find char "./-'") ))
           (let ((year (subseq text (1+ (position #\/ text)))))
             (and (>= (length year) 7)
                  (loop for i from 0 below 4 always (digit-char-p (char year i))))))))

(defun find-arxivs (text)
  "Every arXiv id TEXT names, with or without the arXiv: prefix."
  (let ((found '()) (start 0))
    (loop
      (let ((at (search "arxiv" text :start2 start :test #'char-equal)))
        (unless at (return (nreverse found)))
        (let* ((i (+ at 5))
               (i (if (and (< i (length text)) (char= (char text i) #\:)) (1+ i) i)))
          (loop while (and (< i (length text)) (member (char text i) '(#\Space #\Tab #\:)))
                do (incf i))
          (let ((end (loop for j from i below (min (length text) (+ i 24))
                           while (and (not (member (char text j) '(#\Space #\Tab #\, #\) #\] #\;)))
                                      (or (alphanumericp (char text j)) (find (char text j) "./-'")))
                           count 1)))
            (let ((id (subseq text i (min (length text) (+ i end)))))
              (when (arxiv-id-p id) (push id found)))
            (setf start (max (+ at 5) (+ i end)))))))))

;;; --- entries ------------------------------------------------------------------

(defun numbering-stripped (line)
  "LINE without the list numbering a reference list gives it: 1. [2] - *3*."
  (string-left-trim " \t"
                     (string-left-trim "*_)]"
                                      (string-left-trim "0123456789.[]*)-" line))))

(defun find-year (text)
  "The publication year TEXT names, or NIL."
  (loop for word in (words text)
        when (and (= (length word) 4) (every #'digit-char-p word)
                  (<= 1900 (parse-integer word) 2035))
          return (parse-integer word)))

(defun find-surname (text)
  "The first author's surname, as a reference list writes it, or NIL."
  (let ((head (string-left-trim " \t" (or text ""))))
    (loop for i from 0 below (length head)
          for char = (char head i)
          until (find char ",;.(")
          finally
             (let ((name (subseq head 0 i)))
               (return (and (plusp (length name)) (< (length name) 40)
                            (every (lambda (c) (or (alpha-char-p c) (char= c #\-) (char= c #\Space) (char= c #\'))) name)
                            (first (words name))))))))

(defun reference-entry (raw index)
  "RAW, one line of a reference list, as a citation plist."
  (let* ((line (numbering-stripped raw))
         (doi (first (find-dois line)))
         (pmid (first (find-pmids line)))
         (arxiv (first (find-arxivs line))))
    (list :index index :raw (string-trim " \t" (or raw "")) :text line
          :doi doi :pmid pmid :arxiv arxiv
          :year (find-year line) :author (find-surname line))))

(defun reference-line-p (line)
  "Whether LINE is a full reference entry rather than prose."
  (let* ((line (string-trim " \t" line))
         (words (words line)))
    (and (>= (length line) 24)
         (>= (length words) 6)
         (or (find-year line) (find-dois line) (find-pmids line) (find-arxivs line)))))

(defun reference-lines (text)
  "TEXT's lines that read as reference entries, as strings."
  (let ((lines (uiop:split-string (or text "") :separator '(#\Newline))))
    (remove-if-not #'reference-line-p lines)))

(defun extract-citations (text &key (max-items 40))
  "TEXT's checkable references, at most MAX-ITEMS, each a plist.

A block with a DOI, a PMID or an arXiv id is one reference; otherwise each
reference-list line is one. Prose is not a reference: a line needs an author,
a year and a title's worth of words before it counts."
  (let ((entries '()) (index 0))
    (dolist (line (reference-lines text))
      (when (< index max-items)
        (push (reference-entry line index) entries)
        (incf index)))
    (nreverse entries)))

;;; --- BibTeX --------------------------------------------------------------------

(defun bibtex-entries (text &key (max-items 40))
  "TEXT's BibTeX entries as citations: the fields a registry check needs."
  (let ((entries '()) (start 0) (index 0))
    (loop
      (let ((at (search "@" text :start2 start)))
        (unless at (return (nreverse entries)))
        (let* ((open (position #\{ text :start at))
               (close (and open (position #\} text :start open))))
          (if (null close)
              (return (nreverse entries))
              (let* ((body (subseq text open (1+ close)))
                     (doi (first (find-dois body)))
                     (year (bib-field body "year"))
                     (author (bib-field body "author"))
                     (title (bib-field body "title")))
                (when (< index max-items)
                  (push (list :index index
                              :raw (format nil "~@[~a. ~]~@[~a. ~]~@[~a~]" author title year)
                              :text title :doi doi :pmid nil :arxiv nil
                              :year (and year (find-year year))
                              :author (and author (find-surname (string-replace-all author " and " " "))))
                        entries)
                  (incf index))
                (setf start (1+ close)))))))))

(defun bib-field (body name)
  "The value of field NAME in a BibTeX entry body, or NIL."
  (let ((at (search name body :test #'char-equal)))
    (when at
      (let* ((eq-at (position #\= body :start at))
             (open (and eq-at (position #\{ body :start eq-at)))
             (close (and open (position #\} body :start open))))
        (when (and open close (< (- close open) 400))
          (string-trim " \t{}" (subseq body (1+ open) close)))))))

(defun string-replace-all (text old new)
  (with-output-to-string (out)
    (let ((start 0))
      (loop
        (let ((at (search old text :start2 start)))
          (if at
              (progn (write-string text out :start start :end at)
                     (write-string new out)
                     (setf start (+ at (length old))))
              (progn (write-string text out :start start) (return))))))))

(defun bibtex-p (text)
  "Whether TEXT holds BibTeX entries rather than a plain reference list."
  (and (search "@" text) (some (lambda (kind) (search kind text :test #'char-equal))
                                 '("@article" "@book" "@inproceedings" "@misc" "@techreport" "@phdthesis"))))

(defun citations-from (text &key (max-items 40))
  "TEXT checked the way its shape asks: BibTeX when it is BibTeX, a reference
list otherwise."
  (if (bibtex-p text)
      (bibtex-entries text :max-items max-items)
      (extract-citations text :max-items max-items)))
