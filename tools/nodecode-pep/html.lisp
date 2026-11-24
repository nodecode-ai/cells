;;;; html.lisp --- a PEP page as text, with one section a caller can name.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The published page's <section id="pep-content"> becomes plain text with
;;;; one "## Heading" line per section: the page header and the table of
;;;; contents are left out, code blocks fenced, inline code in backticks,
;;;; lists as "- " items and tables as "| " rows. SECTIONS records where each
;;;; heading's body starts, which is all that picking one section needs.
;;;;
;;;; One pass over the HTML: a tag is dispatched to START-TAG or END-TAG, the
;;;; text between tags to DATA-TEXT, and a skip counter drops the header, the
;;;; contents tree, scripts and nav.

(in-package #:nodecode-pep)

(defstruct (pep-section (:constructor make-pep-section (level id start body-start)))
  "One section of a PEP page: LEVEL from its nesting (2 for a top-level
section, 3 for a subsection), ID the page's own anchor, TITLE its heading as
one line, START and BODY-START where the heading and its body begin in the
page's text."
  (level 1 :type fixnum)
  (id "" :type string)
  (title "" :type string)
  (start 0 :type fixnum)
  (body-start 0 :type fixnum))

;;; --- small text helpers -----------------------------------------------------

(defun blank-char-p (char)
  "T when CHAR is one of the blanks a page's mark-up separates words with."
  (member char '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun split-on-blanks (text)
  "TEXT's words: split on runs of blanks, empty pieces dropped."
  (remove "" (uiop:split-string (or text "")
                                 :separator '(#\Space #\Tab #\Newline #\Return #\Page))
          :test #'string=))

(defun squeeze (text)
  "TEXT with every run of blanks as one space and none at either end."
  (format nil "~{~a~^ ~}" (split-on-blanks text)))

(defun starts-with-p (prefix text)
  (and (>= (length text) (length prefix)) (string= prefix text :end2 (length prefix))))

(defun ends-with-p (suffix text)
  (and (>= (length text) (length suffix))
       (string= suffix text :start2 (- (length text) (length suffix)))))
(defun squeeze-heading (title)
  "TITLE as one line, without the pilcrow or hash a page appends to a heading."
  (let ((text (squeeze title)))
    (loop while (or (ends-with-p "¶" text) (ends-with-p "#" text))
          do (setf text (squeeze (subseq text 0 (1- (length text))))))
    text))

(defparameter +entities+
  '(("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\"") ("apos" . "'")
    ("nbsp" . " ") ("mdash" . "—") ("ndash" . "–") ("hellip" . "…")
    ("lsquo" . "‘") ("rsquo" . "’") ("ldquo" . "“") ("rdquo" . "”")
    ("times" . "×") ("copy" . "©") ("reg" . "®") ("trade" . "™")
    ("deg" . "°") ("middot" . "·") ("bull" . "•") ("rarr" . "→")
    ("larr" . "←") ("le" . "≤") ("ge" . "≥") ("ne" . "≠") ("laquo" . "«")
    ("raquo" . "»"))
  "The character references a PEP page writes, by name.")

(defun entity-char (body)
  "The character BODY names — an entity's name, or #8212 / #x2014 — or NIL."
  (cond
    ((and (plusp (length body)) (char= (char body 0) #\#))
     (let ((digits (subseq body 1)))
       (when (plusp (length digits))
         (let ((code (if (member (char digits 0) '(#\x #\X) :test #'char=)
                         (ignore-errors (parse-integer digits :start 1 :radix 16))
                         (ignore-errors (parse-integer digits)))))
           (and code (plusp code) (< code 1114112) (code-char code))))))
    (t (cdr (assoc body +entities+ :test #'string=)))))

(defun decode-entities (text)
  "TEXT with its character references — &amp;, &#8212;, &#x2014; — as characters."
  (if (null (search "&" text))
      text
      (with-output-to-string (out)
        (let ((i 0) (n (length text)))
          (loop while (< i n) do
            (let ((amp (position #\& text :start i)))
              (cond
                ((null amp) (write-string (subseq text i) out) (setf i n))
                (t
                 (write-string (subseq text i amp) out)
                 (let ((semi (and (< (1+ amp) n)
                                  (position #\; text :start (1+ amp) :end (min n (+ amp 12))))))
                   (if (null semi)
                       (progn (write-char #\& out) (setf i (1+ amp)))
                       (let ((char (entity-char (subseq text (1+ amp) semi))))
                         (write-string (or char (subseq text amp (1+ semi))) out)
                         (setf i (1+ semi)))))))))))))

;;; --- the reader -------------------------------------------------------------

(defstruct (page-reader (:constructor %make-page-reader))
  "The text being built and where the reader is in the page."
  (parts '())                   ; reversed: the text as written, chunk by chunk
  (size 0)
  (tail "")                     ; the last two characters written
  (sections '())                ; reversed: PEP-SECTIONs, innermost first
  (depth 0)                     ; <section> nesting inside #pep-content; 0 = outside
  (skip nil)                    ; the tag whose whole subtree is being dropped
  (skip-level 0)
  (pre 0)
  (code 0)
  (title '())                   ; reversed: the heading being read
  (heading nil)                 ; whether a heading is being read
  (lists '())                   ; (tag . next-number), innermost first
  (cells 0)
  (in-row nil)
  (in-cell 0))

(defun write-text (reader text)
  "Append TEXT to READER's page text, and to the heading being read."
  (when (plusp (length text))
    (push text (page-reader-parts reader))
    (incf (page-reader-size reader) (length text))
    (let ((joined (concatenate 'string (page-reader-tail reader) text)))
      (setf (page-reader-tail reader) (subseq joined (max 0 (- (length joined) 2)))))
    (when (page-reader-heading reader)
      (push text (page-reader-title reader)))))

(defun newlines (reader count)
  "Make sure at least COUNT newlines end the text, as a block boundary wants."
  (when (plusp (page-reader-size reader))
    (let* ((tail (page-reader-tail reader))
           (have (- (length tail) (length (string-right-trim '(#\Newline) tail)))))
      (when (< have count)
        (write-text reader (make-string (- count have) :initial-element #\Newline))))))

(defun at-line-start-p (reader)
  "T when nothing has been written or the text ends with a newline."
  (let ((tail (page-reader-tail reader)))
    (or (zerop (page-reader-size reader))
        (char= #\Newline (char tail (1- (length tail)))))))

(defun tail-space-p (reader)
  "T when the text ends with a space."
  (let ((tail (page-reader-tail reader)))
    (and (plusp (length tail)) (char= #\Space (char tail (1- (length tail)))))))

(defun attr (attrs name)
  "ATTRS' value for NAME, or NIL."
  (cdr (assoc name attrs :test #'string=)))


;;; --- tags -------------------------------------------------------------------

(defun parse-tag (text)
  "TEXT, one start tag without its brackets => (values NAME ATTRS SELF-CLOSING-P)."
  (let* ((text (string-right-trim '(#\Space #\Tab #\Newline #\Return) text))
         (self (and (plusp (length text)) (char= #\/ (char text (1- (length text))))))
         (text (if self
                   (string-right-trim '(#\Space #\Tab) (subseq text 0 (1- (length text))))
                   text))
         (n (length text))
         (i (or (position-if #'blank-char-p text) n))
         (name (string-downcase (subseq text 0 i)))
         (attrs '()))
    (loop while (< i n) do
      (cond
        ((blank-char-p (char text i)) (incf i))
        (t
         (let ((start i))
           (loop while (and (< i n) (not (blank-char-p (char text i)))
                            (not (char= #\= (char text i))))
                 do (incf i))
           (let ((key (string-downcase (subseq text start i)))
                 (value ""))
             (when (and (< i n) (char= #\= (char text i)))
               (incf i)
               (when (< i n)
                 (if (member (char text i) '(#\" #\') :test #'char=)
                     (let* ((quote-char (char text i))
                            (close (position quote-char text :start (1+ i))))
                       (setf value (if close (subseq text (1+ i) close) (subseq text (1+ i)))
                             i (if close (1+ close) n)))
                     (let ((value-start i))
                       (loop while (and (< i n) (not (blank-char-p (char text i))))
                             do (incf i))
                       (setf value (subseq text value-start i))))))
             (when (plusp (length key))
               (push (cons key (decode-entities value)) attrs)))))))
    (values name (nreverse attrs) self)))

(defun skip-start (reader tag)
  "Drop everything REader reads until TAG's own end tag."
  (setf (page-reader-skip reader) tag
        (page-reader-skip-level reader) 1))

(defun heading-level (tag)
  "The level a heading TAG (h2 ... h6) writes when no section encloses it."
  (or (digit-char-p (char tag 1)) 2))

(defun start-tag (reader tag attrs)
  "One start tag: the content boundary, a skipped subtree, a section or a block."
  (let ((id (or (attr attrs "id") "")))
    (cond
      ((page-reader-skip reader)
       (when (string= tag (page-reader-skip reader))
         (incf (page-reader-skip-level reader))))
      ((and (zerop (page-reader-depth reader)) (string= tag "section") (string= id "pep-content"))
       (setf (page-reader-depth reader) 1))
      ((zerop (page-reader-depth reader)) nil)
      ((or (member tag '("h1" "script" "style" "nav") :test #'string=)
           (and (string= tag "section") (string= id "contents"))
           (and (string= tag "dl")
                (member "rfc2822" (split-on-blanks (attr attrs "class")) :test #'string=)))
       (skip-start reader tag))
      ((string= tag "section")
       (incf (page-reader-depth reader))
       (newlines reader 2)
       (push (make-pep-section (page-reader-depth reader) id
                                (page-reader-size reader) (page-reader-size reader))
             (page-reader-sections reader)))
      ((member tag '("h2" "h3" "h4" "h5" "h6") :test #'string=)
       (newlines reader 2)
       (let ((level (if (page-reader-sections reader)
                        (pep-section-level (first (page-reader-sections reader)))
                        (or (heading-level tag) 2))))
         (write-text reader (concatenate 'string (make-string level :initial-element #\#) " "))
         (setf (page-reader-title reader) '()
               (page-reader-heading reader) t)))
      ((string= tag "pre")
       (newlines reader 2)
       (write-text reader (format nil "```~%"))
       (incf (page-reader-pre reader)))
      ((and (string= tag "code") (zerop (page-reader-pre reader)))
       (incf (page-reader-code reader))
       (write-text reader "`"))
      ((string= tag "br") (write-text reader (string #\Newline)))
      ((and (string= tag "img") (attr attrs "alt"))
       (write-text reader (format nil "[image: ~a]" (attr attrs "alt"))))
      ((member tag '("ul" "ol") :test #'string=)
       (newlines reader 1)
       (push (cons tag 1) (page-reader-lists reader)))
      ((string= tag "li")
       (newlines reader 1)
       (let ((indent (make-string (* 2 (max 0 (1- (length (page-reader-lists reader)))))
                                   :initial-element #\Space))
             (top (first (page-reader-lists reader)))
             (marker "- "))
         (when (and top (string= (car top) "ol"))
           (setf marker (format nil "~d. " (cdr top)))
           (incf (cdr top)))
         (write-text reader (concatenate 'string indent marker))))
      ((string= tag "tr")
       (newlines reader 1)
       (write-text reader "| ")
       (setf (page-reader-cells reader) 0
             (page-reader-in-row reader) t))
      ((member tag '("td" "th") :test #'string=)
       (when (plusp (page-reader-cells reader)) (write-text reader " | "))
       (incf (page-reader-cells reader))
       (incf (page-reader-in-cell reader)))
      ((string= tag "dd")
       (newlines reader 1)
       (write-text reader "  "))
      ((member tag '("dt" "figcaption") :test #'string=) (newlines reader 1))
      ((member tag '("p" "div" "dl" "table" "blockquote" "figure" "details" "summary"
                     "aside")
               :test #'string=)
       (newlines reader 2)))))

(defun end-tag (reader tag)
  "One end tag: what START-TAG opened, closed."
  (cond
    ((page-reader-skip reader)
     (when (string= tag (page-reader-skip reader))
       (decf (page-reader-skip-level reader))
       (when (zerop (page-reader-skip-level reader))
         (setf (page-reader-skip reader) nil))))
    ((zerop (page-reader-depth reader)) nil)
    ((string= tag "section")
     (decf (page-reader-depth reader))
     (newlines reader 2))
    ((member tag '("h2" "h3" "h4" "h5" "h6") :test #'string=)
     (when (and (page-reader-heading reader) (page-reader-sections reader))
       (setf (pep-section-title (first (page-reader-sections reader)))
             (squeeze-heading (format nil "~{~a~^~}" (reverse (page-reader-title reader))))))
     (setf (page-reader-title reader) '()
            (page-reader-heading reader) nil)
     (newlines reader 1)
     (when (page-reader-sections reader)
       (setf (pep-section-body-start (first (page-reader-sections reader)))
             (page-reader-size reader)))
     (newlines reader 2))
    ((string= tag "pre")
     (when (plusp (page-reader-pre reader))
       (decf (page-reader-pre reader))
       (newlines reader 1)
       (write-text reader "```")
       (newlines reader 2)))
    ((string= tag "code")
     (when (plusp (page-reader-code reader))
       (decf (page-reader-code reader))
       (write-text reader "`")))
    ((member tag '("td" "th") :test #'string=)
     (setf (page-reader-in-cell reader) (max 0 (1- (page-reader-in-cell reader)))))
    ((string= tag "tr")
     (when (page-reader-in-row reader)
       (setf (page-reader-in-row reader) nil)
       (write-text reader " |")
       (newlines reader 1)))
    ((member tag '("ul" "ol") :test #'string=)
     (when (page-reader-lists reader) (pop (page-reader-lists reader)))
     (newlines reader (if (page-reader-lists reader) 1 2)))
    ((member tag '("dt" "dd" "figcaption") :test #'string=) (newlines reader 1))
    ((member tag '("p" "div" "dl" "table" "blockquote" "figure" "details" "summary"
                   "aside")
             :test #'string=)
     (newlines reader 2))))

(defun data-text (reader data)
  "The text between tags: in the page, collapsed unless it is inside <pre>."
  (when (and (null (page-reader-skip reader)) (plusp (page-reader-depth reader)))
    (if (plusp (page-reader-pre reader))
        (write-text reader data)
        (let ((text (squeeze data)))
          (cond
            ((string= text "")
             (unless (or (and (page-reader-in-row reader) (zerop (page-reader-in-cell reader)))
                         (zerop (length data))
                         (at-line-start-p reader)
                         (tail-space-p reader))
               (write-text reader " ")))
            (t
             (when (and (blank-char-p (char data 0))
                        (not (at-line-start-p reader))
                        (not (tail-space-p reader)))
               (setf text (concatenate 'string " " text)))
             (when (blank-char-p (char data (1- (length data))))
               (setf text (concatenate 'string text " ")))
             (write-text reader text)))))))

(defun read-tag (reader text)
  "Dispatch one tag, TEXT the characters between its < and >."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (cond
      ((zerop (length text)) nil)
      ((char= #\! (char text 0)) nil)
      ((char= #\/ (char text 0))
       (end-tag reader (string-downcase (subseq text 1 (or (position-if #'blank-char-p text) (length text))))))
      (t (multiple-value-bind (name attrs self)
             (parse-tag text)
           (start-tag reader name attrs)
           (when self (end-tag reader name)))))))

(defun join-parts (reader)
  "READER's text whole."
  (format nil "~{~a~}" (nreverse (page-reader-parts reader))))

(defun parse-page (html)
  "HTML, a PEP's published page => (values TEXT SECTIONS): the text of its
pep-content with one \"## Heading\" line per section, and the SECTIONS that
have a heading, in page order."
  (let ((reader (%make-page-reader))
        (i 0)
        (n (length html)))
    (loop while (< i n) do
      (let ((lt (position #\< html :start i)))
        (cond
          ((null lt)
           (data-text reader (decode-entities (subseq html i)))
           (setf i n))
          (t
           (when (> lt i) (data-text reader (decode-entities (subseq html i lt))))
           (cond
             ((starts-with-p "<!--" (subseq html lt (min n (+ lt 4))))
              (let ((end (search "-->" html :start2 lt)))
                (setf i (if end (+ end 3) n))))
             ((char= #\! (char html (1+ lt)))
              (let ((gt (position #\> html :start lt)))
                (setf i (if gt (1+ gt) n))))
             (t
              (let ((gt (position #\> html :start lt)))
                (if (null gt)
                    (setf i n)
                    (progn (read-tag reader (subseq html (1+ lt) gt))
                           (setf i (1+ gt)))))))))))
    (values (join-parts reader)
            (remove "" (nreverse (page-reader-sections reader))
                    :key #'pep-section-title :test #'string=))))

;;; --- one section out of it ---------------------------------------------------

(defun collapse-blank-lines (text)
  "TEXT with runs of three or more newlines as two."
  (with-output-to-string (out)
    (let ((i 0) (n (length text)))
      (loop while (< i n) do
        (if (char= #\Newline (char text i))
            (let ((j i))
              (loop while (and (< j n) (char= #\Newline (char text j))) do (incf j))
              (write-string (if (>= (- j i) 3)
                                (format nil "~2%")
                                (make-string (- j i) :initial-element #\Newline))
                            out)
              (setf i j))
            (progn (write-char (char text i) out) (incf i)))))))

(defun clean-text (text)
  "TEXT without trailing blanks on a line, with no run of blank lines, trimmed."
  (let ((lines (mapcar (lambda (line)
                         (string-right-trim '(#\Space #\Tab #\Return) line))
                       (uiop:split-string text :separator '(#\Newline)))))
    (string-trim '(#\Space #\Tab #\Newline #\Return)
                 (collapse-blank-lines (format nil "~{~a~^~%~}" lines)))))

(defun spaced (text)
  "TEXT with underscores as spaces and every blank run as one space."
  (squeeze (substitute #\Space #\_ (or text ""))))

(defun section-key (text)
  "TEXT for matching a heading: case, underscores, dashes and blank runs do not count."
  (string-downcase (squeeze (substitute #\Space #\-
                                         (substitute #\Space #\_ (or text ""))))))

(defun find-section-index (sections wanted)
  "The index in SECTIONS of the section WANTED names, or NIL. An empty
WANTED is the first top-level section, usually the Abstract; else a heading
that is exactly WANTED, then one that differs only in case, _ or -, or the id
in the page's own link."
  (when sections
    (if (string= wanted "")
        (let ((top (reduce #'min sections :key #'pep-section-level)))
          (position-if (lambda (section) (= top (pep-section-level section))) sections))
        (or (position-if (lambda (section) (string= (spaced (pep-section-title section)) wanted))
                         sections)
            (let ((key (section-key wanted)))
              (position-if (lambda (section)
                             (or (string= (section-key (pep-section-title section)) key)
                                 (string= (section-key (pep-section-id section)) key)))
                           sections))))))

(defun section-path (sections index)
  "The headings leading to SECTIONS' INDEXth section, outermost first."
  (let ((path (list (pep-section-title (nth index sections))))
        (level (pep-section-level (nth index sections))))
    (loop for section in (reverse (subseq sections 0 index))
          do (when (< (pep-section-level section) level)
               (push (pep-section-title section) path)
               (setf level (pep-section-level section))))
    path))

(defun section-body (text sections index)
  "SECTIONS' INDEXth section out of TEXT: its body and its subsections',
to the next heading of the same or a higher level."
  (let* ((section (nth index sections))
         (end (or (loop for other in (nthcdr (1+ index) sections)
                        when (<= (pep-section-level other) (pep-section-level section))
                          return (pep-section-start other))
                  (length text))))
    (clean-text (subseq text (pep-section-body-start section) end))))

(defun top-level-titles (sections)
  "SECTIONS' top-level headings, in page order."
  (let ((top (reduce #'min sections :key #'pep-section-level :initial-value 99)))
    (loop for section in sections
          when (= top (pep-section-level section))
            collect (pep-section-title section))))

(defun heading-line-p (line)
  "T when LINE is one of the page reader's own \"## \" headings."
  (let ((line (string-left-trim '(#\Space #\Tab) line)))
    (and (plusp (length line)) (char= #\# (char line 0)))))

(defun prose-p (body)
  "T when BODY holds more than its heading lines."
  (some (lambda (line)
          (let ((line (string-left-trim '(#\Space #\Tab) line)))
            (and (plusp (length line)) (not (heading-line-p line)))))
        (uiop:split-string (or body "") :separator '(#\Newline))))
