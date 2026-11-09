;;;; office.lisp --- what a Word, PowerPoint and Excel file says, and how its
;;;; text changes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No Office library sits under this: a document is its XML parts, read as
;;;; text by one scanner and edited by rewriting the one element a change
;;;; touches. A Word edit can leave a tracked change (w:del + w:ins) so a
;;;; reviewer accepts or rejects it in Word.

(in-package #:nodecode-office)

;;; --- XML, read as text -------------------------------------------------------

(defun xml-unescape (text)
  (with-output-to-string (out)
    (loop with index = 0 and length = (length text)
          while (< index length)
          for char = (char text index)
          do (if (char= char #\&)
                 (let ((semicolon (position #\; text :start index :end (min length (+ index 12)))))
                   (if semicolon
                       (let ((name (subseq text (1+ index) semicolon)))
                         (write-string (or (cdr (assoc name '(("amp" . "&") ("lt" . "<") ("gt" . ">")
                                                          ("quot" . "\"") ("apos" . "'"))
                                                       :test #'string=))
                                             name)
                                       out)
                         (setf index (1+ semicolon)))
                       (progn (write-char char out) (incf index))))
                 (progn (write-char char out) (incf index))))))

(defun xml-escape (text)
  (with-output-to-string (out)
    (loop for char across text
          do (case char
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (t (write-char char out))))))

(defun tag-extent (text name start)
  "The close of the open tag NAME starting at START, and its content's start."
  (let* ((open (search (concatenate 'string "<" name) text :start2 start))
         (stop (and open (position #\> text :start open))))
    (and stop (values open stop (1+ stop)))))

(defun tag-name-of (body)
  "The element name of a tag's BODY, its closing slash and space aside."
  (let* ((offset (if (and (plusp (length body)) (char= (char body 0) #\/)) 1 0))
         (end (or (position-if (lambda (char) (member char '(#\Space #\Tab #\Newline #\Return #\/))) body :start offset)
                  (length body))))
    (subseq body offset end)))

(defun scan-body (xml text-tags para-tags)
  "XML as paragraphs: a list of (START . RUNS), RUN a SCAN-RUN of one text
TAG element. Runs outside any paragraph are the document's own."
  (let ((paragraphs '()) (runs '()) (start 0) (position 0))
    (loop
      (let ((open (position #\< xml :start position)))
        (when (null open) (return))
        (let ((close (position #\> xml :start open)))
          (when (null close) (return))
          (let* ((body (subseq xml (1+ open) close))
                 (name (tag-name-of body))
                 (closing (and (plusp (length body)) (char= (char body 0) #\/)))
                 (empty (and (plusp (length body)) (char= (char body (1- (length body))) #\/))))
            (cond
              ((and (member name para-tags :test #'string=) (not closing))
               (when runs (push (cons start (nreverse runs)) paragraphs))
               (setf runs '() start (1+ close) position (1+ close))
               (when empty (push (cons start '()) paragraphs)))
              ((and (member name para-tags :test #'string=) closing)
               (push (cons start (nreverse runs)) paragraphs)
               (setf runs '() start (1+ close) position (1+ close)))
              ((and (member name text-tags :test #'string=) (not closing) (not empty))
               (let ((end (search (concatenate 'string "</" name) xml :start2 close)))
                 (if (null end)
                     (setf position (1+ close))
                     (progn (push (make-scan-run :tag name
                                                  :text (subseq xml (1+ close) end)
                                                  :start (1+ close) :end end)
                                  runs)
                            (setf position end)))))
              (t (setf position (1+ close))))))))
    (if runs
        (nreverse (cons (cons start (nreverse runs)) paragraphs))
        (nreverse paragraphs))))

;;; --- the parts of a document ------------------------------------------------

(defparameter +kinds+ '(("docx" . :docx) ("pptx" . :pptx) ("xlsx" . :xlsx)))


(defun office-kind (path)
  "PATH's format as a keyword, from its suffix, or NIL for a file Office
never wrote."
  (cdr (assoc (string-downcase (or (pathname-type (pathname path)) "")) +kinds+ :test #'string=)))

(defun archive (path)
  "PATH's entries, or a refusal."
  (let ((octets (read-octets path)))
    (unless octets (fail "~a does not exist" path))
    (handler-case (zip-entries octets)
      (error (condition)
        (fail "~a is not a readable Office file (a .docx, .pptx or .xlsx is a ZIP of XML parts): ~a"
              path (nlk:one-line (princ-to-string condition)))))))

(defun find-part (entries name)
  (find name entries :key #'zip-entry-name :test #'string=))

(defun slide-number (name)
  "The N of ppt/slides/slideN.xml, or NIL for any other part."
  (let ((prefix "ppt/slides/slide") (suffix ".xml"))
    (when (and (uiop:string-prefix-p prefix name) (uiop:string-suffix-p name suffix))
      (parse-integer name :start (length prefix) :end (- (length name) (length suffix)) :junk-allowed t))))

(defun shared-strings (entries)
  "xl/sharedStrings.xml as a vector of strings, one per <si>."
  (let ((entry (find-part entries "xl/sharedStrings.xml"))
        (strings (make-array 0 :adjustable t :fill-pointer 0)))
    (when entry
      (let ((xml (zip-entry-text entry)))
        (loop with position = 0
              for (start close content) = (multiple-value-list (tag-extent xml "si" position))
              while start
              do (let ((end (search "</si>" xml :start2 content))) (unless end (return))
                   (let ((text (with-output-to-string (out)
                                 (loop for (open tag-close inner) =
                                       (multiple-value-list (tag-extent xml "t" content))
                                       while (and open (< open end))
                                       do (let ((stop (search "</t>" xml :start2 inner)))
                                            (unless (and stop (< stop end)) (return))
                                            (write-string (subseq xml inner stop) out)
                                            (setf content (1+ stop)))))))
                     (vector-push-extend (xml-unescape text) strings))
                   (setf position (+ end 5))))))
    strings))

(defun xlsx-sheets (entries)
  "The workbook's sheets as (NAME . PART), in the order the workbook lists."
  (let ((workbook (find-part entries "xl/workbook.xml"))
        (rels (find-part entries "xl/_rels/workbook.xml.rels"))
        (targets (make-hash-table :test #'equal))
        (sheets '()))
    (when rels
      (let ((xml (zip-entry-text rels)))
        (loop with position = 0
              for (start close content) = (multiple-value-list (tag-extent xml "Relationship" position))
              while start
              do (let ((tag (subseq xml start close)))
                   (let ((id (attribute-of tag "Id")) (target (attribute-of tag "Target")))
                     (when (and id target)
                       (setf (gethash id targets)
                             (if (uiop:string-prefix-p "/" target) (subseq target 1)
                                 (concatenate 'string "xl/" target))))))
                 (setf position (1+ close))))) 
    (when workbook
      (let ((xml (zip-entry-text workbook)))
        (loop with position = 0
              for (start close content) = (multiple-value-list (tag-extent xml "sheet" position))
              while start
              do (let* ((tag (subseq xml start close))
                        (name (attribute-of tag "name"))
                        (id (attribute-of tag "r:id"))
                        (part (and id (gethash id targets))))
                   (when name
                     (push (cons name part) sheets)))
                 (setf position (1+ close)))))
    (nreverse sheets)))


(defun attribute-of (tag name)
  "TAG's attribute NAME, unescaped, or NIL."
  (let* ((needle (concatenate 'string name "=\""))
         (start (search needle tag)))
    (and start (let ((end (position #\" tag :start (+ start (length needle)))))
                 (and end (xml-unescape (subseq tag (+ start (length needle)) end)))))))

(defparameter +cell-pattern+ "<c ")


(defun sheet-cells (xml shared)
  "XML's non-empty cells as (REF . TEXT), in document order."
  (let ((cells '()) (position 0))
    (loop
      (let ((open (search +cell-pattern+ xml :start2 position)))
        (when (null open) (return))
        (let* ((close (position #\> xml :start open))
               (tag (subseq xml open close))
               (ref (attribute-of tag "r"))
               (type (attribute-of tag "t"))
               (end (search "</c>" xml :start2 close)))
          (unless end (return))
          (let* ((inner (subseq xml (1+ close) end))
                 (value (cell-value inner type shared)))
            (when (and ref value (plusp (length value)))
              (push (cons ref value) cells)))
          (setf position (1+ end)))))
    (nreverse cells)))


(defun cell-value (inner type shared)
  (cond
    ((equal type "s") (let ((index (text-of inner "v")))
                         (and index (ignore-errors (aref shared (parse-integer index :junk-allowed t))))))
    ((equal type "inlineStr") (xml-unescape (or (text-of inner "t") "")))
    (t (let ((text (or (text-of inner "v") (text-of inner "t"))))
         (and text (xml-unescape text))))))


(defun text-of (xml name)
  "The content of the first <NAME> element in XML, or NIL."
  (multiple-value-bind (open close content) (tag-extent xml name 0)
    (declare (ignore open))
    (when close
      (let ((end (search (concatenate 'string "</" name ">") xml :start2 content)))
        (and end (subseq xml content end))))))


;;; --- a document's text, as blocks ----------------------------------------------

(defun paragraph-blocks (xml text-tags label)
  "XML's paragraphs as blocks: (list :where \"LABEL N\" :text ...)."
  (let ((blocks '())
        (index 0))
    (dolist (paragraph (scan-body xml text-tags '("w:p" "a:p")))
      (let ((text (with-output-to-string (out)
                    (dolist (run (cdr paragraph))
                      (unless (string= (scan-run-tag run) "w:delText")
                        (write-string (xml-unescape (scan-run-text run)) out))))))
        (push (list :where (format nil "~a ~d" label index) :text text) blocks))
      (incf index))
    (nreverse blocks)))

(defun slide-parts (entries)
  "The slide parts, in slide order."
  (let ((slides (remove-if-not (lambda (entry) (slide-number (zip-entry-name entry))) entries)))
    (sort slides #'< :key (lambda (entry) (slide-number (zip-entry-name entry))))))

(defun sheet-blocks (entries)
  "Every non-empty cell of every sheet, as a block at SHEET!REF."
  (let ((blocks '())
        (shared (shared-strings entries)))
    (dolist (sheet (xlsx-sheets entries))
      (let ((entry (and (cdr sheet) (find-part entries (cdr sheet)))))
        (when entry
          (dolist (cell (sheet-cells (zip-entry-text entry) shared))
            (push (list :where (format nil "~a!~a" (car sheet) (car cell)) :text (cdr cell))
                  blocks)))))
    (nreverse blocks)))

(defun document-blocks (entries kind)
  "KIND's text as blocks: a plist (:where W :text T) each."
  (ecase kind
    (:docx
     (let ((entry (find-part entries "word/document.xml")))
       (unless entry (fail "this .docx has no word/document.xml: it is not a Word document"))
       (paragraph-blocks (zip-entry-text entry) '("w:t" "w:delText") "paragraph")))
    (:pptx
     (loop for entry in (slide-parts entries)
           append (paragraph-blocks (zip-entry-text entry) '("a:t")
                                    (format nil "slide ~d paragraph" (slide-number (zip-entry-name entry))))))
    (:xlsx (sheet-blocks entries))))

;;; --- changing text -------------------------------------------------------------

(defun replace-occurrences (text find replacement nth)
  "TEXT with FIND replaced by REPLACEMENT, the NTH one alone when NTH is
not NIL. => (values TEXT COUNT)."
  (let ((out (make-string-output-stream)) (position 0) (seen 0) (count 0))
    (loop
      (let ((hit (search find text :start2 position)))
        (unless hit (write-string (subseq text position) out) (return))
        (incf seen)
        (write-string (subseq text position hit) out)
        (cond ((and nth (/= seen nth))
               (write-string find out))
              (t (write-string replacement out) (incf count)))
        (setf position (+ hit (length find)))))
    (values (get-output-stream-string out) count)))

(defun replace-in-part (xml tags find replacement nth)
  "XML with FIND replaced inside every element named in TAGS. => (values XML COUNT)."
  (let ((out (make-string-output-stream)) (position 0) (count 0) (seen 0) (length (length xml)))
    (loop
      (let ((next (first (loop for tag in tags
                        for open = (search (concatenate 'string "<" tag) xml :start2 position)
                        when (and open (< (+ open 1 (length tag)) length)
                                  (member (char xml (+ open 1 (length tag))) '(#\Space #\Tab #\Newline #\Return #\>)))
                          collect (cons open tag)))))
        (unless next (write-string (subseq xml position) out) (return))
        (let* ((tag (cdr next))
               (close (position #\> xml :start (car next)))
               (content (1+ close))
               (end (search (concatenate 'string "</" tag ">") xml :start2 content)))
          (unless end (write-string (subseq xml position) out) (return))
          (write-string (subseq xml position content) out)
          (let* ((text (xml-unescape (subseq xml content end)))
                 (occurrences (count-find text find)))
            (multiple-value-bind (new-text replaced)
                (replace-occurrences text find replacement (and nth (- nth seen)))
              (write-string (xml-escape new-text) out)
              (incf count replaced)
              (incf seen occurrences)))
          (setf position end))))
    (values (get-output-stream-string out) count)))

(defun count-find (text find)
  (let ((count 0) (position 0))
    (loop (let ((hit (search find text :start2 position)))
            (unless hit (return count))
            (incf count)
            (setf position (+ hit (length find)))))))


(defun tracked-change-xml (run find replacement author date id)
  "The XML replacing RUN's one run element: its text before FIND, the FIND
deleted as a tracked deletion, REPLACEMENT inserted as a tracked insertion,
and the rest of the run."
  (let* ((body (xml-unescape (scan-run-text run)))
         (hit (search find body))
         (before (subseq body 0 hit))
         (after (subseq body (+ hit (length find))))
         (properties (scan-run-properties run)))
    (flet ((text-run (tag text)
             (format nil "<w:r>~a<~a xml:space=\"preserve\">~a</~a></w:r>"
                     properties tag (xml-escape text) tag)))
      (with-output-to-string (out)
        (when (plusp (length before)) (write-string (text-run "w:t" before) out))
        (format out "<w:del w:id=\"~d\" w:author=\"~a\" w:date=\"~a\"><w:r>~a<w:delText xml:space=\"preserve\">~a</w:delText></w:r></w:del>"
                id (xml-escape author) date properties (xml-escape find))
        (format out "<w:ins w:id=\"~d\" w:author=\"~a\" w:date=\"~a\"><w:r>~a<w:t xml:space=\"preserve\">~a</w:t></w:r></w:ins>"
                (1+ id) (xml-escape author) date properties (xml-escape replacement))
        (when (plusp (length after)) (write-string (text-run "w:t" after) out))))))


(defun scan-run-properties (run)
  "RUN's w:rPr element, XML and all, or an empty string."
  (let* ((xml (scan-run-xml run))
         (start (and xml (search "<w:rPr" xml))))
    (if start
        (let ((end (search "</w:rPr>" xml :start2 start)))
          (if end (subseq xml start (+ end 8)) ""))
        "")))

(defstruct (scan-run (:constructor make-scan-run))
  tag text start end
  xml)


;;; --- the tracked edit ---------------------------------------------------------

(defun run-element-start (xml before)
  "The offset of the <w:r> or <w:r ...> element that opens at or before BEFORE."
  (let ((position before))
    (loop
      (let ((hit (search "<w:r" xml :from-end t :end2 position)))
        (unless hit (return 0))
        (let ((after (and (< (+ hit 4) (length xml)) (char xml (+ hit 4)))))
          (if (or (null after) (member after '(#\Space #\Tab #\Newline #\Return #\>)))
              (return hit)
              (setf position hit)))))))

(defun tracked-edit (entries find replacement author nth)
  "Word's word/document.xml with the NTH FIND (the first by default) deleted
and REPLACEMENT inserted, as tracked changes. => (values XML COUNT)."
  (let ((entry (find-part entries "word/document.xml")))
    (unless entry (fail "this .docx has no word/document.xml: it is not a Word document"))
    (let* ((xml (zip-entry-text entry))
           (paragraphs (scan-body xml '("w:t") '("w:p")))
           (seen 0))
      (dolist (paragraph paragraphs)
        (dolist (run (cdr paragraph))
          (let ((text (xml-unescape (scan-run-text run))))
            (when (search find text)
              (incf seen)
              (when (or (null nth) (= seen nth))
                (let* ((run-start (run-element-start xml (scan-run-start run)))
                       (close (or (search "</w:r>" xml :start2 (scan-run-end run)) (length xml)))
                       (run-end (min (length xml) (+ close 6)))
                       (change (tracked-change-xml
                                (make-scan-run :tag (scan-run-tag run)
                                               :text (scan-run-text run)
                                               :start (scan-run-start run)
                                               :end (scan-run-end run)
                                               :xml (subseq xml run-start run-end))
                                find replacement author (change-date) (id-for xml))))
                  (return-from tracked-edit
                    (values (concatenate 'string (subseq xml 0 run-start) change (subseq xml run-end))
                            1))))))))
      (values xml 0))))

(defun id-for (xml)
  "A change id no other revision in XML carries."
  (let ((high 0) (position 0))
    (loop (let ((hit (or (search "w:id=\"" xml :start2 position) (return high))))
            (let* ((start (+ hit 6))
                   (end (position #\" xml :start start))
                   (value (ignore-errors (parse-integer xml :start start :end end))))
              (when value (setf high (max high value)))
              (setf position (1+ (or end start))))))
    (1+ high)))


(defun change-date ()
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" year month day hour minute second)))
