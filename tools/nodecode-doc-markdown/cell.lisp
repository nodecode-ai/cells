;;;; cell.lisp --- one local file to Markdown: the tool the model calls, /doc for the operator.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of TakeshiTGAL's hermes-plugin-doc-markdown, a Hermes
;;;; Agent plugin that registered the tool doc_convert over Microsoft's
;;;; MarkItDown: one absolute local path in, Markdown out, the path resolved
;;;; before it is judged (symlinks followed, a path component starting with
;;;; "." refused, the harness's own home refused, an allowlist of extensions,
;;;; a size cap), and a refusal that says what to do next.
;;;;
;;;; The rewrite keeps the tool and the guard and drops the Python runtime.
;;;; A .docx, .xlsx or .pptx is read as what it is -- a zip of OOXML parts --
;;;; with a DEFLATE decoder (RFC 1951) and a central-directory reader in this
;;;; file; a PDF's FlateDecode streams are inflated the same way and its text
;;;; operators read out best-effort; HTML, CSV, JSON and plain text need no
;;;; archive at all. Nothing here runs node, python or MarkItDown, and nothing
;;;; leaves the machine.

(defpackage #:nodecode-doc-markdown
  (:use #:cl)
  (:nicknames #:doc-markdown)
  (:export #:start-cell #:stop-cell #:setting
           #:convert-file #:run-convert #:run-doc
           #:doc-markdown-error #:doc-markdown-error-detail
           #:doc-markdown-error-kind
           #:inflate #:zip-part #:zip-names))

(in-package #:nodecode-doc-markdown)

(nlk:define-peripheral doc-markdown :not-running t)

;;; --- what a refusal is -------------------------------------------------------------

;;; NLK:DEFINE-PERIPHERAL already gave this package DOC-MARKDOWN-ERROR and its
;;; DETAIL reader; what a refusal adds is which kind it is, so a caller can
;;; tell a missing file from a bad argument without reading the prose.
(nlk:define-error doc-markdown-refusal (doc-markdown-error)
  ((kind :initarg :kind :reader doc-markdown-error-kind
         :initform "conversion_failed"))
  (:report "~a" detail))

(defun refuse (kind control &rest args)
  "Signal DOC-MARKDOWN-ERROR of KIND with the formatted text; the text carries
what to do next, as the original's next_step did."
  (error 'doc-markdown-refusal :kind kind :detail (apply #'format nil control args)))

(defun failure-text (condition)
  "The one line a refused doc_convert answers the model."
  (format nil "ERROR: doc_convert: ~a: ~a"
          (doc-markdown-error-kind condition)
          (doc-markdown-error-detail condition)))

;;; --- the section's values, read where they are used -------------------------------

(defun setting-int (name default)
  "NAME's value as an integer, DEFAULT when the cell is not running or the
value is not one."
  (let ((value (ignore-errors (setting name))))
    (if (integerp value) value default)))

(defun setting-true (name)
  "NAME's value as a boolean: T only while the cell runs and the section says
T, so a setting the operator switched off is off."
  (if *doc-markdown*
      (and (setting name) t)
      t))

(defun max-file-bytes () (setting-int :max-file-bytes 20971520))
(defun max-output-chars () (setting-int :max-output-chars 200000))

;;; --- the path guard: resolve first, then judge --------------------------------------
;;;
;;; The original's paths.py in the little of it this rewrite keeps: an absolute
;;; path only, no URL, every symbolic link followed before anything is judged,
;;; a component that starts with "." refused, the harness's home refused, an
;;; allowlist of extensions, and a size cap read from the section and never
;;; from the call. Judging after the resolve is what makes a symlink that
;;; lands under a refused location refused.

(defparameter +allowed-extensions+ '("pdf" "docx" "pptx" "xlsx" "html" "htm" "csv" "json" "txt" "md")
  "What this cell converts. The original's allowlist also had .xls, which was
best-effort there; a BIFF workbook is not read here (see the NOTICE).")

(defparameter +url-prefixes+ '("http://" "https://" "file://")
  "A URL in any letter case is a URL, not a path, as the original read it.")

(defun extension-of (path)
  (string-downcase (or (pathname-type (pathname path)) "")))

(defun canonical (path)
  "PATH with every symbolic link followed, as a namestring."
  (namestring (or (ignore-errors (uiop:resolve-symlinks (pathname path)))
                  (pathname path))))

(defun home-prefix ()
  "This home's Nodecode folder, with a trailing separator: the one place under
the home a document is never read from. Resolved the way a path is, so a home
that is itself a symlink compares equal."
  (canonical (namestring (merge-pathnames ".nodecode/" (user-homedir-pathname)))))

(defun hidden-component-p (namestring)
  "Whether any component of NAMESTRING starts with a dot.
REALPATH has already removed \".\" and \"..\", so every remaining component that
starts with a dot is refused, exactly as the original refused it."
  (let ((start 0))
    (loop
      (let* ((end (position #\/ namestring :start start))
             (part (subseq namestring start (or end (length namestring)))))
        (when (and (plusp (length part)) (char= (char part 0) #\.))
          (return t))
        (if end
            (setf start (1+ end))
            (return nil))))))

(defun resolved-to-string (raw)
  "RAW with a leading ~ expanded, as the original expanded it."
  (cond
    ((string= raw "~") (namestring (user-homedir-pathname)))
    ((and (>= (length raw) 2) (char= (char raw 0) #\~) (char= (char raw 1) #\/))
     (namestring (merge-pathnames (subseq raw 2) (user-homedir-pathname))))
    (t raw)))

(defun resolve-readable-file (path)
  "PATH resolved and judged. => the real namestring, or a DOC-MARKDOWN-ERROR."
  (let ((raw (nlk:trimmed (or path ""))))
    (when (zerop (length raw))
      (refuse "bad_path"
              "path is required: pass one absolute local file path, for example /home/me/docs/report.pdf"))
    (when (some (lambda (prefix) (uiop:string-prefix-p prefix (string-downcase raw))) +url-prefixes+)
      (refuse "url_not_allowed"
              "the doc_convert tool reads local files only: pass a local absolute path, not an http, https or file URL"))
    (let ((expanded (resolved-to-string raw)))
      (unless (and (plusp (length expanded)) (char= (char expanded 0) #\/))
        (refuse "bad_path"
                "~s is not an absolute path: pass the full path (for example /home/me/docs/report.pdf)" raw))
      (let ((real (canonical expanded)))
        (when (and (setting-true :refuse-hidden) (hidden-component-p real))
          (refuse "path_not_allowed"
                  "any path component that starts with '.' is refused: copy the file to a normal folder (for example Documents) and pass that path"))
        (when (uiop:string-prefix-p (home-prefix) real)
          (refuse "path_not_allowed"
                  "files inside the Nodecode home folder are not read: copy the file to a normal folder and pass that path"))
        (unless (probe-file real)
          (refuse "file_not_found"
                  "no file at the given path: check the path, and list the folder first if unsure"))
        (when (uiop:directory-exists-p real)
          (refuse "not_a_file" "the path is not a regular file: pass a file path, not a directory"))
        (let ((ext (extension-of real)))
          (unless (member ext +allowed-extensions+ :test #'string=)
            (refuse "unsupported_type"
                    "extension ~a is not in this cell's supported set (pdf, docx, pptx, xlsx, html, htm, csv, json, txt, md): export it to one of those first"
                    (if (zerop (length ext)) "(none)" (concatenate 'string "." ext)))))
        (let ((limit (max-file-bytes))
              (size (with-open-file (in real :element-type '(unsigned-byte 8))
                      (file-length in))))
          (when (> size limit)
            (refuse "file_too_large"
                    "the file is ~:d bytes (limit ~:d): split it into smaller files, or raise max_file_bytes in the doc-markdown section" size limit)))
        real))))

;;; --- bytes -------------------------------------------------------------------------

(defun read-octets (path)
  "Every byte of PATH."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence data in)
      data)))

(defun utf8-text (octets what)
  "OCTETS decoded as UTF-8, a byte-order mark dropped. Text that is not UTF-8
refuses rather than being guessed at, as the original's charset table did for
an unlabelled local file."
  (let ((start (if (and (>= (length octets) 3)
                         (= (aref octets 0) #xEF) (= (aref octets 1) #xBB) (= (aref octets 2) #xBF))
                   3
                   0)))
    (handler-case (flexi-streams:octets-to-string octets :start start :external-format :utf-8)
      (error ()
        (refuse "encoding_unconfirmed"
                "~a is not UTF-8 text: save it as UTF-8, or hand this cell a container it reads (docx, xlsx, pptx, pdf)"
                what)))))

(defun octets-of-string (text)
  (flexi-streams:string-to-octets text :external-format :utf-8))

;;; --- DEFLATE (RFC 1951) ------------------------------------------------------------
;;;
;;; A .docx is a zip, and a zip's member is raw DEFLATE. This is the whole of
;;; what a document needs: stored, fixed-Huffman and dynamic-Huffman blocks,
;;; the two length/distance tables, and a bit reader that refuses to run off
;;; the end rather than reading whatever follows.

(defparameter +inflate-limit+ (* 128 1024 1024)
  "The most one member may inflate to, so a hostile archive cannot fill memory.")

(defparameter +length-base+
  #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258))

(defparameter +length-extra+ #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))

(defparameter +distance-base+
  #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073
    4097 6145 8193 12289 16385 24577))

(defparameter +distance-extra+ #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))

(defparameter +code-length-order+ #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)
  "The order the dynamic block's code-length code lengths arrive in.")

(defstruct (bit-reader (:constructor make-bit-reader (data)) (:copier nil))
  (data #() :read-only t)
  (pos 0 :type fixnum))

(defun corrupt (control &rest args)
  (refuse "conversion_failed" "~a" (apply #'format nil control args)))

(defun bit-at (reader position)
  "The bit at POSITION, counted from the start of the stream, first bit of a
byte lowest."
  (let ((data (bit-reader-data reader)))
    (when (>= position (* 8 (length data)))
      (corrupt "the archive's compressed stream ends early"))
    (ldb (byte 1 (logand position 7)) (aref data (ash position -3)))))

(defun reader-bit (reader)
  "One bit at the reader's position; the position moves past it."
  (prog1 (bit-at reader (bit-reader-pos reader))
    (incf (bit-reader-pos reader))))

(defun reader-bits (reader count)
  "COUNT bits from the reader's position, first bit lowest; the position moves
past them."
  (let ((value 0)
        (position (bit-reader-pos reader)))
    (dotimes (index count)
      (setf value (logior value (ash (bit-at reader (+ position index)) index))))
    (incf (bit-reader-pos reader) count)
    value))

(defun reader-align (reader)
  "Move to the next byte boundary, as a stored block requires."
  (setf (bit-reader-pos reader) (* 8 (ceiling (bit-reader-pos reader) 8))))

(defstruct (huffman (:constructor make-huffman (counts symbols)) (:copier nil))
  (counts #() :read-only t)
  (symbols #() :read-only t))

(defun huffman-table (lengths)
  "The canonical Huffman table for LENGTHS, a vector of code lengths."
  (let ((counts (make-array 16 :initial-element 0)))
    (loop for length across lengths
          do (when (plusp length) (incf (aref counts length))))
    (let ((offsets (make-array 16 :initial-element 0)))
      (loop for length from 1 below 15
            do (setf (aref offsets (1+ length)) (+ (aref offsets length) (aref counts length))))
      (let ((symbols (make-array (reduce #'+ counts :initial-value 0)))
            (next (copy-seq offsets)))
        (loop for symbol from 0
              for length across lengths
              do (when (plusp length)
                   (setf (aref symbols (aref next length)) symbol)
                   (incf (aref next length))))
        (make-huffman counts symbols)))))

(defun huffman-symbol (reader table)
  "The next symbol of TABLE, read bit by bit in the canonical code."
  (let ((code 0) (first 0) (index 0))
    (loop for length from 1 to 15
          do (setf code (logior code (reader-bit reader)))
             (let ((count (aref (huffman-counts table) length)))
               (when (< (- code first) count)
                 (return-from huffman-symbol
                   (aref (huffman-symbols table) (+ index (- code first)))))
               (incf index count)
               (setf first (ash (+ first count) 1))
               (setf code (ash code 1)))
          finally (corrupt "a compressed stream holds a code no table defines"))))

(defun fixed-tables ()
  "The fixed-Huffman lit/len and distance tables of RFC 1951."
  (let ((literals (make-array 288 :initial-element 8)))
    (loop for symbol from 144 below 256 do (setf (aref literals symbol) 9))
    (loop for symbol from 256 below 280 do (setf (aref literals symbol) 7))
    (values (huffman-table literals)
            (huffman-table (make-array 30 :initial-element 5)))))

(defun dynamic-tables (reader)
  "The lit/len and distance tables a dynamic block declares."
  (let* ((literal-count (+ 257 (reader-bits reader 5)))
         (distance-count (+ 1 (reader-bits reader 5)))
         (code-count (+ 4 (reader-bits reader 4)))
         (code-lengths (make-array 19 :initial-element 0)))
    (loop for index below code-count
          do (setf (aref code-lengths (aref +code-length-order+ index)) (reader-bits reader 3)))
    (let* ((code-table (huffman-table code-lengths))
           (total (+ literal-count distance-count))
           (lengths (make-array total :initial-element 0))
           (index 0))
      (labels ((put (length count)
                 (dotimes (step count)
                   (when (>= index total)
                     (corrupt "a dynamic block declares more lengths than it has codes"))
                   (setf (aref lengths index) length)
                   (incf index))))
        (loop while (< index total)
              do (let ((symbol (huffman-symbol reader code-table)))
                   (case symbol
                     (16 (unless (plusp index)
                           (corrupt "a length repeat with no length before it"))
                         (put (aref lengths (1- index)) (+ 3 (reader-bits reader 2))))
                     (17 (put 0 (+ 3 (reader-bits reader 3))))
                     (18 (put 0 (+ 11 (reader-bits reader 7))))
                     (t (put symbol 1))))))
      (values (huffman-table (subseq lengths 0 literal-count))
              (huffman-table (subseq lengths literal-count))))))

(defun inflate-block (reader out literals distances)
  "One Huffman block, appended to OUT."
  (loop
    (let ((symbol (huffman-symbol reader literals)))
      (cond
        ((< symbol 256) (vector-push-extend symbol out))
        ((= symbol 256) (return))
        (t
         (let* ((index (- symbol 257))
                (length (+ (aref +length-base+ index) (reader-bits reader (aref +length-extra+ index))))
                (where (huffman-symbol reader distances))
                (distance (+ (aref +distance-base+ where)
                             (reader-bits reader (aref +distance-extra+ where))))
                (start (- (fill-pointer out) distance)))
           (when (or (>= where (length +distance-base+)) (minusp start))
             (corrupt "a compressed stream copies from before it begins"))
           (dotimes (step length)
             (vector-push-extend (aref out (+ start step)) out)))))
      (when (> (fill-pointer out) +inflate-limit+)
        (corrupt "a member inflates past ~:d bytes" +inflate-limit+)))))

(defun inflate (octets)
  "OCTETS, a raw DEFLATE stream (RFC 1951), decompressed."
  (let ((reader (make-bit-reader octets))
        (out (make-array 4096 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (loop
      (let ((final (reader-bits reader 1))
            (kind (reader-bits reader 2)))
        (case kind
          (0 (reader-align reader)
             (let ((length (reader-bits reader 16))
                   (check (reader-bits reader 16)))
               (unless (= check (ldb (byte 16 0) (lognot length)))
                 (corrupt "a stored block's length does not match its check"))
               (dotimes (step length)
                 (vector-push-extend (reader-bits reader 8) out))))
          (1 (multiple-value-bind (literals distances) (fixed-tables)
               (inflate-block reader out literals distances)))
          (2 (multiple-value-bind (literals distances) (dynamic-tables reader)
               (inflate-block reader out literals distances)))
          (t (corrupt "a compressed stream holds a block type no reader knows")))
        (when (= final 1)
          (return (coerce out '(simple-array (unsigned-byte 8) (*)))))))))

;;; --- the zip container ---------------------------------------------------------------

(defstruct (zip-entry (:constructor make-zip-entry (name method compressed-size size offset))
                      (:copier nil))
  (name "" :read-only t)
  (method 0 :read-only t)
  (compressed-size 0 :read-only t)
  (size 0 :read-only t)
  (offset 0 :read-only t))

(defun le16 (data at)
  (+ (aref data at) (ash (aref data (+ at 1)) 8)))

(defun le32 (data at)
  (+ (aref data at)
     (ash (aref data (+ at 1)) 8)
     (ash (aref data (+ at 2)) 16)
     (ash (aref data (+ at 3)) 24)))

(defun end-of-central-directory (data)
  "Where the zip's end-of-central-directory record starts."
  (let ((last (- (length data) 22))
        (first (max 0 (- (length data) 65558))))
    (loop for at from last downto first
          when (and (>= at 0) (= (le32 data at) #x06054b50))
            return at
          finally (corrupt "the archive has no end-of-central-directory record: it is not a zip"))))

(defun zip-entries (data)
  "Every entry the archive's central directory declares."
  (let* ((end (end-of-central-directory data))
         (count (le16 data (+ end 10)))
         (entries '())
         (at (le32 data (+ end 16))))
    (dotimes (index count)
      (unless (= (le32 data at) #x02014b50)
        (corrupt "the archive's central directory is malformed"))
      (let ((name-length (le16 data (+ at 28)))
            (extra-length (le16 data (+ at 30)))
            (comment-length (le16 data (+ at 32))))
        (push (make-zip-entry
               (map 'string #'code-char
                    (subseq data (+ at 46) (+ at 46 name-length)))
               (le16 data (+ at 10))
               (le32 data (+ at 20))
               (le32 data (+ at 24))
               (le32 data (+ at 42)))
              entries)
        (incf at (+ 46 name-length extra-length comment-length))))
    (nreverse entries)))

(defun zip-names (data)
  "Every member's name, in the archive's order."
  (mapcar #'zip-entry-name (zip-entries data)))

(defun zip-part (data name)
  "The bytes of member NAME, or NIL when the archive holds no such member."
  (let ((entry (find name (zip-entries data) :key #'zip-entry-name :test #'string=)))
    (when entry
      (let ((at (zip-entry-offset entry)))
        (unless (= (le32 data at) #x04034b50)
          (corrupt "the archive's local header for ~a is malformed" name))
        (let* ((name-length (le16 data (+ at 26)))
               (extra-length (le16 data (+ at 28)))
               (start (+ at 30 name-length extra-length))
               (raw (subseq data start (+ start (zip-entry-compressed-size entry)))))
          (case (zip-entry-method entry)
            (0 raw)
            (8 (inflate raw))
            (t (corrupt "member ~a uses compression method ~d, which is not DEFLATE"
                        name (zip-entry-method entry)))))))))

;;; --- XML, as far as a document needs it ---------------------------------------------
;;;
;;; Events, not a tree: a start tag with its attributes, an empty tag, an end
;;; tag, and the text between. A comment, CDATA, a processing instruction and
;;; a DOCTYPE are the four things a scanner has to skip whole.

(defun parse-attributes (string)
  "STRING's name=\"value\" pairs as an alist of (name . value) strings."
  (let ((attributes '())
        (position 0)
        (length (length string)))
    (loop while (< position length)
          do (let ((equals (position #\= string :start position)))
               (if (null equals)
                   (setf position length)
                   (let* ((name (string-trim '(#\Space #\Tab #\Newline #\Return #\/)
                                             (subseq string position equals)))
                          (quote-at (position-if (lambda (character)
                                                   (or (char= character #\")
                                                       (char= character #\')))
                                                 string :start (1+ equals))))
                     (if (null quote-at)
                         (setf position length)
                         (let* ((quote-character (char string quote-at))
                                (end (or (position quote-character string :start (1+ quote-at))
                                         length)))
                           (push (cons name (subseq string (1+ quote-at) end)) attributes)
                           (setf position (1+ end))))))))
    (nreverse attributes)))

(defun xml-tag-of (body next)
  "The event a tag's BODY is, and the position after the tag, NEXT."
  (if (and (plusp (length body)) (char= (char body 0) #\/))
      (values (list :end (subseq body 1)) next)
      (let* ((empty (and (plusp (length body))
                          (char= (char body (1- (length body))) #\/)))
             (inner (if empty (subseq body 0 (1- (length body))) body))
             (space (position-if (lambda (character)
                                   (member character '(#\Space #\Tab #\Newline #\Return)))
                                 inner)))
        (values (list (if empty :empty :start)
                      (if space (subseq inner 0 space) inner)
                      (if space (parse-attributes (subseq inner (1+ space))) '()))
                next))))

(defun xml-events (text)
  "TEXT's events, in order: (:start NAME ATTRIBUTES), (:empty NAME ATTRIBUTES),
(:end NAME), and (:text STRING)."
  (let ((events '())
        (position 0)
        (length (length text)))
    (labels ((emit (event) (push event events))
             (tag-event (after)
               "The event of the tag whose body starts at AFTER; the position after it."
               (let ((close (or (position #\> text :start after)
                                (corrupt "an XML tag is never closed"))))
                 (multiple-value-bind (event next) (xml-tag-of (subseq text after close) (1+ close))
                   (when event (emit event))
                   next))))
      (loop while (< position length)
            do (let ((start (position #\< text :start position)))
                 (cond
                   ((null start)
                    (when (> length position) (emit (list :text (subseq text position length))))
                    (setf position length))
                   (t
                    (when (> start position) (emit (list :text (subseq text position start))))
                    (let ((after (1+ start)))
                      (cond
                        ((>= after length) (setf position length))
                        ((text-at-p text after "!--")
                         (setf position (min length (+ 4 (or (search "-->" text :start2 after) length)))))
                        ((text-at-p text after "![CDATA[")
                         (let ((close (or (search "]]>" text :start2 after) length)))
                           (emit (list :text (subseq text (+ after 8) close)))
                           (setf position (min length (+ 3 close)))))
                        ((char= (char text after) #\?)
                         (setf position (min length (+ 2 (or (search "?>" text :start2 after) length)))))
                        ((char= (char text after) #\!)
                         (setf position (min length (1+ (or (position #\> text :start after) length)))))
                        (t (setf position (tag-event after)))))))))
      (nreverse events))))

(defun text-at-p (text at prefix)
  "Whether PREFIX sits at AT in TEXT."
  (let ((end (+ at (length prefix))))
    (and (<= end (length text))
         (string= prefix text :start2 at :end2 end))))

(defun local-name (name)
  "NAME without its namespace prefix."
  (let ((colon (position #\: name)))
    (if colon (subseq name (1+ colon)) name)))

(defun attribute (attributes name)
  "ATTRIBUTES' value for NAME, matched by local name, or NIL."
  (cdr (find name attributes :key (lambda (pair) (local-name (car pair))) :test #'string=)))

(defun element-p (event name)
  "Whether EVENT opens, closes or empties the element NAME, by local name."
  (and (member (first event) '(:start :empty :end))
       (string= (local-name (second event)) name)))

;;; --- OOXML paragraphs ----------------------------------------------------------------
;;;
;;; A .docx and a .pptx are the same shape: paragraphs of runs of text, with
;;; tabs and line breaks between, and tables as cells. One walker reads both;
;;; each format says how a paragraph becomes a line.

(defun heading-level (style)
  "The level a docx heading style names, or NIL."
  (when style
    (let ((downcased (string-downcase style)))
      (when (uiop:string-prefix-p "heading" downcased)
        (let ((digits (nlk:trimmed (subseq downcased (length "heading")))))
          (when (and (plusp (length digits)) (every #'digit-char-p digits)
                     (<= 1 (parse-integer digits) 6))
            (parse-integer digits)))))))

(defun ooxml-lines (xml)
  "The paragraphs of an OOXML part as lines: a heading with its #s, a table
row one line of cells, a tab and a break kept inside the line they fall in."
  (let ((lines '())
        (line (make-string-output-stream))
        (open-line nil)
        (in-text 0)
        (heading nil)
        (in-cell nil)
        (fresh nil))
    (labels ((start-line ()
               (unless open-line
                 (setf line (make-string-output-stream)
                       open-line t)))
             (flush-line ()
               (when open-line
                 (let ((text (string-right-trim
                              '(#\Space #\Tab)
                              (get-output-stream-string line))))
                   (push (if heading
                             (format nil "~a ~a" (make-string heading :initial-element #\#) text)
                             text)
                         lines))
                 (setf open-line nil
                       heading nil))))
      (dolist (event (xml-events xml))
        (case (first event)
          (:text
           (when (plusp in-text)
             (start-line)
             (write-string (second event) line)))
          (:start
           (cond
             ((element-p event "t") (incf in-text) (start-line))
             ((element-p event "p")
              (if in-cell
                  (when (and fresh open-line)
                    (write-char #\Space line)
                    (setf fresh nil))
                  (flush-line)))
             ((element-p event "tc")
              (start-line)
              (write-char #\| line)
              (setf in-cell t
                    fresh t))
             ((element-p event "pStyle")
              (setf heading (heading-level (attribute (third event) "val"))))))
          (:empty
           (cond
             ((element-p event "pStyle")
              (setf heading (heading-level (attribute (third event) "val"))))
             ((element-p event "tab") (start-line) (write-char #\Tab line))
             ((or (element-p event "br") (element-p event "cr"))
              (start-line) (write-char #\Newline line))
             ((element-p event "p") (unless in-cell (flush-line)))))
          (:end
           (cond
             ((element-p event "t") (when (plusp in-text) (decf in-text)))
             ((element-p event "p") (unless in-cell (flush-line)))
             ((element-p event "tc")
              (when open-line (write-char #\Space line))
              (setf in-cell nil
                    fresh nil))
             ((element-p event "tr")
              (when open-line (write-char #\| line) (flush-line)))))))
      (flush-line)
      (nreverse lines))))

(defun part-text (octets name what)
  "Member NAME of the OOXML archive OCTETS, as text."
  (let ((part (zip-part octets name)))
    (unless part
      (refuse "conversion_failed" "~a holds no ~a: it is not a file of the type its name says" what name))
    (utf8-text part what)))

(defun docx-markdown (octets what)
  (tidy (format nil "~{~a~%~}" (ooxml-lines (part-text octets "word/document.xml" what)))))

(defun ends-with-p (string suffix)
  "Whether STRING ends with SUFFIX."
  (let ((start (- (length string) (length suffix))))
    (and (not (minusp start))
         (string= suffix string :start2 start))))

(defun part-number (name prefix suffix)
  "The number a part's name carries between PREFIX and SUFFIX."
  (or (ignore-errors (parse-integer name
                                     :start (length prefix)
                                     :end (max (length prefix) (- (length name) (length suffix)))
                                     :junk-allowed t))
      0))

(defun pptx-markdown (octets what)
  (let ((slides (remove-if-not (lambda (name)
                                  (and (uiop:string-prefix-p "ppt/slides/slide" name)
                                       (ends-with-p name ".xml")))
                                (zip-names octets))))
    (unless slides
      (refuse "conversion_failed" "~a holds no slides: it is not a readable pptx file" what))
    (tidy (format nil "~{~a~%~}"
                  (loop for name in (sort slides #'< :key (lambda (name)
                                                             (part-number name "ppt/slides/slide" ".xml")))
                        for number from 1
                        append (cons (format nil "## Slide ~d" number)
                                     (ooxml-lines (part-text octets name what))))))))

;;; --- xlsx: a workbook's sheets as Markdown tables -------------------------------------

(defun xlsx-shared-strings (text)
  "The workbook's shared-string table, as a vector."
  (let ((strings '())
        (current nil)
        (in-text 0))
    (dolist (event (xml-events text))
      (case (first event)
        (:text
         (when (and (plusp in-text) current) (write-string (second event) current)))
        (:start
         (cond
           ((element-p event "si") (setf current (make-string-output-stream)))
           ((element-p event "t") (incf in-text))))
        (:end
         (cond
           ((element-p event "t") (when (plusp in-text) (decf in-text)))
           ((element-p event "si")
            (push (if current (get-output-stream-string current) "") strings)
            (setf current nil))))))
    (coerce (nreverse strings) 'vector)))

(defun xlsx-sheet-names (text)
  "The sheet names a workbook part declares, in order."
  (loop for event in (xml-events text)
        when (element-p event "sheet")
          collect (or (attribute (third event) "name") "")))

(defun column-index (reference)
  "The zero-based column a cell reference such as AB12 names, or NIL."
  (let ((index 0) (letters 0))
    (loop for character across reference
          while (alpha-char-p character)
          do (setf index (+ (* index 26)
                            (1+ (- (char-code (char-upcase character)) (char-code #\A))))
                   letters (1+ letters)))
    (if (zerop letters) nil (1- index))))

(defun xlsx-rows (text shared)
  "TEXT's worksheet as rows of strings; SHARED is the workbook's string table."
  (let ((rows '())
        (cells '())
        (position 0)
        (in-value 0)
        (value nil)
        (kind nil)
        (column nil))
    (labels ((flush-cell ()
               (when value
                 (let ((content (get-output-stream-string value)))
                   (push (cons (or column position)
                               (if (string= kind "s")
                                   (let ((index (ignore-errors (parse-integer content :junk-allowed t))))
                                     (if (and index (< -1 index (length shared)))
                                         (aref shared index)
                                         ""))
                                   content))
                         cells))
                 (setf value nil
                       kind nil
                       column nil)))
             (flush-row ()
               (flush-cell)
               (when cells
                 (let ((row '())
                       (next 0))
                   (dolist (cell (sort (copy-list cells) #'< :key #'car))
                     (loop while (< next (car cell))
                           do (push "" row) (incf next))
                     (push (cdr cell) row)
                     (setf next (1+ (car cell))))
                   (push (nreverse row) rows)))
               (setf cells '()
                     position 0)))
      (dolist (event (xml-events text))
        (case (first event)
          (:text
           (when (and (plusp in-value) value) (write-string (second event) value)))
          (:start
           (cond
             ((element-p event "c")
              (setf value (make-string-output-stream)
                    kind (attribute (third event) "t")
                    column (column-index (or (attribute (third event) "r") ""))))
             ((element-p event "v") (incf in-value))
             ((element-p event "t") (incf in-value))
             ((element-p event "row") (setf position 0))))
          (:end
           (cond
             ((element-p event "v") (when (plusp in-value) (decf in-value)))
             ((element-p event "t") (when (plusp in-value) (decf in-value)))
             ((element-p event "c") (flush-cell))
             ((element-p event "row") (flush-row))))))
      (flush-row)
      (nreverse rows))))

(defun row-line (cells)
  "One Markdown table row."
  (format nil "| ~{~a~^ | ~} |"
          (mapcar (lambda (cell) (substitute #\Space #\| cell)) cells)))

(defun markdown-table (rows)
  "ROWS, a list of lists of strings, as one Markdown table; NIL when empty."
  (when rows
    (let* ((width (reduce #'max rows :key #'length))
           (padded (mapcar (lambda (row)
                             (append row (make-list (- width (length row)) :initial-element "")))
                           rows)))
      (format nil "~{~a~^~%~}"
              (list* (row-line (first padded))
                     (row-line (make-list width :initial-element "---"))
                     (mapcar #'row-line (rest padded)))))))

(defun xlsx-markdown (octets what)
  (let ((sheets (remove-if-not (lambda (name)
                                  (and (uiop:string-prefix-p "xl/worksheets/sheet" name)
                                       (ends-with-p name ".xml")))
                                (zip-names octets)))
        (shared (let ((part (zip-part octets "xl/sharedStrings.xml")))
                  (if part (xlsx-shared-strings (utf8-text part what)) #())))
        (names (xlsx-sheet-names (part-text octets "xl/workbook.xml" what))))
    (unless sheets
      (refuse "conversion_failed" "~a holds no worksheets: it is not a readable xlsx file" what))
    (tidy (format nil "~{~a~^~%~%~}"
                  (loop for name in (sort sheets #'< :key (lambda (name)
                                                             (part-number name "xl/worksheets/sheet" ".xml")))
                        for number from 1
                        collect (format nil "## ~a~%~%~a"
                                        (or (nth (1- number) names)
                                            (format nil "Sheet ~d" number))
                                        (markdown-table
                                         (xlsx-rows (part-text octets name what) shared))))))))

;;; --- html ----------------------------------------------------------------------------

(defun decode-entities (text)
  "TEXT with the character references an HTML page uses resolved."
  (with-output-to-string (out)
    (let ((position 0)
          (length (length text)))
      (loop while (< position length)
            do (let ((ampersand (position #\& text :start position)))
                 (if (null ampersand)
                     (progn (write-string text out :start position)
                            (setf position length))
                     (progn
                       (write-string text out :start position :end ampersand)
                       (let ((semicolon (and (< (1+ ampersand) length)
                                             (position #\; text :start (1+ ampersand)
                                                          :end (min length (+ ampersand 12))))))
                         (if (null semicolon)
                             (progn (write-char #\& out)
                                    (setf position (1+ ampersand)))
                             (let* ((body (subseq text (1+ ampersand) semicolon))
                                    (replacement
                                      (cond
                                        ((string-equal body "amp") "&")
                                        ((string-equal body "lt") "<")
                                        ((string-equal body "gt") ">")
                                        ((string-equal body "quot") "\"")
                                        ((string-equal body "apos") "'")
                                        ((string-equal body "nbsp") " ")
                                        ((and (plusp (length body)) (char= (char body 0) #\#))
                                         (let* ((hex (and (> (length body) 1)
                                                          (member (char body 1) '(#\x #\X))))
                                                (digits (subseq body (if hex 2 1)))
                                                (code (ignore-errors
                                                        (parse-integer digits
                                                                       :radix (if hex 16 10)
                                                                       :junk-allowed t))))
                                           (if (and code (< 0 code 1114112))
                                               (string (code-char code))
                                               "")))
                                        (t nil))))
                               (if replacement
                                   (progn (write-string replacement out)
                                          (setf position (1+ semicolon)))
                                   (progn (write-char #\& out)
                                          (setf position (1+ ampersand))))))))))))))

(defparameter +html-blocks+
  '("p" "div" "br" "tr" "table" "ul" "ol" "li" "blockquote" "pre" "section"
    "article" "header" "footer" "h1" "h2" "h3" "h4" "h5" "h6" "hr" "title")
  "The elements an HTML page breaks a line at: a reader wants the paragraph
structure the tags carry, not one long line.")

(defun collapse-into (piece out blank)
  "Write PIECE to OUT with every run of whitespace one space; answer the new
BLANK, which says whether the last character written was one."
  (loop for character across piece
        do (if (member character '(#\Space #\Tab #\Newline #\Return #\Page))
               (unless blank
                 (write-char #\Space out)
                 (setf blank t))
               (progn (write-char character out)
                      (setf blank nil))))
  blank)

(defun html-to-markdown (text)
  "TEXT, an HTML page, as Markdown's own paragraphs: headings with their #s,
list items with their dashes, table cells separated by bars, and the rest of
the markup gone."
  (let ((out (make-string-output-stream))
        (blank t)
        (skip 0))
    (labels ((break-line ()
               (write-char #\Newline out)
               (setf blank t)))
      (dolist (event (xml-events text))
        (case (first event)
          (:text
           (when (zerop skip)
             (setf blank (collapse-into (decode-entities (second event)) out blank))))
          (:start
           (let ((name (local-name (second event))))
             (cond
               ((member name '("script" "style" "head") :test #'string=) (incf skip))
               ((and (= (length name) 2) (char= (char name 0) #\h)
                     (digit-char-p (char name 1)))
                (break-line)
                (write-string (make-string (digit-char-p (char name 1)) :initial-element #\#) out)
                (write-char #\Space out)
                (setf blank nil))
               ((string= name "li") (break-line) (write-string "- " out) (setf blank nil))
               ((member name '("td" "th") :test #'string=)
                (write-string " | " out) (setf blank nil))
               ((member name +html-blocks+ :test #'string=) (break-line)))))
          (:empty
           (let ((name (local-name (second event))))
             (when (member name +html-blocks+ :test #'string=) (break-line))))
          (:end
           (let ((name (local-name (second event))))
             (cond
               ((member name '("script" "style" "head") :test #'string=)
                (when (plusp skip) (decf skip)))
               ((member name +html-blocks+ :test #'string=) (break-line)))))))
      (tidy (get-output-stream-string out)))))

;;; --- csv -----------------------------------------------------------------------------

(defun csv-rows (text)
  "TEXT's CSV records, each a list of field strings."
  (let ((rows '())
        (row '())
        (field '())
        (quoted nil)
        (position 0)
        (length (length text)))
    (labels ((end-field ()
               (push (coerce (nreverse field) 'string) row)
               (setf field '()))
             (end-row ()
               (end-field)
               (push (nreverse row) rows)
               (setf row '())))
      (loop while (< position length)
            do (let ((character (char text position)))
                 (cond
                   (quoted
                    (cond
                      ((char= character #\")
                       (if (and (< (1+ position) length)
                                (char= (char text (1+ position)) #\"))
                           (progn (push #\" field) (incf position 2))
                           (progn (setf quoted nil) (incf position))))
                      (t (push character field) (incf position))))
                   ((char= character #\") (setf quoted t) (incf position))
                   ((char= character #\,) (end-field) (incf position))
                   ((or (char= character #\Newline) (char= character #\Return))
                    (end-row)
                    (when (and (char= character #\Return) (< (1+ position) length)
                               (char= (char text (1+ position)) #\Newline))
                      (incf position))
                    (incf position))
                   (t (push character field) (incf position)))))
      (when (or field row) (end-row))
      (nreverse rows))))

;;; --- pdf -----------------------------------------------------------------------------
;;;
;;; Best-effort, and said so: every stream is inflated when its first bytes
;;; say zlib, and the text operators inside (Tj, TJ and the array of strings a
;;; TJ carries) are read out. A scan with no text layer, or a page whose text
;;; lives in compressed object streams this reader does not follow, answers
;;; nothing and refuses rather than answering noise.

(defun latin1 (octets)
  "OCTETS as a string of one character a byte."
  (map 'string #'code-char octets))

(defun try-inflate (octets)
  "OCTETS inflated, or NIL when they are not a DEFLATE stream."
  (handler-case (latin1 (inflate octets))
    (error () nil)))

(defun pdf-streams (text)
  "Every stream TEXT holds, as a latin-1 string, inflated where it is zlib."
  (let ((streams '())
        (position 0))
    (loop
      (let ((marker (search "stream" text :start2 position)))
        (when (null marker) (return (nreverse streams)))
        (let* ((start (stream-data-start text (+ marker 6)))
               (end (search "endstream" text :start2 start)))
          (when (null end) (return (nreverse streams)))
          (let* ((raw (subseq text start end))
                 (octets (map '(vector (unsigned-byte 8)) #'char-code raw)))
            (push (or (and (>= (length octets) 2) (= (aref octets 0) #x78)
                           (try-inflate (subseq octets 2)))
                      (try-inflate octets)
                      raw)
                  streams))
          (setf position (+ end 9)))))))

(defun stream-data-start (text from)
  "Where a PDF stream's bytes begin, past the end-of-line its keyword ends at."
  (let ((start from)
        (length (length text)))
    (loop while (and (< start length)
                     (member (char text start) '(#\Return #\Newline #\Space #\Tab)
                             :test #'char=))
          do (incf start))
    start))

(defun pdf-literal-string (text start)
  "The PDF string that begins after the ( at START. => (values NEXT TEXT)."
  (let ((out (make-string-output-stream))
        (position (1+ start))
        (depth 1)
        (length (length text)))
    (block literal
      (loop while (< position length)
          do (let ((character (char text position)))
               (cond
                 ((char= character #\\)
                  (let ((next (and (< (1+ position) length) (char text (1+ position)))))
                    (incf position 2)
                    (case next
                      (#\n (write-char #\Newline out))
                      (#\r (write-char #\Return out))
                      (#\t (write-char #\Tab out))
                      (#\b (write-char #\Backspace out))
                      (#\f (write-char #\Page out))
                      (#\Newline nil)
                      ((#\( #\) #\\ #\') (write-char next out))
                      (t (when next (write-char next out))))))
                 ((char= character #\() (incf depth) (write-char character out) (incf position))
                 ((char= character #\))
                  (decf depth)
                  (incf position)
                  (when (zerop depth)
                    (return-from literal (values position (get-output-stream-string out))))
                  (write-char character out))
                 (t (write-char character out) (incf position)))))
      (values position (get-output-stream-string out)))))

(defun pdf-word-end (content from)
  "Where the token that starts at FROM ends."
  (loop for at from from below (length content)
        while (not (member (char content at)
                           '(#\Space #\Newline #\Return #\Tab #\Page
                             #\( #\) #\< #\> #\[ #\] #\/ #\%)
                           :test #'char=))
        finally (return at)))

(defun pdf-hex-string (hex)
  "The text a PDF hexadecimal string draws."
  (let ((digits (remove-if-not (lambda (character) (digit-char-p character 16)) hex)))
    (with-output-to-string (out)
      (loop for index from 0 below (- (length digits) (mod (length digits) 2)) by 2
            do (write-char (code-char (parse-integer digits :start index :end (+ index 2) :radix 16))
                           out)))))

(defun pdf-content-lines (content)
  "The text one PDF content stream draws, one string a line."
  (let ((lines '())
        (line (make-string-output-stream))
        (pending '())
        (position 0)
        (length (length content)))
    (labels ((flush-pending ()
               (dolist (piece (nreverse pending)) (write-string piece line))
               (setf pending '()))
             (flush-line ()
               (flush-pending)
               (let ((text (string-trim '(#\Space #\Tab #\Return)
                                         (get-output-stream-string line))))
                 (when (plusp (length text)) (push text lines)))))
      (loop while (< position length)
            do (let ((character (char content position)))
                 (cond
                   ((member character '(#\Space #\Newline #\Return #\Tab #\Page)
                            :test #'char=)
                    (incf position))
                   ((char= character #\%) (setf position (or (position #\Newline content :start position)
                                                             length)))
                   ((char= character #\()
                    (multiple-value-bind (next piece) (pdf-literal-string content position)
                      (push piece pending)
                      (setf position next)))
                   ((char= character #\<)
                    (if (and (< (1+ position) length) (char= (char content (1+ position)) #\<))
                        (incf position 2)
                        (let ((close (or (position #\> content :start position) length)))
                          (push (pdf-hex-string (subseq content (1+ position) close)) pending)
                          (setf position (1+ close)))))
                   ((char= character #\>) (incf position))
                   ((char= character #\[) (incf position))
                   ((char= character #\]) (incf position))
                   ((char= character #\/) (setf position (pdf-word-end content (1+ position))))
                   ((or (alpha-char-p character) (char= character #\'))
                    (let* ((end (pdf-word-end content position))
                           (word (subseq content position end)))
                      (cond
                        ((member word '("Tj" "TJ" "'" "\"") :test #'string=) (flush-pending))
                        ((member word '("Td" "TD" "T*" "ET" "BT") :test #'string=) (flush-line)))
                      (setf position (max end (1+ position)))))
                   (t (incf position)))))
      (flush-line)
      (nreverse lines))))

(defun pdf-markdown (octets what)
  (let ((lines (loop for stream in (pdf-streams (latin1 octets))
                     append (pdf-content-lines stream))))
    (unless lines
      (refuse "conversion_failed"
              "~a holds no text this reader finds (a scanned page, or text in compressed object streams): convert it with a PDF reader of your own (pdftotext, for example) and pass the text file"
              what))
    (tidy (format nil "~{~a~%~}" lines))))

;;; --- one file in, Markdown out ---------------------------------------------------------

(defun tidy (text)
  "TEXT with trailing spaces dropped, a run of blank lines cut to one, and the
ends trimmed: what every reader above answers before it is handed on."
  (let ((kept '())
        (blank nil))
    (dolist (line (uiop:split-string text :separator '(#\Newline)))
      (let ((trimmed (string-right-trim '(#\Space #\Tab #\Return) line)))
        (if (zerop (length trimmed))
            (unless (or blank (null kept))
              (push "" kept)
              (setf blank t))
            (progn (push trimmed kept)
                   (setf blank nil)))))
    (format nil "~{~a~^~%~}" (nreverse kept))))

(defun markdown-for-file (path extension)
  "PATH's Markdown, read as EXTENSION says it is read."
  (let ((what (file-namestring path)))
    (cond
      ((string= extension "pdf") (pdf-markdown (read-octets path) what))
      ((string= extension "docx") (docx-markdown (read-octets path) what))
      ((string= extension "pptx") (pptx-markdown (read-octets path) what))
      ((string= extension "xlsx") (xlsx-markdown (read-octets path) what))
      ((member extension '("html" "htm") :test #'string=)
       (html-to-markdown (utf8-text (read-octets path) what)))
      ((string= extension "csv")
       (let ((table (markdown-table (csv-rows (utf8-text (read-octets path) what)))))
         (if (null table)
             (utf8-text (read-octets path) what)
             table)))
      (t (utf8-text (read-octets path) what)))))

(defun convert-file (path &key max-chars)
  "PATH's Markdown, cut at MAX-CHARS or the section's max_output_chars.
=> (values MARKDOWN SIZE EXTENSION NAME KEPT TOTAL)."
  (let* ((real (resolve-readable-file path))
         (size (with-open-file (in real :element-type '(unsigned-byte 8))
                 (file-length in)))
         (extension (extension-of real))
         (markdown (markdown-for-file real extension))
         (limit (min (or (and (integerp max-chars) (plusp max-chars) max-chars)
                         most-positive-fixnum)
                     (max-output-chars)))
         (total (length markdown))
         (kept (min total limit)))
    (when (> total limit)
      (setf markdown (format nil "~a~%~%[truncated: first ~:d of ~:d characters]"
                             (subseq markdown 0 limit) kept total)))
    (values markdown size extension (file-namestring real) kept total)))

;;; --- what the model calls ---------------------------------------------------------------

(nlk:define-contract convert-request
  (:doc "Convert one local file to Markdown: an absolute path in, the document's text as Markdown out. Reads PDF, DOCX, PPTX, XLSX, HTML, CSV, JSON, TXT and MD; after every symbolic link is followed, it refuses the Nodecode home, a path component that starts with '.', a relative path, an http/https/file URL, a missing file, a directory, an unsupported extension or a file over the section's max_file_bytes. It reads local files only and fetches no page.")
  ("path" :text :required t
          :doc "the absolute local file path to convert, for example /home/me/docs/report.pdf")
  ("max_chars" :integer
               :doc "the longest Markdown to return; the cut is the smaller of this and the section's max_output_chars, and a value below 1 is refused"))

(defun run-convert (request)
  "The doc_convert tool: a refusal is answered, never signalled."
  (handler-case
      (let ((max-chars (convert-request-max-chars request)))
        (when (and max-chars (< max-chars 1))
          (refuse "bad_argument"
                  "max_chars must be 1 or more: pass a positive integer, or leave it out and the section's max_output_chars is the cut"))
        (multiple-value-bind (markdown size extension name kept total)
            (convert-file (convert-request-path request) :max-chars max-chars)
          (format nil "Converted ~a (~a, ~:d bytes) to Markdown~a.~%~%~a"
                  name extension size
                  (if (< kept total)
                      (format nil " (the first ~:d of ~:d characters)" kept total)
                      "")
                  markdown)))
    (doc-markdown-error (condition) (failure-text condition))
    (error (condition)
      (format nil "ERROR: doc_convert: internal: ~a - retry once, and if it repeats tell the user the conversion failed without pasting the file"
              condition))))

;;; --- what the operator types --------------------------------------------------------------

(defun run-doc (args session-id)
  "/doc PATH => PATH's Markdown, or why it was not read."
  (declare (ignore session-id))
  (let ((path (nlk:trimmed (or args ""))))
    (if (zerop (length path))
        "doc: name a file to convert, for example /doc /home/me/docs/report.pdf"
        (handler-case
            (multiple-value-bind (markdown size extension name)
                (convert-file path)
              (format nil "doc: ~a (~a, ~:d bytes)~%~%~a" name extension size markdown))
          (doc-markdown-error (condition)
            (format nil "doc: ~a: ~a" (doc-markdown-error-kind condition)
                    (doc-markdown-error-detail condition)))))))

;;; --- what the manual says ------------------------------------------------------------------

(defparameter +manual+
  "doc-markdown: converts one local file to Markdown -- a PDF, DOCX, PPTX,
XLSX, HTML, CSV, JSON, TXT or MD file, read on this machine, nothing fetched.
The model calls it as the tool doc_convert with an absolute path, /doc PATH
does the same from the shell. The guard is the original plugin's: every
symbolic link is followed before anything is judged, then a path component
that starts with '.' is refused, the Nodecode home is refused, a relative
path, an http/https/file URL, a missing file, a directory and an extension
outside the list are refused, and the size cap is the section's
max_file_bytes. The section doc-markdown holds max_file_bytes (20 MiB by
default), max_output_chars (the longest Markdown kept, 200000 by default)
and refuse_hidden. A .docx, .xlsx or .pptx is read as the zip of OOXML parts
it is, a PDF's streams are inflated and its text operators read best-effort
(a scan, or text in compressed object streams, answers nothing and says so),
and a .xls workbook is not read at all: save it as .xlsx or .csv first. The
worker is not a sandbox: a file is read by this process, so a hostile
document is still a document this process parsed."
  "What (help :doc-markdown) answers while the cell runs.")

;;; --- the cell --------------------------------------------------------------------------------

(nle:define-cell doc-markdown
  (:section ("doc-markdown")
    (:guide "nothing here is required: max_file_bytes caps what is read, max_output_chars caps what is returned, refuse_hidden refuses any path component that starts with '.'")
    ("max_file_bytes" :integer :default 20971520 :min 1
                      :doc "the largest file read, in bytes (20 MiB by default)")
    ("max_output_chars" :integer :default 200000 :min 1
                        :doc "the longest Markdown returned, in characters")
    ("refuse_hidden" :boolean :default t
                     :doc "refuse a path with a component that starts with '.', judged after symbolic links are followed"))
  (:tool "doc_convert" (nlk:contract 'convert-request) 'run-convert)
  (:command "doc" 'run-doc
            :description "Convert one local file to Markdown"
            :argument-hint "PATH")
  (:help :doc-markdown
         "doc-markdown: one local PDF, DOCX, PPTX, XLSX, HTML, CSV, JSON, TXT or MD file to Markdown - the doc_convert tool, /doc"
         +manual+))
