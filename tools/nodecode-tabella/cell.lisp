;;;; cell.lisp --- documents read, converted and written, with pandoc.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three tools over the programs a person already has:
;;;;
;;;;   doc_read               a document's text: pandoc for office and
;;;;                          marked-up formats, pdftotext for a PDF, a direct
;;;;                          read for plain text.
;;;;   doc_read_meta          what the filesystem knows: format, size, mtime.
;;;;   doc_convert            pandoc from one format to another, with the
;;;;                          flag allowlist that keeps a --lua-filter out.
;;;;   doc_create_document    content written as a document, through pandoc.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "tabella": {"output_dir": "~/Documents", "pandoc": "pandoc"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from dyiapanis's Hermes Agent plugin (see NOTICE): the four
;;;; tools and their schemas, the format-name grammar and the pandoc flag
;;;; allowlist, the PDF-as-input lane, and the message each tool answers.

(in-package #:nodecode-tabella)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Tabella reads, converts and writes documents with pandoc, the converter this machine
already has. Four tools, which a person can also call from eval in the tabella package
(tabella:read-text, tabella:document-meta, tabella:convert-file, tabella:create-file).
  doc_read             {path, max_chars}
      A document's text, as Markdown: DOCX, ODT, EPUB, PPTX, HTML, LaTeX, RST and Markdown
      through pandoc; a PDF through pdftotext, which reads the text layer only (a scanned
      page has none); a plain text file read directly. An image is refused: no OCR here.
  doc_read_meta        {path}
      The format (the extension), the size in bytes and when the file last changed.
  doc_convert          {input_path, to_format, from_format, output_path, extra_args, category}
      pandoc from one format to another: markdown to pdf, docx, html, odt or epub, and any
      pair of built-in pandoc formats. A PDF as input is extracted with pdftotext first;
      a PDF as output needs a LaTeX engine (xelatex or pdflatex) on the machine. extra_args
      takes only --toc, --number-sections and -Vkey=value: --filter, --lua-filter and
      --pdf-engine run code and are refused.
  doc_create_document {format, content, title, output_path, category}
      content, a Markdown or HTML string or a list of blocks (heading, paragraph, bullets,
      quote, code, table, page_break), written as the format asked for. Spreadsheets and
      slide decks are not written here.
Without output_path a document is written into the cell's output_dir, under category when
one is given, and the answer names the file it wrote. Section setup, when a tool refuses
for want of it, is in /settings: output_dir is the one required member."
  "What (help :tabella) answers while the cell runs.")

;;; --- formats and suffixes --------------------------------------------------------

(defparameter *text-suffixes*
  '("txt" "md" "markdown" "json" "html" "htm" "xml" "yaml" "yml" "toml" "ini" "cfg"
    "conf" "rst" "fountain" "log" "py" "js" "ts" "css" "org")
  "Suffixes read straight off the disk, as the original read.py does.")

(defparameter *image-suffixes*
  '("jpg" "jpeg" "png" "tif" "tiff" "webp" "bmp" "gif")
  "Suffixes that would need OCR, which this cell does not carry.")

(defparameter *suffix-formats*
  '(("md" . "markdown") ("markdown" . "markdown") ("txt" . "markdown")
    ("html" . "html") ("htm" . "html") ("tex" . "latex") ("typ" . "typst")
    ("docx" . "docx") ("odt" . "odt") ("epub" . "epub") ("rst" . "rst")
    ("org" . "org") ("pptx" . "pptx") ("csv" . "csv") ("pdf" . "pdf"))
  "The extension-to-pandoc-format map of the original convert.py, plus the
formats pandoc reads on its own.")

(defparameter *write-formats*
  '("markdown" "docx" "pdf" "html" "odt" "epub" "rst" "latex")
  "Formats doc_create_document writes: pandoc's document writers. A
spreadsheet or a slide deck took the original's Python builders and is
refused here by name.")

(defparameter *extra-args-allowed* '("--toc" "--number-sections")
  "The only pandoc flags extra_args passes: a --filter, --lua-filter or
--pdf-engine is a Lua or a program run on the model's say-so.")

(defun suffix-of (path)
  "PATH's extension, lowercased, or the empty string."
  (string-downcase (or (pathname-type (pathname path)) "")))

(defun infer-format (path)
  "The pandoc format the extension of PATH names, as pandoc infers it."
  (or (cdr (assoc (suffix-of path) *suffix-formats* :test #'string=))
      (suffix-of path)))

(defun format-name-p (name)
  "Whether NAME is a built-in pandoc format name: letters, digits and
underscores, with +ext/-ext modifiers and no space, path or .lua in it. A
name ending in .lua is a reader pandoc would execute."
  (and (stringp name)
       (plusp (length name))
       (let ((start 0) (ok t))
         (loop for i from 0 below (length name)
               for c = (char name i)
               do (cond ((or (char<= #\a c #\z) (char<= #\0 c #\9) (char= c #\_)))
                        ((member c '(#\+ #\-))
                         (when (= i start) (setf ok nil))
                         (setf start (1+ i)))
                        (t (setf ok nil))))
         (and ok (< start (length name)) t))))

(defun check-format (name which)
  "NAME, a pandoc format, or the refusal naming WHICH it was."
  (unless (format-name-p name)
    (fail "unsupported ~a ~s: a built-in pandoc format name (letters, digits and _, with optional +ext/-modifiers)"
          which name))
  name)

(defun pandoc-variable-p (argument)
  "Whether ARGUMENT is a one-token -Vkey=value pandoc variable."
  (and (> (length argument) 2)
       (uiop:string-prefix-p "-V" argument)
       (let ((rest (subseq argument 2)))
         (and (plusp (or (position #\= rest) 0))
              (find #\= rest)
              (not (find-if (lambda (c) (member c '(#\Space #\Tab))) rest))))))

(defun checked-extra-args (args)
  "ARGS as a list of pandoc flags, or the refusal that names the one flag
that would run code."
  (loop for argument in (coerce (or args '()) 'list)
        do (unless (or (member argument *extra-args-allowed* :test #'string=)
                       (pandoc-variable-p argument))
             (fail "unsupported pandoc argument ~s: extra_args allows only --toc, --number-sections and -Vkey=value (a --filter or --lua-filter runs code)"
                   argument))
        collect argument))

(defun pandoc-program ()
  "The pandoc this machine has, or the refusal that says which to install."
  (let ((program (nlk:trimmed (or (setting :pandoc) ""))))
    (setf program (if (plusp (length program)) program "pandoc"))
    (unless (have-program-p program)
      (fail "pandoc is not on PATH (~a): install it (apt install pandoc, brew install pandoc)" program))
    program))

(defun pdf-engine ()
  "The LaTeX engine a PDF is written with: pdf_engine when set, else
whichever of xelatex and pdflatex this machine has."
  (let ((asked (nlk:trimmed (or (setting :pdf-engine) ""))))
    (cond ((plusp (length asked))
           (unless (have-program-p asked) (fail "the pdf engine ~a is not on PATH" asked))
           asked)
          ((have-program-p "xelatex") "xelatex")
          ((have-program-p "pdflatex") "pdflatex")
          (t (fail "writing a PDF needs a LaTeX engine: install texlive-xetex or texlive-latex-base, or ask for docx or html instead")))))

;;; --- where a document goes --------------------------------------------------

(defun output-dir ()
  "The folder documents are written to, made if it is not there."
  (let* ((configured (nlk:trimmed (or (setting :output-dir) "~/Documents")))
         (dir (nlk:expand-home (if (plusp (length configured)) configured "~/Documents"))))
    (unless (uiop:absolute-pathname-p dir)
      (fail "output_dir ~s is not an absolute path or a ~~/... path" configured))
    (ensure-directories-exist (uiop:ensure-directory-pathname dir))
    (uiop:ensure-directory-pathname dir)))

(defun category-name (category)
  "CATEGORY as a subfolder of output_dir, or NIL; a category that climbs out
of it is refused."
  (let ((category (and category (nlk:trimmed category))))
    (when (and category (plusp (length category)))
      (when (or (search ".." category) (member (char category 0) '(#\/ #\\)))
        (fail "category ~s is a path out of output_dir: give a name such as reports" category))
      (string-right-trim "/\\" category))))

(defun destination (given category stem format)
  "Where a document is written: a path asked for, else
OUTPUT-DIR/[CATEGORY/]STEM.FORMAT."
  (let ((given (and given (nlk:trimmed given))))
    (if (and given (plusp (length given)))
        (let ((path (namestring (nlk:expand-home given))))
          (ensure-directories-exist path)
          path)
        (let* ((dir (output-dir))
               (category (category-name category))
               (dir (if category
                        (ensure-directories-exist
                         (merge-pathnames (format nil "~a/" category) dir))
                        dir)))
          (namestring (merge-pathnames (format nil "~a.~a" stem format) dir))))))

(defun outside-output-dir (path)
  "PATH said differently when it is not under output_dir, so an answer that
wrote somewhere else says so."
  (handler-case
      (let* ((dir (namestring (truename (output-dir))))
             (file (namestring (truename (uiop:ensure-directory-pathname
                                           (directory-namestring (namestring path)))))))
        (unless (uiop:string-prefix-p dir file)
          (format nil " (outside ~a)" (namestring (truename (output-dir))))))
    (error () nil)))

;;; --- reading ----------------------------------------------------------------

(defun plain-text (path)
  "PATH read as a text file: UTF-8, else Latin-1, a NUL byte meaning it was
never text."
  (let* ((octets (with-open-file (in path :element-type '(unsigned-byte 8))
                    (let ((buffer (make-array (file-length in) :element-type '(unsigned-byte 8))))
                      (read-sequence buffer in)
                      buffer)))
         (nul (position 0 octets :end (min 8192 (length octets)))))
    (when nul
      (fail "~a looks binary (a NUL byte at offset ~d) - not a text file" (file-namestring path) nul))
    (let ((text (or (ignore-errors (sb-ext:octets-to-string octets :external-format :utf-8))
                    (sb-ext:octets-to-string octets :external-format :latin-1))))
      (if (and (plusp (length text)) (char= (char text 0) (code-char #xFEFF)))
          (subseq text 1)
          text))))

(defun pdf-text (path)
  "PATH's text through pdftotext, which reads a text layer and no scan."
  (let ((program (nlk:trimmed (or (setting :pdftotext) ""))))
    (setf program (if (plusp (length program)) program "pdftotext"))
    (unless (have-program-p program)
      (fail "reading a PDF needs ~a on PATH (poppler-utils)" program))
    (multiple-value-bind (out err code) (run! (list program "-enc" "UTF-8" (namestring path) "-"))
      (cond ((plusp (length (nlk:trimmed out))) out)
            ((zerop code) (fail "~a found no text in ~a: a scanned page needs OCR, which this cell does not carry"
                                program (file-namestring path)))
            (t (fail "~a could not read ~a (exit ~d)~@[: ~a]" program (file-namestring path) code
                     (mentioned-error err)))))))

(defun pandoc-text (path)
  "PATH's text through pandoc, as GitHub-flavoured Markdown."
  (let ((program (pandoc-program)))
    (multiple-value-bind (out err code) (run! (list program (namestring path) "--to" "gfm"))
      (if (and (zerop code) (plusp (length (nlk:trimmed out))))
          out
          (fail "pandoc could not read ~a (exit ~d)~@[: ~a]" (file-namestring path) code
                (mentioned-error err))))))

(defun read-text (path &key max-chars)
  "PATH's text, by what it is: a text file read directly, a PDF through
pdftotext, anything else through pandoc. => a string, at most MAX-CHARS."
  (let* ((path (namestring (nlk:expand-home (princ-to-string path))))
         (suffix (suffix-of path))
         (text (progn
                 (unless (probe-file path) (fail "no file at ~a" path))
                 (cond ((member suffix *text-suffixes* :test #'string=) (plain-text path))
                     ((member suffix *image-suffixes* :test #'string=)
                      (fail "~a is an image: reading one needs OCR, which this cell does not carry" (file-namestring path)))
                       ((string= suffix "pdf") (pdf-text path))
                       (t (pandoc-text path))))))
    (if (and max-chars (plusp max-chars) (> (length text) max-chars))
        (subseq text 0 max-chars)
        text)))

(defun document-meta (path)
  "What the filesystem knows about PATH: format, size and modification time."
  (let* ((path (namestring (nlk:expand-home (princ-to-string path))))
         (probe (or (probe-file path) (fail "no file at ~a" path))))
    (with-open-file (in probe :element-type '(unsigned-byte 8))
      (nlk:json-object "path" path
                       "format" (suffix-of path)
                       "size_bytes" (file-length in)
                       "mtime" (file-write-date probe)))))

;;; --- converting -------------------------------------------------------------

(defun pandoc-to (source format &key from output extra)
  "Run pandoc on SOURCE into OUTPUT as FORMAT. => OUTPUT, the pdf engine
used when it was a PDF, and the file written."
  (let* ((engine (and (string= format "pdf") (pdf-engine)))
         (argv (append (list (pandoc-program) (namestring source) "-o" output
                             "-f" from "-t" format)
                       (when engine (list "--pdf-engine" engine))
                       extra)))
    (multiple-value-bind (out err code) (run! argv)
      (declare (ignore out))
      (unless (zerop code)
        (fail "pandoc failed (exit ~d)~@[: ~a]" code (mentioned-error err)))
      (unless (probe-file output)
        (fail "pandoc exited 0 and wrote nothing to ~a" output))
      (values output engine))))

(defun converted-markdown (source format)
  "SOURCE read as the format it is, through a temporary Markdown file, for a
source pandoc cannot read itself (a PDF). => (values PATH TMP PATH-TO-DELETE)."
  (declare (ignore format))
  (let ((text (read-text source)))
    (unless (plusp (length (nlk:trimmed text)))
      (fail "~a has no extractable text" (file-namestring source)))
    (let ((tmp (temp-file ".md")))
      (values (write-text tmp text) tmp))))

(defun convert-file (path format &key from output category extra-args)
  "pandoc the document at PATH to FORMAT. => the answer as a JSON object."
  (let* ((source (namestring (nlk:expand-home (princ-to-string path))))
         (target (string-downcase (nlk:trimmed (princ-to-string format)))))
    (unless (probe-file source) (fail "no file at ~a" source))
    (check-format target "to_format")
    (let* ((from (let ((asked (and from (nlk:trimmed from))))
                   (if (and asked (plusp (length asked)))
                       (string-downcase (check-format asked "from_format"))
                       (infer-format source))))
           (extra (checked-extra-args extra-args))
           (stem (pathname-name (pathname source)))
           (out (destination output category stem target))
           (tmp nil))
      (unwind-protect
           (let* ((pdf-in (string= from "pdf"))
                  (source (if pdf-in
                              (multiple-value-bind (written temp) (converted-markdown source from)
                                (setf tmp temp)
                                written)
                              source)))
             (multiple-value-bind (written engine)
                 (pandoc-to source target :from (if pdf-in "markdown" from) :output out :extra extra)
               (nlk:json-object
                "success" t
                "output_path" written
                "from_format" (if pdf-in "pdf" from)
                "to_format" target
                "pdf_engine" engine
                "message" (format nil "Converted ~a -> ~a: ~a~@[~a~]~@[ (pdf engine ~a)~]"
                                   (file-namestring (namestring path)) target written
                                   (outside-output-dir written) engine))))
        (when tmp (ignore-errors (delete-file tmp)))))))

;;; --- creating ----------------------------------------------------------------

(defun field (object key)
  "OBJECT's member KEY as a string, or NIL."
  (and (hash-table-p object) (nlk:json-value object :string key)))

(defun field-list (object key)
  "OBJECT's member KEY as a list of values, or NIL."
  (and (hash-table-p object) (coerce (or (nlk:json-value object :array key) '()) 'list)))

(defun table-row (row)
  "ROW, an array of cells, as one Markdown table line."
  (format nil "| ~{~a~^ | ~} |"
          (mapcar (lambda (cell) (or (princ-to-string (if (null cell) "" cell)) ""))
                  (coerce row 'list))))

(defun render-block (block)
  "BLOCK, a string or one object of the original tool's block list, as
Markdown."
  (cond ((stringp block) block)
        ((hash-table-p block)
         (let ((type (string-downcase (or (field block "type") (field block "kind") "paragraph")))
               (text (or (field block "text") (field block "content") "")))
           (cond ((string= type "heading")
                  (format nil "~a ~a"
                          (make-string (max 1 (min 6 (or (and (hash-table-p block)
                                                              (nlk:json-value block :integer "level"))
                                                          2)))
                                       :initial-element #\#)
                          text))
                 ((string= type "bullets")
                  (format nil "~{~a~^~%~}" (mapcar (lambda (item) (format nil "- ~a" item))
                                                   (field-list block "items"))))
                 ((string= type "quote") (format nil "> ~a" text))
                 ((string= type "code")
                  (format nil "```~@[ ~a~]~%~a~%```" (field block "language") text))
                 ((string= type "table")
                  (let ((rows (mapcar #'table-row (field-list block "rows"))))
                    (if rows
                        (format nil "~{~a~^~%~}" (list* (first rows)
                                                         (format nil "|~{~a~^|~}|"
                                                                 (make-list (length (coerce (first (field-list block "rows")) 'list))
                                                                            :initial-element " --- "))
                                                         (rest rows)))
                        "")))
                 ((string= type "page_break") "---")
                 (t text))))
        (t (princ-to-string block))))

(defun render-content (content)
  "CONTENT, a string or the object/array of blocks the original tool took, as
Markdown."
  (cond ((stringp content) content)
        ((vectorp content) (format nil "~{~a~^~%~%~}" (mapcar #'render-block (coerce content 'list))))
        ((hash-table-p content)
         (or (field content "markdown")
             (field content "html")
             (let ((blocks (or (field-list content "blocks") (field-list content "sections"))))
               (if blocks
                   (format nil "~{~a~^~%~%~}" (mapcar #'render-block blocks))
                   (render-block content)))))
        ((null content) "")
        (t (princ-to-string content))))

(defun slug (text)
  "TEXT as a file name: lowercase letters and digits, a dash where anything
else was."
  (let ((out (make-string-output-stream)))
    (loop for c across (string-downcase (or text ""))
          do (write-char (if (or (char<= #\a c #\z) (char<= #\0 c #\9)) c #\-) out))
    (let ((slug (string-trim "-" (get-output-stream-string out))))
      (if (plusp (length slug)) (subseq slug 0 (min 60 (length slug))) "document"))))

(defun create-file (format content &key title output category)
  "Write CONTENT as a document in FORMAT through pandoc. => the answer as a
JSON object."
  (let* ((target (string-downcase (nlk:trimmed (princ-to-string format))))
         (title (and title (nlk:trimmed title))))
    (check-format target "format")
    (unless (member target *write-formats* :test #'string=)
      (fail "~a is not a format this cell writes: ~{~a~^, ~} (pandoc writes documents; spreadsheets and slide decks took the original's Python builders)"
            target *write-formats*))
    (let* ((markdown (render-content content))
           (html (and (plusp (length (nlk:trimmed markdown)))
                      (char= #\< (char (nlk:trimmed markdown) 0))))
           (stem (slug (if (and title (plusp (length title))) title
                           (format nil "document-~d" (get-universal-time)))))
           (out (destination output category stem target))
           (tmp (temp-file (if html ".html" ".md"))))
      (unless (plusp (length (nlk:trimmed markdown)))
        (fail "content is empty: give a Markdown or HTML string, or a list of blocks"))
      (unwind-protect
           (progn
             (write-text tmp (if (and title (plusp (length title)))
                                 (format nil "# ~a~%~%~a" title markdown)
                                 markdown))
             (multiple-value-bind (written engine)
                 (pandoc-to tmp target :from (if html "html" "markdown") :output out)
               (declare (ignore engine))
               (nlk:json-object
                "success" t
                "output_path" written
                "format" target
                "message" (format nil "Document created (~a): ~a~@[~a~]"
                                  target written (outside-output-dir written)))))
        (ignore-errors (delete-file tmp))))))

;;; --- the tools ---------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer; a refusal becomes the call's failure text, never an
unhandled error."
  `(handler-case (progn ,@body)
     (tabella-error (condition)
       (nle:failure (format nil "ERROR: ~a" (tabella-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: tabella: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun tool-read (call)
  "The doc_read tool: PATH's text, capped."
  (answering
    (let* ((path (read-document-path call))
           (cap (or (read-document-max-chars call) (setting :max-chars)))
           (text (read-text path :max-chars cap)))
      (nlk:encode-json-object
       (nlk:json-object "success" t "path" path "text" text "length" (length text)
                        "message" (format nil "Read ~d characters from ~a" (length text) path))))))

(defun tool-meta (call)
  "The doc_read_meta tool: what the filesystem knows about PATH."
  (answering
    (let ((path (document-metadata-path call)))
      (nlk:encode-json-object
       (nlk:json-object "success" t "path" path "metadata" (document-meta path))))))

(defun tool-convert (call)
  "The doc_convert tool: pandoc PATH to a format."
  (answering
    (nlk:encode-json-object
     (convert-file (convert-document-input-path call)
                   (convert-document-to-format call)
                   :from (convert-document-from-format call)
                   :output (convert-document-output-path call)
                   :category (convert-document-category call)
                   :extra-args (convert-document-extra-args call)))))

(defun tool-create (call)
  "The doc_create_document tool: content written as a document."
  (answering
    (nlk:encode-json-object
     (create-file (create-document-document-format call)
                  (create-document-content call)
                  :title (create-document-title call)
                  :output (create-document-output-path call)
                  :category (create-document-category call)))))

;;; --- /tabella ----------------------------------------------------------------

(defun program-of (key default)
  "The program the section names under KEY, DEFAULT when it names none."
  (let ((name (nlk:trimmed (or (setting key) ""))))
    (if (plusp (length name)) name default)))

(defun status-line ()
  "The cell as it stands: where documents go, and which program is missing."
  (format nil "tabella: documents in ~a~%  pandoc: ~a~%  pdftotext: ~a~%  pdf engine: ~a"
          (namestring (output-dir))
          (or (have-program-p (program-of :pandoc "pandoc")) "not found - install pandoc")
          (or (have-program-p (program-of :pdftotext "pdftotext")) "not found - install poppler-utils")
          (handler-case (pdf-engine) (tabella-error (condition) (tabella-error-detail condition)))))

(defun panel ()
  "The rows bare /tabella offers: read a document, or convert one."
  (nle:list-dialog
   :title "Tabella" :summary "documents: read, convert, create"
   :rows (list (nle:ask-row "Read a document" "Path" "/tabella read"
                             :placeholder "~/Documents/notes.docx" :verb "read"
                             :detail "its text comes back as Markdown")
               (nle:ask-row "Convert a document" "Path and format" "/tabella convert"
                             :placeholder "~/Documents/report.md pdf" :verb "convert"
                             :detail "pandoc writes pdf, docx, html, odt and epub"))))

(defun run-slash (args session-id)
  "/tabella typed alone: where documents go and a panel whose rows read or
convert one. /tabella read PATH answers a document's text; /tabella convert
PATH FORMAT writes it in another format."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (words (and (plusp (length text))
                     (remove "" (uiop:split-string text :separator '(#\Space #\Tab)) :test #'string=)))
         (head (and words (string-downcase (first words)))))
    (cond ((null words) (values (status-line) (panel)))
          ((string= head "read")
           (let ((path (or (second words) (fail "read needs a path: /tabella read ~~/Documents/thing.docx"))))
             (let ((body (read-text path)))
               (format nil "~a~%~%~a"
                       (format nil "read ~d characters from ~a" (length body) path)
                       (nlk:clip body 1500)))))
          ((string= head "convert")
           (destructuring-bind (path format &rest ignored) (rest words)
             (declare (ignore ignored))
             (unless (and path format)
               (fail "convert needs a path and a format: /tabella convert ~~/Documents/report.md pdf"))
             (values (gethash "message" (convert-file path format)))))
          (t (fail "~s is no tabella verb: read PATH, convert PATH FORMAT, or bare /tabella" text)))))

;;; --- the declaration ---------------------------------------------------------

(nle:define-cell tabella
  (:section ("tabella")
    (:guide "output_dir is where created and converted documents are written; pandoc converts and reads office formats, pdftotext reads PDF text, and writing a PDF needs a LaTeX engine (xelatex or pdflatex) on PATH; next step: ask for a document to be turned into a PDF")
    ("output_dir" :path :required t
     :doc "the folder documents are written to, such as ~/Documents")
    ("pandoc" :string :default "pandoc"
     :doc "the pandoc program's name on PATH (or a path to it)")
    ("pdftotext" :string :default "pdftotext"
     :doc "the pdftotext program (poppler) that reads a PDF's text layer")
    ("pdf_engine" :string :default ""
     :doc "xelatex or pdflatex to write a PDF; empty picks whichever is on PATH")
    ("max_chars" :integer :default 200000 :min 1
     :doc "the cap on the text doc_read answers when the call names none"))
  (:help :tabella
         "tabella: doc_read, doc_read_meta, doc_convert and doc_create_document read, convert and write documents with pandoc"
         *manual*)
  (:tool "doc_read" (nlk:contract 'read-document) #'tool-read)
  (:tool "doc_read_meta" (nlk:contract 'document-metadata) #'tool-meta)
  (:tool "doc_convert" (nlk:contract 'convert-document) #'tool-convert)
  (:tool "doc_create_document" (nlk:contract 'create-document) #'tool-create)
  (:command "tabella" 'run-slash
            :catches 'tabella-error
            :description "Documents: read one, or convert one to another format"
            :argument-hint "read PATH | convert PATH FORMAT")
  (:start (lambda ()
            ;; output_dir is the one member every write stands on. A section
            ;; that never answered it still starts the rest of the cell, so the
            ;; start is where a missing one is refused, by name, rather than at
            ;; the first document written into a folder nobody chose.
            (unless (plusp (length (nlk:trimmed (or (setting :output-dir) ""))))
              (fail "the tabella section names no output_dir: set it to the folder documents should be written into, such as ~~/Documents")))))
