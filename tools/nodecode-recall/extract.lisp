;;;; extract.lisp --- what a file says, as text, without leaving the machine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Text files are read directly. HTML and XML lose their tags. A PDF is
;;;; read by pdftotext and an Office file by unzip, when the operator already
;;;; has them (README says why); without one the file is still indexed by the
;;;; words in its name, and the note says what could not be read.

(in-package #:nodecode-recall)

(defparameter *text-extensions*
  '("txt" "md" "markdown" "rst" "log" "csv" "tsv" "json" "jsonl" "yaml"
    "yml" "toml" "ini" "cfg" "conf" "xml" "html" "htm" "py" "js" "ts"
    "tsx" "jsx" "sh" "bash" "zsh" "rb" "go" "rs" "java" "kt" "c" "h"
    "cpp" "hpp" "cs" "php" "sql" "tex" "org" "srt" "vtt" "eml" "ics")
  "Extensions read as text as they are.")

(defparameter *office-extensions* '("docx" "xlsx" "pptx" "odt" "ods" "odp")
  "Zip archives whose text is one XML member, read through unzip.")

(defparameter *office-members*
  '(("docx" . "word/document.xml") ("odt" . "content.xml")
    ("ods" . "content.xml") ("odp" . "content.xml")
    ("xlsx" . "xl/sharedStrings.xml") ("pptx" . "ppt/slides/slide1.xml"))
  "The member each Office format keeps its words in: the first slide or
sheet, which is as far as one extraction reads.")

(defparameter *entities*
  '(("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\"")
    ("apos" . "'") ("nbsp" . " ") ("#39" . "'"))
  "The entities a page actually uses, read back so a word is a word.")

(defparameter *max-chunks-per-file* 400
  "A bounded share of one file: a 400-page manual must not swamp a search.")

(defun file-extension (path)
  "PATH's extension, downcased, or the empty string."
  (string-downcase (or (pathname-type path) "")))

(defun text-file-p (path)
  (and (member (file-extension path) *text-extensions* :test #'equal) t))

(defun office-file-p (path)
  (and (member (file-extension path) *office-extensions* :test #'equal) t))

(defun latin-1-text (path limit)
  "PATH's bytes as text, one byte one character, up to LIMIT."
  (handler-case
      (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
        (let* ((size (min (or (ignore-errors (file-length in)) 0) limit))
               (octets (make-array size :element-type '(unsigned-byte 8)))
               (got (read-sequence octets in)))
          (map 'string #'code-char (subseq octets 0 got))))
    (error () "")))

(defun run-capture (program arguments)
  "PROGRAM with ARGUMENTS, its standard output as a string; NIL when the
operator has no such program, or it fails."
  (ignore-errors
   (uiop:run-program (cons program arguments)
                      :output :string :error-output nil :external-format :utf-8)))

(defun decode-entities (text)
  "TEXT with &amp; and its kin read back, one pass, unknown entities left be."
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length text))
          do (if (char= (char text i) #\&)
                 (let ((end (position #\; text :start i
                                            :end (min (length text) (+ i 12)))))
                   (uiop:if-let (replacement (and end
                                                  (cdr (assoc (subseq text (1+ i) end)
                                                              *entities* :test #'equal))))
                     (progn (write-string replacement out) (setf i (1+ end)))
                     (progn (write-char #\& out) (incf i))))
                 (progn (write-char (char text i) out) (incf i))))))

(defun strip-tags (text)
  "TEXT with every <...> replaced by a space, so a page's words are what an
index sees."
  (with-output-to-string (out)
    (loop with inside = nil
          for character across text
          do (cond ((char= character #\<) (setf inside t) (write-char #\Space out))
                   ((char= character #\>) (setf inside nil))
                   (inside)
                   (t (write-char character out))))))

(defun clean-text (text)
  "One normal form: no run of blanks, at most one blank line between blocks."
  (with-output-to-string (out)
    (loop with blank = 0
          with blanked = t
          for character across text
          do (cond ((member character '(#\Space #\Tab #\Return) :test #'char=)
                    (unless blanked (write-char #\Space out)) (setf blanked t))
                   ((char= character #\Newline)
                    (when (< blank 2) (write-char #\Newline out))
                    (incf blank) (setf blanked t))
                   (t (write-char character out) (setf blank 0 blanked nil))))))

(defun extract-text (path max-bytes)
  "The text PATH holds => (values TEXT NOTE). NOTE says why TEXT is empty."
  (let ((size (ignore-errors (with-open-file (in path :element-type '(unsigned-byte 8))
                                (file-length in)))))
    (cond
      ((and size (zerop size)) (values "" "empty file"))
      ((and size (> size max-bytes))
       (values "" (format nil "larger than ~d MB" (floor max-bytes (* 1024 1024)))))
      ((text-file-p path)
       (let ((text (or (nlk:read-text path max-bytes) (latin-1-text path max-bytes))))
         (if (zerop (length text))
             (values "" "unreadable: no text")
             (values (clean-text
                      (if (member (file-extension path) '("html" "htm" "xml") :test #'equal)
                          (decode-entities (strip-tags text))
                          text))
                     ""))))
      ((equal (file-extension path) "pdf")
       (let ((text (run-capture "pdftotext" (list "-l" "200" "-layout"
                                                    (uiop:native-namestring path) "-"))))
         (if (or (null text) (zerop (length (nlk:trimmed text))))
             (values "" "pdf: no text, or pdftotext is not installed")
             (values (clean-text text) ""))))
      ((office-file-p path)
       (let* ((member (cdr (assoc (file-extension path) *office-members* :test #'equal)))
              (xml (and member (run-capture "unzip" (list "-p" (uiop:native-namestring path)
                                                            member)))))
         (if (or (null xml) (zerop (length (nlk:trimmed xml))))
             (values "" (format nil "~a: no text, or unzip is not installed"
                                 (file-extension path)))
             (values (clean-text (decode-entities (strip-tags xml))) ""))))
      (t (values "" (format nil "unsupported: ~a" (file-extension path)))))))

(defun chunk-text (text size limit)
  "TEXT cut into pieces of at most SIZE characters, at a blank or a line end
where one sits near the cut, at most LIMIT of them."
  (let ((size (max 200 size))
        (chunks '())
        (start 0)
        (length (length text)))
    (loop while (and (< start length) (< (length chunks) limit))
          do (let* ((end (min length (+ start size)))
                    (cut (if (>= end length)
                             end
                             (or (position-if (lambda (character)
                                                (member character '(#\Space #\Newline #\Tab)
                                                        :test #'char=))
                                              text :start (max start (- end 200)) :end end
                                              :from-end t)
                                 end))))
               (let ((piece (nlk:trimmed (subseq text start (max cut (1+ start))))))
                 (when (plusp (length piece)) (push piece chunks)))
               (setf start (max (1+ cut) (1+ start)))))
    (nreverse chunks)))
