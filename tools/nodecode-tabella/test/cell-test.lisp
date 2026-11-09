;;;; cell-test.lisp --- the tabella cell against stubbed programs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every program is the stubbed *RUN-ARGV* -- a list of the argv it was
;;;; handed, and a file written where an argv names one -- and every lookup
;;;; the stubbed *PROGRAM-PRESENT-P*: nothing starts pandoc, reads PATH or
;;;; touches a folder outside the scratch one. Each tool is called the way a
;;;; model's call reaches it, through the real dispatch, contract and all, so
;;;; the schema the provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "tabella" "TABELLA-CELL-" :start nodecode-tabella:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defun tabella-folder (&optional (name "tabella"))
  "A folder of this test's own, absent before it is made. => its namestring."
  (let ((dir (merge-pathnames (format nil "~a-~d-~d/" name (get-universal-time) (random 1000000))
                               (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    (namestring dir)))

(defun tabella-file (folder name &optional (text ""))
  "NAME inside FOLDER, holding TEXT."
  (let ((path (merge-pathnames name (uiop:ensure-directory-pathname folder))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string text out))
    (namestring path)))

(defparameter *tabella-programs* '(("pandoc" . "pandoc") ("pdftotext" . "pdftotext")
                                   ("xelatex" . "xelatex"))
  "What the stubbed lookup finds: the programs a healthy machine has.")

(defmacro with-tabella-world ((calls &key (programs '*tabella-programs*) (text "stub text"))
                              &body body)
  "BODY with the cell's two seams stubbed: CALLS collects the argv of every
program run, oldest first; a run whose argv names -o writes TEXT there and
answers 0, a run without one answers TEXT as its stdout; PROGRAMS is what a
lookup finds."
  `(let* ((,calls '())
         (programs ,programs)
         (nodecode-tabella:*run-argv*
           (lambda (argv)
             (push argv ,calls)
             (let ((out (second (member "-o" argv :test #'string=))))
               (when out
                 (with-open-file (o out :direction :output :if-exists :supersede
                                        :if-does-not-exist :create :external-format :utf-8)
                   (write-string ,text o))))
             (values ,text "" 0)))
         (nodecode-tabella:*program-present-p*
           (lambda (name) (cdr (assoc name programs :test #'string=)))))
     ,@body))

(defmacro with-tabella-cell ((folder &rest pairs) &body body)
  "BODY with the cell started on FOLDER as its output_dir and PAIRS beside it,
stopped on unwind."
  `(with-cell-stop ((tabella-start "output_dir" ,folder ,@pairs))
     ,@body))

(defun tabella-answer (call-id tool arguments)
  "TOOL's answer to ARGUMENTS as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call call-id tool arguments)))

(defun tabella-json (call-id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (tabella-answer call-id tool arguments)))

(defun argument-after (argv flag)
  "ARGV's element after FLAG, or NIL."
  (second (member flag argv :test #'string=)))

(defun program-run (calls name)
  "The argv in CALLS whose program is NAME, oldest first."
  (find-if (lambda (argv) (and argv (search name (first argv)))) calls))

;;; --- the lifecycle, from the one declaration --------------------------------------

(define-cell-lifecycle-tests "tabella"
  (:config "output_dir" (tabella-folder))
  (:help :tabella)
  (:command "tabella")
  (:refused ("max_chars" 0)))

(deftest tabella-cell-refuses-a-section-with-no-output-dir ()
  ;; output_dir is the one member every write stands on: a section that never
  ;; answered it is refused by name at start, not at the first document.
  (is (signals-error nodecode-tabella:tabella-error (tabella-start "enabled" t))))

(deftest tabella-cell-refuses-a-document-outside-its-output-dir-guard ()
  ;; A category that climbs out of output_dir is refused before anything runs.
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let ((answer (tabella-answer "c1" "doc_convert"
                                       (format nil "{\"input_path\":~s,\"to_format\":\"html\",\"category\":\"../out\"}"
                                               (tabella-file folder "a.md" "hello")))))
          (is (search "path out of output_dir" answer))
          (is (null calls) "nothing ran"))))))

;;; --- doc_read ---------------------------------------------------------------------

(deftest tabella-cell-reads-plain-text-itself ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let* ((path (tabella-file folder "notes.md" "# Title\n\nbody"))
               (answer (tabella-json "c1" "doc_read" (format nil "{\"path\":~s}" path))))
          (is (equal "# Title\n\nbody" (gethash "text" answer)))
          (is (= 13 (gethash "length" answer)))
          (is (equal path (gethash "path" answer)))
          (is (null calls) "no program ran for a text file"))))))

(deftest tabella-cell-reads-a-docx-through-pandoc ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls :text "## from pandoc")
      (with-tabella-cell (folder)
        (let* ((path (tabella-file folder "contract.docx" "stub"))
               (argv (progn (tabella-answer "c1" "doc_read" (format nil "{\"path\":~s}" path))
                            (program-run calls "pandoc"))))
          (is argv "pandoc ran")
          (when argv
            (is (equal path (second argv)) "on the file read")
            (is (equal "gfm" (argument-after argv "--to")) "as Markdown")))))))

(deftest tabella-cell-reads-a-pdf-through-pdftotext ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let* ((path (tabella-file folder "report.pdf" "%PDF-1.4"))
               (answer (tabella-json "c1" "doc_read" (format nil "{\"path\":~s}" path)))
               (argv (program-run calls "pdftotext")))
          (is (equal "stub text" (gethash "text" answer)))
          (is (equal "UTF-8" (argument-after argv "-enc")))
          (is (equal "-" (car (last argv))) "pdftotext writes to stdout"))))))

(deftest tabella-cell-caps-the-text-it-answers ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let* ((path (tabella-file folder "long.md" (make-string 500 :initial-element #\x)))
               (answer (tabella-json "c1" "doc_read"
                                      (format nil "{\"path\":~s,\"max_chars\":10}" path))))
          (is (= 10 (gethash "length" answer))))))))

(deftest tabella-cell-reads-a-document-metadata-off-the-filesystem ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let* ((path (tabella-file folder "row.xlsx" "12345"))
               (answer (tabella-json "c1" "doc_read_meta" (format nil "{\"path\":~s}" path)))
               (meta (gethash "metadata" answer)))
          (is (equal "xlsx" (gethash "format" meta)))
          (is (= 5 (gethash "size_bytes" meta)))
          (is (integerp (gethash "mtime" meta)))
          (is (null calls) "no program runs for metadata"))))))

;;; --- doc_convert ------------------------------------------------------------------

(deftest tabella-cell-converts-markdown-to-a-pdf-with-a-latex-engine ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let* ((path (tabella-file folder "report.md" "# Report"))
               (answer (tabella-json "c1" "doc_convert"
                                      (format nil "{\"input_path\":~s,\"to_format\":\"pdf\"}" path)))
               (out (gethash "output_path" answer))
               (argv (program-run calls "pandoc")))
          (is (gethash "success" answer))
          (is (equal "pdf" (gethash "to_format" answer)))
          (is (equal "xelatex" (gethash "pdf_engine" answer)))
          (is (uiop:string-prefix-p folder out) "written into output_dir")
          (is (equal ".pdf" (subseq out (- (length out) 4))))
          (is (equal "xelatex" (argument-after argv "--pdf-engine")))
          (is (equal "pdf" (argument-after argv "-t")))
          (is (equal "markdown" (argument-after argv "-f"))))))))

(deftest tabella-cell-refuses-a-format-name-that-is-a-lua-reader ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let* ((path (tabella-file folder "a.md" "# a"))
               (answer (tabella-answer "c1" "doc_convert"
                                        (format nil "{\"input_path\":~s,\"to_format\":\"evil.lua\"}" path))))
          (is (search "unsupported to_format" answer))
          (is (null calls) "pandoc never ran"))))))

(deftest tabella-cell-refuses-a-pandoc-flag-that-runs-code ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let* ((path (tabella-file folder "a.md" "# a"))
               (answer (tabella-answer "c1" "doc_convert"
                                        (format nil "{\"input_path\":~s,\"to_format\":\"html\",\"extra_args\":[\"--lua-filter=evil.lua\"]}"
                                                path))))
          (is (search "--lua-filter" answer))
          (is (null calls) "pandoc never ran"))))))

(deftest tabella-cell-passes-the-allowlisted-flags-through ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let ((path (tabella-file folder "a.md" "# a")))
          (tabella-answer "c1" "doc_convert"
                           (format nil "{\"input_path\":~s,\"to_format\":\"docx\",\"extra_args\":[\"--toc\",\"-Vgeometry=margin=1in\"]}"
                                   path))
          (let ((argv (program-run calls "pandoc")))
            (is (member "--toc" argv :test #'string=))
            (is (member "-Vgeometry=margin=1in" argv :test #'string=))))))))

;;; --- doc_create_document ----------------------------------------------------------

(deftest tabella-cell-creates-a-document-from-blocks ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let* ((out (namestring (merge-pathnames "minutes.docx" (uiop:ensure-directory-pathname folder))))
               (answer (tabella-json
                        "c1" "doc_create_document"
                        (format nil "{\"format\":\"docx\",\"title\":\"Minutes\",\"output_path\":~s,\"content\":[{\"type\":\"heading\",\"level\":2,\"text\":\"Decisions\"},{\"type\":\"bullets\",\"items\":[\"ship it\",\"tell nobody\"]}]}"
                                out)))
               (argv (program-run calls "pandoc")))
          (is (gethash "success" answer))
          (is (equal out (gethash "output_path" answer)))
          (is (equal "markdown" (argument-after argv "-f")))
          (is (equal "docx" (argument-after argv "-t")))
          (is (probe-file out) "the document is where the answer says"))))))

(deftest tabella-cell-refuses-a-spreadsheet ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let ((answer (tabella-answer "c1" "doc_create_document"
                                       "{\"format\":\"xlsx\",\"content\":\"a,b\"}")))
          (is (search "not a format this cell writes" answer))
          (is (null calls) "nothing ran"))))))

;;; --- /tabella ---------------------------------------------------------------------

(deftest tabella-cell-command-says-where-documents-go ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let ((text (cell-entry "nodecode-tabella" "tabella" nil)))
          (is (search folder text) "the output_dir is named")
          (is (search "pandoc" text)))))))

(deftest tabella-cell-command-reads-a-document ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let ((path (tabella-file folder "note.md" "meeting at noon")))
          (is (search "meeting at noon" (cell-entry "nodecode-tabella" "tabella"
                                                    (format nil "read ~a" path))))
          (is (null calls) "a text file needs no program"))))))

(deftest tabella-cell-command-converts-a-document ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let ((path (tabella-file folder "note.md" "# note")))
          (is (search "Converted note.md -> html"
                      (cell-entry "nodecode-tabella" "tabella" (format nil "convert ~a html" path)))))))))

(deftest tabella-cell-command-refuses-an-unknown-verb ()
  (let ((folder (tabella-folder)))
    (with-tabella-world (calls)
      (with-tabella-cell (folder)
        (let ((answer (handler-case (cell-entry "nodecode-tabella" "tabella" "frobnicate")
                        (nodecode-tabella:tabella-error (condition)
                          (nodecode-tabella:tabella-error-detail condition)))))
          (is (search "no tabella verb" answer)))))))
