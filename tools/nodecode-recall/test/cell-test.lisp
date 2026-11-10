;;;; cell-test.lisp --- the recall cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every file is written in a scratch folder under the run's own home,
;;;; every index a fresh one, and no test reaches the network or a program
;;;; the run does not ship: the extraction tests read what this process can
;;;; read, and a PDF or an Office file is a file name and a note.

(in-package #:nodecode.test)

(define-test-slice "recall" "RECALL-CELL-" :start nodecode-recall:start-cell)

(define-cell-lifecycle-tests "recall"
  (:config "refresh_on_start" nil)
  (:hooks 'nle::read-harness-sections)
  (:help :recall)
  (:command "recall")
  (:refused ("max_hints" 0) ("chunk_chars" 5) ("max_files" "many"))
  (:idle recall:recall-error (recall:search "x")))

;;; --- fixtures -------------------------------------------------------------

(defvar *recall-test-folders* 0
  "How many scratch folders this run has made; each is its own.")

(defun recall-scratch-directory ()
  "A fresh folder outside the organism's home -- the policy refuses that home,
which is the point of the opens-and-windows test."
  (let ((directory (merge-pathnames
                     (format nil "nl-recall-~d-~d/" (get-universal-time)
                             (incf *recall-test-folders*))
                     (uiop:temporary-directory))))
    (ensure-directories-exist directory)
    directory))

(defun recall-file (directory name text)
  "NAME written under DIRECTORY with TEXT => its path."
  (let ((path (merge-pathnames name (uiop:ensure-directory-pathname directory))))
    (ensure-directories-exist path)
    (nlk:with-output-file (out path) (write-string text out))
    path))

(defmacro with-recall ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS and an empty index."
  `(with-cell-stop ((recall-start ,@pairs "refresh_on_start" nil))
     (recall:forget-files :all t)
     ,@body))

;;; --- the tools ------------------------------------------------------------

(deftest recall-cell-indexes-and-finds ()
  (let* ((directory (recall-scratch-directory))
         (invoice (recall-file directory "acme-2026-03.md"
                                (concatenate 'string "# Acme invoice\n\nThe powder coating delivery is late; "
                                             "see the penalty clause in section 4.\n")))
         (garden (recall-file directory "garden.md" "Tomatoes and basil, two beds, watered daily.\n")))
    (declare (ignore invoice garden))
    (with-recall ("roots" (vector (uiop:native-namestring directory)))
      (let ((summary (recall:index-files)))
        (is (= 2 (gethash "indexed" summary)) "both files are indexed")
        (is (= 2 (gethash "files" summary)) "and the summary counts them"))
      (let* ((results (recall:search "powder coating delivery"))
             (best (first results)))
        (is (plusp (length results)) "the invoice is found")
        (is (cl:search "acme-2026-03.md" (getf best :path)) "best first, and it is the invoice")
        (is (cl:search "[powder]" (getf best :snippet)) "the matched word is bracketed")
        (is (plusp (length (getf best :modified))) "and the file's date rides along"))
      (is (null (recall:search "trombone")) "a word no file carries finds nothing"))))

(deftest recall-cell-opens-and-windows ()
  (let* ((directory (recall-scratch-directory))
         (notes (recall-file directory "notes.txt"
                              (format nil "line one is here. ~{~a ~}~%"
                                      (loop for i below 200 collect "more words follow"))))
         (secret (recall-file directory ".ssh/id_rsa" "PRIVATE KEY MATERIAL")))
    (with-recall ()
      (let ((document (recall:open-document (uiop:native-namestring notes))))
        (is (cl:search "line one" (gethash "text" document)) "the text is returned")
        (is (plusp (gethash "chars_total" document)) "with its whole length"))
      (let ((window (recall:open-document (uiop:native-namestring notes) :offset 5 :max-chars 500)))
        (is (= 500 (gethash "returned" window)) "a window is exactly as wide as asked")
        (is (gethash "truncated" window) "and says there is more")
        (is (= 5 (gethash "offset" window)) "from where the caller asked"))
      (is (signals-error recall:recall-error (recall:open-document secret)) "a secret is refused"))))

(deftest recall-cell-forgets-and-counts ()
  (let* ((directory (recall-scratch-directory))
         (invoice (recall-file directory "invoice.md" "Acme invoice for powder coating.\n"))
         (garden (recall-file directory "garden.md" "Tomatoes and basil.\n")))
    (declare (ignore garden))
    (with-recall ("roots" (vector (uiop:native-namestring directory)))
      (recall:index-files)
      (let ((counted (gethash "files" (recall:stats-object))))
        (is (= 2 counted) (format nil "two files indexed, got ~a" counted)))
      (let ((dropped (recall:forget-files :path (uiop:native-namestring invoice))))
        (is (= 1 (gethash "files_removed" dropped)) "forget drops the one path")
        (is (= 1 (gethash "files_remaining" dropped)) "and says what is left"))
      (is (= 1 (gethash "files" (recall:stats-object))) "the index really shrank")
      (is (gethash "cleared" (recall:forget-files :all t)) "all clears what is left")
      (is (zerop (gethash "files" (recall:stats-object))) "to nothing"))))

(deftest recall-cell-hints-at-a-document-question ()
  (let ((directory (recall-scratch-directory)))
    (recall-file directory "acme-2026-03.md"
                  "Acme invoice for powder coating, March 2026, delivery late.\n")
    (with-recall ("roots" (vector (uiop:native-namestring directory)))
      (recall:index-files)
      (let ((block (recall:ambient-block "where is the Acme invoice from March"
                                          :session-id "recall-hint-one")))
        (is (cl:search "[recall]" block) "the block says who it is")
        (is (cl:search "acme-2026-03.md" block) "and names the file")
        (is (cl:search "recall_open" block) "and tells the model to read it first"))
      (is (equal "" (recall:ambient-block "where is the Acme invoice from March" :session-id "recall-hint-one")) "a session is not shown the same file twice")
      (is (equal "" (recall:ambient-block "ok, thanks, carry on" :session-id "recall-hint-two")) "a message about no document says nothing"))))

(deftest recall-cell-extracts-and-chunks ()
  (let* ((directory (recall-scratch-directory))
         (page (recall-file directory "page.html"
                             (concatenate 'string "<html><body><h1>Delivery</h1>"
                                          "<p>Powder coating &amp; steel</p></body></html>\n"))))
    (multiple-value-bind (text note) (nodecode-recall::extract-text page (* 1024 1024))
      (is (cl:search "Delivery" text) "the page's words are kept")
      (is (null (cl:search "<h1>" text)) "its tags are not")
      (is (cl:search "Powder coating & steel" text) "and the entity reads back as the character")
      (is (equal "" note) "with no skip reason")))
  (let* ((long (format nil "~{~a ~}" (loop for i below 500 collect "word")))
         (chunks (nodecode-recall::chunk-text long 200 400)))
    (is (> (length chunks) 1) "long text is cut")
    (is (every (lambda (chunk) (<= (length chunk) 200)) chunks) "and no chunk exceeds the size")))
