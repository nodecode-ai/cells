;;;; cell-test.lisp --- lifecycle, the five tools, /mediarag, a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a corpus in a scratch
;;;; folder: that a start installs the five tools, /mediarag and the manual and
;;;; a stop takes every one of them back; that a missing corpus_root refuses;
;;;; that a note, a folder of files and a fetched URL each become rows a search
;;;; answers; and that a WAV file's rows carry the time windows the original's
;;;; audio lane gave them.

(in-package #:nodecode.test)

(define-test-slice "mediarag" "MEDIARAG-CELL-" :start nodecode-mediarag:start-cell)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "mediarag"
  (:help :mediarag)
  (:command "mediarag")
  (:config "corpus_root" (namestring (uiop:temporary-directory)) "max_chunk_chars" 400)
  (:running (is (equal nodecode-mediarag::+primer+ (nle:help :mediarag)) "(help :mediarag) is the primer")
            (is (nle::find-tool "mediarag_search") "the search tool is registered")
            (is (nle::find-tool "mediarag_add_note") "the note tool is registered")
            (is (nle::find-tool "mediarag_ingest_dir") "the ingest tool is registered")
            (is (nle::find-tool "mediarag_add_source") "the source tool is registered")
            (is (nle::find-tool "mediarag_status") "the status tool is registered")
            (is (= 400 (nodecode-mediarag::setting :max-chunk-chars)) "the section's chunk size"))
  (:refused ("corpus_root" 5) ("max_chunk_chars" 0))
  (:stopped (is (null (nle::find-tool "mediarag_search")) "stop unregisters the tools")
            (is (null (nle::find-tool "mediarag_status")))))

;;; --- one scratch corpus ------------------------------------------------------

(defmacro with-mediarag (&body body)
  "BODY with the cell started on a corpus folder of its own, removed after."
  (let ((root (gensym "ROOT")))
    `(let ((,root (format nil "~amediarag-~a/" (namestring (uiop:temporary-directory))
                          (symbol-name (gensym "cell")))))
       (ensure-directories-exist (uiop:ensure-directory-pathname ,root))
       (unwind-protect
            (with-cell-stop ((mediarag-start "corpus_root" ,root "max_chunk_chars" 400))
              ,@body)
         (ignore-errors
          (uiop:delete-directory-tree (uiop:ensure-directory-pathname ,root) :validate t))))))

;;; --- the tools ------------------------------------------------------------------

(deftest mediarag-cell-a-missing-corpus-root-refuses ()
  (is (signals-error nlk:config-refusal (nodecode-mediarag:start-cell nil)) "an unanswered section refuses and names corpus_root"))

(deftest mediarag-cell-a-note-is-indexed-and-found-by-the-command ()
  (with-mediarag
    (is (search "noted 1 row" (nodecode-mediarag:add-note
                               "The landing page moved to the docs folder.")) "the note tool answers what it stored")
    (let ((answer (nodecode-mediarag:search-text "landing page")))
      (is (search "note://" answer) "the search tool finds the note row")
      (is (search "landing page" (string-downcase answer)) "with the sentence in it"))
    (is (search "1 rows over 1 sources" (cell-entry "nodecode-mediarag" "mediarag" "")) "/mediarag reports the corpus with no argument")
    (is (search "landing page" (cell-entry "nodecode-mediarag" "mediarag" "landing")) "/mediarag searches with an argument")))

(defun wav-octets (seconds)
  "A 16 kHz mono 16-bit WAVE of SECONDS of silence, as octets."
  (let* ((rate 16000) (data (* rate seconds 2))
         (octets (make-array (+ 44 data) :element-type '(unsigned-byte 8) :initial-element 0)))
    (labels ((text (at value)
               (loop for index below 4
                     do (setf (aref octets (+ at index)) (char-code (char value index)))))
             (word (at value)
               (loop for index below 4
                     do (setf (aref octets (+ at index)) (ldb (byte 8 (* 8 index)) value)))))
      (text 0 "RIFF") (word 4 (+ 36 data)) (text 8 "WAVE")
      (text 12 "fmt ") (word 16 16) (word 20 1) (word 22 1)
      (word 24 rate) (word 28 (* rate 2)) (word 32 2) (word 34 16)
      (text 36 "data") (word 40 data))
    octets))

(deftest mediarag-cell-a-folder-becomes-searchable-rows ()
  (with-mediarag
    (let* ((folder (merge-pathnames "docs/" (nodecode-mediarag::corpus-root)))
           (notes (merge-pathnames "notes.md" folder))
           (sound (merge-pathnames "audiobook.wav" folder)))
      (ensure-directories-exist notes)
      (with-open-file (out notes :direction :output :if-exists :supersede)
        (write-string "The release notes say the harvest starts in September." out))
      (with-open-file (out sound :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
        (write-sequence (wav-octets 30) out))
      (is (search "indexed 2 of 2" (nodecode-mediarag:ingest-dir (namestring folder))) "the ingest tool reports both files")
      (let ((answer (nodecode-mediarag:search-text "harvest september")))
        (is (search "notes.md" answer) "the document is found by its words"))
      (let ((answer (nodecode-mediarag:search-text "audiobook" :kind "audio")))
        (is (search "[11.2-22.4s]" answer) "the wav is cut into the original's 11.2s windows"))
      (let ((answer (nodecode-mediarag:search-text "anything" :kind "image")))
        (is (search "no rows of kind image" answer) "a kind with no rows answers plainly")))))

(deftest mediarag-cell-a-url-source-is-fetched-through-the-stubbed-wire ()
  (with-mediarag
    (let ((original (symbol-function 'nodecode-mediarag::http-call)))
      (unwind-protect
           (progn
             (setf (symbol-function 'nodecode-mediarag::http-call)
                   (lambda (method url &rest arguments)
                     (declare (ignore method url arguments))
                     (values "# Trip notes\n\nThe ferry leaves the harbour at six." 200)))
             (is (search "fetched https://example.test/trip.md"
                         (nodecode-mediarag:add-source "https://example.test/trip.md")) "the URL is fetched into the cache and indexed")
             (is (search "ferry" (nodecode-mediarag:search-text "ferry")) "its words are searchable")
             (is (search "no rows of kind image" (nodecode-mediarag:search-text "ferry" :kind "image")) "and it landed as a text row"))
        (setf (symbol-function 'nodecode-mediarag::http-call) original)))))
