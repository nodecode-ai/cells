;;;; cell-test.lisp --- the lifecycle, the search itself, and the corpus it refuses without.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over real files in a
;;;; temporary folder: that a start installs the tool, the command and the
;;;; manual and a stop takes them back; that a search answers the passage a
;;;; heading chain leads to, ranks a section title above a mention in a body,
;;;; keeps to one corpus root, quotes an excerpt around the match and never
;;;; walks a dot-directory; that a file written after the index was built is
;;;; found after a reindex; and that a cell with no roots names the setting it
;;;; needs instead of answering nothing. There is no network and no service:
;;;; the corpus is a folder of markdown this file writes.

(in-package #:nodecode.test)

;;; --- a corpus ---------------------------------------------------------------------

(defun prose (&rest lines)
  "LINES as one text, a newline between them."
  (format nil "~{~a~^~%~}" lines))

(defvar *corpora* 0 "How many temporary corpora this run has written.")

(defun write-corpus-into (root files)
  "Write FILES, (NAME . TEXT) pairs, under ROOT. => ROOT."
  (dolist (file files root)
    (let ((path (merge-pathnames (car file) root)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                :if-does-not-exist :create :external-format :utf-8)
        (write-string (cdr file) out)))))

(defun write-corpus (files)
  "A temporary folder holding FILES, (NAME . TEXT) pairs with NAME relative.
=> its pathname, directory form."
  (write-corpus-into (uiop:ensure-directory-pathname
                      (format nil "~amd-search-~a-~a/" (uiop:temporary-directory)
                              (get-universal-time) (incf *corpora*)))
                     files))

(defparameter *guide*
  (prose "# Guide"
         ""
         "The gateway is one process on the machine."
         ""
         "## Install"
         ""
         "Install the gateway from the package."
         ""
         "### Linux"
         ""
         "On Linux, run the installer with the package manager.")
  "A file whose heading chain an answer has to quote.")

(defun md-search-call (&rest pairs)
  "Run the registered md_search tool with PAIRS, as dispatch calls it."
  (let ((entry (nle::find-tool "md_search")))
    (assert entry () "no tool named md_search")
    (funcall (third entry)
             (nle::tool-body-arguments entry (apply #'nlk:make-json-object pairs)))))

(defun on-md-search (roots &rest config)
  "A start over ROOTS as the corpus, then CONFIG's pairs."
  (apply #'md-search-start "roots" roots config))

(defmacro md-search-carries ((&rest pairs) &body checks)
  "Assert the text md_search answers for PAIRS carries every CHECK."
  `(is-carrying (answer (md-search-call ,@pairs)) ,@checks))

(defmacro command-carries ((&rest args) &body checks)
  "Assert the text /md-search answers for ARGS carries every CHECK."
  `(is-carrying (answer (cell-entry "nodecode-md-search" "md-search" ,@args)) ,@checks))

(define-test-slice "md-search" "MD-SEARCH-CELL-" :start nodecode-md-search:start-cell)

;;; --- lifecycle --------------------------------------------------------------------

(defparameter *lifecycle-corpus*
  (write-corpus (list (cons "guide.md"
                            (prose "# Guide" "" "The gateway starts on loopback."))))
  "One corpus the lifecycle tests start and stop over.")

(define-cell-lifecycle-tests "md-search"
  (:config "roots" (vector (namestring *lifecycle-corpus*)))
  (:help :md-search)
  (:command "md-search")
  (:running (is (nle::find-tool "md_search") "md_search is registered")
            (is (= 5 (nodecode-md-search::setting :max-results)) "five passages by default")
            (is (= 300 (nodecode-md-search::setting :refresh-seconds)))
            (is (member ".git" (nodecode-md-search::setting :skip) :test #'equal) "a version-control directory is never walked"))
  (:stopped (is (null (nle::find-tool "md_search")) "stop unregisters the tool"))
  (:refused ("max_results" 0) ("snippet_chars" 10))
  (:idle nodecode-md-search:md-search-error (nodecode-md-search:md-search "anything")))

;;; --- the search -------------------------------------------------------------------

(deftest md-search-cell-answers-the-passage-a-heading-chain-leads-to ()
  (let ((root (write-corpus (list (cons "guide.md" *guide*)
                                  (cons "notes/coffee.md"
                                        (prose "# Coffee" "" "groceries, then the market."))))))
    (with-cell-stop ((on-md-search (vector (namestring root))))
      (let ((answer (md-search-call "query" "installer")))
        (is-carrying answer
                     ("md_search (lexical, 1 result" "a count and the tier")
                     ("guide.md  [Guide > Install > Linux]" "the heading chain names where it sits")
                     ("run the installer" "the excerpt quotes the match")
                     (:absent "coffee" "only the file that matched"))))))

(deftest md-search-cell-ranks-a-section-title-above-a-mention-in-a-body ()
  (let ((root (write-corpus (list (cons "beacon.md" (prose "# Beacon" "" "nothing else is written here."))
                                  (cons "notes.md" (prose "# Notes" "" "we keep a beacon near the door."))))))
    (with-cell-stop ((on-md-search (vector (namestring root))))
      (let ((answer (md-search-call "query" "beacon")))
        (is (< (or (search "beacon.md" answer) 0) (or (search "notes.md" answer) most-positive-fixnum)) "the heading match comes first")))))

(deftest md-search-cell-keeps-to-one-corpus-root ()
  (let ((root (write-corpus (list (cons "alpha/one.md" (prose "# One" "" "the lantern is here."))
                                  (cons "beta/two.md" (prose "# Two" "" "the lantern is there."))))))
    (with-cell-stop ((on-md-search (vector (format nil "alpha=~a/alpha" (namestring root))
                                           (format nil "beta=~a/beta" (namestring root)))))
      (let ((answer (md-search-call "query" "lantern" "root" "beta")))
        (is-carrying answer ("beta/two.md" "the named root") (:absent "alpha/one.md" "the root left out"))))))

(deftest md-search-cell-never-walks-a-dot-directory ()
  (let ((root (write-corpus (list (cons "note.md" (prose "# Note" "" "the lantern is here."))
                                  (cons ".hidden/lost.md" (prose "# Lost" "" "the lantern is under a dot."))))))
    (with-cell-stop ((on-md-search (vector (namestring root))))
      (let ((answer (md-search-call "query" "lantern")))
        (is-carrying answer ("note.md" "the note is indexed") (:absent "lost.md" "the dot-directory is not"))))))

(deftest md-search-cell-says-what-a-corpus-holds-when-nothing-matches ()
  (let ((root (write-corpus (list (cons "note.md" (prose "# Note" "" "the lantern is here."))))))
    (with-cell-stop ((on-md-search (vector (namestring root))))
      (let ((answer (md-search-call "query" "zeppelin")))
        (is-carrying answer ("no markdown matches" "the refusal is the answer")
                     ("1 file" "and the corpus it looked at"))))))

(deftest md-search-cell-finds-a-file-written-after-the-index-was-built ()
  (let ((root (write-corpus (list (cons "note.md" (prose "# Note" "" "the lantern is here."))))))
    (with-cell-stop ((on-md-search (vector (namestring root))))
      (md-search-carries ("query" "foghorn") ("no markdown matches"))
      (write-corpus-into root (list (cons "later.md" (prose "# Later" "" "the foghorn sounds at noon."))))
      (command-carries ("reindex")
                   ("reindexed 2 files" "the reindex sees the new file"))
      (md-search-carries ("query" "foghorn") ("later.md" "and the search finds it")))))

;;; --- /md-search -------------------------------------------------------------------

(deftest md-search-cell-the-command-searches-and-reports ()
  (let ((root (write-corpus (list (cons "guide.md" *guide*)))))
    (with-cell-stop ((on-md-search (vector (namestring root))))
      (command-carries ("")
                   ("1 root" "the status names the corpus") ("no index built" "and the index"))
      (command-carries ("installer")
                   ("Guide > Install > Linux" "words typed after the command search"))
      (command-carries ("reindex")
                   ("reindexed 1 file into" "reindex answers its counts")))))

;;; --- no roots ---------------------------------------------------------------------

(deftest md-search-cell-names-the-setting-it-needs-without-roots ()
  (with-cell-stop ((md-search-start))
    (md-search-carries ("query" "anything")
                 ("no markdown roots are set" "the refusal says what is missing")
                 ("roots" "and names the setting"))))

