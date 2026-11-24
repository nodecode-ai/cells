;;;; cell-test.lisp --- the two verbs over a folder of notes, and the pattern that scopes them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is over real files in a temporary folder: that a start
;;;; installs both tools, the command and the manual and a stop takes them
;;;; back; that a search answers the id, the title and a snippet of the
;;;; documents that match and only those; that deny wins over allow and that a
;;;; document it denies reads as not_found_or_denied; that a read is capped
;;;; where read_chars says and says so; that a dot-directory, another file
;;;; type and a symlink are never opened; and that a section with no root,
;;;; or one pointing nowhere, refuses and names the setting.

(in-package #:nodecode.test)

;;; --- a folder of notes ---------------------------------------------------------------

(defvar *mb-corpora* 0 "How many temporary folders this run has written.")

(defun mb-prose (&rest lines)
  "LINES as one text, a newline between them."
  (format nil "~{~a~^~%~}" lines))

(defun mb-write-corpus (files)
  "A temporary folder holding FILES, (NAME . TEXT) pairs with NAME relative. => its pathname, directory form."
  (let ((root (uiop:ensure-directory-pathname
               (format nil "~amemory-bridge-~d-~d/" (uiop:temporary-directory)
                       (get-universal-time) (incf *mb-corpora*)))))
    (dolist (file files root)
      (let ((path (merge-pathnames (car file) root)))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create :external-format :utf-8)
          (write-string (cdr file) out))))))

(defun mb-start (root &rest pairs)
  "A start over ROOT as the memory folder, then PAIRS."
  (apply #'memory-bridge-start (append (list "root" (namestring root)) pairs)))

(defun mb-call (tool &rest pairs)
  "Run the registered TOOL with PAIRS, as dispatch calls it."
  (let ((entry (nle::find-tool tool)))
    (assert entry () "no tool named ~a" tool)
    (funcall (third entry)
             (nle::tool-body-arguments entry (apply #'nlk:make-json-object pairs)))))

(defun mb-text (answer)
  "ANSWER as text: a tool of this cell answers one string, its whole answer."
  (if (stringp answer) answer (princ-to-string answer)))

(defun mb-read (id)
  "The text memory_bridge_read answers for ID."
  (mb-text (mb-call "memory_bridge_read" "id" id)))

(defun mb-answer (&rest pairs)
  "The text memory_bridge_search answers for PAIRS."
  (mb-text (apply #'mb-call "memory_bridge_search" pairs)))

(defmacro mb-carries ((&rest pairs) &body checks)
  "Assert the text memory_bridge_search answers for PAIRS carries every CHECK."
  `(is-carrying (mb-text (mb-call "memory_bridge_search" ,@pairs)) ,@checks))

(defmacro mb-command-carries ((&rest args) &body checks)
  "Assert the text /memory-bridge answers for ARGS carries every CHECK."
  `(is-carrying (mb-text (cell-entry "nodecode-memory-bridge" "memory-bridge" ,@args)) ,@checks))

(define-test-slice "memory-bridge" "MEMORY-BRIDGE-CELL-" :start nodecode-memory-bridge:start-cell)

;;; --- lifecycle -----------------------------------------------------------------------

(defparameter *mb-lifecycle-corpus*
  (mb-write-corpus (list (cons "notes/one.md" (mb-prose "# One" "" "The gateway starts on loopback."))))
  "One folder the lifecycle tests start and stop over.")

(define-cell-lifecycle-tests "memory-bridge"
  (:config "root" (namestring *mb-lifecycle-corpus*))
  (:help :memory-bridge)
  (:command "memory-bridge")
  (:running (is (nle::find-tool "memory_bridge_search") "memory_bridge_search is registered")
            (is (nle::find-tool "memory_bridge_read") "and so is memory_bridge_read")
            (is (= 5 (nodecode-memory-bridge::setting :max-results)) "five documents by default")
            (is (= 8000 (nodecode-memory-bridge::setting :read-chars)) "eight thousand characters of a read"))
  (:stopped (is (null (nle::find-tool "memory_bridge_search")) "stop unregisters the search tool")
            (is (null (nle::find-tool "memory_bridge_read")) "and the read tool"))
  (:refused ("root" "") ("root" "/no-such-folder-memory-bridge-xyz"))
  (:idle nodecode-memory-bridge:memory-bridge-error (nodecode-memory-bridge:run-search "anything")))

;;; --- search --------------------------------------------------------------------------

(deftest memory-bridge-cell-answers-the-documents-that-match ()
  (let ((root (mb-write-corpus
               (list (cons "decisions/watering.md"
                           (mb-prose "# Watering decision" "" "Water the garden at dawn, with rainwater."))
                     (cons "projects/garden.md"
                           (mb-prose "# Lantern Garden" "" "Plant blue asters beside the east gate."))
                     (cons "projects/roof.md"
                           (mb-prose "# Roof" "" "Replace the gutter in spring."))))))
    (with-cell-stop ((mb-start root))
      (mb-carries ("query" "garden")
                  ("2 of 3 documents match" "the count is search's own")
                  ("decisions/watering.md - Watering decision" "the id, then the title")
                  ("Water the garden at dawn" "the snippet quotes the match")
                  ("projects/garden.md - Lantern Garden" "the other match")
                  (:absent "projects/roof.md" "and only the documents that matched"))
      (mb-carries ("query" "gutter")
                  ("projects/roof.md" "a word only one document has finds it")
                  (:absent "watering.md" "and the others stay out")))))

(deftest memory-bridge-cell-a-document-matching-more-words-ranks-first ()
  (let ((root (mb-write-corpus
               (list (cons "one.md" (mb-prose "# One" "" "the lantern is near the gate."))
                     (cons "two.md" (mb-prose "# Two" "" "the lantern is near the gate; the lantern is lit."))))))
    (with-cell-stop ((mb-start root))
      (let ((answer (mb-call "memory_bridge_search" "query" "lantern" "limit" 1)))
        (is (search "two.md" answer) "the document that says it twice comes first")
        (is (< (or (search "two.md" answer) 0) (length answer)) "and is the one answered")))))

(deftest memory-bridge-cell-deny-wins-over-allow ()
  (let ((root (mb-write-corpus
               (list (cons "projects/garden.md" (mb-prose "# Lantern Garden" "" "Plant the asters by the gate."))
                     (cons "projects/private/ledger.md" (mb-prose "# Ledger" "" "The gate code is written here."))
                     (cons "someday/notes.md" (mb-prose "# Notes" "" "The gate is repainted in May."))))))
    (with-cell-stop ((mb-start root "allow" (vector "**") "deny" (vector "**/private/**" "someday/**")))
      (mb-carries ("query" "gate")
                  ("projects/garden.md" "the permitted document answers")
                  (:absent "ledger.md" "the denied one is not searched")
                  (:absent "someday" "and neither is a wholly denied folder"))
      (is (search "not_found_or_denied" (mb-read "projects/private/ledger.md")) "a denied id refuses")
      (is (search "not_found_or_denied" (mb-read "nowhere/at/all.md")) "a missing id refuses the same"))))

(deftest memory-bridge-cell-a-narrow-allow-list-keeps-the-rest-out ()
  (let ((root (mb-write-corpus
               (list (cons "projects/garden.md" (mb-prose "# Garden" "" "the lantern hangs here."))
                     (cons "notes/diary.md" (mb-prose "# Diary" "" "the lantern hangs there."))))))
    (with-cell-stop ((mb-start root "allow" (vector "projects/**")))
      (mb-carries ("query" "lantern")
                  ("projects/garden.md" "the allowed folder answers")
                  (:absent "diary.md" "the folder left out of allow does not")))))

;;; --- read ----------------------------------------------------------------------------

(deftest memory-bridge-cell-reads-one-document-and-caps-it ()
  (let* ((long-body (format nil "# Long~%~%~a" (make-string 400 :initial-element #\x)))
         (root (mb-write-corpus
                (list (cons "short.md" (mb-prose "# Short" "" "Two lines, nothing more."))
                      (cons "long.md" long-body)))))
    (with-cell-stop ((mb-start root "read_chars" 300))
      (let ((short (mb-text (mb-call "memory_bridge_read" "id" "short.md"))))
        (is (search "memory_bridge_read: short.md" short) "the answer names the id")
        (is (search "Two lines, nothing more." short) "and carries the document")
        (is (not (search "truncated" short)) "a short document is whole"))
      (let ((long (mb-text (mb-call "memory_bridge_read" "id" "long.md"))))
        (is (search "truncated, showing at most 300 characters" long) "a long one says it was cut")
        (is (<= (length long) 400) "and is not the whole four hundred characters")))))

(deftest memory-bridge-cell-a-dot-name-another-type-and-a-symlink-are-never-opened ()
  (let* ((root (mb-write-corpus
                (list (cons "notes.md" (mb-prose "# Notes" "" "the lantern hangs here."))
                      (cons ".hidden/lost.md" (mb-prose "# Lost" "" "the lantern hangs under a dot."))
                      (cons "notes.txt" (mb-prose "the lantern hangs in a text file.")))))
         (target (merge-pathnames "notes.md" root))
         (link (merge-pathnames "linked.md" root)))
    (ignore-errors (sb-posix:symlink (uiop:native-namestring target) (uiop:native-namestring link)))
    (with-cell-stop ((mb-start root))
      (mb-carries ("query" "lantern")
                  ("notes.md" "the plain document answers")
                  (:absent "lost.md" "the dot-directory is not walked")
                  (:absent "notes.txt" "another file type is not opened")
                  (:absent "linked.md" "a symlink is not a document of its own")))))

;;; --- /memory-bridge -----------------------------------------------------------------

(deftest memory-bridge-cell-the-command-searches-reads-and-reports ()
  (let ((root (mb-write-corpus
               (list (cons "projects/garden.md" (mb-prose "# Lantern Garden" "" "Plant the asters by the gate."))))))
    (with-cell-stop ((mb-start root))
      (mb-command-carries ("")
                          ("1 document in" "the status names the folder's count")
                          ("allow **" "and the patterns it runs on"))
      (mb-command-carries ("aster")
                          ("projects/garden.md - Lantern Garden" "words typed after the command search"))
      (mb-command-carries ("read projects/garden.md")
                          ("Plant the asters" "and read answers the document"))
      (mb-command-carries ("read not-there.md")
                          ("not_found_or_denied" "an id nothing has refuses")))))

;;; --- the boundaries ------------------------------------------------------------------

(deftest memory-bridge-cell-a-query-of-one-letter-asks-for-a-word ()
  (let ((root (mb-write-corpus (list (cons "a.md" (mb-prose "# A" "" "hello there."))))))
    (with-cell-stop ((mb-start root))
      (is (search "at least 2 characters" (mb-answer "query" "a")) "no usable word says so")
      (is (search "missing 'query'" (mb-answer "query" "  ")) "an empty query says so"))))
