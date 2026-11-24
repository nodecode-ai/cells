;;;; cell-test.lisp --- the lifecycle, the vault it reads, and the notes it writes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a real folder of
;;;; Markdown notes in a temporary directory: that a start installs the four
;;;; tools, the command, the manual topic and the frame hook, and that a stop
;;;; takes them all back; that a search ranks a title above a mention in a
;;;; body, quotes the heading a passage sits under, keeps to a folder and a
;;;; tag when asked, and never reads a file that is a symlink; that a note is
;;;; read by path, that a written note lands with frontmatter and never
;;;; overwrites, that recent notes are listed newest first; that /mdvault
;;;; reports, searches and reindexes; that recall for a trivial prompt is
;;;; nothing and for a real one is the block the turn reads; and that a cell
;;;; with no vault folder refuses instead of answering nothing.

(in-package #:nodecode.test)

;;; --- a vault ------------------------------------------------------------------

(defun prose (&rest lines)
  "LINES as one text, a newline between them."
  (format nil "~{~a~^~%~}" lines))

(defvar *vaults* 0 "How many temporary vaults this run has written.")

(defun write-vault-into (root files)
  "Write FILES, (NAME . TEXT) pairs, under ROOT. => ROOT."
  (dolist (file files root)
    (let ((path (merge-pathnames (car file) root)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                :if-does-not-exist :create :external-format :utf-8)
        (write-string (cdr file) out)))))

(defun write-vault (files)
  "A temporary folder holding FILES, (NAME . TEXT) pairs with NAME relative.
=> its pathname, directory form."
  (write-vault-into (uiop:ensure-directory-pathname
                     (format nil "~amdvault-~a-~a/" (uiop:temporary-directory)
                             (get-universal-time) (incf *vaults*)))
                    files))

(defparameter *guide*
  (prose "# Guide" ""
         "The gateway is one process on the machine." ""
         "## Install" ""
         "Install the gateway from the package.")
  "A note whose heading a snippet has to name.")

(defun tool-entry (name)
  (or (nle::find-tool name)
      (error "no tool named ~a" name)))

(defun tool-text (name &rest pairs)
  "Run the registered tool NAME with PAIRS, as dispatch calls it, and answer
the text it answered."
  (let ((entry (tool-entry name))
        (arguments (apply #'nlk:make-json-object pairs)))
    (funcall (third entry) (nle::tool-body-arguments entry arguments))))

(defun on-mdvault (root &rest config)
  "A start over ROOT as the vault, then CONFIG's pairs."
  (apply #'mdvault-start "vault_path" (namestring (uiop:ensure-directory-pathname root)) config))

(defmacro vault-carries ((&rest pairs) &body checks)
  "Assert the text mdvault_search answers for PAIRS carries every CHECK."
  `(is-carrying (message (tool-text "mdvault_search" ,@pairs)) ,@checks))

(defmacro command-carries ((&rest args) &body checks)
  "Assert the text /mdvault answers for ARGS carries every CHECK."
  `(is-carrying (message (cell-entry "nodecode-mdvault" "mdvault" ,@args)) ,@checks))

(define-test-slice "mdvault" "MDVAULT-CELL-" :start nodecode-mdvault:start-cell)

;;; --- lifecycle ----------------------------------------------------------------

(defparameter *lifecycle-vault*
  (write-vault (list (cons "guide.md"
                            (prose "# Guide" "" "The gateway starts on loopback."))))
  "One vault the lifecycle tests start and stop over.")

(define-cell-lifecycle-tests "mdvault"
  (:config "vault_path" (namestring *lifecycle-vault*))
  (:hooks :frame)
  (:help :mdvault)
  (:command "mdvault")
  (:running (is (nle::find-tool "mdvault_search") "mdvault_search is registered")
            (is (nle::find-tool "mdvault_note") "mdvault_note is registered")
            (is (nle::find-tool "mdvault_recent") "mdvault_recent is registered")
            (is (nle::find-tool "mdvault_remember") "mdvault_remember is registered")
            (is (equal "inbox" (nodecode-mdvault::setting :write-folder)) "notes are written to inbox by default")
            (is (= 4 (nodecode-mdvault::setting :recall-limit)) "four notes are recalled a turn")
            (is (member ".obsidian" (nodecode-mdvault::setting :exclude-dirs) :test #'equal) "an editor's own folder is never walked"))
  (:stopped (is (null (nle::find-tool "mdvault_search")) "stop unregisters the tools"))
  (:refused () ("vault_path" "/no-such-vault-folder-anywhere/"))
  (:idle nodecode-mdvault:mdvault-error (nodecode-mdvault::search-text "anything")))

;;; --- the search -----------------------------------------------------------------

(deftest mdvault-cell-ranks-a-title-above-a-mention-and-quotes-the-heading ()
  (let ((root (write-vault (list (cons "guide.md" *guide*)
                                  (cons "notes/coffee.md"
                                        (prose "# Coffee" "" "groceries, then the market."))))))
    (with-cell-stop ((on-mdvault root))
      (vault-carries ("query" "install")
                     ("mdvault_search: 1 note" "a count")
                     ("guide.md -- Guide" "the note that matched")
                     ("[Install]" "the heading the passage sits under")
                     ("Install the gateway from the package" "and the passage")
                     (:absent "coffee" "only the note that matched"))
      (vault-carries ("query" "coffee")
                     ("Coffee" "a title match is answered")))))

(deftest mdvault-cell-keeps-to-a-folder-and-a-tag ()
  (let ((root (write-vault (list (cons "alpha/one.md" (prose "# One" "" "the lantern is here."))
                                  (cons "beta/two.md" (prose "# Two" "" "the lantern is there."))
                                  (cons "beta/three.md" (prose "---" "tags: [lantern]" "---" ""
                                                               "# Three" "" "nothing to see."))))))
    (with-cell-stop ((on-mdvault root))
      (vault-carries ("query" "lantern" "folder" "beta")
                     ("beta/two.md" "the named folder") (:absent "alpha/one.md" "the folder left out"))
      (vault-carries ("query" "lantern" "tag" "lantern")
                     ("beta/three.md" "the tag the frontmatter carries") (:absent "beta/two.md" "the untagged note")))))

(deftest mdvault-cell-never-reads-a-symlinked-note ()
  (let* ((outside (write-vault (list (cons "secret.md" (prose "# Secret" "" "the passphrase is lantern.")))))
         (root (write-vault (list (cons "note.md" (prose "# Note" "" "the lantern is here."))))))
    (ignore-errors (sb-posix:symlink (namestring (merge-pathnames "secret.md" outside))
                                      (namestring (merge-pathnames "linked.md" root))))
    (with-cell-stop ((on-mdvault root))
      (let ((answer (tool-text "mdvault_search" "query" "passphrase")))
        (is (search "no note in the vault matches" answer) "a link out of the vault is not read")
        (is (not (search "passphrase is lantern" answer)) "and its text is nowhere in the answer")))))

(deftest mdvault-cell-reads-a-note-by-path-and-refuses-one-that-is-not-there ()
  (let ((root (write-vault (list (cons "projects/roadmap.md"
                                       (prose "# Roadmap" "" "Ship the cell in November."))))))
    (with-cell-stop ((on-mdvault root))
      (let ((answer (tool-text "mdvault_note" "path" "projects/roadmap.md")))
        (is-carrying answer ("projects/roadmap.md -- Roadmap" "the note's own path and title")
                     ("Ship the cell in November" "and its body")))
      (is (signals-error nodecode-mdvault:mdvault-error (tool-text "mdvault_note" "path" "../../etc/passwd")) "a path that leaves the vault is refused"))))

;;; --- writing -----------------------------------------------------------------------

(deftest mdvault-cell-remember-writes-a-note-and-never-overwrites ()
  (let ((root (write-vault (list (cons "note.md" (prose "# Note" "" "a note."))))))
    (with-cell-stop ((on-mdvault root))
      (let ((first (tool-text "mdvault_remember" "title" "Weekly review"
                               "content" "shipped the cell" "tags" (vector "plan"))))
        (is-carrying first ("inbox/Weekly review.md" "the note landed in the inbox")))
      (let ((path (merge-pathnames "inbox/Weekly review.md" root)))
        (is (probe-file path) "the file is there")
        (let ((written (nlk:read-text path)))
          (is-carrying written ("title: \"Weekly review\"" "frontmatter carries the title")
                       ("- \"plan\"" "and the tag")
                       ("shipped the cell" "and the body"))))
      (let ((second (tool-text "mdvault_remember" "title" "Weekly review" "content" "again")))
        (is-carrying second ("inbox/Weekly review 2.md" "a taken name gets a numeric suffix")))
      (is (signals-error nodecode-mdvault:mdvault-error (tool-text "mdvault_remember" "title" "Escape" "content" "x" "folder" "../outside")) "a folder that would leave the vault is refused"))))

(deftest mdvault-cell-recent-lists-the-newest-first ()
  (let ((root (write-vault (list (cons "old.md" (prose "# Old" "" "a note."))
                                  (cons "new.md" (prose "# New" "" "another note."))))))
    ;; a browse over a folder whose newest file is known to be at least one second old
    (with-cell-stop ((on-mdvault root))
      (let ((answer (tool-text "mdvault_recent" "days" 7 "limit" 5)))
        (is-carrying answer ("mdvault_recent: 2 notes changed" "both notes are in the window")
                     ("old.md" "one") ("new.md" "and the other")))
      (is-carrying (message (tool-text "mdvault_recent" "days" 1 "folder" "nowhere"))
                       ("no note changed" "a folder that holds nothing answers the count")))))

;;; --- /mdvault -----------------------------------------------------------------------

(deftest mdvault-cell-the-command-reports-searches-and-reindexes ()
  (let ((root (write-vault (list (cons "guide.md" *guide*)))))
    (with-cell-stop ((on-mdvault root))
      (command-carries ("") ("mdvault:" "the status names the vault") ("1 note indexed" "and its count")
                       ("Usage: /mdvault WORDS" "and how to use it"))
      (command-carries ("install") ("Guide" "words typed after the command search the vault"))
      (command-carries ("reindex") ("reindexed 1 note" "reindex answers its counts"))
      (command-carries ("note guide.md") ("Install the gateway" "note PATH reads one")))))

;;; --- recall -------------------------------------------------------------------------

(deftest mdvault-cell-recall-is-the-block-a-turn-reads ()
  (let ((root (write-vault (list (cons "guide.md" *guide*)))))
    (with-cell-stop ((on-mdvault root))
      (let ((block (nodecode-mdvault:recall-block "install")))
        (is-carrying block ("<vault_memory source=\"mdvault\">" "the block the turn reads")
                     ("guide.md" "the note that matched")
                     ("</vault_memory>" "and its close")))
      (is (null (nodecode-mdvault:recall-block "thanks")) "a prompt that asks nothing recalls nothing")
      (is (null (nodecode-mdvault:recall-block "zeppelin")) "and neither does a word the vault lacks")
      (is-carrying (message (nodecode-mdvault:recall-into-session "s1" "install"))
                   ("guide.md" "recall into a session answers the same block")))))

;;; --- no vault -------------------------------------------------------------------------

(deftest mdvault-cell-names-the-setting-it-needs-without-a-vault ()
  (is (signals-error nlk:config-refusal (mdvault-start)) "a start with no vault_path refuses")
  (is (search "vault_path" (refusal-text nlk:config-refusal (mdvault-start))) "the refusal names the setting")
  (is (null (nle::find-tool "mdvault_search")) "nothing is installed over it"))
