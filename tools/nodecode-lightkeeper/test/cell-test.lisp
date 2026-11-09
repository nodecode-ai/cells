;;;; cell-test.lisp --- the vault, the finds, the pointers, the primers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test reads a vault built in a scratch folder: four notes, two of
;;;; them under a folder the vault skips on purpose. Nothing here reaches the
;;;; network, an operator file or a live session.

(in-package #:nodecode.test)

(define-test-slice "lightkeeper" "LIGHTKEEPER-CELL-" :start nodecode-lightkeeper:start-cell)

;;; --- the fixture vault ---------------------------------------------------------------

(defparameter *lk-vault* nil
  "The fixture vault's namestring, built once.")

(defun lk-put (root relative text)
  "Write TEXT at ROOT/RELATIVE, folders and all."
  (let ((file (merge-pathnames relative root)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string text out))))

(defun lk-fixture ()
  "Build the fixture vault under a scratch folder and answer its namestring."
  (let ((root (merge-pathnames "lightkeeper-test-vault/" (uiop:temporary-directory))))
    (lk-put root "Home.md"
            (format nil "---~%type: meta~%updated: 2026-10-18~%---~%# Home~%~%## What it is~%~%A map of how a library is built and how the pieces connect.~%"))
    (lk-put root "2 Modules/Books.md"
            (format nil "---~%type: module~%status: shipped~%depth: deep~%updated: 2026-10-18~%tags: [module, content]~%---~%# Books~%~%## What it is~%~%A book holds chapters and pages in order. It is the unit permissions apply to.~%~%## Where it lives~%~%| Surface | Path |~%|---|---|~%| Model | `app/Models/Book.php` |~%| Controllers | `app/Entities/Controllers/` |~%~%## How it works~%~%Pages are ordered by the sort rules.~%~%## Gotchas~%~%Deleting a book orphans its pages.~%~%## Connects to~%~%- [[Chapters]] — chapters are the book's optional middle level~%- [[Sort Rules]] — a sort rule decides the order of a book's contents~%"))
    (lk-put root "2 Modules/Chapters.md"
            (format nil "---~%type: module~%status: shipped~%depth: stub~%updated: 2026-10-18~%---~%# Chapters~%~%## What it is~%~%A chapter groups pages inside a book.~%~%## Where it lives~%~%| Model | `app/Models/Chapter.php` |~%~%## How it works~%~%_Stub: written up the first time someone works on it._~%"))
    (lk-put root "_templates/Module.md"
            (format nil "---~%type: template~%---~%# Module template~%~%## What it is~%~%A template note, never a note of its own.~%~%## Where it lives~%~%| Model | `app/Models/Template.php` |~%"))
    (namestring root)))

(defun lk-vault ()
  "The fixture vault, built on first use."
  (or *lk-vault* (setf *lk-vault* (lk-fixture))))

(defun lk-absent-vault ()
  "A folder that is not there."
  (namestring (merge-pathnames "lightkeeper-absent-vault/" (uiop:temporary-directory))))

;;; --- lifecycle -------------------------------------------------------------------------

(define-cell-lifecycle-tests "lightkeeper"
  (:config "vault" (lk-vault))
  (:hooks :round :tool)
  (:help :lightkeeper)
  (:command "lightkeeper")
  (:running
   (is (equal nodecode-lightkeeper:+primer+ (nle:help :lightkeeper)) "(help :lightkeeper) is the primer")
   (is (search "vault" (nodecode-lightkeeper::prime)) "the primer reads the vault"))
  (:refused ("vault" 5) ("vault" (lk-absent-vault)))
  (:idle nodecode-lightkeeper:lightkeeper-error
         (nodecode-lightkeeper:find-notes "books")
         (nodecode-lightkeeper:read-note "Books")))

;;; --- find -------------------------------------------------------------------------------

(deftest lightkeeper-cell-finds-a-note-by-topic ()
  (with-cell-stop ((lightkeeper-start "vault" (lk-vault)))
    (let ((text (nodecode-lightkeeper:find-notes "books")))
      (is (search "Books (" text) "the note's own name leads the hit")
      (is (search "A book holds chapters and pages in order." text) "its one line follows")
      (is (search "→ 2 Modules/Books.md" text) "and where the note is")
      (is (search "connects:" text) "a hit carries its blast radius")
      (is (search "chapters are the book's optional middle level" text) "with the reason for each link"))
    (is (search "no note matches" (nodecode-lightkeeper:find-notes "zebra quintet")) "a query nothing answers says so")
    (is (search "Chapters" (nodecode-lightkeeper:find-notes "chapter groups pages")) "a topic only the body carries still finds its note")))

(deftest lightkeeper-cell-finds-the-note-that-owns-a-file ()
  (with-cell-stop ((lightkeeper-start "vault" (lk-vault)))
    (let ((text (nodecode-lightkeeper:find-notes "" :paths '("/somewhere/app/Models/Book.php"))))
      (is (search "Books (" text) "the exact path answers its note")
      (is (search "[app/Models/Book.php]" text) "and says the path it matched"))
    (is (search "Books (" (nodecode-lightkeeper:find-notes "" :paths '("/x/app/Entities/Controllers/BookController.php"))) "a file under a claimed folder answers the folder's note")
    (is (search "no note claims" (nodecode-lightkeeper:find-notes "" :paths '("/x/app/Models/Nowhere.php"))) "a file nothing claims says so")
    (is (search "no note claims" (nodecode-lightkeeper:find-notes "" :paths '("/x/app/Models/Template.php"))) "a note under the vault's templates folder claims nothing")))

;;; --- one note ---------------------------------------------------------------------------

(deftest lightkeeper-cell-reads-one-note-or-one-section ()
  (with-cell-stop ((lightkeeper-start "vault" (lk-vault)))
    (let ((whole (nodecode-lightkeeper:read-note "Books"))
          (gotchas (nodecode-lightkeeper:read-note "Books" :section "Gotchas")))
      (is (search "Deleting a book orphans its pages." whole) "the whole note answers")
      (is (search "A book holds chapters" whole) "with every section in it")
      (is (search "orphans" gotchas) "one section answers alone")
      (is (null (search "What it is" gotchas)) "and nothing else does"))
    (is (search "Chapters" (nodecode-lightkeeper::check-vault)) "check names what is still a stub")))

;;; --- the hooks --------------------------------------------------------------------------

(deftest lightkeeper-cell-a-write-names-the-note-that-owns-the-file ()
  (with-cell-stop ((lightkeeper-start "vault" (lk-vault)))
    (nodecode-lightkeeper::remember-write "lk-s1" "/somewhere/app/Models/Book.php")
    (let ((text (nodecode-lightkeeper::note-writes
                 (list :name "eval" :session-id "lk-s1")
                 (lambda (op) (declare (ignore op)) "the result"))))
      (is (search "the result" text) "the result's own text survives")
      (is (search "is documented by vault note \"Books\"" text) "and the note that owns the file is named")
      (is (search "part of the definition of done" text) "with what to do about it"))
    (let ((again (nodecode-lightkeeper::note-writes
                  (list :name "eval" :session-id "lk-s1")
                  (lambda (op) (declare (ignore op)) "the result"))))
      (is (equal "the result" again) "a second call in the same session says nothing twice"))))

(deftest lightkeeper-cell-primes-a-session-once ()
  (with-cell-stop ((lightkeeper-start "vault" (lk-vault)))
    (let ((first (nodecode-lightkeeper::prime-round
                  (list :session-id "lk-s2" :turn "lk-turn-1")
                  (lambda (op) (declare (ignore op)) nil))))
      (is (stringp first) "the first round hears the primer")
      (is (search "knowledge vault" first) "which says the vault is there")
      (is (search "lightkeeper_find" first) "and names the verb to query it"))
    (let ((second (nodecode-lightkeeper::prime-round
                   (list :session-id "lk-s2" :turn "lk-turn-2")
                   (lambda (op) (declare (ignore op)) nil))))
      (is (null second) "the second round hears it no more"))))
