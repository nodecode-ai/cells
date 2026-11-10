;;;; support.lisp --- the test slice, and a scratch Cortive library.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; CORTIVE-CELL- name prefix; RUN-CORTIVE-TESTS runs exactly that slice.
;;;;
;;;; Nothing here reaches outside: the library is a temporary SQLite file made
;;;; by the tests themselves, with the captures, the FTS5 index and the two
;;;; collection tables the cell reads. One row is encrypted, one repeats
;;;; another's content hash, one is not about Lisbon: the ranking, the skip
;;;; and the collapse all have something to prove.

(in-package #:nodecode.test)

(define-test-slice "cortive" "CORTIVE-CELL-" :start nodecode-cortive:start-cell)

(defvar *library-file* nil
  "The scratch library this test runs against.")

(defun insert-capture (db &key id title source tags content timestamp hash (encrypted 0))
  (sqlite:execute-non-query
   db "insert into captures (id, title, url, domain, tags, source, content, note, timestamp,
                             content_hash, is_encrypted)
       values (?, ?, ?, '', ?, ?, ?, '', ?, ?, ?)"
   id title (format nil "https://example.com/~a" id) tags source content timestamp hash encrypted)
  (sqlite:execute-non-query
   db "insert into captures_fts (rowid, id, title, url, domain, tags, source, content, note)
       values ((select rowid from captures where id = ?), ?, ?, ?, '', ?, ?, ?, '')"
   id id title (format nil "https://example.com/~a" id) tags source content))

(defun make-library ()
  "A scratch library with five captures and one collection; => its path."
  (let ((path (format nil "~acortive-cell-~d.db" (uiop:temporary-directory) (random 1000000))))
    (when (probe-file path) (ignore-errors (delete-file path)))
    (sqlite:with-open-database (db (namestring path))
      (sqlite:execute-non-query
       db "create table captures (id text primary key, title text, url text, domain text,
                                  tags text, source text, content text, note text, timestamp text,
                                  content_hash text, is_encrypted integer default 0)")
      (sqlite:execute-non-query
       db "create virtual table captures_fts using fts5(id, title, url, domain, tags, source,
                                                        content, note)")
      (sqlite:execute-non-query
       db "create table collections (id text primary key, name text, pinned integer default 0,
                                     member_count integer default 0)")
      (sqlite:execute-non-query
       db "create table capture_collections (collection_id text, capture_id text, score real default 0)")
      (sqlite:execute-non-query db "pragma user_version = 11")
      (insert-capture db :id "ferry1" :title "Lisbon ferry notes" :source "browser_extension"
                       :tags "[\"travel\", \"portugal\"]"
                       :content "The 7:30 crossing from Cais do Sodre to Cacilhas takes ten minutes and costs about two euros."
                       :timestamp "2026-05-15T09:00:00Z" :hash "h-ferry")
      (insert-capture db :id "sour1" :title "Sourdough starter schedule" :source "pdf_import"
                       :tags "[\"baking\"]"
                       :content "Feed the starter twice a day, at twelve hour intervals, with equal weights of flour and water."
                       :timestamp "2026-06-01T08:00:00Z" :hash "h-sour")
      (insert-capture db :id "hotel1" :title "Lisbon hotel booking" :source "screenshot_capture"
                       :tags "[\"travel\"]"
                       :content "Booked the Hotel Baixa for three nights, check-in on Friday."
                       :timestamp "2026-05-20T18:00:00Z" :hash "h-hotel")
      (insert-capture db :id "copy1" :title "Lisbon ferry notes, copied" :source "clipboard_capture"
                       :tags "[\"travel\"]"
                       :content "The 7:30 crossing from Cais do Sodre to Cacilhas takes ten minutes and costs about two euros."
                       :timestamp "2026-05-10T09:00:00Z" :hash "h-ferry")
      (insert-capture db :id "locked1" :title "Lisbon ferry password" :source "clipboard_capture"
                       :tags "[]" :content "lisbon ferry password: Kettle42"
                       :timestamp "2026-05-21T09:00:00Z" :hash "h-locked" :encrypted 1)
      (sqlite:execute-non-query
       db "insert into collections (id, name, pinned, member_count) values ('col1', 'Travel', 1, 2)")
      (sqlite:execute-non-query
       db "insert into capture_collections (collection_id, capture_id, score) values
           ('col1', 'ferry1', 0.9), ('col1', 'hotel1', 0.5)"))
    path))

(defmacro with-cortive-fixture (&body body)
  "Run BODY with a fresh scratch library behind *LIBRARY-FILE*."
  `(let ((*library-file* (make-library)))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file *library-file*)))))

(defmacro with-cortive ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS (guard, limit) and stopped on unwind."
  `(with-cell-stop ((cortive-start "library" *library-file* ,@pairs))
     ,@body))

(defmacro with-library-cell ((&rest pairs) &body body)
  "Run BODY on a fresh scratch library with the cell started on PAIRS."
  `(with-cortive-fixture (with-cortive ,pairs ,@body)))

(defun cortive-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))
