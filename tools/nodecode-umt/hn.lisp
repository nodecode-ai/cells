;;;; hn.lisp --- Hacker News: the ranked lists, a keyword search, one thread.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The public Firebase API answers the front page, the newest and the best
;;;; rankings as an array of item ids, and one item as an object; the public
;;;; Algolia index answers a keyword search. Both are keyless and read-only.
;;;; Nothing is cached: a ranking is the ranking now.

(in-package #:nodecode-umt)

(defparameter +hn-limit+ 30 "How many stories one list or search may answer with.")
(defparameter +hn-text-limit+ 500 "How much of a story's own text a list shows.")

(defun hn-api-base () (string-right-trim "/" (setting :hn-api-base)))
(defun hn-search-base () (string-right-trim "/" (setting :hn-search-base)))

;;; --- one item --------------------------------------------------------------------

(defun hn-item (id)
  "The item ID's JSON object, or NIL when it is gone, dead or deleted."
  (let ((item (http-json (format nil "~a/item/~d.json" (hn-api-base) id) :what "Hacker News")))
    (when (and (hash-table-p item)
               (not (nlk:json-value item :boolean "dead"))
               (not (nlk:json-value item :boolean "deleted")))
      item)))

(defun hn-story (item)
  "ITEM as the plist a list or a thread renders."
  (list :id (nlk:json-value item :integer "id")
        :title (or (nlk:json-value item :text "title") "Untitled story")
        :url (nlk:json-value item :string "url")
        :author (nlk:json-value item :string "by")
        :score (or (nlk:json-value item :integer "score") 0)
        :comments (or (nlk:json-value item :integer "descendants") 0)
        :at (timestamp (nlk:json-value item :integer "time"))
        :text (plain-text (nlk:json-value item :string "text"))))

(defun hn-story-list (endpoint limit)
  "The first LIMIT stories of ENDPOINT's ranking, in its order."
  (let* ((ids (nlk:json-array (http-json (format nil "~a/~a.json" (hn-api-base) endpoint)
                                          :what "Hacker News")))
         (wanted (subseq ids 0 (min limit (length ids)))))
    (loop for id across wanted
          for item = (hn-item id)
          when (and item (equal "story" (nlk:json-value item :string "type")))
            collect (hn-story item))))

;;; --- the rendering --------------------------------------------------------------

(defun render-stories (heading stories)
  "HEADING, then one story a block: title, its numbers, its url, its own text."
  (if (null stories)
      (format nil "~a: nothing found." heading)
      (with-output-to-string (out)
        (format out "~a (~d):~%" heading (length stories))
        (loop for story in stories
              for index from 1
              do (format out "~d. ~a~%" index (getf story :title))
                 (format out "   ~d points, ~d comments~@[ - ~a~]~@[, ~a~]~%"
                         (getf story :score) (getf story :comments)
                         (getf story :author) (getf story :at))
                 (when (getf story :url) (format out "   ~a~%" (getf story :url)))
                 (when (getf story :text)
                   (format out "   ~a~%" (nlk:clip (getf story :text) +hn-text-limit+ :ellipsis "...")))
                 (when (getf story :id) (format out "   hn item ~d~%" (getf story :id)))))))

(defun render-comment (comment depth out)
  "COMMENT and its replies, DEPTH levels in."
  (format out "~v@t~a~@[ (~a)~]: ~a~%"
          (* 2 depth) (or (getf comment :author) "unknown") (getf comment :at)
          (or (getf comment :text) "[no text]"))
  (dolist (reply (getf comment :replies))
    (render-comment reply (1+ depth) out)))

(defun render-thread (story replies)
  "One thread: the story, its text, then every reply indented under its parent."
  (with-output-to-string (out)
    (format out "~a~%" (getf story :title))
    (format out "hn item ~d; ~d points, ~d comments~@[ - ~a~]~@[, ~a~]~%"
            (getf story :id) (getf story :score) (getf story :comments)
            (getf story :author) (getf story :at))
    (when (getf story :url) (format out "~a~%" (getf story :url)))
    (when (getf story :text) (format out "~%~a~%" (getf story :text)))
    (if replies
        (progn (format out "~%Replies:~%")
               (loop for reply in replies do (render-comment reply 1 out)))
        (format out "~%No replies yet.~%"))))

(defun thread-replies (kids depth max-children)
  "KIDS' comments, each with its own replies to DEPTH, at most MAX-CHILDREN a level."
  (when (plusp depth)
    (loop for kid across (subseq kids 0 (min max-children (length kids)))
          for item = (hn-item kid)
          when (and item (equal "comment" (nlk:json-value item :string "type")))
            collect (list :author (nlk:json-value item :string "by")
                          :at (timestamp (nlk:json-value item :integer "time"))
                          :text (plain-text (nlk:json-value item :string "text") :limit 400)
                          :replies (thread-replies (nlk:json-array item "kids") (1- depth) max-children)))))

;;; --- the verbs --------------------------------------------------------------------

(define-verb top-stories (&key (limit 10))
  "The Hacker News front page, in rank order: LIMIT stories (10 by default, at
most 30), each with its title, points, comment count, author, date, url and
item id. The current ranking, not a search: use SEARCH-STORIES for a topic."
  (render-stories "Top Hacker News stories"
                  (hn-story-list "topstories" (bounded limit 1 +hn-limit+))))

(define-verb new-stories (&key (limit 10))
  "The newest Hacker News submissions, newest first: LIMIT stories (10 by
default, at most 30). Use when recency matters more than score."
  (render-stories "New Hacker News stories"
                  (hn-story-list "newstories" (bounded limit 1 +hn-limit+))))

(define-verb best-stories (&key (limit 10))
  "Hacker News's best ranking -- high-scoring stories beyond the current front
page: LIMIT stories (10 by default, at most 30)."
  (render-stories "Hacker News best stories"
                  (hn-story-list "beststories" (bounded limit 1 +hn-limit+))))

(define-verb search-stories (query &key (limit 10))
  "Search Hacker News stories for QUERY through the public Algolia index,
ranked by its relevance: at most LIMIT stories (10 by default, 30 at most),
each with points, comments, author, date, url and item id."
  (let ((query (nlk:trimmed (princ-to-string query))))
    (unless (plusp (length query)) (fail "a search needs a query"))
    (let* ((url (format nil "~a/search?tags=story&query=~a&hitsPerPage=~d"
                         (hn-search-base) (url-encode query) (bounded limit 1 +hn-limit+)))
           (payload (http-json url :what "Hacker News search"))
           (hits (nlk:json-array payload "hits")))
      (render-stories
       (format nil "Hacker News stories matching ~s" query)
       (loop for hit across hits
             collect (list :id (ignore-errors (parse-integer (or (nlk:json-value hit :string "objectID") "")
                                                            :junk-allowed t))
                           :title (or (nlk:json-value hit :text "title") "Untitled story")
                           :url (nlk:json-value hit :string "url")
                           :author (nlk:json-value hit :string "author")
                           :score (or (nlk:json-value hit :integer "points") 0)
                           :comments (or (nlk:json-value hit :integer "num_comments") 0)
                           :at (nlk:json-value hit :string "created_at")
                           :text (plain-text (nlk:json-value hit :string "story_text"))))))))

(define-verb item-thread (id &key (depth 2) (max-children 20))
  "The discussion on Hacker News item ID: the story, its own text, and its
comment tree to DEPTH levels (2 by default, at most 6), at most MAX-CHILDREN
(20 by default, at most 50) replies a level. Use it once you have an item id
from a list or a search."
  (let ((id (if (integerp id) id (ignore-errors (parse-integer (princ-to-string id) :junk-allowed t)))))
    (unless id (fail "a thread needs a numeric Hacker News item id"))
    (let ((root (hn-item id)))
      (unless root (fail "Hacker News item ~d was not found" id))
      (render-thread (hn-story root)
                     (thread-replies (nlk:json-array root "kids")
                                     (bounded depth 1 6)
                                     (bounded max-children 1 50))))))
