;;;; sources.lisp --- the five peers, one request each, and what they answer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything the cell knows about the outside: the endpoints, the one
;;;; parameter set each gets, and how its answer becomes records. CELL.LISP
;;;; is the only caller.
;;;;
;;;; The peers are Hacker News (evidence: a story that links an item),
;;;; Apple's iTunes Search (podcast shows, episodes, audiobooks), Open
;;;; Library (books), LibriVox (public-domain audiobooks) and, only when a
;;;; key is configured, YouTube. Each request carries the query and returns
;;;; bounded metadata; nothing is downloaded, no cover, preview or media file
;;;; is fetched, and no key of an unenabled source travels.
;;;;
;;;; A record is a plist:
;;;;   (:id "ol:OL123W" :format "book" :title "..." :contributors ("...")
;;;;    :url "https://..." :seconds 3600 :source "openlibrary" :mentions 0)
;;;; The id is the source's own identity with the source's prefix, so two
;;;; sources never collide and the same work is recognised on a second
;;;; search.

(in-package #:nodecode-hackertube)

;;; --- the bounds ----------------------------------------------------------

(defparameter +user-agent+
  "nodecode-hackertube/0.1.0 (+https://github.com/nodecode-ai/cells)"
  "Who the peers see asking. It names the cell and where it comes from, and
nothing about the operator.")

(defparameter +max-response-bytes+ (* 2 1024 1024)
  "The most one answer may be: twenty rows of metadata. A peer that sends
more is cut, not waited on.")

(defparameter +per-source-limit+ 20
  "How many rows one lookup asks a peer for. The original asked for the same.")

(defparameter +json-headers+
  '(("user-agent" . "nodecode-hackertube/0.1.0 (+https://github.com/nodecode-ai/cells)")
    ("accept" . "application/json"))
  "What every lookup sends. The user agent is repeated from +USER-AGENT+
because a header list is built before the call; a drift is a test away.")

(defparameter +sections+
  '(("videos" "video")
    ("podcasts" "podcast_show" "podcast_episode")
    ("books" "book")
    ("audiobooks" "audiobook"))
  "The sections a caller may name, each with the record formats it admits,
as the original's SECTIONS maps them.")

(defparameter +sources+
  '(("hn" . "Hacker News")
    ("itunes" . "Apple Podcasts")
    ("openlibrary" . "Open Library")
    ("librivox" . "LibriVox")
    ("youtube" . "YouTube"))
  "Every source, and the name a block shows it under.")

(defparameter +work-scanner+
  (cl-ppcre:create-scanner "^/works/OL\\d+W$")
  "An Open Library work key, as the original's regexp requires. Anything
else in a result is not a work page and is dropped.")

;;; --- names --------------------------------------------------------------

(defun source-name (id)
  "The name a block shows source ID under."
  (or (cdr (assoc id +sources+ :test #'string=)) id))

(defun section-formats (section)
  "The record formats SECTION admits, or NIL when it is no section."
  (cdr (assoc section +sections+ :test #'string=)))

(defun section-names (sections)
  "SECTIONS as a list of lowercase names: absent means every section."
  (let ((sections (cond ((null sections) (mapcar #'first +sections+))
                        ((vectorp sections) (coerce sections 'list))
                        ((listp sections) sections)
                        (t (mapcar #'first +sections+)))))
    (mapcar (lambda (name) (string-downcase (clean name 40))) sections)))

(defun formats-for (sections)
  "Every format the named SECTIONS admit; no section named means all four."
  (let ((sections (section-names sections)))
    (remove-duplicates (loop for section in sections
                             for formats = (section-formats section)
                             when formats append (copy-list formats))
                       :test #'string=)))

(defun clean (text &optional (limit 200))
  "TEXT as one line: whitespace collapsed, a title's newlines gone, and
nothing below U+0020 left to break a block. A title is a peer's string and
may hold anything."
  (when (stringp text)
    (let ((out (make-string-output-stream))
          (spaced nil))
      (loop for char across text
            do (if (or (<= (char-code char) 32) (= (char-code char) 127))
                   (setf spaced t)
                   (progn (when spaced (write-char #\Space out))
                          (setf spaced nil)
                          (write-char char out))))
      (let ((text (string-trim " " (get-output-stream-string out))))
        (when (plusp (length text))
          (if (> (length text) limit) (subseq text 0 limit) text))))))

(defun peer (url)
  "The host URL is on, for a message."
  (or (ignore-errors (quri:uri-host (quri:uri url))) url))

(defun normalized-url (url)
  "URL lowercased with its trailing slash gone: how two peers are compared
when one links the other."
  (when (and (stringp url)
             (or (uiop:string-prefix-p "https://" url) (uiop:string-prefix-p "http://" url)))
    (string-right-trim "/" (string-downcase (string-trim " " url)))))

;;; --- json, read leniently ------------------------------------------------

(defun json-field (object key)
  "OBJECT's member KEY when OBJECT is a JSON object, else NIL."
  (and (hash-table-p object) (gethash key object)))

(defun json-text (object key &optional default)
  "OBJECT's member KEY as text: DEFAULT when it is absent, null or not text."
  (let ((value (json-field object key)))
    (if (stringp value) value default)))

(defun json-integer (object key)
  "OBJECT's member KEY when it is an integer, else NIL. Never coerced."
  (let ((value (json-field object key)))
    (and (integerp value) value)))

(defun json-list (value)
  "VALUE as a list: a JSON array decodes to a vector, and anything else is
no list at all."
  (cond ((vectorp value) (coerce value 'list))
        ((and (listp value) value) value)
        (t nil)))

(defun json-texts (object key)
  "OBJECT's member KEY as a list of its non-empty text elements."
  (remove nil (mapcar #'clean (json-list (json-field object key)))))

(defun millis->seconds (millis)
  "MILLIS as whole seconds, or NIL when unknown. Never zero."
  (and millis (plusp millis) (floor millis 1000)))

;;; --- the one network call ------------------------------------------------

(defun query-string (params)
  "PARAMS, an alist of (name . value), as an encoded query string."
  (format nil "~{~a=~a~^&~}"
          (loop for (name . value) in params
                append (list (quri:url-encode (string name))
                             (quri:url-encode (princ-to-string value) :encoding :utf-8)))))

(defun http (url params headers timeout)
  "The cell's one network call: NLK:HTTP GET of URL with PARAMS, the body
as text under the byte cap. Tests stub this name and never touch the network."
  (multiple-value-bind (body status)
      (nlk:http :get (format nil "~a?~a" url (query-string params))
                :headers headers
                :timeout timeout
                :max-bytes +max-response-bytes+)
    (values (if (stringp body) body (nlk:decode-octets body)) status)))

(defun fetch-json (url params &key (headers +json-headers+))
  "PARAMS asked of URL => the decoded JSON. A peer that is down, slow or
answers HTTP non-200 is one refusal naming the host, never a lost turn."
  (multiple-value-bind (body status)
      (handler-case (http url params headers (setting :timeout-seconds))
        (hackertube-error (condition) (error condition))
        (error (condition)
          (fail "~a could not be reached (~a)" (peer url) (type-of condition))))
    (unless (eql status 200)
      (fail "~a answered HTTP ~a" (peer url) status))
    (let ((json (ignore-errors (nlk:decode-json body))))
      (unless (or (hash-table-p json) (vectorp json))
        (fail "unreadable answer from ~a" (peer url)))
      json)))

;;; --- a record ------------------------------------------------------------

(defun record (&key id format title contributors url seconds source)
  "One record, or NIL when its identity, title or url is missing."
  (let ((title (clean title))
        (url (and (stringp url) (string-trim " " url))))
    (when (and id title url (uiop:string-prefix-p "https://" url))
      (list :id id
            :format format
            :title title
            :contributors (remove nil (mapcar #'clean contributors))
            :url url
            :seconds (and seconds (plusp seconds) seconds)
            :source source
            :mentions 0))))

;;; --- Hacker News: evidence, never a media record -------------------------

(defun hn-hits (query)
  "The Hacker News stories for QUERY, each (:id \"123\" :url \"https://...\").
The original keeps a story id, a comment permalink and the time; comment
text and usernames are not kept here either."
  (let* ((json (fetch-json "https://hn.algolia.com/api/v1/search"
                            (list (cons "query" query)
                                  (cons "tags" "story")
                                  (cons "hitsPerPage" +per-source-limit+))))
         (hits (json-list (json-field json "hits"))))
    (loop for hit in hits
          for id = (json-field hit "objectID")
          for url = (normalized-url (json-text hit "url"))
          when (and (stringp id) url (every #'digit-char-p id))
            collect (list :id id :url url))))

;;; --- Apple podcasts, episodes and audiobooks -----------------------------

(defun itunes-format (row)
  "ROW's format, as the original's _format reads kind and wrapperType."
  (let ((kind (json-text row "kind"))
        (wrapper (json-text row "wrapperType")))
    (cond ((equal kind "podcast") "podcast_show")
          ((or (equal kind "podcast-episode") (equal wrapper "podcastEpisode"))
           "podcast_episode")
          ((equal wrapper "audiobook") "audiobook")
          (t nil))))

(defun itunes-record (row)
  "ROW as a record, or NIL when it is not a show, an episode or an
audiobook, or carries no identity. A show's trackTimeMillis is its length
of run, not an episode's duration, so a show has none."
  (let* ((format (itunes-format row))
         ;; An episode is a track of its show, so its identity and its page
         ;; are the track's; a show and an audiobook are a collection.
         (episode-p (equal format "podcast_episode"))
         (key (json-integer row (if episode-p "trackId" "collectionId")))
         (prefix (cond ((equal format "podcast_show") "podcast")
                       ((equal format "audiobook") "audiobook")
                       (t "episode"))))
    (when (and format key)
      (record :id (format nil "itunes:~a:~d" prefix key)
              :format format
              :title (json-text row (if episode-p "trackName" "collectionName"))
              :contributors (list (json-text row "artistName"))
              :url (json-text row (if episode-p "trackViewUrl" "collectionViewUrl"))
              :seconds (unless (equal format "podcast_show")
                         (millis->seconds (json-integer row "trackTimeMillis")))
              :source "itunes"))))

(defun itunes-records (query media)
  "One bounded iTunes Search for QUERY in MEDIA (podcast or audiobook)."
  (let* ((json (fetch-json "https://itunes.apple.com/search"
                            (list (cons "term" query)
                                  (cons "media" media)
                                  (cons "limit" +per-source-limit+))))
         (rows (json-list (json-field json "results"))))
    (loop for row in rows
          when (hash-table-p row)
            append (let ((record (itunes-record row)))
                     (and record (list record))))))

;;; --- Open Library: books -------------------------------------------------

(defparameter +openlibrary-fields+
  "key,title,author_name,first_publish_year,id_project_gutenberg"
  "Only the fields the parser maps, as the original asked for.")

(defun openlibrary-headers (contact)
  "The headers one Open Library lookup sends: CONTACT, when the operator
gave one, rides the user agent and nowhere else."
  (if (and (stringp contact) (plusp (length (string-trim " " contact))))
      `(("user-agent" . ,(format nil "~a contact=~a" +user-agent+ (string-trim " " contact)))
        ("accept" . "application/json"))
      +json-headers+))

(defun openlibrary-records (query contact)
  "One bounded Open Library search for QUERY. A doc whose key is not a work
page is dropped, as the original drops it."
  (let* ((json (fetch-json "https://openlibrary.org/search.json"
                            (list (cons "q" query)
                                  (cons "limit" +per-source-limit+)
                                  (cons "fields" +openlibrary-fields+))
                            :headers (openlibrary-headers contact)))
         (docs (json-list (json-field json "docs"))))
    (loop for doc in docs
          for key = (json-text doc "key")
          when (and (hash-table-p doc) key (cl-ppcre:scan +work-scanner+ key))
            append (let ((record
                           (record :id (format nil "ol:~a" (subseq key (length "/works/")))
                                   :format "book"
                                   :title (json-text doc "title")
                                   :contributors (json-texts doc "author_name")
                                   :url (format nil "https://openlibrary.org~a" key)
                                   :source "openlibrary")))
                     (and record (list record))))))

;;; --- LibriVox: public-domain audiobooks ----------------------------------

(defparameter +librivox-fields+
  "{id,title,url_librivox,url_text_source,language,totaltimesecs,authors}"
  "Only the fields the parser maps, as the original asked for.")

(defun librivox-contributors (book)
  "BOOK's readers and authors as names, from the API's author objects."
  (loop for author in (json-list (json-field book "authors"))
        when (hash-table-p author)
          append (let ((name (clean (format nil "~@[~a ~]~@[~a~]"
                                            (json-text author "first_name")
                                            (json-text author "last_name")))))
                   (and name (list name)))))

(defun librivox-records (query)
  "One bounded LibriVox author search. The API answers 404 when the author
has no recording: that is no results, not a source that is down."
  (let* ((json (handler-case
                    (fetch-json "https://librivox.org/api/feed/audiobooks"
                                (list (cons "author" query)
                                      (cons "format" "json")
                                      (cons "limit" +per-source-limit+)
                                      (cons "fields" +librivox-fields+)))
                  (hackertube-error (condition)
                    (if (search "HTTP 404" (princ-to-string condition))
                        nil
                        (error condition)))))
         (books (json-list (json-field json "books"))))
    (loop for book in books
          for raw = (json-field book "id")
          for id = (cond ((integerp raw) (princ-to-string raw))
                         ((and (stringp raw) (plusp (length raw))
                               (every #'digit-char-p raw))
                          raw)
                         (t nil))
          when (and (hash-table-p book) id)
            append (let ((record
                           (record :id (format nil "lv:~a" id)
                                   :format "audiobook"
                                   :title (json-text book "title")
                                   :contributors (librivox-contributors book)
                                   :url (json-text book "url_librivox")
                                   :seconds (let ((seconds (json-integer book "totaltimesecs")))
                                              (and seconds (plusp seconds) seconds))
                                   :source "librivox")))
                     (and record (list record))))))

;;; --- YouTube: opt-in, one key --------------------------------------------

(defun youtube-records (query key)
  "One bounded YouTube Data API search for QUERY. Duration is not asked
for, so a video has none: the original leaves an unknown duration unknown
rather than filling it in."
  (let* ((json (fetch-json "https://www.googleapis.com/youtube/v3/search"
                            (list (cons "part" "snippet")
                                  (cons "type" "video")
                                  (cons "q" query)
                                  (cons "maxResults" +per-source-limit+)
                                  (cons "key" key))))
         (rows (json-list (json-field json "items"))))
    (loop for row in rows
          for video = (json-text (json-field row "id") "videoId")
          for snippet = (json-field row "snippet")
          when (and (hash-table-p row) video)
            append (let ((record
                           (record :id (format nil "yt:~a" video)
                                   :format "video"
                                   :title (json-text snippet "title")
                                   :contributors (list (json-text snippet "channelTitle"))
                                   :url (format nil "https://www.youtube.com/watch?v=~a" video)
                                   :source "youtube")))
                     (and record (list record))))))
