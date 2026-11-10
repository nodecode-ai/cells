;;;; support.lisp --- the hackertube test runner and its scripted seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; HACKERTUBE-CELL- prefix, and RUN-HACKERTUBE-TESTS runs exactly that
;;;; slice, so this system's test-op never re-runs the core suite.
;;;;
;;;; The one network call is NODECODE-HACKERTUBE::HTTP; WITH-SCRIPTED-SOURCES
;;;; stubs that name, records every call and answers scripted JSON per peer
;;;; host, so no test ever opens a socket. The library writes one file under
;;;; the home, which the run's scratch home makes disposable; each library
;;;; test removes it first.

(in-package #:nodecode.test)

(define-test-slice "hackertube" "HACKERTUBE-CELL-" :start nodecode-hackertube:start-cell)

;;; --- config and lifecycle fixtures ----------------------------------------------

(defmacro with-hackertube ((&rest config-pairs) &body body)
  "Run BODY with the cell started on CONFIG-PAIRS and stopped on unwind."
  `(with-cell-stop ((hackertube-start ,@config-pairs))
     ,@body))

(defun empty-library ()
  "Remove the library file, so a test starts with nothing saved."
  (let ((path (nodecode-hackertube::library-path)))
    (when (probe-file path) (delete-file path))))

;;; --- the scripted seam -----------------------------------------------------------

(defun ht-object (&rest specs)
  "One JSON object built from alternating key/value pairs."
  (apply #'nlk:make-json-object specs))

(defun ht-json (&rest specs)
  "A JSON text body built from alternating key/value pairs."
  (nlk:encode-json-object (apply #'nlk:make-json-object specs)))

(defstruct (hackertube-http (:copier nil))
  url params headers timeout)

(defun hackertube-param (call name)
  "The value CALL asked for under NAME, or NIL."
  (let ((pair (assoc name (hackertube-http-params call) :test #'string=)))
    (and pair (princ-to-string (cdr pair)))))

(defun call-to (calls host)
  "The recorded call HOST was asked, or NIL."
  (find host calls :key #'hackertube-http-url
                :test (lambda (host url) (and (search host url) t))))

(defmacro with-scripted-sources ((calls answer) &body forms)
  "Run FORMS with the cell's one network call stubbed: every call recorded
into the list CALLS, oldest first, and ANSWER -- a function of the call
answering (values TEXT STATUS) -- answered instead of the network."
  `(let ((,calls '()))
     (with-stubbed-fdefinition (nodecode-hackertube::http (url params headers timeout)
                                  (let ((call (make-hackertube-http :url url :params params
                                                                    :headers headers
                                                                    :timeout timeout)))
                                    (setf ,calls (append ,calls (list call)))
                                    (funcall ,answer call)))
       ,@forms)))

;;; --- the peers' answers ------------------------------------------------------------

(defparameter +itunes-podcast+
  (ht-json "results"
           (vector (ht-object "kind" "podcast"
                               "collectionId" 111
                               "collectionName" "Reasonable Doubts"
                               "artistName" "Two Hosts"
                               "collectionViewUrl" "https://podcasts.apple.com/us/podcast/id111")
                   (ht-object "wrapperType" "podcastEpisode"
                               "kind" "podcast-episode"
                               "trackId" 222
                               "collectionId" 111
                               "trackName" "Le Guin on Le Guin"
                               "artistName" "Two Hosts"
                               "trackViewUrl" "https://podcasts.apple.com/us/podcast/id111?i=222"
                               "trackTimeMillis" 1800000)))
  "What Apple's Search API answers for a podcast query.")

(defparameter +itunes-audiobook+
  (ht-json "results"
           (vector (ht-object "wrapperType" "audiobook"
                               "collectionId" 333
                               "collectionName" "The Dispossessed (Unabridged)"
                               "artistName" "Ursula K. Le Guin"
                               "collectionViewUrl" "https://books.apple.com/us/audiobook/id333"
                               "trackTimeMillis" 3600000)))
  "What Apple's Search API answers for an audiobook query.")

(defparameter +openlibrary+
  (ht-json "docs"
           (vector (ht-object "key" "/works/OL123W"
                               "title" "The Dispossessed"
                               "author_name" (vector "Ursula K. Le Guin"))
                   (ht-object "key" "/subjects/place:earth"
                               "title" "Not a work page")))
  "What Open Library answers: one work and one doc that is no work page.")

(defparameter +librivox+
  (ht-json "books"
           (vector (ht-object "id" 42
                               "title" "Persuasion"
                               "url_librivox" "https://librivox.org/persuasion-by-jane-austen/"
                               "totaltimesecs" 27000
                               "authors" (vector (ht-object "first_name" "Jane"
                                                             "last_name" "Austen")))))
  "What LibriVox answers for an author search.")

(defparameter +hn+
  (ht-json "hits"
           (vector (ht-object "objectID" "900"
                               "url" "https://openlibrary.org/works/OL123W")
                   (ht-object "objectID" "901"
                               "url" "https://example.org/unrelated")))
  "What Hacker News answers: one story linking the Open Library work.")

(defparameter +youtube+
  (ht-json "items"
           (vector (ht-object "id" (ht-object "videoId" "abc123")
                               "snippet" (ht-object "title" "A talk about software"
                                                    "channelTitle" "A Channel"))))
  "What YouTube's Data API answers for a video search.")

(defun serves (body)
  "A scripted answer: BODY as HTTP 200, whatever was asked."
  (lambda (call) (declare (ignore call)) (values body 200)))

(defun fixture-answer ()
  "A scripted ANSWER over the fixtures above: every peer answers its own
body, chosen by host, and Apple's body by the media it was asked for."
  (lambda (call)
    (let ((url (hackertube-http-url call)))
      (cond ((search "itunes.apple.com" url)
             (values (if (equal "audiobook" (hackertube-param call "media"))
                         +itunes-audiobook+
                         +itunes-podcast+)
                     200))
            ((search "openlibrary.org" url) (values +openlibrary+ 200))
            ((search "librivox.org" url) (values +librivox+ 200))
            ((search "hn.algolia.com" url) (values +hn+ 200))
            ((search "googleapis.com" url) (values +youtube+ 200))
            (t (values "{}" 200))))))

;;; --- calling what the cell contributes -------------------------------------------

(defun hackertube-search-tool (query &optional sections (max-results nil))
  "The registered `hackertube_search' tool, called the way the engine calls
it: the declaration materializes the record, the body answers the text."
  (let ((entry (nle::find-tool "hackertube_search")))
    (assert entry () "the hackertube_search tool is not registered")
    (funcall (third entry)
             (nlk:contract-decode
              (second entry)
              (apply #'nlk:make-json-object
                     (append (list "query" query
                                   "sections" (coerce (or sections '("videos" "podcasts"
                                                                     "books" "audiobooks"))
                                                       'vector))
                             (when max-results (list "max_results" max-results))))))))

(defun hackertube-library-tool (action &optional (item-id ""))
  "The registered `hackertube_library' tool, called the way the engine calls it."
  (let ((entry (nle::find-tool "hackertube_library")))
    (assert entry () "the hackertube_library tool is not registered")
    (funcall (third entry)
             (nlk:contract-decode
              (second entry)
              (nlk:make-json-object "action" action "item_id" item-id)))))
