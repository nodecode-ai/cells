;;;; cell-test.lisp --- the hackertube cell, end to end over a scripted seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: that a start installs both tools, the command and
;;;; the manual and a stop takes them back, and a bad section refuses; that a
;;;; query reaches the peers it names and the block carries each record's
;;;; title, url and id; that a work Hacker News links leads, and a record
;;;; naming none of the query's words is dropped unless nothing matches; that
;;;; one failed source is named and never hides the others' records; that a
;;;; record is saved into the library under the home, listed, forgotten, and
;;;; that an id this process never showed answers the repair; that YouTube
;;;; answers only when a key is configured; and that a search with no query
;;;; never reaches a body.

(in-package #:nodecode.test)

;;; --- lifecycle, settings and the tools' shape ------------------------------------

(define-cell-lifecycle-tests "hackertube"
  (:help :hackertube)
  (:command "hackertube")
  (:running
   (is (nle::find-tool "hackertube_search") "the search tool is registered")
   (is (nle::find-tool "hackertube_library") "and the library tool with it")
   (is (equal '("hn" "itunes" "openlibrary" "librivox") (nodecode-hackertube::setting :sources)) "the four keyless sources are enabled by default")
   (is (= 10 (nodecode-hackertube::setting :max-results)) "and one search lists ten lines"))
  (:stopped
   (is (null (nle::find-tool "hackertube_search")) "stop takes the search tool out")
   (is (null (nle::find-tool "hackertube_library")) "and the library tool"))
  (:refused ("max_results" 0) ("timeout_seconds" 0)
            ("sources" #("nope")) ("sources" #("youtube")))
  (:idle nodecode-hackertube:hackertube-error
         (nodecode-hackertube:find-media "ursula le guin")))

(deftest hackertube-cell-refuses-a-search-with-no-query ()
  ;; The one member the declaration requires: a call without it never reaches
  ;; the body, which is the repair the model reads.
  (is (signals-error nlk:contract-refusal (nlk:contract-decode (nlk:contract 'nodecode-hackertube::media-search) (nlk:make-json-object "sections" (vector "books"))))))

;;; --- one query, the peers, and the block ----------------------------------------

(deftest hackertube-cell-a-linked-work-leads-and-every-line-carries-its-id ()
  (with-hackertube ()
    (with-scripted-sources (calls (fixture-answer))
      (let ((text (hackertube-search-tool "ursula le guin" '("podcasts" "books"))))
        (is (search "The Dispossessed" text) "the book the sources answered is in the block")
        (is (search "https://openlibrary.org/works/OL123W" text) "with its original url")
        (is (search "id ol:OL123W" text) "and the id the library takes")
        (is (search "linked by 1 Hacker News story" text) "the story that links it is counted")
        (is (< (search "The Dispossessed" text) (search "Reasonable Doubts" text)) "and the linked work leads the unlinked one")
        (is (not (search "Not a work page" text)) "a doc that is not a work page is dropped")
        (is (equal "ursula le guin" (hackertube-param (call-to calls "openlibrary.org") "q")) "the query travels to Open Library")
        (is (equal "podcast" (hackertube-param (call-to calls "itunes.apple.com") "media")) "and Apple is asked for a podcast")
        (is (equal "20" (hackertube-param (call-to calls "hn.algolia.com") "hitsPerPage")) "a bounded page is asked for, never the whole index")))))

(deftest hackertube-cell-drops-a-record-that-names-none-of-the-query ()
  (with-hackertube ()
    (with-scripted-sources (calls (fixture-answer))
      (let ((text (hackertube-search-tool "ursula le guin" '("books" "audiobooks"))))
        (is (search "The Dispossessed" text) "the matching records stay")
        (is (not (search "Persuasion" text)) "a record that names none of the words is dropped")))))

(deftest hackertube-cell-answers-a-loose-record-rather-than-nothing ()
  (with-hackertube ()
    (with-scripted-sources (calls (fixture-answer))
      (let ((text (hackertube-search-tool "zzz" '("audiobooks"))))
        (is (search "2 results for \"zzz\"" text) "the block counts what it shows")
        (is (search "Persuasion" text) "and a record the words never name is still shown when nothing matches")))))

(deftest hackertube-cell-names-a-failed-source-and-keeps-the-others ()
  (with-hackertube ()
    (with-scripted-sources (calls (lambda (call)
                                     (if (search "openlibrary.org" (hackertube-http-url call))
                                         (values "{\"error\":\"nope\"}" 500)
                                         (funcall (fixture-answer) call))))
      (let ((text (hackertube-search-tool "persuasion" '("books" "audiobooks"))))
        (is (search "source Open Library failed" text) "the source that failed is named")
        (is (search "answered HTTP 500" text) "with what it answered")
        (is (search "Persuasion" text) "and the other source's record is still in the block")))))

;;; --- the library ----------------------------------------------------------------

(deftest hackertube-cell-keeps-lists-and-forgets-a-record ()
  (empty-library)
  (with-hackertube ()
    (with-scripted-sources (calls (fixture-answer))
      (hackertube-search-tool "ursula le guin" '("books"))
      (is (search "nothing saved yet" (hackertube-library-tool "list")) "the library starts empty")
      (is (search "saved The Dispossessed" (hackertube-library-tool "save" "ol:OL123W")) "a result line's id saves the record")
      (is (probe-file (nodecode-hackertube::library-path)) "the library is one file under the home")
      (is (search "id ol:OL123W" (hackertube-library-tool "list")) "the list answers the same line a search does")
      (is (search "forgot ol:OL123W" (hackertube-library-tool "forget" "ol:OL123W")) "forget takes it out")
      (is (search "nothing saved yet" (hackertube-library-tool "list")) "and the library is empty again"))))

(deftest hackertube-cell-save-of-an-unseen-id-is-a-repair ()
  (empty-library)
  (with-hackertube ()
    (let ((text (hackertube-library-tool "save" "ol:OL999W")))
      (is (search "no result with id ol:OL999W" text) "the id is named back")
      (is (search "search for it again" text) "and the repair with it"))))

;;; --- the keyed source, and the command ------------------------------------------

(deftest hackertube-cell-youtube-answers-only-with-a-configured-key ()
  (let ((name "NODECODE_HACKERTUBE_TEST_YOUTUBE_KEY"))
    (setf (uiop:getenv name) "test-key")
    (nlk:with-cleanup ((setf (uiop:getenv name) ""))
      (with-hackertube ("sources" #("youtube") "youtube_key_env" name)
        (with-scripted-sources (calls (fixture-answer))
          (let ((text (hackertube-search-tool "karpathy" '("videos"))))
            (is (search "[video] A talk about software" text) "the video is a record")
            (is (search "id yt:abc123" text) "with the id its line ends with")
            (is (equal "test-key" (hackertube-param (first calls) "key")) "the key travels in the request that needs it")))))))

(deftest hackertube-cell-the-command-answers-the-block-and-the-library ()
  (empty-library)
  (with-hackertube ()
    (with-scripted-sources (calls (fixture-answer))
      (let ((text (nodecode-hackertube::run-hackertube "persuasion" nil)))
        (is (search "hackertube: " text) "the command answers the block a search does")
        (is (search "Persuasion" text) "with the records in it"))
      (is (search "nothing saved yet" (nodecode-hackertube::run-hackertube "saved" nil)) "saved answers the library")
      (multiple-value-bind (text dialog) (nodecode-hackertube::run-hackertube "" nil)
        (is (search "focused media search" text) "bare, it names what it does")
        (is dialog "and offers a row to type the query in")))))
