;;;; cell-test.lisp --- the cell against a stubbed public AppView.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches the AppView and no
;;;; key is needed. What is proved is the cell's own wiring: the section (a
;;;; healthy start and three refused ones), the three tools through the
;;;; dispatch the model calls them by, the watch over a saved account, and
;;;; /bluesky.

(in-package #:nodecode.test)

;;; --- lifecycle and config ------------------------------------------------------------

(define-test-slice "bluesky-feed" "BLUESKY-FEED-CELL-" :start nodecode-bluesky-feed:start-cell)

(define-cell-lifecycle-tests "bluesky-feed"
  (:help :bluesky)
  (:command "bluesky")
  (:running
   (is (nle::find-tool "bluesky_profile") "the profile tool is registered")
   (is (nle::find-tool "bluesky_feed") "and the feed")
   (is (nle::find-tool "bluesky_watch") "and the watch")
   (is (= 3 (count-if (lambda (entry) (eql 0 (search "bluesky_" (first entry)))) nle::*tools*)) "three of them, and no others"))
  (:refused ("watch_pages" 0) ("max_chars" 10) ("timeout_seconds" 0))
  (:idle nodecode-bluesky-feed:bluesky-feed-error (nodecode-bluesky-feed:author-feed "bsky.app")))

;;; --- fixtures ------------------------------------------------------------------------

(defparameter *feed* '()
  "The rows the stub answers for getAuthorFeed, newest first.")

(defparameter *urls* '()
  "Every address the stub was asked for, newest first.")

(defparameter *status* 200
  "The status the stub answers for one request, when a test sets it low.")

(defun profile-body ()
  "One getProfile answer, avatar and bio included so a test can see them dropped."
  (nlk:encode-json-object
   (nlk:json-object "did" "did:plc:alice" "handle" "alice.test" "displayName" "Alice"
                     "avatar" "https://cdn.example/a.png" "description" "a bio"
                     "followersCount" 12 "followsCount" 7 "postsCount" 42)))

(defun post-json (uri &key (handle "alice.test") (did "did:plc:alice")
                          (text "hello") (kind nil))
  "One feed row carrying URI, by DID under HANDLE, with TEXT on it."
  (nlk:json-object
   "post" (nlk:json-object "uri" uri
                            "author" (nlk:json-object "did" did "handle" handle)
                            "record" (nlk:json-object "text" text
                                                       "createdAt" "2026-11-01T09:00:00Z"))
   "reason" (and kind (nlk:json-object "$type" kind))))

(defparameter *cursor* nil
  "The cursor the stub's answer offers; NIL means the feed ran out.")

(defun feed-body ()
  "One getAuthorFeed answer over *FEED*, offering *CURSOR*."
  (nlk:encode-json-object (nlk:json-object "feed" (coerce *feed* 'vector)
                                            "cursor" *cursor*)))

(defun stub-http (url)
  "What the stubbed AppView answers URL: the profile, the feed, or nothing."
  (push url *urls*)
  (cond ((search "getProfile" url) (values (profile-body) *status* nil nil))
        ((search "getAuthorFeed" url) (values (feed-body) *status* nil nil))
        (t (values "{\"error\":\"NotFound\"}" 404 nil nil))))

(defmacro with-appview (&body body)
  "A started cell, one stubbed AppView, and no saved watch either side."
  `(with-cell-stop ((bluesky-feed-start))
     (let ((path (nodecode-bluesky-feed::state-file)))
       (when (probe-file path) (ignore-errors (delete-file path)))
       (unwind-protect
            (let ((*urls* '()) (*feed* '()) (*cursor* nil) (*status* 200)
                  (nodecode-bluesky-feed::*http* (lambda (method url &rest options)
                            (declare (ignore method options))
                            (stub-http url))))
              ,@body)
         (when (probe-file path) (ignore-errors (delete-file path)))))))

(defun called (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "bluesky-call" name arguments)))

(defun slash (&rest words)
  "One /bluesky invocation, as the operand of a command answers it."
  (multiple-value-bind (text dialog)
      (cell-entry "nodecode-bluesky-feed" "bluesky" (format nil "~{~a~^ ~}" words))
    (declare (ignore dialog))
    text))

(defun saved ()
  "The saved watch, as the decoded object, or NIL."
  (let ((path (nodecode-bluesky-feed::state-file)))
    (if (probe-file path) (nlk:decode-json (uiop:read-file-string path)) nil)))

;;; --- the two reads ---------------------------------------------------------------------

(deftest bluesky-feed-cell-profile-answers-the-counts ()
  (with-appview
    (let ((answer (called "bluesky_profile" "{\"actor\": \"alice.test\"}")))
    (is (search "@alice.test" answer) "the handle is named")
    (is (search "followers 12, following 7, posts 42" answer) "the three counts, as the plugin read them")
    (is (null (search "cdn.example" answer)) "the avatar address is not returned")
    (is (null (search "a bio" answer)) "nor the bio"))))

(deftest bluesky-feed-cell-feed-lists-posts-and-marks-a-repost ()
  (with-appview
    (setf *feed* (list (post-json "at://app.bsky.feed.post/p1" :text "first words")
                      (post-json "at://app.bsky.feed.post/p2" :handle "bob.test" :did "did:plc:bob"
                                 :text "borrowed words"
                                 :kind "app.bsky.feed.defs#reasonRepost")))
  (let ((answer (called "bluesky_feed" "{\"actor\": \"alice.test\"}")))
    (is (search "first words" answer) "a post is its text")
    (is (search "@bob.test (repost)" answer) "a repost says so and keeps the original author")
    (is (search "at://app.bsky.feed.post/p1" answer) "every line carries the post's address")
    (is (search "filter=posts_no_replies" (first *urls*)) "replies are left out by the request itself")
    (is (search "limit=10" (first *urls*)) "and one page is asked for, as the plugin asked"))))

(deftest bluesky-feed-cell-a-failed-status-is-a-refusal-in-words ()
  (with-appview
    (setf *status* 503)
  (let ((answer (called "bluesky_profile" "{\"actor\": \"alice.test\"}")))
    (is (search "ERROR" answer) "the call fails rather than answering empties")
    (is (search "503" answer) "and names the status"))))

(deftest bluesky-feed-cell-a-handle-with-a-space-is-refused-before-a-request ()
  (with-appview
    (let ((answer (called "bluesky_feed" "{\"actor\": \"two words\"}")))
    (is (search "ERROR" answer) "the argument is refused")
    (is (null *urls*) "and nothing was fetched"))))

;;; --- the watch -------------------------------------------------------------------------

(deftest bluesky-feed-cell-watch-with-nothing-saved-says-so ()
  (with-appview
    (let ((answer (called "bluesky_watch" "{}")))
    (is (search "No account is watched" answer) "the model is told how to save one")
    (is (null *urls*) "and nothing was fetched"))))

(deftest bluesky-feed-cell-first-check-records-the-baseline ()
  (with-appview
    (setf *feed* (list (post-json "at://app.bsky.feed.post/old" :text "OLD BODY")))
  (is (search "Watching @alice.test" (slash "watch" "alice.test")) "the profile read confirms the handle")
  (is (equal "did:plc:alice" (nlk:json-value (saved) :text "did")) "the DID is saved, so a rename does not matter")
  (let ((answer (called "bluesky_watch" "{}")))
    (is (search "Baseline stored" answer) "the first check only records what is there")
    (is (null (search "OLD BODY" answer)) "and announces none of it"))))

(deftest bluesky-feed-cell-the-next-check-announces-what-is-new ()
  (with-appview
    (setf *feed* (list (post-json "at://app.bsky.feed.post/old" :text "OLD BODY")))
  (slash "watch" "alice.test")
  (called "bluesky_watch" "{}")
  (setf *feed* (list (post-json "at://app.bsky.feed.post/new" :text "just posted")
                      (post-json "at://app.bsky.feed.post/old" :text "OLD BODY")))
  (let ((answer (called "bluesky_watch" "{}")))
    (is (search "1 new post from @alice.test" answer) "one new post, counted")
    (is (search "just posted" answer) "with its text")
    (is (null (search "already there" answer)) "and not the one already seen"))
  (is (search "No new posts" (called "bluesky_watch" "{}")) "a second check right after has nothing to add")
  (let ((status (slash "status"))) (is (search "2 saved posts" status) "both are saved under the Nodecode home"))))

(deftest bluesky-feed-cell-a-check-that-is-all-new-posts-the-miss-note ()
  (with-appview
    (setf *feed* (list (post-json "at://app.bsky.feed.post/one" :text "one")))
  (slash "watch" "alice.test")
  (called "bluesky_watch" "{}")
  (setf *feed* (list (post-json "at://app.bsky.feed.post/two" :text "two")
                      (post-json "at://app.bsky.feed.post/three" :text "three")))
  (setf *cursor* "next-cursor")
  (let ((answer (called "bluesky_watch" "{}")))
    (is (search "may have been missed" answer) "a page with nothing seen on it says so"))))

(deftest bluesky-feed-cell-forget-drops-the-saved-account ()
  (with-appview
    (slash "watch" "alice.test")
  (is (probe-file (nodecode-bluesky-feed::state-file)) "the watch is saved")
  (is (search "No account is watched now" (slash "forget")) "forget says what it did")
  (is (null (nodecode-bluesky-feed::saved-state)) "and nothing is saved")))