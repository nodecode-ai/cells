;;;; cell-test.lisp --- the rss cell against a canned feed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one network seam, HTTP-GET, is stubbed through *HTTP* with a small
;;;; RSS 2.0 document and an Atom one: nothing here touches the network or a
;;;; feed host. What is proved is what the plugin gave a user -- subscribe,
;;;; fetch, parse, unread, read, find, mark, star, mute, digest, and the
;;;; library surviving a restart -- plus the tool and /rss answering, and the
;;;; section refusing when it names no feed.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-test-slice "rss" "RSS-CELL-" :start nodecode-rss:start-cell)

(define-cell-lifecycle-tests "rss"
  (:config "feeds" "https://example.com/feed.xml")
  (:help :rss)
  (:command "rss")
  (:running (is (= 15 (nodecode-rss::setting :timeout-seconds)) "fifteen seconds a fetch by default")
            (is (= 25 (nodecode-rss::setting :max-entries)) "twenty-five entries a feed by default")
            (is (= 1 (length (nodecode-rss::feeds))) "the section's feed is subscribed at start")
            (is (equal "https://example.com/feed.xml" (getf (first (nodecode-rss::feeds)) :url)) "and it is the url the section named"))
  (:refused ("feeds" "not-a-url") ("feeds" "") ("timeout_seconds" 0))
  (:idle nodecode-rss:rss-error (nodecode-rss::do-action "unread")))

;;; --- fixtures ----------------------------------------------------------------------

(defparameter +rss-feed+
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rss version=\"2.0\"><channel><title>Example Notes</title><link>https://example.com/</link>
<description>notes to finish</description>
<item><title>First &amp; best</title><link>https://example.com/one</link>
<description>&lt;p&gt;All about &lt;b&gt;one&lt;/b&gt;.&lt;/p&gt;</description>
<pubDate>Mon, 02 Nov 2026 10:00:00 GMT</pubDate></item>
<item><title>Quietly second</title><link>https://example.com/two</link>
<description><![CDATA[<p>Two, quietly.</p>]]></description>
<pubDate>Sun, 01 Nov 2026 10:00:00 GMT</pubDate></item>
</channel></rss>")

(defparameter +rss-atom+
  "<feed xmlns=\"http://www.w3.org/2005/Atom\"><title>Atom Example</title>
<entry><title>Tenth</title><link rel=\"alternate\" href=\"https://atom.example/x\"/>
<summary>A tenth story.</summary><updated>2026-11-03T09:00:00Z</updated></entry>
</feed>")

(defmacro with-feed ((&rest config) &body body)
  "A started rss cell on CONFIG, its library emptied, one canned feed answering."
  `(with-cell-stop ((rss-start ,@config))
     (nodecode-rss::reset-library)
     (let ((nodecode-rss::*http* (lambda (url)
                                   (declare (ignore url))
                                   (values +rss-feed+ 200))))
       ,@body)))

;;; --- the library --------------------------------------------------------------------

(deftest rss-cell-a-feed-is-parsed-and-what-is-new-comes-back ()
  (with-feed ("feeds" "https://example.com/feed.xml")
    (let ((refresh (nodecode-rss::do-action "refresh")))
      (is (not (search "failed" refresh)) "the fetch did not fail")
      (is (search "2 new" refresh) "both entries of a first fetch are new")
      (is (search "Example Notes" refresh) "and the feed keeps the title it answered with"))
    (let ((unread (nodecode-rss::do-action "unread")))
      (is (search "First & best" unread) "the title is read back, its reference spelled")
      (is (search "Quietly second" unread) "and the second entry with it")
      (let ((first-id (getf (first (nodecode-rss::articles)) :id)))
        (is (search "https://example.com/one"
                    (nodecode-rss::do-action "read" :id first-id)) "an article answers with its url and its summary")
        (is (search "All about one." (nodecode-rss::do-action "read" :id first-id)) "and the markup of its description is stripped")))
    (is (search "0 new" (nodecode-rss::do-action "refresh")) "a second fetch adds nothing")
    (is (= 2 (length (nodecode-rss::articles))) "and the library still holds two articles")))

(deftest rss-cell-an-atom-entry-is-the-same-story ()
  (with-cell-stop ((rss-start "feeds" "https://atom.example/feed"))
    (nodecode-rss::reset-library)
    (let ((nodecode-rss::*http* (lambda (url)
                                  (declare (ignore url))
                                  (values +rss-atom+ 200))))
      (is (search "1 new" (nodecode-rss::do-action "refresh")) "the entry is new")
      (is (search "Tenth" (nodecode-rss::do-action "unread")) "an Atom entry lists like an RSS item")
      (is (search "atom.example/x" (nodecode-rss::do-action "read" :id (getf (first (nodecode-rss::articles)) :id))) "its link href is the article's url"))))

(deftest rss-cell-unread-leaves-out-what-is-read-or-muted ()
  (with-feed ("feeds" "https://example.com/feed.xml")
    (nodecode-rss::do-action "refresh")
    (let ((first-id (getf (first (nodecode-rss::articles)) :id)))
      (nodecode-rss::do-action "mark" :id first-id)
      (is (not (search (getf (first (nodecode-rss::articles)) :title)
                       (nodecode-rss::do-action "unread"))) "a read article leaves the unread list"))
    (nodecode-rss::do-action "mute" :text "quietly")
    (is (search "quietly" (nodecode-rss::do-action "mutes")) "the mute is listed")
    (is (search "quietly" (nodecode-rss::do-action "find" :text "quietly")) "a muted article is still findable")
    (is (equal "Nothing unread." (nodecode-rss::do-action "unread")) "and nothing else is unread")
    (is (search "No longer muting" (nodecode-rss::do-action "unmute" :text "quietly")) "unmuting says so")
    (is (search "was not muted" (nodecode-rss::do-action "unmute" :text "quietly")) "and a second unmute says it was not muted")))

(deftest rss-cell-a-starred-article-survives-a-restart ()
  (let ((starred nil))
    (with-cell-stop ((rss-start "feeds" "https://example.com/feed.xml"))
      (nodecode-rss::reset-library)
      (let ((nodecode-rss::*http* (lambda (url)
                                    (declare (ignore url))
                                    (values +rss-feed+ 200))))
        (nodecode-rss::do-action "refresh"))
      (setf starred (getf (second (nodecode-rss::articles)) :id))
      (is (search "starred" (nodecode-rss::do-action "star" :id starred)) "an article is starred")
      (funcall stop)
      (setf stop nil))
    (with-cell-stop ((rss-start "feeds" "https://example.com/feed.xml"))
      (is (= 1 (length (nodecode-rss::feeds))) "the subscription is back after a restart")
      (let ((again (find starred (nodecode-rss::articles)
                         :key (lambda (article) (getf article :id)) :test #'equal)))
        (is again "and so is the article it saved")
        (is (and again (getf again :star)) "still starred")))))

(deftest rss-cell-a-digest-says-what-is-unread-per-feed ()
  (with-feed ("feeds" "https://example.com/feed.xml")
    (nodecode-rss::do-action "refresh")
    (let ((digest (nodecode-rss::do-action "digest")))
      (is (search "Example Notes" digest) "the feed is named")
      (is (search "2 unread" digest) "with its unread count")
      (is (search "First & best" digest) "and its headlines, for the model to summarize"))))

;;; --- the tool, /rss, the section -----------------------------------------------------

(deftest rss-cell-the-tool-answers-through-its-contract ()
  (with-feed ("feeds" "https://example.com/feed.xml")
    (is-present (entry (nle::find-tool "rss"))
      "the rss tool is registered"
      (let* ((contract (second entry))
             (call (nlk:contract-decode contract (nlk:make-json-object "action" "list")))
             (answer (funcall (third entry) call)))
        (is (search "subscription" answer) "list answers with the subscriptions")
        (is (search "example.com/feed.xml" answer) "and their urls")))
    (let* ((entry (nle::find-tool "rss"))
           (call (nlk:contract-decode (second entry) (nlk:make-json-object "action" "mute"))))
      (is (search "ERROR" (funcall (third entry) call)) "an action missing what it needs answers an error, not a broken turn"))
    (is (signals-error nlk:contract-refusal
          (nlk:contract-decode (second (nle::find-tool "rss"))
                               (nlk:make-json-object "action" "not-an-action"))) "an action outside the declared set is refused")))

(deftest rss-cell-the-slash-command-works-the-same-library ()
  (with-feed ("feeds" "https://example.com/feed.xml")
    (is (search "2 new" (cell-entry "nodecode-rss" "rss" "refresh")) "/rss refresh fetches")
    (is (search "First & best" (cell-entry "nodecode-rss" "rss" "unread")) "/rss unread lists")
    (is (search "2 match" (cell-entry "nodecode-rss" "rss" "find example.com")) "/rss find searches the library")))

(deftest rss-cell-a-section-with-no-feed-refuses ()
  (is (signals-error nlk:config-refusal (funcall (rss-start))) "feeds is required: a section without it refuses at start")
  (is (signals-error nlk:config-refusal (funcall (rss-start "feeds" "ftp://example.com/feed"))) "and a feed that is not an http(s) address refuses in words"))
