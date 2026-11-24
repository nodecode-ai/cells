;;;; cell-test.lisp --- the daily edition against canned query feeds.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one network seam, HTTP-GET, is stubbed through *HTTP* with small RSS
;;;; documents: nothing here touches the network or a news host. What is proved
;;;; is what the plugin gave a reader -- interests into a front page, one story
;;;; once with every publisher credited, consensus leading, an earlier day's
;;;; stories withheld, seven back issues kept -- plus the tool and
;;;; /daily-edition answering, and the section refusing when it names no
;;;; interest.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------------

(define-test-slice "daily-edition" "DAILY-EDITION-CELL-" :start nodecode-daily-edition:start-cell)

(define-cell-lifecycle-tests "daily-edition"
  (:config "interests" #("fusion energy"))
  (:help :daily-edition)
  (:command "daily-edition")
  (:running (is (equal '("fusion energy") (nodecode-daily-edition::setting :interests)) "the section's interest is the paper's")
            (is (= 20 (nodecode-daily-edition::setting :max-stories)) "twenty stories a page by default")
            (is (= 12 (nodecode-daily-edition::setting :timeout-seconds)) "twelve seconds a fetch by default"))
  (:refused ("interests" #()) ("timeout_seconds" 0))
  (:idle nodecode-daily-edition:daily-edition-error (nodecode-daily-edition::do-action "print")))

;;; --- fixtures ---------------------------------------------------------------------------

(defparameter +feed-a+
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rss version=\"2.0\"><channel><title>fusion energy</title>
<item><title>Council approves the fusion energy pilot plant - Alpha Post</title>
<link>https://alpha.example/fusion-plant?utm_source=rss</link>
<description>&lt;p&gt;The council approved the pilot plant on Tuesday.&lt;/p&gt;</description>
<pubDate>Mon, 02 Nov 2026 10:00:00 GMT</pubDate>
<source url=\"https://alpha.example\">Alpha Post</source></item>
<item><title>Harbour dredging delayed again - Alpha Post</title>
<link>https://alpha.example/harbour</link>
<description>Port authority postpones the work.</description>
<pubDate>Sun, 01 Nov 2026 08:00:00 GMT</pubDate>
<source url=\"https://alpha.example\">Alpha Post</source></item>
</channel></rss>")

(defparameter +feed-b+
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rss version=\"2.0\"><channel><title>housing</title>
<item><title>Council approves fusion energy pilot plant - Beta Wire</title>
<link>https://beta.example/2026/11/fusion-pilot</link>
<description>The plant won approval on Tuesday evening.</description>
<pubDate>Mon, 02 Nov 2026 12:00:00 GMT</pubDate>
<source url=\"https://beta.example\">Beta Wire</source></item>
</channel></rss>")

(defparameter +feed-c+
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rss version=\"2.0\"><channel><title>ferries</title>
<item><title>Ferry service resumes after storm - Gamma Daily</title>
<link>https://gamma.example/ferry</link>
<description>The crossing reopened this morning.</description>
<pubDate>Tue, 03 Nov 2026 06:00:00 GMT</pubDate>
<source url=\"https://gamma.example\">Gamma Daily</source></item>
</channel></rss>")

(defun fixture-provider (&rest feeds) "An *HTTP* stub answering FEEDS in order, and the last one again after that."
  (let ((rest feeds))
    (lambda (url)
      (declare (ignore url))
      (values (or (pop rest) (car (last feeds))) 200))))

(defun prompt-day (&optional (universal (encode-universal-time 0 0 9 2 11 2026 0))) "A fixed universal time, so a date label is proved and not the clock's."
  (setf nodecode-daily-edition::*now* universal))

(defmacro with-paper ((&rest config) &body body) "A started daily-edition cell on CONFIG with an empty archive."
  `(with-cell-stop ((daily-edition-start ,@config))
     (nodecode-daily-edition::reset-state)
     ,@body))

;;; --- one story once ----------------------------------------------------------------------

(deftest daily-edition-cell-a-cross-posted-story-is-one-story ()
  (with-paper ("interests" #("fusion energy" "housing"))
    (prompt-day)
    (let ((nodecode-daily-edition::*http* (fixture-provider +feed-a+ +feed-b+)))
      (multiple-value-bind (edition standfirst errors)
          (nodecode-daily-edition::compose-edition)
        (declare (ignore standfirst))
        (is (null errors) "both interests were read")
        (is (= 2 (getf edition :story-count)) "the two mastheads' account of one vote is one story, plus the harbour item")
        (is (= 1 (getf edition :cross-posted)) "one story is cross-posted")
        (let ((lead (getf edition :lead)))
          (is (search "fusion energy pilot plant" (getf lead :title)) "the story two mastheads carried leads")
          (is (equal '("Alpha Post" "Beta Wire") (nodecode-daily-edition::story-line lead)) "and it credits both publishers"))))))

(deftest daily-edition-cell-a-page-is-rendered-and-filed ()
  (with-paper ("interests" #("fusion energy"))
    (prompt-day)
    (let ((nodecode-daily-edition::*http* (fixture-provider +feed-a+ +feed-b+)))
      (let ((page (nodecode-daily-edition::do-action "print")))
        (is (search "DAILY EDITION — No. 1 — Monday, 2 November 2026" page) "the masthead is dated and numbered")
        (is (search "LEAD: Council approves the fusion energy pilot plant" page) "the lead is printed")
        (is (search "https://alpha.example/harbour" page) "with the other story's address")
        (is (not (search "utm_source" page)) "a tracking member is not printed"))
      (is (= 1 (length (nodecode-daily-edition::archive))) "the edition is filed as a back issue")
      (is (= 1 (getf (first (nodecode-daily-edition::archive)) :number)) "under its number")
      (is (search "No. 1" (nodecode-daily-edition::do-action "archive")) "and the archive lists it"))))

(deftest daily-edition-cell-an-earlier-days-stories-are-withheld ()
  (with-paper ("interests" #("fusion energy" "ferries"))
    (let ((nodecode-daily-edition::*http* (fixture-provider +feed-a+)))
      (prompt-day (encode-universal-time 0 0 9 1 11 2026 0))
      (nodecode-daily-edition::do-action "print")
      (is (= 1 (length (nodecode-daily-edition::archive))) "yesterday's edition is filed")
      (setf nodecode-daily-edition::*http* (fixture-provider +feed-a+ +feed-c+))
      (prompt-day (encode-universal-time 0 0 9 2 11 2026 0))
      (let ((today (nodecode-daily-edition::do-action "print")))
        (is (search "DAILY EDITION — No. 2" today) "today's edition is the second")
        (is (search "2 withheld" today) "and says what it withheld")
        (is (not (search "LEAD: Council approves" today)) "a story yesterday printed is not printed again")
        (is (search "Ferry service resumes" today) "while a story it did not print still runs"))
      (is (= 2 (length (nodecode-daily-edition::archive))) "two back issues are kept"))))

(deftest daily-edition-cell-seven-back-issues-are-kept ()
  (with-paper ("interests" #("fusion energy"))
    (dotimes (day 9)
      (nodecode-daily-edition::record-edition
       (nodecode-daily-edition::build-edition
        :sources (list (list :source "fusion energy"
                             :items (list (list :title (format nil "Day ~d" day)
                                                :url (format nil "https://example.com/~d" day)
                                                :at 0))))
        :now (encode-universal-time 0 0 9 (1+ day) 10 2026 0))))
    (is (= 7 (length (nodecode-daily-edition::archive))) "seven issues are the shelf")
    (is (equal "2026-10-09" (getf (first (nodecode-daily-edition::archive)) :iso-date)) "the newest issue is first")))

;;; --- the boundaries ------------------------------------------------------------------------

(deftest daily-edition-cell-the-tool-answers-through-its-contract ()
  (with-paper ("interests" #("fusion energy"))
    (prompt-day)
    (let ((nodecode-daily-edition::*http* (fixture-provider +feed-a+ +feed-b+)))
      (is-present (entry (nle::find-tool "daily_edition")) "the daily_edition tool is registered"
        (let* ((contract (second entry))
               (call (nlk:contract-decode contract (nlk:make-json-object "action" "print")))
               (page (funcall (third entry) call)))
          (is (search "DAILY EDITION" page) "print answers the front page")
          (is (search "2 stories" page) "with its count")
          (is (signals-error nlk:contract-refusal
                (nlk:contract-decode contract (nlk:make-json-object "action" "style"))) "an action outside the declared set is refused")))
      (let* ((entry (nle::find-tool "daily_edition"))
             (call (nlk:contract-decode (second entry) (nlk:make-json-object "action" "sources"))))
        (is (search "fusion energy" (funcall (third entry) call)) "sources names the interests the paper is compiled from")))))

(deftest daily-edition-cell-the-slash-command-prints-the-same-paper ()
  (with-paper ("interests" #("fusion energy"))
    (prompt-day)
    (let ((nodecode-daily-edition::*http* (fixture-provider +feed-a+ +feed-b+)))
      (is (search "DAILY EDITION — No. 1" (cell-entry "nodecode-daily-edition" "daily-edition" "")) "/daily-edition prints today's edition")
      (is (search "interest" (string-downcase (cell-entry "nodecode-daily-edition" "daily-edition" "sources"))) "/daily-edition sources says what it draws on")
      (is (search "No. 1" (cell-entry "nodecode-daily-edition" "daily-edition" "archive")) "/daily-edition archive lists the back issue"))))

(deftest daily-edition-cell-a-failing-interest-is-reported-not-fatal ()
  (with-paper ("interests" #("fusion energy" "housing"))
    (prompt-day)
    (let ((nodecode-daily-edition::*http*
            (lambda (url)
              (if (search "housing" url)
                  (values "gone" 503)
                  (values +feed-a+ 200)))))
      (let ((page (nodecode-daily-edition::do-action "print")))
        (is (search "Council approves" page) "the interest that answered is printed")
        (is (search "Sources that could not be read" page) "the one that failed is named")
        (is (search "housing" page) "with the interest that failed")))))
