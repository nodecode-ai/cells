;;;; cell-test.lisp --- lifecycle, the tool, the block, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not GitHub: the config
;;;; surface (a member of the wrong shape refuses the start), the tool through
;;;; the real dispatch seam against a stubbed API, the block a reference turns
;;;; into, the follow-up requests a pull request and its comments cost, and the
;;;; refusals an unreachable one gets.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "github-refs"
  (:config "max_chars" 6000)
  (:help :gh-ref)
  (:command "gh")
  (:running
   (is (nle::find-tool "github_reference") "github_reference is registered")
   (is (= 6000 (nodecode-github-refs::setting :max-chars)) "the section's max_chars is the one in force"))
  (:stopped
   (is (null (nle::find-tool "github_reference")) "stop unregisters the tool"))
  (:refused
   ("max_chars" 10)
   ("max_chars" "many")
   ("max_comments" -1))
  (:idle nodecode-github-refs:github-refs-error
   (nodecode-github-refs:reference "octo/tools#42")))

;;; --- the block a reference becomes --------------------------------------------------

(deftest github-refs-cell-an-issue-arrives-as-one-block (with-gh-refs ()
  (let ((text (gh-refs-call-tool "github_reference" "{\"reference\":\"octo/tools#42\"}")))
    (is (cl:search "GitHub issue octo/tools#42: Allow plugins to register context references" text) "the title it names")
    (is (cl:search "https://github.com/octo/tools/issues/42" text) "with its url")
    (is (cl:search "State: closed (completed)" text) "the state, and the reason it closed")
    (is (cl:search "opened by @pat on 2026-05-15" text) "who opened it and when")
    (is (cl:search "closed 2026-08-13" text) "when it closed")
    (is (cl:search "3 comments" text) "how many comments it has")
    (is (cl:search "Labels: type/feature, P3" text) "its labels")
    (is (cl:search "Assignees: @sam" text) "who is assigned")
    (is (cl:search "Milestone: Phase 1" text) "and its milestone")
    (is (cl:search "Attach a reference." text) "the description")
    (is (cl:search "--- @ann · 2026-05-16" text) "the comments, oldest first, by their author")
    (is (cl:search "Source: GitHub, read anonymously." text) "and the line saying where it came from")
    (is (cl:search "not instructions" text) "which tells a model how to read it"))
  (let ((call (first (last *calls*))))
    (is (equal :get (getf call :method)) "the first request is a plain GET")
    (is (equal "2022-11-28" (call-header call "x-github-api-version")) "asking for the version the original asked for")
    (is (cl:search "application/vnd.github+json" (call-header call "accept")) "and GitHub's JSON media type")
    (is (null (call-header call "authorization")) "with no credential: public repositories, read anonymously"))
  (is (= 2 (length *calls*)) "one request for the issue and one for its comments")))

(deftest github-refs-cell-html-comments-are-dropped-outside-a-fence (with-gh-refs ()
  (let ((text (gh-refs-call-tool "github_reference" "{\"reference\":\"octo/tools#42\"}")))
    (is (null (cl:search "a template hint" text)) "a template hint is not shown, as GitHub does not show it")
    (is (null (cl:search "hidden marker" text)) "and neither is a comment's own hidden marker")
    (is (cl:search "kept in a fence" text) "but a commented line inside a fenced block is kept"))))

;;; --- a pull request, and the requests it costs --------------------------------------

(deftest github-refs-cell-a-pull-request-adds-its-branches-and-size (with-gh-refs ()
  (let ((text (gh-refs-call-tool "github_reference" "{\"reference\":\"octo/tools#77\"}")))
    (is (cl:search "GitHub pull request octo/tools#77" text) "it reads as a pull request")
    (is (cl:search "State: merged 2026-08-13" text) "whose state is where it merged")
    (is (cl:search "Branch: octo:context-refs → octo:main · 3 commits, +120 −4 in 2 files" text) "with its branches and change size"))
  (is (= 1 (length (calls-matching "/pulls/77"))) "one request for the pull request's own details")
  (is (null (calls-matching "/comments")) "and none for comments it does not have")))

(deftest github-refs-cell-a-copied-url-reads-as-the-same-reference (with-gh-refs ()
  (let ((text (nodecode-github-refs:reference "https://github.com/octo/tools/pull/77#discussion_r1")))
    (is (cl:search "octo/tools#77" text) "a url with a fragment names the same reference"))
  (is (null (cl:search "resolved from" (nodecode-github-refs:reference "octo/tools#77"))) "and nothing moved, so nothing is said about it")))

(deftest github-refs-cell-a-renamed-repository-says-so (with-gh-refs ()
  (let ((text (nodecode-github-refs:reference "octo/tools#9")))
    (is (cl:search "resolved from octo/tools#9" text) "the block names the reference that was asked for")
    (is (cl:search "neworg/newrepo#9" text) "and where GitHub says it is now"))))

;;; --- the comments a reference carries --------------------------------------------

(deftest github-refs-cell-the-newest-comments-come-in-page-requests (with-gh-refs ("max_comments" 2)
  (let ((text (gh-refs-call-tool "github_reference" "{\"reference\":\"octo/tools#42\"}")))
    (is (cl:search "Comments (the last 2 of 3, oldest first):" text) "the heading counts what was left out")
    (is (cl:search "Shipped in 0.21." text) "the newest comment is the one that survives"))
  (let ((calls (calls-matching "/comments")))
    (is (= 2 (length calls)) "two pages were asked for, not the whole thread")
    (is (every (lambda (call) (cl:search "per_page=2" (getf call :url))) calls) "each page sized to max_comments")
    (is (plusp (length (remove-if-not (lambda (call) (cl:search "page=1" (getf call :url))) calls))) "the page before the last was asked for")
    (is (plusp (length (remove-if-not (lambda (call) (cl:search "page=2" (getf call :url))) calls))) "and the last page too"))))

(deftest github-refs-cell-no-comments-asked-for-is-no-comments-request (with-gh-refs ("max_comments" 0)
  (let ((text (gh-refs-call-tool "github_reference" "{\"reference\":\"octo/tools#42\"}")))
    (is (null (cl:search "Comments (" text)) "no comment section arrives"))
  (is (null (calls-matching "/comments")) "and no comment page was asked for")))

;;; --- the description bound ---------------------------------------------------------

(deftest github-refs-cell-a-long-description-is-cut-at-the-bound (with-gh-refs ("max_chars" 200)
  (setf *answer*
        (lambda (call)
          (declare (ignore call))
          (values (format nil "{\"number\":5,\"title\":\"Long\",\"html_url\":\"https://github.com/octo/tools/issues/5\",\"state\":\"open\",\"comments\":0,\"user\":{\"login\":\"eve\"},\"created_at\":\"2026-01-01T00:00:00Z\",\"body\":\"~{~a~^ ~}\"}"
                          (loop repeat 200 collect "word"))
                  200 nil)))
  (let ((text (nodecode-github-refs:reference "octo/tools#5")))
    (is (cl:search "(description truncated to 200 characters)" text) "the block says how long the description was cut to")
    (is (< (length text) 800) "so the whole block stays small"))))

;;; --- refusals -------------------------------------------------------------------

(deftest github-refs-cell-a-number-that-does-not-exist-refuses (with-gh-refs ()
  (let ((condition (signals-error nodecode-github-refs:github-refs-error
                                  (nodecode-github-refs:reference "octo/tools#404"))))
    (is (cl:search "no issue or pull request octo/tools#404" (princ-to-string condition)) "the refusal names the reference")
    (is (cl:search "not exist, or the repository is private" (princ-to-string condition)) "and the two reasons GitHub leaves"))))

(deftest github-refs-cell-a-reference-that-does-not-parse-never-reaches-the-wire (with-gh-refs ()
  (let ((condition (signals-error nodecode-github-refs:github-refs-error
                                  (nodecode-github-refs:reference "just some words"))))
    (is (cl:search "owner/repo#123" (princ-to-string condition)) "the refusal shows how a reference is written"))
  (is (null *calls*) "and nothing was dialled")))

(deftest github-refs-cell-a-used-up-limit-says-when-it-resets (with-gh-refs ()
  (setf *answer*
        (lambda (call)
          (declare (ignore call))
          (let ((headers (make-hash-table :test #'equal)))
            (setf (gethash "x-ratelimit-remaining" headers) "0"
                  (gethash "x-ratelimit-reset" headers) "1800000000")
            (values "{\"message\":\"API rate limit exceeded\"}" 403 headers))))
  (let ((condition (signals-error nodecode-github-refs:github-refs-error
                                  (nodecode-github-refs:reference "octo/tools#42"))))
    (is (cl:search "60 requests an hour" (princ-to-string condition)) "the refusal says which limit")
    (is (cl:search "resets at" (princ-to-string condition)) "and when it resets, to the minute"))))

(deftest github-refs-cell-a-call-with-no-reference-is-refused (with-gh-refs ()
  (let ((condition (signals-error nlk:contract-refusal
                                  (nlk:contract-decode (nlk:contract 'nodecode-github-refs::github-reference-request)
                                                       (nlk:json-object)))))
    (is (cl:search "reference" (princ-to-string condition)) "the refusal names the member a call must carry"))
  (is (null *calls*) "and nothing was dialled")))

;;; --- the operator's door -----------------------------------------------------------

(deftest github-refs-cell-the-command-reads-a-reference (with-gh-refs ()
  (let ((text (cell-entry "nodecode-github-refs" "gh" "octo/tools#42" "gh-cell-1")))
    (is (cl:search "State: closed (completed)" text) "/gh reads the reference"))
  (let ((bare (cell-entry "nodecode-github-refs" "gh" "" "gh-cell-1")))
    (is (cl:search "github_reference" bare) "with no reference it names what the cell gives")
    (is (cl:search "60 requests an hour" bare) "and the limit an anonymous read has")))
)
