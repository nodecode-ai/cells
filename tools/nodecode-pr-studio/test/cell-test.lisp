;;;; cell-test.lisp --- PR Studio against a stubbed gh.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the lifecycle every cell owns; that the inbox makes
;;;; three searches, groups them and shows a pull request once; that one read
;;;; answers the header, the failing checks, the description and the
;;;; conversation; that a diff is summarised a file a line and cut when long;
;;;; that the review threads carry the comment id a reply needs; that a
;;;; comment, a reply and a review are posted with the body and the lines the
;;;; call named; that read_only and a call without its required members are
;;;; refused before gh runs; and that /prs answers the inbox.
;;;; Nothing here needs gh installed, signed in, or runnable at all.

(in-package #:nodecode.test)

(define-test-slice "pr-studio" "PR-STUDIO-CELL-" :start nodecode-pr-studio:start-cell)

;;; --- the stubbed process seam --------------------------------------------------------

(defvar *answers* nil
  "Canned gh answers, in order of the search: (SUBSTRING OUTPUT EXIT-CODE).")
(defvar *calls* nil
  "Every command the cell ran, newest first: one line a call.")
(defvar *files* nil
  "The text each --input or --body-file argument held, newest first.")

(defun stub-answer (program args)
  (let ((line (format nil "~a ~{~a~^ ~}" program args)))
    (push line *calls*)
    (loop for (option argument) on args by #'cdr
          when (and (stringp option)
                    (member option '("--input" "--body-file") :test #'string=)
                    (stringp argument))
            do (push (ignore-errors (uiop:read-file-string argument)) *files*))
    (dolist (entry *answers* (values "" 0))
      (when (search (first entry) line)
        (return-from stub-answer (values (second entry) (third entry)))))))

(defmacro with-gh-answers (answers &body body)
  `(let ((*answers* ,answers)
         (*calls* '())
         (*files* '())
         (nodecode-pr-studio::*run*
           (lambda (dir program args timeout)
             (declare (ignore dir timeout))
             (stub-answer program args))))
     ,@body))

(defmacro with-gh-stub ((&rest answers) &body body)
  "ANSWERS: (SUBSTRING OUTPUT EXIT-CODE) each, in order of the search."
  `(with-gh-answers (list ,@(mapcar (lambda (answer) `(list ,@answer)) answers)) ,@body))

(defun some-call (needle)
  (some (lambda (line) (and (search needle line) t)) *calls*))

(defparameter *pull*
  "{\"number\":12,\"title\":\"Fix the retry loop\",\"state\":\"OPEN\",\"isDraft\":false,
    \"author\":{\"login\":\"dev\"},\"baseRefName\":\"main\",\"headRefName\":\"fix-retry\",
    \"headRefOid\":\"abc1234def5678\",\"additions\":120,\"deletions\":8,\"changedFiles\":4,
    \"body\":\"Retries gave up too early.\",\"url\":\"https://github.com/o/r/pull/12\",
    \"reviewDecision\":\"REVIEW_REQUIRED\",\"mergeable\":\"MERGEABLE\",\"mergeStateStatus\":\"CLEAN\",
    \"labels\":[{\"name\":\"bug\"}],
    \"statusCheckRollup\":[{\"__typename\":\"CheckRun\",\"name\":\"lint\",\"conclusion\":\"SUCCESS\"},
      {\"__typename\":\"CheckRun\",\"name\":\"build\",\"conclusion\":\"FAILURE\"}],
    \"commits\":[{\"oid\":\"abc1234def5678\",\"messageHeadline\":\"Retry four times\",
      \"committedDate\":\"2026-01-02T03:04:05Z\"}],
    \"comments\":[{\"author\":{\"login\":\"rev\"},\"body\":\"Why four and not three?\",
      \"createdAt\":\"2026-01-03T03:04:05Z\"}],
    \"reviews\":[{\"author\":{\"login\":\"rev\"},\"state\":\"CHANGES_REQUESTED\",
      \"body\":\"One question.\",\"submittedAt\":\"2026-01-04T03:04:05Z\"}]}"
  "One pull request, as gh pr view prints it.")

(defparameter *search-row*
  "{\"number\":7,\"title\":\"Add a cache\",\"isDraft\":false,\"state\":\"open\",
    \"url\":\"https://github.com/o/r/pull/7\",\"author\":{\"login\":\"dev\"},
    \"repository\":{\"nameWithOwner\":\"o/r\"},\"reviewDecision\":\"REVIEW_REQUIRED\",
    \"updatedAt\":\"2026-01-05T03:04:05Z\",\"commentsCount\":2}"
  "One gh search prs row.")

(defparameter *threads*
  "{\"data\":{\"repository\":{\"pullRequest\":{\"reviewThreads\":{\"nodes\":[
     {\"id\":\"THREAD_1\",\"isResolved\":false,\"isOutdated\":false,\"path\":\"src/app.lisp\",
      \"line\":42,\"diffSide\":\"RIGHT\",\"comments\":{\"nodes\":[
        {\"id\":\"C_1\",\"databaseId\":987654,\"body\":\"This can be nil.\",
         \"createdAt\":\"2026-01-06T03:04:05Z\",\"author\":{\"login\":\"rev\"}}]}},
     {\"id\":\"THREAD_2\",\"isResolved\":true,\"isOutdated\":false,\"path\":\"src/b.lisp\",
      \"line\":3,\"diffSide\":\"RIGHT\",\"comments\":{\"nodes\":[
        {\"id\":\"C_2\",\"databaseId\":123456,\"body\":\"Done.\",
         \"createdAt\":\"2026-01-06T04:04:05Z\",\"author\":{\"login\":\"dev\"}}]}}]}}}}}"
  "Two review threads, one of them resolved, as gh api graphql prints them.")

(defparameter *patch*
  "diff --git a/src/app.lisp b/src/app.lisp
--- a/src/app.lisp
+++ b/src/app.lisp
@@ -1,3 +1,4 @@
 line one
-line two
+line two, changed
+line three
"
  "One changed file: plus two lines, minus one.")

(defun inbox-answers (&rest more)
  (append more
          (list (list "--review-requested=@me" (format nil "[~a]" *search-row*) 0)
                (list "--author=@me" (format nil "[~a]" *search-row*) 0)
                (list "--involves=@me" (format nil "[~a]" *search-row*) 0))))

(defun pr-object (pairs)
  "A JSON object from alternating key/value PAIRS."
  (apply #'nlk:make-json-object pairs))

(defun pr-request (name &rest pairs)
  "The contract NAME's record, decoded from PAIRS."
  (nlk:contract-decode (nlk:contract name) (pr-object pairs)))

(defun inbox-request (&rest pairs)
  (apply #'pr-request 'nodecode-pr-studio::pr-inbox-request pairs))

(defun show-request (&rest pairs)
  (apply #'pr-request 'nodecode-pr-studio::pr-show-request pairs))

(defun diff-request (&rest pairs)
  (apply #'pr-request 'nodecode-pr-studio::pr-diff-request pairs))

(defun threads-request (&rest pairs)
  (apply #'pr-request 'nodecode-pr-studio::pr-threads-request pairs))

(defun comment-request (&rest pairs)
  (apply #'pr-request 'nodecode-pr-studio::pr-comment-request pairs))

(defun review-request (&rest pairs)
  (apply #'pr-request 'nodecode-pr-studio::pr-review-request pairs))

;;; --- lifecycle ----------------------------------------------------------------------

(define-cell-lifecycle-tests "pr-studio"
  (:config "read_only" t)
  (:running (is (= 10 (nodecode-pr-studio::setting :limit)) "ten rows a group by default")
            (is (equal "gh" (nodecode-pr-studio::setting :gh)) "the gh on PATH is the one it runs")
            (is (eq t (nodecode-pr-studio::setting :read-only)) "read_only is on when the section says so"))
  (:refused ("limit" 0) ("max_chars" 500) ("timeout_seconds" 0) ("events" 0))
  (:help :pr-studio)
  (:command "prs")
  (:idle nodecode-pr-studio:pr-studio-error
         (nodecode-pr-studio:inbox) (nodecode-pr-studio:show "o/r" 1)))

;;; --- the inbox ----------------------------------------------------------------------

(deftest pr-studio-cell-inbox-groups-three-searches ()
  (with-cell-stop ((pr-studio-start))
    (with-gh-answers (inbox-answers)
      (let ((text (nodecode-pr-studio::inbox-tool (inbox-request))))
        (is (search "Needs your review (1)" text))
        (is (search "Your pull requests (0)" text) "a PR shows in the first group that holds it")
        (is (search "Involves you (0)" text))
        (is (search "o/r#7" text) "the row names the repository and the number")
        (is (search "@dev" text) "and its author")
        (is (search "Add a cache" text) "and its title")
        (is (search "review_required" text) "and the review decision")
        (is (= 3 (length *calls*)) "three searches, one a group")
        (is (some-call "search prs"))))))

(deftest pr-studio-cell-inbox-can-name-one-repository ()
  (with-cell-stop ((pr-studio-start))
    (with-gh-answers (inbox-answers)
      (nodecode-pr-studio::inbox-tool (inbox-request "repo" "o/r"))
      (is (every (lambda (line) (search "--repo=o/r" line)) *calls*) "every search is scoped to the repository named"))
    (with-gh-stub ()
      (is (search "not a repository"
                  (refusal-text nodecode-pr-studio:pr-studio-error
                                (nodecode-pr-studio:inbox :repo "not-a-repo"))) "a repository that is not owner/name is refused")
      (is (null *calls*) "before any search runs"))))

;;; --- one pull request ----------------------------------------------------------------

(deftest pr-studio-cell-show-reads-one-pr ()
  (with-cell-stop ((pr-studio-start))
    (with-gh-stub (("pr view 12" *pull* 0))
      (let ((text (nodecode-pr-studio::show-tool (show-request "repo" "o/r" "number" 12))))
        (is (search "o/r#12  Fix the retry loop" text))
        (is (search "+120 -8 in 4 files" text))
        (is (search "fix-retry -> main" text))
        (is (search "review REVIEW_REQUIRED" text))
        (is (search "build (failure)" text) "the failing check is named")
        (is (null (search "lint (failure)" text)) "a passing one is not")
        (is (search "labels: bug" text))
        (is (search "Retries gave up too early." text) "the description is in it")
        (is (search "Why four and not three?" text) "and the conversation")
        (is (search "CHANGES_REQUESTED" text) "a review's state is named")
        (is (some-call "pr view 12 -R o/r"))))))

(deftest pr-studio-cell-show-without-repo-is-refused ()
  (with-cell-stop ((pr-studio-start))
    (with-gh-stub ()
      (is (signals-error nlk:contract-refusal
                         (nodecode-pr-studio::show-tool (show-request "number" 12))) "a call missing a required member is refused at the boundary")
      (is (signals-error nlk:contract-refusal
                         (nodecode-pr-studio::show-tool (show-request "repo" "o/r"))) "so is one with no number")
      (is (null *calls*) "and gh never runs"))))

;;; --- the diff -------------------------------------------------------------------------

(deftest pr-studio-cell-diff-summarises-and-shows-the-patch ()
  (with-cell-stop ((pr-studio-start))
    (with-gh-stub (("pr diff 12" *patch* 0))
      (let ((text (nodecode-pr-studio::diff-tool (diff-request "repo" "o/r" "number" 12))))
        (is (search "+2 -1  src/app.lisp" text) "churn is counted from the patch itself")
        (is (search "+line two, changed" text) "and the patch follows")))))

(deftest pr-studio-cell-diff-is-cut-with-a-note ()
  (with-cell-stop ((pr-studio-start "max_chars" 2000))
    (with-gh-stub (("pr diff 12"
                    (with-output-to-string (out)
                      (write-string *patch* out)
                      (dotimes (i 400) (format out "+a line that makes the patch long ~d~%" i)))
                    0))
      (let ((text (nodecode-pr-studio::diff-tool (diff-request "repo" "o/r" "number" 12))))
        (is (search "[... cut at 2000 of" text) "a long patch is cut, and says where")
        (is (> (length text) 2000))))))

;;; --- the threads ----------------------------------------------------------------------

(deftest pr-studio-cell-threads-carry-the-comment-id ()
  (with-cell-stop ((pr-studio-start))
    (with-gh-stub (("api graphql" *threads* 0))
      (let ((text (nodecode-pr-studio::threads-tool (threads-request "repo" "o/r"
                                                                     "number" 12))))
        (is (search "review threads in o/r#12: 2, 1 unresolved" text))
        (is (search "src/app.lisp:42 (right)" text) "a thread names its place in the diff")
        (is (search "reply with comment_id 987654" text) "and the id a reply takes")
        (is (search "This can be nil." text) "with the comment itself")
        (is (search "src/b.lisp:3 (right) resolved" text) "a resolved thread says so")
        (is (some-call "owner=o") "the query is scoped to the repository"))
      (with-gh-stub (("api graphql" *threads* 0))
        (let ((text (nodecode-pr-studio::threads-tool
                     (threads-request "repo" "o/r" "number" 12 "only_unresolved" t))))
          (is (null (search "src/b.lisp" text)) "only_unresolved drops the resolved thread")
          (is (search "src/app.lisp" text)))))))

;;; --- the writes -----------------------------------------------------------------------

(deftest pr-studio-cell-comments-and-replies-through-gh ()
  (with-cell-stop ((pr-studio-start))
    (with-gh-stub (("pr comment 12" "https://github.com/o/r/pull/12#issuecomment-1\n" 0))
      (let ((text (nodecode-pr-studio::comment-tool
                   (comment-request "repo" "o/r" "number" 12 "body" "Thanks, fixed."))))
        (is (search "commented on o/r#12: https://github.com/o/r/pull/12#issuecomment-1" text))
        (is (equal "Thanks, fixed." (first *files*)) "the body is written to the file gh reads")
        (is (some-call "--body-file"))))
    (with-gh-stub (("comments/987654/replies"
                    "{\"html_url\":\"https://github.com/o/r/pull/12#discussion_r9\"}" 0))
      (let ((text (nodecode-pr-studio::comment-tool
                   (comment-request "repo" "o/r" "number" 12
                                    "body" "It cannot: the guard above." "reply_to" 987654))))
        (is (search "replied in the thread of comment 987654" text))
        (is (search "discussion_r9" text))
        (is (search "It cannot: the guard above." (first *files*)) "the reply rides as the JSON gh api takes")
        (is (search "repos/o/r/pulls/12/comments/987654/replies" (first *calls*)))))))

(deftest pr-studio-cell-review-carries-its-lines ()
  (with-cell-stop ((pr-studio-start))
    (with-gh-stub (("pulls/12/reviews"
                    "{\"html_url\":\"https://github.com/o/r/pull/12#pullrequestreview-3\"}" 0))
      (let ((text (nodecode-pr-studio::review-tool
                   (review-request
                    "repo" "o/r" "number" 12 "event" "COMMENT" "body" "Two notes."
                    "comments" (vector (pr-object (list "path" "src/app.lisp" "line" 42
                                                         "body" "Nil here.")))))))
        (is (search "review comment on o/r#12" text))
        (is (search "pullrequestreview-3" text))
        (let* ((payload (shasht:read-json (first *files*)))
               (comments (nlk:json-value payload :array "comments")))
          (is (equal "COMMENT" (nlk:json-value payload :string "event")))
          (is (equal "Two notes." (nlk:json-value payload :string "body")))
          (is (= 1 (length comments)))
          (is (equal "src/app.lisp" (nlk:json-value (aref comments 0) :string "path")) "the inline comment rides in the review")
          (is (= 42 (nlk:json-value (aref comments 0) :integer "line")))
          (is (equal "RIGHT" (nlk:json-value (aref comments 0) :string "side"))))))
    (with-gh-stub ()
      (is (search "request changes needs a summary"
                  (refusal-text nodecode-pr-studio:pr-studio-error
                                (nodecode-pr-studio:review "o/r" 12 "REQUEST_CHANGES"))) "request changes with nothing to say is refused before gh runs")
      (is (null *calls*)))))

(deftest pr-studio-cell-read-only-refuses-every-write ()
  (with-cell-stop ((pr-studio-start "read_only" t))
    (with-gh-stub ()
      (is (search "read_only is on"
                  (refusal-text nodecode-pr-studio:pr-studio-error
                                (nodecode-pr-studio:comment "o/r" 12 "Hi"))) "a comment is refused while read_only is on")
      (is (search "read_only is on"
                  (refusal-text nodecode-pr-studio:pr-studio-error
                                (nodecode-pr-studio:review "o/r" 12 "APPROVE"))) "and a review too")
      (is (null *calls*) "with gh never run"))))

;;; --- /prs -----------------------------------------------------------------------------

(deftest pr-studio-cell-slash-answers-the-inbox ()
  (with-cell-stop ((pr-studio-start))
    (with-gh-answers (inbox-answers)
      (let ((text (cell-entry "nodecode-pr-studio" "prs" "")))
        (is (search "Needs your review" text))
        (is (search "o/r#7" text))))
    (with-gh-answers (inbox-answers)
      (let ((text (cell-entry "nodecode-pr-studio" "prs" "o/r")))
        (is (search "pull requests in o/r" text))
        (is (every (lambda (line) (search "--repo=o/r" line)) *calls*))))))
