;;;; cell-test.lisp --- the github-mentions cell against a stubbed GitHub.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every GitHub endpoint is a stubbed NLK:HTTP and the token a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches GitHub, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it —
;;;; through the real dispatch, contract and all — so the schema the provider
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "github-mentions" "GITHUB-MENTIONS-CELL-")

(defun github-mentions-config (&rest pairs)
  "A whole shared config holding the `github_mentions' section PAIRS make,
with its token variable and one approved author defaulted."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "token_env" section "GITHUB_TOKEN")
    (alexandria:ensure-gethash "allowed_authors" section (vector "alice"))
    (nlk:json-object "github_mentions" section)))

(defun github-mentions-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-github-mentions:start-cell (apply #'github-mentions-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-github-token (&body body)
  "BODY with the token environment answering one fixed token."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "GITHUB_TOKEN") "test-token"))
     ,@body))

(defmacro with-github-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order — the most specific first — and FORM runs with METHOD, URL and OPTIONS
bound and answers (values TEXT STATUS), as NLK:HTTP does. ASKED collects
(METHOD URL CONTENT HEADERS), oldest first, once BODY reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content) (getf options :headers)) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected GitHub request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

;;; --- what GitHub answers -------------------------------------------------------------

(defparameter +login+ "{\"login\":\"octo\"}"
  "GET /user: the token's own login.")

(defparameter +notification+
  (concatenate 'string
               "{\"id\":\"101\",\"unread\":true,\"reason\":\"mention\","
               "\"updated_at\":\"2026-01-02T10:00:00Z\","
               "\"subject\":{\"title\":\"Parser crashes on empty input\","
               "\"url\":\"https://api.github.com/repos/o/r/issues/7\",\"type\":\"Issue\","
               "\"latest_comment_url\":\"https://api.github.com/repos/o/r/issues/comments/555\"},"
               "\"repository\":{\"full_name\":\"o/r\",\"owner\":{\"login\":\"o\"},\"name\":\"r\"}}")
  "One unread notification whose reason is a mention, as GitHub lists it.")

(defparameter +pull-notification+
  (concatenate 'string
               "{\"id\":\"102\",\"unread\":true,\"reason\":\"mention\","
               "\"updated_at\":\"2026-01-02T10:00:00Z\","
               "\"subject\":{\"title\":\"Add a parser\","
               "\"url\":\"https://api.github.com/repos/o/r/pulls/7\",\"type\":\"PullRequest\"},"
               "\"repository\":{\"full_name\":\"o/r\",\"owner\":{\"login\":\"o\"},\"name\":\"r\"}}")
  "One unread mention on a pull request.")

(defparameter +issue+
  "{\"number\":7,\"title\":\"Parser crashes on empty input\",\"body\":\"The parser crashes on empty input.\",\"state\":\"open\",\"html_url\":\"https://github.com/o/r/issues/7\",\"user\":{\"login\":\"alice\"},\"created_at\":\"2026-01-01T09:00:00Z\"}"
  "GET /repos/o/r/issues/7.")

(defparameter +pull+
  "{\"number\":7,\"title\":\"Add a parser\",\"body\":\"Adds a parser.\",\"state\":\"open\",\"html_url\":\"https://github.com/o/r/pull/7\",\"user\":{\"login\":\"bob\"},\"created_at\":\"2026-01-01T09:00:00Z\"}"
  "GET /repos/o/r/pulls/7.")

(defparameter +comments+
  "[{\"id\":555,\"body\":\"@octo please fix the parser\",\"html_url\":\"https://github.com/o/r/issues/7#issuecomment-555\",\"user\":{\"login\":\"alice\"},\"created_at\":\"2026-01-02T10:00:00Z\",\"updated_at\":\"2026-01-02T10:00:00Z\"}]"
  "The conversation's one comment: the mention.")

(defparameter +mallory-comments+
  "[{\"id\":556,\"body\":\"@octo please fix the parser\",\"html_url\":\"https://github.com/o/r/issues/7#issuecomment-556\",\"user\":{\"login\":\"mallory\"},\"created_at\":\"2026-01-02T10:00:00Z\",\"updated_at\":\"2026-01-02T10:00:00Z\"}]"
  "The same mention, by an author allowed_authors does not name.")

(defparameter +review-comments+
  "[{\"id\":99,\"body\":\"@octo take a look at this\",\"html_url\":\"https://github.com/o/r/pull/7#discussion_r99\",\"user\":{\"login\":\"alice\"},\"created_at\":\"2026-01-02T10:00:00Z\",\"updated_at\":\"2026-01-02T10:00:00Z\"}]"
  "One inline review comment: the mention.")

(defparameter +posted+
  "{\"html_url\":\"https://github.com/o/r/issues/7#issuecomment-556\"}"
  "POST to a thread's comments.")

;;; --- the answers, as the model reads them --------------------------------------------

(defun gm-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun gm-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (gm-answer id tool arguments)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "github-mentions"
  (:help :github-mentions)
  (:command "mentions")
  (:fixture with-github-token)
  (:refused ("token_env" "") ("allowed_authors" #())))

;;; --- github_mentions -----------------------------------------------------------------

(deftest github-mentions-cell-lists-a-mention (with-github-token)
  (with-cell-stop ((github-mentions-start))
    (with-github-wire (asked ("/user" (values +login+ 200))
                             ("/repos/o/r/issues/7/comments" (values +comments+ 200))
                             ("/repos/o/r/issues/7" (values +issue+ 200))
                             ("/notifications?" (values (format nil "[~a]" +notification+) 200)))
      (let* ((object (gm-object "gm-1" "github_mentions" "{}"))
             (mentions (nlk:json-array object "mentions"))
             (mention (and mentions (aref mentions 0))))
        (is (= 1 (nlk:json-value object :integer "count")) "one unread mention")
        (is (equal "octo" (nlk:json-value object :string "login")) "the token's own login is the mention target")
        (is-present mention
          (is (equal "101" (nlk:json-value mention :string "notification_id")) "the thread id to answer with")
          (is (equal "o/r" (nlk:json-value mention :string "repo")))
          (is (= 7 (nlk:json-value mention :integer "number")))
          (is (equal "alice" (nlk:json-value mention :string "author")))
          (is (equal "please fix the parser" (nlk:json-value mention :string "request"))
              "the request is the mention's paragraph, the mention taken out")
          (is (search "please fix the parser" (nlk:json-value mention :string "body")))))
      (is (= 4 (length asked)) "one login read, the list, the thread and its comments"))))

(deftest github-mentions-cell-ignores-an-unapproved-author (with-github-token)
  (with-cell-stop ((github-mentions-start))
    (with-github-wire (asked ("/user" (values +login+ 200))
                             ("/repos/o/r/issues/7/comments" (values +mallory-comments+ 200))
                             ("/repos/o/r/issues/7" (values +issue+ 200))
                             ("/notifications?" (values (format nil "[~a]" +notification+) 200)))
      (let ((object (gm-object "gm-2" "github_mentions" "{}")))
        (is (= 0 (nlk:json-value object :integer "count")) "mallory is not in allowed_authors")
        (is (= 1 (nlk:json-value object :integer "unresolved")) "and the thread is reported, not silently dropped")))))

(deftest github-mentions-cell-filters-by-repository (with-github-token)
  (with-cell-stop ((github-mentions-start))
    (with-github-wire (asked ("/user" (values +login+ 200))
                             ("/notifications?" (values (format nil "[~a]" +notification+) 200)))
      (let ((object (gm-object "gm-3" "github_mentions" "{\"repo\":\"other/repo\"}")))
        (is (= 0 (nlk:json-value object :integer "count")) "a mention in another repository is not listed")
        (is (= 2 (length asked)) "and no thread is read")))))

;;; --- github_mention_reply ---------------------------------------------------------------

(deftest github-mentions-cell-replies-in-the-thread (with-github-token)
  (with-cell-stop ((github-mentions-start))
    (with-github-wire (asked ("/notifications/threads/101" (values +notification+ 200))
                             ("/user" (values +login+ 200))
                             ("/repos/o/r/issues/7/comments"
                              (if (eq method :post) (values +posted+ 201) (values +comments+ 200)))
                             ("/repos/o/r/issues/7" (values +issue+ 200)))
      (let ((object (gm-object "gm-4" "github_mention_reply"
                                "{\"notification_id\":\"101\",\"body\":\"Fixed in 9c1.\"}")))
        (is (eq t (nlk:json-value object :boolean "posted")))
        (is (equal "https://github.com/o/r/issues/7#issuecomment-556"
                   (nlk:json-value object :string "url")))
        (is (eq t (nlk:json-value object :boolean "marked_read")) "the notification is read, so it is not listed again"))
      (let ((post (find-if (lambda (call) (eq (first call) :post)) asked))
            (patch (find-if (lambda (call) (eq (first call) :patch)) asked)))
        (is (equal "/repos/o/r/issues/7/comments"
                   (and post (subseq (second post) (length "https://api.github.com"))))
            "the answer is a comment in the mention's own thread")
        (is (search "Fixed in 9c1." (or (and post (third post)) "")) "with the body the model wrote")
        (is (search "/notifications/threads/101" (or (and patch (second patch)) "")) "and the thread is marked read")))))

(deftest github-mentions-cell-replies-inline-to-a-review-comment (with-github-token)
  (with-cell-stop ((github-mentions-start))
    (with-github-wire (asked ("/notifications/threads/102" (values +pull-notification+ 200))
                             ("/user" (values +login+ 200))
                             ("/pulls/7/comments"
                              (if (eq method :post) (values +posted+ 201) (values +review-comments+ 200)))
                             ("/pulls/7/reviews" (values "[]" 200))
                             ("/repos/o/r/issues/7/comments" (values "[]" 200))
                             ("/repos/o/r/pulls/7" (values +pull+ 200)))
      (gm-object "gm-5" "github_mention_reply"
                  "{\"notification_id\":\"102\",\"body\":\"Looks right to me.\"}")
      (let ((post (find-if (lambda (call) (eq (first call) :post)) asked)))
        (is post "the answer was posted")
        (is (search "/repos/o/r/pulls/7/comments" (second post))
            "as an inline reply, in the review thread")
        (is (search "\"in_reply_to\":99" (or (third post) ""))
            "answering the review comment that mentioned the login")))))

(deftest github-mentions-cell-dismisses-with-an-empty-body (with-github-token)
  (with-cell-stop ((github-mentions-start))
    (with-github-wire (asked ("/notifications/threads/101" (values +notification+ 200)))
      (let ((object (gm-object "gm-6" "github_mention_reply"
                                "{\"notification_id\":\"101\",\"body\":\"\"}")))
        (is (eq t (nlk:json-value object :boolean "dismissed")))
        (is (search "/notifications/threads/101" (second (second asked))))
        (is (eq :patch (first (second asked))) "marked read, and nothing was posted")
        (is (= 2 (length asked)) "the notification read, then its thread marked read")))))

;;; --- github_mention_thread ---------------------------------------------------------------

(deftest github-mentions-cell-reads-a-thread (with-github-token)
  (with-cell-stop ((github-mentions-start))
    (with-github-wire (asked ("/notifications/threads/101" (values +notification+ 200))
                             ("/repos/o/r/issues/7/comments" (values +comments+ 200))
                             ("/repos/o/r/issues/7" (values +issue+ 200)))
      (let* ((object (gm-object "gm-7" "github_mention_thread" "{\"notification_id\":\"101\"}"))
             (comments (nlk:json-array object "comments")))
        (is (equal "Parser crashes on empty input" (nlk:json-value object :string "title")))
        (is (equal "open" (nlk:json-value object :string "state")))
        (is (= 1 (length comments)))
        (is (equal "alice" (nlk:json-value (aref comments 0) :string "author")))
        (is (search "please fix the parser" (nlk:json-value (aref comments 0) :string "body")))))))

;;; --- /mentions -----------------------------------------------------------------------------

(deftest github-mentions-cell-command-lists-mentions (with-github-token)
  (with-cell-stop ((github-mentions-start))
    (with-github-wire (asked ("/user" (values +login+ 200))
                             ("/repos/o/r/issues/7/comments" (values +comments+ 200))
                             ("/repos/o/r/issues/7" (values +issue+ 200))
                             ("/notifications?" (values (format nil "[~a]" +notification+) 200)))
      (let ((text (cell-entry "nodecode-github-mentions" "mentions" "")))
        (is (search "[o/r #7]" text) "the row names the repository and the thread")
        (is (search "@alice" text) "and who is asking")
        (is (search "please fix the parser" text) "and what for")))))

(deftest github-mentions-cell-command-says-when-nothing-awaits (with-github-token)
  (with-cell-stop ((github-mentions-start))
    (with-github-wire (asked ("/user" (values +login+ 200))
                             ("/notifications?" (values "[]" 200)))
      (is (search "No unread @mentions of @octo" (cell-entry "nodecode-github-mentions" "mentions" ""))))))
