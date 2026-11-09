;;;; cell-test.lisp --- the GitHub repos cell against a stubbed gh.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the lifecycle every cell owns; that a list answers a
;;;; header and one line per repository with its stars, forks, language, issues
;;;; and age; that query, visibility, sort and limit narrow and order it; that
;;;; a fetched list is reused inside cache_seconds and re-read at 0; that the
;;;; totals add up; that a signed-out gh is a failed result carrying gh's
;;;; words; that another account reads /users/<login>/repos with no
;;;; affiliation filter; that /repos answers the same list; that a section
;;;; whose gh is blank refuses; and that a verb with nothing started says so.
;;;; Nothing here needs gh installed, and nothing leaves the machine.

(in-package #:nodecode.test)

(define-test-slice "github-repos" "GITHUB-REPOS-CELL-" :start nodecode-github-repos:start-cell)

;;; --- the stubbed process seam --------------------------------------------------------

(defvar *answers* nil
  "Canned gh answers, in order of the search: (SUBSTRING OUTPUT EXIT-CODE).")
(defvar *calls* nil
  "Every command the cell ran, newest first: one line a call.")

(defun stub-answer (program args)
  (let ((line (format nil "~a ~{~a~^ ~}" program args)))
    (push line *calls*)
    (dolist (entry *answers* (values "" 0))
      (when (search (first entry) line)
        (return-from stub-answer (values (second entry) (third entry)))))))

(defmacro with-gh ((&rest answers) &body body)
  "Run BODY with gh answering ANSWERS -- (SUBSTRING OUTPUT EXIT-CODE) ... -- and
a cache of its own."
  `(let ((*answers* (list ,@(mapcar (lambda (answer) `(list ,@answer)) answers)))
         (*calls* '())
         (nodecode-github-repos::*cache* nil)
         (nodecode-github-repos::*run*
           (lambda (program args timeout)
             (declare (ignore timeout))
             (stub-answer program args))))
     ,@body))

(defun some-call (needle)
  (some (lambda (line) (and (search needle line) t)) *calls*))

;;; --- the fixture the REST API would answer -------------------------------------------

(defun json-text (value)
  "VALUE as a JSON string, or null."
  (if value (format nil "~s" value) "null"))

(defun repo-json (full-name &key description private fork archived stars forks language pushed issues)
  "One repository as /user/repos answers it, spelled out rather than decoded."
  (format nil "{\"full_name\":~a,\"name\":~a,\"description\":~a,\"private\":~a,\"fork\":~a,~
               \"archived\":~a,\"stargazers_count\":~d,\"forks_count\":~d,\"language\":~a,~
               \"pushed_at\":~a,\"created_at\":\"2024-01-01T00:00:00Z\",~
               \"open_issues_count\":~d,\"html_url\":\"https://github.com/~a\"}"
          (json-text full-name)
          (json-text (subseq full-name (1+ (position #\/ full-name))))
          (json-text description)
          (if private "true" "false")
          (if fork "true" "false")
          (if archived "true" "false")
          (or stars 0) (or forks 0)
          (json-text language)
          (json-text pushed)
          (or issues 0)
          full-name))

(defun sample-json ()
  "Three repositories: one public, one private fork, one archived and bare."
  (format nil "[~a,~a,~a]"
          (repo-json "tomekdot/alpha" :description "A parser for a small language"
                      :stars 42 :forks 5 :language "Common Lisp"
                      :pushed "2026-11-01T10:00:00Z" :issues 3)
          (repo-json "tomekdot/beta" :description "Kitchen sink" :private t :fork t
                      :stars 7 :forks 1 :language "Python" :pushed "2025-08-14T10:00:00Z")
          (repo-json "someone/gamma" :archived t :pushed "2026-10-20T10:00:00Z")))

;;; --- the tool calls -----------------------------------------------------------------

(defun object-of (pairs)
  "A JSON object from alternating key/value PAIRS."
  (let ((object (nlk:json-object)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(defun list-request (&rest pairs)
  (nlk:contract-decode (nlk:contract 'nodecode-github-repos::repos-request)
                        (object-of pairs)))

(defun stats-request (&rest pairs)
  (nlk:contract-decode (nlk:contract 'nodecode-github-repos::stats-request)
                        (object-of pairs)))

(defun list-text (&rest pairs)
  (nodecode-github-repos::repos-tool (apply #'list-request pairs)))

(defun stats-text (&rest pairs)
  (nodecode-github-repos::stats-tool (apply #'stats-request pairs)))

;;; --- lifecycle ----------------------------------------------------------------------

(define-cell-lifecycle-tests "github-repos"
  (:command "repos")
  (:running (is (= 40 (nodecode-github-repos::setting :limit)) "the list shows 40 rows by default")
            (is (string= "gh" (nodecode-github-repos::setting :gh)) "and runs gh from PATH")
            (is (= 60 (nodecode-github-repos::setting :cache-seconds)) "a fetch is reused for a minute")
            (is (not (null (nle::find-tool "github_repo_stats"))) "the totals tool is registered"))
  (:refused ("sort" "wobble"))
  (:idle nodecode-github-repos:github-repos-error
         (nodecode-github-repos:repos :sort "stars")))

;;; --- the list -----------------------------------------------------------------------

(deftest github-repos-cell-lists-what-gh-answers ()
  (with-cell-stop ((github-repos-start))
    (with-gh (("api /user/repos" (sample-json) 0))
      (let ((text (list-text)))
        (is (search "GitHub repositories of tomekdot: 3" text) "the header counts them")
        (is (search "tomekdot/alpha" text) "and the first repository is named")
        (is (search "stars 42  forks 5  Common Lisp  3 issues  pushed " text) "with its counts, language and age")
        (is (search "A parser for a small language" text) "and its description")
        (is (search "tomekdot/beta [private] [fork]" text) "a private fork says so")
        (is (search "someone/gamma [archived]" text) "so does an archived repository")
        (is (some-call "api /user/repos?per_page=100&page=1") "one page of a hundred is asked for")
        (is (some-call "affiliation=owner") "and only the account's own repositories")))))

(deftest github-repos-cell-narrows-and-orders ()
  (with-cell-stop ((github-repos-start))
    (with-gh (("api /user/repos" (sample-json) 0))
      (let ((private (list-text "visibility" "private"))
            (found (list-text "query" "parser"))
            (stars (list-text "sort" "stars"))
            (one (list-text "limit" 1)))
        (is (search "tomekdot/beta" private) "only the private one survives the filter")
        (is (not (search "tomekdot/alpha" private)))
        (is (search "tomekdot/alpha" found) "a query reads names, descriptions and languages")
        (is (not (search "someone/gamma" found)))
        (is (search "sorted by stars" stars) "the order is named")
        (is (< (search "tomekdot/alpha" stars) (search "tomekdot/beta" stars)) "most stars first")
        (is (search "tomekdot/alpha" one) "a limit of one keeps the newest")
        (is (not (search "someone/gamma" one)))
        (is (search "[2 more" one) "and it says how many were cut")))))

(deftest github-repos-cell-a-bad-login-is-refused ()
  (with-cell-stop ((github-repos-start))
    (with-gh (("api " (sample-json) 0))
      (let ((text (list-text "account" "not a login")))
        (is (search "ERROR:" text) "the refusal is a failed result")
        (is (search "no GitHub login" text) "naming what was wrong")
        (is (null *calls*) "before gh ran")))))

(deftest github-repos-cell-another-account-reads-its-public-repos ()
  (with-cell-stop ((github-repos-start))
    (with-gh (("api /users/someone/repos" (sample-json) 0))
      (list-text "account" "someone")
      (is (some-call "api /users/someone/repos?") "another account is read from its own path")
      (is (not (some-call "affiliation=owner")) "with no owner filter to apply"))))

(deftest github-repos-cell-a-signed-out-gh-is-a-failed-result ()
  (with-cell-stop ((github-repos-start))
    (with-gh (("api /user/repos" "gh: To get started with GitHub CLI, please run: gh auth login" 1))
      (let ((text (list-text)))
        (is (search "ERROR:" text) "the failure reaches the model")
        (is (search "gh auth login" text) "in gh's own words")))))

(deftest github-repos-cell-a-list-is-cached-for-a-minute ()
  (with-cell-stop ((github-repos-start))
    (with-gh (("api /user/repos" (sample-json) 0))
      (list-text)
      (list-text)
      (is (= 1 (length *calls*)) "the second read answers from the cache"))))

(deftest github-repos-cell-cache-zero-reads-gh-every-time ()
  (with-cell-stop ((github-repos-start "cache_seconds" 0))
    (with-gh (("api /user/repos" (sample-json) 0))
      (list-text)
      (list-text)
      (is (= 2 (length *calls*)) "no cache keeps a list"))))

;;; --- the totals ---------------------------------------------------------------------

(deftest github-repos-cell-totals-add-up ()
  (with-cell-stop ((github-repos-start))
    (with-gh (("api /user/repos" (sample-json) 0))
      (let ((text (stats-text)))
        (is (search "of tomekdot: 3" text) "every repository is counted")
        (is (search "2 public, 1 private, 1 forks, 1 archived" text) "by visibility and kind")
        (is (search "49 stars, 3 open issues" text) "with the stars and issues summed")
        (is (search "languages:" text) "and the languages they are written in")
        (is (search "Common Lisp (1)" text))
        (is (search "Python (1)" text))))))

;;; --- the command -------------------------------------------------------------------

(deftest github-repos-cell-the-command-lists ()
  (with-cell-stop ((github-repos-start))
    (with-gh (("api /user/repos" (sample-json) 0))
      (let ((text (cell-entry "nodecode-github-repos" "repos" "kitchen")))
        (is (search "tomekdot/beta" text) "the query narrows what /repos lists")
        (is (not (search "tomekdot/alpha" text)))))))

;;; --- the boundary ------------------------------------------------------------------

(deftest github-repos-cell-a-visibility-outside-the-declaration-refuses ()
  (is (signals-error nlk:contract-refusal (list-request "visibility" "secret"))))

(deftest github-repos-cell-a-blank-gh-refuses-by-name ()
  (let ((condition (signals-error nlk:config-refusal (github-repos-start "gh" ""))))
    (is (search "gh" (princ-to-string condition)) "the refusal names the setting")
    (is (null (nle::find-tool "github_repos")) "and installs no list tool")
    (is (null (nle::find-tool "github_repo_stats")) "nor the totals tool")))
