;;;; cell-test.lisp --- the two tools, /jobsearch, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring against a stubbed program: that
;;;; each tool asks agent-data the argv the plugin asked it, that a search
;;;; answers the rows to judge from with the id and url a detail read needs,
;;;; that a posting read names the fields the page left out, that a refused
;;;; call is a refusal carrying the body the CLI printed, and that the program
;;;; is really run with its exit code really read. Nothing here needs
;;;; agent-data installed or a network.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "job-search"
  (:help :job-search)
  (:command "jobsearch")
  (:running (is (equal "f9a6ec16-0bfd-44d8-b3ee-073776745ee7"
                       (nodecode-job-search::setting :listing)) "the plugin's listing id is the default")
            (is (= 6000 (nodecode-job-search::setting :max-chars)) "six thousand characters of a posting by default"))
  (:refused ("command" "definitely-no-such-program-xyz"))
  (:idle nodecode-job-search:job-search-error
         (job-search:find-postings :keywords "senior designer")
         (job-search:posting :posting-id "jp_22d0d871db24"
                             :source-url "https://jobs.ashbyhq.com/tidewater/jp_22d0d871db24"
                             :source "ashby")))

;;; --- the search ---------------------------------------------------------------------

(deftest job-search-cell-a-search-asks-the-route-the-plugin-asked ()
  (with-cell-stop ((job-search-start))
    (with-run-stub (:search +search-json+)
      (let ((answer (job-search:find-postings :keywords "senior designer" :source "ashby"
                                       :location "United States" :limit 5)))
        (is (equal '("call" "f9a6ec16-0bfd-44d8-b3ee-073776745ee7" "search-jobs"
                     "--source" "ashby"
                     "--keywords" "senior designer"
                     "--location" "United States"
                     "--limit" "5")
                   (last-call)) "one search, one source, the flags the plugin's wrapper passed")
        (is (search "2 postings from ashby" answer) "the count and the source that answered")
        (is (search "Senior Product Designer - Tidewater Health - Remote (US)" answer) "the row a reader judges from")
        (is (search "$180K - $210K" answer) "with the salary where the source gives one")
        (is (search "jp_22d0d871db24" answer) "the id a detail read needs")
        (is (search "https://jobs.ashbyhq.com/tidewater/jp_22d0d871db24" answer) "beside the url it must be paired with")
        (is (search "more with cursor cur_2" answer) "and the next page named")))))

(deftest job-search-cell-a-search-names-the-source-that-really-answered ()
  (with-cell-stop ((job-search-start))
    (with-run-stub (:search +search-json+)
      (let ((answer (job-search:find-postings :keywords "senior designer" :source "linkedin")))
        (is (search "(asked for linkedin)" answer) "a source that answered for another is said out loud")))))

(deftest job-search-cell-an-empty-page-says-so-without-refusing ()
  (with-cell-stop ((job-search-start))
    (with-run-stub (:search +empty-search-json+)
      (let ((answer (job-search:find-postings :keywords "a role nobody has" :source "linkedin")))
        (is (search "0 postings from linkedin" answer) "nothing found is an answer")
        (is (search "none this time" answer) "and it says what it found")))))

;;; --- the posting read ---------------------------------------------------------------

(deftest job-search-cell-a-posting-read-names-what-the-page-left-out ()
  (with-cell-stop ((job-search-start))
    (with-run-stub (:posting +posting-json+)
      (let ((answer (job-search:posting
                     :posting-id "jp_22d0d871db24"
                     :source-url "https://jobs.ashbyhq.com/tidewater/jp_22d0d871db24"
                     :source "ashby")))
        (is (equal '("call" "f9a6ec16-0bfd-44d8-b3ee-073776745ee7" "get-posting"
                     "--posting_id" "jp_22d0d871db24"
                     "--source_url" "https://jobs.ashbyhq.com/tidewater/jp_22d0d871db24"
                     "--source" "ashby")
                   (last-call)) "the pair and the source, in the shape the route takes")
        (is (search "Owns a care-navigation area end to end" answer) "the description")
        (is (search "Salary: $180K - $210K" answer) "the salary")
        (is (search "leaves these unstated: employment_type, workplace_type" answer) "and what the page did not yield, named rather than assumed")
        (is (search "Source: ashby https://jobs.ashbyhq.com/tidewater/jp_22d0d871db24" answer) "with where the user applies")))))

;;; --- refusals ------------------------------------------------------------------------

(deftest job-search-cell-a-failed-call-refuses-with-the-cli-s-body ()
  (with-cell-stop ((job-search-start))
    (with-run-stub (:search +refused-json+ :code 1)
      (let ((said (refusal (lambda () (job-search:find-postings :keywords "senior designer")))))
        (is (search "The search failed" said) "a failing exit code is a refusal")
        (is (search "insufficient_credits" said) "carrying what the CLI printed")))))

(deftest job-search-cell-a-verb-refuses-what-it-cannot-ask ()
  (with-cell-stop ((job-search-start))
    (with-run-stub ()
      (is (search "needs keywords" (refusal (lambda () (job-search:find-postings :keywords " ")))) "no keywords, no call")
      (is (search "needs the posting id"
                  (refusal (lambda () (job-search:posting :source-url "https://x.test/j"
                                                          :source "ashby")))) "no id, no call")
      (is (search "needs the source_url"
                  (refusal (lambda () (job-search:posting :posting-id "jp_x" :source "ashby")))) "no url, no call")
      (is (null *calls*) "and nothing was spent"))))

;;; --- /jobsearch ----------------------------------------------------------------------

(deftest job-search-cell-jobsearch-says-whether-a-key-is-set ()
  (with-cell-stop ((job-search-start))
    (with-run-stub (:whoami +whoami-key+)
      (is (search "has an API key" (nodecode-job-search::run-slash nil nil)))
      (is (equal '("whoami") (last-call)) "whoami is local and free"))
    (with-run-stub (:whoami +whoami-nokey+)
      (let ((answer (nodecode-job-search::run-slash nil nil)))
        (is (search "has no API key" answer) "a missing key is named")
        (is (search "agent-data init" answer) "with the command that fixes it")))
    (with-run-stub (:search +search-json+)
      (nodecode-job-search::run-slash "senior designer" nil)
      (is (member "--keywords" (last-call) :test #'equal) "words after the command are a search")
      (is (equal "5" (car (last (last-call)))) "of five rows, for the operator to read"))))

;;; --- the tools the model calls ---------------------------------------------------------

(deftest job-search-cell-both-tools-are-registered-and-answer-through-their-contracts ()
  (with-cell-stop ((job-search-start))
    (with-run-stub (:search +search-json+ :posting +posting-json+)
      (dolist (name '("job_search" "job_posting"))
        (let ((entry (nle::find-tool name)))
          (is (consp entry) (format nil "~a is registered" name))
          (is (hash-table-p (nlk:contract-schema (second entry))) "against a declared contract")
          (let* ((schema (second entry))
                 (arguments (if (equal name "job_search")
                                (nlk:make-json-object "keywords" "senior designer"
                                                       "source" "ashby")
                                (nlk:make-json-object "posting_id" "jp_22d0d871db24"
                                                       "source_url" "https://jobs.ashbyhq.com/tidewater/jp_22d0d871db24"
                                                       "source" "ashby")))
                 (answer (funcall (third entry) (nlk:contract-decode schema arguments))))
            (is (search (if (equal name "job_search")
                            "Senior Product Designer"
                            "Owns a care-navigation")
                        answer)
                (format nil "~a answers with what the program said" name))))))
    ;; the contract refuses a source outside the four
    (let ((entry (nle::find-tool "job_search")))
      (is (signals-error nlk:contract-refusal
                         (nlk:contract-decode (second entry)
                                              (nlk:make-json-object "keywords" "x"
                                                                     "source" "monster"))) "a fifth source is refused before any call"))))

;;; --- the program really runs -------------------------------------------------------------

(deftest job-search-cell-the-program-is-really-run-and-its-exit-code-read ()
  (let ((directory (merge-pathnames "job-search-run/" (uiop:temporary-directory))))
    (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree directory :validate t)))
      (let ((program (agent-data-recording-program directory :output +search-json+)))
        (with-cell-stop ((job-search-start "command" program))
          (let ((answer (job-search:find-postings :keywords "senior designer" :source "ashby")))
            (is (stringp answer) "an answer, not a condition")
            (is (search "Senior Product Designer" (or answer "")) "the program's answer is the tool's answer")
            (let ((argv (uiop:read-file-string (merge-pathnames "argv" directory))))
              (is (search "search-jobs" argv) "the program was asked for a search")
              (is (search "senior designer" argv) "with the keywords as an argument")))))
      (let ((failing (agent-data-recording-program
                      (merge-pathnames "failing/" directory)
                      :output "" :status 3 :error +refused-json+)))
        (with-cell-stop ((job-search-start "command" failing))
          (let ((said (refusal (lambda () (job-search:find-postings :keywords "senior designer")))))
            (is (search "insufficient_credits" said) "a failing exit code is read as a failure, with the body it printed")))))))
