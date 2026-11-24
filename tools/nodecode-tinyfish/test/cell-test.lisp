;;;; cell-test.lisp --- lifecycle, the five tools, /tinyfish.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a stubbed TinyFish:
;;;; that a start installs the five tools, /tinyfish and the manual, and a
;;;; stop takes every one of them back; that a wrong setting refuses; that
;;;; search asks the configured host with the key and numbers the results;
;;;; that fetch posts the urls it was given and answers the page; that the
;;;; agent starts a run, waits for it and answers its result, and that a run
;;;; still going says so; that a profile is created and answers its sign-in
;;;; link; that a refusal names what TinyFish said and what to fix; and that
;;;; a keyless cell refuses without dialing.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "tinyfish"
  (:config "api_key" "tf-test")
  (:help :tinyfish)
  (:command "tinyfish")
  (:running (is (nle::find-tool "tinyfish_search") "the search tool is registered")
            (is (nle::find-tool "tinyfish_fetch") "the fetch tool is registered")
            (is (nle::find-tool "tinyfish_agent") "the agent tool is registered")
            (is (nle::find-tool "tinyfish_list_profiles") "the profile list tool is registered")
            (is (nle::find-tool "tinyfish_create_profile") "the profile create tool is registered")
            (is (equal "tf-test" (nodecode-tinyfish::setting :api-key)))
            (is (= 5 (nodecode-tinyfish::setting :max-results)))
            (is (equal "https://search.test" (nodecode-tinyfish::base :search))))
  (:refused ("timeout_seconds" 0) ("max_results" 0) ("max_chars" 10))
  (:stopped (is (null (nle::find-tool "tinyfish_search")) "stop unregisters the tools")
            (is (null (nle::find-tool "tinyfish_agent")))
            (is (null (nle::find-tool "tinyfish_create_profile")))))

;;; --- search ------------------------------------------------------------------

(deftest tinyfish-cell-search-asks-the-configured-host-with-the-key ()
  (with-tinyfish ()
    (tinyfish-answering "search.test"
                        (tinyfish-json (nlk:json-object
                                        "results"
                                        (vector (nlk:json-object "title" "TinyFish"
                                                                 "url" "https://example.com/a"
                                                                 "snippet" "the web, built in")
                                                (nlk:json-object "title" "Second"
                                                                 "url" "https://example.com/b"))))) 
    (let ((answer (tinyfish:search-text "tinyfish")))
      (is (search "1. TinyFish" answer) "results are numbered")
      (is (search "https://example.com/a" answer) "with their url")
      (is (search "the web, built in" answer) "and their snippet")
      (is (search "2. Second" answer) "every result is listed"))
    (let ((request (first (tinyfish-requests))))
      (is (equal "GET" (first request)))
      (is (search "query=tinyfish" (second request)) "the query rides the URL")
      (is (equal "tf-test" (tinyfish-header request "x-api-key")) "the section's key")
      (is (equal "nodecode" (tinyfish-header request "x-tf-client-name"))))))

(deftest tinyfish-cell-search-lists-only-as-many-as-asked ()
  (with-tinyfish ("max_results" 1)
    (tinyfish-answering "search.test"
                        (tinyfish-json (nlk:json-object
                                        "results"
                                        (vector (nlk:json-object "title" "One" "url" "https://example.com/1")
                                                (nlk:json-object "title" "Two" "url" "https://example.com/2")))))
    (let ((answer (tinyfish:search-text "anything")))
      (is (search "1. One" answer))
      (is (not (search "Two" answer)) "the section's max_results caps what is listed"))))

(deftest tinyfish-cell-a-search-with-no-results-says-so ()
  (with-tinyfish ()
    (tinyfish-answering "search.test" (tinyfish-json (nlk:json-object "results" (vector))))
    (is (search "found nothing" (tinyfish:search-text "nothing at all")))))

;;; --- fetch -------------------------------------------------------------------

(deftest tinyfish-cell-fetch-posts-the-urls-and-answers-the-page ()
  (with-tinyfish ()
    (tinyfish-answering "fetch.test"
                        (tinyfish-json (nlk:json-object
                                        "results"
                                        (vector (nlk:json-object "url" "https://example.com/"
                                                                 "title" "Example Domain"
                                                                 "text" "# Example Domain")))))
    (let ((answer (tinyfish:fetch-text (list "https://example.com/"))))
      (is (search "## https://example.com/" answer) "the page is named")
      (is (search "# Example Domain" answer) "with the text TinyFish read"))
    (let* ((request (first (tinyfish-requests)))
           (body (tinyfish-body request)))
      (is (equal "POST" (first request)))
      (is (equal '("https://example.com/") (coerce (gethash "urls" body) 'list)))
      (is (equal "markdown" (gethash "format" body)) "markdown unless the call names another"))))

(deftest tinyfish-cell-a-fetch-error-is-named-for-its-url ()
  (with-tinyfish ()
    (tinyfish-answering "fetch.test"
                        (tinyfish-json (nlk:json-object
                                        "errors"
                                        (vector (nlk:json-object "url" "https://example.com/gone"
                                                                 "error" "404 from the site")))))
    (let ((answer (tinyfish:fetch-text (list "https://example.com/gone"))))
      (is (search "https://example.com/gone" answer))
      (is (search "ERROR: 404 from the site" answer) "the failure names its url"))))

;;; --- the agent ----------------------------------------------------------------

(deftest tinyfish-cell-agent-starts-a-run-and-answers-its-result ()
  (with-tinyfish ()
    (tinyfish-answering "run-async" (tinyfish-json (nlk:json-object "run_id" "run-1")))
    (tinyfish-answering "/v1/runs/run-1"
                        (tinyfish-json (nlk:json-object "run_id" "run-1"
                                                         "status" "COMPLETED"
                                                         "result" "the first price is 12 dollars")))
    (let ((answer (tinyfish:agent-text "https://shop.test/search" "return the first price")))
      (is (search "run-1" answer) "the run is named")
      (is (search "COMPLETED" answer) "with its state")
      (is (search "12 dollars" answer) "and what the agent found"))
    (let* ((start (first (tinyfish-requests)))
           (body (tinyfish-body start)))
      (is (equal "POST" (first start)))
      (is (search "/v1/automation/run-async" (second start)))
      (is (equal "https://shop.test/search" (gethash "url" body)))
      (is (equal "return the first price" (gethash "goal" body)))
      (is (equal "lite" (gethash "browser_profile" body))))))

(deftest tinyfish-cell-agent-names-a-profile-it-runs-with ()
  (with-tinyfish ()
    (tinyfish-answering "run-async" (tinyfish-json (nlk:json-object "run_id" "run-2")))
    (tinyfish-answering "/v1/runs/run-2"
                        (tinyfish-json (nlk:json-object "status" "COMPLETED" "result" "done")))
    (tinyfish:agent-text "https://shop.test/" "read the order" :profile-id "profile-9")
    (let ((body (tinyfish-body (first (tinyfish-requests)))))
      (is (eq t (gethash "use_profile" body)) "an id implies use_profile")
      (is (equal "profile-9" (gethash "profile_id" body))))))

(deftest tinyfish-cell-a-run-still-going-answers-its-run-id ()
  (with-tinyfish ("agent_timeout_seconds" 1)
    (tinyfish-answering "run-async" (tinyfish-json (nlk:json-object "run_id" "run-3")))
    (tinyfish-answering "/v1/runs/run-3"
                        (tinyfish-json (nlk:json-object "status" "RUNNING")))
    (let ((answer (tinyfish:agent-text "https://shop.test/" "wait for it")))
      (is (search "RUNNING" answer))
      (is (search "run-3" answer) "the id to ask about later is in the answer"))))

;;; --- profiles -----------------------------------------------------------------

(deftest tinyfish-cell-profiles-are-listed-with-their-sites ()
  (with-tinyfish ()
    (tinyfish-answering "/v1/profiles"
                        (tinyfish-json (nlk:json-object
                                        "profiles"
                                        (vector (nlk:json-object "id" "p1" "name" "Shop account"
                                                                 "is_default" t
                                                                 "signed_in_sites"
                                                                 (vector (nlk:json-object "site" "shop.test")))))))
    (let ((answer (tinyfish:profiles-text)))
      (is (search "Shop account" answer))
      (is (search "p1" answer) "the id a run takes")
      (is (search "shop.test" answer) "and the site it covers"))))

(deftest tinyfish-cell-a-new-profile-answers-its-sign-in-link ()
  (with-tinyfish ()
    (tinyfish-answering "/v1/profiles"
                        (tinyfish-json (nlk:json-object "id" "p2" "name" "Shop account")))
    (let ((answer (tinyfish:create-profile-text "Shop account" :site "shop.test")))
      (is (search "https://agent.test/profiles/p2/setup" answer) "the link to sign in at")
      (is (search "domain=shop.test" answer) "on the site named")
      (is (search "profile_id p2" answer) "and the id to run with"))
    (let ((body (tinyfish-body (first (tinyfish-requests)))))
      (is (equal "Shop account" (gethash "name" body)))
      (is (null (gethash "site" body)) "the site rides the sign-in link, not the profile"))))

;;; --- refusals ------------------------------------------------------------------

(deftest tinyfish-cell-without-a-key-refuses-and-never-dials ()
  (with-tinyfish ("api_key" "")
    (let ((answer (tinyfish:search-text "anything")))
      (is (search "ERROR: TINYFISH-ERROR" answer) "the refusal is a failure the model reads")
      (is (search "no TinyFish API key" answer) "naming what to set"))
    (is (null (tinyfish-requests)) "and nothing was dialed")
    (is (search "missing" (tinyfish:status-text)) "the command says the key is not set")))

(deftest tinyfish-cell-a-refusal-names-what-tinyfish-said ()
  (with-tinyfish ()
    (tinyfish-answering "search.test" (tinyfish-json (nlk:json-object "error" "bad key") 401))
    (let ((answer (tinyfish:search-text "anything")))
      (is (search "ERROR: TINYFISH-ERROR" answer))
      (is (search "HTTP 401" answer) "with the status")
      (is (search "bad key" answer) "and TinyFish's own sentence"))
    (is (search "HTTP 401" nodecode-tinyfish::*last-error*) "and the command keeps it")))

;;; --- the command ---------------------------------------------------------------

(deftest tinyfish-cell-command-reports-the-key-hosts-and-credits ()
  (with-tinyfish ()
    (tinyfish-answering "/v1/wallet"
                        (tinyfish-json (nlk:json-object "available_balance" "12.50"
                                                         "currency" "USD" "as_of" "today")))
    (let ((answer (tinyfish:run-command "" "s1")))
      (is (search "API key: set" answer))
      (is (search "https://agent.test" answer) "the agent host")
      (is (search "12.50 USD" answer) "the credits left"))
    (is (search "/tinyfish takes nothing" (tinyfish:run-command "nonsense" "s1")))))

(deftest tinyfish-cell-an-account-with-no-wallet-is-not-a-failure ()
  (with-tinyfish ()
    (tinyfish-answering "/v1/wallet" (tinyfish-json (nlk:json-object "detail" "not found") 404))
    (let ((answer (tinyfish:run-command "usage" "s1")))
      (is (search "no wallet" answer) "404 is legacy billing, an answer")
      (is (not (search "TINYFISH-ERROR" answer))))
    (is (null nodecode-tinyfish::*last-error*))))
