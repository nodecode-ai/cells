;;;; cell-test.lisp --- the cell against a stubbed GitHub.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every GitHub endpoint is a stubbed HTTP-SEND and the token a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches GitHub, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it --
;;;; through the real dispatch, contract and all -- so the schema the provider
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "github-app" "GITHUB-APP-CELL-")

(defun github-app-config (&rest pairs) "A whole shared config holding the `github_app' section PAIRS make, with a
token variable named and the gh CLI left alone unless a test asks for it."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "token_env" section "GITHUB_TOKEN")
    (alexandria:ensure-gethash "gh_cli" section nil)
    (nlk:json-object "github_app" section)))

(defun github-app-start (&rest pairs) "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-github-app:start-cell (apply #'github-app-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-github-token (&body body) "BODY with the token environment answering one fixed token."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "GITHUB_TOKEN") "test-token"))
     ,@body))

(defmacro with-github-wire ((asked &rest routes) &body body) "BODY with HTTP-SEND answering by URL. ROUTES are (FRAGMENT FORM), tried in
order -- the most specific first -- and FORM runs with METHOD, URL and OPTIONS
bound and answers (values TEXT STATUS), as NLK:HTTP does. ASKED collects
(METHOD URL CONTENT HEADERS), oldest first, once BODY reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nodecode-github-app::http-send (method url &rest options)
                                   (push (list method url (getf options :content)
                                               (getf options :headers))
                                         ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected GitHub request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun ga-object (id tool arguments) "TOOL's answer to ARGUMENTS, as the model reads it, decoded."
  (nlk:decode-json (nle::message-content (execute-wire-call id tool arguments))))

(defun ga-text (id tool arguments)
  (nle::message-content (execute-wire-call id tool arguments)))

(defun ga-header (call name)
  (cdr (assoc name (fourth call) :test #'string-equal)))

;;; --- what GitHub answers -------------------------------------------------------------

(defparameter +login+ "{\"login\":\"octo\",\"type\":\"User\"}" "GET /user: the token's own login.")

(defparameter +repos+ "[{\"full_name\":\"o/r\"},{\"full_name\":\"o/other\"}]" "GET /user/repos.")

(defparameter +issues+
  (concatenate 'string "[{\"number\":12,\"title\":\"Crashes on empty input\",\"state\":\"open\"," "\"user\":{\"login\":\"alice\"},\"labels\":[{\"name\":\"bug\"}]," "\"created_at\":\"2026-01-01T09:00:00Z\",\"updated_at\":\"2026-01-02T10:00:00Z\"}," "{\"number\":13,\"title\":\"Add a parser\",\"state\":\"open\"," "\"user\":{\"login\":\"bob\"},\"labels\":[],\"pull_request\":{\"url\":\"x\"}," "\"created_at\":\"2026-01-01T09:00:00Z\",\"updated_at\":\"2026-01-02T10:00:00Z\"}]") "GET /repos/o/r/issues: one issue and one pull request.")

(defparameter +posted+ "{\"number\":14,\"html_url\":\"https://github.com/o/r/issues/14\",\"state\":\"open\",\"title\":\"Crashes on empty input\"}" "POST /repos/o/r/issues.")

(defparameter +merged+ "{\"merged\":true,\"message\":\"Pull Request successfully merged\",\"sha\":\"abc123\"}" "PUT /repos/o/r/pulls/9/merge.")

(defparameter +pulls+ "{\"number\":9,\"head\":{\"sha\":\"1111111111111111111111111111111111111111\"}}" "GET /repos/o/r/pulls/9.")

(defparameter +review+ "{\"id\":77,\"state\":\"APPROVED\",\"html_url\":\"https://github.com/o/r/pull/9#pullrequestreview-77\"}" "POST /repos/o/r/pulls/9/reviews.")

(defparameter +sha+ "1111111111111111111111111111111111111111" "A full 40-character head SHA.")

;;; --- the lifecycle, from the one declaration ------------------------------------------

(define-cell-lifecycle-tests "github-app"
  (:config "token_env" "GITHUB_TOKEN" "gh_cli" nil)
  (:help :github-app)
  (:command "github-app")
  (:fixture with-github-token)
  (:refused ("token_env" "" "gh_cli" nil)
            ("app_id" "123456" "gh_cli" nil)))

;;; --- github_identity --------------------------------------------------------------------

(deftest github-app-cell-identity-names-the-actor (with-github-token)
  (with-cell-stop ((github-app-start "token_env" "GITHUB_TOKEN" "gh_cli" nil))
    (with-github-wire (asked ("/user/repos" (values +repos+ 200))
                             ("/user" (values +login+ 200)))
      (let ((object (ga-object "ga-1" "github_identity" "{}")))
        (is (eq t (nlk:json-value object :boolean "success")))
        (is (equal "pat" (nlk:json-value object :string "auth_method")) "a token is a pat, as the original named it")
        (is (equal "octo" (nlk:json-value object :string "actor")) "the token's own login")
        (is (equal '("o/r" "o/other")
                   (loop for repo across (nlk:json-array object "accessible_repos") collect repo)) "and the repositories it reaches")))))

;;; --- github_list_issues ------------------------------------------------------------------

(deftest github-app-cell-lists-issues-and-pull-requests (with-github-token)
  (with-cell-stop ((github-app-start "token_env" "GITHUB_TOKEN" "gh_cli" nil))
    (with-github-wire (asked ("/repos/o/r/issues" (values +issues+ 200))
                             ("/user" (values +login+ 200)))
      (let* ((object (ga-object "ga-2" "github_list_issues" "{\"repo\":\"o/r\"}"))
             (rows (nlk:json-array object "issues")))
        (is (= 2 (nlk:json-value object :integer "count")) (ga-text "ga-2" "github_list_issues" "{\"repo\":\"o/r\"}"))
        (is (equal "Crashes on empty input" (nlk:json-value (aref rows 0) :string "title")))
        (is (equal "alice" (nlk:json-value (aref rows 0) :string "user")))
        (is (null (nlk:json-value (aref rows 0) :boolean "pull_request")) "an issue is not a pull request")
        (is (nlk:json-value (aref rows 1) :boolean "pull_request") "and a pull request says so")
        (is (equal "octo" (nlk:json-value (nlk:json-value object :object "attribution") :string "actor")) "the answer says who asked")
        (let ((call (first asked)))
          (is (cl:search "state=open" (second call)) "the default state rides the query")
          (is (cl:search "per_page=30" (second call)) "and the default page size"))))))

(deftest github-app-cell-a-repository-must-be-explicit (with-github-token)
  (with-cell-stop ((github-app-start "token_env" "GITHUB_TOKEN" "gh_cli" nil))
    (with-github-wire (asked)
      (let ((object (ga-object "ga-3" "github_list_issues" "{\"repo\":\"o/r/issues\"}")))
        (is (null (nlk:json-value object :boolean "success")))
        (is (cl:search "owner/name" (nlk:json-value (nlk:json-value object :object "error") :string "message")) "the refusal says the shape")
        (is (null asked) "and nothing was dialled")))))

;;; --- the write gate -------------------------------------------------------------------------

(deftest github-app-cell-a-write-is-refused-until-the-gate-is-open (with-github-token)
  (with-cell-stop ((github-app-start "token_env" "GITHUB_TOKEN" "gh_cli" nil))
    (with-github-wire (asked)
      (dolist (call '(("github_create_issue" "{\"repo\":\"o/r\",\"title\":\"Crashes\"}")
                      ("github_comment_issue" "{\"repo\":\"o/r\",\"number\":1,\"body\":\"hi\"}")
                      ("github_review_pr" "{\"repo\":\"o/r\",\"number\":9,\"event\":\"APPROVE\",\"commit_id\":\"1111111111111111111111111111111111111111\"}")
                      ("github_merge_pr" "{\"repo\":\"o/r\",\"number\":9,\"expected_head_sha\":\"1111111111111111111111111111111111111111\"}")))
        (let ((object (ga-object "ga-4" (first call) (second call))))
          (is (null (nlk:json-value object :boolean "success")))
          (is (equal "write_actions_disabled"
                     (nlk:json-value (nlk:json-value object :object "error") :string "code")) "the original's own refusal code")))
      (is (null asked) "no write reached GitHub"))))

(deftest github-app-cell-an-open-gate-lets-an-issue-be-created (with-github-token)
  (with-cell-stop ((github-app-start "token_env" "GITHUB_TOKEN" "gh_cli" nil "allow_write_actions" t))
    (with-github-wire (asked ("/repos/o/r/issues" (values +posted+ 201))
                             ("/user" (values +login+ 200)))
      (let ((object (ga-object "ga-5" "github_create_issue" "{\"repo\":\"o/r\",\"title\":\"Crashes on empty input\",\"labels\":[\"bug\"]}")))
        (is (eq t (nlk:json-value object :boolean "success")) (ga-text "ga-5" "github_create_issue" "{\"repo\":\"o/r\",\"title\":\"Crashes on empty input\",\"labels\":[\"bug\"]}"))
        (is (= 14 (nlk:json-value object :integer "issue_number")))
        (is (cl:search "github.com/o/r/issues/14" (nlk:json-value object :string "url")))
        (is (equal "octo" (nlk:json-value (nlk:json-value object :object "attribution") :string "actor")) "the answer says which identity opened it"))
      (let ((call (first asked)))
        (is (eq :post (first call)) "one POST")
        (is (cl:search "\"title\":\"Crashes on empty input\"" (third call)) "with the title")
        (is (cl:search "\"labels\":[\"bug\"]" (third call)) "and the labels")
        (is (equal "Bearer test-token" (ga-header call "authorization")) "as the token's own user")))))

;;; --- reviews and merges ------------------------------------------------------------------------

(deftest github-app-cell-a-stale-head-is-refused-before-the-review (with-github-token)
  (with-cell-stop ((github-app-start "token_env" "GITHUB_TOKEN" "gh_cli" nil "allow_write_actions" t))
    (with-github-wire (asked ("/pulls/9" (values +pulls+ 200)))
      (let ((object (ga-object "ga-6" "github_review_pr" "{\"repo\":\"o/r\",\"number\":9,\"event\":\"APPROVE\",\"commit_id\":\"2222222222222222222222222222222222222222\"}")))
        (is (null (nlk:json-value object :boolean "success")))
        (is (cl:search "head moved"
                       (nlk:json-value (nlk:json-value object :object "error") :string "message")) "the review never reaches a head it did not read")
        (is (= 1 (length asked)) "only the reading request")))))

(deftest github-app-cell-a-review-rides-the-commit-it-read (with-github-token)
  (with-cell-stop ((github-app-start "token_env" "GITHUB_TOKEN" "gh_cli" nil "allow_write_actions" t))
    (with-github-wire (asked ("/reviews" (values +review+ 200))
                             ("/pulls/9" (values +pulls+ 200))
                             ("/user" (values +login+ 200)))
      (let ((object (ga-object "ga-7" "github_review_pr" "{\"repo\":\"o/r\",\"number\":9,\"event\":\"APPROVE\",\"commit_id\":\"1111111111111111111111111111111111111111\",\"body\":\"Looks right.\"}")))
        (is (eq t (nlk:json-value object :boolean "success")) (ga-text "ga-7" "github_review_pr" "{\"repo\":\"o/r\",\"number\":9,\"event\":\"APPROVE\",\"commit_id\":\"1111111111111111111111111111111111111111\",\"body\":\"Looks right.\"}"))
        (is (= 77 (nlk:json-value object :integer "review_id"))))
      (let ((post (find-if (lambda (call) (eq (first call) :post)) asked)))
        (is post "the review was submitted")
        (is (cl:search "\"event\":\"APPROVE\"" (third post)) "the event is upper-cased, as GitHub wants it")
        (is (cl:search "\"commit_id\":\"1111111111111111111111111111111111111111\"" (third post)) "against the head it read")
        (is (cl:search "\"body\":\"Looks right.\"" (third post)))))))

(deftest github-app-cell-a-merge-needs-the-full-head-sha (with-github-token)
  (with-cell-stop ((github-app-start "token_env" "GITHUB_TOKEN" "gh_cli" nil "allow_write_actions" t))
    (with-github-wire (asked)
      (let ((object (ga-object "ga-8" "github_merge_pr" "{\"repo\":\"o/r\",\"number\":9,\"expected_head_sha\":\"abc\"}")))
        (is (null (nlk:json-value object :boolean "success")))
        (is (cl:search "40-character"
                       (nlk:json-value (nlk:json-value object :object "error") :string "message")))
        (is (null asked) "and GitHub was not asked to merge anything")))))

(deftest github-app-cell-a-merge-sends-its-method-and-sha (with-github-token)
  (with-cell-stop ((github-app-start "token_env" "GITHUB_TOKEN" "gh_cli" nil "allow_write_actions" t))
    (with-github-wire (asked ("/merge" (values +merged+ 200))
                             ("/user" (values +login+ 200)))
      (let ((object (ga-object "ga-9" "github_merge_pr"
                                (format nil "{\"repo\":\"o/r\",\"number\":9,\"expected_head_sha\":\"~a\",\"method\":\"rebase\"}" +sha+))))
        (is (eq t (nlk:json-value object :boolean "merged")) (ga-text "ga-9" "github_merge_pr" (format nil "{\"repo\":\"o/r\",\"number\":9,\"expected_head_sha\":\"~a\",\"method\":\"rebase\"}" +sha+)))
        (is (equal "abc123" (nlk:json-value object :string "sha"))))
      (let ((call (first asked)))
        (is (eq :put (first call)))
        (is (cl:search "\"merge_method\":\"rebase\"" (third call)))
        (is (cl:search "\"sha\":\"1111111111111111111111111111111111111111\"" (third call)))))))

;;; --- the App's own identity ---------------------------------------------------------------------

(deftest github-app-cell-the-private-key-is-read-as-der ()
  ;; SEQUENCE { version 0, n 5, e 3, d 3 }: what the DER reader must see.
  (let ((der (coerce '(#x30 #x0c #x02 #x01 #x00 #x02 #x01 #x05
                        #x02 #x01 #x03 #x02 #x01 #x03)
                     '(vector (unsigned-byte 8)))))
    (multiple-value-bind (n d) (nodecode-github-app::rsa-numbers der)
      (is (= 5 n) "the modulus")
      (is (= 3 d) "and the private exponent"))))

(deftest github-app-cell-the-jwt-claims-the-app (with-github-token)
  (let* ((der (coerce '(#x30 #x0c #x02 #x01 #x00 #x02 #x01 #x05
                         #x02 #x01 #x03 #x02 #x01 #x03)
                      '(vector (unsigned-byte 8))))
         (pem (format nil "-----BEGIN RSA PRIVATE KEY-----~%~a~%-----END RSA PRIVATE KEY-----"
                      (nodecode-github-app::base64url der))))
    (with-cell-stop ((github-app-start "app_id" "123456" "installation_id" "45678901" "private_key" pem "gh_cli" nil))
      (with-stubbed-fdefinition (nodecode-github-app::rsa-sign (message n d)
                                  (declare (ignore message n d))
                                  (make-array 4 :element-type '(unsigned-byte 8)
                                                :initial-element 7))
        (let* ((jwt (nodecode-github-app::app-jwt :now 1000000))
               (payload (second (uiop:split-string jwt :separator (list #\.))))
               (claims (nlk:decode-json
                        (map 'string #'code-char (nodecode-github-app::base64-decode payload)))))
          (is (= 3 (length (uiop:split-string jwt :separator (list #\.)))) "header, payload and signature")
          (is (equal "123456" (nlk:json-value claims :string "iss")) "the App ID is the issuer")
          (is (= 1000480 (nlk:json-value claims :integer "exp")) "issued a minute early, valid nine")
          (let* ((header (first (uiop:split-string jwt :separator (list #\.))))
                 (fields (nlk:decode-json
                          (map 'string #'code-char (nodecode-github-app::base64-decode header)))))
            (is (equal "RS256" (nlk:json-value fields :string "alg")) "signed RS256")))))))

(deftest github-app-cell-an-installation-token-is-minted-and-attributed (with-github-token)
  (let* ((der (coerce '(#x30 #x0c #x02 #x01 #x00 #x02 #x01 #x05
                         #x02 #x01 #x03 #x02 #x01 #x03)
                      '(vector (unsigned-byte 8))))
         (pem (format nil "-----BEGIN RSA PRIVATE KEY-----~%~a~%-----END RSA PRIVATE KEY-----"
                      (nodecode-github-app::base64url der))))
    (with-cell-stop ((github-app-start "app_id" "123456" "installation_id" "45678901" "private_key" pem "app_slug" "nodecode-agent" "gh_cli" nil))
      (with-stubbed-fdefinition (nodecode-github-app::rsa-sign (message n d)
                                  (declare (ignore message n d))
                                  (make-array 4 :element-type '(unsigned-byte 8)
                                                :initial-element 7))
        (with-github-wire (asked ("/installation/repositories" (values "{\"repositories\":[{\"full_name\":\"o/r\"}]}" 200))
                             ("/access_tokens" (values "{\"token\":\"ghs_install\"}" 201)))
          (let ((object (ga-object "ga-10" "github_identity" "{}")))
            (is (equal "github-app" (nlk:json-value object :string "auth_method")))
            (is (equal "nodecode-agent[bot]" (nlk:json-value object :string "actor")) "the bot attribution the original insisted on"))
          (let ((call (first asked)))
            (is (eq :post (first call)))
            (is (cl:search "/app/installations/45678901/access_tokens" (second call)))
            (is (cl:search "Bearer " (or (ga-header call "authorization") "")) "the JWT signs the exchange")))))))
