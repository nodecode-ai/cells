;;;; cell-test.lisp --- lifecycle, the store, the scrub, the gate, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring and the three layers the
;;;; original shipped: a start installs one advice, three tools, one command
;;;; and one manual topic and a stop takes them all back; a key is stored under
;;;; a name and answers a fingerprint and a length, never a value; every tool
;;;; result comes back with the stored values scrubbed, and a value too short
;;;; to scrub is left alone; a call that reads a credential store is refused
;;;; before it runs, while an allowed pattern and a plain call go through; and
;;;; the command stores, forgets and verifies without ever echoing a value.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "secret-guard"
  (:hooks :tool)
  (:command "secret")
  (:help :secret-guard)
  (:running
   (is (nle::find-tool "secret_request") "the request tool is registered")
   (is (nle::find-tool "secret_list") "and the list")
   (is (nle::find-tool "secret_check") "and the check")
   (is (eq t (nodecode-secret-guard::setting :redact-tool-results)) "results are scrubbed by default")
   (is (eq t (nodecode-secret-guard::setting :block-secret-reads)) "and credential reads are refused")
   (is (eq t (nodecode-secret-guard::setting :export-env)) "and stored keys reach the environment"))
  (:stopped
   (is (null (guard-hook)) "stop takes the advice off :tool")
   (is (null (nle::find-tool "secret_list")) "and unregisters the tools"))
  (:refused ("allow" 5)))

;;; --- the store -------------------------------------------------------------------

(deftest secret-guard-cell-a-key-answers-a-fingerprint-and-a-length
    (with-cell-stop ((apply #'secret-guard-start (fresh-store)))
  (guard-env "SG_TEST_SOURCE" +sample-key+)
  (let ((answer (guard-slash "add OPENAI_API_KEY SG_TEST_SOURCE")))
    (is (search "stored OPENAI_API_KEY" answer) "the key is stored under its name")
    (is (search (nodecode-secret-guard:fingerprint +sample-key+) answer) "the answer carries the fingerprint")
    (is (search (format nil "~d characters" (length +sample-key+)) answer) "and the length")
    (is (not (search +sample-key+ answer)) "and never the value"))
  (let ((listing (guard-list)))
    (is (search "OPENAI_API_KEY" listing) "the listing names it")
    (is (search "SG_TEST_SOURCE" listing) "and says where it came from")
    (is (not (search +sample-key+ listing)) "and never its value"))
  (is (equal +sample-key+ (nodecode-secret-guard:secret "OPENAI_API_KEY")) "a form reads the value with (secret)")
  (is (search "OPENAI_API_KEY" (guard-request "OPENAI_API_KEY" "the image model")) "secret_request says it is stored")
  #-win32
  (let ((file (sb-posix:stat (namestring (nodecode-secret-guard::store-path)))))
    (is (= #o600 (logand #o777 (sb-posix:stat-mode file))) "the store is private to the user"))))

(deftest secret-guard-cell-the-command-forgets-and-verifies (with-cell-stop ((apply #'secret-guard-start (fresh-store)))
  (guard-env "SG_TEST_FORGET" +sample-key+)
  (guard-slash "add GROQ_API_KEY SG_TEST_FORGET")
  (is (search "ok" (guard-slash "verify GROQ_API_KEY")) "a value that matches its fingerprint verifies")
  (setf (nodecode-secret-guard:held-secret-value (nodecode-secret-guard:held "GROQ_API_KEY")) "somethingelse")
  (is (search "CHANGED" (guard-slash "verify GROQ_API_KEY")) "a value changed outside the cell is reported")
  (is (search "forgot GROQ_API_KEY" (guard-slash "remove GROQ_API_KEY")) "remove forgets it")
  (is (search "no secret named GROQ_API_KEY" (guard-list "GROQ_API_KEY")) "and the listing knows no such name")))

(deftest secret-guard-cell-a-source-that-is-neither-variable-nor-file-refuses
    (with-cell-stop ((apply #'secret-guard-start (fresh-store)))
  (is (search "neither an environment variable that is set nor a file that exists" (guard-slash "add NOWHERE_KEY SG_TEST_NOWHERE")) "nothing is stored and the command says why")
  (is (search "no secrets held" (guard-slash "")) "the store is still empty")
  (is (search "not a name a variable could have" (guard-slash "add 9bad SG_TEST_NOWHERE")) "a name no variable could have is refused")))

;;; --- the scrub ---------------------------------------------------------------------

(deftest secret-guard-cell-every-result-comes-back-scrubbed (with-cell-stop ((apply #'secret-guard-start (fresh-store)))
  (guard-env "SG_TEST_SCRUB" +sample-key+)
  (guard-slash "add ANTHROPIC_API_KEY SG_TEST_SCRUB")
  (let ((answer (funcall (guard-hook)
                          (list :name "eval" :arguments (nlk:json-object "form" "(sh \"env-x\")")
                                :call-id "c-1")
                          (lambda (op) (declare (ignore op)) (format nil "the key is ~a, all of it" +sample-key+)))))
    (is (search "[secret:ANTHROPIC_API_KEY redacted]" answer) "the value comes back as the original's marker")
    (is (not (search +sample-key+ answer)) "and the value itself is gone"))
  (is (equal "nothing to see here" (guard-passes "nothing to see here")) "a result with no secret is handed back unchanged")))

(deftest secret-guard-cell-a-short-value-is-held-but-not-scrubbed (with-cell-stop ((apply #'secret-guard-start (fresh-store)))
  (guard-env "SG_TEST_SHORT" +short-value+)
  (guard-slash "add SHORT_PIN SG_TEST_SHORT")
  (is (search "too short to scrub" (guard-slash "list SHORT_PIN")) "the store says it is not scrubbed")
  (is (equal "pin abc9 and a 9" (guard-passes "pin abc9 and a 9")) "four characters are left where they were")))

(deftest secret-guard-cell-secret-check-reads-a-draft (with-cell-stop ((apply #'secret-guard-start (fresh-store)))
  (guard-env "SG_TEST_CHECK" +sample-key+)
  (guard-slash "add OPENAI_API_KEY SG_TEST_CHECK")
  (let ((answer (guard-check (format nil "here is my config: OPENAI_API_KEY=~a" +sample-key+))))
    (is (search "LEAKED" answer) "a draft carrying a stored value is leaked")
    (is (search "OPENAI_API_KEY" answer) "the name is said")
    (is (not (search +sample-key+ answer)) "and the value is scrubbed out of the evidence")
    (is (search "[secret:OPENAI_API_KEY redacted]" answer) "leaving the draft a person can share"))
  (is (search "CLEAR" (guard-check "the build is green and the port is 8080")) "a plain draft is clear")))

;;; --- the gate ----------------------------------------------------------------------

(deftest secret-guard-cell-a-credential-read-is-refused (with-cell-stop ((apply #'secret-guard-start (fresh-store)))
  (multiple-value-bind (answer ran) (guard-call "eval" "form" (format nil "(sh \"~a\")" +credential-path+))
    (is (search "ERROR: refused by nodecode-secret-guard" answer) "a reader pointed at a credential store is refused")
    (is (search "credentials" answer) "the file is named")
    (is (null ran) "and the call never ran"))
  (multiple-value-bind (answer ran) (guard-call "eval" "form" "(sh \"printenv\")")
    (is (search "dumps the whole environment" answer) "a bare printenv is refused")
    (is (null ran) "before it runs"))
  (multiple-value-bind (answer ran) (guard-call "eval" "form" "(sh \"security find-generic-password -w -s x\")")
    (is (search "keychain" answer) "and a keychain print")
    (is (null ran) "before it runs"))
  (multiple-value-bind (answer ran) (guard-call "eval" "form" "(sh \"ls -la ~/src\")")
    (is (equal "the call answered" answer) "a plain call is handed to next")
    (is (equal t ran) "and it runs"))))

(deftest secret-guard-cell-the-gate-obeys-its-switch
    (with-cell-stop ((apply #'secret-guard-start (fresh-store "block_secret_reads" nil)))
  (multiple-value-bind (answer ran) (guard-call "eval" "form" (format nil "(sh \"~a\")" +credential-path+))
    (is (equal "the call answered" answer) "with the gate off the read runs")
    (is (equal t ran) "and next is called"))))

(deftest secret-guard-cell-an-allow-pattern-exempts-a-call
    (with-cell-stop ((apply #'secret-guard-start (fresh-store "allow" (vector "credentials"))))
  (multiple-value-bind (answer ran) (guard-call "eval" "form" (format nil "(sh \"~a\")" +credential-path+))
    (is (equal "the call answered" answer) "an allow pattern exempts the call")
    (is (equal t ran) "so it runs"))))
