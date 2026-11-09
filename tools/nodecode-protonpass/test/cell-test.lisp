;;;; cell-test.lisp --- the Proton Pass cell against a stubbed pass-cli.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test installs pass-cli, needs a Proton account or touches the
;;;; network: every read goes through NODECODE-PROTONPASS::*RUN*, a stub that
;;;; records the command line it was handed and answers what the test chose.
;;;; What is under test is the part this cell owns — the argv a tool builds,
;;;; the reference it accepts, the cache it keeps, and the string it answers.

(in-package #:nodecode.test)

(define-test-slice "protonpass" "PROTONPASS-CELL-" :start nodecode-protonpass:start-cell)

(define-cell-lifecycle-tests "protonpass"
  (:help :protonpass)
  (:command "protonpass")
  (:running
   (is (equal '("pass-cli") (nodecode-protonpass::setting :command))
       "the default command is pass-cli on PATH")
   (is (nle::find-tool "protonpass_get") "the one-reference read is registered")
   (is (nle::find-tool "protonpass_list") "so is the vault listing")
   (is (nle::find-tool "protonpass_vault") "and the bulk read")
   (is (nle::find-tool "protonpass_status") "and the status"))
  (:stopped
   (is (null (nle::find-tool "protonpass_get")) "and stop takes every one back")
   (is (null (nle::find-tool "protonpass_status"))))
  (:refused ("command" 5)
            ("timeout_seconds" 0)
            ("cache_ttl_seconds" -1)))

;;; --- the stub --------------------------------------------------------------

(defvar *calls* nil
  "The command lines the stub was handed, newest first.")

(defun stub (&rest answers)
  "A *RUN* stub: ANSWERS are (OUT ERR STATUS) triples, taken in order, the
last one standing for every call after them. Each call's argv goes on *CALLS*."
  (let ((remaining (or (copy-list answers) (list (list "" "" 0)))))
    (lambda (argv &key environment)
      (declare (ignore environment))
      (push argv *calls*)
      (destructuring-bind (out err status)
          (if (rest remaining) (pop remaining) (first remaining))
        (values out err status)))))

(defmacro with-pass-cli (answers &body body)
  "BODY with the cell started and pass-cli answered by a stub: ANSWERS is a
list of (OUT ERR STATUS) triples, taken in call order, the last standing for
every call after them."
  `(let ((*calls* nil)
         (nodecode-protonpass::*run*
           (stub ,@(loop for answer in answers collect `(list ,@answer)))))
     (with-cell-stop ((protonpass-start))
       ,@body)))

(defun pass-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "protonpass-call" name arguments)))

(defun ran (expected description)
  "Assert that the newest run the stub was handed is EXPECTED, saying WHY."
  (is (equal expected (first *calls*)) description))

(defparameter +vault-json+
  "{\"items\": [{\"content\": {\"title\": \"OPENAI_API_KEY\", \"content\": {\"Login\": {\"password\": \"sk-1\"}}, \"extra_fields\": [{\"name\": \"api key\", \"content\": {\"text\": \"custom-1\"}}]}}, {\"content\": {\"title\": \"my note\", \"content\": {\"Note\": {\"note\": \"hi\"}}}}]}"
  "An `item list --output json` answer: one env-var-titled item with a typed
password and a custom field, and one item whose title is no env-var name.")

;;; --- one reference is one item view ---------------------------------------

(deftest protonpass-cell-a-reference-is-one-item-view ()
  (with-pass-cli (("sk-live-123" "" 0))
    (is (equal "sk-live-123"
               (pass-call "protonpass_get" "{\"reference\": \"pass://Hermes/OpenAI/password\"}"))
        "the answer is the value alone")
    (ran '("pass-cli" "item" "view" "--" "pass://Hermes/OpenAI/password")
         "one argv-only item view, `--` before the reference so a leading dash is no flag")
    (pass-call "protonpass_get" "{\"reference\": \"Hermes/OpenAI/api key\", \"name\": \"OPENAI_API_KEY\"}")
    (ran '("pass-cli" "item" "view" "--" "pass://Hermes/OpenAI/api key")
         "the scheme is optional in the call and written out for pass-cli")))

(deftest protonpass-cell-a-reference-of-two-segments-is-refused ()
  (with-pass-cli ()
    (let ((answer (pass-call "protonpass_get" "{\"reference\": \"pass://Hermes/OpenAI\"}")))
      (is (search "three segments" answer) "the model is told what a reference is")
      (is (null *calls*) "and pass-cli never ran"))))

(deftest protonpass-cell-a-call-without-a-reference-is-refused ()
  (with-pass-cli ()
    (let ((answer (pass-call "protonpass_get" "{}")))
      (is (search "reference" answer) "the refusal names the member the call left out")
      (is (null *calls*) "and nothing ran"))))

(deftest protonpass-cell-a-value-is-cached-for-the-ttl ()
  (with-pass-cli (("one" "" 0))
    (pass-call "protonpass_get" "{\"reference\": \"pass://A/b/password\"}")
    (pass-call "protonpass_get" "{\"reference\": \"pass://A/b/password\"}")
    (is (= 1 (length *calls*)) "the second call reads this image's cache, not pass-cli")
    (pass-call "protonpass_get" "{\"reference\": \"pass://A/b/password\", \"fresh\": true}")
    (is (= 2 (length *calls*)) "fresh reads past it"))
  (with-pass-cli (("one" "" 0))
    (with-cell-stop ((protonpass-start "cache_ttl_seconds" 0))
      (pass-call "protonpass_get" "{\"reference\": \"pass://A/b/password\"}")
      (pass-call "protonpass_get" "{\"reference\": \"pass://A/b/password\"}")
      (is (= 2 (length *calls*)) "0 leaves no cache at all"))))

;;; --- a vault --------------------------------------------------------------

(deftest protonpass-cell-a-vault-read-takes-the-env-var-titled-items ()
  (with-pass-cli ((+vault-json+ "" 0))
    (let ((answer (pass-call "protonpass_vault" "{\"vault\": \"Hermes\"}")))
      (ran '("pass-cli" "item" "list" "--filter-state" "active" "--show-secrets"
             "--output" "json" "--" "Hermes")
           "one listing that carries the secrets, `--` before the vault")
      (is (search "OPENAI_API_KEY=sk-1" answer)
          "the item titled like an environment variable contributes its field")
      (is (search "1 variable" answer) "and only that one")
      (is (not (search "my note" answer)) "an item with a plain title is not one"))
    (let ((answer (pass-call "protonpass_vault" "{\"vault\": \"Hermes\", \"field\": \"api key\"}")))
      (is (search "OPENAI_API_KEY=custom-1" answer) "a custom field is read by its name"))))

(deftest protonpass-cell-a-listing-names-the-items-without-a-value ()
  (with-pass-cli ((+vault-json+ "" 0))
    (let ((answer (pass-call "protonpass_list" "{\"vault\": \"Hermes\"}")))
      (ran '("pass-cli" "item" "list" "--filter-state" "active" "--output" "json" "--" "Hermes")
           "a listing without --show-secrets: no value crosses")
      (is (search "OPENAI_API_KEY" answer) "every title is named")
      (is (search "my note" answer) "including the ones no env name")
      (is (not (search "sk-1" answer)) "and no value at all"))))

(deftest protonpass-cell-a-call-that-names-no-vault-asks-for-one ()
  (with-pass-cli ()
    (let ((answer (pass-call "protonpass_list" "{}")))
      (is (search "no vault named" answer) "the refusal says what is missing")
      (is (search "protonpass section" answer) "and which setting would answer it")
      (is (null *calls*) "nothing ran"))))

(deftest protonpass-cell-the-section-s-vault-and-field-stand-in ()
  (with-pass-cli ((+vault-json+ "" 0))
    (with-cell-stop ((protonpass-start "vault" "Work" "field" "username"))
      (let ((answer (pass-call "protonpass_vault" "{}")))
        (is (member "Work" (first *calls*) :test #'equal)
            "the section's vault is the one read")
        (is (search "field username" answer)
            "and the section's field is the one taken")))))

;;; --- the token -----------------------------------------------------------

(deftest protonpass-cell-a-dead-session-is-renewed-once-with-the-token ()
  (let ((nodecode-protonpass::*getenv*
          (lambda (name) (and (equal name "PROTONPASS_TOKEN") "tok-1"))))
    (let ((*calls* nil)
          (nodecode-protonpass::*run*
            (stub (list "" "This operation requires an authenticated client" 1)
                  (list "" "" 0)
                  (list "sk-1" "" 0))))
      (with-cell-stop ((protonpass-start "token_env" "PROTONPASS_TOKEN"))
        (is (equal "sk-1" (pass-call "protonpass_get" "{\"reference\": \"pass://A/b/password\"}"))
            "the read that failed is retried after the login")
        (is (equal '("pass-cli" "login") (second *calls*))
            "the token login ran between the two")
        (is (equal '("pass-cli" "item" "view" "--" "pass://A/b/password") (third *calls*))
            "and the read before it")))))

(deftest protonpass-cell-a-login-without-a-token-names-the-variable ()
  (with-pass-cli ()
    (let ((answer (refusal-text nodecode-protonpass::protonpass-error
                     (nodecode-protonpass::login))))
      (is (search "PROTON_PASS_PERSONAL_ACCESS_TOKEN" answer)
          "the refusal names the variable to fill")
      (is (search "pass-cli login" answer) "and the terminal's own way")
      (is (null *calls*) "nothing was run"))))

;;; --- what pass-cli inherits ------------------------------------------------

(deftest protonpass-cell-the-child-is-told-why-and-what-it-may-use ()
  (with-pass-cli ()
    (let ((environment (nodecode-protonpass::environment :reason "Nodecode: load OPENAI_API_KEY")))
      (is (member "PROTON_PASS_DISABLE_TELEMETRY=1" environment :test #'equal)
          "pass-cli's telemetry is off")
      (is (member "PROTON_PASS_AGENT_REASON=Nodecode: load OPENAI_API_KEY" environment
                  :test #'equal)
          "and the read's reason reaches its audit log")
      (is (notany (lambda (entry) (uiop:string-prefix-p "PROTON_PASS_SESSION_DIR" entry))
                  environment)
          "with no token there is no session of the cell's own: the login this process sees is used")))
  (let ((nodecode-protonpass::*getenv*
          (lambda (name) (and (equal name "PROTON_PASS_PERSONAL_ACCESS_TOKEN") "tok-2"))))
    (with-pass-cli ()
      (let ((login (nodecode-protonpass::environment :reason "Nodecode: sign in" :login t)))
        (is (member "PROTON_PASS_PERSONAL_ACCESS_TOKEN=tok-2" login :test #'equal)
            "a login carries the token in the child's environment, never argv")
        (is (some (lambda (entry) (uiop:string-prefix-p "PROTON_PASS_SESSION_DIR=" entry)) login)
            "and lands in a session directory of the cell's own")))))

;;; --- the operator's command ----------------------------------------------

(deftest protonpass-cell-the-command-reports-without-reading-a-value ()
  (with-pass-cli (("pass-cli 2.4.2" "" 0) ("" "" 0))
    (let ((answer (nodecode-protonpass::run-command "" nil)))
      (is (search "pass-cli 2.4.2" answer) "the CLI's own version")
      (is (search "signed in: yes" answer) "and that the session answers")
      (is (= 2 (length *calls*)) "one --version and one info, and no item read"))))

(deftest protonpass-cell-the-command-reports-the-mappings-as-a-dry-run ()
  (with-pass-cli ()
    (with-cell-stop ((protonpass-start "mappings" #("OPENAI_API_KEY=pass://Hermes/OpenAI/password")))
      (let ((answer (nodecode-protonpass::run-command "env" nil)))
        (is (search "OPENAI_API_KEY <- pass://Hermes/OpenAI/password" answer)
            "the mapping is named")
        (is (search "would export" answer) "and what a startup would do with it")
        (is (null *calls*) "a dry run reads nothing")))))

(deftest protonpass-cell-the-command-reads-one-reference-as-asked ()
  (with-pass-cli (("sk-9" "" 0))
    (is (equal "sk-9" (nodecode-protonpass::run-command "get pass://Hermes/OpenAI/password" nil))
        "/protonpass get answers the value, because it was asked for")
    (is (search "usage" (nodecode-protonpass::run-command "get" nil))
        "and says how when it names none")))
