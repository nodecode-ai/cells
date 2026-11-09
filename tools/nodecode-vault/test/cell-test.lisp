;;;; cell-test.lisp --- lifecycle, the five tools, the command, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a real store in a
;;;; scratch folder and a stubbed command runner: that a start installs the
;;;; five tools, /vault and the manual, and a stop takes every one of them
;;;; back; that a wrong setting refuses; that a value put in comes back out
;;;; while the file on disk holds no plaintext; that a wrong key is refused
;;;; rather than half-read; that a store nothing was written to is empty, not
;;;; a failure; that a missing key names setup and creates nothing; that
;;;; vault_run hands the mapped variables to the command and not to the
;;;; transcript; and that /vault answers every verb.

(in-package #:nodecode.test)

(define-test-slice "vault" "VAULT-CELL-" :start nodecode-vault:start-cell)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "vault"
  (:help :vault)
  (:command "vault")
  (:running
   (is (nle::find-tool "vault_list") "the list tool is registered")
   (is (nle::find-tool "vault_get") "the get tool is registered")
   (is (nle::find-tool "vault_put") "the put tool is registered")
   (is (nle::find-tool "vault_remove") "the remove tool is registered")
   (is (nle::find-tool "vault_run") "the run tool is registered")
   (is (null (nodecode-vault::setting :store)) "the store path is unset until the operator names one")
   (is (= 60 (nodecode-vault::setting :timeout-seconds))))
  (:refused ("timeout_seconds" 0) ("store" 5) ("env" 5))
  (:stopped
   (is (null (nle::find-tool "vault_get")) "stop unregisters the tools")
   (is (null (nle::find-tool "vault_put")))
   (is (null (nle::find-tool "vault_run")))))

;;; --- a store of its own ------------------------------------------------------------

(defvar *vault-cell-stores* 0 "How many scratch stores this run made.")

(defmacro with-vault ((&rest pairs) &body body)
  "BODY with the cell started on a fresh store and key file under this run's
home, and any PAIRS added to the section."
  `(let* ((base (format nil "vault-cell-~d" (incf *vault-cell-stores*)))
          (store (namestring (nlk:home (format nil "~a/store.sexp" base))))
          (key (namestring (nlk:home (format nil "~a/master.key" base)))))
     (with-cell-stop ((vault-start "store" store "key_file" key ,@pairs))
       ,@body)))

(defun vault-call (tool value)
  "Run the registered TOOL on VALUE, as a model's call would."
  (funcall (third (nle::find-tool tool)) value))

(defun vault-store-text ()
  (uiop:read-file-string (nodecode-vault::store-path)))

;;; --- the tools --------------------------------------------------------------------

(deftest vault-cell-a-value-put-in-comes-back-out-and-the-file-is-ciphertext ()
  (with-vault ()
    (vault:setup)
    (is (search "stored demo.key"
                (vault-call "vault_put" (nodecode-vault::make-put-call
                                          :item "demo" :field "key" :value "sk-test-1234"
                                          :label "Demo"))))
    (is (equal "sk-test-1234"
               (vault-call "vault_get" (nodecode-vault::make-get-call :ref "hv:demo:key"))) "the value comes back through the seal")
    (is (equal "sk-test-1234" (vault:secret "hv:demo:key")) "and through the verb")
    (let ((text (vault-store-text)))
      (is (not (search "sk-test-1234" text)) "the file holds no plaintext")
      (is (not (search "demo" text)) "not even the item id"))
    (let ((line (vault-call "vault_list" (nodecode-vault::make-list-call))))
      (is (search "demo" line) "the list names the item")
      (is (search "key" line) "and its fields")
      (is (not (search "sk-test-1234" line)) "never a value"))
    (is (search "removed demo"
                (vault-call "vault_remove" (nodecode-vault::make-remove-call :item "demo"))))
    (is (equal "(empty vault)"
               (vault-call "vault_list" (nodecode-vault::make-list-call))) "an empty vault answers, it does not fail")
    (is (not (search "sk-test-1234" (vault-store-text))) "a removed value is gone from the file")))

(deftest vault-cell-a-wrong-key-is-refused-not-half-read ()
  (with-vault ()
    (vault:setup)
    (vault:put "demo" "key" "sk-test-1234")
    (let ((key (nodecode-vault::key-file-path)))
      (with-open-file (out key :direction :output :if-exists :supersede)
        (format out "~a~%" (make-string 64 :initial-element #\a))))
    (is (search "ERROR: VAULT-ERROR"
                (vault-call "vault_get" (nodecode-vault::make-get-call :ref "hv:demo:key"))) "the tag does not verify, so the read fails loudly")
    (is (search "did not open" nodecode-vault::*last-error*))))

(deftest vault-cell-without-a-key-a-read-names-setup-and-creates-nothing ()
  (with-vault ()
    (is (search "run /vault setup once"
                (vault-call "vault_get" (nodecode-vault::make-get-call :ref "hv:demo:key"))) "the refusal names the way back")
    (is (null (probe-file (nodecode-vault::key-file-path))) "a read makes no key")
    (is (null (probe-file (nodecode-vault::store-path))) "and no store")
    (is (search "ERROR: VAULT-ERROR"
                (vault-call "vault_get" (nodecode-vault::make-get-call :ref "demo"))) "a ref that is not hv: is refused too")))

(deftest vault-cell-run-hands-a-command-the-mapped-variables-and-not-the-transcript ()
  (with-vault ("env" (vector "DEMO_TOKEN=hv:demo:token"))
    (vault:setup)
    (vault:put "demo" "token" "sk-live-9")
    (is (equal '(("DEMO_TOKEN" . "hv:demo:token")) (vault:mapping)) "the section's mapping")
    (let ((seen '()))
      (let ((nodecode-vault::*shell-with-env*
              (lambda (command env) (push (list command env) seen) "the command's output")))
        (is (equal "the command's output"
                   (vault-call "vault_run" (nodecode-vault::make-run-call
                                             :command "printenv DEMO_TOKEN"))) "the tool answers the command's output"))
      (is (equal "printenv DEMO_TOKEN" (first (first seen))) "the command as given")
      (is (equal "sk-live-9" (cdr (assoc "DEMO_TOKEN" (second (first seen)) :test #'equal))) "the secret is in the command's environment, not in the answer"))
    (is (equal "sk-live-9" (string-trim '(#\Newline #\Space) (vault:run "printenv DEMO_TOKEN"))) "and the real runner lays it over this process's own environment")))

;;; --- the command ------------------------------------------------------------------

(deftest vault-cell-the-command-answers-every-verb ()
  (with-vault ()
    (is (search "vault ready" (cell-entry "nodecode-vault" "vault" "setup")))
    (is (search "stored demo.key" (cell-entry "nodecode-vault" "vault" "put demo key sk-1")))
    (is (equal "demo: key (set)" (cell-entry "nodecode-vault" "vault" "show hv:demo")) "show masks the value")
    (is (equal "sk-1" (cell-entry "nodecode-vault" "vault" "get hv:demo:key")) "get answers it")
    (is (search "DEMO=hv:demo" (cell-entry "nodecode-vault" "vault" "env demo DEMO")) "env prints the mapping to paste into the section")
    (is (search "items: 1" (cell-entry "nodecode-vault" "vault" "doctor")))
    (is (search "removed demo" (cell-entry "nodecode-vault" "vault" "rm demo")))
    (is (search "unknown /vault verb" (cell-entry "nodecode-vault" "vault" "nope")) "a verb it does not know is said so, not signalled")))

;;; --- the refusals -----------------------------------------------------------------

(deftest vault-cell-a-missing-member-or-setting-refuses ()
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-vault::get-call) (nlk:json-object))) "a call with no ref is refused before any body runs")
  (with-vault ()
    (is (signals-error nlk:config-refusal (vault-start "timeout_seconds" 0)) "an integer under its minimum refuses at start")
    (is (signals-error nlk:config-refusal (vault-start "env" 5)) "a list that is not a list refuses at start")))
