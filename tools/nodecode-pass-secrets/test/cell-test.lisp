;;;; cell-test.lisp --- the pass-secrets cell against its own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The store is real -- a scratch tree of .gpg files the fixture writes, so
;;;; the walk is under test -- and every pass call is stubbed: nothing spawns
;;;; a process, no passphrase is asked, no GPG key is needed, no network is
;;;; reached.

(in-package #:nodecode.test)

(define-test-slice "pass-secrets" "PASS-SECRETS-CELL-" :start nodecode-pass-secrets:start-cell)

(defparameter +root+
  (merge-pathnames "nodecode-pass-secrets-test/" (uiop:temporary-directory))
  "The scratch store every test writes and reads.")

(defparameter +entries+
  '("shared/DATABASE_URL" "shared/OPENAI_API_KEY" "shared/lower-case"
    "shared/nested/DEEP_TOKEN" "work/MULTILINE" "work/OPENAI_API_KEY")
  "The entries the scratch store holds, as pass names: a nested one, one whose
name no environment could hold, and one leaf name spelled in two
subdirectories.")

(defvar *calls* '()
  "Every argv the stub was handed this test, newest first.")

(defun stub-pass (argv)
  "The pass CLI as a test sees it: `--version', `show' for the entries the
fixture wrote, a plain listing, and a refusal for anything else."
  (push argv *calls*)
  (let ((arguments (rest argv)))
    (cond
      ((member "--version" arguments :test #'equal) (values "pass 1.7.4" "" 0))
      ((equal (first arguments) "show")
       (let ((name (third arguments)))
         (cond ((equal name "shared/OPENAI_API_KEY") (values "sk-test-123" "" 0))
               ((equal name "work/OPENAI_API_KEY") (values "sk-test-456" "" 0))
               ((equal name "shared/DATABASE_URL") (values "postgres://localhost/db" "" 0))
               ((equal name "shared/nested/DEEP_TOKEN") (values "deep-value" "" 0))
               ((equal name "work/MULTILINE")
                (values "s3cret
user: admin
url: https://example.test" "" 0))
               (t (values "" (format nil "Error: ~a is not in the password store." name) 1)))))
      ((equal arguments '("ls")) (values "Password Store
|-- shared
|   `-- OPENAI_API_KEY
" "" 0))
      (t (values "" "" 0)))))

(defun scratch-root ()
  "The scratch store's pathname for this test run."
  +root+)

(defun write-store (root)
  "Make ROOT a password store holding +ENTRIES+, each a .gpg file."
  (dolist (name +entries+ root)
    (let ((file (merge-pathnames (format nil "~a.gpg" name) root)))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :supersede)
        (write-string "gpg" out)))))

(defmacro with-store (&body body)
  "BODY with the scratch store on disk and every pass call stubbed."
  `(let* ((root (scratch-root))
          (*calls* '())
          (nodecode-pass-secrets::*run* #'stub-pass))
     (when (probe-file root)
       (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))
     (write-store root)
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(defmacro with-cell (&body body)
  "BODY inside the cell started on the scratch store: both subdirectories."
  `(with-store
     (with-cell-stop ((pass-secrets-start "store" (namestring (scratch-root))
                                          "subdirs" (vector "shared" "work")))
       ,@body)))

(defun wire (tool &rest pairs)
  "TOOL called the way the model calls it, as the text the model reads back."
  (nle::message-content
   (execute-wire-call "pass-secrets-call" tool (apply #'nlk:make-json-object pairs))))

(defun ran (expected description)
  "Assert that the newest pass call the stub was handed is EXPECTED."
  (is (equal expected (first *calls*)) description))

;;; --- the cell's own life ---------------------------------------------------

(define-cell-lifecycle-tests "pass-secrets"
  (:fixture with-store)
  (:config "store" (namestring (scratch-root)))
  (:help :pass-secrets)
  (:command "pass")
  (:running
   (is (equal "pass" (nodecode-pass-secrets::setting :binary)) "the default binary")
   (is (equal '("shared") (nodecode-pass-secrets::setting :subdirs)) "the documented default subdirectory")
   (is (nle::find-tool "pass_list") "the listing is registered")
   (is (nle::find-tool "pass_show") "so is the one read")
   (is (nle::find-tool "pass_env") "and the bulk read")
   (is (nle::find-tool "pass_status") "and the status"))
  (:stopped
   (is (null (nle::find-tool "pass_show")) "and stop takes every one back")
   (is (null (nle::find-tool "pass_env")) "the bulk read too"))
  (:refused ("timeout" 1)))

;;; --- pass_list ---------------------------------------------------------------

(deftest pass-secrets-cell-list-names-entries-and-no-value ()
  (with-cell
    (let ((answer (wire "pass_list")))
      (is (search "6 entries under shared, work" answer) "both configured subdirectories")
      (is (search "shared/nested/DEEP_TOKEN  env: DEEP_TOKEN" answer) "the nested entry is walked to")
      (is (search "shared/lower-case  (its name is no environment variable's)" answer) "an entry no environment could hold is marked, not hidden")
      (is (null (search "sk-test" answer)) "and no value is in a listing"))))

(deftest pass-secrets-cell-list-takes-one-subdirectory ()
  (with-cell
    (let ((answer (wire "pass_list" "subdir" "work")))
      (is (search "2 entries under work" answer) "one subdirectory, as asked")
      (is (null (search "DATABASE_URL" answer)) "and nothing from the other"))))

;;; --- pass_show ------------------------------------------------------------------

(deftest pass-secrets-cell-show-answers-the-value-alone ()
  (with-cell
    (is (equal "sk-test-123" (wire "pass_show" "name" "shared/OPENAI_API_KEY")) "the value, and nothing else")
    (ran '("pass" "show" "--" "shared/OPENAI_API_KEY")
         "one argv, `--' before the entry so a leading dash is no flag")))

(deftest pass-secrets-cell-show-can-take-the-first-line ()
  (with-cell
    (is (equal "s3cret" (wire "pass_show" "name" "work/MULTILINE" "first_line" t)) "the password alone, not the metadata under it")
    (is (search "user: admin" (wire "pass_show" "name" "work/MULTILINE")) "and the whole entry without it")))

(deftest pass-secrets-cell-show-of-an-unknown-entry-says-so ()
  (with-cell
    (let ((report (nodecode-pass-secrets:decode-secret-read
                   (nlk:make-json-object "name" "shared/NOT_THERE"))))
      (is (signals-error nodecode-pass-secrets:pass-secrets-error
                         (nodecode-pass-secrets:read-secret report)) "a cell error, not a silent empty answer"))
    (let ((answer (wire "pass_show" "name" "shared/NOT_THERE")))
      (is (search "is not in the password store" answer) "pass's own words come back")
      (is (search "pass_list" answer) "and the listing to try is named"))))

(deftest pass-secrets-cell-a-name-that-leaves-the-store-refuses ()
  (with-cell
    (dolist (name '("../outside" "/etc/shadow" ""))
      (is (signals-error nodecode-pass-secrets:pass-secrets-error
                         (nodecode-pass-secrets:show name))
          (format nil "~s never reaches pass" name)))
    (is (notany (lambda (argv) (member "show" argv :test #'equal)) *calls*) "and no show was asked of pass")))

(deftest pass-secrets-cell-a-read-without-a-name-refuses ()
  (is (signals-error nlk:contract-refusal
                     (nodecode-pass-secrets:decode-secret-read (nlk:make-json-object))) "the one required member of the boundary"))

;;; --- pass_env --------------------------------------------------------------------

(deftest pass-secrets-cell-env-is-the-store-as-a-mapping ()
  (with-cell
    (let ((answer (wire "pass_env")))
      (is (search "OPENAI_API_KEY=sk-test-123" answer) "the entry named for a variable")
      (is (search "DEEP_TOKEN=deep-value" answer) "a nested one by its leaf name")
      (is (null (search "OPENAI_API_KEY=sk-test-456" answer)) "the first claim of a leaf name wins")
      (is (null (search "lower-case=" answer)) "an entry no environment could hold is skipped")
      (is (search "skipped 2 entries" answer) "and the skip is reported: the duplicate and the bad name"))))

;;; --- pass_status, and the prerequisite ---------------------------------------------

(deftest pass-secrets-cell-status-counts-and-reads-nothing ()
  (with-cell
    (let ((answer (wire "pass_status")))
      (is (search "pass: pass 1.7.4" answer) "the CLI that answered")
      (is (search "shared (4), work (2)" answer) "a count for each subdirectory")
      (is (null (search "sk-test" answer)) "and not one value"))))

(deftest pass-secrets-cell-a-missing-store-is-said-not-signalled ()
  (with-store
    (with-cell-stop ((pass-secrets-start "store" (namestring (merge-pathnames "gone/" (scratch-root)))))
      (let ((answer (wire "pass_status")))
        (is (search "missing" answer) "the store is not there")
        (is (search "pass init" answer) "and the fix is named")))))

;;; --- /pass, and the Lisp read --------------------------------------------------------

(deftest pass-secrets-cell-command-runs-pass-itself ()
  (with-cell
    (let ((answer (nodecode-pass-secrets::run-command "" "s1")))
      (is (search "subdirs: shared (4), work (2)" answer) "no arguments is the status"))
    (let ((answer (nodecode-pass-secrets::run-command "ls" "s1")))
      (is (search "OPENAI_API_KEY" answer) "typed arguments are pass's own listing"))
    (ran '("pass" "ls") "one argv, no shell in between")))

(deftest pass-secrets-cell-a-lisp-read-reaches-the-same-entry ()
  (with-cell
    (is (equal "deep-value" (nodecode-pass-secrets:secret "shared/nested/DEEP_TOKEN")) "the value a form gets, without a tool call")
    (ran '("pass" "show" "--" "shared/nested/DEEP_TOKEN") "through the same one argv")))
