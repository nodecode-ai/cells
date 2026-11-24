;;;; cell-test.lisp --- the lifecycle, the three tools, /roam, the sealed store.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved: the cell starts and stops; each tool answers through
;;;; the seam the model calls it by; a push writes one sealed bundle and
;;;; the content is not in the clear; a second push is the next version;
;;;; a pull previews, and only writes with allow_agent_apply, after
;;;; backing up what it replaces; a wrong passphrase writes nothing; an
;;;; URL store goes over the one HTTP seam, stubbed here; a bundle naming
;;;; a path outside the home is refused; /roam says where things stand; a
;;;; missing store refuses by naming the member. Every file a test makes
;;;; lives under this run's own scratch home, and no test dials anything.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defun roam-test-folder (name)
  "NAME as a namestring under the run's own scratch home."
  (namestring (nlk:home name)))

(defun roam-store-folder () (roam-test-folder "roam-store/"))
(defun roam-pass-path () (roam-test-folder "roam-pass.txt"))
(defun roam-other-pass-path () (roam-test-folder "roam-other-pass.txt"))

(defun roam-reset ()
  "One test's home: the scope, the store and what roam remembers, gone;
two passphrases to choose between."
  (dolist (name '("roam-scope/" "roam-store/" ".roam/"))
    (ignore-errors (uiop:delete-directory-tree (nlk:home name) :validate t :if-does-not-exist :ignore)))
  (nlk:write-file-atomically (nlk:home "roam-pass.txt") "correct horse battery staple" :mode :private)
  (nlk:write-file-atomically (nlk:home "roam-other-pass.txt") "a different horse" :mode :private))

(defun roam-write (relative text)
  "RELATIVE under the home written as TEXT."
  (let ((path (nlk:home relative)))
    (ensure-directories-exist path)
    (nlk:write-file-atomically path text)
    path))

(defun roam-read (relative)
  (uiop:read-file-string (nlk:home relative)))

(defun roam-start-here (&rest pairs)
  "The cell started over this run's folder store and scope, with the
passphrase in a file: no test depends on the environment."
  (apply #'roam-start (append (list "store" (roam-store-folder)
                                    "include" (vector "roam-scope/")
                                    "passphrase_env" ""
                                    "passphrase_file" (roam-pass-path))
                              pairs)))

(defun roam-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun roam-answer (text)
  "TEXT -- a tool's answer -- decoded, as the model reads it."
  (handler-case (nlk:decode-json text)
    (error (condition) (error "~a: ~a" condition text))))

(defun roam-refusal (name arguments)
  "The ERROR line tool NAME answers ARGUMENTS with."
  (let ((text (roam-tool name arguments)))
    (assert (and (stringp text) (uiop:string-prefix-p "ERROR: roam:" text))
            () "not a refusal: ~a" text)
    text))

(defun roam-bundle-path ()
  (merge-pathnames "roam-default.nroam" (nlk:home "roam-store/")))

;;; --- the lifecycle -------------------------------------------------------------

(define-test-slice "roam" "ROAM-CELL-" :start nodecode-roam:start-cell)

(define-cell-lifecycle-tests "roam"
  (:config "store" (roam-store-folder)
           "include" (vector "roam-scope/")
           "passphrase_env" ""
           "passphrase_file" (roam-pass-path))
  (:help :roam)
  (:command "roam")
  (:running (is (nle::find-tool "roam_status") "roam_status is registered")
            (is (nle::find-tool "roam_push"))
            (is (nle::find-tool "roam_pull"))
            (is (pathnamep (nodecode-roam::state-file)) "the folder it remembers itself in")
            (is (equal "default" (nodecode-roam::bundle-profile)) "one bundle a profile"))
  (:stopped (is (null (nle::find-tool "roam_push")) "stop unregisters the tools"))
  (:refused ("store" 5))
  (:idle nodecode-roam:roam-error (nodecode-roam::run-command "" nil)))

(deftest roam-cell-a-missing-store-refuses-by-name ()
  (roam-reset)
  (with-cell-stop ((roam-start "include" (vector "roam-scope/")))
    (is-carrying (text (roam-refusal "roam_status" (nlk:json-object)))
                 "store" "not set")))

;;; --- the push ------------------------------------------------------------------

(deftest roam-cell-a-push-writes-one-sealed-bundle ()
  (roam-reset)
  (roam-write "roam-scope/config.jsonc" "{\"mark\":\"SECRET-MARK\"}")
  (roam-write "roam-scope/cells/mine/cell.lisp" ";; mine")
  (with-cell-stop ((roam-start-here))
    (let ((pushed (roam-answer (roam-tool "roam_push" (nlk:json-object "note" "first")))))
      (is (= 2 (nlk:json-value pushed :integer "files")) "both files are in the bundle")
      (is (= 1 (nlk:json-value pushed :integer "version")))
      (let ((bundle (nodecode-roam::read-octets (roam-bundle-path))))
        (is (< 32 (length bundle)) "the store holds the bundle")
        (is (not (search (nodecode-roam::text-octets "SECRET-MARK") bundle)) "the content is sealed: the file's text is not in the clear")
        (is (equal "default" (nlk:json-value (nodecode-roam::bundle-head bundle) :string "profile")) "and its head reads without the passphrase")))
    (let ((status (roam-answer (roam-tool "roam_status" (nlk:json-object)))))
      (is (= 1 (nlk:json-value status :integer "remote" "version")) "status sees the store's version")
      (is (null (nlk:json-value status :boolean "local" "changedSinceLastPush")) "and that this home has not changed since")
      (is (null (nlk:json-value status :boolean "drift"))))
    (let ((second (roam-answer (roam-tool "roam_push" (nlk:json-object)))))
      (is (= 2 (nlk:json-value second :integer "version")) "a second push is the next version"))))

;;; --- the pull ------------------------------------------------------------------

(deftest roam-cell-a-pull-previews-then-restores-with-a-backup ()
  (roam-reset)
  (roam-write "roam-scope/config.jsonc" "first")
  (with-cell-stop ((roam-start-here))
    (roam-tool "roam_push" (nlk:json-object))
    (roam-write "roam-scope/config.jsonc" "second")
    (is-carrying (text (roam-refusal "roam_pull" (nlk:json-object "apply" t)))
                 "allow_agent_apply")
    (is (equal "second" (roam-read "roam-scope/config.jsonc")) "a pull the section does not allow writes nothing")
    (let ((plan (roam-answer (roam-tool "roam_pull" (nlk:json-object)))))
      (is (null (nlk:json-value plan :boolean "applied")) "a preview says so")
      (is (= 1 (nlk:json-value plan :integer "updated")) "and what it would replace"))
    (with-cell-stop ((roam-start-here "allow_agent_apply" t))
      (let ((applied (roam-answer (roam-tool "roam_pull" (nlk:json-object "apply" t)))))
        (is (eq t (nlk:json-value applied :boolean "applied")))
        (is (= 1 (nlk:json-value applied :integer "updated"))))
      (is (equal "first" (roam-read "roam-scope/config.jsonc")) "the store's copy is back")
      (let ((backups (nodecode-roam::backup-directories)))
        (is (= 1 (length backups)) "the replaced file was backed up")
        (let ((held (uiop:read-file-string
                     (merge-pathnames "roam-scope/config.jsonc"
                                      (merge-pathnames (format nil "~a/" (first backups))
                                                       (nodecode-roam::backup-root))))))
          (is (equal "second" held) "and the backup holds what was replaced"))))))

(deftest roam-cell-a-wrong-passphrase-refuses-and-writes-nothing ()
  (roam-reset)
  (roam-write "roam-scope/config.jsonc" "kept")
  (with-cell-stop ((roam-start-here))
    (roam-tool "roam_push" (nlk:json-object)))
  (roam-write "roam-scope/config.jsonc" "changed here")
  (with-cell-stop ((roam-start-here "passphrase_file" (roam-other-pass-path)
                                    "allow_agent_apply" t))
    (is-carrying (text (roam-refusal "roam_pull" (nlk:json-object "apply" t)))
                 "does not authenticate")
    (is (equal "changed here" (roam-read "roam-scope/config.jsonc")) "nothing was written under a passphrase that cannot open the bundle")))

(deftest roam-cell-a-bundle-that-points-outside-the-home-is-refused ()
  (roam-reset)
  (with-cell-stop ((roam-start-here))
    (let* ((files (vector (nlk:json-object "path" "../escaped.txt" "size" 1
                                            "sha256" "00" "text" "x")))
           (manifest (nlk:json-object "format" 1 "savedAt" "2026-01-01T00:00:00Z"
                                       "profile" "default" "contentSha256" "00" "files" files))
           (head (nlk:json-object "format" 1 "version" 1 "savedAt" "2026-01-01T00:00:00Z"
                                   "profile" "default" "files" 1 "bytes" 1 "contentSha256" "00"))
           (bundle (nodecode-roam::seal-bundle (nodecode-roam::passphrase)
                                                (nlk:encode-json-octets head)
                                                (nlk:encode-json-octets manifest)))
           (path (roam-bundle-path)))
      (ensure-directories-exist path)
      (nlk:write-file-atomically path bundle))
    (is-carrying (text (roam-refusal "roam_pull" (nlk:json-object)))
                 "not a path inside the home")
    (is (not (probe-file (nlk:home "../escaped.txt"))) "nothing was written outside the home")))

;;; --- the URL store ---------------------------------------------------------------

(deftest roam-cell-a-url-store-goes-over-the-one-http-seam ()
  (roam-reset)
  (roam-write "roam-scope/config.jsonc" "over the wire")
  (let ((stored nil) (asked '()))
    (with-stubbed-fdefinition (nlk:http (method url &rest args)
                               (push (list* method url args) asked)
                               (case method
                                 (:put (setf stored (getf args :content)) (values "" 200))
                                 (:get (if stored (values stored 200) (values "" 404)))
                                 (t (values "" 500))))
      (with-cell-stop ((roam-start-here "store" "https://store.example/roam-bundle"))
        (let ((before (roam-answer (roam-tool "roam_status" (nlk:json-object)))))
          (is (equal "http" (nlk:json-value before :string "backend")))
          (is (null (nlk:json-value before :object "remote")) "an empty store answers no remote"))
        (let ((pushed (roam-answer (roam-tool "roam_push" (nlk:json-object)))))
          (is (= 1 (nlk:json-value pushed :integer "version")))
          (is (eq :put (first (first asked))) "the write is one PUT to the store's URL")
          (is (equal "https://store.example/roam-bundle" (second (first asked))))
          (is (not (null stored)) "the store was handed the sealed bundle"))
        (let ((plan (roam-answer (roam-tool "roam_pull" (nlk:json-object)))))
          (is (= 1 (nlk:json-value plan :integer "same")) "the bundle read back over the wire is this home's own content"))))))

;;; --- /roam -------------------------------------------------------------------------

(deftest roam-cell-the-command-says-where-things-stand ()
  (roam-reset)
  (roam-write "roam-scope/config.jsonc" "one")
  (with-cell-stop ((roam-start-here))
    (let* ((bare (multiple-value-list (nodecode-roam::run-command "" "s1")))
           (pushed (nodecode-roam::run-command "push" "s1"))
           (diff (nodecode-roam::run-command "diff" "s1"))
           (backups (nodecode-roam::run-command "backups" "s1")))
      (is-carrying (text (first bare)) "roam: profile default" "local" "store" "scope")
      (is (not (null (second bare))) "bare /roam offers what an operator does from there")
      (is-carrying pushed "pushed version 1")
      (is-carrying diff "would add" "leave 1 alone")
      (is-carrying backups "no backup yet"))))

(deftest roam-cell-the-command-refuses-what-it-does-not-know ()
  (roam-reset)
  (with-cell-stop ((roam-start-here))
    (let ((refused (signals-error nodecode-roam:roam-error
                                   (nodecode-roam::run-command "sideways" "s1"))))
      (is (not (null refused)) "a verb it does not know refuses"))))







