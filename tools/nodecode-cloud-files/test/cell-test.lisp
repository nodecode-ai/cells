;;;; cell-test.lisp --- the cloud-files cell against a scratch folder tree.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test works in a tree of its own under the temporary folder: nothing
;;;; reads or writes the operator's files. Each verb is called the way a
;;;; model's call reaches it -- through the real dispatch, contract and all --
;;;; so the schema the provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "cloud-files" "CLOUD-FILES-CELL-")

;;; --- the fixture tree ---------------------------------------------------------------

(defun fixture-write (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
    (write-string text out)))

(defun make-fixture-tree ()
  "A scratch tree: documents, code, a node_modules, a hidden file, a link out."
  (let* ((root (uiop:ensure-directory-pathname
                (format nil "~acloud-files-test-~d/"
                        (uiop:native-namestring (uiop:temporary-directory))
                        (get-universal-time))))
         (docs (merge-pathnames "docs/" root)))
    (fixture-write (merge-pathnames "readme.md" docs) "# hello")
    (fixture-write (merge-pathnames "notes.txt" docs) "alpha beta")
    (fixture-write (merge-pathnames "main.lisp" root) "(in-package :cl)")
    (fixture-write (merge-pathnames "node_modules/pkg/index.js" root) "module.exports = 1")
    (fixture-write (merge-pathnames ".hidden.md" root) "# hidden")
    #+unix (ignore-errors (sb-posix:symlink "/etc" (uiop:native-namestring (merge-pathnames "escape" root))))
    (uiop:native-namestring root)))

(defparameter *fixture-root* (make-fixture-tree)
  "The tree every test works in, made once.")

(defparameter *fixture-roots* (vector *fixture-root*)
  "The roots list every start is given.")

(defun cloud-files-call (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun cloud-files-object (id tool arguments)
  (let ((answer (cloud-files-call id tool arguments)))
    (handler-case (nlk:decode-json answer)
      (error () (error "~a answered: ~a" tool answer)))))

(defun entry-paths (object)
  (map 'list (lambda (entry) (nlk:json-value entry :string "path"))
       (nlk:json-array object "entries")))

;;; --- the lifecycle, from the one declaration ------------------------------------------

(defun cloud-files-config (&rest pairs)
  "A whole shared config holding the `cloud-files' section PAIRS make."
  (let ((section (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key section) value))
    (nlk:json-object "cloud-files" section)))

(defun cloud-files-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-cloud-files:start-cell (apply #'cloud-files-config pairs)))

(define-cell-lifecycle-tests "cloud-files"
  (:config "roots" *fixture-roots*)
  (:help :cloud-files)
  (:command "files")
  (:refused ("roots" (vector))))

(deftest cloud-files-cell-without-a-root-refuses ()
  ;; The one setting no verb can work without.
  (let ((condition (signals-error nlk:config-refusal
                     (cloud-files-start "roots" (vector)))))
    (is (search "no roots" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "files_list")) "and installs nothing")))

;;; --- files_list ------------------------------------------------------------------------

(deftest cloud-files-cell-lists-a-folder ()
  (with-cell-stop ((cloud-files-start "roots" (vector *fixture-root*)))
    (let* ((object (cloud-files-object "cf-list-1" "files_list" "{}"))
           (roots (nlk:json-array object "roots")))
      (is (= 1 (length roots)) "no root named answers the exposed folders themselves")
      (is (equal "r0" (nlk:json-value (aref roots 0) :string "id")) "with their ids"))
    (let* ((object (cloud-files-object "cf-list-2" "files_list" "{\"root\":\"r0\",\"path\":\"docs\"}"))
           (paths (entry-paths object)))
      (is (equal '("docs/notes.txt" "docs/readme.md") (sort paths #'string-lessp)) "a folder lists its entries, directories first"))
    (let* ((object (cloud-files-object "cf-list-3" "files_list" "{\"root\":\"r0\"}"))
           (paths (entry-paths object)))
      (is (member "docs" paths :test #'equal) "the top level names its folder")
      (is (member ".hidden.md" paths :test #'equal) "and its dot files")
      (is (notany (lambda (path) (search "node_modules/" path)) paths) "one level only"))))

;;; --- the guards -----------------------------------------------------------------------

(deftest cloud-files-cell-refuses-what-leaves-the-root ()
  (with-cell-stop ((cloud-files-start "roots" (vector *fixture-root*)))
    (let ((answer (cloud-files-call "cf-esc-1" "files_list" "{\"root\":\"r0\",\"path\":\"..\"}")))
      (is (search "parent traversal" answer) ".. is refused"))
    (let ((answer (cloud-files-call "cf-esc-2" "files_list" "{\"root\":\"r0\",\"path\":\"/etc\"}")))
      (is (search "absolute" answer) "an absolute path is refused"))
    (let ((answer (cloud-files-call "cf-esc-3" "files_read" "{\"root\":\"r0\",\"path\":\"escape/passwd\"}")))
      (is (search "leaves" answer) "a symbolic link out of the root is refused"))
    (let ((answer (cloud-files-call "cf-esc-4" "files_list" "{\"root\":\"r9\"}")))
      (is (search "not an exposed folder" answer) "an unknown root is refused"))))

;;; --- files_read and files_save ---------------------------------------------------------

(deftest cloud-files-cell-reads-and-saves-atomically ()
  (with-cell-stop ((cloud-files-start "roots" (vector *fixture-root*)))
    (let* ((read (cloud-files-object "cf-read-1" "files_read" "{\"root\":\"r0\",\"path\":\"docs/readme.md\"}"))
           (digest (nlk:json-value read :string "sha256")))
      (is (equal "# hello" (nlk:json-value read :string "text")) "the file comes back as text")
      (is (equal (nlk:sha256-text "# hello") digest) "with the sha256 of what was read")
      (let ((saved (cloud-files-object
                    "cf-save-1" "files_save"
                    (format nil "{\"root\":\"r0\",\"path\":\"docs/readme.md\",\"text\":\"# hello again\",\"base_sha256\":\"~a\"}"
                            digest))))
        (is (eq t (nlk:json-value saved :boolean "ok")) "a save with the sha256 just read goes through")
        (is (equal (nlk:sha256-text "# hello again") (nlk:json-value saved :string "sha256")) "and answers the new sha256"))
      (let ((conflict (cloud-files-object
                       "cf-save-2" "files_save"
                       (format nil "{\"root\":\"r0\",\"path\":\"docs/readme.md\",\"text\":\"stale\",\"base_sha256\":\"~a\"}"
                               digest))))
        (is (eq t (nlk:json-value conflict :boolean "conflict")) "a save against the sha256 from before is a conflict, not an overwrite")
        (is (equal "# hello again" (nlk:json-value conflict :string "text")) "and it carries the file as it is now"))
      (let ((refused (cloud-files-call "cf-save-3" "files_save"
                                        (format nil "{\"root\":\"r0\",\"path\":\"docs/readme.md\",\"text\":\"x\",\"base_sha256\":\"\"}"))))
        (is (search "exists" refused) "an existing file without a base sha256 is refused"))
      (let ((refused (cloud-files-call "cf-save-4" "files_save"
                                        "{\"root\":\"r0\",\"path\":\"main.lisp\",\"text\":\"x\"}")))
        (is (search "Markdown" refused) "only Markdown and text files are saved")))))

;;; --- files_search and files_mkdir ------------------------------------------------------

(deftest cloud-files-cell-searches-by-name ()
  (with-cell-stop ((cloud-files-start "roots" (vector *fixture-root*)))
    (let* ((object (cloud-files-object "cf-search-1" "files_search" "{\"root\":\"r0\",\"query\":\"readme\"}"))
           (paths (map 'list (lambda (entry) (nlk:json-value entry :string "path"))
                       (nlk:json-array object "results"))))
      (is (equal '("docs/readme.md") paths) "a substring finds the file below a folder"))
    (let* ((object (cloud-files-object "cf-search-2" "files_search" "{\"root\":\"r0\",\"query\":\"*.js\"}"))
           (paths (map 'list (lambda (entry) (nlk:json-value entry :string "path"))
                       (nlk:json-array object "results"))))
      (is (not (search "node_modules" (format nil "~{~a~}" paths))) "heavy folders are skipped, so a glob inside one does not match"))))

(deftest cloud-files-cell-creates-a-folder ()
  (with-cell-stop ((cloud-files-start "roots" (vector *fixture-root*)))
    (let ((object (cloud-files-object "cf-mkdir-1" "files_mkdir" "{\"root\":\"r0\",\"path\":\"inbox/2026\"}")))
      (is (equal '("inbox" "2026") (coerce (nlk:json-array object "created") 'list)) "every missing folder is created and named")
      (is (uiop:directory-exists-p (merge-pathnames "inbox/2026/" *fixture-root*)) "and it is on disk"))
    (let ((object (cloud-files-object "cf-mkdir-2" "files_mkdir" "{\"root\":\"r0\",\"path\":\"inbox\"}")))
      (is (eq t (nlk:json-value object :boolean "exists")) "a folder that is there already says so"))))
