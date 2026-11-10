;;;; cell-test.lisp --- lifecycle, the checkpoint tool, the seal, restore, retention.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring and its storage: a start
;;;; installs the tool, /ark and the manual and a stop takes them back; the
;;;; checkpoint tool writes a manifest naming the config and the cells folder;
;;;; credentials are never in it; a passphrase seals the objects and a restore
;;;; gives a file back byte for byte; the wrong passphrase or none at all is
;;;; refused; a second snapshot stores only what changed; the policy prunes
;;;; and the status notices a brain that shrank. Everything is a folder in
;;;; the scratch home, so nothing here dials anything.

(in-package #:nodecode.test)

(define-test-slice "ark" "ARK-CELL-"
  :start nodecode-ark:start-cell
  :defaults (("kdf_iterations" 1000)))

;;; --- the scratch home -----------------------------------------------------------

(defun ark-put (relative text)
  "TEXT at RELATIVE under the scratch home."
  (let ((path (nlk:home relative)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create
                               :element-type '(unsigned-byte 8))
      (write-sequence (sb-ext:string-to-octets text :external-format :utf-8) out))
    path))

(defun ark-get (relative)
  "RELATIVE's bytes under the scratch home, as a string."
  (with-open-file (in (nlk:home relative) :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      (sb-ext:octets-to-string bytes :external-format :utf-8))))

(defun ark-reset ()
  "A scratch home holding none of the last test's files or repository."
  (dolist (folder '("ark/" "cells/"))
    (let ((path (nlk:home folder)))
      (when (uiop:directory-exists-p path)
        (uiop:delete-directory-tree path :validate t :if-does-not-exist :ignore))))
  (uiop:delete-file-if-exists (nlk:home "config.jsonc"))
  nil)

(defmacro with-ark-fresh (&body body)
  "BODY in a scratch home with no repository and no cells from an earlier test."
  `(progn (ark-reset) ,@body))

(defun ark-tool (arguments)
  "The checkpoint tool called through the wire with the JSON object ARGUMENTS."
  (nle::message-content
   (execute-wire-call "call-1" "ark_checkpoint" (nlk:encode-json-object arguments))))

(defun ark-command (&rest words)
  "One /ark line, typed as WORDS."
  (nodecode-ark::run-command (format nil "~{~a~^ ~}" words) nil))

(defun ark-paths (manifest)
  "The file names MANIFEST holds."
  (mapcar (lambda (entry) (nlk:json-value entry :string "path"))
          (nodecode-ark::snapshot-files manifest)))

;;; --- the declaration ------------------------------------------------------------

(define-cell-lifecycle-tests "ark"
  (:config "kdf_iterations" 1000)
  (:fixture with-ark-fresh)
  (:help :ark)
  (:command "ark")
  (:running (is (nle::find-tool "ark_checkpoint") "ark_checkpoint is registered")
            (is (equal "ark" (car (last (pathname-directory
                                          (nodecode-ark::repository-root))))) "with no repository set the store is this home's own ark folder"))
  (:stopped (is (null (nle::find-tool "ark_checkpoint")) "stop unregisters the tool"))
  (:refused ("kdf_iterations" 100) ("keep_last" 0) ("max_file_mb" 0)))

;;; --- what a checkpoint captures ----------------------------------------------------

(deftest ark-cell-a-checkpoint-captures-config-and-cells ()
  (with-ark-fresh)
  (with-cell-stop ((ark-start "kdf_iterations" 1000))
    (ark-put "config.jsonc" "{\"model\":\"fixture\"}\n")
    (ark-put "cells/you/note.lisp" "(defparameter *note* 1)\n")
    (let* ((text (ark-tool (nlk:json-object "label" "before editing memory")))
           (answer (nlk:decode-json text)))
      (is (eq t (nlk:json-value answer :boolean "ok")) "the tool answers ok")
      (is (plusp (length (or (nlk:json-value answer :string "snapshot_id") ""))) "and a snapshot id")
      (is (search "/ark restore PATH" (nlk:json-value answer :string "restore_hint")) "and says how to put one back")
      (let ((manifest (nodecode-ark::find-snapshot nil)))
        (is (= 2 (nodecode-ark::snapshot-count manifest)) "the config and the cell file are both in it")
        (is (equal "before editing memory" (nodecode-ark::snapshot-label manifest)) "the label is kept")
        (is (member "cells/you/note.lisp" (ark-paths manifest) :test #'string=) "the brain is captured")
        (is (= 1 (nlk:json-value manifest :integer "sequence")) "the first snapshot is ordinal one")))))

(deftest ark-cell-credentials-and-git-are-never-captured ()
  (with-ark-fresh)
  (with-cell-stop ((ark-start "kdf_iterations" 1000))
    (ark-put "cells/you/note.lisp" "(defparameter *note* 1)\n")
    (ark-put "cells/.env" "API_KEY=secret\n")
    (ark-put "cells/auth.json" "{}\n")
    (ark-put "cells/keys/pairing.key" "secret\n")
    (ark-put "cells/.git/config" "[core]\n")
    (nodecode-ark::create-snapshot :label "t" :trigger "test" :skip-unchanged nil)
    (let ((paths (ark-paths (nodecode-ark::find-snapshot nil))))
      (is (member "cells/you/note.lisp" paths :test #'string=) "the brain is in")
      (is (not (member "cells/.env" paths :test #'string=)) ".env is a credential, not brain")
      (is (not (member "cells/auth.json" paths :test #'string=)) "and auth.json is a credential too")
      (is (notany (lambda (path) (uiop:string-suffix-p ".key" path)) paths) "no key file is captured")
      (is (not (member "cells/.git/config" paths :test #'string=)) "the cells folder's own git is skipped"))))

;;; --- the seal, and one file back ---------------------------------------------------

(deftest ark-cell-a-passphrase-seals-and-restore-returns-the-file ()
  (with-ark-fresh)
  (with-stubbed-fdefinition (uiop:getenv (name)
                             (and (string= name "ARK_TEST_PASSPHRASE") "correct horse battery"))
    (with-cell-stop ((ark-start "kdf_iterations" 1000 "passphrase_env" "ARK_TEST_PASSPHRASE"))
      (ark-put "cells/you/secret.lisp" "(defparameter *plaintext-marker* 42)\n")
      (let* ((manifest (nodecode-ark::create-snapshot :label "sealed" :trigger "test" :skip-unchanged nil))
             (entry (nodecode-ark::item-in manifest "cells/you/secret.lisp"))
             (object (nlk:json-value entry :string "object"))
             (blob (nodecode-ark::read-octets (nodecode-ark::object-path object))))
        (is (eq t (nlk:json-value manifest :boolean "encrypted")) "the manifest says it is sealed")
        (is (not (search (sb-ext:string-to-octets "plaintext-marker") blob)) "the stored bytes do not spell the file")
        (is (not (search (sb-ext:string-to-octets "cells/you") blob)) "and the store does not leak the path either")
        (delete-file (nlk:home "cells/you/secret.lisp"))
        (is (search "restored cells/you/secret.lisp"
                    (ark-command "restore" "cells/you/secret.lisp"
                                 (nodecode-ark::snapshot-id manifest) "yes")) "the command says what it restored")
        (is (equal "(defparameter *plaintext-marker* 42)\n" (ark-get "cells/you/secret.lisp")) "and the file is back, byte for byte")))))

(deftest ark-cell-a-restore-is-a-preview-until-yes ()
  (with-ark-fresh)
  (with-cell-stop ((ark-start "kdf_iterations" 1000))
    (ark-put "cells/you/note.lisp" "first\n")
    (let ((manifest (nodecode-ark::create-snapshot :label "t" :trigger "test" :skip-unchanged nil)))
      (ark-put "cells/you/note.lisp" "second\n")
      (is (search "would restore" (ark-command "restore" "cells/you/note.lisp")) "without yes it only says what it would do")
      (is (equal "second\n" (ark-get "cells/you/note.lisp")) "and the file on disk is untouched")
      (ark-command "restore" "cells/you/note.lisp" (nodecode-ark::snapshot-id manifest) "yes")
      (is (equal "first\n" (ark-get "cells/you/note.lisp")) "with yes the older version is back")
      (is (search "not a file inside the home"
                  (ark-command "restore" "../outside.lisp" "yes")) "a path that would leave the home is refused"))))

(deftest ark-cell-a-wrong-or-missing-passphrase-is-refused ()
  (with-ark-fresh)
  (with-stubbed-fdefinition (uiop:getenv (name)
                             (and (string= name "ARK_TEST_PASSPHRASE") "right"))
    (with-cell-stop ((ark-start "kdf_iterations" 1000 "passphrase_env" "ARK_TEST_PASSPHRASE"))
      (ark-put "cells/you/note.lisp" "x\n")
      (nodecode-ark::create-snapshot :label "t" :trigger "test" :skip-unchanged nil)))
  (with-stubbed-fdefinition (uiop:getenv (name)
                             (and (string= name "ARK_TEST_PASSPHRASE") "wrong"))
    (with-cell-stop ((ark-start "kdf_iterations" 1000 "passphrase_env" "ARK_TEST_PASSPHRASE"))
      (is (search "does not open" (ark-command "snap")) "a wrong passphrase is named as such")))
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (with-cell-stop ((ark-start "kdf_iterations" 1000 "passphrase_env" "ARK_TEST_PASSPHRASE"))
      (is (search "encrypted" (ark-command "snap")) "and so is a missing one"))))

;;; --- dedup, retention, and the health line -----------------------------------------

(deftest ark-cell-a-second-snapshot-stores-only-what-changed ()
  (with-ark-fresh)
  (with-cell-stop ((ark-start "kdf_iterations" 1000))
    (ark-put "cells/a.lisp" "one\n")
    (ark-put "cells/b.lisp" "two\n")
    (let ((first (nodecode-ark::create-snapshot :label "first" :trigger "test" :skip-unchanged nil)))
      (is (= 2 (nlk:json-value first :integer "new_objects")) "both files are stored once")
      (ark-put "cells/b.lisp" "two, edited\n")
      (let ((second (nodecode-ark::create-snapshot :label "second" :trigger "test")))
        (is (= 1 (nlk:json-value second :integer "new_objects")) "only the changed file is stored again")
        (is (= 2 (length (nodecode-ark::snapshot-manifests))) "two snapshots")
        (is (= 2 (nlk:json-value second :integer "sequence")) "in the order they were taken")))
    (let ((again (nodecode-ark::create-snapshot :label "again" :trigger "test")))
      (is (= 2 (length (nodecode-ark::snapshot-manifests))) "an unchanged home takes no new snapshot")
      (is (equal "second" (nodecode-ark::snapshot-label again)) "and answers the last one"))))

(deftest ark-cell-prune-keeps-the-policy-and-frees-what-none-refers-to ()
  (with-ark-fresh)
  (with-cell-stop ((ark-start "kdf_iterations" 1000 "keep_last" 1
                               "keep_hourly" 0 "keep_daily" 0 "keep_monthly" 0))
    (dotimes (index 3)
      (ark-put "cells/x.lisp" (format nil "version ~d~%" index))
      (nodecode-ark::create-snapshot :label (format nil "v~d" index) :trigger "test" :skip-unchanged nil))
    (is (= 3 (length (nodecode-ark::snapshot-manifests))) "three snapshots")
    (is (search "2 snapshots beyond the policy" (ark-command "prune")) "two are beyond the policy")
    (is (= 1 (length (nodecode-ark::snapshot-manifests))) "one is kept")
    (is (search "Verified 1 file" (ark-command "verify")) "what is kept still opens")))

(deftest ark-cell-status-notices-a-brain-that-shrank ()
  (with-ark-fresh)
  (with-cell-stop ((ark-start "kdf_iterations" 1000))
    (dotimes (index 4) (ark-put (format nil "cells/m~d.lisp" index) "x\n"))
    (nodecode-ark::create-snapshot :label "four" :trigger "test" :skip-unchanged nil)
    (dotimes (index 3) (delete-file (nlk:home (format nil "cells/m~d.lisp" index))))
    (let ((status (ark-command "status")))
      (is (search "0 added, 0 changed, 3 removed" status) "the drift since the snapshot is counted")
      (is (search "THE BRAIN SHRANK" status) "and a sudden loss is called out")))
  (with-ark-fresh)
  (with-cell-stop ((ark-start "kdf_iterations" 1000))
    (is (search "No repository yet" (ark-command "status")) "with no repository the status says so and how to fix it")
    (is (search "/ark snap" (ark-command "status")) "and points at the first snapshot")))
