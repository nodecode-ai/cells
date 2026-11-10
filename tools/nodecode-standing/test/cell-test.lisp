;;;; cell-test.lisp --- standing lifecycle, what a call is read as, the gate,
;;;; the three proofs, and the record the command shows.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here is offline: a page is a stub, a file is one this test
;;;; wrote, a command is one this test put on the allowlist, and the record is
;;;; a file of the run's own scratch folder.

(in-package #:nodecode.test)

(defvar *standing-case* 0
  "A number that keeps each test's scratch folder and record its own.")

(defun standing-scratch (&optional (label "case"))
  "A fresh folder inside this run's temporary directory."
  (let ((dir (merge-pathnames (format nil "nc-standing-~a-~a-~a/" label
                                       (get-universal-time) (incf *standing-case*))
                               (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames "keep" dir))
    dir))

(defun standing-file (dir name)
  "DIR as a directory pathname, and NAME inside it."
  (merge-pathnames name (uiop:ensure-directory-pathname dir)))

(defun standing-record (&optional (label "case"))
  "A record file of this test's own, in a folder of this test's own."
  (namestring (standing-file (standing-scratch label) "ledger.sexp")))

(defun standing-op (form &optional strings)
  "A tool call of FORM, with the strings object STRINGS when given."
  (list :name "eval"
        :arguments (if strings
                       (nlk:json-object "form" form "strings" strings)
                       (nlk:json-object "form" form))
        :call-id "c"))

(defun standing-verdict (form &optional strings)
  "The installed hook's answer for FORM: :ALLOWED, or the refusal string."
  (funcall (cell-hook "nodecode-standing" :tool)
           (standing-op form strings)
           (lambda (op) (declare (ignore op)) :allowed)))

(defun standing-allow (&rest arguments)
  "An ASK seam that allows every question."
  (declare (ignore arguments))
  (values "Allow once" nil))

(defun standing-deny (&rest arguments)
  "An ASK seam that denies every question."
  (declare (ignore arguments))
  (values "Deny" nil))

(defun standing-proof-call (kind target expect)
  "standing_proof called through the wire, as a provider sends it."
  (princ-to-string
   (nle::message-content
    (execute-wire-call "call-1" "standing_proof"
                       (nlk:encode-json-object
                        (nlk:make-json-object "kind" kind "target" target
                                               "expect" expect))))))

(defun standing-outcome (kind target expect)
  "A check's verdict as a keyword, and the answer it came in."
  (let ((answer (standing-proof-call kind target expect)))
    (values (cond ((uiop:string-prefix-p "pass:" answer) :pass)
                  ((uiop:string-prefix-p "fail:" answer) :fail)
                  ((uiop:string-prefix-p "unavailable:" answer) :unavailable)
                  (t :other))
            answer)))

(defun standing-categories (form)
  "What the cell reads FORM as: its categories, and whether the writes stay in
the workspace."
  (nodecode-standing::call-categories form nil (uiop:getcwd)))

(defun standing-workspace ()
  "The configured workspace as a namestring."
  (let ((value (nodecode-standing::setting :workspace)))
    (namestring (uiop:ensure-directory-pathname
                 (if (pathnamep value) value (uiop:parse-native-namestring value))))))

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "standing"
  (:config "ledger" (standing-record "lifecycle"))
  (:hooks :tool)
  (:help :standing)
  (:command "standing")
  (:running (is (equal "default" (nodecode-standing::setting :actor)) "the declared default actor is what the cell runs on")
            (is (equal "L1" (nodecode-standing::setting :level)) "and L1 is where a new actor starts"))
  (:refused ("level" "L9")
            ("promote_after" 0)
            ("timeout" 0)))

;;; --- what a call is read as ------------------------------------------------

(deftest standing-cell-reads-what-a-call-runs (with-cell-stop ((standing-start "ledger" (standing-record "reads")))
  (multiple-value-bind (categories local) (standing-categories "(+ 6 7)")
    (is (equal '(:read) categories) "arithmetic is a read")
    (is (null local) "and writes nothing"))
  (multiple-value-bind (categories local) (standing-categories "(sh \"rm -rf build\")")
    (declare (ignore local))
    (is (member :shell categories) "a shell call")
    (is (member :scrub categories) "and the rm in it is a scrub"))
  (multiple-value-bind (categories local) (standing-categories "(sh \"git push origin main\")")
    (declare (ignore local))
    (is (member :send categories) "a git push is a send"))
  (multiple-value-bind (categories local) (standing-categories "(sh \"ls -la\")")
    (declare (ignore local))
    (is (equal '(:shell) categories) "an ordinary command is only a shell"))
  (multiple-value-bind (categories local) (standing-categories "(sh command)")
    (declare (ignore local))
    (is (member :unknown categories) "a command hidden behind a variable is unknown"))
  (multiple-value-bind (categories local) (standing-categories "(write-file \"notes.txt\" \"x\")")
    (declare (ignore categories))
    (is (null local) "a write outside any workspace is not local"))
  (multiple-value-bind (categories local) (standing-categories "(((")
    (declare (ignore local))
    (is (equal '(:unknown) categories) "a form that does not read is unknown"))))

(deftest standing-cell-counts-a-workspace-write-as-local (with-cell-stop ((standing-start "ledger" (standing-record "local")
                                                                                        "workspace" (namestring (standing-scratch "workspace"))))
  (let ((target (namestring (standing-file (standing-workspace) "notes.txt"))))
    (multiple-value-bind (categories local) (standing-categories (format nil "(write-file ~s ~s)" target "x"))
      (is (member :write categories) "it is a write")
      (is (eq t local) "and it is local, being inside the workspace")))))

;;; --- the gate --------------------------------------------------------------

(deftest standing-cell-asks-before-a-shell-at-l1 (with-cell-stop ((standing-start "ledger" (standing-record "gate")))
  (let ((nodecode-standing::*ask* #'standing-deny))
    (is (search "refused by nodecode-standing" (standing-verdict "(sh \"ls -la\")")) "a shell at L1 asks, and a Deny refuses it"))
  (let ((nodecode-standing::*ask* #'standing-allow))
    (is (eq :allowed (standing-verdict "(sh \"ls -la\")")) "the same call runs when the operator allows it once"))
  (let ((nodecode-standing::*ask* #'standing-deny))
    (is (eq :allowed (standing-verdict "(+ 1 2)")) "a read never asks")
    (is (search "refused by nodecode-standing" (standing-verdict "(sh \"deploy.sh\")")) "a spin asks too"))))

(deftest standing-cell-refuses-a-call-that-owes-a-proof (with-cell-stop ((standing-start "ledger" (standing-record "owed")
                                                                                         "proof_before" (vector "eval")
                                                                                         "command_allowlist" (vector "echo ok")))
  (is (search "standing_proof" (standing-verdict "(+ 1 2)")) "a gated tool owes a pass first")
  (is (eq :pass (standing-outcome "command" "echo ok" "ok")) "the check passes")
  (let ((nodecode-standing::*ask* #'standing-allow))
    (is (eq :allowed (standing-verdict "(+ 1 2)")) "and the call it owed runs after it"))))

;;; --- the proofs ------------------------------------------------------------

(deftest standing-cell-proofs-a-file (with-cell-stop ((standing-start "ledger" (standing-record "file")
                                                                        "workspace" (namestring (standing-scratch "artifacts"))))
  (let* ((path (standing-file (standing-workspace) "artifact.txt"))
         (digest (subseq (nlk:sha256-text "a checked artifact") 7)))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string "a checked artifact" out))
    (is (eq :pass (standing-outcome "file" (namestring path) digest)) "the file's sha256 matches")
    (is (eq :pass (standing-outcome "file" (namestring path) "checked")) "the text is in it")
    (is (eq :fail (standing-outcome "file" (namestring path) "something else")) "a text it does not hold fails")
    (is (eq :unavailable (standing-outcome "file" "/etc/hostname" "x")) "a file outside the proof roots is not read"))))

(deftest standing-cell-proofs-an-allowlisted-command (with-cell-stop ((standing-start "ledger" (standing-record "cmd")
                                                                                       "command_allowlist" (vector "echo ok")))
  (is (eq :pass (standing-outcome "command" "echo ok" "ok")) "an allowlisted command's output is read")
  (is (eq :fail (standing-outcome "command" "echo ok" "nope")) "a text it did not print fails")
  (is (eq :unavailable (standing-outcome "command" "echo nope" "nope")) "a command nobody allowlisted does not run")))

(deftest standing-cell-proofs-a-page (with-cell-stop ((standing-start "ledger" (standing-record "page")
                                                                           "http_proof_targets" (vector "https://example.test/health")))
  (let ((nodecode-standing::*http* (lambda (url)
                                     (declare (ignore url))
                                     (values "the service is ok" 200))))
    (is (eq :pass (standing-outcome "http" "https://example.test/health" "is ok")) "a page under an allowlisted prefix is read")
    (is (eq :fail (standing-outcome "http" "https://example.test/health" "is down")) "a text it does not carry fails")
    (is (eq :unavailable (standing-outcome "http" "https://elsewhere.test/health" "ok")) "a page outside every prefix is not fetched"))))

;;; --- the record ------------------------------------------------------------

(deftest standing-cell-promotes-and-demotes-on-its-proofs (with-cell-stop ((standing-start "ledger" (standing-record "record")
                                                                                       "level" "L0"
                                                                                       "promote_after" 2
                                                                                       "max_auto_level" "L2"
                                                                                       "command_allowlist" (vector "echo ok")))
  (multiple-value-bind (outcome answer) (standing-outcome "command" "echo ok" "ok")
    (is (eq :pass outcome) "a first pass counts")
    (is (search "is at L0" answer) "and leaves the level where it was"))
  (multiple-value-bind (outcome answer) (standing-outcome "command" "echo ok" "ok")
    (is (eq :pass outcome) "a second pass")
    (is (search "is at L1" answer) "raises the level to L1"))
  (multiple-value-bind (outcome answer) (standing-outcome "command" "echo ok" "nope")
    (is (eq :fail outcome) "a failed check")
    (is (search "is at L0" answer) "lowers the level again"))
  (is (search "is now at L3" (nodecode-standing::run-slash "set default L3 the owner said so" nil)) "the operator's own word sets any level")
  (let ((table (nodecode-standing::run-slash "" nil)))
    (is (search "actor | level" table) "the table names its columns")
    (is (search "L3" table) "and shows the level the operator set"))
  (is (search "owner" (nodecode-standing::run-slash "log default" nil)) "the log keeps the owner's own event")))
