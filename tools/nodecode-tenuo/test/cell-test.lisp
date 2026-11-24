;;;; cell-test.lisp --- tenuo's lifecycle, the warrant check, and the audit log.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The core proves the hook seam and the tool dispatch; what is proved here
;;;; is this cell's own data and wiring: that the section's settings reach the
;;;; decision, that a warrant's tools and argument constraints are what a call
;;;; is measured against, that an expired (or unsigned) warrant refuses, that
;;;; every decision lands in the audit log one JSON line at a time, and that
;;;; both tools answer through the same seam the model calls them on.

(in-package #:nodecode.test)

;;; --- fixtures --------------------------------------------------------------

(defun tenuo-warrant (text)
  "TEXT written to a fresh warrant file under the temporary directory."
  (let ((path (merge-pathnames (format nil "tenuo-~a.warrant" (gensym))
                                (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string text out))
    (uiop:native-namestring path)))

(defun tenuo-verdict (tool &optional arguments)
  "The installed hook's answer for TOOL with ARGUMENTS: :ALLOWED or the
refusal string."
  (funcall (cell-hook "nodecode-tenuo" :tool)
           (list :name tool :arguments (or arguments (nlk:json-object)) :call-id "c")
           (lambda (op) (declare (ignore op)) :allowed)))

(defparameter +allow-eval+
  "{\"capabilities\": {\"eval\": \"*\"}}")

(defparameter +scoped+
  "{\"capabilities\": {\"read_file\": {\"path\": {\"under\": \"/data/reports\"}},
                       \"websearch\": {\"query\": {\"glob\": \"release-*\"}},
                       \"cron\": {\"action\": {\"one_of\": [\"list\", \"show\"]}}}}")

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "tenuo"
  (:hooks :tool)
  (:help :tenuo)
  (:command "tenuo")
  (:refused ("on_denial" "maybe")))

;;; --- the decision ----------------------------------------------------------

(deftest tenuo-cell-a-call-the-warrant-does-not-name-is-refused ()
  (with-cell-stop ((tenuo-start "warrant" (tenuo-warrant +allow-eval+)))
    (is (eq :allowed (tenuo-verdict "eval")))
    (let ((refusal (tenuo-verdict "read_file" (nlk:json-object "path" "/etc/passwd"))))
      (is (stringp refusal) "a tool the warrant does not name is refused")
      (is (search "refused by nodecode-tenuo" refusal) "saying who refused")
      (is (search "read_file is not in the warrant" refusal) "and why"))))

(deftest tenuo-cell-argument-constraints-are-what-a-call-is-measured-against ()
  (with-cell-stop ((tenuo-start "warrant" (tenuo-warrant +scoped+)))
    (flet ((reads (path) (tenuo-verdict "read_file" (nlk:json-object "path" path))))
      (is (eq :allowed (reads "/data/reports/q3.csv")) "a path under the warrant's root passes")
      (is (eq :allowed (reads "/data/reports")) "and the root itself")
      (is (stringp (reads "/data/reports-x/q3.csv")) "a sibling whose name starts the same is not under it")
      (is (stringp (reads "/data/reports/../../etc/passwd")) "traversal is resolved before the compare")
      (is (stringp (reads "/etc/passwd"))))
    (flet ((searches (query) (tenuo-verdict "websearch" (nlk:json-object "query" query))))
      (is (eq :allowed (searches "release-notes")) "the glob holds")
      (is (stringp (searches "invoice")) "and refuses what it does not match"))
    (flet ((crons (action) (tenuo-verdict "cron" (nlk:json-object "action" action))))
      (is (eq :allowed (crons "list")) "one_of admits a member")
      (is (stringp (crons "delete")) "and refuses the rest"))
    (is (stringp (tenuo-verdict "read_file" (nlk:json-object "query" "x"))) "a constrained argument the call does not carry refuses")))

(deftest tenuo-cell-an-expired-warrant-refuses-every-call ()
  (with-cell-stop ((tenuo-start "warrant"
                                (tenuo-warrant
                                 "{\"expires_at\": \"2001-01-01T00:00:00Z\", \"capabilities\": {\"eval\": \"*\"}}")))
    (let ((refusal (tenuo-verdict "eval")))
      (is (stringp refusal) "past its expiry the warrant allows nothing")
      (is (search "expired" refusal)))))

(deftest tenuo-cell-without-a-warrant-every-call-passes ()
  (with-cell-stop ((tenuo-start))
    (is (eq :allowed (tenuo-verdict "eval")) "the documented no-op")
    (is (search "no warrant set" (nodecode-tenuo::status-text)) "and /tenuo says so")))

;;; --- the audit log ---------------------------------------------------------

(deftest tenuo-cell-every-decision-lands-in-the-audit-log ()
  (let ((log (merge-pathnames (format nil "tenuo-~a.jsonl" (gensym)) (uiop:temporary-directory))))
    (unwind-protect
         (with-cell-stop ((tenuo-start "warrant" (tenuo-warrant +allow-eval+)
                                       "audit_log" (uiop:native-namestring log)))
           (is (eq :allowed (tenuo-verdict "eval")))
           (is (stringp (tenuo-verdict "read_file" (nlk:json-object "path" "/etc/passwd"))))
           (is (probe-file log) "the log was written")
           (let ((records (nodecode-tenuo::audit-records :limit 10)))
             (is (= 2 (length records)) "one line per call")
             (is (equal '("ALLOW" "DENY")
                        (mapcar (lambda (record) (gethash "decision" record)) records)) "the decision each call got")
             (is (equal "/etc/passwd"
                        (gethash "path" (gethash "arguments" (second records)))) "with the arguments the call carried")))
      (ignore-errors (delete-file log)))))

(deftest tenuo-cell-on_denial-log-records-but-lets-the-call-run ()
  (with-cell-stop ((tenuo-start "warrant" (tenuo-warrant +allow-eval+) "on_denial" "log"))
    (let ((ran (list nil)))
      (let ((answer (funcall (cell-hook "nodecode-tenuo" :tool)
                              (list :name "read_file" :arguments (nlk:json-object) :call-id "c")
                              (lambda (op) (declare (ignore op)) (setf (car ran) t) :allowed))))
        (is (eq :allowed answer) "log mode lets the refused call run")
        (is (car ran) "the call passed through to the next handler")))))

;;; --- what the model asks ---------------------------------------------------

(deftest tenuo-cell-the-check-tool-answers-through-the-dispatch-seam ()
  (with-cell-stop ((tenuo-start "warrant" (tenuo-warrant +allow-eval+)))
    (flet ((check (json) (funcall (third (nle::find-tool "tenuo_check")) (nlk:decode-json json))))
      (is (search "allowed" (check "{\"tool\": \"eval\", \"arguments\": {\"form\": \"(* 2 3)\"}}")) "a call the warrant allows")
      (is (search "refused: read_file is not in the warrant"
                  (check "{\"tool\": \"read_file\", \"arguments\": {\"path\": \"/etc/passwd\"}}")) "and one it does not"))))

(deftest tenuo-cell-the-audit-tool-reads-what-the-log-holds ()
  (let ((log (merge-pathnames (format nil "tenuo-~a.jsonl" (gensym)) (uiop:temporary-directory))))
    (unwind-protect
         (with-cell-stop ((tenuo-start "warrant" (tenuo-warrant +allow-eval+)
                                       "audit_log" (uiop:native-namestring log)))
           (tenuo-verdict "read_file")
           (funcall (third (nle::find-tool "tenuo_audit")) (nlk:decode-json "{\"denied_only\": true}"))
           (is (search "DENY" (nodecode-tenuo::audit-text :denied-only t)) "a denial is listed")
           (is (search "DENY" (nodecode-tenuo::audit-text :limit 5)) "and the whole log lists it too"))
      (ignore-errors (delete-file log)))))
