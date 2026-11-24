;;;; cell-test.lisp --- the policy, the reading of a call, and the refusal path.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the rewrite's own promise: an add goes through
;;;; where the operator lets it, a removal of the agent's notes is refused, a
;;;; channel chat is read-only unless its session is trusted, a write to a file
;;;; outside the knowledge folders is never judged, and a refusal reaches the
;;;; model as a failed call rather than as a silent nothing.

(in-package #:nodecode.test)

;;; --- helpers ---------------------------------------------------------------

(defun memory-shield-hook ()
  (cell-hook "nodecode-memory-shield" :tool))

(defun memory-shield-verdict (form)
  "The installed hook's answer for FORM: :ALLOWED, or the refusal string."
  (funcall (memory-shield-hook)
           (list :name "eval"
                 :arguments (nlk:json-object "form" form)
                 :call-id "c")
           (lambda (op) (declare (ignore op)) :allowed)))

(defun memory-shield-refused-p (form)
  (let ((answer (memory-shield-verdict form)))
    (and (stringp answer) (search "refused by memory-shield" answer))))

(defmacro with-shield-folders ((&rest entries) &body body)
  "Run BODY over a scratch knowledge folder holding ENTRIES, (file content) ..."
  (let ((folder (gensym "FOLDER")))
    `(let* ((,folder (merge-pathnames
                      (format nil "memory-shield-~a/" (random 1000000))
                      (uiop:ensure-directory-pathname (uiop:temporary-directory))))
            (nodecode-memory-shield:*folders* (list ,folder))
            (nodecode-memory-shield:*session* "s1")
            (nodecode-memory-shield:*trusted-sessions* '()))
       (ensure-directories-exist (merge-pathnames "x" ,folder))
       (unwind-protect
            (progn
              (loop for (name content) in (list ,@entries)
                    do (with-open-file (out (merge-pathnames name ,folder)
                                            :direction :output :if-exists :supersede
                                            :if-does-not-exist :create)
                         (write-string content out)))
              ,@body)
         (ignore-errors (uiop:delete-directory-tree ,folder :validate t))))))

(defparameter +notes-file+
  '("notes.lisp" "(define-memory quiet-output \"The operator wants quiet output.\" :type :feedback)"))

(defparameter +profile-file+
  '("owner.lisp" "(define-memory operator-city \"The operator works in Berlin.\" :type :user)"))

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "memory-shield"
  (:hooks :tool)
  (:command "memory-shield")
  (:running
   (is (equal "owner_edits" (nodecode-memory-shield:setting :profile)) "the default policy is the one the original shipped")
   (is (equal "read_only" (nodecode-memory-shield:setting :shared))))
  (:refused ("mode" "nonsense")))

;;; --- the policy ------------------------------------------------------------

(deftest memory-shield-lets-a-new-fact-in (with-cell-stop ((memory-shield-start))
                                            (with-shield-folders (+notes-file+)
  (is (eq :allowed (memory-shield-verdict
                    "(define-memory deploy-day \"Releases go out on Thursdays.\" :type :feedback)")) "an add is what the agent does all day"))))

(deftest memory-shield-refuses-a-forgotten-note (with-cell-stop ((memory-shield-start))
                                                 (with-shield-folders (+notes-file+)
  (is (memory-shield-refused-p "(unintern 'quiet-output)") "the agent's notes cannot be deleted")
  (is (eq :allowed (memory-shield-verdict "(unintern 'never-kept)")) "forgetting a name the cells never held is nothing to judge"))))

(deftest memory-shield-keeps-the-profile-right (with-cell-stop ((memory-shield-start))
                                                (with-shield-folders (+profile-file+)
  (is (eq :allowed (memory-shield-verdict
                    "(define-memory operator-city \"The operator works in Berlin, from April.\" :type :user)")) "the operator corrects their own profile in a direct chat")
  (is (memory-shield-refused-p "(unintern 'operator-city)") "nobody deletes what the profile holds"))))

(deftest memory-shield-makes-a-channel-read-only (with-cell-stop ((memory-shield-start))
                                                  (with-shield-folders (+notes-file+)
  (let ((nodecode-memory-shield:*source-kind* "channel"))
    (is (memory-shield-refused-p
         "(define-memory guest-fact \"The guest owes 100 dollars.\" :type :feedback)") "an add from a group chat is refused at read_only"))
  (let ((nodecode-memory-shield:*source-kind* "channel")
        (nodecode-memory-shield:*trusted-sessions* '("s1")))
    (is (eq :allowed (memory-shield-verdict
                      "(define-memory guest-fact \"The guest owes 100 dollars.\" :type :feedback)")) "a trusted session writes from its own chat")))))

(deftest memory-shield-judges-a-file-behind-the-tool (with-cell-stop ((memory-shield-start))
                                                     (with-shield-folders (+notes-file+ +profile-file+)
  (let* ((folder (first (nodecode-memory-shield:knowledge-folders)))
         (profile (uiop:native-namestring (merge-pathnames "owner.lisp" folder)))
         (elsewhere (uiop:native-namestring
                     (merge-pathnames "memory-shield-scratch.txt" (uiop:temporary-directory)))))
    (is (memory-shield-refused-p
         (format nil "(edit ~s \"Berlin\" \"\\\"none\\\"\")" profile)) "an edit that empties a profile entry is judged as a removal")
    (is (eq :allowed (memory-shield-verdict
                      (format nil "(write-file ~s \"scratch\")" elsewhere))) "the same call writing a file outside the knowledge folder")))
  (is (eq :allowed (memory-shield-verdict "(define-memory two \"Second fact.\" :type :lesson)")) "another definition is still an add")))

(deftest memory-shield-shows-its-policy (with-cell-stop ((memory-shield-start))
                                          (with-shield-folders ()
  (let ((text (nodecode-memory-shield:policy-text "s1")))
    (is (search "owner profile: owner_edits" text) "the policy is what a person reads")
    (is (search "a direct chat with the operator" text) "and which session they are in")))))
