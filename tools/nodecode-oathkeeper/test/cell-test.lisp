;;;; cell-test.lisp --- lifecycle, the oath's reach, the tools and the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section, the one :TOOL
;;;; hook refusing a call an oath forbids and letting the rest run, the evidence
;;;; a require line waits for (this session only), the oath file's own
;;;; protection, the inert state with no oath, the snapshot's report of a change
;;;; made outside the gate, the two tools and /oathkeeper. Nothing here touches
;;;; the network or runs a process: every call is a form handed to the hook.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "oathkeeper"
  (:hooks :tool)
  (:command "oathkeeper")
  (:help :oathkeeper)
  (:running (is (char= #\/ (char (nodecode-oathkeeper::oath-file) 0)) "the oath file resolves to an absolute path"))
  (:refused ("hash_limit_bytes" 0)))

;;; --- the oath's own reading ---------------------------------------------------

(deftest oathkeeper-cell-parses-and-matches ()
  (multiple-value-bind (missions rules errors)
      (nodecode-oathkeeper::parse-oath-text
       (format nil "# my oath~%standing mission: keep going~%never delete: /a/b/**~%always require backup before modify: /a/b/**~%nonsense"))
    (is (= 1 (length missions)) "one mission is read")
    (is (= 2 (length rules)) "two rules are read")
    (is (= 1 (length errors)) "an unparsed line is one error, ignored")
    (is (string= "delete" (getf (first rules) :verb)) "the verb is read")
    (is (string= "backup" (getf (second rules) :evidence)) "the evidence a require line wants is read"))
  (is (nodecode-oathkeeper::pattern-matches-p "/a/*.md" "/a/x.md") "a glob covers its match")
  (is (not (nodecode-oathkeeper::pattern-matches-p "/a/b/**" "/a/bc/x.txt")) "and a tree is not its sibling with the same prefix")
  (is (nodecode-oathkeeper::pattern-matches-p "/a/b" "/a/b/c.txt") "a plain pattern covers its subtree")
  (is (not (nodecode-oathkeeper::pattern-matches-p "/a/b" "/a/bc.txt")) "and stops at a segment"))

(deftest oathkeeper-cell-reads-what-a-call-changes ()
  ;; The commands a form runs, not its comments: the plugin's bounded scan, read
  ;; against an eval form instead of a shell tool of its own.
  (flet ((deletes (form &optional strings)
           (remove-if-not (lambda (action) (string= "delete" (getf action :verb)))
                          (oathkeeper-actions form strings))))
    (is (= 1 (length (deletes "(sh \"rm -rf /tmp/a\")"))) "rm is a delete")
    (is (= 1 (length (deletes "(uiop:run-program (list \"rm\" \"-rf\" \"/tmp/a\"))"))) "an argv list is a delete too")
    (is (= 1 (length (deletes "(sh \"cd x && rm -rf /tmp/a\")"))) "so is a later command in a line")
    (is (zerop (length (deletes "(* 6 7)"))) "arithmetic changes nothing")
    (is (zerop (length (deletes "(look \"/tmp/a\")"))) "and a read is not a delete")
    (is (= 1 (length (deletes "(delete-file \"/tmp/a\")"))) "the file verb is read")
    (is (string= "/tmp/a"
                 (getf (first (deletes "(delete-file ($ \"victim\"))"
                                      (oathkeeper-strings (list "victim" "/tmp/a"))))
                       :target)) "a path the strings object carries is read too")))

;;; --- what the gate refuses ----------------------------------------------------

(deftest oathkeeper-cell-blocks-a-never-rule ()
  (let* ((dir (oathkeeper-scratch "sworn/"))
         (protected (concatenate 'string dir "notes/keep.txt")))
    (with-oath ((format nil "# my oath~%never delete: ~a**" dir))
      (let ((verdict (oathkeeper-verdict (format nil "(sh \"rm -rf ~a\")" protected))))
        (is (stringp verdict) "a delete of a sworn tree is refused")
        (is (search "oath line 2" verdict) "and the violated line is cited")
        (is (search "forbids delete" verdict) "with the verb it forbade"))
      (is (string= :ran (oathkeeper-verdict "(sh \"rm -rf /tmp/elsewhere\")")) "a delete elsewhere passes")
      (is (string= :ran (oathkeeper-verdict (format nil "(look \"~a\")" protected))) "and a read of the sworn tree passes")
      (is (member "blocked" (oathkeeper-events) :test #'equal) "the refusal is on the ledger"))))

(deftest oathkeeper-cell-protects-the-oath-file-itself ()
  (with-oath ("never modify: /nowhere/**")
    (let ((verdict (oathkeeper-verdict
                    (format nil "(write-file \"~a\" \"# emptied\")" (oathkeeper-file)))))
      (is (stringp verdict) "the agent cannot write the oath")
      (is (search "only the owner may change" verdict) "and is told why"))
    (is (stringp (oathkeeper-verdict (format nil "(sh \"rm ~a\")" (oathkeeper-file)))) "nor delete it")))

(deftest oathkeeper-cell-requires-evidence-before-modify ()
  (let* ((dir (oathkeeper-scratch "sworn/"))
         (file (concatenate 'string dir "note.md")))
    (with-oath ((format nil "always require backup before modify: ~a**" dir))
      (flet ((modify () (oathkeeper-verdict (format nil "(write-file \"~a\" \"new\")" file))))
        (is (stringp (modify)) "a modify with no backup behind it is refused")
        (is (search "requires evidence 'backup'" (modify)) "and names the evidence it wants")
        (is (string= :ran (oathkeeper-verdict (format nil "(sh \"cp ~a ~a.bak\")" file file))) "a backup copy passes, and records the evidence")
        (is (string= :ran (modify)) "so the modify it covers then passes"))
      (let ((nodecode-oathkeeper::*session* "another-session"))
        (is (stringp (oathkeeper-verdict (format nil "(write-file \"~a\" \"again\")" file))) "evidence from another session does not satisfy the line")))))

(deftest oathkeeper-cell-is-inert-without-an-oath ()
  (with-no-oath
    (is (string= :ran (oathkeeper-verdict "(sh \"rm -rf /tmp/anything\")")) "with no oath, nothing is refused")
    (is (null (nodecode-oathkeeper::snapshot-now :report t)) "and nothing is watched")))

(deftest oathkeeper-cell-reports-changes-made-outside-the-gate ()
  (let* ((dir (oathkeeper-scratch "watched/"))
         (file (concatenate 'string dir "note.txt")))
    (with-oath ((format nil "never delete: ~a**" dir))
      (nlk:write-file-atomically file "changed by hand, not by a call")
      (is (string= :ran (oathkeeper-verdict "(sh \"true\")")) "an innocent call still runs")
      (is (member "outside_change_detected" (oathkeeper-events) :test #'equal) "and the change that never passed the gate is reported")
      (is (string= :ran (oathkeeper-verdict
                          (format nil "(write-file \"~a\" \"through the gate\")" file))) "a change made through an approved call passes")
      (let ((count (count "outside_change_detected" (oathkeeper-events) :test #'equal)))
        (is (string= :ran (oathkeeper-verdict "(sh \"true\")")) "the next call is checked")
        (is (= count (count "outside_change_detected" (oathkeeper-events) :test #'equal)) "and the gate's own change is not reported again")))))

;;; --- the tools and the command ------------------------------------------------

(deftest oathkeeper-cell-tools-and-command ()
  (with-oath ((format nil "standing mission: be kind to the owner's files~%never delete: /sworn/**"))
    (let ((status (nlk:decode-json (nodecode-oathkeeper::status-tool nil))))
      (is (nlk:json-value status :boolean "oath_exists") "oath_status says the oath exists")
      (is (= 1 (length (nlk:json-value status :array "missions"))) "and carries the mission")
      (is (= 1 (length (nlk:json-value status :array "rules"))) "and the rule"))
    (let ((allowed (nlk:decode-json
                    (nodecode-oathkeeper::check-tool
                     (nodecode-oathkeeper::decode-check-call
                      (nlk:make-json-object "form" "(sh \"rm -rf /sworn/notes\")"))))))
      (is (not (nlk:json-value allowed :boolean "allowed")) "oath_check refuses the hypothetical delete")
      (is (= 1 (length (nlk:json-value allowed :array "violations"))) "and names the one line"))
    (let ((fine (nlk:decode-json
                 (nodecode-oathkeeper::check-tool
                  (nodecode-oathkeeper::decode-check-call
                   (nlk:make-json-object "form" "(* 6 7)"))))))
      (is (nlk:json-value fine :boolean "allowed") "and allows a call that changes nothing"))
    (let ((said (nodecode-oathkeeper::run-command "" "s1")))
      (is (search "Standing missions" said) "/oathkeeper shows the missions")
      (is (search "line 2: never delete: /sworn/**" said) "and the rule with its line number"))))
