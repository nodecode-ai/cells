;;;; cell-test.lisp --- the tray's rules, at the two seams and through the tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the lifecycle the declaration owes (install, stop,
;;;; idempotence, veto, a refused section); that the file verbs record the
;;;; paths they were given, once each, with the newer action in place; that a
;;;; tool call naming a path is caught at the :TOOL point and one that names
;;;; none is not; that the cap keeps the newest and drops the oldest; and
;;;; that both tools and /files answer from the same list and empty it.
;;;;
;;;; A real store and real verbs: the only fixture is a scratch home.

(in-package #:nodecode.test)

(define-test-slice "file-tray" "FILE-TRAY-CELL-" :start nodecode-file-tray:start-cell)

(define-cell-lifecycle-tests "file-tray"
  (:hooks 'nle:edit 'nle:write-file 'nle:look :tool)
  (:help :files)
  (:command "files")
  (:refused ("max_items" 0) ("max_items" "many"))
  (:running (is (= 40 (nodecode-file-tray::setting :max-items))
                 "the default cap is the original's 40")))

(defmacro with-tray ((session &rest config) &body body)
  "BODY in SESSION's evaluation, over a temp store, the cell started on
CONFIG."
  `(with-temp-store ()
     (ensure-durable-session ,session)
     (with-cell-stop ((file-tray-start ,@config))
       (with-durable-turn (turn ,session :current t)
         (let ((nlk:*scribe-session-id* ,session))
           ,@body)))))

(defun tray-of (session)
  "SESSION's tray, for a test to read."
  (nodecode-file-tray:tray-items session))

(defun tray-names (session)
  (mapcar (lambda (item) (nodecode-file-tray:display-name (getf item :path)))
          (tray-of session)))

(defun call-list (&optional arguments)
  "file_tray_list's own answer for ARGUMENTS, through its registered body."
  (funcall (third (nle::find-tool "file_tray_list"))
           (or arguments (make-hash-table :test #'equal))))

(defun call-clear ()
  (funcall (third (nle::find-tool "file_tray_clear")) (make-hash-table :test #'equal)))

;;; --- the verbs ---------------------------------------------------------------

(deftest file-tray-cell-records-the-file-verbs ()
  (with-tray ("file-tray-verbs")
    (let ((path (namestring (nlk:home "tray/first.txt"))))
      (nle:write-file path "alpha beta")
      (is (equal '("first.txt" "write")
                 (list (first (tray-names "file-tray-verbs"))
                       (getf (first (tray-of "file-tray-verbs")) :action)))
          "a write lands on the tray as the path it wrote")
      (nle:edit path "alpha" "gamma")
      (let ((items (tray-of "file-tray-verbs")))
        (is (= 1 (length items)) "one path, touched twice, is one entry")
        (is (equal "edit" (getf (first items) :action)) "the newer action stands")
        (is (equal path (getf (first items) :path)) "and the path is the one the verb ran on"))
      (let ((json (call-list)))
        (is (search path json) "the tool answers the path")
        (is (search "\"count\":1" json) "with a count")
        (is (search "\"markdown\"" json) "and a markdown list, by default")
        (is (search "1. `" json) "whose first line is the entry"))
      (let ((arguments (make-hash-table :test #'equal)))
        (setf (gethash "markdown" arguments) nil)
        (is (not (search "\"markdown\"" (call-list arguments)))
            "a call that says markdown false gets the items alone")))))

;;; --- a tool call's arguments -------------------------------------------------

(deftest file-tray-cell-catches-a-tool-call-that-names-a-path ()
  (with-tray ("file-tray-tool")
    (let ((arguments (nlk:make-json-object "path" "notes/todo.md" "limit" 20)))
      (is (equal "done"
                 (nlk:call-hooked :tool
                                  (list :name "read_file" :arguments arguments
                                        :call-id "call-1")
                                  (lambda (op) (declare (ignore op)) "done")))
          "the call itself answers as it would unadvised")
      (let ((items (tray-of "file-tray-tool")))
        (is (= 1 (length items)) "its path is on the tray")
        (is (equal "notes/todo.md" (getf (first items) :path)))
        (is (equal "read_file" (getf (first items) :action))
            "and the tool that ran is the action")))
    (nlk:call-hooked :tool
                     (list :name "eval"
                           :arguments (nlk:make-json-object "form" "(+ 1 2)")
                           :call-id "call-2")
                     (lambda (op) (declare (ignore op)) "3"))
    (is (= 1 (length (tray-of "file-tray-tool")))
        "a call naming no path leaves the tray as it was")
    (nlk:call-hooked :tool
                     (list :name "multi"
                           :arguments (nlk:make-json-object
                                       "paths" (vector "a/one.lisp" "b/two.lisp"))
                           :call-id "call-3")
                     (lambda (op) (declare (ignore op)) "ok"))
    (is (equal '("todo.md" "one.lisp" "two.lisp") (tray-names "file-tray-tool"))
        "a paths array records each string in it, after what was there")))

;;; --- the cap and one path's place ---------------------------------------------

(deftest file-tray-cell-keeps-paths-unique-and-capped ()
  (with-tray ("file-tray-cap" "max_items" 3)
    (is (= 3 (nodecode-file-tray::setting :max-items)) "the section's cap is the one read")
    (dotimes (index 5)
      (nle:write-file (namestring (nlk:home (format nil "tray/f~d.txt" index))) "x"))
    (is (equal '("f2.txt" "f3.txt" "f4.txt") (tray-names "file-tray-cap"))
        "past the cap the oldest paths leave, newest last")
    (nle:edit (namestring (nlk:home "tray/f2.txt")) "x" "y")
    (let ((items (tray-of "file-tray-cap")))
      (is (equal '("f2.txt" "f3.txt" "f4.txt") (tray-names "file-tray-cap"))
          "a path already on the tray keeps its place")
      (is (equal "edit" (getf (first items) :action))
          "and takes the newer action, as the original did"))
    (is (= 3 (nodecode-file-tray:clear-paths "file-tray-cap"))
        "clearing answers how many paths went")))

;;; --- /files and the two tools ------------------------------------------------

(deftest file-tray-cell-clears-through-the-tool-and-the-command ()
  (with-tray ("file-tray-clear")
    (nle:write-file (namestring (nlk:home "tray/second.txt")) "one")
    (multiple-value-bind (text dialog) (file-tray:run-files "" "file-tray-clear")
      (is (search "second.txt" text) "bare /files answers the list")
      (is (not (null dialog)) "and the panel whose row clears it"))
    (is (search "second.txt" (file-tray:run-files "list" "file-tray-clear"))
        "/files list answers it too")
    (is (search "Usage" (file-tray:run-files "wat" "file-tray-clear"))
        "an unknown word answers the usage")
    (is (search "cleared" (file-tray:run-files "clear" "file-tray-clear")))
    (is (null (tray-of "file-tray-clear")) "and the tray is empty")
    (nle:write-file (namestring (nlk:home "tray/third.txt")) "one")
    (is (search "\"ok\":true" (call-clear)) "file_tray_clear answers ok")
    (is (null (tray-of "file-tray-clear")) "and empties the same list")
    (is (search "empty" (call-list)) "which the list then says is empty")))

;;; --- a path, as it is kept ---------------------------------------------------

(deftest file-tray-cell-normalizes-a-path ()
  (is (equal "/a/b.txt" (nodecode-file-tray:normalize-path "  \"/a//b.txt\"  ")))
  (is (equal "/a/b.txt" (nodecode-file-tray:normalize-path "\\a\\b.txt")))
  (is (null (nodecode-file-tray:normalize-path "https://example.com/a.txt"))
      "a URL is no path")
  (is (null (nodecode-file-tray:normalize-path "")) "nor is nothing")
  (is (null (nodecode-file-tray:normalize-path (make-string 501 :initial-element #\a)))
      "nor one past 500 characters")
  (is (equal "b.txt" (nodecode-file-tray:display-name "/a/b.txt"))
      "the label is the last segment"))
