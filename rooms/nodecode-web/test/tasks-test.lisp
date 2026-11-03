;;;; tasks-test.lisp --- the Board's To do list: kept, version-checked, cleaned.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun tasks-request (method &optional object)
  "A clack environment for /api/web/tasks: METHOD, OBJECT as its JSON body."
  (let ((octets (and object (if (stringp object)
                                (sb-ext:string-to-octets object :external-format :utf-8)
                                (nlk:encode-json-octets object)))))
    (list :request-method method
          :content-length (and octets (length octets))
          :raw-body (and octets (flexi-streams:make-in-memory-input-stream octets)))))

(defun tasks-answer (answer)
  "(STATUS OBJECT) of what the route answered: 200 for an object it returned."
  (if (hash-table-p answer)
      (list 200 answer)
      (destructuring-bind (status headers body) answer
        (declare (ignore headers))
        (list status (nlk:decode-json (sb-ext:octets-to-string body :external-format :utf-8))))))

(defun a-task (id prompt &rest more)
  (let ((task (nlk:json-object "id" id "prompt" prompt "folder" "/tmp/proj" "created_at" "2026-11-03T10:00:00Z")))
    (loop for (key value) on more by #'cddr do (setf (gethash key task) value))
    task))

(deftest web-cell-board-keeps-its-tasks-in-the-home (with-temp-workspace (directory))
  (let ((nlk::*home* (uiop:ensure-directory-pathname directory)))
    (destructuring-bind (status board) (tasks-answer (nodecode-web::tasks-route (tasks-request :get)))
      (is (= 200 status))
      (is (= 0 (gethash "version" board)) "no file: an empty board at version 0")
      (is (zerop (length (gethash "tasks" board)))))
    (destructuring-bind (status board)
        (tasks-answer (nodecode-web::tasks-route
                       (tasks-request :put (nlk:json-object "version" 0 "tasks"
                                                            (vector (a-task "t1" "Fix the login page" "worktree" t
                                                                            "stray" "not kept")
                                                                    (a-task "t2" "Write the release notes"))))))
      (is (= 200 status))
      (is (= 1 (gethash "version" board)) "a kept list moves the version on")
      (is (equal '("t1" "t2") (map 'list (lambda (task) (gethash "id" task)) (gethash "tasks" board))) "in the order sent")
      (is (eq t (gethash "worktree" (aref (gethash "tasks" board) 0))))
      (is (null (nth-value 1 (gethash "stray" (aref (gethash "tasks" board) 0)))) "a field a task does not have is not kept"))
    (is (probe-file (merge-pathnames "web/tasks.json" directory)) "the file is in the home")
    (destructuring-bind (status board) (tasks-answer (nodecode-web::tasks-route (tasks-request :get)))
      (is (= 200 status))
      (is (= 1 (gethash "version" board)) "read back as kept")
      (is (equal "Fix the login page" (gethash "prompt" (aref (gethash "tasks" board) 0)))))
    ;; A window that read the board at version 0 sends its list after another
    ;; window kept one: refused, with the board as it now is.
    (destructuring-bind (status answer)
        (tasks-answer (nodecode-web::tasks-route
                       (tasks-request :put (nlk:json-object "version" 0 "tasks" (vector (a-task "t3" "Other window"))))))
      (is (= 409 status) "an older version never overwrites")
      (is (equal "moved" (nlk:json-value answer :string "error" "code")))
      (is (= 1 (gethash "version" answer)) "the current version rides the refusal")
      (is (= 2 (length (gethash "tasks" answer))) "and the current list, to merge into"))
    (destructuring-bind (status board) (tasks-answer (nodecode-web::tasks-route (tasks-request :get)))
      (declare (ignore status))
      (is (= 2 (length (gethash "tasks" board))) "the refused list was not kept"))))

(deftest web-cell-board-refuses-what-is-not-a-task (with-temp-workspace (directory))
  (let ((nlk::*home* (uiop:ensure-directory-pathname directory)))
    (flet ((refusal (body)
             (destructuring-bind (status answer) (tasks-answer (nodecode-web::tasks-route (tasks-request :put body)))
               (list status (nlk:json-value answer :string "error" "code")))))
      (is (equal '(400 "not_json") (refusal "{not json")))
      (is (equal '(400 "no_version") (refusal (nlk:json-object "tasks" #()))))
      (is (equal '(400 "no_tasks") (refusal (nlk:json-object "version" 0))))
      (is (equal '(400 "bad_task") (refusal (nlk:json-object "version" 0 "tasks" (vector (a-task "t1" "   ")))))
          "a task with no prompt")
      (is (equal '(400 "bad_task") (refusal (nlk:json-object "version" 0 "tasks" (vector (a-task "t1" "a" "folder" 7)))))
          "a folder that is not a string")
      (is (equal '(400 "bad_task") (refusal (nlk:json-object "version" 0 "tasks" (vector (a-task "t1" "a") (a-task "t1" "b")))))
          "two tasks of one id")
      (is (equal '(413 "too_many")
                 (refusal (nlk:json-object "version" 0 "tasks"
                                           (coerce (loop for n below 501 collect (a-task (format nil "t~d" n) "x")) 'vector)))))
      (is (equal '(405 "method") (list (first (tasks-answer (nodecode-web::tasks-route (tasks-request :delete))))
                                       "method"))))
    (is (not (probe-file (merge-pathnames "web/tasks.json" directory))) "nothing refused was kept")))
