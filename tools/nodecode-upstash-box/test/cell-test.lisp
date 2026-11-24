;;;; cell-test.lisp --- the Upstash Box cell against a stubbed API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Box API endpoint is a stubbed NLK:HTTP: nothing reaches Upstash,
;;;; and no command runs anywhere. What is proved is the cell's own wiring:
;;;; the section (a healthy start, a size and a timeout refused, a section
;;;; with no key refused), the four tools through the seam the model calls
;;;; them by -- their path, their headers, their body and their answer --
;;;; the Box made on the first command, a paused one woken, a gone one
;;;; replaced, and /box showing, listing, pausing and deleting.

(in-package #:nodecode.test)

(define-test-slice "upstash-box" "UPSTASH-BOX-CELL-"
  :start nodecode-upstash-box:start-cell
  :defaults (("api_key" "k-test") ("base_url" "https://box.test")))

;;; --- the stubbed wire ----------------------------------------------------------------

(defmacro with-box-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS), as NLK:HTTP does. A request no route names answers an empty object
with 200 -- the lifecycle's own pause lands there. ASKED collects
(METHOD URL CONTENT HEADERS), oldest first, once BODY reads it. The Box id
written under the home is forgotten first, so no test inherits another's."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (ignore-errors (delete-file (nodecode-upstash-box::state-path)))
       (setf nodecode-upstash-box::*box* :unread)
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content)
                                               (getf options :headers))
                                         ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (values "{}" 200))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun box-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (third request)))

(defun box-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (fourth request) :test #'string-equal)))

(defun box-json (id status)
  "A Box as the API answers one."
  (format nil "{\"id\":\"~a\",\"name\":\"nodecode-~a\",\"status\":\"~a\",\"size\":\"small\"}"
          id id status))

(defun box-json-string (text)
  "TEXT as a JSON string literal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for character across text
          do (case character
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (t (write-char character out))))
    (write-char #\" out)))

(defun box-exec-json (output exit &optional error)
  "A command's result, as POST /exec answers it."
  (format nil "{\"output\":~a,\"error\":~a,\"exit_code\":~d}"
          (box-json-string output) (box-json-string (or error "")) exit))

(defun box-request (asked fragment)
  "The first logged request whose URL carries FRAGMENT, or NIL."
  (find fragment asked :key #'second :test #'search))

(defun box-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

;;; --- the lifecycle, from the one declaration ------------------------------------------

(define-cell-lifecycle-tests "upstash-box"
  (:fixture with-box-wire)
  (:help :upstash-box)
  (:command "box")
  (:running (is (nle::find-tool "box_exec") "box_exec is registered")
            (is (nle::find-tool "box_read"))
            (is (nle::find-tool "box_write"))
            (is (nle::find-tool "box_list"))
            (is (equal "https://box.test" (nodecode-upstash-box::root)))
            (is (equal "small" (nodecode-upstash-box::setting :size)) "the default size")
            (is (= 120 (nodecode-upstash-box::setting :timeout-seconds))))
  (:stopped (is (null (nle::find-tool "box_exec")) "stop unregisters the tools"))
  (:refused ("size" "huge") ("timeout_seconds" 0) ("max_output_chars" 5))
  (:idle nodecode-upstash-box:upstash-box-error
         (nodecode-upstash-box::run-command-slash "" nil)))

(deftest upstash-box-cell-without-a-key-refuses ()
  ;; The one setting no verb can work without: the section names no key and
  ;; the environment has none, so the start refuses before anything installs.
  (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignore name)) nil)
    (let ((condition (signals-error nlk:config-refusal (upstash-box-start "api_key" ""))))
      (is (search "Box API key" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "box_exec")) "and installs nothing"))))

;;; --- the first command makes the Box ------------------------------------------------

(deftest upstash-box-cell-the-first-command-creates-the-box ()
  (with-box-wire (asked
                  ("/exec" (values (box-exec-json "hello" 0) 200))
                  ("/v2/box" (values (box-json "box_new" "running") 200)))
    (with-cell-stop ((upstash-box-start "size" "medium" "runtime" "python"))
      (is (search "box box_new, exit 0" (box-answer "b1" "box_exec" "{\"command\":\"echo hello\"}")))
      (is (search "hello" (box-answer "b1b" "box_exec" "{\"command\":\"echo hello\"}")))
      (let ((created (first asked)))
        (is (eq :post (first created)) "a Box is created with POST")
        (is (equal "https://box.test/v2/box" (second created)))
        (is (equal "k-test" (box-request-header created "x-box-api-key")) "the key rides the header")
        (let ((body (box-request-body created)))
          (is (equal "medium" (nlk:json-value body :string "size")) "the section's size")
          (is (equal "python" (nlk:json-value body :string "runtime")) "the section's runtime")
          (is (equal "nodecode" (aref (nlk:json-value body :array "labels") 0)) "labelled for the console")
          (is (search "nodecode-" (nlk:json-value body :string "name")) "named, so it is findable")))
      (let ((exec (box-request asked "/exec")))
        (is (equal "https://box.test/v2/box/box_new/exec" (second exec)))
        (let* ((body (box-request-body exec))
               (command (nlk:json-value body :array "command")))
          (is (equal "sh" (aref command 0)) "the command runs through sh -c")
          (is (equal "echo hello" (aref command 2)))
          (is (null (nlk:json-value body :string "folder")) "no cwd means the Box user's home")))
      (is (equal "box_new" (nodecode-upstash-box::stored-box)) "the id is written under the home"))))

(deftest upstash-box-cell-a-paused-box-is-woken-first ()
  (with-box-wire (asked
                  ("/exec" (values (box-exec-json "woken" 0) 200))
                  ("/resume" (values "{}" 200))
                  ("/v2/box/box_paused" (values (box-json "box_paused" "paused") 200)))
    (with-cell-stop ((upstash-box-start))
      (setf (nodecode-upstash-box::current-box) "box_paused")
      (is (search "box box_paused, exit 0" (box-answer "b2" "box_exec" "{\"command\":\"true\"}")))
      (is (equal '(:get :post :post) (mapcar #'first asked)) "the state read, the resume, the command")
      (is (equal "https://box.test/v2/box/box_paused/resume" (second (second asked)))))))

(deftest upstash-box-cell-a-gone-box-is-replaced-said-out-loud ()
  (with-box-wire (asked
                  ("/v2/box/box_gone" (values "{\"error\":\"Box not found\"}" 404))
                  ("/exec" (values (box-exec-json "fresh" 0) 200))
                  ("/v2/box" (values (box-json "box_fresh" "running") 200)))
    (with-cell-stop ((upstash-box-start))
      (setf (nodecode-upstash-box::current-box) "box_gone")
      (let ((answer (box-answer "b3" "box_exec" "{\"command\":\"true\"}")))
        (is (search "the Box box_gone this cell held was gone" answer) "the model is told")
        (is (search "box box_fresh, exit 0" answer))
        (is (search "fresh" answer) "and the command ran there"))
      (is (equal "box_fresh" (nodecode-upstash-box::current-box)) "the new Box is the one held"))))

(deftest upstash-box-cell-a-failed-command-carries-its-reason ()
  (with-box-wire (asked
                  ("/exec" (values (box-exec-json "" 3 "no such file: /nope") 200))
                  ("/v2/box/box_f" (values (box-json "box_f" "running") 200)))
    (with-cell-stop ((upstash-box-start))
      (setf (nodecode-upstash-box::current-box) "box_f")
      (is-carrying (answer (box-answer "b4" "box_exec" "{\"command\":\"cat /nope\"}"))
                   ("box box_f, exit 3" "the exit code is the model's")
                   ("no such file: /nope" "and the reason, which /exec puts in error")))))

(deftest upstash-box-cell-a-refused-request-is-a-repair-line ()
  (with-box-wire (asked
                  ("/exec" (values "{\"error\":\"Invalid API key\"}" 401))
                  ("/v2/box/box_f" (values (box-json "box_f" "running") 200)))
    (with-cell-stop ((upstash-box-start))
      (setf (nodecode-upstash-box::current-box) "box_f")
      (is-carrying (answer (box-answer "b4b" "box_exec" "{\"command\":\"true\"}"))
                   ("ERROR: upstash-box:" "the refusal is the model's repair")
                   ("HTTP 401") ("Invalid API key")))))

;;; --- files ----------------------------------------------------------------------------

(deftest upstash-box-cell-files-cross-over-by-path ()
  (with-box-wire (asked
                  ("/files/read" (values "{\"content\":\"hello from the box\"}" 200))
                  ("/files/write" (values "{}" 200))
                  ("/v2/box/box_f" (values (box-json "box_f" "running") 200)))
    (with-cell-stop ((upstash-box-start))
      (setf (nodecode-upstash-box::current-box) "box_f")
      (is (equal "hello from the box" (box-answer "b5" "box_read" "{\"path\":\"/workspace/a b.txt\"}")))
      (let ((read (box-request asked "/files/read")))
        (is (search "/v2/box/box_f/files/read?path=%2Fworkspace%2Fa%20b.txt" (second read)) "the path is url-encoded")
        (is (search "encoding=utf-8" (second read))))
      (is (search "Wrote 5 characters to /w/x.txt"
                  (box-answer "b6" "box_write" "{\"path\":\"/w/x.txt\",\"content\":\"hello\"}")))
      (let ((body (box-request-body (box-request asked "/files/write"))))
        (is (equal "/w/x.txt" (nlk:json-value body :string "path")))
        (is (equal "hello" (nlk:json-value body :string "content")))))))

(deftest upstash-box-cell-lists-the-account-s-boxes ()
  (with-box-wire (asked
                  ("/v2/box?label=nodecode"
                   (values (concatenate 'string "[" (box-json "box_a" "running") ","
                                        (box-json "box_b" "paused") "]") 200)))
    (with-cell-stop ((upstash-box-start))
      (is-carrying (answer (box-answer "b7" "box_list" "{}"))
                   ("2 Boxes:" "both are listed")
                   ("box_a  nodecode-box_a  running")
                   ("box_b  nodecode-box_b  paused"))
      (is (equal "https://box.test/v2/box?label=nodecode" (second (first asked))) "only this cell's label, unless the call says all"))))

;;; --- /box ------------------------------------------------------------------------------

(deftest upstash-box-cell-the-command-shows-lists-and-deletes ()
  (with-box-wire (asked
                  ("/v2/box/box_held" (values (box-json "box_held" "running") 200))
                  ("/pause" (values "{}" 200))
                  ("/v2/box?label=nodecode" (values (concatenate 'string "[" (box-json "box_held" "running") "]") 200)))
    (with-cell-stop ((upstash-box-start))
      (setf (nodecode-upstash-box::current-box) "box_held")
      (is (search "Box box_held: running" (cell-entry "nodecode-upstash-box" "box" "")) "bare /box shows the Box this cell holds")
      (is (search "box_held  nodecode-box_held  running"
                  (cell-entry "nodecode-upstash-box" "box" "list")))
      (is (search "Deleted Box box_held" (cell-entry "nodecode-upstash-box" "box" "delete")))
      (is (eq :delete (first (car (last asked)))) "the delete is the last thing said")
      (is (null (nodecode-upstash-box::current-box)) "and the Box is forgotten")
      (is (null (nodecode-upstash-box::stored-box)) "on disk too"))))
