;;;; cell-test.lisp --- lifecycle, the nine tools and /computer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, and a start with no router arguments refused), the tools through
;;;; the seam the model calls them by -- the router tool they ask for, the
;;;; arguments they send and the text they answer -- and /computer. Every
;;;; call into the router is stubbed, so no node starts and nothing leaves
;;;; the image.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *calls* '()
  "Every stubbed router call of the test, (TOOL . ARGUMENTS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed router answers: a function of TOOL and ARGUMENTS => a result object.")

(defmacro with-alans-way-router (&body body)
  "BODY with the router stubbed: a call is logged in *CALLS* and answered by
*ANSWER-FN*, so no test starts a process."
  `(let ((*calls* '())
         (*answer-fn* (lambda (tool)
                        (declare (ignore tool))
                        (error "unexpected router call")))
         (nodecode-alans-way::*router-call*
          (lambda (tool arguments)
            (push (cons tool arguments) *calls*)
            (funcall *answer-fn* tool))))
     ,@body))

(defun alans-way-asked ()
  "The stubbed calls, oldest first."
  (reverse *calls*))

(defun alans-way-call (name)
  "The arguments of the first call to NAME."
  (cdr (find name (alans-way-asked) :key #'car :test #'equal)))

(defun cell-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun alans-way-text-of (thunk)
  "THUNK's answer, or the refusal it signals as its text."
  (handler-case (funcall thunk)
    (condition (condition) (princ-to-string condition))))

(defun alans-way-reply (text &key host (error-p nil))
  "An MCP result carrying TEXT, as the router answers it."
  (nlk:json-object
   "content" (vector (nlk:json-object "type" "text" "text" text))
   "isError" (and error-p t)
   "_meta" (nlk:json-object "workspace" (nlk:json-object "host" host))))

(defun alans-way-apps-reply (&optional (host "computer"))
  (alans-way-reply
   "{\"host\":\"mac\",\"apps\":[{\"name\":\"Finder\",\"bundleId\":\"com.apple.finder\",\"pid\":400,\"frontmost\":true},{\"name\":\"Notes\",\"bundleId\":\"com.apple.Notes\",\"pid\":512}]}"
   :host host))

(defparameter *snapshot-json*
  "{\"windowId\":7,\"generation\":3,\"elements\":[{\"ref\":\"r1\",\"role\":\"button\",\"title\":\"New Note\",\"x\":10,\"y\":20,\"width\":40,\"height\":20},{\"ref\":\"r2\",\"role\":\"textfield\",\"title\":\"Search\",\"x\":100,\"y\":20,\"width\":200,\"height\":24,\"value\":\"\"}]}"
  "What the stubbed snapshot answers: two controls, one of them a field.")

(defun alans-way-snapshot-reply (&optional (host "computer"))
  (alans-way-reply *snapshot-json* :host host))

(defun alans-way-action-reply (&optional (note "Pressed."))
  (alans-way-reply
   (format nil "{\"generation\":4,\"results\":[{\"ok\":true,\"note\":~s}]}" note)
   :host "computer"))

(defun alans-way-answer (&rest functions)
  "Answer each expected router call in turn, one function per call."
  (let ((queue (copy-list functions)))
    (setf *answer-fn*
          (lambda (tool)
            (unless queue
              (error "unexpected router call ~a" tool))
            (funcall (pop queue) tool)))))

(defun alans-way-app-and-tree (tool)
  "The two-answer stub: the app list, then the element tree."
  (if (equal tool "workspace_computer_apps")
      (alans-way-apps-reply)
      (alans-way-snapshot-reply)))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "alans-way" "ALANS-WAY-CELL-" :start nodecode-alans-way:start-cell
  :defaults (("router_args" (vector "workspace-router.mjs"))))

(define-cell-lifecycle-tests "alans-way"
  (:fixture with-alans-way-router)
  (:help :computer)
  (:command "computer")
  (:running (is (equal "node" (nodecode-alans-way::setting :router-command)) "node by default")
            (is (equal '("workspace-router.mjs") (nodecode-alans-way::setting :router-args)))
            (is (= 60 (nodecode-alans-way::setting :call-timeout)))
            (is (nle::find-tool "computer_apps") "computer_apps is registered")
            (is (nle::find-tool "computer_capture"))
            (is (nle::find-tool "computer_click")))
  (:stopped (is (null (nle::find-tool "computer_capture")) "stop unregisters the tools"))
  (:refused ("router_args" #())))

(deftest alans-way-cell-a-section-without-router-arguments-is-refused ()
  (with-alans-way-router
    (let ((refusal (refusal-text nlk:config-refusal (alans-way-start "router_args" #()))))
      (is (search "router_args" refusal))
      (is (search "workspace router" refusal)))
    (is (null (nle::find-tool "computer_apps")) "a refused start installed nothing")))

;;; --- computer_apps and computer_capture ------------------------------------------

(deftest alans-way-cell-apps-are-listed-with-their-host ()
  (with-alans-way-router
    (with-cell-stop ((alans-way-start))
      (setf *answer-fn* (lambda (tool) (declare (ignore tool)) (alans-way-apps-reply)))
      (let ((answer (cell-tool "computer_apps" (nlk:json-object))))
        (is (search "On computer (mac):" answer) answer)
        (is (search "Finder (pid 400) [frontmost] com.apple.finder" answer))
        (is (search "Notes (pid 512)" answer)))
      (is (equal "computer" (nodecode-alans-way::current :host)) "the host it answered from is kept"))))

(deftest alans-way-cell-a-capture-numbers-the-controls-it-reads ()
  (with-alans-way-router
    (with-cell-stop ((alans-way-start))
      (setf *answer-fn* #'alans-way-app-and-tree)
      (let ((answer (cell-tool "computer_capture" (nlk:json-object "app" "Finder"))))
        (is (search "Finder (pid 400), window 7" answer) "the target is named")
        (is (search " button \"New Note\" at (10, 20) 40x20" answer) "the first control, numbered")
        (is (search " textfield \"Search\"" answer))
        (is (search "No screenshot is attached" answer) "named controls need no pixels"))
      (let ((arguments (alans-way-call "workspace_computer_snapshot")))
        (is (equal 400 (gethash "pid" arguments)) "the pid the name resolved to")
        (is (null (gethash "delta" arguments)) "a whole list, not a delta"))
      (is (= 2 (length (nodecode-alans-way::current-elements)))))))

(deftest alans-way-cell-a-reply-from-another-host-is-recorded ()
  (with-alans-way-router
    (with-cell-stop ((alans-way-start))
      (setf *answer-fn* #'alans-way-app-and-tree)
      (is (search "New Note" (cell-tool "computer_capture" (nlk:json-object "app" "Finder"))))
      (setf *answer-fn* (lambda (tool) (declare (ignore tool)) (alans-way-snapshot-reply "vps")))
      (is (search "New Note" (cell-tool "computer_capture" (nlk:json-object "pid" 400 "app" "Finder"))))
      (is (equal "vps" (nodecode-alans-way::current :host)) "the host that answered is kept"))))

;;; --- the gestures ---------------------------------------------------------------

(deftest alans-way-cell-a-click-acts-on-the-numbered-control ()
  (with-alans-way-router
    (with-cell-stop ((alans-way-start))
      (setf *answer-fn* #'alans-way-app-and-tree)
      (cell-tool "computer_capture" (nlk:json-object "app" "Finder"))
      (setf *calls* '())
      (alans-way-answer (lambda (tool) (declare (ignore tool)) (alans-way-action-reply)))
      (let ((answer (cell-tool "computer_click" (nlk:json-object "element" 1))))
        (is (search "click sent to Finder (pid 400)" answer) answer)
        (is (search "Pressed." answer) answer))
      (let ((arguments (alans-way-call "workspace_computer_action")))
        (is (equal 400 (gethash "pid" arguments)))
        (is (equal "press" (gethash "action" arguments)) "a numbered control is pressed by ref")
        (is (equal "r1" (gethash "ref" arguments)))
        (is (equal 7 (gethash "windowId" arguments)) "the window the capture read")
        (is (equal 3 (gethash "generation" arguments)))
        (is (equal "computer" (gethash "expect_host" arguments))))
      (let ((refusal (alans-way-text-of
                      (lambda () (cell-tool "computer_click" (nlk:json-object "element" 9))))))
        (is (search "element 9 is not in the last capture" refusal))))))

(deftest alans-way-cell-set-value-types-into-the-numbered-field ()
  (with-alans-way-router
    (with-cell-stop ((alans-way-start))
      (setf *answer-fn* #'alans-way-app-and-tree)
      (cell-tool "computer_capture" (nlk:json-object "app" "Finder"))
      (setf *calls* '())
      (alans-way-answer (lambda (tool) (declare (ignore tool)) (alans-way-action-reply "Typed.")))
      (let ((answer (cell-tool "computer_set_value"
                              (nlk:json-object "element" 2 "value" "quarterly notes"))))
        (is (search "Typed." answer) answer))
      (let ((arguments (alans-way-call "workspace_computer_action")))
        (is (equal "type" (gethash "action" arguments)))
        (is (equal "r2" (gethash "ref" arguments)))
        (is (equal "quarterly notes" (gethash "text" arguments))))
      (let ((refusal (alans-way-text-of
                      (lambda () (cell-tool "computer_set_value" (nlk:json-object "value" "x"))))))
        (is (search "set_value needs element" refusal))))))

(deftest alans-way-cell-a-router-failure-is-the-models-repair ()
  (with-alans-way-router
    (with-cell-stop ((alans-way-start))
      (setf *answer-fn*
            (lambda (tool) (declare (ignore tool))
              (alans-way-reply "locked: the screen is locked" :error-p t)))
      (let ((refusal (alans-way-text-of
                      (lambda () (cell-tool "computer_apps" (nlk:json-object))))))
        (is (search "ERROR: alans-way:" refusal))
        (is (search "locked" refusal))
        (is (search "the screen is locked" refusal))))))

;;; --- /computer --------------------------------------------------------------------

(deftest alans-way-cell-the-command-says-what-it-drives ()
  (with-alans-way-router
    (with-cell-stop ((alans-way-start))
      (is (search "Driving nothing yet" (cell-entry "nodecode-alans-way" "computer" "")))
      (setf *answer-fn* #'alans-way-app-and-tree)
      (cell-tool "computer_capture" (nlk:json-object "app" "Finder"))
      (is (search "Driving Finder (pid 400), window 7 on computer: 2 controls"
                  (cell-entry "nodecode-alans-way" "computer" "")))
      (is (search "Notes (pid 512)" (cell-entry "nodecode-alans-way" "computer" "apps"))))))
