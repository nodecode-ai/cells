;;;; cell-test.lisp --- lifecycle, the editor verbs over a stubbed bridge, and /godot.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a path that is not a folder refused, a start that names no bridge
;;;; refused), the frame one request sends -- its action, its token, its id and
;;;; its payload -- the answer rendered back to the model, the ways an exchange
;;;; fails, and /godot. Every bridge is a stubbed one, so no test opens a
;;;; socket and no editor has to be running.

(in-package #:nodecode.test)

;;; --- a scratch Godot project ------------------------------------------------

(defun godot-project (&key (port "9080") (token "tok-1"))
  "A folder that looks like a Godot project with the addon's published files,
and its path: what the `project' member points at."
  (let* ((root (merge-pathnames "godot-cell-project/" (uiop:temporary-directory)))
         (argos (merge-pathnames ".argos/" root)))
    (ensure-directories-exist (merge-pathnames "keep" argos))
    (dolist (pair (list (cons "port" port) (cons "token" token)))
      (with-open-file (out (merge-pathnames (car pair) argos)
                           :direction :output :if-exists :supersede)
        (format out "~a~%" (cdr pair))))
    (string-right-trim "/" (uiop:native-namestring root))))

;;; --- the stubbed bridge -------------------------------------------------------

(defvar *frames* '()
  "Every frame the stub bridge was sent, decoded, newest first.")

(defvar *urls* '()
  "Every URL a bridge was opened to, newest first.")

(defvar *opens* 0)
(defvar *closes* 0)

(defvar *answer-fn* nil
  "What the stub bridge answers: a function of the decoded REQUEST =>
the decoded reply.")

(defun godot-reply (request result)
  "The `ok' reply the addon sends to REQUEST, carrying RESULT."
  (nlk:json-object "ok" t "id" (gethash "id" request) "result" result))

(defun godot-refusal (request code &optional detail)
  "The `error' reply the addon sends to REQUEST."
  (nlk:make-json-object "ok" nil "id" (gethash "id" request) "error" code "detail" detail))

(defmacro with-godot-bridge (&body body)
  "BODY with the cell's bridge stubbed: every exchange is logged in *FRAMES*
and *URLS* and answered by *ANSWER-FN*, so nothing dials 127.0.0.1."
  `(let ((*frames* '())
         (*urls* '())
         (*opens* 0)
         (*closes* 0)
         (*answer-fn* (lambda (request) (godot-reply request (nlk:make-json-object "pong" t))))
         (nodecode-godot::*bridge-factory*
           (lambda (url)
             (push url *urls*)
             (incf *opens*)
             (nodecode-godot::make-bridge
              (lambda (text) (push (nlk:decode-json text) *frames*))
              (lambda ()
                (if *frames*
                    (nlk:encode-json-object (funcall *answer-fn* (car *frames*)))
                    ""))
              (lambda () (incf *closes*))))))
     ,@body))

(defun godot-sent ()
  "The frames the stub was sent, oldest first."
  (reverse *frames*))

(defun godot-field (request name)
  (gethash name (or request (nlk:make-json-object))))

(defun godot-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun godot-answers (result)
  "Make the editor answer every request with RESULT."
  (setf *answer-fn* (lambda (request) (godot-reply request result))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "godot" "GODOT-CELL-" :start nodecode-godot:start-cell)

(define-cell-lifecycle-tests "godot"
  (:fixture with-godot-bridge)
  (:config "project" (godot-project))
  (:help :godot)
  (:command "godot")
  (:running (is (nle::find-tool "godot_scene_tree") "the editor verbs are registered")
            (is (nle::find-tool "godot_batch"))
            (is (nle::find-tool "godot_capture"))
            (is (equal "9080" (nodecode-godot::bridge-file "port")) "the addon's port is read back"))
  (:stopped (is (null (nle::find-tool "godot_scene_tree")) "stop unregisters the verbs"))
  (:refused ())
  (:idle nodecode-godot:godot-error (nodecode-godot::run-godot-command "" nil)))

(deftest godot-cell-a-path-that-is-not-a-folder-is-refused ()
  (with-godot-bridge
    (is-carrying (answer (refusal-text nlk:config-refusal
                                        (godot-start "project" "/nonexistent/godot-cell-project")))
                 ("is not a folder" "project.godot"))))

;;; --- the frame one call sends ------------------------------------------------

(deftest godot-cell-a-tool-call-carries-its-action-token-and-payload ()
  (with-godot-bridge
    (with-cell-stop ((godot-start "project" (godot-project)))
      (godot-answers (nlk:json-object "edited" (nlk:json-object "path" "res://main.tscn")))
      (let ((answer (godot-tool "godot_scene_tree" (nlk:json-object "max_nodes" 500))))
        (is (search "res://main.tscn" answer) "the result is what the model reads")
        (is (= 1 *opens*) "one bridge per exchange")
        (is (= 1 *closes*) "and it is closed after the answer")
        (is (equal '("ws://127.0.0.1:9080/") *urls*) "the published port, on loopback")
        (let ((frame (first (godot-sent))))
          (is (equal "scene.get_tree" (godot-field frame "action")))
          (is (equal "tok-1" (godot-field frame "token")) "the token the addon published")
          (is (= 1 (godot-field frame "id")) "the first request of the run")
          (is (= 500 (godot-field frame "max_nodes")) "the argument rides the frame"))))))

(deftest godot-cell-the-section-wins-over-the-published-files ()
  (with-godot-bridge
    (with-cell-stop ((godot-start "project" (godot-project) "port" 9091 "token" "pinned"))
      (godot-tool "godot_ping" (nlk:make-json-object))
      (is (equal '("ws://127.0.0.1:9091/") *urls*) "the pinned port")
      (is (equal "pinned" (godot-field (first (godot-sent)) "token")) "the pinned token"))))

(deftest godot-cell-a-node-set-carries-its-value-untouched ()
  (with-godot-bridge
    (with-cell-stop ((godot-start "project" (godot-project)))
      (godot-answers (nlk:json-object "set" t))
      (is (search "\"set\": true"
                  (godot-tool "godot_node_set"
                               (nlk:json-object "path" "Player" "prop" "position"
                                                "value" "@vec2:16,32"))))
      (let ((frame (first (godot-sent))))
        (is (equal "node.set_prop" (godot-field frame "action")))
        (is (equal "position" (godot-field frame "prop")))
        (is (equal "@vec2:16,32" (godot-field frame "value")) "the value is passed as written")))))

(deftest godot-cell-a-batch-is-one-frame ()
  (with-godot-bridge
    (with-cell-stop ((godot-start "project" (godot-project)))
      (godot-answers (nlk:json-object "ok" t "ops" 2))
      (godot-tool "godot_batch"
                   (nlk:json-object "name" "arena" "on_error" "abort"
                                     "ops" (vector (nlk:json-object "action" "node.create"
                                                                     "class" "Node2D"
                                                                     "parent" ""))))
      (let ((frame (first (godot-sent))))
        (is (equal "action.batch" (godot-field frame "action")))
        (is (equal "arena" (godot-field frame "name")))
        (is (vectorp (godot-field frame "ops")) "the operations ride as one array")))))

(deftest godot-cell-play-and-capture-pick-their-action ()
  (with-godot-bridge
    (with-cell-stop ((godot-start "project" (godot-project)))
      (godot-tool "godot_play" (nlk:make-json-object))
      (is (equal "run.play_main" (godot-field (first (godot-sent)) "action")))
      (godot-tool "godot_play" (nlk:json-object "scene_path" "res://levels/boss.tscn"))
      (is (equal "run.play_scene" (godot-field (first *frames*) "action")))
      (is (equal "res://levels/boss.tscn" (godot-field (first *frames*) "path")))
      (godot-tool "godot_capture" (nlk:json-object "out" "shot.png" "session" "qa"))
      (let ((frame (first *frames*)))
        (is (equal "capture.editor_2d" (godot-field frame "action")))
        (is (equal "qa" (godot-field frame "session")) "the capture lands under the session")
        (is (equal "shot.png" (godot-field frame "out"))))
      (godot-tool "godot_capture" (nlk:json-object "session" "qa" "scene_path" "res://main.tscn"))
      (let ((frame (first *frames*)))
        (is (equal "capture.scene" (godot-field frame "action")))
        (is (equal "res://main.tscn" (godot-field frame "path")))))))

(deftest godot-cell-raw-carries-its-own-payload ()
  (with-godot-bridge
    (with-cell-stop ((godot-start "project" (godot-project)))
      (godot-tool "godot_raw" (nlk:json-object "action" "scene.get_tree"
                                                 "payload" (nlk:json-object "max_depth" 3)))
      (let ((frame (first (godot-sent))))
        (is (equal "scene.get_tree" (godot-field frame "action")))
        (is (= 3 (godot-field frame "max_depth")) "the payload is merged into the frame"))
      (is-carrying (answer (godot-tool "godot_raw" (nlk:make-json-object)))
                   ("ERROR: godot:" "needs \"action\"")))))

;;; --- what a failure looks like ------------------------------------------------

(deftest godot-cell-a-refused-action-is-a-repair-not-a-crash ()
  (with-godot-bridge
    (with-cell-stop ((godot-start "project" (godot-project)))
      (setf *answer-fn*
            (lambda (request) (godot-refusal request "no_scene" "no hay escena editada")))
      (is-carrying (answer (godot-tool "godot_scene_tree" (nlk:make-json-object)))
                   "ERROR: godot:" "no_scene" "no hay escena editada"))))

(deftest godot-cell-a-bridge-that-never-opens-says-so ()
  (with-godot-bridge
    (with-cell-stop ((godot-start "project" (godot-project)))
      (let ((nodecode-godot::*bridge-factory*
              (lambda (url)
                (declare (ignore url))
                (error "connection refused"))))
        (is-carrying (answer (godot-tool "godot_ping" (nlk:make-json-object)))
                     ("ERROR: godot:" "connection refused"))))))

(deftest godot-cell-no-published-port-is-a-repair ()
  (with-godot-bridge
    (with-cell-stop ((godot-start "project" (godot-project :port "")))
      (is-carrying (answer (godot-tool "godot_ping" (nlk:make-json-object)))
                   ("ERROR: godot:" "no bridge port")))))

;;; --- /godot ------------------------------------------------------------------

(deftest godot-cell-the-command-shows-the-editor ()
  (with-godot-bridge
    (with-cell-stop ((godot-start "project" (godot-project)))
      (godot-answers (nlk:json-object
                       "godot" (nlk:json-object "string" "4.3.stable.official" "major" 4)
                       "edited_scene" "res://main.tscn"
                       "is_playing" nil
                       "port" 9080))
      (let ((answer (cell-entry "nodecode-godot" "godot" "")))
        (is (search "ws://127.0.0.1:9080/" answer))
        (is (search "Godot 4.3.stable.official" answer))
        (is (search "res://main.tscn" answer))
        (is (search "playing: no" answer)))
      (setf *answer-fn* (lambda (request) (godot-refusal request "auth" "token invalido")))
      (let ((answer (cell-entry "nodecode-godot" "godot" "")))
        (is (search "no answer: " answer))
        (is (search "auth" answer) "a dead bridge is reported, not signalled")))))
