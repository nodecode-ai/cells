;;;; cell.lisp --- named model routes for subagents: one `delegate' tool and
;;;; /delegation, over the routes in the `delegation' section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A route is a name and a line about it, and behind it a provider, a model
;;;; and an optional reasoning effort. Only the names and the lines reach the
;;;; model -- the provider, the model id and the endpoint never become tool
;;;; parameters, so the agent cannot invent a route -- and a task that picks
;;;; one runs in a fresh subagent session on exactly that route, which reports
;;;; what it ran on. A task that picks none runs on `default_route' when one is
;;;; set, else on the parent's own model.
;;;;
;;;; `backend' is the mode: `model_map' hands the agent the route list,
;;;; `default' keeps every subagent on the parent's model and refuses a route
;;;; out loud, naming /delegation, rather than running it on the wrong one.
;;;; The section is read at start, so the tool list of a chat keeps the
;;;; choices it started with; /delegation saves a mode and says how to apply it.
;;;;
;;;; The rewrite of Hermes's `model_map' delegation provider; Hermes's
;;;; `profile_routes' mode is not here (a Nodecode profile is a separate home
;;;; with its own organism, not an in-image seat). See NOTICE.

(defpackage #:nodecode-delegation
  (:use #:cl))

(in-package #:nodecode-delegation)

(nlk:define-peripheral delegation :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "A named model route is one line of this home's config: a name the agent picks, a
line about it the agent reads, and the provider, model and optional
reasoning_effort it stands for. Only names and lines reach the model.
With delegation.backend model_map, the `delegate' tool takes a task and a route:
it runs the task in a fresh subagent session on that route and answers the
subagent's last words and the route it ran on. A task with no route runs on
delegation.default_route when set, else on the parent's own model. With backend
default the tool refuses a route and names /delegation. /delegation prints the
mode and the routes, and /delegation model_map (or /delegation default) saves
the mode; a restart of the cell, (restart-cells \"nodecode-delegation\"), is what
puts the new tool list on. Routes are written in the delegation section:
\"routes\": [{\"name\": \"fast\", \"description\": \"Cheap and quick.\", \"provider\":
\"openrouter\", \"model\": \"google/gemini-3-flash-preview\"}]. A malformed route is
skipped, and the cell's notice says which and why.")

;;; --- routes -------------------------------------------------------------------

(defstruct (route (:constructor make-route (name description provider model effort)))
  "One named model route: what the agent picks, what it reads, and what the
subagent actually runs on."
  name description provider model effort)

(defun squeeze (value)
  "VALUE as one line of text: runs of whitespace collapsed to one space."
  (if (stringp value)
      (with-output-to-string (out)
        (let ((space nil))
          (loop for char across (string-trim '(#\Space #\Tab #\Newline #\Return) value)
                do (if (member char '(#\Space #\Tab #\Newline #\Return))
                       (setf space t)
                       (progn (when space (write-char #\Space out))
                              (setf space nil)
                              (write-char char out))))))
      ""))

(defun name-char-p (char)
  (or (alphanumericp char) (member char '(#\. #\_ #\-))))

(defun route-name-p (name)
  "Whether NAME is a route name: letters, digits, '.', '_' or '-', up to 64,
not starting with a punctuation mark."
  (and (stringp name)
       (<= 1 (length name) 64)
       (alphanumericp (char name 0))
       (every #'name-char-p name)))

(defun route-entry (object i)
  "One route off the config's `routes' array, as a ROUTE, or a problem string
naming index I."
  (flet ((text (key) (squeeze (nlk:json-value object :any key))))
    (let ((name (text "name")) (description (text "description"))
          (provider (text "provider")) (model (text "model"))
          (effort (text "reasoning_effort")))
      (cond ((not (route-name-p name))
             (format nil "routes[~d] needs a name (letters, digits, '.', '_' or '-', ~
                          no spaces; it is what the agent picks)" i))
            ((zerop (length description))
             (format nil "route ~a needs a description: it is what the agent reads to choose" name))
            ((zerop (length model))
             (format nil "route ~a needs a model" name))
            ((zerop (length provider))
             (format nil "route ~a needs a provider: a Nodecode provider section carries the endpoint" name))
            (t (make-route name description provider model
                           (if (zerop (length effort)) nil effort)))))))

(defun read-routes (raw)
  "The `routes' array as a list of ROUTE in config order, and the problems of
the entries skipped. A malformed entry is skipped, never guessed at."
  (let ((entries (cond ((vectorp raw) (coerce raw 'list))
                       ((listp raw) raw)
                       (t nil)))
        (routes '()) (problems '()))
    (when (and raw (not (or (vectorp raw) (listp raw))))
      (push "delegation.routes must be an array of route objects; ignoring it" problems))
    (loop for object in entries
          for i from 0
          do (if (not (hash-table-p object))
                 (push (format nil "routes[~d] is not an object" i) problems)
                 (let ((route (route-entry object i)))
                   (cond ((stringp route) (push route problems))
                         ((find (route-name route) routes :key #'route-name :test #'string=)
                          (push (format nil "route ~a is listed twice; the first one stands"
                                        (route-name route))
                                problems))
                         (t (push route routes))))))
    (values (nreverse routes) (nreverse problems))))

(defun routes () (setting :routes))

(defun route-names () (mapcar #'route-name (routes)))

(defun route-for (name)
  (and (stringp name)
       (find name (routes) :key #'route-name :test #'string=)))

(defun route-summary (route)
  "`provider . model [. reasoning X]' for one route."
  (format nil "~a · ~a~@[ · reasoning ~a~]"
          (route-provider route) (route-model route) (route-effort route)))

(defun route-report (route &optional session)
  "The line a delegated answer carries: what it ran on, and where it ran."
  (format nil "[route ~a: ~a~@[ · session ~a~]]"
          (route-name route) (route-summary route) session))

;;; --- the subagent -------------------------------------------------------------

(defparameter *backend* "default"
  "The mode the cell is running in: `model_map' or `default'. Set at start from
the section; /delegation moves it when it saves a mode.")

(defparameter *run-task* 'run-task
  "What a routed task is handed to: a function (ROUTE TASK SECONDS) answering
the report. A symbol, so a test or another cell can substitute one.")

(defparameter *save-mode* 'save-mode
  "What /delegation writes a mode through: (MODE) => a line about the write.")

(defun event-count (id kind) (nlk:events :session-id id :kind kind :as :count))

(defun last-reply (id)
  "The newest assistant text of session ID, or NIL."
  (let* ((payload (nlk:events :session-id id :kind "turn.assistant_message_completed"
                               :order :newest :as :payload))
         (message (and payload (nlk:json-value payload :any "message")))
         (text (and message (nle::content-text (nlk:json-value message :any "content")))))
    (and (stringp text) (plusp (length text)) text)))

(defun active-turn-id (id)
  (let ((payload (nlk:events :session-id id :kind "turn.started" :order :newest :as :payload)))
    (and payload (nlk:json-value payload :string "turn-id"))))

(defun wait-for-idle (id seconds)
  "Wait until ID's turn ends => :DONE, or :TIMEOUT once it was asked to stop."
  (let ((deadline (+ (get-universal-time) seconds)))
    (loop
      (let ((begun (event-count id "turn.started"))
            (ended (+ (event-count id "turn.completed")
                      (event-count id "turn.failed")
                      (event-count id "turn.cancelled"))))
        (cond ((and (plusp begun) (>= ended begun)) (return :done))
              ((> (get-universal-time) deadline)
               (ignore-errors (nlk:request-cancel-turn id (active-turn-id id)
                                                       "the delegate call's seconds ran out"))
               (return :timeout))
              (t (sleep 0.5)))))))

(defun run-task (route task seconds)
  "Run TASK in a fresh subagent session on ROUTE and answer its last words and
the route they came off."
  (unless (nlk:store-open-p) (fail "no store is open; a subagent is a durable session"))
  (let* ((origin nlk:*scribe-session-id*)
         (id (nlk:make-durable-id "delegate"))
         (cwd (or (and origin (ignore-errors (nlk:find-session-cwd origin))) (uiop:getcwd)))
         (parent (and origin (nlk:session-exists-p origin) origin)))
    (nlk:create-session :id id :cwd (namestring cwd) :parent parent :detached t)
    ;; After the create, so it overrides the parent's pin: newest wins.
    (nlk:record-session-model-selection id
                                        :provider (route-provider route)
                                        :model (route-model route)
                                        :effort (route-effort route))
    (nle:submit id task :source "delegation" :source-id origin)
    (let ((ending (wait-for-idle id seconds)))
      (format nil "~a~%~a~a"
              (or (last-reply id) "(the subagent's turn ended without an answer)")
              (route-report route id)
              (if (eq ending :timeout)
                  (format nil "~%the subagent ran past its ~d seconds and was stopped" seconds)
                  "")))))

;;; --- the tool -----------------------------------------------------------------

(defun route-lines ()
  (with-output-to-string (out)
    (loop for route in (routes)
          for sep = "" then (string #\Newline)
          do (format out "~a~a: ~a" sep (route-name route) (route-description route)))))

(defun delegate-schema ()
  "The `delegate' schema for the routes this cell started with: an enum of their
names, whose description carries only the names and the lines written for them."
  (let ((properties (nlk:make-json-object
                     "task" (nlk:json-object
                             "type" "string"
                             "description" "The task the subagent does, whole: it sees only this.")
                     "route" (nlk:json-object
                              "type" "string"
                              "enum" (coerce (route-names) 'vector)
                              "description" (format nil "Which configured model route runs this subagent; ~
                                                         omit for the default route.~%~a" (route-lines))))))
    (list :description "Run one task in a subagent on a named model route, and answer what it said."
          :parameters (nlk:make-json-object
                       "type" "object" "properties" properties "required" (vector "task")))))

(defun delegate (arguments)
  "The `delegate' tool: ARGUMENTS the decoded call, a hash table."
  (let ((task (nlk:json-value arguments :string "task"))
        (picked (nlk:json-value arguments :string "route")))
    (unless (and (stringp task) (plusp (length (squeeze task))))
      (fail "a delegated task needs some words: task"))
    (unless (string= *backend* "model_map")
      (fail "delegation.backend is ~a: every subagent runs the parent's model. /delegation model_map ~
             switches it." (string-downcase *backend*)))
    (let ((name (if (and (stringp picked) (plusp (length picked))) picked (setting :default-route))))
      (when (or (null name) (zerop (length name)))
        (fail "no route given and delegation.default_route is empty: name a route, ~
               or set delegation.default_route"))
      (let ((route (route-for name)))
        (unless route
          (fail "~a is not a configured route~@[ (delegation.routes: ~{~a~^, ~})~]"
                name (route-names)))
        (funcall *run-task* route task (setting :seconds))))))

;;; --- /delegation ---------------------------------------------------------------

(defparameter +modes+ '(("model_map" . "Model routes: the agent picks a named model route per subagent")
                         ("default" . "Default: every subagent runs the parent's model"))
  "The modes this cell knows, and the line each one shows.")

(defun mode-label (mode)
  (if (string= mode "model_map") "Model routes" "Default"))

(defun describe-mode ()
  "The text /delegation typed alone answers with: the mode, then each route as
its name, its line and what it runs on."
  (with-output-to-string (out)
    (format out "Delegation mode: ~a (delegation.backend: ~a)~%"
            (mode-label *backend*) (string-downcase *backend*))
    (if (setting :default-route)
        (format out "  Model routes (~d), default: ~a:~%" (length (routes)) (setting :default-route))
        (format out "  Model routes (~d), no default:~%" (length (routes))))
    (if (routes)
        (dolist (route (routes))
          (format out "    ~a  ~a~%    ~a~@[  (default)~]~%"
                  (route-name route) (route-description route) (route-summary route)
                  (equal (route-name route) (setting :default-route))))
        (format out "    (none yet: write them in the delegation section of the config)~%"))
    (format out "  A task that picks no route runs on the default, else on the parent's model.")))

(defun save-mode (mode)
  "Save MODE as this home's delegation.backend. => a line about the write."
  (handler-case
      (progn (nle:config-set '("delegation") (list :backend mode))
             (format nil "delegation.backend is now ~a" mode))
    (error (condition)
      (format nil "could not write the config (~a); the mode is in effect for this cell until it restarts"
              condition))))

(defun run-slash (args session-id)
  "Typing /delegation alone: the mode and the routes, with the other mode as a row.
/delegation MODE saves the mode."
  (declare (ignore session-id))
  (let ((wanted (squeeze args)))
    (cond
      ((zerop (length wanted))
       (let ((other (if (string= *backend* "model_map") "default" "model_map")))
         (values (describe-mode)
                 (nle:list-dialog
                  :title "Delegation"
                  :rows (list (nle:run-row (format nil "Switch to ~a" (mode-label other))
                                           (format nil "/delegation ~a" other)))))))
      ((string-equal wanted "show") (describe-mode))
      (t
       (let ((mode (cond ((member wanted '("model_map" "models" "2") :test #'string-equal) "model_map")
                         ((member wanted '("default" "1") :test #'string-equal) "default")
                         (t nil))))
         (unless mode
           (fail "~a is not a delegation mode: use default or model_map" wanted))
         (setf *backend* mode)
         (format nil "~a~%~a~%New sessions use it; (restart-cells \"nodecode-delegation\") puts the new tool list on."
                 (funcall *save-mode* mode)
                 (if (string= mode "model_map")
                     "The `delegate' tool offers the configured routes."
                     "Every subagent runs the parent's model; a route of the other mode is refused.")))))))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell delegation
  (:section ("delegation")
    (:guide "One route per model you delegate to: routes is an array of {name, description, provider, model} objects (reasoning_effort optional); backend model_map hands the agent those names, default keeps every subagent on the parent's model")
    ("backend" :choice :options '("default" "model_map") :default "default"
               :doc "model_map: the agent picks a named route per subagent; default: every subagent runs the parent's model")
    ("default_route" :string
                     :doc "route a task that picks none runs on; empty means the parent's model")
    ("seconds" :integer :default 900 :min 30
               :doc "how long one delegated subagent's turn may run before it is stopped"))
  (:settings (lambda (values table)
               (multiple-value-bind (routes problems)
                   (read-routes (ignore-errors (nlk:json-value table :any "routes")))
                 (append values (list :routes routes :route-problems problems)))))
  (:help :delegation
         "delegation: named model routes a subagent can be sent to, and /delegation to see or switch the mode"
         +primer+)
  (:command "delegation" 'run-slash :catches 'delegation-error
            :description "The delegation mode and its model routes; /delegation MODE switches it")
  (:start (lambda ()
            (setf *backend* (setting :backend))
            (when (setting :route-problems)
              (nle:notice (format nil "delegation: ~{~a~^; ~}" (setting :route-problems))
                          :level :warning :key "nodecode-delegation")
              (nle:on-stop (lambda () (nle:notice nil :key "nodecode-delegation"))))
            (when (and (string= *backend* "model_map") (routes))
              (nle:register-tool "delegate" (delegate-schema) #'delegate)
              (nle:on-stop (lambda () (nle:unregister-tool "delegate")))))))
