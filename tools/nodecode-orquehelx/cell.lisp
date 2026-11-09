;;;; cell.lisp --- routes across subscriptions: one delegate_to tool and /ohx.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A route is the name the model uses to pick where a subagent runs, and
;;;; behind it the provider and model that subagent runs on. `delegate_to'
;;;; takes a route, a goal and optional context, runs the goal in a fresh
;;;; subagent session pinned to that provider and model, and answers what the
;;;; child said. When the child fails because its provider ran out of quota,
;;;; the answer says so, names the other routes and carries the instruction
;;;; the `policy' asks for -- it never switches routes on its own.
;;;;
;;;; The rewrite of TicoraX's OrqueHelx plugin. See NOTICE.

(defpackage #:nodecode-orquehelx
  (:use #:cl))

(in-package #:nodecode-orquehelx)

(nlk:define-peripheral orquehelx :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "A route is the name a subagent can be sent to, and behind it the provider and
model it runs on. With routes written in the orquehelx section, the `delegate_to'
tool takes a route, a goal and optional context, runs the goal in a fresh subagent
session pinned to that route's provider and model, and answers the child's last
words and the route it ran on. `policy' is what the parent does when a route runs
out of quota: ask (tell the user and wait, the default), parent_decides (may
resend on another route, saying which) or wait (nothing else). /ohx routes lists
the routes; /ohx quota says which provider is recorded out of quota. Routes are
written as \"routes\": [{\"name\": \"claude\", \"provider\": \"anthropic\",
\"model\": \"claude-haiku-4-5\"}]: a name is lowercase letters, digits or '-' up
to 32, and provider and model are both required. A malformed route is skipped,
and the cell's notice says which and why.")

;;; --- routes -------------------------------------------------------------------

(defstruct (route (:constructor make-route (name provider model)))
  "One route: the name the agent picks, and the provider and model a subagent
sent to it runs on."
  name provider model)

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

(defun route-name-p (name)
  "Whether NAME is a route name: lowercase letters, digits or '-', up to 32,
starting with a letter or a digit."
  (and (stringp name)
       (<= 1 (length name) 32)
       (alphanumericp (char name 0))
       (every (lambda (char) (or (digit-char-p char) (char<= #\a char #\z) (char= char #\-))) name)))

(defparameter +fields+ '("name" "provider" "model")
  "The keys one route object carries, and the only ones.")

(defun route-entry (object i)
  "One route off the config's `routes' array, as a ROUTE, or a problem string
naming index I."
  (flet ((text (key) (squeeze (nlk:json-value object :any key))))
    (let ((name (text "name")) (provider (text "provider")) (model (text "model"))
          (extra (loop for key being the hash-keys of object
                          when (not (member key +fields+ :test #'string=)) collect key)))
      (cond ((not (route-name-p name))
             (format nil "routes[~d] needs a name: lowercase letters, digits or '-', up to 32 ~
                          (it is what the agent picks)" i))
            (extra
             (format nil "route ~a: unknown fields ~{~a~^, ~}" name extra))
            ((zerop (length provider))
             (format nil "route ~a needs a provider: a Nodecode provider section carries the endpoint" name))
            ((zerop (length model))
             (format nil "route ~a needs a model: without one the subagent would run the parent's model ~
                          on another provider" name))
            (t (make-route name provider model))))))

(defun read-routes (raw)
  "The `routes' array as a list of ROUTE in config order, and the problems of
the entries skipped. A malformed entry is skipped, never guessed at."
  (let ((entries (cond ((vectorp raw) (coerce raw 'list))
                       ((listp raw) raw)
                       (t nil)))
        (routes '()) (problems '()))
    (when (and raw (not (or (vectorp raw) (listp raw))))
      (push "orquehelx.routes must be an array of route objects; ignoring it" problems))
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
  "`provider (model)' for one route."
  (format nil "~a (~a)" (route-provider route) (route-model route)))

(defun route-lines ()
  "Each route as its name, then what it runs on -- the text the tool's route
parameter carries."
  (format nil "~{~a~^, ~}"
          (loop for route in (routes)
                collect (format nil "~a (~a)" (route-name route) (route-model route)))))

;;; --- quota bookkeeping --------------------------------------------------------

(defstruct (exhaustion (:constructor make-exhaustion (provider detail seen reset-at)))
  "One provider seen out of quota: the failure's own words, when, and the reset
it named -- NIL when it named none, which is what /ohx prints as unknown."
  provider detail seen reset-at)

(defparameter *exhaustions* (make-hash-table :test #'equal)
  "Provider -> EXHAUSTION, the last one recorded in this image.")

(defparameter *exhausted-turn* nil
  "The turn id in which a route ran out of quota: with policy ask or wait no
further route may be picked until the user's answer opens a new turn.")

(defun clock-text (universal-time)
  "UNIVERSAL-TIME as the wall clock reads it here, HH:MM."
  (multiple-value-bind (second minute hour) (decode-universal-time universal-time)
    (declare (ignore second))
    (format nil "~2,'0d:~2,'0d" hour minute)))

(defun exhaustion-text-p (text)
  "Whether the failure TEXT is a provider saying it ran out of quota, by the
original's own rules: RESOURCE_EXHAUSTED with the words that go with it, or a
429 that speaks of a usage limit or a quota. Anything else is a failure, not
quota, and is never reported as one."
  (let* ((words (string-downcase (or text "")))
         (said (lambda (phrase) (and (search phrase words) t))))
    (or (and (funcall said "resource_exhausted")
             (or (funcall said "individual quota") (funcall said "quota exceeded")
                 (funcall said "code 429")))
        (and (funcall said "429") (or (funcall said "usage limit") (funcall said "quota"))))))

(defun record-exhaustion (provider text &optional reset-at)
  "Note PROVIDER as out of quota, in TEXT's own words, resetting at RESET-AT
when the provider said so."
  (setf (gethash provider *exhaustions*)
        (make-exhaustion provider (subseq text 0 (min (length text) 300))
                          (get-universal-time) reset-at)))

(defun exhaustion-for (provider)
  "The recorded exhaustion of PROVIDER, or NIL."
  (gethash provider *exhaustions*))

(defun exhaustion-recent-p (exhaustion)
  "Whether EXHAUSTION was seen in the last day, the window /ohx quota reports."
  (and exhaustion
       (<= (- (get-universal-time) (exhaustion-seen exhaustion)) (* 24 3600))))

;;; --- the subagent -------------------------------------------------------------

(defstruct (outcome (:constructor make-outcome (status &key summary error session)))
  "What one delegated turn came to: STATUS is :COMPLETED, :FAILED or :TIMEOUT;
SUMMARY is the child's last words, ERROR the failure's detail."
  status summary error session)

(defparameter *run-task* 'run-task
  "What a routed task is handed to: a function (ROUTE GOAL CONTEXT SECONDS)
answering an OUTCOME. A symbol, so a test or another cell can substitute one.")

(defparameter *current-turn* 'current-turn-id
  "What answers the turn the parent is in: a function () answering the id or
NIL. A symbol, so a test can stand in for the store's own.")

(defun event-count (id kind) (nlk:events :session-id id :kind kind :as :count))

(defun last-reply (id)
  "The newest assistant text of session ID, or NIL."
  (let* ((payload (nlk:events :session-id id :kind "turn.assistant_message_completed"
                               :order :newest :as :payload))
         (message (and payload (nlk:json-value payload :any "message")))
         (text (and message (nle::content-text (nlk:json-value message :any "content")))))
    (and (stringp text) (plusp (length text)) text)))

(defun last-failure (id)
  "The detail of session ID's newest failed turn, or NIL."
  (let ((event (nlk:events :session-id id :kind "turn.failed" :order :newest :limit 1 :as :instances)))
    (and event (nlk:turn-failed-detail event))))

(defun current-turn-id ()
  "The turn the calling session is in, from the store, or NIL."
  (let ((id nlk:*scribe-session-id*))
    (when (and (stringp id) (nlk:store-open-p)
               (ignore-errors (nlk:session-exists-p id)))
      (let ((payload (nlk:events :session-id id :kind "turn.started" :order :newest :as :payload)))
        (and payload (nlk:json-value payload :string "turn-id"))))))

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
                                                       "the delegate_to call's seconds ran out"))
               (return :timeout))
              (t (sleep 0.5)))))))

(defun task-text (goal context)
  "The words a subagent gets: the goal whole, then the context it cannot see."
  (if (and context (plusp (length context)))
      (format nil "~a~%~%Context:~%~a" goal context)
      goal))

(defun run-task (route goal context seconds)
  "Run GOAL in a fresh subagent session pinned to ROUTE's provider and model, and
answer an OUTCOME: its last words, or the failure that ended it."
  (unless (nlk:store-open-p) (fail "no store is open; a subagent is a durable session"))
  (let* ((origin nlk:*scribe-session-id*)
         (id (nlk:make-durable-id "delegate"))
         (cwd (or (and origin (ignore-errors (nlk:find-session-cwd origin))) (uiop:getcwd)))
         (parent (and origin (nlk:session-exists-p origin) origin)))
    (nlk:create-session :id id :cwd (namestring cwd) :parent parent :detached t)
    ;; After the create, so it overrides the parent's pin: newest wins.
    (nlk:record-session-model-selection id
                                        :provider (route-provider route)
                                        :model (route-model route))
    (nle:submit id (task-text goal context) :source "orquehelx" :source-id origin)
    (let ((ending (wait-for-idle id seconds))
          (failure nil))
      (when (eq ending :done) (setf failure (last-failure id)))
      (cond ((eq ending :timeout)
             (make-outcome :timeout :session id
                           :error (format nil "the subagent ran past its ~d seconds and was stopped"
                                          seconds)))
            (failure (make-outcome :failed :session id :error failure :summary (last-reply id)))
            (t (make-outcome :completed :session id :summary (last-reply id)))))))

;;; --- the policy ---------------------------------------------------------------

(defparameter +policies+
  '(("ask" . "Do not retry or switch routes on your own. Tell the user and ask whether to send the task to another route (say which model each one uses) or wait for the reset.")
    ("parent_decides" . "You may resend the task to another route if its model suits the task; tell the user which route you chose and why.")
    ("wait" . "Do nothing else with this task. Tell the user and wait for their next instruction."))
  "What the parent does when a route runs out of quota, and the words it is told.")

(defun policy-line (policy)
  (or (cdr (assoc policy +policies+ :test #'string=)) (cdr (assoc "ask" +policies+))))

(defun approval-needed-p ()
  "Whether this turn already saw a route run out, with a policy that wants the
user's answer before another route is picked."
  (and (not (string= (setting :policy) "parent_decides"))
       (let ((turn (funcall *current-turn*)))
         (and turn (equal turn *exhausted-turn*)))))

;;; --- the tool -----------------------------------------------------------------

(defun delegate-schema ()
  "The `delegate_to' schema: the route names (and the model behind each, as the
original's did), the goal and the context."
  (let ((route (nlk:json-object "type" "string"
                                 "description" (format nil "Subscription the subagent runs on.~@[~%~a~]"
                                                        (let ((lines (route-lines)))
                                                          (and (plusp (length lines)) lines))))))
    (when (routes) (setf (gethash "enum" route) (coerce (route-names) 'vector)))
    (list :description (format nil "Delegate a task to a subagent that runs on another subscription and return ~
                                     its result. ~a If the route is out of quota you are told; it never switches ~
                                     on its own."
                               (if (routes)
                                   (format nil "Available routes: ~{~a~^, ~}." (route-names))
                                   "No routes are configured yet: write them in the orquehelx section of the config."))
          :parameters (nlk:make-json-object
                       "type" "object"
                       "properties" (nlk:make-json-object
                                     "route" route
                                     "goal" (nlk:json-object
                                             "type" "string"
                                             "description" "Complete, self-contained task for the subagent.")
                                     "context" (nlk:json-object
                                                "type" "string"
                                                "description" "Data the subagent needs and cannot see."))
                       "required" (vector "route" "goal")))))

(defun out-of-quota-json (route exhaustion)
  "The answer a route that ran out of quota gets: the reset time when the
provider named one, the other routes, and the policy's instruction."
  (let ((reset (exhaustion-reset-at exhaustion)))
    (nlk:encode-json-object
     (nlk:make-json-object
      "status" "out_of_quota"
      "route" (route-name route)
      "provider" (route-provider route)
      "reset_at" (or reset :null)
      "other_routes" (coerce (remove (route-name route) (route-names) :test #'string=) 'vector)
      "instruction" (format nil "Route ~a is out of quota (~a). ~a"
                             (route-name route)
                             (if reset
                                 (format nil "resets at ~a" reset)
                                 "reset time unknown: the provider does not report it")
                             (policy-line (setting :policy)))))))

(defun comparison-safe-exhaustion (provider start)
  "PROVIDER's recorded exhaustion when it was seen at or after START; otherwise
a fresh record naming no detail, so a route whose failure named quota in its own
words is out of quota all the same."
  (let ((ex (exhaustion-for provider)))
    (if (and ex (>= (exhaustion-seen ex) start))
        ex
        (make-exhaustion provider "" start nil))))

(defun delegate-to (arguments)
  "The `delegate_to' tool: ARGUMENTS the decoded call, a hash table."
  (let ((name (nlk:json-value arguments :string "route"))
        (goal (squeeze (nlk:json-value arguments :string "goal")))
        (context (squeeze (nlk:json-value arguments :string "context"))))
    (unless (routes)
      (fail "no routes configured: write them in the orquehelx section of the config, ~
             as {\"name\", \"provider\", \"model\"} objects"))
    (let ((route (route-for name)))
      (unless route
        (fail "unknown route ~a; the routes are ~{~a~^, ~}" (or name "(none)") (route-names)))
      (when (zerop (length goal))
        (fail "missing 'goal': describe the complete task for the subagent"))
      (when (approval-needed-p)
        (fail "a route ran out of quota in this turn and the policy requires the user to approve the next ~
               step: ask them and wait for their answer before delegating again"))
      (let* ((start (get-universal-time))
             (outcome (funcall *run-task* route goal (and (plusp (length context)) context)
                                (setting :seconds))))
        (cond
          ((eq (outcome-status outcome) :completed)
           (nlk:encode-json-object
            (nlk:make-json-object "status" "completed" "route" (route-name route)
                                   "summary" (or (outcome-summary outcome) "(no answer)")
                                   "session" (or (outcome-session outcome) :null))))
          ((and (eq (outcome-status outcome) :failed)
                (exhaustion-text-p (outcome-error outcome)))
           (record-exhaustion (route-provider route) (outcome-error outcome))
           (unless (string= (setting :policy) "parent_decides")
             (setf *exhausted-turn* (funcall *current-turn*)))
           (out-of-quota-json route (comparison-safe-exhaustion (route-provider route) start)))
          (t
           (nlk:encode-json-object
            (nlk:make-json-object "status" (string-downcase (symbol-name (outcome-status outcome)))
                                   "route" (route-name route)
                                   "error" (or (outcome-error outcome) "the subagent failed")
                                   "summary" (or (outcome-summary outcome) :null)
                                   "session" (or (outcome-session outcome) :null)))))))))

;;; --- /ohx ---------------------------------------------------------------------

(defparameter +usage+ "usage: /ohx routes | /ohx quota"
  "The bare /ohx answer, and its hint.")

(defun routes-report ()
  "The text /ohx routes answers with: each route, its provider and its model."
  (if (routes)
      (format nil "~{~a~%~}"
              (loop for route in (routes)
                    collect (format nil "~a: ~a" (route-name route) (route-summary route))))
      "no routes configured: write them in the orquehelx section of the config"))

(defun quota-line (route)
  "One route's usage: the exhaustion recorded for its provider, or that nothing
was recorded. A percentage is never estimated."
  (let ((ex (exhaustion-for (route-provider route))))
    (cond
      ((exhaustion-recent-p ex)
       (format nil "~a: out of quota since ~a (reset: ~a)"
               (route-name route) (clock-text (exhaustion-seen ex))
               (or (exhaustion-reset-at ex) "unknown")))
      (t
       (format nil "~a: no data (nothing recorded as out of quota for ~a)"
               (route-name route) (route-provider route))))))

(defun quota-report ()
  "The text /ohx quota answers with: one line per route, the policy after them."
  (if (routes)
      (format nil "~{~a~%~}policy: ~a"
              (mapcar #'quota-line (routes)) (setting :policy))
      "no routes configured: write them in the orquehelx section of the config"))

(defun run-slash (args session-id)
  "Typing /ohx alone: how the command is used. /ohx routes and /ohx quota
answer their own lines."
  (declare (ignore session-id))
  (let ((wanted (squeeze args)))
    (cond ((string-equal wanted "routes") (routes-report))
          ((string-equal wanted "quota") (quota-report))
          ((zerop (length wanted)) +usage+)
          (t (format nil "~a is not a /ohx word: ~a" wanted +usage+)))))

;;; --- the cell -----------------------------------------------------------------

(nle:define-cell orquehelx
  (:section ("orquehelx")
    (:guide "One route per subscription you delegate to: routes is an array of {name, provider, model} objects; policy is what the parent does when one runs out of quota (ask, parent_decides or wait)")
    ("policy" :choice :options '("ask" "parent_decides" "wait") :default "ask"
              :doc "ask: tell the user and wait for their answer; parent_decides: the parent may pick another route; wait: nothing else")
    ("seconds" :integer :default 900 :min 30
               :doc "how long one delegated subagent's turn may run before it is stopped"))
  (:settings (lambda (values table)
               (multiple-value-bind (routes problems)
                   (read-routes (ignore-errors (nlk:json-value table :any "routes")))
                 (append values (list :routes routes :route-problems problems)))))
  (:help :orquehelx
         "orquehelx: routes a subagent to the subscription you pick, and says plainly when one is out of quota"
         +primer+)
  (:tool "delegate_to" (delegate-schema) #'delegate-to)
  (:command "ohx" 'run-slash :catches 'orquehelx-error
            :description "The routes across subscriptions, and what ran out of quota"
            :argument-hint "routes|quota")
  (:start (lambda ()
            (when (setting :route-problems)
              (nle:notice (format nil "orquehelx: ~{~a~^; ~}" (setting :route-problems))
                          :level :warning :key "nodecode-orquehelx")
              (nle:on-stop (lambda () (nle:notice nil :key "nodecode-orquehelx")))))))
