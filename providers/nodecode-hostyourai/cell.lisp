;;;; cell.lisp --- the cell: HostYourAI among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four hooks, each declining for every provider but hostyourai:
;;;;
;;;;   MODELS-CATALOG-TABLE    the catalog carries HostYourAI's row: the
;;;;                            chat lane's package, this section's base and
;;;;                            the hyai/auto model, so /connect offers it,
;;;;                            /models lists it and a turn resolves its lane
;;;;                            and address the way it does any catalog provider's
;;;;   :CREDENTIAL              a key from HOSTYOURAI_API_KEY, the variable the
;;;;                            plugin reads, and no other variable; a key
;;;;                            /connect saved answers before this point does
;;;;   REQUEST-BODY             a hostyourai round on hyai/auto carries the
;;;;                            section's task and policy as an "hyai" object
;;;;   WALK-PROVIDER-STREAM    the same hint as X-HYAI-Task / X-HYAI-Policy
;;;;                            headers when the section says transport headers
;;;;
;;;; Task `auto' is the plugin's other half: before the round goes out, the
;;;; last user message is sent -- 200 characters of it -- to the service's
;;;; System One model, and the task it answers with (at 0.8 confidence or
;;;; better) is the hint. Anything less is `chat', which is what the plugin
;;;; falls back to as well.
;;;;
;;;; The /hostyourai command shows and sets the same two words, and reads the
;;;; account's balance. Config, a sibling top-level key:
;;;;   "hostyourai": {"base_url": "https://hostyourai.com/api/v1",
;;;;                  "task": "auto", "policy": "speed", "transport": "body"}
;;;; A vetoed section ("enabled": false) installs nothing, and HostYourAI is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-hostyourai)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a hostyourai round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

;;; --- the catalog ------------------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
HostYourAI's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with HostYourAI's row, made once
per catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

;;; --- the sticky hint ----------------------------------------------------------------

(defvar *routing* nil
  "(:task ... :policy ... :transport ...) the running cell steers by: the
section's own, then whatever /hostyourai set, which is also written to the
section so the next start begins from it.")

(defvar *pause-until* 0
  "Universal time before which auto asks the service nothing: after a real
failure the plugin waits a minute, and so does this.")

(defun routing ()
  "The routing plist, or the section's defaults when the cell is not started."
  (or *routing* (list :task +router+ :policy +router+ :transport "body")))

(defun hints (body)
  "The routing hint BODY's round carries, as (:task ... :policy ...) with
`router' left out, or NIL when it carries none."
  (let* ((state (routing))
         (task (getf state :task))
         (policy (getf state :policy))
         (out '()))
    (when (equal task +auto+)
      (setf task (auto-task-for body)))
    (when (and task (not (equal task +router+))) (setf (getf out :task) task))
    (when (and policy (not (equal policy +router+))) (setf (getf out :policy) policy))
    out))

(defun auto-task-for (body)
  "The task auto answers for BODY's last user message: `chat' whenever the
service cannot give a good enough answer, as the plugin does."
  (let ((key (api-key)))
    (if (or (null key) (<= (get-universal-time) *pause-until*))
        "chat"
        (multiple-value-bind (prompt previous) (texts body)
          (if (< (+ (length prompt) (length previous)) +auto-min-chars+)
              "chat"
              (or (handler-case (classify prompt previous key)
                    (error () (setf *pause-until* (+ (get-universal-time) 60)) nil))
                  "chat"))))))

(defun header-list (hints)
  "HINTS as the request headers the headers transport names."
  (loop for (key . name) in '((:task . "X-HYAI-Task") (:policy . "X-HYAI-Policy"))
        for value = (getf hints key)
        when value collect (cons name value)))

;;; --- the hooks -----------------------------------------------------------------------

(defun credential (op next)
  "The :CREDENTIAL answer for hostyourai: the key HOSTYOURAI_API_KEY holds,
else the keyless stand-in. Never NEXT for hostyourai: the ladder behind this
point falls back to the chat family's default variable, and would send
OPENAI_API_KEY to HostYourAI."
  (if (equal (getf op :provider) +provider+)
      (let ((key (nle::credential-env +env+)))
        (if key
            (nle:make-credential key :env)
            (nle:make-credential "public" :public)))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a hostyourai round on hyai/auto carries the hint as
an `hyai' object."
  (when (and (ours-p config) (hash-table-p body)
             (equal (getf (routing) :transport) "body"))
    (let ((hint (hints body)))
      (when hint
        (setf (gethash "hyai" body)
              (nlk:json-object :opt "task" (getf hint :task) :opt "policy" (getf hint :policy))))))
  body)

(defun request-object (request-json)
  "REQUEST-JSON -- the round's body as octets -- as the object it spells, or NIL."
  (when request-json
    (handler-case
        (nlk:decode-json (if (stringp request-json)
                              request-json
                              (sb-ext:octets-to-string request-json :external-format :utf-8)))
      (error () nil))))

(defun walk (next fold &rest keys &key config headers request-json &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a hostyourai round on the headers transport
carries the same hint as X-HYAI-Task / X-HYAI-Policy."
  (if (and (ours-p config) (equal (getf (routing) :transport) "headers"))
      (let ((hint (hints (request-object request-json))))
        (apply next fold
               :headers (append headers (header-list hint))
               (alexandria:remove-from-plist keys :headers)))
      (apply next fold keys)))

;;; --- /hostyourai -----------------------------------------------------------------------

(defparameter +usage+
  (format nil "Usage: /hostyourai [status|task <value>|policy <value>|off|usage|help]~%  task:   ~{~a~^|~}|~a|~a~%  policy: ~{~a~^|~}|~a~%  shorthand: /hostyourai code cost (a task and/or a policy, in any order)"
          +tasks+ +auto+ +router+ +policies+ +router+)
  "What /hostyourai answers with no arguments and with help.")

(defparameter +primer+
  "HostYourAI is a model provider on this home: the model hyai/auto picks a warm model for each
request, and a task and a policy steer which. The operator's /hostyourai command sets them, per
home: /hostyourai task code, /hostyourai policy speed, /hostyourai auto (a small model picks the
task per message), /hostyourai off, and /hostyourai usage for the account's balance. They are the
cell's hostyourai section too (task, policy, transport). Only hyai/auto requests carry a hint, and
the settings cost nothing when the round runs on another provider."
  "What (help :hostyourai) answers while the cell runs.")

(defun words-of (args)
  "ARGS as the lowercased words the command reads."
  (loop for word in (uiop:split-string (or args "") :separator " ")
        for trimmed = (string-trim '(#\Space #\Tab) word)
        when (plusp (length trimmed)) collect (string-downcase trimmed)))

(defun status-text ()
  "The one line /hostyourai status answers, in the plugin's own words."
  (let ((task (getf (routing) :task))
        (policy (getf (routing) :policy)))
    (if (and (equal task +router+) (equal policy +router+))
        "HostYourAI routing: not set (hyai/auto decides per request)"
        (format nil "HostYourAI routing: task=~a policy=~a (every chat on this home; only hyai/auto requests use it)"
                (if (equal task +router+) "(unset)" task)
                (if (equal policy +router+) "(unset)" policy)))))

(defun save-routing ()
  "Keep the choice in the shared config, so the next start begins from it."
  (handler-case
      (nle:config-set '("hostyourai")
                       (list :task (getf (routing) :task)
                             :policy (getf (routing) :policy)))
    (error () nil)))

(defun set-routing (&rest pairs)
  "Write PAIRS into the routing state, save it, and answer the status line."
  (let ((state (copy-list (routing))))
    (loop for (key value) on pairs by #'cddr
          do (setf (getf state key) value))
    (setf *routing* state))
  (save-routing)
  (status-text))

(defun usage-report ()
  "The account's balance and this month's spend, or why there is none."
  (let ((key (api-key)))
    (if (null key)
        "HostYourAI balance unavailable: no HOSTYOURAI_API_KEY set."
        (multiple-value-bind (answer status)
            (nle:http-fetch (balance-url) :headers (auth-headers key) :timeout 5)
          (if (null answer)
              (format nil "HostYourAI balance unavailable: ~a." status)
              (balance-text answer (setting :usage-warn-eur)))))))

(defun task-command (value &aux (value (or value "")))
  (cond ((member value (append +tasks+ (list +auto+ +router+)) :test #'equal)
         (set-routing :task value))
        (t (format nil "Unknown task.~%~a" +usage+))))

(defun policy-command (value &aux (value (or value "")))
  (cond ((member value (append +policies+ (list +router+)) :test #'equal)
         (set-routing :policy value))
        (t (format nil "Unknown policy.~%~a" +usage+))))

(defun shorthand (words)
  "Bare values in any order: at most one task and one policy at once."
  (let ((tasks (remove-if-not (lambda (word) (member word +tasks+ :test #'equal)) words))
        (policies (remove-if-not (lambda (word) (member word +policies+ :test #'equal)) words)))
    (if (and (= (+ (length tasks) (length policies)) (length words))
             (<= (length tasks) 1) (<= (length policies) 1))
        (apply #'set-routing
               (append (when tasks (list :task (first tasks)))
                       (when policies (list :policy (first policies)))))
        +usage+)))

(defun routing-command (words)
  "The /hostyourai grammar above, once the first word is not usage or help."
  (let ((first (first words)))
    (cond ((null first) (format nil "~a~%~%~a" (status-text) +usage+))
          ((equal first "status") (status-text))
          ((member first '("off" "clear") :test #'equal)
           (set-routing :task +router+ :policy +router+)
           (format nil "HostYourAI routing cleared — hyai/auto decides per request.~%~a" (status-text)))
          ((equal first "task") (task-command (second words)))
          ((equal first "policy") (policy-command (second words)))
          (t (shorthand words)))))

(defun run-command (args session-id)
  "One /hostyourai invocation => the answer's text. The balance is the
operator's own account, and this command is typed by the operator."
  (declare (ignore session-id))
  (let* ((words (words-of args))
         (first (first words)))
    (cond ((equal first "usage") (usage-report))
          ((equal first "help") +usage+)
          (t (routing-command words)))))

;;; --- the cell ----------------------------------------------------------------------------

(nle:define-cell hostyourai
  (:section ("hostyourai")
    (:guide "make a key at https://hostyourai.com/app/docs; save it with /connect or set HOSTYOURAI_API_KEY; task and policy steer the hyai/auto router, and task auto has a small model pick one for each message")
    ("base_url" :string :default +base+
     :doc "the API base the chat lane appends /chat/completions to")
    ("task" :choice :options '("router" "auto" "chat" "code" "summarise" "classify" "extract" "rag" "legal" "reason")
     :default "router"
     :doc "the work kind hyai/auto should expect: a task, auto (decided per message), or router (decided alone)")
    ("policy" :choice :options '("router" "balanced" "speed" "cost" "quality")
     :default "router"
     :doc "what hyai/auto optimizes for, or router to let it choose")
    ("transport" :choice :options '("body" "headers") :default "body"
     :doc "where the hint rides: an hyai object in the request body, or X-HYAI-Task and X-HYAI-Policy headers")
    ("usage_warn_eur" :integer :default 5 :min 0
     :doc "say the balance is low below this many euros; 0 never says it"))
  (:start (lambda ()
            (forget-catalog)
            (setf *routing* (list :task (setting :task)
                                   :policy (setting :policy)
                                   :transport (setting :transport))
                  *pause-until* 0)
            (nle:on-stop (lambda ()
                           (forget-catalog)
                           (setf *routing* nil)))))
  (:help :hostyourai "hostyourai: HostYourAI's hyai/auto router, its task and policy hints, and the account's balance" +primer+)
  (:command "hostyourai" 'run-command
            :description "HostYourAI: routing hints and balance"
            :argument-hint "[status|task|policy|usage|off] [value]")
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk))
