;;;; support.lisp --- a scripted MSP child, and the tool as a provider calls it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here starts `muse': the transport is a script that answers the
;;;; handshake, streams an agent message, asks one approval and completes the
;;;; turn -- so what the cell sent, and what it did with what came back, are
;;;; facts a test reads instead of a provider's mood.

(in-package #:nodecode.test)

(defstruct (muse-script (:copier nil))
  "A stand-in for the MSP child: what it was sent, and what it will answer."
  (queue '())
  (sent '())
  (session "muse-session-1")
  (texts '("the answer"))
  (terminal "completed")
  (quota nil)
  (stall nil))

(defun muse-script-post (script frame)
  (setf (muse-script-queue script)
        (nconc (muse-script-queue script) (list frame)))
  script)

(defun muse-script-reply (id &optional result)
  (nlk:json-object "jsonrpc" "2.0" "id" id
                    "result" (or result (nlk:json-object))))

(defun muse-script-event (script method params)
  (muse-script-post script (nlk:json-object "jsonrpc" "2.0" "method" method
                                             "params" params)))

(defun muse-script-completion (script)
  (if (muse-script-quota script)
      (nlk:json-object "terminal" "failed"
                       "error" (nlk:json-object "quota" (nlk:json-object "limit" 1)))
      (nlk:json-object "terminal" (muse-script-terminal script))))

(defun muse-script-approval (script)
  (nlk:json-object
   "jsonrpc" "2.0" "id" "msp-1" "method" "approval/request"
   "params" (nlk:json-object
             "approvalId" "a1"
             "sessionId" (muse-script-session script)
             "currentRequirementId" "r1"
             "availableChoices"
             (vector (nlk:json-object "decision" "denied" "choiceId" "c-deny")
                     (nlk:json-object "decision" "approved" "choiceId" "c-allow")))))

(defun muse-script-send (script frame)
  "What the scripted child does with a frame: the handshake answers, and the
turn's stream."
  (push frame (muse-script-sent script))
  (unless (muse-script-stall script)
    (let ((method (nlk:json-value frame :string "method"))
          (id (nlk:json-value frame :any "id")))
      (cond
        ((equal method "initialize")
         (muse-script-post script (muse-script-reply id)))
        ((equal method "session/start")
         (muse-script-post
          script
          (muse-script-reply
           id
           (nlk:json-object "session"
                            (nlk:json-object "sessionId"
                                             (muse-script-session script))))))
        ((equal method "turn/start")
         (muse-script-post script (muse-script-reply id))
         (muse-script-event script "item/started"
                            (nlk:json-object
                             "item" (nlk:json-object "kind" "agentMessage"
                                                      "itemId" "m1")))
         (dolist (text (muse-script-texts script))
           (muse-script-event script "item/delta"
                              (nlk:json-object "itemId" "m1" "field" "text"
                                               "delta" text)))
         (muse-script-post script (muse-script-approval script))
         (muse-script-event script "turn/completed"
                            (muse-script-completion script))))))
  t)

(defun muse-script-receive (script timeout)
  (declare (ignore timeout))
  (cond ((muse-script-queue script) (pop (muse-script-queue script)))
        ((muse-script-stall script) :timeout)
        (t :closed)))

(defun muse-script-transport (script)
  "SCRIPT as a transport the cell can speak to: a child that is a function."
  (nodecode-muse-harness::make-transport
   :label "muse script"
   :script (list :send (lambda (frame) (muse-script-send script frame))
                 :receive (lambda (timeout) (muse-script-receive script timeout))
                 :close (lambda () t))))

(defmacro with-muse-script ((script) &body body)
  "Run BODY with the cell's transport scripted by SCRIPT."
  `(let ((nodecode-muse-harness:*transport-maker*
           (lambda (plan)
             (declare (ignore plan))
             (muse-script-transport ,script))))
     ,@body))

(defun muse-sent (script method)
  "The frame SCRIPT was sent with METHOD, or NIL."
  (find method (muse-script-sent script)
        :key (lambda (frame) (nlk:json-value frame :string "method"))
        :test #'equal))

(defun muse-decision (script)
  "The choiceId the cell answered Muse's approval with, or NIL."
  (let ((frame (muse-sent script "approval/decide")))
    (and frame (nlk:json-value frame :string "params" "choiceId"))))

(defun muse-tool (&rest pairs)
  "Call the registered muse_turn tool the way a provider's call arrives:
PAIRS alternating wire keys and values, decoded through the contract."
  (let ((entry (nle::find-tool "muse_turn")))
    (unless entry
      (error "the muse_turn tool is not registered"))
    (funcall (third entry)
             (nodecode-muse-harness::decode-muse-turn
              (apply #'nlk:make-json-object pairs)))))
