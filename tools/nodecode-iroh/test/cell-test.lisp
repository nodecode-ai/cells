;;;; cell-test.lisp --- the iroh cell against a scripted sidecar.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here runs a program: the cell's one process seam
;;;; (NODECODE-IROH::*RUN-PROGRAM*) is bound to a stand-in that answers the
;;;; `addr' and `serve' argv the cell builds, and records each one, so "the
;;;; sidecar was asked this" is a fact a test can read. State goes to a temp
;;;; directory of the test's own, never the run's home. The tools are called
;;;; through the real dispatch seam, so the schema the provider is shown and
;;;; the body that runs are the same declaration.

(in-package #:nodecode.test)

(define-test-slice "iroh" "IROH-CELL-" :start nodecode-iroh:start-cell)

(define-cell-lifecycle-tests "iroh"
  (:help :iroh)
  (:command "iroh")
  (:running (is (equal "default" (nodecode-iroh::setting :relay)) "relay defaults to the public ones")
            (is (= 120 (nodecode-iroh::setting :timeout-seconds)) "a call may take two minutes")
            (dolist (name '("iroh_peer_status" "iroh_peer_list" "iroh_peer_make_ticket"
                            "iroh_peer_pair" "iroh_peer_call"))
              (is (nle::find-tool name) (format nil "~a is registered" name))))
  (:refused ("timeout_seconds" 1))
  (:idle iroh:iroh-error (iroh:peers)))

;;; --- the scripted sidecar -----------------------------------------------------

(defvar *asked* '()
  "Every sidecar invocation the stub took, newest first: (ARGV INPUT).")

(defparameter +peer+ "peerlocal1"
  "The endpoint id the stub's `addr' answers: this agent's own.")

(defun stub-answer (&key (status "completed") (text "done") (error ""))
  "The JSON-RPC reply `serve' makes to one dial."
  (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"status\":~s,\"text\":~s,\"error\":~s}}~%"
          status text error))

(defun sidecar-stub (&key (direct #("127.0.0.1:5000")) (reply "done"))
  "A stand-in for NLK:RUN-BOUNDED: `addr' answers this agent's endpoint,
`serve' one JSON-RPC result. Each invocation lands in *ASKED*."
  (lambda (argv &key seconds input)
    (declare (ignore seconds))
    (push (list argv input) *asked*)
    (values (if (member "addr" argv :test #'equal)
                (nlk:encode-json-object
                 (nlk:json-object "endpointId" +peer+ "addrs" (coerce direct 'vector)))
                (stub-answer :text reply))
            "" 0)))

(defun scratch-state ()
  "A state directory of this test's own."
  (ensure-directories-exist
   (merge-pathnames (format nil "iroh-~a/" (random 1000000)) (uiop:temporary-directory))))

(defmacro with-iroh-cell ((&rest config) &body body)
  "BODY with the cell started on CONFIG pairs (a sidecar named unless the
call says otherwise), its state in a temp directory and the sidecar stubbed."
  `(with-cell-stop ((iroh-start ,@(or config (list "sidecar" "/nonexistent/hermes-iroh-sidecar"))))
     (let ((nodecode-iroh::*state* (scratch-state))
           (nodecode-iroh::*run-program* (sidecar-stub))
           (*asked* '()))
       ,@body)))

(defun refusal (condition-type thunk)
  "THUNK's refusal as text when it signals CONDITION-TYPE, else NIL."
  (handler-case (progn (funcall thunk) nil)
    (condition (condition)
      (if (typep condition condition-type)
          (princ-to-string condition)
          (error condition)))))

(defun ticket-line (text)
  "The ticket in TEXT: its first line."
  (first (nlk:lines text)))

(defvar *calls* 0
  "A counter for the wire call ids.")

(defun tool-text (name &rest arguments)
  "The text tool NAME answers, called the way the provider's call is."
  (princ-to-string
   (nle::message-content
    (execute-wire-call (format nil "iroh-call-~d" (incf *calls*)) name
                       (nlk:encode-json-object
                        (if arguments
                            (apply #'nlk:make-json-object arguments)
                            (nlk:json-object)))))))

;;; --- tickets and pairing ------------------------------------------------------

(deftest iroh-cell-a-ticket-pairs-a-peer-exactly-once ()
  (with-iroh-cell ()
    (let ((text (iroh:ticket)))
      (is (uiop:string-prefix-p (format nil "hermes-iroh://pair?peer=~a&" +peer+) text)
          "the ticket names this agent's endpoint")
      (is (search "nonce=" text) "with a one-use nonce")
      (is (search "addr=127.0.0.1" text) "and the direct address a peer can dial")
      (is (search "expires in 15 minutes" text) "good for the ticket lifetime")
      (let ((ticket (ticket-line text)))
        (is (search "confirm" (refusal 'iroh:iroh-error (lambda () (iroh:pair ticket))))
            "pairing refuses until the operator confirms, and says so")
        (is (search +peer+ (refusal 'iroh:iroh-error (lambda () (iroh:pair ticket))))
            "the refusal names the peer the ticket would trust")
        (is (search (format nil "paired with ~a" +peer+) (iroh:pair ticket :confirm t))
            "confirming pairs it")
        (is (search +peer+ (iroh:peers)) "the peer list knows it")
        (is (search "nonce" (refusal 'iroh:iroh-error (lambda () (iroh:pair ticket :confirm t))))
            "the same ticket cannot pair twice")
        (is (search +peer+ (tool-text "iroh_peer_list"))
            "iroh_peer_list answers from the same registry")))))

(deftest iroh-cell-an-expired-or-malformed-ticket-is-refused ()
  (with-iroh-cell ()
    (let ((old (format nil "hermes-iroh://pair?peer=peerold1&secret=~a&ts=~a&nonce=~a"
                       (make-string 24 :initial-element #\b)
                       (- (nlk:unix-now) 3600)
                       (make-string 32 :initial-element #\0))))
      (is (search "expired" (refusal 'iroh:iroh-error (lambda () (iroh:pair old :confirm t))))
          "an hour-old ticket is expired")
      (is (search "no pairing ticket"
                  (refusal 'iroh:iroh-error (lambda () (iroh:pair "hello" :confirm t))))
          "so is a string that is no ticket")
      (is (search "secret"
                  (refusal 'iroh:iroh-error
                           (lambda () (iroh:pair (format nil "hermes-iroh://pair?peer=peerx1&ts=1&nonce=~a"
                                                         (make-string 32 :initial-element #\0))
                                                 :confirm t))))
          "a ticket with no secret is refused")
      (is (null (nodecode-iroh::find-peer "peerold1")) "and an expired one paired nothing"))))

;;; --- a call -------------------------------------------------------------------

(deftest iroh-cell-a-call-goes-out-redacted-and-answers ()
  (with-iroh-cell ()
    (let* ((ticket (ticket-line (iroh:ticket)))
           (sent '()))
      (iroh:pair ticket :confirm t)
      (setf nodecode-iroh::*run-program*
            (lambda (argv &key seconds input)
              (declare (ignore seconds))
              (push (list argv input) sent)
              (values (stub-answer :text "the build is green") "" 0)))
      (let ((answer (tool-text "iroh_peer_call"
                               "peer" +peer+
                               "message" "run the build, token=abcdefghijklmnop")))
        (is (search "completed" answer) "the peer's status comes back")
        (is (search "the build is green" answer) "and its words")
        (is (= 1 (length sent)) "one process, one call")
        (destructuring-bind (argv input) (first sent)
          (declare (ignore argv))
          (is (search "[REDACTED]" input) "a credential-shaped token is scrubbed on the way out")
          (is (null (search "abcdefghijklmnop" input)) "and the token itself did not leave")
          (is (search "authSecret" input) "the pairing secret authenticates the task")
          (is (search +peer+ input) "and the peer is the one paired"))
        (is (plusp (length (or (nlk:json-value (nodecode-iroh::find-peer +peer+) :string "last_called") "")))
            "and the peer's record says when it was last called"))
      (is (search "not a paired peer"
                  (refusal 'iroh:iroh-error (lambda () (iroh:call "peernobody9" "hi"))))
          "an unpaired peer is refused before any process starts"))))

;;; --- no sidecar, and the panel --------------------------------------------------

(deftest iroh-cell-without-a-sidecar-it-says-what-to-build ()
  (with-iroh-cell ("sidecar" "")
    (is (search "no sidecar" (tool-text "iroh_peer_status")) "the status names the gap")
    (is (search "sidecar" (refusal 'iroh:iroh-error (lambda () (iroh:ticket))))
        "a ticket needs an endpoint, and says where it comes from")
    (is (search "no peers" (tool-text "iroh_peer_list")) "and no peer is paired")))

(deftest iroh-cell-the-command-shows-the-status-and-its-panel ()
  (with-iroh-cell ()
    (let ((text (nodecode-iroh::run-command "" nil)))
      (is (search "peers: 0" text) "bare /iroh reports the interconnect")
      (is (search "the sidecar answers" text) "including whether the sidecar answers"))
    (let* ((dialog (nth-value 1 (nodecode-iroh::run-command "" nil)))
           (object (nle::dialog-json dialog)))
      (is (equal "No peers paired" (nle::json-value object :text "summary")) "and its panel says so")
      (is (= 2 (length (nle::json-array object "rows"))) "with the two rows that start everything"))
    (is (search "hermes-iroh://pair?peer=" (nodecode-iroh::run-command "ticket" nil))
        "`/iroh ticket` issues this agent's ticket")))
