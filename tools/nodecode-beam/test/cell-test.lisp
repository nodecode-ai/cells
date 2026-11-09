;;;; cell-test.lisp --- the beam cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Beam API call is a stubbed NLK:HTTP, every beam CLI run a stubbed
;;;; *RUN*, and the environment a stubbed *GETENV*: nothing reaches Beam, the
;;;; network, the CLI or this machine's environment.

(in-package #:nodecode.test)

(define-test-slice "beam" "BEAM-CELL-" :start nodecode-beam:start-cell)

(define-cell-lifecycle-tests "beam"
  (:help :beam)
  (:command "beam")
  (:refused ("timeout_seconds" 0)))

;;; --- fixtures ----------------------------------------------------------------------

(defun beam-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun beam-refusal (thunk)
  "THUNK's answer, or the refusal it signals as its text."
  (handler-case (funcall thunk)
    (nodecode-beam:beam-error (condition) (princ-to-string condition))))

(defmacro with-beam-key ((&optional (key "b1m_test")) &body body)
  "BODY with the environment holding KEY under BEAM_API_KEY, and nothing else."
  `(let ((nodecode-beam::*getenv* (lambda (name)
                                     (and (equal name "BEAM_API_KEY") ,key))))
     ,@body))

(defmacro with-beam-wire ((answer &key (status 200)) &body body)
  "BODY with NLK:HTTP answering ANSWER at STATUS, and ASKED, a symbol macro,
holding (METHOD URL OPTIONS) oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url options) ,log)
                                   (values ,answer ,status nil))
         (symbol-macrolet ((asked (reverse ,log)))
           ,@body)))))

;;; --- the account ------------------------------------------------------------------

(deftest beam-cell-account-checks-the-key ()
  (with-cell-stop ((beam-start))
    (with-beam-key ()
      (with-beam-wire ("{\"status\":\"active\",\"user_type\":\"organization\",\"current_key_role\":\"owner\",\"current_key_prefix\":\"b1m_abc\"}")
        (let ((answer (beam-tool "beam_account" (nlk:json-object))))
          (is (search "active" answer))
          (is (search "owner" answer))
          (is (search "b1m_abc" answer) "the key's prefix, never the key")
          (destructuring-bind (method url options) (first asked)
            (is (eq :get method))
            (is (equal "https://beamcore.b1m.ai/auth/me" url))
            (is (equal "b1m_test" (cdr (assoc "X-Api-Key" (getf options :headers)
                                               :test #'string-equal))))))))))

(deftest beam-cell-refuses-without-a-key ()
  (with-cell-stop ((beam-start))
    (with-beam-key (nil)
      (is (search "BEAM_API_KEY is not set"
                  (beam-refusal (lambda () (beam-tool "beam_account" (nlk:json-object)))))))))

(deftest beam-cell-says-a-refused-key ()
  (with-cell-stop ((beam-start))
    (with-beam-key ()
      (with-beam-wire ("{}" :status 403)
        (is (search "Beam refused the API key (HTTP 403)"
                    (beam-refusal (lambda () (beam-tool "beam_account" (nlk:json-object))))))))))

;;; --- transfers -----------------------------------------------------------------------

(deftest beam-cell-lists-the-transfers ()
  (with-cell-stop ((beam-start))
    (with-beam-key ()
      (with-beam-wire ("{\"transfers\":[{\"id\":\"t-1\",\"status\":\"completed\",\"total_bytes\":1000000000,\"bytes_transferred\":1000000000,\"created_at\":\"2026-01-02T03:04:05Z\"},{\"id\":\"t-2\",\"status\":\"failed\",\"error_message\":\"source_access_denied\"}]}")
        (let ((answer (beam-tool "beam_transfers" (nlk:json-object "action" "list" "limit" 5))))
          (is (search "2 transfers" answer))
          (is (search "t-1" answer))
          (is (search "source_access_denied" answer))
          (let ((url (second (first asked))))
            (is (search "/transfers?" url))
            (is (search "limit=5" url))))))))

(deftest beam-cell-cancels-only-when-confirmed ()
  (with-cell-stop ((beam-start))
    (with-beam-key ()
      (with-beam-wire ("{\"message\":\"cancelling transfer t-1\"}")
        (is (search "confirmed"
                    (beam-refusal (lambda ()
                                    (beam-tool "beam_transfers"
                                               (nlk:json-object "action" "cancel" "id" "t-1"))))))
        (is (null (first asked)) "nothing was spent before the operator said so")
        (let ((answer (beam-tool "beam_transfers"
                                 (nlk:json-object "action" "cancel" "id" "t-1" "confirmed" t))))
          (is (search "cancelling transfer t-1" answer))
          (destructuring-bind (method url &rest options) (first asked)
            (declare (ignore options))
            (is (eq :post method))
            (is (equal "https://beamcore.b1m.ai/transfers/t-1/cancel" url))))))))

(deftest beam-cell-shows-one-transfer ()
  (with-cell-stop ((beam-start))
    (with-beam-key ()
      (with-beam-wire ("{\"id\":\"t-9\",\"status\":\"in_progress\",\"total_bytes\":2000,\"bytes_transferred\":500}")
        (let ((answer (beam-tool "beam_transfers" (nlk:json-object "action" "show" "id" "t-9"))))
          (is (search "t-9" answer))
          (is (search "25.0%" answer) "how far it has gone")
          (is (equal "https://beamcore.b1m.ai/transfers/t-9" (second (first asked)))))))))

;;; --- Rooms through the CLI -----------------------------------------------------------

(deftest beam-cell-rooms-asks-the-cli-for-one-room ()
  (with-cell-stop ((beam-start))
    (let ((ran nil))
      (let ((nodecode-beam::*run*
              (lambda (arguments input)
                (setf ran (list arguments input))
                (values "{\"room\":{\"id\":\"r1\",\"state\":\"active\"}}" "" 0))))
        (let ((answer (beam-tool "beam_rooms" (nlk:json-object "action" "show" "room" "r1"))))
          (is (equal '("--json" "--no-interactive" "room" "r1" "show") (first ran)))
          (is (search "r1" answer))
          (is (search "active" answer)))))))

(deftest beam-cell-rooms-publishes-the-message-on-stdin ()
  (with-cell-stop ((beam-start))
    (let ((ran nil))
      (let ((nodecode-beam::*run*
              (lambda (arguments input)
                (setf ran (list arguments input))
                (values "{}" "" 0))))
        (beam-tool "beam_rooms" (nlk:json-object "action" "publish" "room" "r1"
                                                  "channel" "c1" "message" "hello"))
        (is (equal '("--json" "--no-interactive" "room" "r1" "channel" "c1" "publish" "--stdin")
                   (first ran)))
        (is (equal "hello" (second ran)) "the message rides on standard input")))))

(deftest beam-cell-rooms-says-what-the-cli-said ()
  (with-cell-stop ((beam-start))
    (let ((nodecode-beam::*run* (lambda (arguments input)
                                  (declare (ignore arguments input))
                                  (values "" "beam: not enrolled on this machine" 6))))
      (let ((answer (beam-tool "beam_rooms" (nlk:json-object "action" "agent_status"))))
        (is (search "failed (exit 6)" answer))
        (is (search "not enrolled" answer))))))

(deftest beam-cell-rooms-creates-only-when-confirmed ()
  (with-cell-stop ((beam-start))
    (let ((ran nil))
      (let ((nodecode-beam::*run* (lambda (arguments input)
                                    (declare (ignore input))
                                    (setf ran arguments)
                                    (values "{\"room\":{\"id\":\"r2\"}}" "" 0))))
        (is (search "1 credit"
                    (beam-refusal (lambda () (beam-tool "beam_rooms" (nlk:json-object "action" "create"))))))
        (is (null ran))
        (is (search "r2" (beam-tool "beam_rooms" (nlk:json-object "action" "create" "confirmed" t))))
        (is (equal "create" (fourth ran)) "room create")))))

;;; --- /beam ----------------------------------------------------------------------------

(deftest beam-cell-command-checks-the-key ()
  (with-cell-stop ((beam-start))
    (with-beam-key ()
      (with-beam-wire ("{\"status\":\"active\",\"current_key_prefix\":\"b1m_abc\"}")
        (is (search "b1m_abc" (nodecode-beam::run-command "" "s1")))
        (is (search "/auth/me" (second (first asked))))))
    (with-beam-key ()
      (with-beam-wire ("{\"transfers\":[]}")
        (is (search "No transfers" (nodecode-beam::run-command "transfers" "s1")))))))
