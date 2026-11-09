;;;; cell-test.lisp --- lifecycle, the tool, /mpp, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, an integer under its floor refused, a start with no payer
;;;; refused), the mpp_fetch tool through the seam the model calls it by --
;;;; a free answer passing through, a 402 challenge paid once and retried
;;;; with the payer's Authorization, an origin the allowlist refuses and an
;;;; amount over the cap left unpaid -- and /mpp. Every request is a stubbed
;;;; NLK:HTTP and every payer a stubbed UIOP:RUN-PROGRAM, so nothing reaches
;;;; the network and no wallet is touched.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: (METHOD URL ARGS) => (values TEXT STATUS HEADERS).")

(defvar *payer-fn* nil
  "What the stubbed UIOP:RUN-PROGRAM answers: (COMMAND INPUT) => (values OUTPUT ERRORS CODE).")

(defmacro with-mpp-wire (&body body)
  "BODY with NLK:HTTP and UIOP:RUN-PROGRAM stubbed, so no test dials out."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                         (declare (ignore method url args))
                         (error "unexpected request")))
         (*payer-fn* (lambda (command input)
                       (declare (ignore command input))
                       (error "unexpected payer"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       (with-stubbed-fdefinition (uiop:run-program (command &rest args)
                                  (funcall *payer-fn* command (getf args :input)))
         ,@body))))

(defun mpp-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun mpp-header-of (request name)
  "REQUEST's header NAME among the :HEADERS it carried."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun mpp-answer (text status)
  "Every request answers TEXT at STATUS, with no headers."
  (setf *answer-fn* (lambda (method url args)
                       (declare (ignore method url args))
                       (values text status '()))))

(defun mpp-b64 (text)
  "TEXT base64url-encoded, without padding."
  (let* ((octets (sb-ext:string-to-octets text :external-format :utf-8))
         (bits 0) (count 0) (out '()))
    (flet ((emit (value)
             (push (char "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
                         value)
                   out)))
      (loop for octet across octets
            do (setf bits (logior (ash bits 8) octet)
                     count (+ count 8))
               (loop while (>= count 6)
                     do (setf count (- count 6))
                        (emit (ldb (byte 6 count) bits))
                        (setf bits (ldb (byte count 0) bits))))
      (when (plusp count)
        (emit (ldb (byte 6 0) (ash bits (- 6 count))))))
    (coerce (nreverse out) 'string)))

(defun mpp-challenge-header (&key (amount "100") (chain 42))
  "A WWW-Authenticate value carrying one tempo charge challenge."
  (format nil "Payment id=\"c-1\", realm=\"test\", method=\"tempo\", intent=\"charge\", request=\"~a\""
          (mpp-b64 (format nil "{\"amount\":\"~a\",\"currency\":\"0x20c0\",\"recipient\":\"0xabc\",\"methodDetails\":{\"chainId\":~a}}"
                            amount chain))))

(defun mpp-paid-answer (text status &key challenge)
  "Every request answers TEXT at STATUS, carrying CHALLENGE in WWW-Authenticate."
  (setf *answer-fn* (lambda (method url args)
                       (declare (ignore method url args))
                       (values text status
                               (if t
                                   (list (cons "WWW-Authenticate" (mpp-challenge-header)))
                                   '())))))

(defun mpp-tool (arguments)
  "mpp_fetch called through the wire with the JSON object ARGUMENTS => the result text."
  (assert (nle::find-tool "mpp_fetch") () "mpp_fetch is not registered")
  (nle::message-content (execute-wire-call "call-1" "mpp_fetch" (nlk:encode-json-object arguments))))

(defun mpp-json (text)
  "TEXT decoded as JSON, for the assertions that read an answer's fields."
  (nlk:decode-json text))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "mpp" "MPP-CELL-")

(defun mpp-config (&rest pairs)
  "A whole shared config with one `mpp' section built from PAIRS."
  (nlk:json-object "mpp" (apply #'nlk:make-json-object pairs)))

(defun mpp-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-mpp:start-cell (apply #'mpp-config pairs)))

(define-cell-lifecycle-tests "mpp"
  (:fixture with-mpp-wire)
  (:config "payer_command" "pay.sh")
  (:help :mpp)
  (:command "mpp")
  (:running (is (nle::find-tool "mpp_fetch") "mpp_fetch is registered")
            (is (equal "pay.sh" (nodecode-mpp::setting :payer-command)) "the payer the section names")
            (is (= 0 (nodecode-mpp::setting :max-amount)) "no cap unless the section sets one")
            (is (= 50000 (nodecode-mpp::setting :max-body-chars)) "the default body ceiling"))
  (:stopped (is (null (nle::find-tool "mpp_fetch")) "stop unregisters the tool"))
  (:refused ("max_amount" -1) ("timeout_seconds" 0) ("max_body_chars" 0))
  (:idle nodecode-mpp:mpp-error (nodecode-mpp::run-command "" nil)))

(deftest mpp-cell-a-missing-payer-refuses-the-start ()
  (with-mpp-wire
    (is-carrying (answer (refusal-text nlk:config-refusal (mpp-start)))
                 ("no payer" "the refusal names what to set"))
    (is (null (nle::find-tool "mpp_fetch")) "a refused start installed nothing")))

;;; --- mpp_fetch ---------------------------------------------------------------






(deftest mpp-cell-a-free-answer-passes-through ()
  (with-mpp-wire
    (with-cell-stop ((mpp-start "payer_command" "pay.sh"))
      (mpp-answer "{\"ok\":true}" 200)
      (let* ((text (mpp-tool (nlk:json-object "url" "https://api.test/ping")))
             (body (mpp-json text)))
        (is (= 200 (nlk:json-value body :integer "status")) "the status passes through")
        (is (equal "{\"ok\":true}" (nlk:json-value body :string "body")) "the body too")
        (is (null (nlk:json-value body :boolean "paid")) "nothing was paid")
        (is (= 1 (length (mpp-asked))) "one exchange")
        (is (eq :get (first (first (mpp-asked)))) "GET unless the call says otherwise")))))

(deftest mpp-cell-a-402-is-paid-once-and-retried ()
  (with-mpp-wire
    (with-cell-stop ((mpp-start "payer_command" "pay.sh" "allowed_origins" (vector "https://api.test")))
      (let ((calls 0))
        (setf *answer-fn*
              (lambda (method url args)
                (declare (ignore method url args))
                (if (= (incf calls) 1)
                    (values "{\"error\":\"payment required\"}" 402
                            (list (cons "WWW-Authenticate" (mpp-challenge-header))))
                    (values "{\"paid\":true}" 200 '()))))
        (setf *payer-fn*
              (lambda (command input)
                (declare (ignore command))
                (is (search "\"amount\":\"100\"" input) "the payer reads the challenge's price")
                (values "Payment token-1" "" 0)))
        (let* ((text (mpp-tool (nlk:json-object "url" "https://api.test/paid")))
               (body (mpp-json text)))
          (is (= 200 (nlk:json-value body :integer "status")) "the retry's answer")
          (is (nlk:json-value body :boolean "paid") "the answer says it was paid")
          (is (equal "100" (nlk:json-value (nlk:json-value body :object "payment") :string "amount"))
              "the payment repeats the price")
          (is (= 2 (length (mpp-asked))) "one paid retry, no more")
          (is (equal "Payment token-1" (mpp-header-of (second (mpp-asked)) "authorization"))
              "the retry carries the payer's Authorization"))))))

(deftest mpp-cell-an-origin-outside-the-allowlist-is-not-paid ()
  (with-mpp-wire
    (with-cell-stop ((mpp-start "payer_command" "pay.sh"
                                "allowed_origins" (vector "https://mpp.dev")))
      (mpp-paid-answer "{}" 402)
      (let* ((text (mpp-tool (nlk:json-object "url" "https://api.test/paid")))
             (body (mpp-json text)))
        (is (= 402 (nlk:json-value body :integer "status")) "the 402 comes back as it was")
        (is (search "not in allowed_origins" (nlk:json-value body :string "refusal"))
            "the refusal says why")
        (is (= 1 (length (mpp-asked))) "no retry")
        (is (equal "100" (nlk:json-value (aref (nlk:json-value body :array "challenges") 0)
                                           :string "amount"))
            "the price the model may choose to pay")))))

(deftest mpp-cell-an-amount-over-the-cap-is-not-paid ()
  (with-mpp-wire
    (with-cell-stop ((mpp-start "payer_command" "pay.sh" "max_amount" 50))
      (mpp-paid-answer "{}" 402)
      (let* ((text (mpp-tool (nlk:json-object "url" "https://api.test/paid")))
             (body (mpp-json text)))
        (is (search "over max_amount" (nlk:json-value body :string "refusal"))
            "the cap refuses it")
        (is (= 1 (length (mpp-asked))) "no retry")))))

(deftest mpp-cell-a-private-url-is-refused-before-any-request ()
  (with-mpp-wire
    (with-cell-stop ((mpp-start "payer_command" "pay.sh"))
      (is-carrying (answer (mpp-tool (nlk:json-object "url" "http://127.0.0.1:8080/paid")))
                   ("private or internal"))
      (is (null (mpp-asked)) "nothing was dialled"))))

;;; --- /mpp ---------------------------------------------------------------------

(deftest mpp-cell-the-command-states-the-wallet-or-fetches ()
  (with-mpp-wire
    (with-cell-stop ((mpp-start "payer_command" "pay.sh"
                                "allowed_origins" (vector "https://mpp.dev")
                                "max_amount" 250))
      (let ((answer (cell-entry "nodecode-mpp" "mpp" "")))
        (is (search "payer \"pay.sh\"" answer) "the payer is named")
        (is (search "at most 250 per payment" answer) "the cap is named")
        (is (null (mpp-asked)) "a status read asks nothing"))
      (mpp-answer "{\"ok\":true}" 200)
      (let ((answer (cell-entry "nodecode-mpp" "mpp" "https://api.test/ping")))
        (is (search "HTTP 200" answer) "the status of the fetch")
        (is (= 1 (length (mpp-asked))) "one exchange")))))
