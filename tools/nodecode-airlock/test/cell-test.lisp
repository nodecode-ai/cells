;;;; cell-test.lisp --- the cell against a stubbed Airlock: what it asks, what
;;;; it makes of the answer, and what it refuses to trust.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exchange goes through NLE:HTTP, so the whole protocol is one stub
;;;; here: nothing is dialled and no sandbox starts. The receipts are the
;;;; fixtures -- real SHA-256 and Ed25519 over real canonical JSON -- so the
;;;; verification path is exercised end to end, and the tampered variants are
;;;; made by moving one byte of them.

(in-package #:nodecode.test)

(define-test-slice "airlock" "AIRLOCK-CELL-"
  :start nodecode-airlock:start-cell)

;;; --- the gate these tests answer as ---------------------------------------------

(defvar *asked* nil
  "Every exchange the stub answered this test, newest first, as
(METHOD URL HEADERS BODY).")

(defvar *answers* nil
  "Answers a test overrides the stub with: (MATCH TEXT STATUS), first match wins,
MATCH a substring of the URL or a function of it.")

(defmacro with-gate (&body body)
  "BODY with NLE:HTTP stubbed as an Airlock deployment: the arena answers the
allow fixture, health answers ok, anything else {} with 200, unless *ANSWERS*
says otherwise for a URL."
  `(let ((*asked* nil) (*answers* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list method url (getf args :headers) (getf args :content))
                                      *asked*)
                                (let ((rule (find-if (lambda (rule)
                                               (let ((match (first rule)))
                                                 (if (functionp match)
                                                     (funcall match url)
                                                     (search match url))))
                                             *answers*)))
                                  (cond (rule (values (second rule) (third rule)))
                                        ((search "/api/arena" url) (values +allow-step+ 200))
                                        ((search "/healthz" url) (values "{\"ok\":true}" 200))
                                        (t (values "{}" 200)))))
       ,@body)))

(defun answer-with (match text status)
  "Make the stub answer every URL holding MATCH (or matching the function) with
TEXT and STATUS."
  (push (list match text status) *answers*))

(defun asked-for (fragment)
  "The newest exchange whose URL holds FRAGMENT."
  (find-if (lambda (asked) (search fragment (second asked))) *asked*))

(defun asked-body (asked)
  "The JSON body ASKED carried, decoded, or NIL."
  (let ((content (fourth asked)))
    (and content (plusp (length content)) (nlk:decode-json content))))

(defun asked-header (asked name)
  "The value of header NAME in ASKED, or NIL."
  (cdr (assoc name (third asked) :test #'string-equal)))

(defun replace-all (text old new &key last)
  "TEXT with OLD replaced by NEW -- the last occurrence only with LAST, every
one otherwise. The fixtures are moved by one byte this way: nothing is
re-serialized, so a mismatch is about trust and not about JSON."
  (if (null last)
      (with-output-to-string (out)
        (loop with start = 0
              for at = (search old text :start2 start)
              do (write-string (subseq text start (or at (length text))) out)
                 (when (null at) (return))
                 (write-string new out)
                 (setf start (+ at (length old)))))
      (let ((at (search old text :from-end t)))
        (unless at (return-from replace-all text))
        (concatenate 'string (subseq text 0 at) new (subseq text (+ at (length old)))))))

(defun flip-signature-byte (step-text)
  "STEP-TEXT with one hex digit of its receipt signature moved: the signed
bytes are intact, so the hash still covers them, and only the signature
refuses."
  (let* ((at (search "\"sig_ed25519\":\"" step-text))
         (start (+ at (length "\"sig_ed25519\":\"")))
         (other (if (char= (char step-text start) #\0) #\1 #\0)))
    (concatenate 'string (subseq step-text 0 start) (string other)
                 (subseq step-text (1+ start)))))

(defun airlock-call (&rest pairs)
  "The airlock_run tool as the engine calls it: the declared contract decoded,
then the body."
  (let ((entry (nle::find-tool "airlock_run")))
    (assert entry () "airlock_run is not registered")
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defmacro with-airlock ((&rest pairs) &body body)
  "BODY with the cell started on a healthy section -- the advertiseable key,
unless PAIRS say otherwise -- and the gate stubbed."
  `(with-gate
     (with-cell-stop ((airlock-start "base_url" "https://airlock.example.test"
                                     "public_key_hex" +allow-public-key+
                                     ,@pairs))
       ,@body)))

;;; --- lifecycle, one assertion per thing NLE:DEFINE-CELL owns ------------------------

(define-cell-lifecycle-tests "airlock"
  (:config "base_url" "https://airlock.example.test" "public_key_hex" +allow-public-key+)
  (:help :airlock)
  (:command "airlock")
  (:running (is (equal "bash" (nodecode-airlock::setting :language)) "language defaults to bash")
            (is (eq t (nodecode-airlock::setting :verify-receipts)) "receipts are verified by default")
            (is (equal 60 (nodecode-airlock::setting :timeout-s)) "the timeout defaults to a minute"))
  (:refused ("base_url" "http://airlock.example.test" "public_key_hex" +allow-public-key+)
            ("base_url" "https://airlock.example.test/arena" "public_key_hex" +allow-public-key+)
            ("base_url" "https://airlock.example.test" "public_key_hex" "0123456789abcdef")
            ("base_url" "https://airlock.example.test"))
  (:idle nodecode-airlock:airlock-error
         (nodecode-airlock:run "true")))

;;; --- the canonical form the signature covers ----------------------------------------

(deftest airlock-cell-canonical-json-is-rfc-8785 ()
  (let ((control (string (code-char 1))))
    (is (equal "{\"C\":2,\"a\":1,\"b\":[true,false,null,\"x\\u0001\"]}"
               (with-output-to-string (out)
                 (nodecode-airlock::jcs-write
                  (nlk:make-json-object "C" 2
                                         "b" (vector t nil :null (concatenate 'string "x" control))
                                         "a" 1)
                  out))) "members sort by code unit, strings escape minimally, T/NIL/:NULL read true/false/null"))
  (is (equal "1" (nodecode-airlock::es-float 1.0d0)) "an integral float has no trailing .0")
  (is (equal "100" (nodecode-airlock::es-float 100.0d0)) "fixed form inside the fixed window")
  (is (equal "1e-7" (nodecode-airlock::es-float 1.0d-7)) "and exponential below it")
  (is (equal "0.000001" (nodecode-airlock::es-float 1.0d-6)) "the last fixed exponent"))

(deftest airlock-cell-the-fixture-receipt-checks-out ()
  (let* ((step (nlk:decode-json +allow-step+))
         (receipt (gethash "receipt" step)))
    (is (hash-table-p (nodecode-airlock::verify-receipt receipt +allow-public-key+)) "hash and signature verify over the canonical bytes")
    (is (equal (gethash "hash" receipt)
               (nodecode-airlock::octets->hex
                (ironclad:digest-sequence
                 :sha256 (nodecode-airlock::signed-payload receipt)))) "the canonical bytes this cell builds are the bytes the fixture signed")))

;;; --- one command through the gate --------------------------------------------------

(deftest airlock-cell-the-tool-verifies-one-command ()
  (with-airlock ()
    (let ((answer (airlock-call "command" "echo hello")))
      (is (search "[VERIFIED] airlock" answer) "the answer opens with the verdict it earned")
      (is (search "hello" answer) "the command's output is the answer")
      (is (search "exit 0" answer) "and the exit status with it")
      (is (search "receipt_verified=true" answer) "the receipt verified under the pinned key")
      (is (search "rules=allow-echo" answer) "the rule that fired is shown"))
    (let* ((arena (asked-for "/api/arena"))
           (body (asked-body arena)))
      (is arena "one arena call was made")
      (is (equal "POST" (first arena)))
      (is (search "airlock.example.test" (second arena)))
      (is (equal "application/json" (asked-header arena "Accept")))
      (is (null (asked-header arena "Authorization")) "no token is sent when the section names none")
      (is (equal "echo hello" (nlk:json-value body :string "code")))
      (is (equal "bash" (nlk:json-value body :string "lang")) "the section's arena language")
      (is (not (nth-value 1 (gethash "input_text" body))) "input_text is omitted when empty"))))

(deftest airlock-cell-the-token-rides-every-call ()
  (let ((saved (uiop:getenv "AIRLOCK_TEST_TOKEN")))
    (unwind-protect
         (with-gate
           (setf (uiop:getenv "AIRLOCK_TEST_TOKEN") "airlock-token")
           (with-cell-stop ((airlock-start "base_url" "https://airlock.example.test"
                                          "public_key_hex" +allow-public-key+
                                          "token_env" "AIRLOCK_TEST_TOKEN"))
             (airlock-call "command" "echo hello")
             (is (equal "Bearer airlock-token"
                        (asked-header (asked-for "/api/arena") "Authorization")))))
      (if saved
          (setf (uiop:getenv "AIRLOCK_TEST_TOKEN") saved)
          (sb-posix:unsetenv "AIRLOCK_TEST_TOKEN")))))

(deftest airlock-cell-a-deny-is-blocked-and-shows-no-output ()
  (with-airlock ()
    (answer-with "/api/arena" +deny-step+ 200)
    (let ((answer (airlock-call "command" "curl https://example.test")))
      (is (search "[BLOCKED] airlock" answer) "a deny is BLOCKED even with a good signature")
      (is (search "verdict=deny" answer))
      (is (search "exit 126" answer) "the blocked exit status rides the answer")
      (is (search "rules=block-curl" answer))
      (is (not (search "hello" answer)) "a denied command has no output to show"))))

(deftest airlock-cell-what-is-not-bound-to-the-signature-is-refused ()
  (with-airlock ()
    ;; The receipt's own reason moved: the hash no longer covers these bytes.
    (answer-with "/api/arena" (replace-all +allow-step+
                                           "matched rule allow-echo"
                                           "matched rule allow-echoo"
                                           :last t)
                 200)
    (let ((answer (airlock-call "command" "echo hello")))
      (is (search "[BLOCKED] airlock" answer) "a receipt that does not hash to its bytes is not trusted")
      (is (search "INVALID RECEIPT" answer))
      (is (search "exit 74" answer)))
    (setf *answers* nil)
    ;; The signature moved: the canonical bytes are intact, the key is not the one that signed them.
    (answer-with "/api/arena" (flip-signature-byte +allow-step+) 200)
    (let ((answer (airlock-call "command" "echo hello")))
      (is (search "INVALID RECEIPT" answer) "an Ed25519 signature that does not verify refuses")
      (is (search "does not verify under the public key" answer)))
    (setf *answers* nil)
    ;; The outer stdout moved: the step is unsigned, so its output must match the signed digest.
    (answer-with "/api/arena" (replace-all +allow-step+ "hello\\n" "hello there\\n") 200)
    (let ((answer (airlock-call "command" "echo hello")))
      (is (search "BINDING FAILED" answer) "an outer stdout that is not the signed one refuses")
      (is (search "stdout_sha256" answer))
      (is (search "exit 74" answer)))))

(deftest airlock-cell-without-a-pinned-key-the-answer-is-only-allowed ()
  (with-gate
    (with-cell-stop ((airlock-start "base_url" "https://airlock.example.test"
                                    "verify_receipts" nil))
      (let ((answer (airlock-call "command" "echo hello")))
        (is (search "[ALLOWED] airlock" answer) "it ran, but nothing signed it as far as this cell knows")
        (is (search "receipt_verified=false" answer))
        (is (search "hello" answer) "the output is still shown")))))

(deftest airlock-cell-the-command-answers-health-and-the-last-receipt ()
  (with-airlock ()
    (let ((status (cell-entry "nodecode-airlock" "airlock" "")))
      (is (search "airlock.example.test" status) "the command names the origin it dials")
      (is (search "pinned key" status))
      (is (asked-for "/healthz") "the command asks the gate's own health"))
    (airlock-call "command" "echo hello")
    (let ((last (cell-entry "nodecode-airlock" "airlock" "last")))
      (is (search "last receipt" last))
      (is (search (nodecode-airlock::require-receipt-member
                   (nlk:decode-json +allow-step+) "receipt_id" "the step")
                  last) "the receipt the tool verified is the one the command shows"))))

(deftest airlock-cell-an-unreachable-gate-is-a-refusal ()
  (with-gate
    (with-cell-stop ((airlock-start "base_url" "https://airlock.example.test"
                                    "public_key_hex" +allow-public-key+))
      (with-stubbed-fdefinition (nlk:http (&rest arguments)
                                 (length arguments)
                                 (error "connection refused"))
        (let ((words (refusal-text nodecode-airlock:airlock-error
                       (airlock-call "command" "echo hello"))))
          (is (search "could not be reached" words))
          (is (search "connection refused" words)))))))

(deftest airlock-cell-a-redirect-is-refused ()
  (with-airlock ()
    (answer-with "/api/arena" "" 302)
    (let ((words (refusal-text nodecode-airlock:airlock-error
                   (airlock-call "command" "echo hello"))))
      (is (search "HTTP 302" words) "a redirect is a refusal, not a follow")
      (is (search "never replayed" words)))))
