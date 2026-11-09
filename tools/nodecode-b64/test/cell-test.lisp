;;;; cell-test.lisp --- the cell's half: the tool, the command, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the declaration installs the tool, /b64 and (help :b64)
;;;; and takes all three back on stop; b64_decode answers the description the
;;;; original's @b64: reference put in the message, called through the wire a
;;;; provider's call comes through; a value that cannot be read comes back as a
;;;; line the model can act on rather than a failed call; /b64 answers the same
;;;; for the operator; a value with no "value" member is refused at the
;;;; boundary; and the section's bounds are the ones the reads keep to.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "b64"
  (:config)
  (:help :b64)
  (:command "b64")
  (:running
   (is (= 65536 (nodecode-b64::setting :max-input-chars)) "the original's own ceiling on a value")
   (is (= 1048576 (nodecode-b64::setting :max-inflated-bytes)) "and on what it inflates to"))
  (:refused ("text_chars" 0)))

(deftest b64-cell-the-tool-answers-the-description ()
  (with-cell-stop ((b64-start))
    (let ((answer (b64-tool "b64-call-1" "{\"value\": \"SGVsbG8sIHdvcmxkIQ==\"}")))
      (is (search "base64: 18 characters -> 13 bytes" answer) "how it was read")
      (is (search "Hello, world!" answer) "and what it holds"))
    (let ((answer (b64-tool "b64-call-2" "{\"value\": \"48656c6c6f\", \"encoding\": \"hex\"}")))
      (is (search "  Hello" answer) "the encoding member picks the encoding"))
    (let ((answer (b64-tool "b64-call-3" "{\"value\": \"SGVsbG8sI\"}")))
      (is (search "cut short" answer) "a refusal is a line, not a failed call"))))

(deftest b64-cell-the-command-answers-the-operator ()
  (with-cell-stop ((b64-start))
    (is (search "  Hello" (nodecode-b64:run-slash "hex:48656c6c6f" nil)) "/b64 decodes what is typed after it")
    (is (search "/b64 <base64" (nodecode-b64:run-slash "" nil)) "bare, it says how to use it")
    (is (search "not base64, base64url, hex" (nodecode-b64:run-slash "hello, world!" nil)) "and a value it cannot read comes back with the reason")))

(deftest b64-cell-a-value-is-required-at-the-boundary ()
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-b64::b64-arguments)
                             (nlk:make-json-object "encoding" "auto"))) "a call with no value is refused by the declaration")
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-b64::b64-arguments)
                             (nlk:make-json-object "value" "SGVsbG8=" "encoding" "base32"))) "and so is an encoding outside the four"))

(deftest b64-cell-the-section-bounds-the-reads ()
  (with-cell-stop ((b64-start "max_input_chars" 16))
    (is (search "reads at most 16" (b64-tool "b64-call-4" "{\"value\": \"SGVsbG8sIHdvcmxkIQ==\"}")) "a value longer than the section allows is refused, not cut"))
  (with-cell-stop ((b64-start "max_inflated_bytes" 1024))
    (let* ((zeros (make-array 4000 :element-type '(unsigned-byte 8) :initial-element 0))
           (value (b64-base64 (b64-gzip zeros)))
           (answer (b64-tool "b64-call-5" (format nil "{\"value\": \"~a\"}" value))))
      (is (search "cut at 1,024 bytes" answer) "inflation stops at the section's ceiling")))
  (with-cell-stop ((b64-start "text_chars" 40))
    (let* ((value (b64-text "a sentence longer than forty characters, so the rest is counted"))
           (answer (b64-tool "b64-call-6" (format nil "{\"value\": \"~a\"}" value))))
      (is (search "more characters" answer) "and the rest of a long result is counted"))))
