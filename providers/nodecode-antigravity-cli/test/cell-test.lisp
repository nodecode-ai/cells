;;;; cell-test.lisp --- the antigravity-cli cell against stubbed I/O.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every round's child is stubbed through RUN-AGY-PROCESS: no CLI starts,
;;;; nothing touches the network or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "antigravity-cli" "ANTIGRAVITY-CLI-CELL-" :start nodecode-antigravity-cli:start-cell)

(define-cell-lifecycle-tests "antigravity-cli"
  (:hooks :credential 'nle::configured-provider-inventory 'nle::resolve-model-capability)
  (:refused ("command" 5)))

(defun agy-step (text)
  "One stream-json step_update line carrying TEXT."
  (format nil "{\"event\":\"step_update\",\"step_update\":{\"text_delta\":~s}}" text))

(defmacro with-agy-round ((parts) output &body forms)
  "FORMS with the cell started and the CLI stubbed to answer OUTPUT."
  `(with-cell-stop ((antigravity-cli-start))
     (let ((nle::*provider* "antigravity-cli") (nle::*model* "gemini-3.8-flash")
           (nle::*api-key* "x") (nle::*endpoint* nil))
       (with-stubbed-fdefinition (nodecode-antigravity-cli::run-agy-process (command argv prompt)
                                 (declare (ignore command argv prompt))
                                 ,output)
         (nodecode-antigravity-cli:call-agy-streaming
          (user-context "hello") :on-part (lambda (part) (push part ,parts)))
         ,@forms))))

(deftest antigravity-cli-cell-answers-a-round ()
  (let ((parts '()))
    (with-agy-round (parts)
        (format nil "~a~%~a~%" (agy-step "hel") (agy-step "lo"))
      (is (find :text-delta parts :key #'nle::stream-part-kind)
          "the answer streams as text deltas")
      (is (find :finish parts :key #'nle::stream-part-kind)
          "the stream is closed once"))))

(deftest antigravity-cli-cell-answers-the-rounds-message ()
  (with-cell-stop ((antigravity-cli-start))
    (let ((parts '()) (asked nil))
      (let ((nle::*provider* "antigravity-cli") (nle::*model* "gemini-3.8-flash")
            (nle::*api-key* "x") (nle::*endpoint* nil))
        (with-stubbed-fdefinition (nodecode-antigravity-cli::run-agy-process (command argv prompt)
                                  (setf asked (list command argv prompt))
                                  (format nil "~a~%" (agy-step "hi")))
          (multiple-value-bind (message usage finish request)
              (nodecode-antigravity-cli:call-agy-streaming
               (user-context "hello") :on-part (lambda (part) (push part parts)))
            (declare (ignore usage request))
            (is (equal "hi" (nlk:json-value message :string "content")))
            (is (equal "stop" finish))
            (is (equal "gemini-3.8-flash"
                       (second (member "--model" (second asked) :test #'equal)))
                "the model reaches the CLI's argv")
            (is (search "hello" (third asked)) "the prompt reaches the CLI's stdin")
            (is (member "stream-json" (second asked) :test #'equal)
                "the CLI is asked for stream-json events")))))))

(deftest antigravity-cli-cell-parses-a-tool-block ()
  (let ((parts '()))
    (with-agy-round (parts)
        (format nil "~a~%~a~%~a~%"
                (agy-step "I will ")
                (agy-step "<tool_call>{\"id\":\"c1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":\"{}\"}}</tool_call>")
                (agy-step " done"))
      (let ((call (find :tool-call parts :key #'nle::stream-part-kind)))
        (is call "the block becomes a tool call")
        (is (equal "read_file" (nle::stream-part-tool-name call)))
        (is (equal "I will  done"
                   (nlk:json-value (nodecode-antigravity-cli:call-agy-streaming
                                     (user-context "hello") :on-part nil)
                                   :string "content"))
            "the block itself is not the answer's text")))))

(deftest antigravity-cli-cell-closes-on-a-native-tool-step ()
  (let ((parts '()))
    (with-agy-round (parts)
        (format nil "~a~%~a~%~a~%"
                (agy-step "hi")
                "{\"event\":\"step_update\",\"step_update\":{\"step_type\":\"tool\",\"tool_name\":\"run_command\"}}"
                (agy-step "ignored"))
      (is (equal "hi"
                 (nlk:json-value
                  (nodecode-antigravity-cli:call-agy-streaming
                   (user-context "hello") :on-part nil)
                  :string "content"))
          "the round ends at the native tool step; what came after is not the answer"))))

(deftest antigravity-cli-cell-lists-its-models ()
  (with-cell-stop ((antigravity-cli-start))
    (multiple-value-bind (ids models)
        (nodecode-antigravity-cli::inventory
         (lambda (&rest arguments) (declare (ignore arguments)) (values '() '())))
      (is (member "antigravity-cli" ids :test #'equal))
      (is (= 5 (length models)) "the section's roster is what /models offers")
      (is (equal "Antigravity CLI" (getf (first models) :provider-display))))))
