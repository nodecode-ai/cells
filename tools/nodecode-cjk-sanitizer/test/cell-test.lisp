;;;; cell-test.lisp --- lifecycle; the hook on the stream; /cjk; the tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the start installs one advice on the stream and
;;;; nothing else, and the stop takes it back; a fragment of the answer is
;;;; cleaned where it is made, so no consumer sees the leak; a fragment that
;;;; was nothing but the leak is dropped rather than emitted empty; the
;;;; thinking stream is left alone unless a setting says otherwise;
;;;; /cjk reports, pauses, resumes and clears its counters; and cjk_strip,
;;;; called as the provider's call calls it, answers the cleaned text.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "cjk-sanitizer"
  (:config)
  (:hooks 'nle::emit-stream-part)
  (:help :cjk)
  (:command "cjk")
  (:running
   (is (eq t (nodecode-cjk-sanitizer::setting :strip-chinese)) "the defaults strip Chinese")
   (is (eq t (nodecode-cjk-sanitizer::setting :strip-cyrillic)) "and Cyrillic")
   (is (= 2 (nodecode-cjk-sanitizer::setting :blank-lines)) "one blank line kept"))
  (:refused ("blank_lines" 0)))

(deftest cjk-sanitizer-the-hook-cleans-a-fragment-where-it-is-made ()
  (with-cell-stop ((cjk-sanitizer-start))
    (let ((parts (cjk-parts :text-delta "the build is 完成 done")))
      (is (= 1 (length parts)) "one part out")
      (is (eq :text-delta (nle::stream-part-kind (first parts))) "still a text delta")
      (is (equal "the build is done" (nle::stream-part-text (first parts))) "and it carries no hanzi"))
    (let ((parts (cjk-parts :text-delta "готово")))
      (is (null parts) "a fragment that was only Cyrillic is not emitted empty"))
    (let ((parts (cjk-parts :text-delta "ok")))
      (is (equal "ok" (nle::stream-part-text (first parts))) "plain text is untouched"))
    (let ((parts (cjk-parts :reasoning-delta "思考")))
      (is (equal "思考" (nle::stream-part-text (first parts))) "the thinking stream keeps what the model thought in"))))

(deftest cjk-sanitizer-a-whole-fragment-of-leak-is-dropped ()
  ;; The original answered NIL when nothing changed; here a delta that was
  ;; nothing but the leak is not emitted at all, so no empty span is opened.
  (with-cell-stop ((cjk-sanitizer-start))
    (is (null (cjk-parts :text-delta "完成")) "nothing handed on")
    (is (null (cjk-parts :text-delta "。")) "not even for a punctuation mark")
    (is (search "2 dropped" (nodecode-cjk-sanitizer:run-slash "" nil)) "and the report counts what never streamed")))

(deftest cjk-sanitizer-strip-reasoning-takes-the-thinking-stream-too ()
  (with-cell-stop ((cjk-sanitizer-start "strip_reasoning" t))
    (let ((parts (cjk-parts :reasoning-delta "思考 done")))
      (is (equal " done" (nle::stream-part-text (first parts))) "the thinking stream is cleaned when asked for"))))

(deftest cjk-sanitizer-slash-reports-pauses-and-resumes ()
  (with-cell-stop ((cjk-sanitizer-start))
    (let ((report (nodecode-cjk-sanitizer:run-slash "" nil)))
      (is (search "cjk: on" report) "it starts on")
      (is (search "chinese on" report) "the settings as declared")
      (is (search "0 deltas read, 0 characters taken out" report) "and nothing done yet"))
    (cjk-parts :text-delta "完成 a")
    (is (search "1 deltas read, 2 characters taken out"
                (nodecode-cjk-sanitizer:run-slash "" nil)) "the counters follow the stream")
    (is (search "paused" (nodecode-cjk-sanitizer:run-slash "off" nil)) "/cjk off pauses")
    (is (equal "完成 a" (nle::stream-part-text (first (cjk-parts :text-delta "完成 a")))) "and the next fragment comes through whole")
    (is (search "on -" (nodecode-cjk-sanitizer:run-slash "on" nil)) "/cjk on resumes")
    (is (search "cleared" (nodecode-cjk-sanitizer:run-slash "reset" nil)) "/cjk reset clears")
    (is (search "0 deltas read" (nodecode-cjk-sanitizer:run-slash "" nil)))
    (is (search "not one of its words" (nodecode-cjk-sanitizer:run-slash "wat" nil)))))

(deftest cjk-sanitizer-the-tool-cleans-the-text-it-is-given ()
  (with-cell-stop ((cjk-sanitizer-start))
    (let ((answer (nle::message-content
                   (execute-wire-call "cjk-call-1" "cjk_strip"
                                      "{\"text\": \"the build is 完成 done\"}"))))
      (is (search "the build is done" answer) "the text comes back clean")
      (is (search "took out 2 characters" answer) "and it says how much went"))
    (let ((answer (nle::message-content
                   (execute-wire-call "cjk-call-2" "cjk_strip"
                                      "{\"text\": \"ok かな\", \"scripts\": \"cyrillic\"}"))))
      (is (search "nothing to take out" answer) "a script turned off leaves nothing to do"))
    (is (search "not \"klingon\""
                (nle::message-content
                 (execute-wire-call "cjk-call-3" "cjk_strip"
                                    "{\"text\": \"ok\", \"scripts\": \"klingon\"}"))) "an unknown script is named, not silently ignored")))
