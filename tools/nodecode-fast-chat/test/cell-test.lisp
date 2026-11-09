;;;; cell-test.lisp --- the lifecycle, the argument line, a pinned session.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: that the folder installs exactly what it declares
;;;; and takes it back, that a section without a model is a refusal; that a
;;;; typed line reads as the provider, model, rung, directory and prompt it
;;;; names, and that a model id is not cut at a slash or a colon that is
;;;; part of it; and that the command opens a real session on the target it
;;;; read -- the section's, or the line's -- pinning that pick on the new
;;;; session alone, while the session it was typed in keeps no pick at all.

(in-package #:nodecode.test)

;;; --- the slice and the folder's shape ---------------------------------------

(define-test-slice "fast-chat" "FAST-CHAT-CELL-" :start nodecode-fast-chat:start-cell
  :defaults (("model" "one") ("provider" "") ("effort" "low")))

(define-cell-lifecycle-tests "fast-chat"
  (:config)
  (:command "fast-chat")
  (:help :fast-chat)
  ;; A session cannot be opened on no model, so the folder refuses until the
  ;; section names one.
  (:refused ("model" nil))
  (:idle nodecode-fast-chat:fast-chat-error (nodecode-fast-chat:launch "two" "s1")))

(defun fast-chat-read (line)
  "LINE as the command reads it."
  (nodecode-fast-chat:parse-arguments line))

;;; --- reading the line -------------------------------------------------------

(deftest fast-chat-cell-a-model-id-is-not-cut-where-it-does-not-say-so ()
  (multiple-value-bind (model provider effort)
      (nodecode-fast-chat:split-model-spec "openai-codex/gpt-6.1-sol:high")
    (is (equal "gpt-6.1-sol" model) "the model stands between the prefix and the rung")
    (is (equal "openai-codex" provider) "the one slash names the provider")
    (is (equal "high" effort) "a rung after the colon is the rung"))
  (multiple-value-bind (model provider effort)
      (nodecode-fast-chat:split-model-spec "openrouter/deepseek/deepseek-r1:free")
    (is (equal "openrouter/deepseek/deepseek-r1:free" model) "two slashes are the model's own")
    (is (null provider) "no provider prefix is read")
    (is (null effort) "and free is no rung")))

(deftest fast-chat-cell-a-line-says-what-it-asks-for ()
  (let ((parsed (fast-chat-read
                  "canary-9 --provider acme --effort high --in work --prompt read the log")))
    (is (equal "canary-9" (getf parsed :model)) "the bare word is the model")
    (is (equal "acme" (getf parsed :provider)))
    (is (equal "high" (getf parsed :effort)))
    (is (equal "work" (getf parsed :directory)))
    (is (equal "read the log" (getf parsed :prompt)) "a prompt keeps its spaces")
    (is (null (getf parsed :help)))
    (is (getf (fast-chat-read "--help") :help) "a help line asks for the usage"))
  (is (signals-error nodecode-fast-chat:fast-chat-error (fast-chat-read "one two")) "two models refuse")
  (is (signals-error nodecode-fast-chat:fast-chat-error (fast-chat-read "--provider")) "a flag with no value refuses"))

;;; --- opening a session ------------------------------------------------------

(defun fast-chat-answered-session (answer)
  "The session id the command's text names, or NIL."
  (let* ((words (uiop:split-string answer :separator '(#\Space)))
         (at (position "session" words :test #'string=)))
    (and at (nth (1+ at) words))))

(deftest fast-chat-cell-a-bare-command-opens-the-sections-target ()
  (with-temp-store ()
    (ensure-durable-session "s1")
    (with-cell-stop ((fast-chat-start))
      (let* ((answer (nodecode-fast-chat:launch "" "s1"))
             (id (fast-chat-answered-session answer)))
        (multiple-value-bind (provider model) (nlk:session-model-selection id)
          (is (and id (plusp (length id))) "the answer names the session it opened")
          (is (search "the default model is untouched" answer) "and says the default stayed")
          (is (equal "one" model) "the session runs on the section's model")
          (is (null provider) "a blank provider pins none, so the default lane serves it"))
        (is (equal "low" (nlk:session-model-effort id)) "at the section's rung")
        (is (null (nlk:session-model-selection "s1")) "the session it was typed in keeps no pick")
        (is (not (equal "s1" id)) "the new session is a session of its own")))))

(deftest fast-chat-cell-a-line-opens-its-target-in-its-own-directory ()
  (with-temp-store ()
    (with-temp-workspace (root)
      (ensure-durable-session "s1")
      (with-cell-stop ((fast-chat-start))
        (let ((asked 0))
          (with-provider-stub (nil (incf asked) (wire-message "assistant" :content "ok"))
            (let* ((answer (nodecode-fast-chat:launch
                            (format nil "canary-9:high --in ~a --prompt look around"
                                    (namestring root))
                            "s1"))
                   (id (fast-chat-answered-session answer)))
              (multiple-value-bind (provider model) (nlk:session-model-selection id)
                (is (equal "canary-9" model) "the line's model is the session's pick")
                (is (null provider) "no provider prefix in the word, so none is pinned"))
              (is (equal "high" (nlk:session-model-effort id)) "and its rung")
              (is (equal (truename root) (truename (nlk:find-session-cwd id))) "it works where it was asked to")
              (is (await () (plusp asked)) "the first prompt started a turn")
              (is (null (nlk:session-model-selection "s1")) "the caller's own session kept none"))))))))
