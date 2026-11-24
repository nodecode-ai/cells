;;;; tests.lisp --- the renderer, the tools, the command, the section.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Nothing here dials anything: the cell never does. What is proved is
;;;; that a score renders in each dialect with nothing added and nothing
;;;; spoken that the agent did not write; that code, numbers and links stay
;;;; text; that the operator's quiet switch drops sounds and keeps pauses;
;;;; that a header opens the reply and sets its params; that the two tools
;;;; are registered and their calls are typed; that the command answers;
;;;; and that the section refuses what it cannot run on.

(in-package #:nodecode.test)

(define-test-slice "emotion-chip" "EMOTION-CHIP-" :start nodecode-emotion-chip:start-cell)

(defun chip-render (text dialect &rest keys)
  "TEXT rendered for DIALECT, the way a caller of the library would."
  (apply #'nodecode-emotion-chip:render text dialect keys))

(defun chip-text (text &rest dialect)
  (nodecode-emotion-chip:rendered-text (apply #'chip-render text dialect)))

(defun chip-tool (name &rest pairs)
  "NAME's registered body, called through the contract it declares."
  (let* ((entry (nle::find-tool name))
         (schema (and entry (second entry)))
         (arguments (apply #'nlk:make-json-object pairs)))
    (unless entry (error "no tool named ~a is registered" name))
    (funcall (third entry)
             (if (nlk:contract-p schema)
                 (nlk:contract-decode schema arguments)
                 arguments))))

(defun chip-answer (json)
  "The decoded JSON object a tool answered, for reading a member off it."
  (nlk:decode-json json))

;;; --- lifecycle -------------------------------------------------------------------------

(define-cell-lifecycle-tests "emotion-chip"
  (:config "backend" "eleven" "model" "eleven_v4")
  (:help :emotion-chip)
  (:command "emotion-chip")
  (:running
   (is (nle::find-tool "emotion_chip"))
   (is (nle::find-tool "emotion_chip_guide"))))

;;; --- one cue, every dialect --------------------------------------------------------------

(deftest emotion-chip-renders-one-cue-in-each-dialect ()
  (is (equal "[sighs] It passed." (chip-text "[sigh] It passed." "eleven_v4")))
  (is (equal "[sighing] It passed." (chip-text "[sigh] It passed." "fish_s2")))
  (is (equal "[sighing] It passed." (chip-text "[sigh] It passed." "fish_drama3")))
  (is (equal "It passed." (chip-text "[sigh] It passed." "tagless"))))

(deftest emotion-chip-a-cue-passes-the-agents-own-words-through ()
  (is (equal "[warm, amused] Good." (chip-text "[warm, amused] Good." "eleven_v4")))
  (is (equal "[utterly furious] Enough." (chip-text "[utterly furious] Enough." "eleven_v4")))
  (is (equal "Good." (chip-text "Good." "eleven_v4"))))

(deftest emotion-chip-a-span-becomes-each-dialects-tag ()
  (is (equal "[whispers] it passed." (chip-text "[whisper: it passed.]" "eleven_v4")))
  (is (equal "[whispering] it passed." (chip-text "[whisper: it passed.]" "fish_s2")))
  (is (equal "<whisper>it passed.</whisper>" (chip-text "[whisper: it passed.]" "fish_drama3")))
  (is (equal "it passed." (chip-text "[whisper: it passed.]" "tagless")))
  (is (equal "[soft tone] one [laughing] [soft tone] two" (chip-text "[soft: one [laugh] two]" "fish_s2"))))

(deftest emotion-chip-keeps-code-numbers-and-links-whole ()
  (is (equal "items[0] and x[i][j] and [1] and handlers[pause]"
             (chip-text "items[0] and x[i][j] and [1] and handlers[pause]" "fish_s2")))
  (is (equal "items[0] and x[i][j] and [1] and handlers[pause]"
             (chip-text "items[0] and x[i][j] and [1] and handlers[pause]" "eleven_v4")))
  (is (equal "see docs (https://example.com/a) now"
             (chip-text "see [docs](https://example.com/a) now" "eleven_v4"))))

(deftest emotion-chip-the-quiet-switch-drops-sounds-and-keeps-pauses ()
  (is (equal "[laughs] Right. [pause] Done." (chip-text "[laugh] Right. [pause] Done." "eleven_v4")))
  (is (equal "Right. [pause] Done."
             (chip-text "[laugh] Right. [pause] Done." "eleven_v4" :sounds nil)))
  (is (find "sounds off"
            (nodecode-emotion-chip:rendered-dropped (chip-render "[laugh] Right." "eleven_v4" :sounds nil))
            :key #'nodecode-emotion-chip:dropped-reason :test #'equal)))

(deftest emotion-chip-a-header-opens-the-reply-and-sets-its-params ()
  (let ((rendered (chip-render "[[register: warm, steady]] It passed." "eleven_v4")))
    (is (equal "[warm, steady] It passed." (nodecode-emotion-chip:rendered-text rendered)))
    (is (equal "calm_contentment" (nodecode-emotion-chip:rendered-group rendered)))
    (is (numberp (getf (nodecode-emotion-chip:rendered-params rendered) :stability))))
  (is (equal "It passed." (chip-text "[[register: warm]] It passed." "tagless"))))

;;; --- the tools and the command -----------------------------------------------------------

(deftest emotion-chip-a-tool-call-renders-and-is-typed ()
  (with-cell-stop ((emotion-chip-start "backend" "eleven" "model" "eleven_v4"))
    (let ((answer (chip-answer (chip-tool "emotion_chip" "text" "[sigh] It passed."))))
      (is (equal "[sighs] It passed." (nlk:json-value answer :string "text")))
      (is (equal "eleven_v4" (nlk:json-value answer :string "dialect"))))
    (is (equal "It passed."
               (nlk:json-value (chip-answer (chip-tool "emotion_chip" "text" "[sigh] It passed." "dialect" "tagless"))
                               :string "text")))
    (signals-error nlk:contract-refusal (chip-tool "emotion_chip" "text" "hi" "dialect" "ssml"))
    (signals-error nlk:contract-refusal (chip-tool "emotion_chip"))
    (is (search "[whisper: words]" (chip-tool "emotion_chip_guide" "dialect" "fish_drama3")))))

(deftest emotion-chip-the-command-answers-status-guide-and-a-score ()
  (with-cell-stop ((emotion-chip-start "model" "s2.1-pro"))
    (is (search "emotion-chip: fish_s2" (nodecode-emotion-chip::run-command nil "s")))
    (is (search "[whisper: words]" (nodecode-emotion-chip::run-command "guide" "s")))
    (is (equal "[whispering] it passed." (nodecode-emotion-chip::run-command "[whisper: it passed.]" "s")))
    (is (search "sounds on" (nodecode-emotion-chip::status-text)))))

(deftest emotion-chip-a-setting-outside-its-choices-refuses ()
  (signals-error nlk:config-refusal
    (emotion-chip-start "backend" "azure" "model" "s2.1-pro")))
