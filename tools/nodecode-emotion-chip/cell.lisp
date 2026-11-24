;;;; cell.lisp --- what this folder contributes: two tools, one command, one topic.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The renderer is the cell's own (score.lisp, render.lisp); this file is
;;;; the boundary over it. A tool call is typed by a contract, so a call that
;;;; names a dialect this cell does not have is refused before the body runs,
;;;; and the body reads members, never a hash table.

(in-package #:nodecode-emotion-chip)

(nlk:define-peripheral emotion-chip :not-running t)

(defparameter +grammar+
  "A delivery score is the reply's own brackets:
- [word] or [word, word] before a sentence sets its delivery; add slightly or very for intensity.
- [[register: word; word]] at the very start sets the whole reply's register, and is never spoken.
- Inside one sentence: [soft: words], [whisper: words], [stress: words], [slow: words], [fast: words],
  [smiling: words], [emphasis: words], [forceful: words].
- Sounds: [laugh] [chuckle] [giggle] [sigh] [gasp] [clears-throat], and [pause] to hold a beat.
- Code and numbers stay text: items[0], x[i][j], [1], [::1], and a link [docs](https://example.com)."
  "The grammar, as the manual topic and the tools quote it.")

(defparameter +choosing+
  "Settle what you will say first, then pick the delivery that fits the stance you take:
one opening cue or a register header can carry a whole reply, and change it only when the stance changes.
Neutral is a valid choice, and untagged speech is fine. Write the delivery you want, not the one you don't:
[firm], not [firm without shouting]."
  "The original's guidance on choosing a cue, kept because it is what makes the score read well.")

(defparameter +licence-note+
  "You may voice what is operative in the conversation: delight, curiosity, irritation, worry, sadness,
tenderness, excitement, or plain neutrality. A cue describes the delivery you choose, not a claim about
inner experience; keep facts, uncertainty and disagreement intact."
  "The original's licence line for the agent, unchanged in meaning.")

(defun configured-dialect ()
  "The dialect this home's backend and model speak => (VALUES DIALECT NOTES)."
  (dialect-for (setting :backend) (setting :model)))

(defun setting-sounds-p ()
  "Whether the operator left affect sounds on."
  (if (setting :sounds) t nil))

(defun dialect-carries (dialect)
  "The operators and sounds DIALECT actually renders, as one line."
  (let* ((table (dialect-table dialect))
         (spans (loop for (op . template) in (getf table :spans)
                       when template collect op))
         (events (loop for (id . literal) in (getf table :events)
                       when literal collect id)))
    (format nil "~a renders ~:[no span operators~;~:*~{~a~^, ~}~] and ~:[no sounds~;~:*~{~a~^, ~}~]. ~a"
            (dialect-name dialect) spans events (or (getf table :notes) ""))))

(defun guide-text (&optional dialect)
  "The delivery guide for DIALECT: what a score may say, and how this voice renders it."
  (let ((dialect (or dialect (configured-dialect))))
    (format nil "~a~%~%~a~%~%~a~%~%This voice: ~a"
            +licence-note+ +grammar+ +choosing+ (dialect-carries dialect))))

(defun plist-object (plist)
  "PLIST as a JSON object, its keys downcased."
  (apply #'nlk:make-json-object
         (loop for (key value) on plist by #'cddr
               append (list (string-downcase (string key)) value))))

(defun dropped-strings (dropped)
  "Each drop as one line: what, and why."
  (coerce (loop for row in dropped
                collect (format nil "~a ~a~@[: ~a~]" (dropped-kind row) (dropped-value row)
                                (let ((reason (dropped-reason row)))
                                  (and (plusp (length reason)) reason))))
          'vector))

(defun sounds-argument (value)
  "VALUE, the tool's on/off, against the operator's own switch."
  (cond ((null value) (setting-sounds-p))
        ((string-equal value "on") t)
        ((string-equal value "off") nil)
        (t (setting-sounds-p))))

;;; --- the two tools --------------------------------------------------------------------

(defun render-tool (record)
  "Render the score in RECORD => the rendered text, its params, and what changed."
  (let* ((dialect (or (emotion-chip-render-dialect record) (configured-dialect)))
         (rendered (render (emotion-chip-render-text record) dialect
                           :sounds (sounds-argument (emotion-chip-render-sounds record))
                           :model (setting :model))))
    (nlk:encode-json-object
     (nlk:json-object
      "text" (rendered-text rendered)
      "dialect" (rendered-dialect rendered)
      "params" (plist-object (rendered-params rendered))
      "notes" (coerce (rendered-notes rendered) 'vector)
      "dropped" (dropped-strings (rendered-dropped rendered))))))

(defun guide-tool (record)
  "The delivery guide for the dialect RECORD names, or for this home's."
  (guide-text (or (emotion-chip-guide-dialect record) (configured-dialect))))

;;; --- the command ------------------------------------------------------------------------

(defun status-text ()
  "One line on what this cell renders with, and the grammar to write for it."
  (multiple-value-bind (dialect notes)
      (configured-dialect)
    (format nil "emotion-chip: ~a on ~a, model ~a, sounds ~a~@[ (~{~a~^; ~})~].~%~%~a"
            dialect (setting :backend) (setting :model)
            (if (setting-sounds-p) "on" "off") notes
            (guide-text dialect))))

(defun run-command (args session-id)
  "One /emotion-chip: no argument is the status, guide is the grammar, anything else is a score to render."
  (declare (ignore session-id))
  (let ((argument (string-trim " " (or args ""))))
    (cond
      ((zerop (length argument)) (status-text))
      ((string-equal argument "guide") (guide-text))
      ((string-equal argument "sounds on")
       (nle:config-set '("emotion-chip" "sounds") t)
       "emotion-chip: sounds on")
      ((string-equal argument "sounds off")
       (nle:config-set '("emotion-chip" "sounds") nil)
       "emotion-chip: sounds off")
      (t (rendered-text (render argument (configured-dialect)))))))

;;; --- the boundary the tools are typed by ---------------------------------------------------

(nlk:define-contract emotion-chip-render
  (:doc "Render an agent-written delivery score for the voice this home speaks: bracket cues and sounds
become the tags Fish Audio or ElevenLabs reads, and a voice without tags gets the words alone. The text is
returned exactly as the agent wrote it apart from the cues, and nothing is added that it did not write.")
  ("text" :text :required t
   :doc "the reply, with its score: [sigh], [warm, amused], [whisper: it passed.], [pause], or a leading [[register: calm]]")
  ("dialect" :text :options '("fish_s2" "fish_drama3" "eleven_v4" "tagless")
   :doc "the voice's tag dialect; left out, it is the one this home's backend and model speak")
  ("sounds" :text :options '("on" "off")
   :doc "affect sounds (laughs, sighs); left out, the operator's own switch decides"))

(nlk:define-contract emotion-chip-guide
  (:doc "The delivery score's grammar for one voice: the cues, spans and sounds it renders, and what to
keep in mind when choosing one.")
  ("dialect" :text :options '("fish_s2" "fish_drama3" "eleven_v4" "tagless")
   :doc "the voice to describe; left out, it is this home's"))

(nle:define-cell emotion-chip
  (:section ("emotion-chip")
    (:guide "set backend and the model id the voice runs (s2.1-pro or drama-3 on Fish, eleven_v4 on ElevenLabs); sounds can follow the operator's switch")
    ("backend" :choice :options '("fish" "eleven") :default "fish"
     :doc "which voice service the score is rendered for")
    ("model" :string :required t
     :doc "that service's model id: s2.1-pro, s2-pro, drama-3 or s1 on Fish; eleven_v4, eleven_v4_turbo or eleven_v3 on ElevenLabs")
    ("sounds" :boolean :default t
     :doc "render affect sounds (a laugh, a sigh); off keeps cues, spans and pauses"))
  (:help :emotion-chip
   "emotion-chip: a delivery score the voice renders - render with emotion_chip, or (help emotion-chip)"
   (guide-text "eleven_v4"))
  (:tool "emotion_chip" (nlk:contract 'emotion-chip-render) 'render-tool)
  (:tool "emotion_chip_guide" (nlk:contract 'emotion-chip-guide) 'guide-tool)
  (:command "emotion-chip" 'run-command
            :description "Render a delivery score, or show the grammar"
            :argument-hint "[score] | guide | sounds on|off"))
