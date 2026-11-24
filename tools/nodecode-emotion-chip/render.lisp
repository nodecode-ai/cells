;;;; render.lisp --- a score, rendered for one voice dialect.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Five dialects, the original's: fish_s2 and fish_drama3 (Fish Audio),
;;;; eleven_v4 (ElevenLabs v3/v4 audio tags) and the two tagless ones that
;;;; speak the words alone. A cue the dialect cannot carry is dropped with a
;;;; note, never translated into a different one; the agent's own words are
;;;; passed through exactly. Params (Fish speed, ElevenLabs stability) come
;;;; from the rendering group of the reply's first cue.
;;;;
;;;; Nothing here calls anything: a render is a string and a plist.

(in-package #:nodecode-emotion-chip)

(defparameter *dialects*
  '(("fish_s2"
     :version "2026-10-22"
     :spans (("soft" . "[soft tone] {text}") ("whisper" . "[whispering] {text}")
             ("stress" . "[emphasis] {text}") ("emphasis" . "[emphasis] {text}")
             ("fast" . "[in a hurry tone] {text}") ("slow" . "[slowly] {text}")
             ("smiling" . "[smiling] {text}") ("forceful" . "[firmly] {text}"))
     :events (("chuckle" . "[chuckling]") ("laugh" . "[laughing]") ("laugh:harder" . "[laughing]")
               ("giggle" . "[chuckling]") ("sigh" . "[sighing]") ("sigh:relieved" . "[sighing]")
               ("sigh:resigned" . "[sighing]") ("gasp" . "[gasping]") ("clears-throat" . "[clear throat]")
               ("breath" . nil) ("breath:shaky" . nil) ("exhale" . nil)
               ("pause" . "[break]") ("pause:long" . "[long-break]"))
     :speaker nil
     :notes "Span scope is sentence-level."
     :caps ())
    ("fish_drama3"
     :version "2026-10-22"
     :spans (("soft" . "<soft>{text}</soft>") ("whisper" . "<whisper>{text}</whisper>")
             ("stress" . "<stress>{text}</stress>") ("emphasis" . "<emphasis>{text}</emphasis>")
             ("fast" . "<fast>{text}</fast>") ("slow" . "<slow>{text}</slow>")
             ("smiling" . "[smiling] {text}") ("forceful" . "<forceful>{text}</forceful>"))
     :events (("chuckle" . "[chuckling]") ("laugh" . "[laughing]") ("laugh:harder" . "[laughing]")
               ("giggle" . "[chuckling]") ("sigh" . "[sighing]") ("sigh:relieved" . "[sighing]")
               ("sigh:resigned" . "[sighing]") ("gasp" . "[gasping]") ("clears-throat" . "[clear throat]")
               ("breath" . nil) ("breath:shaky" . "[shaky breath]") ("exhale" . nil)
               ("pause" . "[break]") ("pause:long" . "[long-break]"))
     :speaker "<|speaker:{id}|>"
     :notes ""
     :caps ())
    ("eleven_v4"
     :version "2026-10-22"
     :spans (("soft" . "[softly] {text}") ("whisper" . "[whispers] {text}")
             ("stress" . :uppercase) ("emphasis" . :uppercase)
             ("slow" . "[slowly] {text}") ("fast" . "[rushed] {text}")
             ("smiling" . "[smiling] {text}") ("forceful" . "[firmly] {text}"))
     :events (("chuckle" . "[chuckles]") ("laugh" . "[laughs]") ("laugh:harder" . "[laughs harder]")
               ("giggle" . "[giggles]") ("sigh" . "[sighs]") ("sigh:relieved" . "[sighs]")
               ("sigh:resigned" . "[sighs]") ("gasp" . "[gasps]") ("clears-throat" . "[clears throat]")
               ("breath" . "[exhales]") ("breath:shaky" . "[shaky breath]") ("exhale" . "[exhales]")
               ("pause" . "[pause]") ("pause:long" . "[long pause]"))
     :speaker nil
     :notes "Tags are read only by the v3 and v4 models."
     :caps (("eleven_v4" . 10000) ("eleven_v4_turbo" . 10000) ("eleven_v3" . 5000)
            ("eleven_v3_conversational" . 5000)))
    ("tagless"
     :version "2026-10-22"
     :spans () :events () :speaker nil
     :notes "The words alone: every cue is dropped, and the text reads correctly without them."
     :caps ())
    ("prose"
     :version "2026-10-22"
     :spans () :events () :speaker nil :notes "" :caps ()))
  "Dialect name => what it can carry. A NIL literal is a token this dialect drops.")

(defun dialect-name (name)
  (string-downcase (string name)))

(defun dialect-table (name)
  (cdr (assoc (dialect-name name) *dialects* :test #'string=)))

(defun tagless-p (name)
  (member (dialect-name name) '("tagless" "prose") :test #'string=))

(defun dialect-for (backend model)
  "The dialect BACKEND's MODEL speaks => (VALUES DIALECT NOTES)."
  (let ((backend (string-downcase (string backend)))
        (model (and model (string-downcase (string model)))))
    (cond
      ((string= backend "fish")
       (cond ((member model '("s2.1-pro" "s2.1-pro-free" "s2-pro") :test #'string=) (values "fish_s2" '()))
             ((string= (or model "") "s1") (values "tagless" '("fish s1 takes fixed cues only; rendered tagless")))
             ((member model '("drama-3" "drama-3-preview") :test #'string=) (values "fish_drama3" '()))
             ((null model) (values "fish_s2" '()))
             (t (values "tagless" (list (format nil "unknown Fish model: ~a; rendered tagless" model))))))
      ((string= backend "eleven")
       (cond ((member model '("eleven_v4" "eleven_v4_turbo" "eleven_v3" "eleven_v3_conversational")
                      :test #'string=)
              (values "eleven_v4" '()))
             ((member model '("eleven_flash_v2_5" "eleven_multilingual_v2" "eleven_turbo_v2_5")
                      :test #'string=)
              (values "tagless" (list (format nil "~a reads no audio tags; rendered tagless" model))))
             ((null model) (values "eleven_v4" '()))
             (t (values "tagless" (list (format nil "unknown ElevenLabs model: ~a; rendered tagless" model))))))
      (t (values "tagless" (list (format nil "unknown backend: ~a; rendered tagless" backend)))))))

;;; --- params ------------------------------------------------------------------------------

(defun cue-group (cue)
  "The rendering group CUE's most aroused word belongs to, or NIL."
  (let ((best nil) (best-arousal -1))
    (dolist (word (and cue (cue-words cue)))
      (multiple-value-bind (group arousal) (lexicon word)
        (when (and group (> (abs arousal) best-arousal))
          (setf best group best-arousal (abs arousal)))))
    best))

(defun group-row (group)
  (find group *groups* :key #'first :test #'string=))

(defun clamp (low high value)
  (max low (min high value)))

(defun render-params (dialect group &key (stability 0.5) (similarity 0.75) (speed-offset 0.0))
  "The voice parameters DIALECT asks for when the reply's delivery is GROUP."
  (let ((row (group-row group)))
    (cond
      ((cl-ppcre:scan "^fish_" (dialect-name dialect))
       (list :speed (clamp 0.85 1.10 (+ (if row (second row) 1.0) (or speed-offset 0.0)))))
      ((string= (dialect-name dialect) "eleven_v4")
       (list :stability (clamp 0.0 1.0 (+ (or stability 0.5) (if row (third row) 0.0)))
             :similarity_boost (clamp 0.0 1.0 (or similarity 0.75))))
      (t '()))))

;;; --- the text ----------------------------------------------------------------------------

(defun emit-span (table span)
  "SPAN as TABLE writes it => (VALUES TEXT NOTE): the operator's template, or the words alone."
  (let* ((template (cdr (assoc (span-op span) (getf table :spans) :test #'string=)))
         (at (and (stringp template) (search "{text}" template))))
    (cond ((null template) (values (span-text span) "unsupported"))
          ((eq template :uppercase) (values (string-upcase (span-text span)) nil))
          (at (values (concatenate 'string (subseq template 0 at) (span-text span)
                                   (subseq template (+ at 6)))
                      nil))
          (t (values template nil)))))

(defun emit-event (table event)
  "EVENT as TABLE writes it => (VALUES TEXT NOTE): its vendor literals, or NIL."
  (let ((literal (assoc (event-id event) (getf table :events) :test #'string=)))
    (values (cdr literal) (if (cdr literal) nil "unsupported"))))

(defun affect-sound-p (id)
  (eq (sound-kind id) :affect))

(defun opener-cue (header sounds)
  "The header as the cue it opens the reply with, its sound words dropped when SOUNDS is off."
  (let ((cue (header-cue header)))
    (when (and cue (not sounds))
      (let ((words (loop for part in (cue-parts cue)
                         unless (affect-sound-p (gethash (squeezed (string-downcase (nth-value 1 (rung-of (trimmed part)))))
                                                         (event-table)))
                           collect (nth-value 1 (rung-of (trimmed part))))))
        (setf cue (when words (cue-of (format nil "~{~a~^, ~}" words))))))
    cue))

(defun tidy (string)
  "Gaps closed (a space only between two word characters) and runs of spaces outside brackets as one."
  (let ((out (make-string-output-stream)) (i 0) (n (length string)) (depth 0) (space nil))
    (loop while (< i n) do
      (let ((char (char string i)))
        (cond
          ((char= char (char +gap+ 0))
           (let ((start i))
             (loop while (and (< i n) (char= (char string i) (char +gap+ 0))) do (incf i))
             (let ((before (and (plusp start) (char string (1- start))))
                   (after (and (< i n) (char string i))))
               (when (and before after (alphanumericp before) (alphanumericp after))
                 (unless space (write-char #\Space out)))
               (setf space t))))
          ((char= char #\[) (incf depth) (write-char char out) (setf space nil) (incf i))
          ((char= char #\]) (when (plusp depth) (decf depth)) (write-char char out) (setf space nil) (incf i))
          ((and (zerop depth) (member char '(#\Space #\Tab))) 
           (unless space (write-char #\Space out))
           (setf space t)
           (incf i))
          (t (write-char char out) (setf space nil) (incf i)))))
    (string-trim " " (get-output-stream-string out))))

(defun render-score (score dialect &key (sounds t) (stability 0.5) (similarity 0.75)
                                       (speed-offset 0.0) (model nil))
  "SCORE rendered for DIALECT => a RENDERED: text, params, notes and what was dropped."
  (let* ((name (dialect-name dialect))
         (table (or (dialect-table name) (dialect-table "tagless")))
         (notes (if (dialect-table name) '() (list (format nil "unknown dialect: ~a; rendered tagless" dialect))))
         (tagless (tagless-p name))
         (dropped (copy-list (score-dropped score)))
         (opener (opener-cue (score-header score) sounds)))(let* ((lead (or opener (loop for sentence in (score-sentences score)
                                  when (find-if #'cue-p (sentence-tokens sentence))
                                    return (find-if #'cue-p (sentence-tokens sentence)))))
           (group (cue-group lead))
           (params (render-params name group :stability stability :similarity similarity
                                               :speed-offset speed-offset))
           (chunks '()))
      (when (and (score-header score) (or tagless (null opener)))
        (push (make-dropped :kind "header" :value (header-raw (score-header score)) :reason "not spoken") dropped))
      (let ((first-p t))
       (dolist (sentence (score-sentences score))
        (let ((parts '())
              (cues 0))
          (when (and first-p opener (not tagless))
            (let ((leading (find-if-not (lambda (token)
                                           (and (plain-p token)
                                                (zerop (length (trimmed (remove (char +gap+ 0) (plain-s token)))))))
                                         (sentence-tokens sentence))))
              (unless (cue-p leading)
                (push (format nil "[~a] " (cue-raw opener)) parts))))
          (setf first-p nil)
          (dolist (token (sentence-tokens sentence))
            (cond
              ((plain-p token) (push (plain-s token) parts))
              ((cue-p token)
               (incf cues)
               (if tagless
                   (progn (push (make-dropped :kind "cue" :value (cue-raw token) :reason "unsupported") dropped)
                          (push +gap+ parts))
                   (push (format nil "[~a]" (cue-raw token)) parts)))
              ((span-p token)
               (multiple-value-bind (text note) (emit-span table token)
                 (when note
                   (push (make-dropped :kind "span" :value (span-op token) :reason note) dropped))
                 (push text parts)
                 (when (and (string= name "fish_s2") (not note))
                   (push "span scope lost (sentence-level)" notes)))
               (when tagless (push +gap+ parts) (push +gap+ parts)))
              ((event-p token)
               (cond
                 ((and (not sounds) (affect-sound-p (event-id token)))
                  (push (make-dropped :kind "event" :value (event-id token) :reason "sounds off") dropped)
                  (push +gap+ parts))
                 (t (multiple-value-bind (text note) (emit-event table token)
                      (when note
                        (push (make-dropped :kind "event" :value (event-id token) :reason note) dropped))
                      (push (or text +gap+) parts)))))
              ((speaker-p token)
               (push (make-dropped :kind "speaker" :value (speaker-id token) :reason "unsupported") dropped)
               (push +gap+ parts))))
          (when (> cues 3)
            (push (format nil "sentence has ~a cues (Fish guidance: <=3)" cues) notes))
          (let ((text (tidy (format nil "~{~a~}" (nreverse parts)))))
            (when (plusp (length text)) (push text chunks))))))
      (let* ((text (tidy (format nil "~{~a~^ ~}" (nreverse chunks))))
             (text (if (string= name "eleven_v4")
                       (cl-ppcre:regex-replace-all "(?<=[^\\W_])\\.\\.(?=\\s|$)" text (string (code-char #x2026)))
                       text))
             (cap (and (string= name "eleven_v4")
                       (cdr (assoc (or model "eleven_v4") (getf table :caps) :test #'string=)))))
        (when (and cap (> (length text) cap))
          (push (format nil "character cap exceeded (~a > ~a)" (length text) cap) notes))
        (make-rendered :text text :params params :notes (nreverse notes)
                       :dropped (nreverse dropped) :dialect name :group group)))))

(defun render (text-or-score dialect &rest keys)
  "TEXT or a SCORE, rendered for DIALECT: the one entry point the cell's verbs use."
  (apply #'render-score
         (if (score-p text-or-score) text-or-score (parse-score text-or-score))
         dialect keys))

(defun strip-score (text)
  "TEXT with every cue, span and sound dropped: what a voice without tags speaks."
  (rendered-text (render text "tagless")))

