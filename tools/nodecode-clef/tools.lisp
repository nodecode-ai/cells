;;;; tools.lisp --- the four tools the server lists, the four verbs they share.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin offered whatever tools https://clefdrills.com/mcp listed; at
;;;; the reviewed commit those four were find_music_guides, transpose_chords,
;;;; spell_piano_chord and tab_to_piano_notes, with the member names below.
;;;; Each is registered here under the same name and the same members, and
;;;; answers lines a model reads rather than a JSON envelope it has to parse.
;;;;
;;;; transpose_chords is answered in the cell when offline_transpose is on,
;;;; as the plugin's own scripts/transpose.py answered without a network;
;;;; the other three always ask the server.

(in-package #:nodecode-clef)

;;; --- the boundary: what a model call carries ---------------------------------------

(nlk:define-contract transpose-request
  (:doc "Transpose chord symbols, including slash bass notes. Does not process
audio, lyrics or notation files.")
  ("chords" :text :required t
   :doc "the chord symbols to move, e.g. C G Am F")
  ("semitones" :integer :required t
   :doc "how far to move them, -24 to 24; negative goes down")
  ("preferFlats" :boolean :slot prefer-flats
   :doc "spell the result with flats instead of sharps"))

(nlk:define-contract spell-request
  (:doc "Spell a chord as piano pitches with octave and MIDI values. This does
not create a rhythmic arrangement.")
  ("symbol" :text :slot chord-symbol :required t
   :doc "the chord symbol, e.g. Cmaj7 or F#m7b5")
  ("octave" :integer
   :doc "the octave the spelling starts in, 1 to 6; 4 when absent"))

(nlk:define-contract tab-request
  (:doc "Convert six-string ASCII guitar tab to sounding piano pitches. Tab
spacing does not specify note duration.")
  ("tab" :text :required t
   :doc "the ASCII tab, six lines, one per string")
  ("capo" :integer
   :doc "the capo fret, 0 to 12; 0 when absent"))

(nlk:define-contract guide-request
  (:doc "Find Clef instructional guides by words in the title and description.
Guides are not file converters.")
  ("query" :text :required t
   :doc "the words to find guides by, e.g. guitar or scales"))

;;; --- the endpoint the section names ------------------------------------------------

(defun endpoint ()
  "The MCP endpoint the section names, or the public one."
  (or (nonblank (setting :endpoint)) +endpoint+))

(defun ask (tool arguments)
  "TOOL called on the endpoint, with ARGUMENTS, inside the section's timeout."
  (call-tool (endpoint) tool arguments :timeout (setting :timeout-seconds)))

(defun answer-text (head object)
  "HEAD, and the browser tool OBJECT links when it names one."
  (let ((link (link-line object)))
    (if link (format nil "~a~%~a" head link) head)))

(defun warnings-of (answer)
  "The warning lines ANSWER carries, as a list of strings."
  (let ((warnings (nlk:json-value answer :array "warnings")))
    (and warnings (coerce warnings 'list))))

(defun caution-lines (answer inside)
  "One line a warning, for what ANSWER and its INSIDE object caution about."
  (let ((warnings (append (warnings-of answer) (warnings-of inside))))
    (if warnings (format nil "~%~{~a~^~%~}" (mapcar (lambda (w) (format nil "note: ~a" w)) warnings)) "")))

;;; --- the verbs --------------------------------------------------------------------

(define-verb transpose (chords &key (semitones 0) prefer-flats)
  "Move every chord root in CHORDS SEMITONES semitones. CHORDS is a string of
chord symbols such as \"C G Am F\"; SEMITONES is the interval, negative to go
down; PREFER-FLATS spells the result with flats. => the chords transposed, as
lines."
  (let ((text (nonblank chords)))
    (unless text
      (fail "transpose needs chords: give some, e.g. (clef:transpose \"C G Am F\" :semitones 2)"))
    (unless (integerp semitones)
      (fail "semitones must be a whole number, e.g. 2 or -2, not ~s" semitones))
    (let ((direction (if (minusp semitones) "down" "up"))
          (spelling (if prefer-flats "flats" "sharps")))
      (if (setting :offline-transpose)
          (format nil "clef: ~a (~a ~d semitone~:p, ~a, in the cell)~%"
                  (shift-chords text semitones :prefer-flats prefer-flats)
                  direction (abs semitones) spelling)
          (let ((answer (ask "transpose_chords"
                             (nlk:json-object "chords" text
                                              "semitones" semitones
                                              "preferFlats" (and prefer-flats t)))))
            (answer-text
             (format nil "clef: ~a (~a ~d semitone~:p, ~a, from Clef's server)"
                     (or (nlk:json-value answer :string "chords") text)
                     direction (abs semitones) spelling)
             answer))))))

(define-verb spell-chord (symbol &key (octave 4))
  "Spell the chord SYMBOL as piano pitches from OCTAVE, 1 to 6. => the notes
with their MIDI values, as lines."
  (let ((symbol (nonblank symbol)))
    (unless symbol
      (fail "spell-chord needs a chord symbol, e.g. (clef:spell-chord \"Cmaj7\")"))
    (unless (and (integerp octave) (<= 1 octave 6))
      (fail "octave must be a whole number from 1 to 6, not ~s" octave))
    (let* ((answer (ask "spell_piano_chord"
                         (nlk:json-object "symbol" symbol "octave" octave)))
           (notes (loop for note across (or (nlk:json-value answer :array "notes") #())
                        for name = (nlk:json-value note :string "name")
                        for midi = (nlk:json-value note :integer "midi")
                        when name
                          collect (if midi (format nil "~a (midi ~d)" name midi) name))))
      (answer-text
       (format nil "clef: ~a from octave ~d = ~a"
               symbol octave
               (if notes (format nil "~{~a~^, ~}" notes)
                   "no pitches: Clef does not spell that symbol"))
       answer))))

(define-verb tab-notes (tab &key (capo 0))
  "Convert six lines of ASCII TAB to sounding piano pitches, with CAPO fret
0 to 12. => one line a column of notes, lowest first, and what the server
cautions."
  (let ((tab (nonblank tab)))
    (unless tab
      (fail "tab-notes needs six lines of ASCII tab, e.g. (clef:tab-notes e|-0-| ...)"))
    (unless (and (integerp capo) (<= 0 capo 12))
      (fail "capo must be a whole number from 0 to 12, not ~s" capo))
    (let* ((answer (ask "tab_to_piano_notes" (nlk:json-object "tab" tab "capo" capo)))
           (inside (or (nlk:json-value answer :object "result") answer))
           (events (or (nlk:json-value inside :array "events") #()))
           (lines (loop for event across events
                        for index from 1
                        for midis = (nlk:json-value event :array "midis")
                        collect (format nil "[~d] ~a"
                                        index
                                        (if midis
                                            (format nil "~{~a~^ ~}"
                                                    (loop for midi across midis
                                                          for name = (midi-name midi)
                                                          collect (if name
                                                                      (format nil "~a (~d)" name midi)
                                                                      (format nil "~d" midi))))
                                            "no notes")))))
      (answer-text
       (format nil "clef: ~d column~:p of notes, lowest first~%~a~a"
               (length lines)
               (if lines (format nil "~{~a~^~%~}" lines) "no fretted notes")
               (caution-lines answer inside))
       answer))))

(define-verb guides (query)
  "Find Clef's instructional guides by QUERY, words in a title or description.
=> one block a guide: its title, url and description, or the directory that
holds them all."
  (let ((query (nonblank query)))
    (unless query
      (fail "guides needs words to find them by, e.g. (clef:guides \"guitar\")"))
    (let* ((answer (ask "find_music_guides" (nlk:json-object "query" query)))
           (items (or (nlk:json-value answer :array "guides") #()))
           (directory (nlk:json-value answer :string "directory"))
           (blocks (loop for item across items
                         for index from 1
                         for description = (nlk:json-value item :string "description")
                         collect (format nil "[~d] ~a~%    ~a~@[~%    ~a~]"
                                         index
                                         (or (nlk:json-value item :string "title") "(untitled)")
                                         (or (nlk:json-value item :string "uri") "(no url)")
                                         (and description (plusp (length description)) description)))))
      (answer-text
       (if blocks
           (format nil "clef: ~d guide~:p for ~s~2%~{~a~^~2%~}" (length blocks) query blocks)
           (format nil "clef: no guides for ~s~@[ - the directory holds them all: ~a~]"
                   query (and directory (plusp (length directory)) directory)))
       answer))))

;;; --- the tools the model calls ----------------------------------------------------

(defun transpose-tool (request)
  "The transpose_chords tool: the members one call carries."
  (transpose (transpose-request-chords request)
             :semitones (transpose-request-semitones request)
             :prefer-flats (transpose-request-prefer-flats request)))

(defun spell-tool (request)
  "The spell_piano_chord tool."
  (spell-chord (spell-request-chord-symbol request)
                :octave (or (spell-request-octave request) 4)))

(defun tab-tool (request)
  "The tab_to_piano_notes tool."
  (tab-notes (tab-request-tab request)
             :capo (or (tab-request-capo request) 0)))

(defun guide-tool (request)
  "The find_music_guides tool."
  (guides (guide-request-query request)))

;;; --- the operator's own door ------------------------------------------------------

(defparameter +usage+
  "clef: type +2 C G Am F to transpose, chord Cmaj7 to spell one, or guides guitar to find Clef's guides. This cell also gives the model the find_music_guides, transpose_chords, spell_piano_chord and tab_to_piano_notes tools."
  "What /clef says with nothing usable typed after it.")

(defun number-text-p (text)
  "Does TEXT read as one whole number, signs allowed and nothing else?"
  (and (plusp (length text))
       (every (lambda (character)
                (or (char<= #\0 character #\9) (char= character #\+) (char= character #\-)))
              text)
       (ignore-errors (parse-integer text)) t))

(defun run-slash (args session-id)
  "/clef: transpose, spell or find, the way the tools do it."
  (declare (ignore session-id))
  (let ((text (nonblank args)))
    (if (null text)
        +usage+
        (let* ((space (position #\Space text))
               (verb (if space (subseq text 0 space) text))
               (rest (and space (nonblank (subseq text (1+ space)))))
               (steps (and (number-text-p verb) (parse-integer verb))))
          (cond
            (steps (if rest
                       (transpose rest :semitones steps)
                       (format nil "clef: tell me what to move, e.g. /clef ~a C G Am F" verb)))
            ((string-equal verb "transpose")
             (let* ((gap (and rest (position #\Space rest)))
                    (amount (and gap (number-text-p (subseq rest 0 gap))
                                 (parse-integer rest :end gap))))
               (if (and amount gap (nonblank (subseq rest (1+ gap))))
                   (transpose (subseq rest (1+ gap)) :semitones amount)
                   (format nil "clef: say it as /clef transpose 2 C G Am F, or /clef +2 C G Am F"))))
            ((string-equal verb "chord") (if rest (spell-chord rest) +usage+))
            ((string-equal verb "guides") (if rest (guides rest) +usage+))
            (t +usage+))))))
