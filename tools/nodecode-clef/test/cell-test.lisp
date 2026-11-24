;;;; cell-test.lisp --- lifecycle, the four tools, the wire, the refusals, /clef.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Clef: the config
;;;; surface (a section of the wrong shape refuses the start), the tools
;;;; through the real dispatch seam against a stubbed MCP server, that
;;;; transposition is answered without a call when the section says so and
;;;; through the server when it does not, the text each answer is rendered
;;;; into, the refusal a refused call gets, and the command's three verbs.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "clef"
  (:config)
  (:help :clef)
  (:command "clef")
  (:running
   (is (nle::find-tool "transpose_chords") "transpose_chords is registered")
   (is (nle::find-tool "spell_piano_chord") "spell_piano_chord is registered")
   (is (nle::find-tool "tab_to_piano_notes") "tab_to_piano_notes is registered")
   (is (nle::find-tool "find_music_guides") "find_music_guides is registered")
   (is (equal "https://clefdrills.com/mcp" (nodecode-clef::setting :endpoint)) "the endpoint the plugin's mcp.json named")
   (is (eq t (nodecode-clef::setting :offline-transpose)) "transposition is answered in the cell unless the section says otherwise"))
  (:stopped
   (is (null (nle::find-tool "transpose_chords")) "stop unregisters the tools")
   (is (null (nle::find-tool "find_music_guides")) "every one of them"))
  (:refused
   ("timeout_seconds" 0)))

;;; --- transposition, in the cell ----------------------------------------------------

(deftest clef-cell-transposition-is-answered-in-the-cell (with-clef ()
  (let ((text (clef-call-tool "transpose_chords" "{\"chords\":\"C G Am F#m7b5/Bb\",\"semitones\":2}")))
    (is (cl:search "D A Bm G#m7b5/C" text) "every root moved two semitones up, spelled with sharps")
    (is (null *calls*) "with no request made at all"))
  (let ((text (nodecode-clef::shift-chords "C G Am F#m7b5/Bb" -2 :prefer-flats t)))
    (is (equal "Bb F Gm Em7b5/Ab" text) "down two, flats on request"))
  (is (equal "Cabaret in D#" (nodecode-clef::shift-chords "Cabaret in C" 3)) "a word that starts on a letter stays a word; the root after it still moves")))

;;; --- transposition, through the server ---------------------------------------------

(deftest clef-cell-transposition-can-ask-the-server (with-clef ("offline_transpose" nil)
  (let ((text (clef-call-tool "transpose_chords" "{\"chords\":\"C G Am F#m7b5/Bb\",\"semitones\":2}")))
    (is (cl:search "D A Bm G#m7b5/C" text) "the server's own answer")
    (is (cl:search "from Clef's server" text) "says where it came from")
    (is (cl:search "https://clefdrills.com/tools/chord-transposer/" text) "and links the browser tool"))
  (let ((call (first *calls*)))
    (is (= 1 (length *calls*)) "one request, not one a chord")
    (is (equal "https://clefdrills.com/mcp" (getf call :url)) "to the endpoint the plugin named")
    (is (equal "transpose_chords" (call-tool-name call)) "the tool the catalog lists")
    (is (equal "C G Am F#m7b5/Bb" (call-argument call "chords")) "with the chords the call carried")
    (is (= 2 (call-argument call "semitones")) "and its interval")
    (is (cl:search "application/json" (call-header call "content-type")) "posted as JSON-RPC"))))

;;; --- the other three tools ----------------------------------------------------------

(deftest clef-cell-a-spelled-chord-arrives-with-its-midi-values (with-clef ()
  (let ((text (clef-call-tool "spell_piano_chord" "{\"symbol\":\"Cmaj7\"}")))
    (is (cl:search "C4 (midi 60)" text) "each pitch named, with its MIDI value")
    (is (cl:search "B4 (midi 71)" text) "through the last one")
    (is (cl:search "piano-chord-finder" text) "and the browser tool it links"))
  (is (= 4 (call-argument (first *calls*) "octave")) "a call naming no octave asks for the fourth")))

(deftest clef-cell-tab-columns-arrive-lowest-first-with-the-server-s-caution (with-clef ()
  (let ((text (clef-call-tool "tab_to_piano_notes" "{\"tab\":\"e|-0-|\"}")))
    (is (cl:search "[1] C3 (48) E3 (52) G3 (55) C4 (60) E4 (64)" text) "the first column, its midis named and ordered")
    (is (cl:search "[2] B2 (47) D3 (50) G3 (55) B3 (59) G4 (67)" text) "the second too")
    (is (cl:search "note: Columns show order, not rhythm" text) "and the server's first caution")
    (is (cl:search "Standard six-string guitar tuning only" text) "and its second"))
  (is (= 0 (call-argument (first *calls*) "capo")) "a call naming no capo asks for none")))

(deftest clef-cell-guides-answer-blocks-the-model-can-cite (with-clef ()
  (let ((text (clef-call-tool "find_music_guides" "{\"query\":\"guitar\"}")))
    (is (cl:search "[1] Tabs to Piano Sheet Music" text) "the first guide's title")
    (is (cl:search "https://clefdrills.com/blog/tab-files-to-piano-sheet-music.html" text) "its url")
    (is (cl:search "[2] Guitar, Bass and Ukulele Tabs to Piano" text) "and the second"))
  (is (equal "guitar" (call-argument (first *calls*) "query")) "the words the call carried")))

(deftest clef-cell-no-guides-names-the-directory (with-clef ()
  (setf *answer* (lambda (call)
                   (declare (ignore call))
                   (values (mcp-answer (nlk:json-object
                                        "guides" (vector)
                                        "directory" "https://clefdrills.com/blog/music-conversion-workflow-finder.html"))
                           200)))
  (let ((text (clef-call-tool "find_music_guides" "{\"query\":\"scales\"}")))
    (is (cl:search "no guides for \"scales\"" text) "an empty answer says so")
    (is (cl:search "music-conversion-workflow-finder" text) "and points at the directory"))))

;;; --- what the server refuses --------------------------------------------------------

(deftest clef-cell-a-refused-call-carries-the-server-s-own-sentence (with-clef ()
  (setf *answer* (lambda (call)
                   (declare (ignore call))
                   (values (mcp-refusal "Line 6: unsupported tablature technique") 200)))
  (let ((condition (signals-error nodecode-clef:clef-error
                                  (nodecode-clef:tab-notes "E|-x-|"))))
    (is (cl:search "unsupported tablature technique" (princ-to-string condition)) "the server's sentence, not a status code alone"))))

(deftest clef-cell-a-call-with-no-words-never-reaches-the-wire (with-clef ()
  (let ((condition (signals-error nodecode-clef:clef-error (nodecode-clef:guides "  "))))
    (is (cl:search "needs words" (princ-to-string condition))))
  (is (null *calls*) "and nothing was dialled")))

(deftest clef-cell-a-failed-exchange-is-a-refusal-not-a-crash (with-clef ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values "upstream down" 503)))
  (let ((condition (signals-error nodecode-clef:clef-error
                                  (nodecode-clef:guides "guitar"))))
    (is (cl:search "HTTP 503" (princ-to-string condition)) "the status is named")
    (is (cl:search "upstream down" (princ-to-string condition)) "and what came back"))))

;;; --- the command -------------------------------------------------------------------

(deftest clef-cell-the-command-transposes-spells-and-finds (with-clef ()
  (is (cl:search "transpose_chords"
                 (cell-entry "nodecode-clef" "clef" "" "clef-cell-1")) "bare, it names what it gives")
  (is (cl:search "D A Bm" (cell-entry "nodecode-clef" "clef" "+2 C G Am" "clef-cell-1")) "+2 transposes")
  (is (cl:search "D A Bm" (cell-entry "nodecode-clef" "clef" "transpose 2 C G Am" "clef-cell-1")) "so does the spelled-out verb")
  (is (cl:search "C4 (midi 60)" (cell-entry "nodecode-clef" "clef" "chord Cmaj7" "clef-cell-1")) "chord spells one")
  (is (cl:search "Tabs to Piano Sheet Music" (cell-entry "nodecode-clef" "clef" "guides guitar" "clef-cell-1")) "guides finds them")))
