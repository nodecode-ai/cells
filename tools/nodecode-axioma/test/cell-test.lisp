;;;; cell-test.lisp --- the renderer, the card, the observer and /axioma, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials anything: the renderer is a function of text and
;;;; settings, a card is a record the cell makes from what the model wrote,
;;;; and the :TOOL observer is called with the op a dispatch builds. What is
;;;; proved is the promise the original made: colour never alone, a malformed
;;;; tag never shown, green only behind a call the host reported successful,
;;;; a budget, and a code that says whether what was staged was what ran.

(in-package #:nodecode.test)

(define-test-slice "axioma" "AXIOMA-CELL-" :start nodecode-axioma:start-cell)

;;; --- the lifecycle every cell owes ------------------------------------------

(define-cell-lifecycle-tests "axioma"
  (:hooks :tool)
  (:help :axioma)
  (:command "axioma")
  (:running (is (equal "default" (nodecode-axioma::setting :palette)) "the default palette")
            (is (= 3 (nodecode-axioma::setting :max-spans)) "a reply may carry three marks")
            (is (equal '() (nodecode-axioma::setting :risky-tools)) "no tool is risky until one is named"))
  (:refused ("max_spans" 0) ("palette" "neon")))

(defun axiom-marked (text &key ansi)
  (nodecode-axioma::render text :ansi ansi))

;;; --- the renderer ------------------------------------------------------------

(deftest axioma-cell-a-reply-with-no-mark-comes-back-as-written ()
  (with-cell-stop ((axioma-start))
    (let ((plain "the build is green and nothing here is marked"))
      (is (string= plain (axiom-marked plain)) "no tag, no change")
      (is (string= "array[signal] and samples[signal_1]" (axiom-marked "array[signal] and samples[signal_1]")) "a lookalike with no closer is left alone"))))

(deftest axioma-cell-a-mark-renders-colour-cue-and-word ()
  (with-cell-stop ((axioma-start))
    (let ((coloured (axiom-marked "run it [signal:stop]without a test[/signal] first" :ansi t))
          (markdown (axiom-marked "merge [signal:caution]into main[/signal] now")))
      (is (search "without a test" coloured) "the words are kept")
      (is (search (format nil "~c[38;2;255;69;58m" #\Esc) coloured) "the stop colour, as truecolor ANSI")
      (is (search "(stop)" coloured) "and the word, so the colour is never alone")
      (is (not (search "[/signal]" coloured)) "no tag survives")
      (is (search "**into main**" markdown) "no ANSI on a plain surface: the cue is markdown")
      (is (search "(caution)" markdown) "and the word is still there")))
  (with-cell-stop ((axioma-start "words" "plain_only"))
    (is (search "(stop)" (axiom-marked "[signal:stop]x[/signal]")) "plain_only still says the word where there is no colour")))

(deftest axioma-cell-green-rests-on-a-call-the-host-reported-ok ()
  (with-cell-stop ((axioma-start))
    (let ((text "[signal:go;check=c-9]114 of 114 passed[/signal]"))
      (is (not (search "_114" (axiom-marked text))) "no such call yet: the mark renders plain")
      (is (search "(go)" (axiom-marked text)) "and still says its word")
      (nodecode-axioma::note-result (list :name "eval" :call-id "c-9") "done")
      (is (search "_114 of 114 passed_" (axiom-marked text)) "the call ran ok: now it is styled"))))

(deftest axioma-cell-a-malformed-tag-never-reaches-the-reader ()
  (with-cell-stop ((axioma-start))
    (let ((rendered (axiom-marked "[signal:danger]drop the table[/signal] and [signal:stop]wait[/signal]")))
      (is (search "drop the table" rendered) "the words stay")
      (is (not (search "[signal" rendered)) "no raw tag, in a reply that does carry a mark")
      (is (search "(stop)" rendered) "the real mark still renders"))))

(deftest axioma-cell-the-budget-caps-marks-and-whispers ()
  (with-cell-stop ((axioma-start "max_spans" 1))
    (let ((rendered (axiom-marked "[signal:stop]one[/signal] [signal:caution]two[/signal]")))
      (is (search "**one**" rendered) "the first mark is rendered")
      (is (search "two (caution)" rendered) "past the budget, plain with its word"))))

;;; --- the card ------------------------------------------------------------------

(defun axiom-card ()
  (nodecode-axioma::decode-axioma-card
   (nlk:json-object "tool" "eval"
                     "arguments" (nlk:json-object "form" "(sh \"rm -rf build\")")
                     "light" "yellow"
                     "intent" "clear the build folder"
                     "signal" "a delete outside the workspace"
                     "question" "is build/ the one you mean?")))

(deftest axioma-cell-a-card-answers-its-code-and-verifies ()
  (with-cell-stop ((axioma-start))
    (let* ((text (nodecode-axioma::stage-card (axiom-card)))
           (code (string-trim '(#\Space) (subseq text (+ (search "card code " text) 10)))))
      (is (search "YELLOW" text) "the light is on the card")
      (is (search "is build/ the one you mean?" text) "and the question")
      (is (= 9 (length code)) "the code is XXXX-XXXX")
      (is (search "staged and not shown" (nodecode-axioma::run-slash (format nil "verify ~a" code) nil)) "verify says the call has not run yet")
      (is (search code (nodecode-axioma::run-slash "cards" nil)) "/axioma cards lists it")
      (nodecode-axioma::note-shown
       "eval" (nodecode-axioma::args-digest
               (nodecode-axioma::axioma-card-arguments (axiom-card))))
      (is (search "OK: card card-0 was staged and the eval call it names ran"
                  (nodecode-axioma::run-slash (format nil "verify ~a" code) nil)) "and once the same call runs, the log lines up")
      (is (search "MISMATCH" (nodecode-axioma::run-slash "verify 0000-0000" nil)) "an unknown code is refused in words"))))

;;; --- the observer --------------------------------------------------------------

(deftest axioma-cell-the-observer-runs-the-call-and-records-it ()
  (with-cell-stop ((axioma-start))
    (let ((ran nil))
      (is (equal "fine" (nodecode-axioma::observe-tool
                         (list :name "eval" :call-id "c-1" :arguments (nlk:json-object))
                         (lambda (op) (declare (ignore op)) (setf ran t) "fine"))) "the call's own answer comes back")
      (is ran "and the call ran")
      (is (nodecode-axioma::call-ok-p "c-1") "a passing call backs a later green")
      (nodecode-axioma::observe-tool
       (list :name "eval" :call-id "c-2" :arguments (nlk:json-object))
       (lambda (op) (declare (ignore op)) "ERROR: bad form"))
      (is (not (nodecode-axioma::call-ok-p "c-2")) "a failed call backs nothing"))))

(deftest axioma-cell-a-risky-call-with-no-card-is-noticed-never-blocked ()
  (with-cell-stop ((axioma-start "risky_tools" (vector "eval")))
    (is (equal "fine" (nodecode-axioma::observe-tool
                        (list :name "eval" :call-id "c-3" :arguments (nlk:json-object "a" "b"))
                        (lambda (op) (declare (ignore op)) "fine"))) "the call still runs")
    (let ((notice (cell-notice "nodecode-axioma")))
      (is notice "a standing notice says so")
      (is (search "no advisory card" (second notice)) "in words"))))

;;; --- the command ---------------------------------------------------------------

(deftest axioma-cell-the-command-lists-what-was-recorded ()
  (with-cell-stop ((axioma-start))
    (nodecode-axioma::stage-card (axiom-card))
    (let ((listing (cell-entry "nodecode-axioma" "axioma" "" "axioma-cell-status")))
      (is (search "staged" listing) "the staged card is listed")
      (is (search "eval" listing) "with the tool it names")
      (is (search "is no verb" (nodecode-axioma::run-slash "bogus" nil)) "an unknown verb is refused"))
    (is (search "render what?" (nodecode-axioma::run-slash "render" nil)) "render needs text")
    (is (search "(stop)" (nodecode-axioma::run-slash "render [signal:stop]wait[/signal]" nil)) "and renders the marked text")))
