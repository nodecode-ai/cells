;;;; cell.lisp --- signal marks in a reply, and an advisory card before a call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of percyhendrux123-prog/hermes-axioma (MIT). The
;;;; plugin colored a few phrases of the agent's own reply and, before a
;;;; send/spend/scrub tool call, staged an advisory card. This cell keeps
;;;; both: the model writes [signal:stop]...[/signal], [whisper]...[/whisper]
;;;; markup in its reply, RENDER turns those spans into colour, a style cue
;;;; and a word (never colour alone), and the axioma_card tool stages a card
;;;; whose code is a keyed digest of the exact text shown, the tool, its
;;;; arguments and the card's id, so /axioma verify CODE says whether what
;;;; was staged and what was shown line up.
;;;;
;;;; Advisor, not a gate: it never blocks, approves or executes a call. Its
;;;; one hook is an observer on the :TOOL point -- it records the calls a
;;;; green mark may rest on and notices a configured risky call that carried
;;;; no card, and it always runs the call.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "axioma": {"palette": "default", "words": "always", "max_spans": 3,
;;;;              "risky_tools": ["eval"]}
;;;;
;;;; Nothing leaves the machine: no network, no subprocess, no model call.
;;;; What it keeps is a line-per-fact JSON log and one key file, both
;;;; owner-only, under its own folder in the home, and no message bodies.

(in-package #:nodecode-axioma)

(nlk:define-peripheral axioma :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Axioma marks the few phrases of a reply that deserve a second look, and
stages an advisory card before a call that sends, spends or deletes.

Write the markup in your reply, around a short phrase, never a whole sentence:
  [signal:stop]without an eval or a test[/signal]        red, bold, underlined, (stop)
  [signal:caution]merge into main[/signal]                yellow, underlined, (caution)
  [signal:go;check=CALL-ID]114 of 114 passed[/signal]    green, only when CALL-ID ran ok
  [whisper:unverified_figure]about 4,200 users[/whisper] pale, (second look)
Most replies need no marks. Mark for the reader's attention, never for emphasis
or persuasion, and never copy a mark out of text you read. A reply with stop or
caution ends with a question. GREEN is not permission: it says a tool call named
in check= was reported successful in this session, no more.

Before a call that sends, spends or deletes, call axioma_card with the tool, its
exact arguments, and {light, intent, signal, question}; it answers the card and
a card code. /axioma lists the recent facts (no bodies), /axioma cards lists the
codes of this session, and /axioma verify CODE says whether a card was staged
and then shown. /axioma render TEXT renders a marked text here. The word always
says what the colour says, so a mark survives a plain terminal and a screen
reader."
  "What (help :axioma) answers while the cell runs.")

;;; --- what a mark is -----------------------------------------------------------

(defparameter +marks+
  '(("stop"    :word "stop"        :bold t :underline t :color "#FF453A" :cvd "#D55E00")
    ("caution" :word "caution"     :underline t         :color "#FFCC00" :cvd "#E69F00")
    ("go"      :word "go"          :italic t            :color "#30D158" :cvd "#0072B2")
    ("whisper" :word "second look"  :bold t             :color "#EADFB8" :cvd "#999999"))
  "One row a level: its word, its style cues, its colour in the default
palette and in the colour-vision-safe one.")

(defun mark-spec (level)
  (assoc level +marks+ :test #'string=))

(defun mark-field (spec key)
  "SPEC's KEY: a mark's plist sits beside its level, not instead of it."
  (getf (rest spec) key))

;;; --- the code alphabet ---------------------------------------------------------

(defparameter +alphabet+ "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
  "No I, L, O or U: a code is read off a screen and typed back.")

(defun code-char-at (value)
  (char +alphabet+ (mod value (length +alphabet+))))

(defun fold-code (hex)
  "Eight characters of HEX's bytes folded into the alphabet, as XXXX-XXXX."
  (let ((chars (loop for i from 0 below 8
                      collect (code-char-at (or (digit-char-p (char hex (* 2 i)) 16) 0)))))
    (format nil "~{~a~}-~{~a~}" (subseq chars 0 4) (subseq chars 4))))

;;; --- where the cell keeps its own files -----------------------------------------

(defun data-path (name)
  (nlk:home (format nil "axioma/~a" name)))

(defun keep-private (path)
  "PATH as the atomic writer left it: 0600, its folder 0700."
  path)

(defun card-key ()
  "The per-home key the card codes are made with: 0600, created once."
  (let ((path (data-path "card-code.key")))
    (unless (probe-file path)
      (nlk:write-file-atomically path (format nil "~a~%" (nlk:make-durable-id "axioma"))
                                  :mode #o600 :directory-mode #o700))
    (keep-private path)
    (string-trim '(#\Space #\Tab #\Newline #\Return)
                 (ignore-errors (uiop:read-file-string path)))))

(defun card-code (card-id tool digest text-digest)
  "A code for exactly this card: a keyed digest of its id, its tool, its
arguments and the text that was shown. => XXXX-XXXX."
  (fold-code (nlk:sha256-text
              (format nil "~a|~a|~a|~a|~a"
                      (card-key) card-id tool digest text-digest))))

;;; --- the log: one fact a line, no bodies ---------------------------------------

(defparameter +log-fields+ '("event" "at" "card" "code" "level" "tool" "shown" "digest" "text")
  "The only members a logged line carries. A phrase is never one of them: a
card is logged as its tool, its light, its digests and its code.")

(defun log-path () (data-path "events.jsonl"))

(defun log-rows ()
  "Every logged line as a JSON object, oldest first."
  (let ((path (log-path)))
    (if (not (probe-file path))
        '()
        (loop for line in (nlk:lines (ignore-errors (uiop:read-file-string path)))
              for text = (string-trim '(#\Space #\Return) line)
              when (plusp (length text))
                collect (or (ignore-errors (nlk:decode-json text))
                            (make-hash-table :test #'equal))))))

(defun json-scalar (value)
  "VALUE as one JSON member value."
  (cond ((eq value t) "true")
        ((null value) "false")
        ((integerp value) (princ-to-string value))
        (t (format nil "~s" (princ-to-string value)))))

(defun log-append (pairs)
  "Append one line built from PAIRS, a plist of member to value."
  (let* ((row (format nil "{~{~a~^,~}}"
                       (loop for (key value) on pairs by #'cddr
                             for name = (string-downcase (string key))
                             when (member name +log-fields+ :test #'string=)
                               collect (format nil "~s:~a" name (json-scalar value)))))
         (path (log-path))
         (old (if (probe-file path) (uiop:read-file-string path) "")))
    (ensure-directories-exist path)
    (nlk:write-file-atomically path (concatenate 'string old row (string #\Newline))
                                :mode #o600 :directory-mode #o700)
    (keep-private path)
    row))

(defun log-field (row name) (gethash name row))

(defun (setf log-field) (value row name) (setf (gethash name row) value))

;;; --- the calls a green mark may rest on ----------------------------------------

(defparameter *calls* (make-hash-table :test #'equal)
  "Call ids and tool names the host reported successful in this session.")

(defun failure-text-p (text)
  (and (stringp text) (>= (length text) 6) (string= "ERROR:" text :end2 6)))

(defun note-result (op result)
  "Remember OP's call as successful when RESULT is what a passing call
answers: a string that is not a failure."
  (unless (failure-text-p result)
    (let ((id (getf op :call-id)) (name (getf op :name)))
      (when (stringp id) (setf (gethash id *calls*) t))
      (when (stringp name) (setf (gethash name *calls*) t)))))

(defun call-ok-p (check)
  (and check (gethash check *calls*)))

(defun args-digest (arguments)
  "A digest of the call's arguments, each member by name, in a stable order."
  (let ((parts '()))
    (when (hash-table-p arguments)
      (maphash (lambda (key value)
                 (push (format nil "~a=~a" key (princ-to-string value)) parts))
               arguments))
    (fold-code (nlk:sha256-text (format nil "~{~a~&~}" (sort parts #'string<))))))

;;; --- the renderer ---------------------------------------------------------------

(defun tag-shaped-p (text start)
  "TEXT at START begins something shaped like an axiom tag."
  (flet ((at (needle)
           (and (<= (+ start (length needle)) (length text))
                (string= needle text :start2 start :end2 (+ start (length needle))))))
    (or (at "[signal") (at "[whisper") (at "[/signal") (at "[/whisper"))))

(defun tag-end (text start)
  "The position after the tag at START: past its ], or past the line when a
cut-off reply left none."
  (let ((close (position #\] text :start start)))
    (or (and close (1+ close))
        (position #\Newline text :start start)
        (length text))))

(defun param-check (params)
  "The check= member of a tag's PARAMS, or NIL."
  (loop for part in (uiop:split-string params :separator ";")
        for eq = (position #\= part)
        when (and eq (string= "check" (string-trim '(#\Space) (subseq part 0 eq))))
          return (string-trim '(#\Space) (subseq part (1+ eq)))))

(defun opener-at (text start)
  "TEXT at START begins a complete opener => (values KIND LEVEL CHECK END)."
  (let ((close (position #\] text :start start)))
    (when close
      (let ((inner (subseq text (1+ start) close)))
        (cond
          ((and (> (length inner) 6) (string= "signal" inner :end2 6)
                (char= (char inner 6) #\:))
           (let* ((rest (subseq inner 7))
                  (semi (position #\; rest))
                  (level (if semi (subseq rest 0 semi) rest))
                  (params (if semi (subseq rest (1+ semi)) "")))
             (when (mark-spec level)
               (values "signal" level (param-check params) (1+ close)))))
          ((and (>= (length inner) 7) (string= "whisper" inner :end2 7)
                (or (= (length inner) 7) (char= (char inner 7) #\:) (char= (char inner 7) #\;)))
           (let ((semi (position #\; inner)))
             (values "whisper" "whisper" (and semi (param-check (subseq inner (1+ semi))))
                     (1+ close))))
          (t nil))))))

(defun closer-at (text start)
  "TEXT at START begins a closer => (values KIND END)."
  (flet ((at (needle)
           (and (<= (+ start (length needle)) (length text))
                (string= needle text :start2 start :end2 (+ start (length needle))))))
    (cond ((at "[/signal]") (values "signal" (+ start 9)))
          ((at "[/whisper]") (values "whisper" (+ start 10)))
          (t nil))))

(defun live-mark-p (text)
  "Whether TEXT carries one complete mark: an opener with a closer after it."
  (loop with i = 0 and n = (length text)
        while (< i n)
        do (if (char= (char text i) #\[)
               (multiple-value-bind (kind level check end) (opener-at text i)
                 (declare (ignore level check))
                 (if (and kind (search (format nil "[/~a]" kind) text :start2 end))
                     (return t)
                     (incf i)))
               (incf i))
        finally (return nil)))

(defun colour-of (spec)
  (if (string= (setting :palette) "cvd") (mark-field spec :cvd) (mark-field spec :color)))

(defun ansi-span (body spec)
  "BODY in SPEC's colour and styles, ending with the styles it set turned off
and the default foreground back."
  (let* ((hex (colour-of spec))
         (r (parse-integer hex :start 1 :end 3 :radix 16))
         (g (parse-integer hex :start 3 :end 5 :radix 16))
         (b (parse-integer hex :start 5 :end 7 :radix 16))
         (open (format nil "~c[38;2;~d;~d;~dm~@[~c[1m~]~@[~c[4m~]~@[~c[3m~]"
                       #\Esc r g b
                       (and (mark-field spec :bold) #\Esc)
                       (and (mark-field spec :underline) #\Esc)
                       (and (mark-field spec :italic) #\Esc)))
         (close (concatenate 'string
                             (and (mark-field spec :bold) (format nil "~c[22m" #\Esc))
                             (and (mark-field spec :underline) (format nil "~c[24m" #\Esc))
                             (and (mark-field spec :italic) (format nil "~c[23m" #\Esc))
                             (format nil "~c[39m" #\Esc))))
    (concatenate 'string open body close)))

(defun markdown-span (body spec)
  "BODY with the strongest cue markdown has: bold, italic, else plain. A
markdown chat has no underline, so an underlined level is bold there."
  (cond ((or (mark-field spec :bold) (mark-field spec :underline)) (format nil "**~a**" body))
        ((mark-field spec :italic) (format nil "_~a_" body))
        (t body)))

(defun word-count (text)
  (1+ (count #\Space text)))

(defun span-text (body level check index ansi)
  "BODY rendered as LEVEL: its colour and cue where ANSI, its cue where not,
then its word. Over the budget, too long, or a go mark with no successful
call behind it renders plain, still with its word."
  (let* ((spec (mark-spec level))
         (gated (and (string= level "go") (not (call-ok-p check))))
         (over (or (> index (setting :max-spans))
                   (> (word-count body) (setting :max-span-words))
                   (and (string= level "whisper") (> index (setting :max-whispers)))))
         (plain (or gated over))
         (coloured (and (not plain) ansi (not (string= (setting :palette) "none"))))
         (styled (and (not plain) (or ansi (not (string= (setting :palette) "none")))))
         (shown (cond ((not styled) body)
                      (ansi (ansi-span body spec))
                      (t (markdown-span body spec))))
         (word (and (or (string= (setting :words) "always") (not coloured))
                    (mark-field spec :word))))
    (format nil "~a~@[ (~a)~]" shown word)))

(defun render (text &key ansi)
  "TEXT's signal marks rendered. A reply with no complete mark comes back
exactly as written; in one that carries a mark, a tag the model malformed is
removed, never shown."
  (let ((text (or text "")))
    (if (not (live-mark-p text))
        text
        (with-output-to-string (out)
          (let ((i 0) (index 0) (n (length text)))
            (loop while (< i n)
                  do (let ((c (char text i)))
                       (if (not (char= c #\[))
                           (progn (write-char c out) (incf i))
                           (multiple-value-bind (kind level check end) (opener-at text i)
                             (cond
                               (kind
                                (let ((close (search (format nil "[/~a]" kind) text :start2 end)))
                                  (if close
                                      (progn
                                        (incf index)
                                        (write-string (span-text (subseq text end close)
                                                                  level check index ansi)
                                                      out)
                                        (setf i (+ close (length kind) 3)))
                                      (setf i end))))
                               (t
                                (multiple-value-bind (ckind cend) (closer-at text i)
                                  (cond (ckind (setf i cend))
                                        ((tag-shaped-p text i) (setf i (tag-end text i)))
                                        (t (write-char c out) (incf i)))))))))))))))

;;; --- the card a call is staged with ---------------------------------------------

(nlk:define-contract axioma-card
  (:doc "Stage an advisory card for one exact upcoming tool call: the tool, its
arguments, the light you judge, the sentence that says why, and the question to
ask. Advice only -- it approves, blocks and executes nothing.")
  ("tool"      :text :required t :doc "the tool about to be called, as the host names it")
  ("arguments" :object :doc "the exact arguments of that call")
  ("light"     :text :required t :options '("red" "yellow" "green")
               :doc "red: do not proceed; yellow: works, worth a look; green: a checked call backs it")
  ("intent"    :text :required t :doc "what the call is trying to do, in one phrase")
  ("signal"    :text :required t :slot signal-text :doc "one sentence: why this light")
  ("question"  :text :required t :doc "one question for the person, ending in ?")
  ("affects"   :array :of :string :doc "who else sees or feels this")
  ("told"      :boolean :default t :doc "whether they have been told"))

(defparameter *cards* (make-hash-table :test #'equal)
  "The codes staged this session, each to its row.")

(defun card-body (card)
  "CARD as the words a person reads, without its code line."
  (let ((affects (coerce (axioma-card-affects card) 'list)))
    (format nil "~a  ~a~%intent: ~a~%signal: ~a~%question: ~a~@[~%affects: ~{~a~^, ~}~]~@[~%untold~]~%~
                 advice only; the card is not consent"
            (string-upcase (axioma-card-light card))
            (string-upcase (axioma-card-tool card))
            (axioma-card-intent card)
            (axioma-card-signal-text card)
            (axioma-card-question card)
            affects
            (not (axioma-card-told card)))))

(defun stage-card (card)
  "Stage CARD: log it, and answer the card with the code made from its exact
text, its tool, its arguments and its id."
  (let* ((id (format nil "card-~a" (length (log-rows))))
         (tool (axioma-card-tool card))
         (digest (args-digest (axioma-card-arguments card)))
         (body (card-body card))
         (text-digest (nlk:sha256-text body))
         (code (card-code id tool digest text-digest))
         (row (make-hash-table :test #'equal)))
    (setf (log-field row "card") id (log-field row "tool") tool
          (log-field row "level") (string-downcase (axioma-card-light card))
          (log-field row "digest") digest (log-field row "text") text-digest
          (log-field row "code") code (log-field row "shown") nil)
    (setf (gethash code *cards*) row)
    (log-append (list :event "staged" :card id :code code :tool tool
                      :level (string-downcase (axioma-card-light card)) :digest digest))
    (format nil "~a~%card code ~a" body code)))

;;; --- the :tool observer ---------------------------------------------------------

(defun risky-p (name)
  (and (member name (setting :risky-tools) :test #'string=) t))

(defun staged-for (name digest)
  "The newest staged card for a call of NAME with DIGEST's arguments, or NIL."
  (loop for code being the hash-keys of *cards* using (hash-value row)
        when (and (equal name (log-field row "tool")) (equal digest (log-field row "digest")))
          return (cons code row)))

(defun note-shown (name digest)
  "Mark a staged card shown when a call of NAME with DIGEST's arguments runs;
a configured risky call with no card says so, and is never blocked."
  (let ((staged (staged-for name digest)))
    (cond
      (staged
       (setf (log-field (cdr staged) "shown") t)
       (log-append (list :event "shown" :card (log-field (cdr staged) "card")
                         :code (car staged) :tool name :digest digest)))
      ((risky-p name)
       (log-append (list :event "uncarded" :tool name :digest digest))
       (nle:notice (format nil "axioma: ~a ran with no advisory card (axioma_card stages one)" name)
                   :level :warning :key "nodecode-axioma")))))

(defun observe-tool (op next)
  "One tool call, observed and never changed: OP is (:name TOOL-NAME
:arguments HASH :call-id ID), and NEXT runs the call."
  (let ((result (funcall next op)))
    (handler-case
        (progn (note-result op result)
               (when (setting :cards)
                 (note-shown (princ-to-string (or (getf op :name) ""))
                             (args-digest (getf op :arguments)))))
      (error (condition) (warn "axioma: ~a" condition)))
    result))

;;; --- /axioma --------------------------------------------------------------------

(defun fact-line (row)
  (let ((code (or (log-field row "code") "")))
    (format nil "  ~a  ~a  ~a~@[  ~a~]"
            (or (log-field row "event") "?")
            (or (log-field row "tool") "-")
            (or (log-field row "level") "-")
            (and (plusp (length code)) code))))

(defun recent-lines (count)
  (let* ((rows (reverse (log-rows)))
         (shown (subseq rows 0 (min count (length rows)))))
    (if (null rows)
        "axioma: nothing marked or carded in this home yet"
        (format nil "axioma: ~d fact~:p recorded, newest ~d:~%~{~a~}"
                (length rows) (length shown) (mapcar #'fact-line shown)))))

(defun card-lines ()
  (let ((codes (sort (loop for code being the hash-keys of *cards* collect code) #'string<)))
    (if (null codes)
        "axioma: no card staged this session"
        (format nil "axioma: ~d card~:p staged this session:~%~{~a~}"
                (length codes) (mapcar (lambda (code) (format nil "  ~a" code)) codes)))))

(defun verify (typed)
  "Whether the card TYPED names was staged and then shown, in plain words."
  (let* ((code (string-trim '(#\Space)
                             (if (and (>= (length typed) 10)
                                      (string= "card code" typed :end2 9))
                                 (subseq typed 10)
                                 typed)))
         (row (gethash code *cards*)))
    (cond
      ((null row)
       (format nil "MISMATCH: no card of this session carries ~a; /axioma cards lists the codes this session staged"
               code))
      ((not (string= code (card-code (log-field row "card") (log-field row "tool")
                                     (log-field row "digest") (log-field row "text"))))
       (format nil "MISMATCH: the record of card ~a changed after it was written"
               (log-field row "card")))
      ((log-field row "shown")
       (format nil "OK: card ~a was staged and the ~a call it names ran. This confirms the log, ~
                    not the screen: compare this code with the one on the card you saw"
               (log-field row "card") (log-field row "tool")))
      (t (format nil "OK so far: card ~a was staged and not shown yet (no ~a call of those ~
                      arguments since); the log was not changed after writing"
                 (log-field row "card") (log-field row "tool"))))))

(defun run-slash (args session-id)
  "Typed alone, the recent facts; the verbs cards, verify CODE and render TEXT."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (c) (member c '(#\Space #\Tab))) text))
         (head (string-downcase (if space (subseq text 0 space) text)))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond ((string= text "") (recent-lines 10))
          ((string= head "cards") (card-lines))
          ((string= head "verify")
           (if (zerop (length rest))
               "axioma: verify what? /axioma verify CODE, with a code from /axioma cards"
               (verify rest)))
          ((string= head "render") (if (zerop (length rest)) "axioma: render what?" (render rest)))
          (t (format nil "axioma: ~a is no verb; /axioma, /axioma cards, /axioma verify CODE, ~
                          /axioma render TEXT" head)))))

;;; --- the declaration -------------------------------------------------------------

(nle:define-cell axioma
  (:section ("axioma")
    (:guide "a mark is markup the model writes and this cell renders; the budget caps how many a reply may carry. risky_tools names the tools that deserve an advisory card before they run")
    ("palette" :choice :options '("default" "cvd" "none") :default "default"
               :doc "default traffic colours, a colour-vision-safe set, or none (style and words only)")
    ("words" :choice :options '("always" "plain_only") :default "always"
             :doc "always puts the word after every mark; plain_only only where colour is unavailable")
    ("max_spans" :integer :default 3 :min 1
                 :doc "stop, caution and go marks a reply may carry; the rest render plain")
    ("max_whispers" :integer :default 2 :min 1 :doc "whisper marks a reply may carry")
    ("max_span_words" :integer :default 16 :min 1 :doc "the longest marked phrase, in words")
    ("cards" :boolean :default t
             :doc "whether the axioma_card tool and the uncarded-call notice are on")
    ("risky_tools" :list :default '()
                    :doc "tool names that deserve a card before they run; an uncarded call is noticed, never blocked"))
  (:tool "axioma_card" (nlk:contract 'axioma-card) 'stage-card)
  (:help :axioma "axioma: marks a reply's few phrases with a level, a style and a word, and stages an advisory card before a call; /axioma" +primer+)
  (:hook :tool #'observe-tool)
  (:command "axioma" 'run-slash
            :description "Recent signal marks and advisory cards, and verify a card code"))
