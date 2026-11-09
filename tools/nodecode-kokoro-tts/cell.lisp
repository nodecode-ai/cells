;;;; cell.lisp --- Kokoro, a local voice, as two tools and one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of brunosouzas/hermes-kokoro-tts (see NOTICE). The plugin gave
;;;; Hermes a text-to-speech provider that spoke through its own Kokoro server
;;;; on loopback, one voice per sentence's language. Nodecode has no provider
;;;; seam for speech, so the same server is reached from two tools of the
;;;; cell's own -- kokoro_speak writes a WAV and answers where it is,
;;;; kokoro_voices lists what the server holds -- and from /kokoro. The
;;;; server's request shape, the voice id rule and the per-sentence language
;;;; routing are the original's.

(defpackage #:nodecode-kokoro-tts
  (:use #:cl)
  (:export #:kokoro-tts-error #:kokoro-tts-error-detail))

(in-package #:nodecode-kokoro-tts)

(nlk:define-peripheral kokoro-tts :not-running t)

;;; --- what a voice id is -----------------------------------------------------------

(defparameter +voice-languages+
  '(("a" . "en-us") ("b" . "en-gb") ("e" . "es") ("f" . "fr") ("h" . "hi")
    ("i" . "it") ("j" . "ja") ("p" . "pt-br") ("z" . "zh"))
  "Kokoro-82M's language codes -- the first letter of a voice id -- and the
language each speaks, as the original's catalog has them.")

(defparameter +language-labels+
  '(("a" . "American English") ("b" . "British English") ("e" . "Spanish")
    ("f" . "French") ("h" . "Hindi") ("i" . "Italian") ("j" . "Japanese")
    ("p" . "Brazilian Portuguese") ("z" . "Mandarin Chinese"))
  "What a person calls the language of a voice, by its first letter.")

(defparameter +default-voice+ "af_heart"
  "The voice the plugin's own settings default to.")

(defparameter +default-url+ "http://127.0.0.1:8770"
  "Where the plugin's server listens: loopback, one port, no key.")

(defparameter +sample-rate+ 24000
  "The rate Kokoro's model emits, and the one a WAV of ours declares when a
server's own header said nothing.")

(defparameter +detected-languages+ '("en" "pt" "it")
  "The languages a sentence is told apart in, as the original's detector does.")

(defparameter +always-detected+ '("en" "pt")
  "Detected whatever the voice map says; Italian is scored only when it has a
voice, so its false positives cannot move a sentence for someone who never
asked for it.")

(defun valid-voice-id-p (voice)
  "Whether VOICE is a well-formed Kokoro voice id (\"af_heart\"): a language
letter, a gender letter, an underscore and a name. A path never is, so a voice
can never be used to read an arbitrary file."
  (and (stringp voice)
       (>= (length voice) 4)
       (member (char voice 0) '(#\a #\b #\e #\f #\h #\i #\j #\p #\z))
       (member (char voice 1) '(#\f #\m))
       (char= (char voice 2) #\_)
       (every (lambda (character) (or (lower-case-p character) (digit-char-p character)))
              (subseq voice 3))))

(defun voice-map (entries)
  "ENTRIES -- the section's voices, each \"LANGUAGE=VOICE\" (\"pt=pf_dora\") --
as (values ALIST PROBLEMS): the language-to-voice pairs a sentence can reach,
and one line per entry that is ignored, so nothing is dropped without a word."
  (let ((map '())
        (problems '()))
    (dolist (entry (if (listp entries) entries '()))
      (when (stringp entry)
        (let* ((trimmed (nlk:trimmed entry))
               (at (position #\= trimmed)))
          (cond ((null at)
                 (push (format nil "voices entry ~s is not LANGUAGE=VOICE; ignored" entry) problems))
                (t (let ((language (string-downcase (nlk:trimmed (subseq trimmed 0 at))))
                         (voice (nlk:trimmed (subseq trimmed (1+ at)))))
                     (cond ((not (member language +detected-languages+ :test #'equal))
                            (push (format nil "voices entry ~s is ignored: sentences are detected as ~{~a~^, ~} only"
                                          entry +detected-languages+)
                                  problems))
                           ((not (valid-voice-id-p voice))
                            (push (format nil "voices ~a: ~s is not a Kokoro voice id; ignored"
                                          language voice)
                                  problems))
                           ((assoc language map :test #'equal)
                            (push (format nil "voices ~a is set twice; using ~a" language voice) problems)
                            (setf (cdr (assoc language map :test #'equal)) voice))
                           (t (push (cons language voice) map)))))))))
    (values (nreverse map) (nreverse problems))))

;;; --- the section, read back -------------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, when it is a non-blank string; else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun parse-speed (text)
  "TEXT as the speed a request carries, refused outside the server's own
0.25 to 4.0."
  (let ((number (ignore-errors (read-from-string (or text "1.0")))))
    (unless (and (realp number) (<= 0.25 number 4.0))
      (nlk:config-error "speed ~s is not a number between 0.25 and 4.0, which is what the server takes" text))
    (float number 1.0d0)))

(defun read-settings (values table)
  "The declared VALUES, checked once here: the url is a root with a scheme, the
voice is a Kokoro voice id, the voice map is parsed, and the speed is a number
the server will take."
  (declare (ignore table))
  (let* ((url (string-right-trim "/" (or (getf values :url) "")))
         (voice (or (nonblank (getf values :voice)) +default-voice+))
         (plist (copy-list values)))
    (unless (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
      (nlk:config-error "url ~s has no http:// or https:// scheme: set it to the Kokoro server's root, ~a for the local one"
                        url +default-url+))
    (unless (valid-voice-id-p voice)
      (nlk:config-error "voice ~s is not a Kokoro voice id (af_heart, bf_emma, pf_dora): the first letter is the language, the second the gender"
                        voice))
    (multiple-value-bind (map problems) (voice-map (getf values :voices))
      (setf (getf plist :voice) voice
            (getf plist :url) url
            (getf plist :speed) (parse-speed (getf values :speed))
            (getf plist :voice-map) map
            (getf plist :voice-problems) problems))
    plist))

;;; --- the language a sentence is in ------------------------------------------------
;;; A function-word count: cheap, dependency-free and good enough to tell whole
;;; sentences apart; it does not try to tag single foreign words.

(defun word-table (text)
  "TEXT's words as a set, for the counts below."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (word (uiop:split-string text :separator '(#\Space #\Newline #\Tab)))
      (when (plusp (length word)) (setf (gethash word table) t)))
    table))

(defparameter +english-function-words+
  (word-table "the and is are was were be been being to of in it its you your yours that this these those
                 with on at by from have has had will would can could should not what which who whom
                 when where why how there their they them we our us he she his her him i my an or but
                 if then than about into over after before just also only very all any some each every
                 does did done doing yes hi hello please thanks thank next today now here"))

(defparameter +portuguese-function-words+
  (word-table "o os um uma uns umas que não nao é e em para pra por pelo pela pelos pelas mais mas
                 você voce vocês está esta estão são foi ser ter tem têm isso isto esse essa este
                 aquele aquela como quando onde porque também tambem já ja ainda muito muita seu sua seus
                 suas meu minha nosso nossa ele ela eles elas nós lhe dos das num numa ao aos à às
                 àquele àquela àqueles àquelas àquilo
                 sim olá ola obrigado obrigada hoje agora aqui depois antes então entao sobre até
                 fazer feito pode podem vai vamos estou tenho"))

(defparameter +italian-function-words+
  (word-table "il le un uno una gli di del dello della delle degli nel nello nella nelle negli alla allo
                 alle agli dal dalla dalle sulla sulle che non è c'è perché poiché benché finché affinché
                 nonché anche ancora più molto molta molti poco questo questa questi queste quello quella
                 siamo siete ho hai abbiamo avete hanno stato stata essere fatto ciao grazie ecco bene
                 buongiorno buonasera allora però così già
                 sì oggi domani ieri adesso qui dopo forse niente nulla tutto tutti tutta nessun nessuno
                 nessuna cosa vorrei voglio puoi può stai lui noi voi loro mio mia tuo nostro nostra ma"))

(defparameter +abbreviations+
  '("dr" "dra" "drs" "sr" "sra" "srs" "srta" "mr" "mrs" "ms" "prof" "profa" "st" "jr"
    "vs" "etc" "eg" "ie" "ex" "av" "fig" "cap" "pág" "pag" "approx" "inc" "ltd" "dept"
    "min" "máx" "max" "nº" "vol" "ed")
  "A full stop after one of these does not end a sentence.")

(defparameter +italian-elisions+
  '("l" "un" "dell" "nell" "dall" "sull" "all" "quell" "quest" "nessun")
  "Italian elision: \"l'aggiornamento\", \"dell'anno\". At least two letters
after the apostrophe, so English \"all's\" does not count.")

(defparameter +right-quote+ (code-char #x2019)
  "The typographic apostrophe Italian elision is written with too.")

(defparameter +ellipsis+ (code-char #x2026) "... ends a sentence as . does.")

(defparameter +sentence-stops+ (list #\. #\! #\? +ellipsis+))
(defparameter +closers+ (list #\" #\' #\) #\] +right-quote+ (code-char #x201D)))
(defparameter +space-chars+ '(#\Space #\Tab #\Newline #\Return))

(defun whitespace-p (character)
  (member character +space-chars+ :test #'char=))

(defun tokens-of (text)
  "TEXT's whitespace-delimited tokens."
  (remove-if (lambda (token) (zerop (length token)))
             (uiop:split-string text :separator +space-chars+)))

(defun letters-in (token)
  "TOKEN's letters, lowercased: its punctuation and digits are not words."
  (string-downcase (remove-if-not #'alpha-char-p token)))

(defun prose-words (prose)
  "PROSE's words, lowercased."
  (remove-if (lambda (word) (zerop (length word)))
             (mapcar #'letters-in (tokens-of prose))))

(defun strip-not-prose (text)
  "TEXT without the spans a language signal must not come from: inline code,
URLs, e-mail addresses and paths."
  (let ((out (make-string-output-stream))
        (in-code nil))
    (loop for character across text
          do (cond ((char= character #\`) (setf in-code (not in-code)) (write-char #\Space out))
                   (in-code (write-char #\Space out))
                   (t (write-char character out))))
    (format nil "~{~a~^ ~}"
            (remove-if (lambda (token)
                         (or (find #\/ token) (find #\@ token)
                             (uiop:string-prefix-p "www." (string-downcase token))))
                       (tokens-of (get-output-stream-string out))))))

(defun count-words (words set)
  "How many of WORDS are in SET."
  (loop for word in words count (gethash word set) into total finally (return total)))

(defun count-accents (text characters)
  "How many characters of TEXT are one of CHARACTERS, case aside."
  (count-if (lambda (character) (find (char-downcase character) characters)) text))

(defun count-accented-words (words characters &optional strong italian)
  "How many of WORDS carry one of CHARACTERS and none of STRONG -- a weak
signal, one point a word -- and, with ITALIAN, are not an Italian function
word whose accent is its own."
  (loop for word in words
        when (and (some (lambda (character) (find character characters)) word)
                  (or (null strong) (notany (lambda (character) (find character strong)) word))
                  (not (and italian (gethash word +italian-function-words+))))
          count 1))

(defun italian-words (prose)
  "PROSE's words for the Italian count: a function word inside a hyphenated
compound or an identifier gives Italian no point, and neither does an
all-capitals token, which is an acronym."
  (let ((kept '()))
    (dolist (token (tokens-of prose))
      (unless (or (find #\- token) (find #\_ token)
                  (and (> (length token) 1) (every #'upper-case-p token)))
        (push token kept)))
    (prose-words (format nil "~{~a~^ ~}" (nreverse kept)))))

(defun count-italian-elisions (prose)
  "How many Italian elisions PROSE writes: l'anno, un'altra, dell'aggiornamento."
  (let ((total 0)
        (length (length prose)))
    (dotimes (index length)
      (let ((character (char prose index)))
        (when (or (char= character #\') (char= character +right-quote+))
          (let* ((end index)
                 (at (1- end))
                 (token (progn (loop while (and (>= at 0) (alpha-char-p (char prose at))) do (decf at))
                               (string-downcase (subseq prose (1+ at) end))))
                 (after (1+ index)))
            (when (and (member token +italian-elisions+ :test #'equal)
                       (< (1+ after) length)
                       (alpha-char-p (char prose after))
                       (alpha-char-p (char prose (1+ after))))
              (incf total))))))
    total))

(defun languages-for (voices)
  "The languages sentences are told apart in, given the voice map: English and
Portuguese always, any other one only when it has a voice."
  (remove-if-not (lambda (language)
                    (or (member language +always-detected+ :test #'equal)
                        (assoc language voices :test #'equal)))
                  +detected-languages+))

(defun detect-language (text &optional (languages +detected-languages+))
  "The language of LANGUAGES TEXT is in -- \"pt\", \"en\" or \"it\" -- or NIL
when it gives no clear signal: a count of function words common in one of them
and rare in the others, plus the accents only one of them writes."
  (let* ((prose (strip-not-prose text))
         (words (prose-words prose))
         (italian (member "it" languages :test #'equal))
         (portuguese (member "pt" languages :test #'equal))
         (scores '()))
    (when (member "en" languages :test #'equal)
      (push (cons "en" (count-words words +english-function-words+)) scores))
    (when portuguese
      (push (cons "pt" (+ (count-words words +portuguese-function-words+)
                           (* 2 (count-accents prose "ãõç"))
                           (count-accented-words words "âêôáéíóú" "ãõç" italian)))
            scores))
    (when italian
      (push (cons "it" (+ (count-words (italian-words prose) +italian-function-words+)
                           (count-accented-words words "èìòù")
                           (count-italian-elisions prose)))
            scores))
    (let* ((ordered (sort (copy-list scores) #'> :key #'cdr))
           (best (first ordered))
           (runner-up (second ordered)))
      (when (and best (plusp (cdr best))
                 (not (and runner-up (= (cdr best) (cdr runner-up)))))
        (car best)))))

;;; --- sentences, and the voice each is read by -------------------------------------

(defun last-token (text start end)
  "TEXT's last token at or before END, looked for no further back than START."
  (let ((at (1- end)))
    (loop while (and (>= at start) (whitespace-p (char text at))) do (decf at))
    (let ((stop (1+ at)))
      (loop while (and (>= at start) (not (whitespace-p (char text at)))) do (decf at))
      (subseq text (1+ at) stop))))

(defun abbreviation-before-p (text start end)
  "Whether TEXT's token ending at END is an abbreviation or an initial, so the
full stop after it does not end a sentence."
  (let* ((token (string-left-trim "([\"'‘“" (last-token text start end)))
         (token (string-right-trim "." token))
         (lower (string-downcase token)))
    (and (plusp (length lower))
         (or (member lower +abbreviations+ :test #'equal)
             (and (= (length token) 1) (upper-case-p (char token 0)))
             (and (find #\. lower) (<= (length lower) 5) (alpha-char-p (char lower 0)))))))

(defun split-sentences (text)
  "TEXT's sentences: a full stop, question or exclamation mark followed by
whitespace, or a line break, ends one -- unless the stop closes an abbreviation
or an initial, and never inside inline code, whose punctuation is not prose."
  (let ((sentences '())
        (start 0)
        (length (length text))
        (in-code nil)
        (index 0))
    (labels ((cut (end)
               (let ((sentence (nlk:trimmed (subseq text start (min end length)))))
                 (when (plusp (length sentence)) (push sentence sentences)))
               (setf start end)))
      (loop while (< index length)
            do (let ((character (char text index)))
                 (cond ((char= character #\`) (setf in-code (not in-code)) (incf index))
                       (in-code (incf index))
                       ((find character +sentence-stops+ :test #'char=)
                        (let ((run-end (or (position-if-not (lambda (mark)
                                                              (or (find mark +sentence-stops+ :test #'char=)
                                                                  (find mark +closers+ :test #'char=)))
                                                            text :start index)
                                           length)))
                          (if (and (< run-end length)
                                   (whitespace-p (char text run-end))
                                   (or (not (char= character #\.))
                                       (not (string= "." (subseq text index run-end)))
                                       (not (abbreviation-before-p text start index))))
                              (progn (cut run-end)
                                     (loop while (and (< run-end length)
                                                      (member (char text run-end) '(#\Space #\Tab) :test #'char=))
                                           do (incf run-end))
                                     (setf start run-end
                                           index run-end))
                              (setf index run-end))))
                       ((member character '(#\Newline #\Return) :test #'char=)
                        (cut index)
                        (setf start (incf index)))
                       (t (incf index)))))
      (cut length)
      (nreverse sentences))))

(defun plan-runs (text voices default-voice)
  "TEXT as consecutive (TEXT . VOICE) runs: a sentence with no clear language
keeps the previous sentence's voice (the first falls back to DEFAULT-VOICE),
and neighbours that would use the same voice are merged, so each run is one
request."
  (let ((runs '())
        (current default-voice)
        (languages (languages-for voices)))
    (dolist (sentence (split-sentences text))
      (let ((language (detect-language sentence languages)))
        (when language
          (setf current (or (cdr (assoc language voices :test #'equal)) default-voice)))
        (if (and runs (equal (cdr (car runs)) current))
            (setf (car runs) (cons (format nil "~a ~a" (caar runs) sentence) current))
            (push (cons sentence current) runs))))
    (or (nreverse runs) (list (cons text default-voice)))))

;;; --- WAV, in and out ---------------------------------------------------------------

(defun u16 (octets at)
  (logior (aref octets at) (ash (aref octets (1+ at)) 8)))

(defun u32 (octets at)
  (logior (u16 octets at) (ash (u16 octets (+ at 2)) 16)))

(defun read-wav (octets)
  "OCTETS as (values SHAPE PCM): SHAPE the (:channels :rate :bits) plist the
header declares, PCM the data chunk's octets."
  (unless (and (>= (length octets) 44)
               (string= "RIFF" (map 'string #'code-char (subseq octets 0 4)))
               (string= "WAVE" (map 'string #'code-char (subseq octets 8 12))))
    (fail "the server answered ~d bytes that are not a WAV" (length octets)))
  (let ((shape (list :channels 1 :rate +sample-rate+ :bits 16))
        (pcm nil)
        (at 12))
    (loop while (<= (+ at 8) (length octets))
          do (let ((id (map 'string #'code-char (subseq octets at (+ at 4))))
                   (size (u32 octets (+ at 4))))
               (cond ((string= id "fmt ")
                      (setf shape (list :channels (max 1 (u16 octets (+ at 10)))
                                        :rate (u32 octets (+ at 12))
                                        :bits (u16 octets (+ at 22)))))
                     ((string= id "data")
                      (setf pcm (subseq octets (+ at 8) (min (length octets) (+ at 8 size))))
                      (loop-finish)))
               (incf at (+ 8 size (logand size 1)))))
    (unless pcm (fail "the server's WAV carries no data chunk"))
    (values shape pcm)))

(defun wav-octets (pcm rate channels bits)
  "PCM wrapped in a canonical 44-octet WAV header."
  (let* ((octets (make-array (+ 44 (length pcm)) :element-type '(unsigned-byte 8)))
         (block (max 1 (* channels (floor bits 8)))))
    (labels ((put (at value bytes)
               (loop for index from 0 below bytes
                     do (setf (aref octets (+ at index)) (ldb (byte 8 (* 8 index)) value))))
             (text-at (at string)
               (loop for character across string
                     for index from 0
                     do (setf (aref octets (+ at index)) (char-code character)))))
      (text-at 0 "RIFF")
      (put 4 (+ 36 (length pcm)) 4)
      (text-at 8 "WAVE")
      (text-at 12 "fmt ")
      (put 16 16 4)
      (put 20 1 2)
      (put 22 channels 2)
      (put 24 rate 4)
      (put 28 (* rate block) 4)
      (put 32 block 2)
      (put 34 bits 2)
      (text-at 36 "data")
      (put 40 (length pcm) 4)
      (replace octets pcm :start1 44))
    octets))

;;; --- one request to the server ------------------------------------------------------

(defun body-text-of (body)
  "BODY as text, whether the wire gave back a string or octets."
  (cond ((stringp body) body)
        ((typep body '(vector (unsigned-byte 8)))
         (or (ignore-errors (sb-ext:octets-to-string body :external-format :utf-8))
             (sb-ext:octets-to-string body :external-format :latin-1)))
        (t "")))

(defun lang-code (voice)
  "The Kokoro language code a request for VOICE speaks: the voice's own first
letter, or the language setting's when it names one."
  (let ((override (nonblank (setting :language))))
    (cond ((null override) (string (char voice 0)))
          ((assoc (string-downcase override) +voice-languages+ :test #'equal)
           (string-downcase override))
          ((rassoc (string-downcase override) +voice-languages+ :test #'equal)
           (car (rassoc (string-downcase override) +voice-languages+ :test #'equal)))
          (t (fail "language ~s is not a Kokoro language code (~{~a~^, ~}); leave it unset to speak with the voice's own"
                   override (mapcar #'car +voice-languages+))))))

(defun json-object-of (pairs)
  "PAIRS -- alternating keys and values -- as one JSON object."
  (let ((object (nlk:json-object)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(defun english-words ()
  "The section's English loanwords, as the server's loanword pass takes them."
  (remove-duplicates
   (remove-if-not (lambda (word) (and (stringp word) (<= 1 (length word) 64)))
                  (let ((words (setting :english-words)))
                    (if (listp words) words '())))
   :test #'equal))

(defun post-speech (text voice)
  "TEXT spoken by VOICE: the WAV octets the server answers."
  (let* ((url (format nil "~a/v1/audio/speech" (setting :url)))
         (words (english-words))
         (pairs (list "input" text "voice" voice
                      "speed" (setting :speed)
                      "response_format" "wav"
                      "lang_code" (lang-code voice))))
    (when (and words (not (member (char voice 0) '(#\a #\b))))
      (setf pairs (append pairs (list "english_words" (coerce words 'vector)
                                      "english_accent" (setting :english-accent)))))
    (multiple-value-bind (body status)
        (nlk:http :post url
                  :headers '(("content-type" . "application/json") ("accept" . "audio/wav"))
                  :content (nlk:encode-json-object (json-object-of pairs))
                  :binary t :connect-timeout 5 :timeout (setting :timeout-seconds))
      (when (>= status 400)
        (fail "the server answered HTTP ~a on ~a~@[: ~a~]" status url
              (nlk:clip (nlk:trimmed (body-text-of body)) 300)))
      (let ((octets (if (stringp body)
                        (sb-ext:string-to-octets body :external-format :latin-1)
                        body)))
        (unless (plusp (length octets))
          (fail "the server returned no audio for ~s" (nlk:clip text 40)))
        octets))))

;;; --- saying it, and where the recording goes ---------------------------------------

(defun output-directory ()
  "Where recordings are written: the section's output_dir, else a folder in the
cell's cache."
  (let ((named (nonblank (setting :output-dir))))
    (ensure-directories-exist
     (if named
         (uiop:ensure-directory-pathname named)
         (nlk:cache-path "kokoro/")))))

(defun target-path (path)
  "Where this recording goes: PATH when the call names one, else a fresh file in
OUTPUT-DIRECTORY."
  (let ((named (nonblank path)))
    (if named
        (pathname named)
        (merge-pathnames (format nil "speech-~d.wav" (get-universal-time))
                         (output-directory)))))

(defun spoken-length (pcm-bytes shape)
  "How long PCM-BYTES play, in words a person reads."
  (let* ((channels (getf shape :channels))
         (rate (getf shape :rate))
         (bits (getf shape :bits))
         (seconds (/ pcm-bytes (float (max 1 (* channels rate (floor bits 8)))))))
    (format nil "~,1f s, ~d Hz ~:[stereo~;mono~]" seconds rate (= channels 1))))

(defun speak-text (text &key voice path)
  "TEXT said by the server => the lines the model reads: where the recording is,
how long it plays, and which voices spoke it."
  (let ((said (nlk:trimmed (or text ""))))
    (when (zerop (length said)) (fail "there is nothing to say"))
    (let* ((chosen (nonblank voice))
           (runs (if chosen
                     (progn
                       (unless (valid-voice-id-p chosen)
                         (fail "~s is not a Kokoro voice id (af_heart, pf_dora, if_sara): a language letter, a gender letter and a name"
                               chosen))
                       (list (cons said chosen)))
                     (plan-runs said (setting :voice-map) (setting :voice))))
           (pieces '())
           (shape nil))
      (dolist (run runs)
        (multiple-value-bind (answered pcm) (read-wav (post-speech (car run) (cdr run)))
          (if shape
              (unless (equal answered shape)
                (fail "the server's recordings differ in shape (~s and ~s), and one file cannot hold both"
                      shape answered))
              (setf shape answered))
          (push (cons (cdr run) pcm) pieces)))
      (let* ((ordered (nreverse pieces))
             (bytes (reduce #'+ ordered :key (lambda (piece) (length (cdr piece))) :initial-value 0))
             (pcm (apply #'concatenate '(vector (unsigned-byte 8)) (mapcar #'cdr ordered)))
             (file (target-path path))
             (recorded (wav-octets pcm (getf shape :rate) (getf shape :channels) (getf shape :bits))))
        (when (zerop (length pcm))
          (fail "the server answered a recording with no samples in it"))
        (ensure-directories-exist file)
        (with-open-file (out file :direction :output :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
          (write-sequence recorded out))
        (format nil "Spoke ~d character~:p (~a) into ~a~%  voices: ~{~a~^, ~}~@[~%  ignored: ~{~a~^; ~}~]"
                (length said) (spoken-length bytes shape) (uiop:native-namestring file)
                (mapcar #'car ordered) (setting :voice-problems))))))

;;; --- what the server holds ---------------------------------------------------------

(defun voice-name (voice)
  "A voice's name as a person writes it: \"Heart\", \"Dora\"."
  (string-capitalize (substitute #\Space #\_ (subseq voice (1+ (or (position #\_ voice) 0))))))

(defun voice-label (voice)
  (format nil "~a (~a)" (voice-name voice)
          (or (cdr (assoc (string (char voice 0)) +language-labels+ :test #'equal))
              "unknown language")))

(defun language-letters (language)
  "The voice-id language letters a language name covers: en is a and b."
  (let ((code (string-downcase language)))
    (loop for (letter . bcp47) in +voice-languages+
          when (or (string= code letter)
                   (string= code bcp47)
                   (uiop:string-prefix-p (concatenate 'string code "-") bcp47))
            collect letter)))

(defun fetch-voices ()
  "The voice ids the server holds."
  (let ((url (format nil "~a/v1/voices" (setting :url))))
    (multiple-value-bind (body status)
        (nlk:http :get url :headers '(("accept" . "application/json"))
                  :connect-timeout 5 :timeout (min 15 (setting :timeout-seconds)))
      (when (>= status 400)
        (fail "the server answered HTTP ~a on ~a~@[: ~a~]" status url
              (nlk:clip (nlk:trimmed (body-text-of body)) 300)))
      (let* ((answer (or (ignore-errors (nlk:decode-json (body-text-of body)))
                         (nlk:json-object)))
             (ids (nlk:json-array answer "voices")))
        (remove-if-not #'valid-voice-id-p (coerce (or ids #()) 'list))))))

(defun format-voices (ids &optional language)
  "IDS as the lines the model reads, with the language of each and which one the
section speaks with."
  (let* ((kept (if language
                   (remove-if-not (lambda (id)
                                    (member (string (char id 0)) (language-letters language)
                                            :test #'equal))
                                  ids)
                   ids))
         (default (setting :voice)))
    (with-output-to-string (out)
      (format out "~d voice~:p at ~a~@[, ~a only~]~@[ (~a is the default)~]:"
              (length kept) (setting :url) language
              (and (member default kept :test #'equal) default))
      (if (null kept)
          (format out "~%  none~@[ -- the server holds no ~a voice~]" language)
          (dolist (id (sort (copy-list kept) #'string<))
            (format out "~%  ~a~:[~;  <- the section's voice~]  ~a"
                    id (equal id default) (voice-label id)))))))

;;; --- the verbs the two tools call --------------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (kokoro-tts-error (condition)
       (nle:failure (format nil "ERROR: ~a" (kokoro-tts-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Kokoro: ~a" (nlk:one-line (princ-to-string condition)))))))

(nlk:define-contract kokoro-speak
  (:doc "Say something out loud with the local Kokoro voice and answer where the recording is. Use it when a person will listen rather than read: a greeting, a summary, a spoken copy of an answer. Nothing leaves the machine -- the voice is a Kokoro server on loopback -- and with a voice map each sentence is read in its own language.")
  ("text" :text :required t
          :doc "what to say, in words: markdown marks are read aloud as written")
  ("voice" :text
           :doc "a Kokoro voice id for the whole text (af_heart, bf_emma, pf_dora, if_sara); unset, the section's voice and its language map decide")
  ("path" :text
          :doc "where to write the WAV file; unset, a fresh file under the cell's cache"))

(nlk:define-contract kokoro-voices
  (:doc "List the Kokoro voices the local server holds, each with the language it reads, and which one the section speaks with.")
  ("language" :text :options '("en" "pt" "it" "es" "fr" "hi" "ja" "zh")
              :doc "keep only the voices of this language; unset, every voice the server holds"))

(defun speak-tool (call)
  "The kokoro_speak tool: CALL's text said, and the recording answered for."
  (answering
    (speak-text (kokoro-speak-text call)
                :voice (kokoro-speak-voice call)
                :path (kokoro-speak-path call))))

(defun voices-tool (call)
  "The kokoro_voices tool: what the server holds."
  (answering (format-voices (fetch-voices) (kokoro-voices-language call))))

;;; --- /kokoro -----------------------------------------------------------------------

(defun status-lines ()
  "What /kokoro answers with no text: where the server is, what will speak, and
how a room's spoken replies can use the same voice."
  (let ((url (setting :url))
        (voice (setting :voice))
        (map (setting :voice-map)))
    (with-output-to-string (out)
      (format out "Kokoro voice: ~a, speaking as ~a.~%" url voice)
      (if map
          (format out "  one voice per sentence's language: ~{~a~^, ~}; a sentence with no clear language keeps the voice before it.~%"
                  (loop for (language . named) in map
                        collect (format nil "~a=~a" language named)))
          (format out "  one voice for every sentence; a voice map gives other languages their own (\"voices\": [\"pt=pf_dora\", \"it=if_sara\"]).~%"))
      (format out "  recordings go to ~a~@[; ignored: ~{~a~^; ~}~]~%"
              (uiop:native-namestring (output-directory)) (setting :voice-problems))
      (format out "  a room's spoken replies can use it too: set speech.base_url to ~a/v1, speech.model to kokoro and speech.voice to ~a."
              url voice))))

(defun run-command (args session-id)
  "/kokoro TEXT: say TEXT with the local voice. No argument, where the voice is
and what will speak."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (plusp (length text))
        (answering (speak-text text))
        (status-lines))))

(defparameter +primer+
  "Kokoro reads text aloud on this machine, through a server on loopback: nothing is sent anywhere.
  kokoro_speak   {text, voice?, path?}  writes a WAV and answers where it is
  kokoro_voices  {language?}            the voices the server holds, and where it is
Reach for kokoro_speak when a person will listen to the answer rather than read it -- a greeting, a
summary, a spoken copy. Text is said as written, so hand it the words themselves, not markdown.
With a voice map each sentence is read in its own language, and one answer may use several
voices; a recording that fails says why and costs the words, never the answer."
  "What (help :kokoro) answers while the cell runs.")

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell kokoro-tts
  (:section ("kokoro")
    (:guide "a Kokoro server on loopback -- the plugin's `hermes kokoro install' starts one on port 8770 -- or any Kokoro server speaking /v1/audio/speech: url points at it; voice is the default (af_heart); voices gives another language its own (\"pt=pf_dora\"); next step: ask for something to be said out loud")
    ("url" :string :default +default-url+
           :doc "the Kokoro server's root: http://127.0.0.1:8770 is the local one")
    ("voice" :string :default +default-voice+
             :doc "the voice everything speaks with unless a language or the call names another (a Kokoro voice id: af_heart, bf_emma, pf_dora)")
    ("voices" :list
              :doc "one voice per language, as LANGUAGE=VOICE (\"pt=pf_dora\"): sentences detected as that language are read by it; the languages detected are en, pt and it")
    ("speed" :string :default "1.0"
             :doc "how fast the voice speaks, 0.25 to 4.0")
    ("language" :string
                :doc "single-voice mode: read as this Kokoro language code (a, b, e, f, h, i, j, p, z, or a code such as pt-br) instead of the voice's own; unset, the voice decides")
    ("english_words" :list
                     :doc "English words a non-English voice should say in English, as the server's loanword pass takes them")
    ("english_accent" :choice :options '("a" "b") :default "a"
                       :doc "the accent those English loanwords are read with: a American, b British")
    ("output_dir" :path
                  :doc "where recordings are written; unset, a folder in the cache")
    ("timeout_seconds" :integer :default 120 :min 1
                        :doc "one request's ceiling, in seconds"))
  (:settings #'read-settings)
  (:help :kokoro
         "kokoro: say something out loud with a local Kokoro voice (kokoro_speak, kokoro_voices)"
         +primer+)
  (:tool "kokoro_speak" (nlk:contract 'kokoro-speak) #'speak-tool)
  (:tool "kokoro_voices" (nlk:contract 'kokoro-voices) #'voices-tool)
  (:command "kokoro" 'run-command
            :description "Say something out loud with the local Kokoro voice"
            :argument-hint "TEXT"))
