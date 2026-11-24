;;;; package.lisp --- the cell's own package, and what every file shares.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, as a peripheral's always is: the
;;;; loader finds START-CELL there (NLE:DEFINE-CELL exports it), and every
;;;; contribution is filed under this package's name downcased,
;;;; "nodecode-ttsbuddy", which is this folder's name.

(defpackage #:nodecode-ttsbuddy
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*ttsbuddy*
           #:ttsbuddy-error #:ttsbuddy-error-detail))

(in-package #:nodecode-ttsbuddy)

(nlk:define-peripheral ttsbuddy :not-running t)

;;; --- what TTS Buddy's agent API is, as data -------------------------------------

(defparameter +default-api-url+ "https://www.ttsbuddy.com/v1/agent-tts"
  "The agent endpoint the TTS Buddy CLI posted to (config.go's DefaultAPIURL).")

(defparameter +min-speed+ 0.5
  "The slowest speech the API accepts (types.go's AcceptedSpeedMin).")

(defparameter +max-speed+ 1.5
  "The fastest speech the API accepts (types.go's AcceptedSpeedMax).")

(defparameter +default-poll-timeout+ 600
  "How long a job is followed when the section names no ceiling, in seconds:
the plugin's own default.")

(defparameter +min-poll-timeout+ 30
  "The shortest job ceiling the section may set.")

(defparameter +max-poll-timeout+ 1800
  "The longest job ceiling the section may set.")

(defparameter +max-audio-bytes+ (* 64 1024 1024)
  "A cap on one downloaded MP3, so a misbehaving endpoint cannot fill memory:
the CLI refused any file over 500 MB, and an hour of speech is far under this.")

(defparameter +default-delay+ 3
  "Seconds to wait before the first status poll when the job names no
retry_after_seconds, as the CLI waited.")

(defparameter *sleep* #'sleep
  "How the poll waits. A test binds this; nothing else does.")

(defparameter *now* #'get-universal-time
  "The clock the poll's deadline reads. A test binds this; nothing else does.")

(defparameter *getenv* #'uiop:getenv
  "How an environment variable is read. A test binds this; nothing else does.")

;;; --- the curated voice catalogue, offline ------------------------------------------
;;;
;;; The CLI answered `voices' from an embedded list (internal/api/voices.go's
;;; CuratedVoices), never from the network, so the picker costs nothing. Each
;;; row is (id name gender language engine).

(defparameter +kokoro-voices+
  '(("af_heart" "Madison" "Female" "American English" "kokoro")
    ("af_bella" "Sophia" "Female" "American English" "kokoro")
    ("af_nicole" "Zoe" "Female" "American English" "kokoro")
    ("af_sarah" "Harper" "Female" "American English" "kokoro")
    ("af_alloy" "Allie" "Female" "American English" "kokoro")
    ("am_michael" "Michael" "Male" "American English" "kokoro")
    ("am_adam" "Jackson" "Male" "American English" "kokoro")
    ("am_echo" "Nathan" "Male" "American English" "kokoro")
    ("bf_emma" "Emma" "Female" "British English" "kokoro")
    ("bm_george" "George" "Male" "British English" "kokoro")
    ("ef_dora" "Valentina" "Female" "Spanish" "kokoro")
    ("em_alex" "Alejandro" "Male" "Spanish" "kokoro")
    ("ff_siwis" "Camille" "Female" "French" "kokoro")
    ("hf_alpha" "Priya" "Female" "Hindi" "kokoro")
    ("hm_omega" "Arjun" "Male" "Hindi" "kokoro")
    ("if_sara" "Giulia" "Female" "Italian" "kokoro")
    ("im_nicola" "Marco" "Male" "Italian" "kokoro")
    ("jf_alpha" "Yuki" "Female" "Japanese" "kokoro")
    ("jm_kumo" "Hiroshi" "Male" "Japanese" "kokoro")
    ("pf_dora" "Isabela" "Female" "Portuguese" "kokoro")
    ("pm_alex" "Rafael" "Male" "Portuguese" "kokoro")
    ("zf_xiaobei" "Mei" "Female" "Chinese" "kokoro")
    ("zm_yunjian" "Wei" "Male" "Chinese" "kokoro"))
  "The Kokoro voices the CLI shipped: each speaks its own language.")

(defparameter +supertonic-voices+
  '(("st_m1" "M1" "Male" "multilingual" "supertonic")
    ("st_m2" "M2" "Male" "multilingual" "supertonic")
    ("st_m3" "M3" "Male" "multilingual" "supertonic")
    ("st_m4" "M4" "Male" "multilingual" "supertonic")
    ("st_m5" "M5" "Male" "multilingual" "supertonic")
    ("st_f1" "F1" "Female" "multilingual" "supertonic")
    ("st_f2" "F2" "Female" "multilingual" "supertonic")
    ("st_f3" "F3" "Female" "multilingual" "supertonic")
    ("st_f4" "F4" "Female" "multilingual" "supertonic")
    ("st_f5" "F5" "Female" "multilingual" "supertonic"))
  "The Supertonic Fast voices: one engine, thirty-one languages, chosen by
the section's language or the call's own.")

(defun voice-rows ()
  "Every curated voice the picker lists, Kokoro first."
  (append +kokoro-voices+ +supertonic-voices+))

(defparameter +supertonic-languages+
  '("en" "fr" "de" "ja" "ko" "ar" "es" "it" "pt" "hi" "nl" "pl" "ru" "tr"
    "sv" "uk" "vi" "id" "cs" "da" "el" "fi" "hu" "ro" "sk" "bg" "hr" "lt"
    "lv" "sl" "et")
  "The language codes a Supertonic voice speaks, as voices.go lists them;
Kokoro voices use their own language and take no code.")

;;; --- text, bytes and names ---------------------------------------------------

(defun ascii (text)
  "TEXT's characters as the octets a container header stores them as."
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defun octets-of (data)
  "DATA as octets: NLK:HTTP answers a string when the peer sent text."
  (etypecase data
    (string (map '(vector (unsigned-byte 8)) #'char-code data))
    ((array (unsigned-byte 8) (*)) data)
    ((array t (*)) data)
    (null (make-array 0 :element-type '(unsigned-byte 8)))))

(defun octets-text (data)
  "DATA, text or octets, as a string."
  (if (stringp data) data (map 'string #'code-char data)))

(defun slug (text)
  "TEXT as a file name's stem: lowercase letters, digits and single dashes, at
most 40 of them, never empty."
  (let ((chars '()) (dash t) (n 0))
    (loop for char across (string-downcase (or text ""))
          while (< n 40)
          do (cond ((alphanumericp char) (push char chars) (setf dash nil) (incf n))
                   ((not dash) (push #\- chars) (setf dash t) (incf n))))
    (let ((out (string-right-trim "-" (coerce (nreverse chars) 'string))))
      (if (zerop (length out)) "speech" out))))

(defun datestamp ()
  "The local time as 20261108_101500, the way a file name carries it."
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format nil "~4,'0d~2,'0d~2,'0d_~2,'0d~2,'0d~2,'0d"
            year month date hour minute second)))

(defun fresh-id ()
  "A fresh id for one saved file: 12 lowercase hex digits."
  (format nil "~12,'0x" (random (expt 16 12))))

(defun idempotency-key ()
  "The header the plugin sent so a retried call is not billed twice: a
v4-shaped uuid, 32 hex digits dashed 8-4-4-4-12."
  (let ((hex (format nil "~32,'0x" (random (expt 16 32)))))
    (format nil "~a-~a-4~a-~a-~a"
            (subseq hex 0 8) (subseq hex 8 12) (subseq hex 13 16)
            (subseq hex 16 20) (subseq hex 20 32))))

(defun one-line (text &optional (cap 300))
  "TEXT on one line, at most CAP characters."
  (let ((line (substitute #\Space #\Newline (nlk:trimmed (or text "")))))
    (if (<= (length line) cap) line (concatenate 'string (subseq line 0 (- cap 3)) "..."))))

(defun mp3-p (octets)
  "Whether OCTETS begin like MPEG audio: an ID3 tag, or a frame sync. A 200
carrying a JSON error is what this catches."
  (let ((n (length octets)))
    (or (and (>= n 3) (equalp (subseq octets 0 3) (ascii "ID3")))
        (and (>= n 2) (= (aref octets 0) #xFF) (= (logand #xE0 (aref octets 1)) #xE0)))))
