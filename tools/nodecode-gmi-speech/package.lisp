;;;; package.lisp --- the cell's own package, and what every file shares.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The package is named after the system, as a peripheral's always is: the
;;;; loader finds START-CELL here (NLE:DEFINE-CELL exports it), and every
;;;; contribution is filed under this package's name downcased,
;;;; "nodecode-gmi-speech", which is this folder's name.

(defpackage #:nodecode-gmi-speech
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*gmi-speech*
           #:gmi-speech-error #:gmi-speech-error-detail))

(in-package #:nodecode-gmi-speech)

(nlk:define-peripheral gmi-speech :not-running t)

;;; --- what GMI Cloud's speech request queue is, as data --------------------------

(defparameter +default-base-url+ "https://console.gmicloud.ai/api/v1/ie/requestqueue/apikey"
  "The endpoint every request goes to: the one the plugin dialled (gmi_media.py's
DEFAULT_BASE_URL). The queue answers /models, /models/<id> and /requests.")

(defparameter +speech-models+
  '("minimax-tts-speech-2.6-turbo" "inworld-tts-1.5-mini")
  "The speech models the plugin preferred, in its own order (__init__.py's
SPEECH_MODELS): the first one GMI answers a schema for is the one used when
the section names none.")

(defparameter +fallback-voices+
  '("English_expressive_narrator" "English_radiant_girl")
  "The voices the plugin offered when a model's schema declares no voice
option (__init__.py's FALLBACK_VOICES).")

(defparameter +voice-params+ '("voice_id" "voice")
  "What a GMI speech schema may call its voice parameter (gmi_media.py's
CANDIDATES, voice).")

(defparameter +prompt-params+ '("prompt" "text")
  "What a GMI speech schema may call the words to speak.")

(defparameter +speed-params+ '("speed" "speaking_rate")
  "What a GMI speech schema may call the pace.")

(defparameter +format-params+ '("format" "audio_encoding" "output_format")
  "What a GMI speech schema may call the container.")

(defparameter +format-aliases+ '(("wav" . "LINEAR16") ("ogg" . "OGG_OPUS") ("opus" . "OGG_OPUS"))
  "What the plugin asked for when a model offers the same format under another
name (gmi_media.py's map_payload, format).")

(defparameter +audio-extensions+ '(("mp3" . ".mp3") ("flac" . ".flac") ("ogg" . ".ogg")
                                    ("opus" . ".ogg") ("ogg_opus" . ".ogg")
                                    ("linear16" . ".wav") ("wav" . ".wav"))
  "The container a declared format names, as the file's extension, or NIL for
one no player here opens (gmi_media.py's audio_extension).")

(defparameter +max-audio-bytes+ 50000000
  "A cap on one downloaded recording, the plugin's own 50 MB.")

(defparameter +default-poll-timeout+ 300
  "How long one job is followed before the call gives up, in seconds: the
ceiling the plugin passed to its client's run.")

(defparameter +min-poll-timeout+ 30
  "The shortest job ceiling the section may set.")

(defparameter +max-poll-timeout+ 1800
  "The longest job ceiling the section may set.")

(defparameter +schema-ttl+ 3600
  "How long one model schema is kept before it is read again, in seconds: the
catalog's own hour.")

(defparameter *sleep* #'sleep
  "How the poll waits. A test binds this; nothing else does.")

(defparameter *now* #'get-universal-time
  "The clock the poll's deadline reads. A test binds this; nothing else does.")

(defparameter *getenv* #'uiop:getenv
  "How an environment variable is read. A test binds this; nothing else does.")

(defparameter *schemas* (make-hash-table :test #'equal)
  "Model schemas read lately, by (base-url . model): one read serves every
call inside the hour. Cleared when the cell starts.")

;;; --- text, filenames and bytes -----------------------------------------------

(defun octets-text (data)
  "DATA, text or octets, as a string."
  (if (stringp data) data (map 'string #'code-char data)))

(defun one-line (text &optional (cap 300))
  "TEXT on one line, at most CAP characters."
  (let ((line (substitute #\Space #\Newline (nlk:trimmed (or text "")))))
    (if (<= (length line) cap) line
        (concatenate 'string (subseq line 0 (- cap 3)) "..."))))

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

(defun audio-extension (format)
  "FORMAT's extension as a string, or NIL for a container no player here opens."
  (cdr (assoc (string-downcase (or format "")) +audio-extensions+ :test #'string=)))

(defun without-key (text key)
  "TEXT with KEY taken out, so a failure's words can be shown without the
credential that went out with them."
  (let ((text (substitute #\Space #\Newline (or text ""))))
    (if (or (null key) (zerop (length key)))
        text
        (with-output-to-string (out)
          (loop with start = 0
                for at = (search key text :start2 start)
                do (cond (at (write-string text out :start start :end at)
                              (write-string "[redacted]" out)
                              (setf start (+ at (length key))))
                         (t (write-string text out :start start) (return))))))))

(defun url-path-segment (text)
  "TEXT with everything but an unreserved URL character percent-encoded, as the
plugin quoted a model id into its path."
  (with-output-to-string (out)
    (loop for char across text
          do (if (or (alphanumericp char) (find char "-._~"))
                 (write-char char out)
                 (format out "%~2,'0X" (char-code char))))))
