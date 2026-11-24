;;;; cell.lisp --- a podcast episode from a script: speak, master, keep.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin parsed a two-host script, spoke every line through
;;;; Microsoft Edge's online voices, joined the clips with pauses and a chime,
;;;; ran one loudness pass over the result, wrote chapters and a timestamped
;;;; transcript, and kept the episode in a folder of its own. Here the same
;;;; three tools (produce, voices, episodes) and the same /podcast command do
;;;; it in this image: the voice service's socket, ffmpeg on PATH (a CLI the
;;;; operator already has) for the mastering, and a folder per episode under
;;;; the cell's episodes directory.

(in-package #:nodecode-podcast)

;;; --- what cannot be done as asked ----------------------------------------------

(define-condition script-error (error)
  ((message :initarg :message :initform "the script cannot be produced as given" :reader script-error-message)
   (hint :initarg :hint :initform "Fix the script and call the tool again." :reader script-error-hint))
  (:report (lambda (condition stream) (write-string (script-error-message condition) stream))))

(define-condition synth-error (error)
  ((hint :initarg :hint :initform "Check the internet connection and that the voice exists (podcast_voices)."
         :reader synth-error-hint)))

(define-condition mix-error (error)
  ((hint :initarg :hint :initform "Check that ffmpeg works, or clear the podcast ffmpeg_path setting."
         :reader mix-error-hint)))

;;; --- constants ----------------------------------------------------------------

(defparameter +default-voices+
  '("en-US-AndrewMultilingualNeural" "en-US-AvaMultilingualNeural" "en-GB-RyanNeural")
  "The voices a host gets when the script names none: A, B, then any third host.")

(defparameter +max-line-chars+ 1500 "One turn past this reads as a monologue.")

(defparameter +edge-token+ "6A5AA1D4EAFF4E9FB37E23D68491D6F4"
  "The client token the Edge read-aloud page itself sends; not a secret.")

(defparameter +edge-socket+ "wss://speech.platform.bing.com/consumer/speech/synthesize/readaloud/edge/v1"
  "Microsoft Edge's read-aloud socket, the service the original used.")

(defparameter +voices-url+
  (concatenate 'string
               "https://speech.platform.bing.com/consumer/speech/synthesize/readaloud/voices/list?trustedclienttoken="
               +edge-token+)
  "The voice list, one JSON array of Locale, Gender, ShortName, VoiceTag.")

(defparameter +voice-cache-seconds+ (* 7 24 3600) "A week, as the original cached it.")

(defparameter +sample-rate+ 24000 "The voice service's own rate.")
(defparameter +mp3-bytes-per-second+ 6000 "audio-24khz-48kbitrate-mono-mp3: 48 kbit/s.")

;;; --- small text helpers ---------------------------------------------------------

(defun join-strings (strings separator)
  (with-output-to-string (out)
    (loop for (string . rest) on strings
          do (write-string string out)
             (when rest (write-string separator out)))))

(defun collapse-spaces (text)
  "TEXT with runs of whitespace as one space, ends trimmed."
  (string-trim " "
               (with-output-to-string (out)
                 (let ((space nil))
                   (loop for character across text
                         do (if (member character '(#\Space #\Tab #\Newline #\Return #\Linefeed))
                                (setf space t)
                                (progn (when space (write-char #\Space out) (setf space nil))
                                       (write-char character out))))))))

(defun clean-text (value)
  "VALUE as spoken text: the markdown a voice would read out loud taken off,
links kept as their words, whitespace collapsed."
  (let* ((text (cond ((stringp value) value)
                      ((null value) "")
                      ((eq value :null) "")
                      (t (princ-to-string value))))
         (size (length text))
         (out (make-string-output-stream)))
    (loop with index = 0
          while (< index size)
          do (let ((character (char text index)))
               (cond ((member character '(#\* #\_ #\` #\# #\>))
                      (incf index))
                     ((char= character #\[)
                      (let ((close (position #\] text :start index)))
                        (if (and close (< (1+ close) size) (char= #\( (char text (1+ close))))
                            (let ((end (position #\) text :start (+ close 2))))
                              (if end
                                  (progn (write-string text out :start (1+ index) :end close)
                                         (setf index (1+ end)))
                                  (progn (write-char character out) (incf index))))
                            (progn (write-char character out) (incf index)))))
                     (t (write-char character out) (incf index)))))
    (collapse-spaces (get-output-stream-string out))))

(defun prefix-of (prefix string)
  (and (>= (length string) (length prefix)) (string= prefix string :end2 (length prefix))))

(defun suffix-of (suffix string)
  (and (>= (length string) (length suffix))
       (string= suffix string :start2 (- (length string) (length suffix)))))

(defun signed-value-p (text digits)
  "TEXT as '+5%' or '-20Hz': a sign, at most DIGITS digits, then UNIT."
  (and (> (length text) 2)
       (member (char text 0) '(#\+ #\-))
       (let ((unit (if (char= (char text (1- (length text))) #\%) "%" "Hz")))
         (and (suffix-of unit text)
              (let* ((body (subseq text 1 (- (length text) (length unit))))
                     (limit (if (string= unit "%") 2 3)))
                (and (plusp (length body))
                     (<= (length body) limit)
                     (every #'digit-char-p body)))))))

(defun edge-voice-p (voice)
  "VOICE as the service names one: en-US-AndrewMultilingualNeural, es-ES-ElviraNeural."
  (let ((parts (uiop:split-string voice :separator "-")))
    (and (member (length parts) '(3 4))
         (let ((language (first parts)) (region (second parts)))
           (and (<= 2 (length language) 3)
                (every (lambda (character) (char<= #\a character #\z)) language)
                (<= 2 (length region) 4)
                (every #'alpha-char-p region)
                (suffix-of "Neural" (car (last parts)))
                (> (length (car (last parts))) (length "Neural")))))))

;;; --- arguments the model sent -----------------------------------------------------

(defun argument (args key)
  "ARGS's member KEY as text (trimmed, empty as NIL), or the value itself."
  (let ((value (and (hash-table-p args) (gethash key args))))
    (cond ((stringp value) (let ((trimmed (nlk:trimmed value)))
                              (and (plusp (length trimmed)) trimmed)))
          ((or (null value) (eq value :null)) nil)
          (t value))))

(defun argument-list (args key)
  "ARGS's member KEY as a list of its items (a vector from JSON, a list in a test)."
  (let ((value (and (hash-table-p args) (gethash key args))))
    (cond ((vectorp value) (coerce value 'list))
          ((listp value) value)
          (t nil))))

(defun answered-object (&rest pairs)
  "One JSON object from alternating key/value PAIRS, written the way the original
answered its tools."
  (let ((object (nlk:make-json-object)))
    (loop for (key value) on pairs by #'cddr
          when (not (null value))
            do (setf (gethash key object) value))
    object))

(defun json-true () (nlk:json-verbatim (sb-ext:string-to-octets "true" :external-format :utf-8)))
(defun json-false () (nlk:json-verbatim (sb-ext:string-to-octets "false" :external-format :utf-8)))

(defun answered-text (object) (nlk:encode-json-object object))

(defun answered-error (message &optional hint)
  "The one answer a refusal takes: {\"success\": false, \"error\": ..., \"hint\": ...}."
  (answered-text (answered-object "success" (json-false) "error" message
                                   "hint" (and hint (plusp (length hint)) hint))))

;;; --- the script ------------------------------------------------------------------

(defstruct (host (:constructor make-host (name voice rate pitch)))
  (name "" :read-only t) (voice "" :read-only t) (rate "+0%" :read-only t) (pitch "+0Hz" :read-only t))

(defstruct (line (:constructor make-line (host text chapter)))
  (host nil :read-only t) (text "" :read-only t) (chapter "" :read-only t))

(defstruct (script (:constructor make-script (title hosts lines description chapters chars)))
  (title "" :read-only t) (hosts nil :read-only t) (lines nil :read-only t)
  (description "" :read-only t) (chapters nil :read-only t) (chars 0 :read-only t))

(defun fail-script (format-string &rest arguments)
  (error 'script-error :message (apply #'format nil format-string arguments)
                       :hint "Fix the script and call the tool again."))

(defun parse-hosts (raw default-voices)
  "RAW as the original's parse_hosts read them, one host per entry."
  (let* ((items (cond ((null raw) (list (nlk:json-object "name" "Alex")
                                         (nlk:json-object "name" "Sam")))
                      ((vectorp raw) (coerce raw 'list))
                      ((listp raw) raw)
                      (t (fail-script "hosts must be a list of 1 to 3 hosts."))))
         (hosts '()))
    (unless (<= 1 (length items) 3)
      (fail-script "hosts must be a list of 1 to 3 hosts."))
    (loop for item in items
          for index from 0
          do (let* ((name (cond ((stringp item) item)
                                ((hash-table-p item) (argument item "name"))
                                (t nil)))
                    (voice (or (and (hash-table-p item) (argument item "voice"))
                               (nth (mod index (length default-voices)) default-voices)))
                    (rate (or (and (hash-table-p item) (argument item "rate")) "+0%"))
                    (pitch (or (and (hash-table-p item) (argument item "pitch")) "+0Hz")))
               (unless (and name (plusp (length name)))
                 (fail-script "Host ~a needs a name." (1+ index)))
               (unless (edge-voice-p voice)
                 (fail-script "~s is not an Edge voice name." voice))
               (unless (and (signed-value-p rate 2) (suffix-of "%" rate))
                 (fail-script "rate looks like '+10%' or '-5%'; pitch looks like '+0Hz' or '-20Hz'."))
               (unless (and (signed-value-p pitch 3) (suffix-of "Hz" pitch))
                 (fail-script "rate looks like '+10%' or '-5%'; pitch looks like '+0Hz' or '-20Hz'."))
               (push (make-host name voice rate pitch) hosts)))
    (setf hosts (nreverse hosts))
    (let ((names (mapcar (lambda (host) (string-downcase (host-name host))) hosts)))
      (unless (= (length names) (length (remove-duplicates names :test #'string=)))
        (fail-script "Host names must be different."))
      hosts)))

(defun parse-script (args default-voices max-lines max-chars)
  "ARGS as the original's parse_script read it: title, hosts, lines, chapters."
  (let ((title (clean-text (argument args "title"))))
    (unless (plusp (length title)) (fail-script "title is required."))
    (let* ((hosts (parse-hosts (argument args "hosts") default-voices))
           (by-name (make-hash-table :test 'equal)))
      (dolist (host hosts) (setf (gethash (string-downcase (host-name host)) by-name) host))
      (let ((raw-lines (argument-list args "lines")))
        (unless raw-lines (fail-script "lines must be a non-empty list of {speaker, text}."))
        (when (> (length raw-lines) max-lines)
          (fail-script "Too many lines (~a); the limit is ~a. Merge short turns." (length raw-lines) max-lines))
        (let ((chapter "") (chapters '()) (lines '()) (chars 0))
          (loop for item in raw-lines
                for index from 1
                do (unless (hash-table-p item)
                     (fail-script "Line ~a must be an object with speaker and text." index))
                   (let* ((speaker (string-downcase (or (argument item "speaker") "")))
                          (host (or (gethash speaker by-name)
                                    (and (= 1 (length hosts)) (zerop (length speaker)) (first hosts)))))
                     (unless host
                       (fail-script "Line ~a: speaker ~s is not one of the hosts (~a)."
                                    index (argument item "speaker")
                                    (join-strings (mapcar #'host-name hosts) ", ")))
                     (let ((text (clean-text (argument item "text"))))
                       (unless (plusp (length text)) (fail-script "Line ~a has no text." index))
                       (when (> (length text) +max-line-chars+)
                         (fail-script "Line ~a is ~a characters; keep turns under ~a."
                                      index (length text) +max-line-chars+))
                       (let ((new-chapter (clean-text (argument item "chapter"))))
                         (when (and (plusp (length new-chapter)) (not (string= new-chapter chapter)))
                           (setf chapter new-chapter)
                           (pushnew chapter chapters :test #'string=)))
                       (incf chars (length text))
                       (push (make-line host text chapter) lines))))
          (setf lines (nreverse lines) chapters (nreverse chapters))
          (when (> chars max-chars)
            (fail-script "The script is ~a characters; the limit is ~a. Tighten it." chars max-chars))
          (make-script title hosts lines (clean-text (argument args "description")) chapters chars))))))

;;; --- speaking a line ----------------------------------------------------------------

(defvar *synth* nil
  "The synthesizer every line goes through: (TEXT VOICE RATE PITCH PATH) writes
the audio. A function, so a test speaks the script without a socket.")

(defvar *synth-attempts* 3 "Tries per line before the episode gives up.")

(defun random-request-id ()
  (format nil "~32,'0X" (random (expt 16 32))))

(defun unix-now () (- (get-universal-time) 2208988800))

(defun edge-request-token ()
  "The Sec-MS-GEC header the service now asks for: SHA-256 of the rounded
Windows file time followed by the client token, in upper-case hex."
  (let* ((file-time (+ (* 11644473600 (unix-now)) 0))
         (rounded (* 300000000 (floor file-time 300000000)))
         (text (format nil "~a~a" rounded +edge-token+)))
    (string-upcase (ironclad:byte-array-to-hex-string
                    (ironclad:digest-sequence :sha256 (sb-ext:string-to-octets text :external-format :ascii))))))

(defun xml-escape (text)
  (with-output-to-string (out)
    (loop for character across text
          do (case character
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\' (write-string "&apos;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char character out))))))

(defun ssml-for (text voice rate pitch &key (language "en-US"))
  (format nil "<speak version='1.0' xmlns='http://www.w3.org/2001/10/synthesis' xml:lang='~a'><voice name='~a'><prosody pitch='~a' rate='~a' volume='+0%'>~a</prosody></voice></speak>"
          language voice pitch rate (xml-escape text)))

(defun speech-config-frame ()
  (format nil "X-Timestamp:~a~c~cContent-Type:application/json; charset=utf-8~c~cPath:speech.config~c~c~c~c{\"context\":{\"synthesis\":{\"audio\":{\"metadataoptions\":{\"sentenceBoundaryEnabled\":\"false\",\"wordBoundaryEnabled\":\"false\"},\"outputFormat\":\"audio-24khz-48kbitrate-mono-mp3\"}}}}"
           (unix-now) #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed))

(defun ssml-frame (ssml)
  (format nil "X-RequestId:~a~c~cContent-Type:application/ssml+xml~c~cX-Timestamp:~a~c~cPath:ssml~c~c~c~c~a"
          (random-request-id) #\Return #\Linefeed #\Return #\Linefeed (unix-now)
          #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed ssml))

(defun message-parts (message)
  "MESSAGE as (VALUES PAYLOAD BINARY-P): text or the audio octets."
  (cond ((stringp message) (values message nil))
        ((and (vectorp message) (plusp (length message))
              (typep (aref message 0) '(unsigned-byte 8)))
         (values message t))
        ((vectorp message) (values message nil))
        (t (let ((reader (find-symbol "PAYLOAD" "FAST-WEBSOCKET")))
             (let ((payload (and reader (ignore-errors (funcall reader message)))))
               (if payload
                   (values payload (not (stringp payload)))
                   (values "" nil)))))))

(defun edge-audio-octets (payload)
  "PAYLOAD without the two-byte header length and the header text in front of it."
  (if (and (>= (length payload) 2)
           (typep (aref payload 0) '(unsigned-byte 8)))
      (let ((size (logior (ash (aref payload 0) 8) (aref payload 1))))
        (subseq payload (min (length payload) (+ 2 size))))
      (subseq payload (min (length payload) 2))))

(defun edge-synthesize-line (text voice rate pitch path)
  "One line, spoken by the Edge read-aloud socket and written to PATH."
  (let* ((connection-id (random-request-id))
         (url (format nil "~a?TrustedClientToken=~a&ConnectionId=~a" +edge-socket+ +edge-token+ connection-id))
         (audio (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (done (bt2:make-semaphore :name "podcast-voice"))
         (closed nil)
         (ws (wsd:make-client url
                               :additional-headers
                               (list (cons "Origin" "chrome-extension://jdiccldimpdaibmpdkjnbmckianbfold")
                                     (cons "Pragma" "no-cache")
                                     (cons "Cache-Control" "no-cache")
                                     (cons "Sec-MS-GEC" (edge-request-token))
                                     (cons "Sec-MS-GEC-Version" "1-130.0.2849.68")))))
    (labels ((finish () (unless closed (setf closed t) (ignore-errors (bt2:signal-semaphore done))))
             (on-message (message)
               (multiple-value-bind (payload binary) (message-parts message)
                 (if binary
                     (let ((octets (edge-audio-octets payload)))
                       (loop for octet across octets do (vector-push-extend octet audio)))
                     (when (and (stringp payload) (search "Path:turn.end" payload))
                       (finish))))))
      (wsd:on :message ws (lambda (message) (handler-case (on-message message) (error () nil))))
      (wsd:on :close ws (lambda (&key &allow-other-keys) (declare) (finish)))
      (wsd:on :error ws (lambda (reason) (declare (ignore reason)) (finish)))
      (unwind-protect
           (progn
             (wsd:start-connection ws)
             (wsd:send ws (speech-config-frame) :type :text)
             (wsd:send ws (ssml-frame (ssml-for text voice rate pitch)) :type :text)
             (bt2:wait-on-semaphore done :timeout 90)
             (when (zerop (length audio))
               (error 'synth-error :hint "The voice service answered no audio; try again, or another voice (podcast_voices)."))
             (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                        :if-exists :supersede :if-does-not-exist :create)
               (write-sequence audio out)))
        (ignore-errors (nlk:sever-websocket ws))))))

(defun synthesize-one (line pcm-ignored path)
  (declare (ignore pcm-ignored))
  (loop for attempt from 1 to *synth-attempts*
        do (handler-case
               (progn
                 (funcall (or *synth* #'edge-synthesize-line)
                          (line-text line) (host-voice (line-host line))
                          (host-rate (line-host line)) (host-pitch (line-host line)) path)
                 (unless (and (probe-file path) (plusp (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in))))
                   (error 'synth-error :hint "The voice service returned no audio."))
                 (return path))
             (error (condition)
               (when (= attempt *synth-attempts*)
                 (error 'synth-error :hint (format nil "Could not synthesize a line for ~a (~a): ~a"
                                                   (host-name (line-host line)) (host-voice (line-host line)) condition)))
               (sleep (* attempt attempt)))))
  path)

(defun synthesize-lines (lines workdir)
  "Every line spoken into WORKDIR, one clip each, in order."
  (ensure-directories-exist workdir)
  (loop for line in lines
        for index from 0
        for path = (merge-pathnames (format nil "line_~3,'0d.mp3" index) workdir)
        collect (synthesize-one line nil path)))

;;; --- the voice list --------------------------------------------------------------

(defvar *fetch-voices* nil
  "How the voice list is fetched: a function of no arguments answering the
decoded JSON array. A test stubs it; the cell's own dials the service.")

(defun fetch-edge-voices ()
  (multiple-value-bind (body status) (nlk:http :get +voices-url+ :timeout 30)
    (unless (eql status 200)
      (error 'synth-error :hint (format nil "Could not fetch the Edge voice list: HTTP ~a." status)))
    (let ((decoded (nlk:decode-json (if (stringp body) body (sb-ext:octets-to-string body :external-format :utf-8)))))
      (if (vectorp decoded) (coerce decoded 'list) decoded))))

(defun voice-list (&optional (root nil))
  "The service's voices, cached in ROOT for a week as the original cached them."
  (if *fetch-voices*
      (funcall *fetch-voices*)
      (let ((cache (and root (merge-pathnames "voices.json" root))))
        (labels ((fresh-p (path)
                   (and (probe-file path)
                        (< (- (get-universal-time) (file-write-date path)) +voice-cache-seconds+)))
                 (read-cached (path)
                   (ignore-errors (nlk:decode-json (uiop:read-file-string path)))))
          (or (and cache (fresh-p cache) (read-cached cache))
              (let ((voices (fetch-edge-voices)))
                (when (and cache voices)
                  (ensure-directories-exist cache)
                  (with-open-file (out cache :direction :output :if-exists :supersede :if-does-not-exist :create)
                    (write-string (nlk:encode-json-object (coerce voices 'vector)) out)))
                voices))))))

(defun voice-row (voice)
  (let ((tags (and (hash-table-p voice) (gethash "VoiceTag" voice))))
    (answered-object
     "voice" (and (hash-table-p voice) (gethash "ShortName" voice))
     "locale" (and (hash-table-p voice) (gethash "Locale" voice))
     "gender" (and (hash-table-p voice) (gethash "Gender" voice))
     "personality" (and (hash-table-p tags)
                        (let ((list (gethash "VoicePersonalities" tags)))
                          (and list (join-strings (coerce list 'list) ", ")))))))

;;; --- ffmpeg, and what it does -------------------------------------------------------

(defun probe-program (name)
  (when (and name (plusp (length name)))
    (handler-case
        (multiple-value-bind (output error-output code)
            (uiop:run-program (list name "-version") :output :string :error-output :string
                                                        :ignore-error-status t)
          (declare (ignore output error-output))
          (and (eql code 0) name))
      (error () nil))))

(defvar *ffmpeg-probe* nil
  "How the mastering program is looked for; a test binds it to answer NIL.")

(defun find-ffmpeg ()
  "The ffmpeg this episode is mastered with, or NIL: clips are then joined as they are."
  (if *ffmpeg-probe* (funcall *ffmpeg-probe*)
      (let ((configured (or (setting :ffmpeg-path) "")))
        (if (plusp (length configured))
            (probe-program configured)
            (probe-program "ffmpeg")))))

(defun run-ffmpeg (argv)
  (multiple-value-bind (output error-output code)
      (handler-case (uiop:run-program argv :output :string :error-output :string :ignore-error-status t)
        (error (condition) (error 'mix-error :hint (format nil "ffmpeg could not run: ~a" condition))))
    (declare (ignore output))
    (unless (eql code 0)
      (error 'mix-error :hint (format nil "ffmpeg failed: ~a"
                                      (let ((text (or error-output "")))
                                        (subseq text (max 0 (- (length text) 400)))))))))

(defun to-samples (ffmpeg source destination)
  "SOURCE as mono signed 16-bit 24 kHz samples: no container to parse."
  (run-ffmpeg (list ffmpeg "-y" "-hide_banner" "-loglevel" "error" "-i" (uiop:native-namestring source)
                     "-ac" "1" "-ar" (princ-to-string +sample-rate+) "-f" "s16le"
                     (uiop:native-namestring destination))))

(defun read-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun write-silence (out seconds)
  (loop repeat (round (* +sample-rate+ 2 seconds)) do (write-byte 0 out)))

(defun write-octets (out octets) (write-sequence octets out))

(defun timed-line (index start end) (list index start end))

(defun pause-after (lines index pause)
  "The gap after line INDEX: half a turn, a whole one for a new speaker, 2.6 for a chapter."
  (let ((here (nth index lines)) (next (nth (1+ index) lines)))
    (if (or (null next) (null here)) 0.0
        (cond ((not (string= (line-chapter next) (line-chapter here))) (* pause 2.6))
              ((not (eq (line-host next) (line-host here))) pause)
              (t (* pause 0.5))))))

(defparameter +chime+
  "aevalsrc=exprs='0.32*sin(2*PI*659.25*t)*exp(-3.2*t)+0.24*sin(2*PI*987.77*t)*exp(-2.6*t)+0.16*sin(2*PI*1318.5*t)*exp(-2.2*t)':s=24000:d=1.6"
  "The intro chime's own expression, as the original wrote it.")

(defstruct (episode (:constructor make-episode (path duration timeline mastered)))
  (path nil :read-only t) (duration 0.0 :read-only t) (timeline nil :read-only t)
  (mastered nil :read-only t))

(defun metadata-file (script timeline total path)
  "The FFMETADATA1 file: title, description, and one chapter per section."
  (labels ((escaped (text)
             (with-output-to-string (out)
               (loop for character across text
                     do (if (member character '(#\= #\: #\; #\# #\\ #\Newline))
                            (progn (write-char #\\ out) (write-char character out))
                            (write-char character out)))))
           (chapter-start (timed)
             (line-chapter (nth (first timed) (script-lines script)))))
    (let ((rows (list ";FFMETADATA1"
                       (format nil "title=~a" (escaped (script-title script)))
                       "artist=Podcast"
                       "genre=Podcast"
                       (format nil "comment=~a" (escaped (script-description script)))))
          (starts '()) (seen ""))
      (dolist (timed timeline)
        (let ((chapter (chapter-start timed)))
          (unless (or (zerop (length chapter)) (string= chapter seen))
            (setf seen chapter)
            (push (cons (second timed) chapter) starts))))
      (setf starts (nreverse starts))
      (loop for (start . title) in starts
            for rest on (cdr starts)
            for end = (if rest (caar rest) total)
            do (setf rows (append rows (list "[CHAPTER]" "TIMEBASE=1/1000"
                                             (format nil "START=~a" (round (* start 1000)))
                                             (format nil "END=~a" (round (* end 1000)))
                                             (format nil "title=~a" (escaped title))))))
      (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-line (join-strings rows (string #\Newline)) out)))))

(defun join-raw (clips out)
  "No ffmpeg: one encoder's MP3 clips concatenate, timed by their size."
  (let ((cursor 0.0) (timeline '()))
    (with-open-file (destination out :direction :output :element-type '(unsigned-byte 8)
                                     :if-exists :supersede :if-does-not-exist :create)
      (loop for clip in clips
            for index from 0
            for octets = (read-octets clip)
            for length = (/ (length octets) (float +mp3-bytes-per-second+))
            do (write-sequence octets destination)
               (push (timed-line index cursor (+ cursor length)) timeline)
               (incf cursor length)))
    (make-episode out cursor (nreverse timeline) nil)))

(defun master (script clips out &key ffmpeg pause chime)
  "The episode OUT from CLIPS: pauses between turns, an intro chime, one
loudness pass, chapters; unmastered when ffmpeg is missing."
  (unless ffmpeg (return-from master (join-raw clips out)))
  (let* ((work (merge-pathnames "work/" (uiop:pathname-directory-pathname out)))
         (lines (script-lines script)))
    (ensure-directories-exist work)
    (labels ((sample (path) (merge-pathnames (format nil "~a.pcm" (pathname-name path)) work)))
      (unwind-protect
           (let ((joined (merge-pathnames "joined.pcm" work))
                 (timeline '()) (cursor 0.0) (metadata (merge-pathnames "chapters.txt" work)))
             (dolist (clip clips) (to-samples ffmpeg clip (sample clip)))
             (with-open-file (destination joined :direction :output :element-type '(unsigned-byte 8)
                                                  :if-exists :supersede :if-does-not-exist :create)
               (when chime
                 (let ((chime-path (merge-pathnames "chime.pcm" work)))
                   (run-ffmpeg (list ffmpeg "-y" "-hide_banner" "-loglevel" "error"
                                      "-f" "lavfi" "-i" +chime+ "-ac" "1" "-ar" (princ-to-string +sample-rate+)
                                      "-f" "s16le" (uiop:native-namestring chime-path)))
                   (write-sequence (read-octets chime-path) destination)
                   (write-silence destination 0.25)
                   (incf cursor 1.85)))
               (loop for clip in clips
                     for index from 0
                     for start = cursor
                     for samples = (sample clip)
                     do (write-sequence (read-octets samples) destination)
                        (incf cursor (/ (with-open-file (in samples :element-type '(unsigned-byte 8)) (file-length in)) (* 2.0 +sample-rate+)))
                        (push (timed-line index start cursor) timeline)
                        (let ((gap (pause-after lines index pause)))
                          (write-silence destination gap)
                          (incf cursor gap)))
               (write-silence destination 0.6)
               (incf cursor 0.6))
             (setf timeline (nreverse timeline))
             (metadata-file script timeline cursor metadata)
             (run-ffmpeg (list ffmpeg "-y" "-hide_banner" "-loglevel" "error"
                                "-f" "s16le" "-ar" (princ-to-string +sample-rate+) "-ac" "1"
                                "-i" (uiop:native-namestring joined)
                                "-i" (uiop:native-namestring metadata)
                                "-map" "0:a" "-map_metadata" "1" "-map_chapters" "1"
                                "-af" "loudnorm=I=-16:TP=-1.5:LRA=11,aresample=44100"
                                "-c:a" "libmp3lame" "-b:a" "128k" "-id3v2_version" "3"
                                (uiop:native-namestring out)))
             (make-episode out cursor timeline t))
        (ignore-errors (uiop:delete-directory-tree work :validate t))))))

;;; --- keeping the episode -----------------------------------------------------------

(defun episodes-root ()
  "Where episodes live: the section's episodes_dir, else <home>/podcast/."
  (let ((configured (setting :episodes-dir)))
    (uiop:ensure-directory-pathname
     (if (and configured (plusp (length (string configured))))
         (nlk:expand-home (string configured))
         (nlk:home "podcast/")))))

(defun slugify (title)
  (let* ((lower (string-downcase title))
         (out (make-string-output-stream))
         (dash t))
    (loop for character across lower
          do (if (or (char<= #\a character #\z) (char<= #\0 character #\9))
                 (progn (write-char character out) (setf dash nil))
                 (unless dash (write-char #\- out) (setf dash t))))
    (let ((slug (string-trim "-" (get-output-stream-string out))))
      (if (plusp (length slug)) (subseq slug 0 (min 48 (length slug))) "episode"))))

(defun stamp-now ()
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time))
    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" year month day hour minute second)))

(defun clock (seconds)
  (let ((whole (round seconds)))
    (format nil "~2,'0d:~2,'0d" (floor whole 60) (mod whole 60))))

(defun episode-folder (name) (merge-pathnames (format nil "episodes/~a/" name) (episodes-root)))

(defun new-episode-dir (title)
  (let* ((root (episodes-root))
         (base (format nil "~a-~a" (stamp-now) (slugify title)))
         (path (merge-pathnames (format nil "episodes/~a/" base) root))
         (candidate path) (n 2))
    (loop while (probe-file candidate)
          do (setf candidate (merge-pathnames (format nil "episodes/~a-~a/" base n) root))
             (incf n))
    (ensure-directories-exist candidate)
    candidate))

(defun write-transcript (script episode path)
  (let ((rows (list (format nil "# ~a" (script-title script)) ""))
        (chapter nil))
    (when (plusp (length (script-description script)))
      (setf rows (append rows (list (script-description script) ""))))
    (setf rows (append rows
                       (list (format nil "Hosts: ~a"
                                     (join-strings (mapcar (lambda (host) (format nil "~a (~a)" (host-name host) (host-voice host)))
                                                           (script-hosts script))
                                                   ", "))
                             "")))
    (dolist (timed (episode-timeline episode))
      (let ((line (nth (first timed) (script-lines script))))
        (unless (or (zerop (length (line-chapter line))) (equal (line-chapter line) chapter))
          (setf chapter (line-chapter line))
          (setf rows (append rows (list "" (format nil "## ~a  [~a]" chapter (clock (second timed))) ""))))
        (setf rows (append rows (list (format nil "**[~a] ~a:** ~a"
                                              (clock (second timed)) (host-name (line-host line)) (line-text line)))))))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-line (join-strings rows (string #\Newline)) out))))

(defun chapter-rows (script episode)
  (let ((rows '()) (seen ""))
    (dolist (timed (episode-timeline episode))
      (let ((name (line-chapter (nth (first timed) (script-lines script)))))
        (unless (or (zerop (length name)) (string= name seen))
          (setf seen name)
          (push (answered-object "title" name "start" (clock (second timed))) rows))))
    (coerce (nreverse rows) 'vector)))

(defun save-meta (script episode folder)
  (let* ((meta (answered-object
                "title" (script-title script)
                "description" (script-description script)
                "created" (multiple-value-bind (second minute hour day month year)
                              (decode-universal-time (get-universal-time))
                            (declare (ignore second))
                            (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute))
                "duration" (rationalize (round (* 10 (episode-duration episode))))
                "duration_text" (clock (episode-duration episode))
                "hosts" (coerce (mapcar (lambda (host) (answered-object "name" (host-name host)
                                                                        "voice" (host-voice host)))
                                        (script-hosts script))
                                'vector)
                "lines" (length (script-lines script))
                "chapters" (chapter-rows script episode)
                "mastered" (if (episode-mastered episode) (json-true) (json-false))
                "audio" (uiop:native-namestring (episode-path episode))
                "transcript" (uiop:native-namestring (merge-pathnames "transcript.md" folder))))
         (path (merge-pathnames "episode.json" folder)))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string (nlk:encode-json-object meta) out))
    meta))

(defun list-episodes (&optional (limit 10))
  "Every kept episode, newest first, as the original listed them."
  (let ((folder (merge-pathnames "episodes/" (episodes-root)))
        (rows '()))
    (when (probe-file folder)
      (dolist (child (sort (uiop:subdirectories folder) #'string> :key #'namestring))
        (let ((meta-file (merge-pathnames "episode.json" child))
              (audio (merge-pathnames "episode.mp3" child)))
          (when (and (probe-file meta-file) (probe-file audio))
            (let ((meta (ignore-errors (nlk:decode-json (uiop:read-file-string meta-file)))))
              (when (hash-table-p meta)
                (push (answered-object
                       "id" (car (last (pathname-directory child)))
                       "title" (gethash "title" meta)
                       "created" (gethash "created" meta)
                       "duration_text" (gethash "duration_text" meta)
                       "audio" (gethash "audio" meta)
                       "transcript" (gethash "transcript" meta)
                       "chapters" (gethash "chapters" meta))
                      rows)))))))
    (if (> (length rows) limit) (subseq rows 0 limit) rows)))

;;; --- producing one ----------------------------------------------------------------

(defun produce-episode (args)
  "The whole original produce(): script, voices, mastering, transcript, metadata."
  (let* ((default-voices (list (or (setting :host-a-voice) (first +default-voices+))
                                (or (setting :host-b-voice) (second +default-voices+))
                                (third +default-voices+)))
         (script (parse-script args default-voices
                               (max 1 (min 400 (or (setting :max-lines) 160)))
                               (max 200 (min 100000 (or (setting :max-chars) 24000)))))
         (started (get-internal-real-time))
         (folder (new-episode-dir (script-title script)))
         (episode nil))
    (handler-case
        (let* ((clips (synthesize-lines (script-lines script) (merge-pathnames "clips/" folder)))
               (ffmpeg (find-ffmpeg))
               (result (master script clips (merge-pathnames "episode.mp3" folder)
                               :ffmpeg ffmpeg
                               :pause (/ (or (setting :pause-ms) 350) 1000.0)
                               :chime (if (null (setting :intro-chime)) nil t))))
          (setf episode result)
          (ignore-errors (uiop:delete-directory-tree (merge-pathnames "clips/" folder) :validate t))
          (write-transcript script result (merge-pathnames "transcript.md" folder))
          (let* ((meta (save-meta script result folder))
                 (answer (answered-object
                          "success" (json-true)
                          "episode_id" (car (last (pathname-directory folder)))
                          "title" (script-title script)
                          "duration" (gethash "duration_text" meta)
                          "hosts" (gethash "hosts" meta)
                          "lines" (length (script-lines script))
                          "chapters" (gethash "chapters" meta)
                          "audio" (uiop:native-namestring (merge-pathnames "episode.mp3" folder))
                          "transcript" (uiop:native-namestring (merge-pathnames "transcript.md" folder))
                          "mastered" (gethash "mastered" meta)
                          "render_seconds" (round (/ (- (get-internal-real-time) started)
                                                      internal-time-units-per-second))
                          "note" (unless (episode-mastered result)
                                    "ffmpeg was not found, so the episode has no intro chime, pauses, loudness pass or embedded chapters."))))
            (answered-text answer)))
      (error (condition)
        (unless episode (ignore-errors (uiop:delete-directory-tree folder :validate t)))
        (error condition)))))

(defun produce-tool (args)
  "podcast_produce: the episode, or the one sentence that says how to fix the script."
  (handler-case (produce-episode args)
    (script-error (condition) (answered-error (princ-to-string condition) (script-error-hint condition)))
    (synth-error (condition) (answered-error (princ-to-string condition) (synth-error-hint condition)))
    (mix-error (condition) (answered-error (princ-to-string condition) (mix-error-hint condition)))
    (podcast-error (condition) (answered-error (princ-to-string condition)))
    (error (condition) (answered-error (format nil "Unexpected podcast error: ~a" condition)))))

;;; --- the other two tools ------------------------------------------------------------

(defun voices-tool (args)
  "podcast_voices: the service's voices for a language, and this cell's defaults."
  (handler-case
      (let* ((locale (string-downcase (or (argument args "locale") "en")))
             (gender (string-downcase (or (argument args "gender") "")))
             (rows '()))
        (dolist (voice (voice-list (episodes-root)))
          (let ((voice-locale (string-downcase (or (and (hash-table-p voice) (gethash "Locale" voice)) "")))
                (voice-gender (string-downcase (or (and (hash-table-p voice) (gethash "Gender" voice)) ""))))
            (when (and (prefix-of locale voice-locale)
                       (or (zerop (length gender)) (string= gender voice-gender)))
              (push (voice-row voice) rows))))
        (setf rows (nreverse rows))
        (answered-text (answered-object "success" (json-true) "locale" locale "count" (length rows)
                                         "voices" (coerce (if (> (length rows) 60) (subseq rows 0 60) rows) 'vector)
                                         "defaults" (vector (or (setting :host-a-voice) (first +default-voices+))
                                                            (or (setting :host-b-voice) (second +default-voices+))))))
    (synth-error (condition) (answered-error (princ-to-string condition) (synth-error-hint condition)))
    (error (condition) (answered-error (format nil "Unexpected podcast error: ~a" condition)))))

(defun episode-answer (row)
  (answered-text (answered-object "success" (json-true) "episode" row
                                  "deliver" "Give the audio path in your reply so the operator can open it.")))

(defun episodes-tool (args)
  "podcast_episodes: what was produced, newest first, or one episode again by id."
  (handler-case
      (let* ((wanted (argument args "id"))
             (limit (max 1 (min 50 (or (let ((value (argument args "limit")))
                                         (and (integerp value) value))
                                       10))))
             (rows (list-episodes (if wanted 500 limit))))
        (if wanted
            (let ((match (find wanted rows :key (lambda (row) (gethash "id" row)) :test #'string=)))
              (if match
                  (episode-answer match)
                  (answered-text (answered-object
                                  "success" (json-false)
                                  "error" (format nil "No episode with id ~s." wanted)
                                  "hint" "Call podcast_episodes without id to list them."))))
            (answered-text (answered-object "success" (json-true) "count" (length rows)
                                            "episodes" (coerce rows 'vector)
                                            "folder" (uiop:native-namestring
                                                      (merge-pathnames "episodes/" (episodes-root)))))))
    (error (condition) (answered-error (format nil "Unexpected podcast error: ~a" condition)))))

;;; --- /podcast ---------------------------------------------------------------------

(defun text-list ()
  (let ((rows (list-episodes 10)))
    (if (null rows)
        "No episodes yet. Try: \"Make a 3-minute podcast about <a link or topic>.\""
        (join-strings (cons "Recent episodes:"
                             (mapcar (lambda (row)
                                       (format nil "  ~a  ~a  ~a~%    ~a"
                                               (or (gethash "created" row) "")
                                               (or (gethash "duration_text" row) "")
                                               (or (gethash "title" row) "")
                                               (or (gethash "audio" row) "")))
                                     rows))
                      (string #\Newline)))))

(defun text-voices (locale)
  "The /podcast voices LOCALE line: the names, genders and personalities."
  (let ((args (nlk:make-json-object)))
    (setf (gethash "locale" args) locale)
    (let ((decoded (nlk:decode-json (voices-tool args))))
      (if (null (gethash "success" decoded))
          (format nil "Podcast error: ~a" (gethash "error" decoded))
          (join-strings
           (cons (format nil "~a voices for ~a:" (gethash "count" decoded) locale)
                 (loop for row across (gethash "voices" decoded) collect
                       (format nil "  ~a  (~a~@[, ~a~])"
                               (gethash "voice" row) (gethash "gender" row)
                               (gethash "personality" row))))
           (string #\Newline))))))

(defun text-doctor ()
  (format nil "voice service: ~a~%ffmpeg:         ~a~%episodes:       ~a"
          "Microsoft Edge read-aloud (no key)"
          (or (find-ffmpeg) "not found (episodes are joined without chime, pauses or chapters)")
          (uiop:native-namestring (episodes-root))))

(defun podcast-command (args session-id)
  "The /podcast command: `list', `voices [locale]', `doctor', or nothing for usage."
  (declare (ignore session-id))
  (let* ((words (uiop:split-string (or (nlk:trimmed (or args "")) "") :separator " "))
         (words (remove "" words :test #'string=))
         (verb (string-downcase (or (first words) "list"))))
    (cond ((string= verb "list") (text-list))
          ((string= verb "voices") (text-voices (or (second words) "en")))
          ((string= verb "doctor") (text-doctor))
          (t (join-strings (list "Usage: /podcast [list|voices [locale]|doctor]"
                                  "Ask for an episode: \"Make a 3-minute podcast about <a link or topic>.\"")
                            (string #\Newline))))))

;;; --- the tool schemas ----------------------------------------------------------------

(defun host-schema ()
  (nlk:json-object "type" "object"
                    "description" "One host: a name, and an Edge voice from podcast_voices."
                    "properties" (nlk:json-object
                                  "name" (nlk:json-object "type" "string")
                                  "voice" (nlk:json-object "type" "string"
                                                            "description" "e.g. en-US-AndrewMultilingualNeural")
                                  "rate" (nlk:json-object "type" "string" "description" "e.g. +5% or -10%")
                                  "pitch" (nlk:json-object "type" "string" "description" "e.g. +0Hz or -20Hz"))
                    "required" (vector "name")))

(defun line-schema ()
  (nlk:json-object "type" "object"
                    "description" "One turn of the conversation, in order."
                    "properties" (nlk:json-object
                                  "speaker" (nlk:json-object "type" "string" "description" "A host name.")
                                  "text" (nlk:json-object "type" "string" "description" "Spoken text only, no markdown.")
                                  "chapter" (nlk:json-object "type" "string"
                                                              "description" "Section title, set on the first line of a section."))
                    "required" (vector "speaker" "text")))

(defun produce-schema ()
  (list :description
        "Produce a podcast episode (MP3) from a script you write: each line is spoken by its host's voice, with pauses, an intro chime, normalised loudness, chapter markers and a timestamped transcript. Write the script from sources you actually read; (help :podcast) has the format."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                       "title" (nlk:json-object "type" "string" "description" "Episode title.")
                       "description" (nlk:json-object "type" "string"
                                                       "description" "One or two sentences for the episode notes.")
                       "hosts" (nlk:json-object "type" "array"
                                                 "description" "1-3 hosts; omit for the default pair (Alex and Sam)."
                                                 "items" (host-schema))
                       "lines" (nlk:json-object "type" "array"
                                                 "description" "The conversation in order."
                                                 "items" (line-schema)))
         "required" (vector "title" "lines"))))

(defun voices-schema ()
  (list :description "List the voices available for podcast hosts, filtered by language and gender."
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (nlk:json-object
                                   "locale" (nlk:json-object "type" "string"
                                                              "description" "Language or locale prefix: en, en-GB, es, hi-IN. Default en.")
                                   "gender" (nlk:json-object "type" "string"
                                                              "enum" (vector "Male" "Female"))))))

(defun episodes-schema ()
  (list :description "List podcast episodes produced so far, newest first, with audio and transcript paths; pass id to get one again."
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (nlk:json-object
                                   "id" (nlk:json-object "type" "string" "description" "Episode id from a listing.")
                                   "limit" (nlk:json-object "type" "integer"
                                                             "description" "How many to list (1-50, default 10).")))))

;;; --- the manual ---------------------------------------------------------------------

(defparameter +manual+
  "Podcast Studio turns a script into a mastered two-host MP3: podcast_produce (title, description, hosts, lines), podcast_voices (a locale's voices), podcast_episodes (what exists), and /podcast [list|voices [locale]|doctor].
Read the real sources first, then write the script. Two hosts by default: Alex explains and carries the structure, Sam asks what a listener would and pushes back; omit hosts for them. About 150 spoken words a minute, so a 3-minute episode is ~450 words and 20-30 lines; default 3-5 minutes. Shape: a cold-open hook, a short intro naming the source, 2-4 chapters (set `chapter' on the first line of each) and a takeaway close. Short sentences, contractions, real reactions; most lines under 40 words; no monologues. Spoken text only: no markdown, bullets, URLs, emoji or stage directions. Write numbers the way they are said. Every fact must come from the sources; say where a claim comes from.
Call podcast_produce, fix exactly what an error names, then reply with a 2-3 bullet summary, the duration and chapters, and the audio path. For another language call podcast_voices with a locale, then pass hosts with those voices and write in that language; for one narrator pass a single host.")

(nle:define-cell podcast
  ;; The section derives the setup panel, the primer, the refusal and the
  ;; typed values below from one declaration.
  (:section ("podcast")
    (:guide "nothing is required; a keyless voice service and ffmpeg on PATH need no setting")
    ("host_a_voice" :string :default "en-US-AndrewMultilingualNeural"
     :doc "Voice for the first default host (Alex): see /podcast voices.")
    ("host_b_voice" :string :default "en-US-AvaMultilingualNeural"
     :doc "Voice for the second default host (Sam).")
    ("pause_ms" :integer :default 350 :min 1 :doc "Pause between speakers, in milliseconds; longer between chapters.")
    ("intro_chime" :boolean :default t :doc "Play the short chime at the start of each episode.")
    ("max_lines" :integer :default 160 :min 1 :doc "Most script lines one episode may have.")
    ("max_chars" :integer :default 24000 :min 200 :doc "Most spoken characters one episode may have.")
    ("episodes_dir" :path :doc "Where episodes are kept; empty means <home>/podcast/.")
    ("ffmpeg_path" :path :doc "Path to ffmpeg; empty means find it on PATH."))
  (:tool "podcast_produce" (produce-schema) 'produce-tool)
  (:tool "podcast_voices" (voices-schema) 'voices-tool)
  (:tool "podcast_episodes" (episodes-schema) 'episodes-tool)
  (:command "podcast" 'podcast-command
            :description "Podcast episodes, voices and a doctor line"
            :argument-hint "[list|voices [locale]|doctor]")
  (:help :podcast "Podcast: a two-host episode script spoken by Edge voices into one mastered MP3" +manual+))
