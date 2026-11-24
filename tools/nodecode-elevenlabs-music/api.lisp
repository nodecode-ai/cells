;;;; api.lisp --- one ElevenLabs request: the settings, the key, the bytes, the readable error.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every tool goes through REQUEST: it reads the key out of the section (or
;;;; ELEVENLABS_API_KEY), sends one request to {api_base_url}{path} with the
;;;; xi-api-key header, and turns ElevenLabs' error body into a sentence the
;;;; model can act on. Only the condition's type is ever reported for a
;;;; transport failure, because a transport library's message can echo the
;;;; request headers -- the key among them.

(in-package #:nodecode-elevenlabs-music)

(defparameter +default-base+ "https://api.elevenlabs.io")
(defparameter +keys-url+ "https://elevenlabs.io/app/settings/api-keys")
(defparameter +models+ '("music_v2_5" "music_v2" "music_v1"))
(defparameter +plan-models+ '("music_v2" "music_v2_5"))
(defparameter +formats+
  (list (cons "mp3_44100_128" "mp3") (cons "mp3_44100_192" "mp3")
        (cons "mp3_48000_192" "mp3") (cons "mp3_48000_320" "mp3")
        (cons "opus_48000_128" "opus") (cons "opus_48000_192" "opus"))
  "The output_format choices and the extension each one's bytes are.")
(defparameter +legacy-formats+
  '("mp3_44100_128" "mp3_44100_192" "opus_48000_128" "opus_48000_192")
  "What the stem and video endpoints accept; their enum has no 48 kHz MP3.")
(defparameter +max-response-bytes+ (* 200 1024 1024)
  "No answer may exceed this: a song is a few megabytes, anything past it is not one.")
(defparameter +no-retry+
  "Retrying right away will not help: tell the user and let them decide when to try again.")

;;; --- settings -------------------------------------------------------------

(defun api-base ()
  "The base URL the section names, https (plain http only for loopback: the
key rides in a header and must not cross the network in cleartext)."
  (let ((url (string-right-trim "/" (or (setting :api-base-url) +default-base+))))
    (unless (and (stringp url)
                 (or (uiop:string-prefix-p "https://" url)
                     (some (lambda (prefix) (uiop:string-prefix-p prefix url))
                           '("http://localhost" "http://127.0.0.1" "http://[::1]"))))
      (fail "api_base_url must be an https URL (http only for localhost), not ~s" url))
    url))

(defun environment-key ()
  "ELEVENLABS_API_KEY from the environment, or NIL. A test binds it."
  (uiop:getenv "ELEVENLABS_API_KEY"))

(defun api-key ()
  "The operator's ElevenLabs key: the section's, else ELEVENLABS_API_KEY."
  (flet ((clean (text) (and (stringp text) (string-trim '(#\Space #\Tab) text))))
    (or (let ((key (clean (setting :api-key)))) (and key (plusp (length key)) key))
        (let ((key (clean (environment-key))))
          (and key (plusp (length key)) key))
        (fail "no ElevenLabs API key: set api_key in the elevenlabs-music section of ~
               config.jsonc (one from ~a), or export ELEVENLABS_API_KEY"
              +keys-url+))))

(defun output-format ()
  (or (setting :output-format) "mp3_44100_128"))

(defun legacy-format ()
  "OUTPUT-FORMAT where the stem and video endpoints know it, else 192 kbps MP3."
  (let ((format (output-format)))
    (if (member format +legacy-formats+ :test #'string=) format "mp3_44100_192")))

;;; --- octets ---------------------------------------------------------------

(defparameter +crlf+ (coerce (map 'list #'char-code (format nil "~c~c" #\Return #\LineFeed))
                              '(vector (unsigned-byte 8))))
(defparameter +blank-line+ (concatenate '(vector (unsigned-byte 8)) +crlf+ +crlf+))

(defun octets-of (text)
  (flexi-streams:string-to-octets text :external-format :utf-8))

(defun text-of (octets &key (start 0) (end (length octets)))
  (if (zerop (- end start))
      ""
      (flexi-streams:octets-to-string octets :external-format :utf-8 :start start :end end)))

(defun u16 (octets at)
  (+ (aref octets at) (ash (aref octets (1+ at)) 8)))

(defun u32 (octets at)
  (+ (aref octets at) (ash (aref octets (+ at 1)) 8)
     (ash (aref octets (+ at 2)) 16) (ash (aref octets (+ at 3)) 24)))

(defun append-octets (out bytes)
  (loop for byte across bytes do (vector-push-extend byte out))
  out)

(defun header-value (headers name)
  "NAME's value in HEADERS, a dexador response header table (both its
backends lowercase every name), or NIL."
  (and (hash-table-p headers) (gethash name headers)))

;;; --- one request ----------------------------------------------------------

(defun display-url (url)
  "URL without its query, for error text."
  (subseq url 0 (or (position #\? url) (length url))))

(defun request (method path &key query content content-type (timeout 120)
                                  (cap +max-response-bytes+))
  "One ElevenLabs request. => (values OCTETS STATUS HEADERS). A transport
failure is a refusal naming the condition's type only."
  (let ((url (concatenate 'string (api-base) path (and query (concatenate 'string "?" query))))
        (headers (append (list (cons "xi-api-key" (api-key)))
                         (and content-type (list (cons "content-type" content-type))))))
    (multiple-value-bind (body status response-headers truncated)
        (handler-case
            (nlk:http method url :headers headers :content content :binary t
                              :timeout timeout :max-bytes cap)
          (error (condition)
            (fail "could not reach ElevenLabs (~a): ~a. ~a"
                  (display-url url) (type-of condition) +no-retry+)))
      (when truncated
        (fail "ElevenLabs (~a) answered with more than ~d bytes. ~a"
              (display-url url) cap +no-retry+))
      (values body status response-headers))))

(defun ok (octets status headers)
  "OCTETS when STATUS is a 2xx with a body, else a refusal carrying
ElevenLabs' own message. => (values OCTETS HEADERS)."
  (unless (<= 200 status 299)
    (fail "~a" (api-error status octets)))
  (when (zerop (length octets))
    (fail "ElevenLabs returned an empty response"))
  (values octets headers))

(defun api-message (octets)
  "ElevenLabs' error body as one sentence: {detail: {status, message}},
FastAPI's {detail: [{loc, msg}]}, a bare string, else the raw text. The
vendor's own status word rides back as the second value."
  (let* ((payload (handler-case (nlk:decode-json (text-of octets)) (error () nil)))
         (detail (and (hash-table-p payload) (gethash "detail" payload))))
    (values
     (cond ((hash-table-p detail)
            (or (gethash "message" detail) (gethash "status" detail)))
           ((vectorp detail)
            (format nil "~{~a~^; ~}"
                    (loop for item across detail
                          when (hash-table-p item)
                            collect (format nil "~a: ~a"
                                            (or (gethash "msg" item) "request")
                                            (gethash "message" item)))))
           ((stringp detail) detail)
           (t (let ((text (string-trim '(#\Space #\Tab #\Return #\LineFeed)
                                       (text-of octets))))
                (and (plusp (length text)) (subseq text 0 (min 500 (length text)))))))
     (and (hash-table-p detail) (gethash "status" detail)))))

(defparameter +vendor-hints+
  '(("invalid_api_key" . " (the API key is not a valid ElevenLabs key)")
    ("missing_permissions" . " (enable Music access for this API key)")
    ("quota_exceeded" . " (the ElevenLabs account is out of credits)")))

(defparameter +status-hints+
  '((401 . " (check the API key and your plan's remaining credits)")
    (402 . " (the Music API needs a paid ElevenLabs plan)")
    (403 . " (the Music API needs a paid ElevenLabs plan, or this feature needs a higher tier)")
    (429 . " (rate limited or out of credits; try again later)")))

(defun api-error (status octets)
  (multiple-value-bind (message vendor) (api-message octets)
    (format nil "ElevenLabs returned HTTP ~d: ~a~a" status
            (or message "no message")
            (or (cdr (assoc vendor +vendor-hints+ :test #'string=))
                (cdr (assoc status +status-hints+))
                ""))))

;;; --- multipart, out and back ----------------------------------------------

(defun multipart-body (parts &aux (boundary (format nil "nc-music-~36r" (random (expt 36 9)))))
  "PARTS -- (:field NAME VALUE) or (:file NAME FILE-NAME MIME OCTETS) -- as a
multipart/form-data body. => (values OCTETS CONTENT-TYPE)."
  (let ((out (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (newline (format nil "~c~c" #\Return #\LineFeed)))
    (flet ((text (string) (append-octets out (octets-of string))))
      (dolist (part parts)
        (text (format nil "--~a~a" boundary newline))
        (ecase (first part)
          (:field
           (text (format nil "Content-Disposition: form-data; name=~s~a" (second part) newline))
           (text newline)
           (text (princ-to-string (third part)))
           (text newline))
          (:file
           (destructuring-bind (name file-name mime content) (rest part)
             (text (format nil "Content-Disposition: form-data; name=~s; filename=~s~a"
                           name file-name newline))
             (text (format nil "Content-Type: ~a~a" mime newline))
             (text newline)
             (append-octets out content)
             (text newline)))))
      (text (format nil "--~a--~a" boundary newline)))
    (values out (format nil "multipart/form-data; boundary=~a" boundary))))

(defun multipart-boundary (content-type)
  "The boundary CONTENT-TYPE names, or NIL."
  (let* ((down (string-downcase (or content-type "")))
         (at (search "boundary=" down)))
    (when at
      (let* ((rest (subseq (or content-type "") (+ at 9)))
             (stop (or (position #\; rest) (length rest)))
             (value (string-trim (list #\Space #\Tab (code-char 34)) (subseq rest 0 stop))))
        (and (plusp (length value)) value)))))

(defun multipart-parts (octets content-type)
  "OCTETS' parts as (HEADERS . OCTETS), HEADERS an alist of lowercased names."
  (let ((boundary (multipart-boundary content-type)))
    (unless boundary
      (fail "ElevenLabs answered with ~a, not the multipart reply this tool reads. ~a"
            (or content-type "no content type") +no-retry+))
    (let* ((delimiter (octets-of (format nil "--~a" boundary)))
           (width (length delimiter))
           (positions '())
           (at 0))
      (loop for found = (search delimiter octets :start2 at)
            while found do (push found positions) (setf at (+ found width)))
      (loop for (start . rest) on (nreverse positions)
            for after = (+ start width)
            unless (char= #\- (code-char (aref octets after)))
              collect (let* ((from (if (and (< (+ after 2) (length octets))
                                           (= 13 (aref octets after)) (= 10 (aref octets (1+ after))))
                                    (+ after 2) after))
                             (to (if rest (max from (- (first rest) 2)) (length octets)))
                             (chunk (subseq octets from (min to (length octets))))
                             (gap (search +blank-line+ chunk))
                             (head (and gap (text-of chunk :end gap)))
                             (body (subseq chunk (if gap (+ gap 4) 0))))
                        (cons (loop for line in (and head (uiop:split-string head :separator '(#\Return #\LineFeed)))
                                    for colon = (and (plusp (length line)) (position #\: line))
                                    when colon
                                      collect (cons (string-downcase (string-trim '(#\Space) (subseq line 0 colon)))
                                                    (string-trim '(#\Space #\Tab) (subseq line (1+ colon)))))
                              body))))))

(defun part-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defun split-multipart (octets headers)
  "/v1/music/detailed's multipart/mixed answer as (values METADATA AUDIO):
the JSON part and the audio part."
  (let ((metadata (make-hash-table :test #'equal))
        (audio nil)
        (seen nil))
    (dolist (part (multipart-parts octets (header-value headers "content-type")))
      (let* ((kind (string-downcase (or (part-header (car part) "content-type") "")))
             (body (cdr part)))
        (cond ((uiop:string-prefix-p "application/json" kind)
               (let ((decoded (handler-case (nlk:decode-json body) (error () nil))))
                 (when (hash-table-p decoded)
                   (setf metadata decoded seen t))))
              ((or (uiop:string-prefix-p "audio/" kind)
                   (search "attachment" (or (part-header (car part) "content-disposition") "")
                           :test #'char-equal))
               (setf audio body)))))
    (unless audio
      (fail "ElevenLabs' reply held no audio part. ~a" +no-retry+))
    (values (if seen metadata (make-hash-table :test #'equal)) audio)))

;;; --- a ZIP answer ---------------------------------------------------------

(defparameter +zip-signature+ '(80 75 5 6)
  "The bytes an end-of-central-directory record starts with.")

(defun zip-end (octets)
  "The offset of OCTETS' end-of-central-directory record, or NIL."
  (loop for at downfrom (- (length octets) 22) to 0
        when (and (= (aref octets at) (first +zip-signature+))
                  (= (aref octets (1+ at)) (second +zip-signature+))
                  (= (aref octets (+ at 2)) (third +zip-signature+))
                  (= (aref octets (+ at 3)) (fourth +zip-signature+)))
          return at))

(defun zip-member-bytes (octets data method packed size)
  "MEMBER's bytes: stored, deflated, or the raw span of anything else."
  (let ((span (subseq octets data (min (+ data packed) (length octets)))))
    (case method
      (0 span)
      (8 (chipz:decompress nil 'chipz:deflate span))
      (t (subseq octets data (min (+ data size) (length octets)))))))

(defun zip-members (octets)
  "OCTETS, a ZIP archive, as a list of (NAME . OCTETS), one per file."
  (let ((end (zip-end octets)))
    (unless end
      (fail "ElevenLabs returned something that is not a ZIP archive. ~a" +no-retry+))
    (let ((count (u16 octets (+ end 10)))
          (at (u32 octets (+ end 16)))
          (members '()))
      (dotimes (index count)
        (unless (and (< (+ at 46) (length octets)) (= #x02014b50 (u32 octets at)))
          (return))
        (let* ((method (u16 octets (+ at 10)))
               (packed (u32 octets (+ at 20)))
               (size (u32 octets (+ at 24)))
               (name-length (u16 octets (+ at 28)))
               (extra (u16 octets (+ at 30)))
               (comment (u16 octets (+ at 32)))
               (local (u32 octets (+ at 42)))
               (name (text-of octets :start (+ at 46) :end (+ at 46 name-length)))
               (data (+ local 30 (u16 octets (+ local 26)) (u16 octets (+ local 28)))))
          (unless (uiop:string-suffix-p name "/")
            (push (cons name (zip-member-bytes octets data method packed size)) members))
          (setf at (+ at 46 name-length extra comment))))
      (nreverse members))))
