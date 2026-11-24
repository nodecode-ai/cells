;;;; client.lisp --- the requests the original made, the answers as lines, the verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's provider.py, in Lisp: the same endpoints, the same query
;;;; and body members, the bearer token in the same header, and the user id
;;;; derived the same way -- uuid5(NAMESPACE_DNS, user_name + "memobase_client"),
;;;; so this cell reads and writes the SAME store the original did. Two things
;;;; differ, both deliberate. An answer is rendered as lines a model reads
;;;; rather than a JSON envelope, and a failure SIGNALS MEMOBASE-ERROR
;;;; carrying Memobase's own sentence, so a model gets the reason rather than
;;;; a member it has to parse.
;;;;
;;;; Nothing here touches a file or the environment: the only way out is
;;;; HTTP, the one seam a test stubs.

(in-package #:nodecode-memobase)

;;; --- what a request carries -------------------------------------------------------

(defparameter +context-path+ "/users/context/~a"
  "The original's recall endpoint, as it made it: the assembled profile and
event window under one token budget.")

(defparameter +insert-path+ "/blobs/insert/~a?wait_process=false"
  "The original's write endpoint: one chat blob into the buffer its server
extracts a profile from, asynchronously.")

(defparameter +profile-path+ "/users/profile/~a"
  "The original's profile endpoint.")

(defparameter +uuid-namespace-dns+ "6ba7b8109dad11d180b400c04fd430c8"
  "NAMESPACE_DNS, the namespace the original derived a user id in.")

(defparameter +uuid-name-suffix+ "memobase_client"
  "What the original appended to the user name before hashing it.")

(defparameter +query-chars+ 1500
  "The original's guard against a prompt too long for a query parameter.")

(defparameter +blob-chars+ 3000
  "How much of either side of a turn a write keeps, as the original clipped it.")

(defparameter +read-chars+ 4000
  "How much of a recalled context the verb answers when a call names no length.")

(defparameter +detail-chars+ 600
  "How much of a failed answer's body the refusal quotes.")

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun clipped (text limit)
  "TEXT at most LIMIT characters, with a note when it was cut."
  (if (<= (length text) limit)
      text
      (format nil "~a [~d more characters]" (subseq text 0 limit) (- (length text) limit))))

;;; --- the user id, derived as the original derived it ---------------------------------
;;; uuid5 is SHA-1 of the namespace's sixteen octets and the name, with the
;;; version and variant bits set. The image's digest library carries SHA-256
;;; only, so the forty lines of SHA-1 are here, and a known vector proves
;;; them.

(defun rol32 (value count)
  "VALUE, 32 bits, rotated left by COUNT."
  (ldb (byte 32 0) (logior (ash value count) (ash value (- count 32)))))

(defun sha1-octets (octets)
  "The SHA-1 digest of OCTETS, a (VECTOR (UNSIGNED-BYTE 8)), as 20 octets."
  (let* ((length (length octets))
         (padded (* 64 (ceiling (+ length 9) 64)))
         (message (make-array padded :element-type '(unsigned-byte 8) :initial-element 0))
         (words (make-array 80 :element-type '(unsigned-byte 32))))
    (replace message octets)
    (setf (aref message length) #x80)
    (let ((bits (* 8 length)))
      (dotimes (index 8)
        (setf (aref message (- padded 1 index)) (ldb (byte 8 (* 8 index)) bits))))
    (let ((h0 #x67452301) (h1 #xEFCDAB89) (h2 #x98BADCFE) (h3 #x10325476) (h4 #xC3D2E1F0))
      (loop for offset from 0 below padded by 64
            do (dotimes (index 16)
                 (setf (aref words index)
                       (logior (ash (aref message (+ offset (* 4 index))) 24)
                               (ash (aref message (+ offset (* 4 index) 1)) 16)
                               (ash (aref message (+ offset (* 4 index) 2)) 8)
                               (aref message (+ offset (* 4 index) 3)))))
               (loop for index from 16 below 80
                     do (setf (aref words index)
                              (rol32 (logxor (aref words (- index 3)) (aref words (- index 8))
                                             (aref words (- index 14)) (aref words (- index 16)))
                                     1)))
               (let ((a h0) (b h1) (c h2) (d h3) (e h4))
                 (dotimes (index 80)
                   (multiple-value-bind (f k)
                       (cond ((< index 20)
                              (values (logior (logand b c) (logand (lognot b) d)) #x5A827999))
                             ((< index 40) (values (logxor b c d) #x6ED9EBA1))
                             ((< index 60)
                              (values (logior (logand b c) (logand b d) (logand c d)) #x8F1BBCDC))
                             (t (values (logxor b c d) #xCA62C1D6)))
                     (let ((temp (ldb (byte 32 0) (+ (rol32 a 5) f e k (aref words index)))))
                       (setf e d
                             d c
                             c (rol32 b 30)
                             b a
                             a temp))))
                 (setf h0 (ldb (byte 32 0) (+ h0 a))
                       h1 (ldb (byte 32 0) (+ h1 b))
                       h2 (ldb (byte 32 0) (+ h2 c))
                       h3 (ldb (byte 32 0) (+ h3 d))
                       h4 (ldb (byte 32 0) (+ h4 e)))))
      (let ((digest (make-array 20 :element-type '(unsigned-byte 8))))
        (loop for value in (list h0 h1 h2 h3 h4)
              for base from 0 by 4
              do (loop for index from 0 below 4
                       do (setf (aref digest (+ base index))
                                (ldb (byte 8 (* 8 (- 3 index))) value))))
        digest))))

(defun hex-octets (text)
  "TEXT, an even run of hex digits, as octets."
  (let ((octets (make-array (floor (length text) 2) :element-type '(unsigned-byte 8))))
    (dotimes (index (length octets) octets)
      (setf (aref octets index)
            (parse-integer text :start (* 2 index) :end (+ 2 (* 2 index)) :radix 16)))))

(defun hex-text (octets)
  "OCTETS as lowercase hex."
  (format nil "~(~{~2,'0x~}~)" (coerce octets 'list)))

(defun uuid5 (namespace name)
  "The uuid5 of NAME under NAMESPACE, a hex uuid: what the original's
string_to_uuid computed, so both read one store."
  (let* ((octets (sb-ext:string-to-octets name :external-format :utf-8))
         (digest (sha1-octets (concatenate '(vector (unsigned-byte 8))
                                            (hex-octets namespace) octets)))
         (bytes (subseq digest 0 16))
         (hex nil))
    (setf (aref bytes 6) (logior (logand (aref bytes 6) #x0f) #x50)
          (aref bytes 8) (logior (logand (aref bytes 8) #x3f) #x80))
    (setf hex (hex-text bytes))
    (format nil "~a-~a-~a-~a-~a"
            (subseq hex 0 8) (subseq hex 8 12) (subseq hex 12 16)
            (subseq hex 16 20) (subseq hex 20 32))))

(defun user-id (&optional (name (setting :user-name)))
  "The user id the original used for NAME, defaulting to the section's
user_name: uuid5(NAMESPACE_DNS, NAME + \"memobase_client\")."
  (uuid5 +uuid-namespace-dns+ (concatenate 'string (or (nonblank name) "user")
                                            +uuid-name-suffix+)))

;;; --- the one seam ------------------------------------------------------------------

(defun http (method url &key headers content timeout)
  "The cell's one network call; a test stubs this name. => (values BODY
STATUS), every status a value."
  (nlk:http method url :headers headers :content content :timeout (or timeout 8)))

(defun base ()
  "The Memobase server's base url, without its trailing slash."
  (string-right-trim "/" (or (setting :base-url) "")))

(defun json-string (text)
  "TEXT as a JSON string literal, quotes and all."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for character across text
          do (case character
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (if (< (char-code character) 32)
                      (format out "\\u~4,'0x" (char-code character))
                      (write-char character out)))))
    (write-char #\" out)))

(defun request-headers ()
  "What every request sends: JSON, and the bearer token when the section
carries one -- Memobase's own, from the token or the variable."
  (let ((key (nonblank (setting :api-key))))
    (if key
        (list (cons "accept" "application/json")
              (cons "content-type" "application/json")
              (cons "authorization" (format nil "Bearer ~a" key)))
        (list (cons "accept" "application/json")
              (cons "content-type" "application/json")))))

;;; --- what Memobase answers ---------------------------------------------------------

(defun payload-of (body status what)
  "BODY, Memobase's answer to WHAT, decoded, or a refusal naming the
status and Memobase's own sentence. => the decoded object."
  (let ((text (let ((text (princ-to-string (or body ""))))
                (and (plusp (length text)) (clipped text +detail-chars+)))))
    (when (and (integerp status) (>= status 400))
      (fail "Memobase answered HTTP ~a from ~a~@[ - ~a~]" status what text)))
  (let ((payload (ignore-errors (nlk:decode-json (princ-to-string (or body ""))))))
    (unless (hash-table-p payload)
      (fail "Memobase answered no JSON from ~a (HTTP ~a)~@[ - ~a~]" what status
            (let ((text (princ-to-string (or body ""))))
              (and (plusp (length text)) (clipped text +detail-chars+)))))
    (let ((errno (nlk:json-value payload :integer "errno")))
      (when (and errno (plusp errno))
        (fail "Memobase refused ~a: ~a" what
              (or (nlk:json-value payload :string "errmsg") (format nil "errno ~a" errno)))))
    payload))

(defun call-json (method url what &key content)
  "One exchange with Memobase => the decoded answer, or a refusal."
  (multiple-value-bind (body status)
      (http method url :headers (request-headers) :content content
                            :timeout (setting :timeout-seconds))
    (payload-of body status what)))

;;; --- the addresses -------------------------------------------------------------------

(defun context-url (query)
  "The recall url for QUERY, carrying the original's own parameters: the
one message as chats_str, the token budget, the event window's depth, and
whether the window rides along."
  (format nil "~a~a?chats_str=~a&max_token_size=~d&time_range_in_days=~d~@[&fill_window_with_events=true~]"
          (base) (format nil +context-path+ (user-id))
          (quri:url-encode (format nil "[{\"role\":\"user\",\"content\":~a}]" (json-string query))
                           :encoding :utf-8)
          (setting :max-tokens)
          (setting :time-range-days)
          (setting :fill-window-events)))

(defun insert-url ()
  "The write url: one chat blob into the server's buffer."
  (format nil "~a~a" (base) (format nil +insert-path+ (user-id))))

(defun profile-url ()
  "The profile url."
  (format nil "~a~a" (base) (format nil +profile-path+ (user-id))))

;;; --- the answer as lines ----------------------------------------------------------------

(defun context-text (query context max-length)
  "What a recall answers: the profile and event window Memobase assembled,
under MAX-LENGTH characters (the cell's own ceiling when a call names none)."
  (let ((context (and context (nlk:trimmed context))))
    (if (or (null context) (zerop (length context)))
        (format nil "memobase: nothing is remembered for ~s yet" query)
        (format nil "memobase: what is remembered for ~s~2%~a"
                query (clipped context (or max-length +read-chars+))))))

(defun profile-text (payload)
  "What a profile call answers: the profile Memobase holds, as the JSON it
sent, or a line saying it holds none."
  (let ((data (or (nlk:json-value payload :object "data")
                  (nlk:json-value payload :string "data"))))
    (cond ((null data) (format nil "memobase: no profile for user ~a yet" (user-id)))
          ((stringp data) (format nil "memobase: the profile of user ~a~2%~a" (user-id) data))
          (t (format nil "memobase: the profile of user ~a~2%~a" (user-id)
                      (clipped (nlk:encode-json-object data) +read-chars+))))))

;;; --- the verbs -----------------------------------------------------------------------

(define-verb search (query &key max-length)
  "Recall what Memobase remembers about QUERY for this user: the assembled
profile and event timeline, as text. QUERY is the question or the turn;
MAX-LENGTH caps the characters answered (the cell's own ceiling when
absent). => the context as lines, or a line saying nothing is remembered."
  (let ((query (nonblank query)))
    (unless query
      (fail "a search needs a query: give one, e.g. (memobase:search \"what did we decide about the build\")"))
    (let* ((query (subseq query 0 (min (length query) +query-chars+)))
           (payload (call-json :get (context-url query) "the context endpoint")))
      (context-text query (nlk:json-value payload :string "data" "context") max-length))))

(define-verb profile ()
  "The whole Memobase user profile this cell's user_name resolves to: the
topics and sub-topics Memobase extracted, as the JSON it holds."
  (let ((payload (call-json :get (profile-url) "the profile endpoint")))
    (profile-text payload)))

(define-verb remember (user-text &key assistant)
  "Write one exchange to Memobase so its server extracts a profile from it
in the background. USER-TEXT is what the user said; ASSISTANT the reply,
when there is one. => a line saying what was written."
  (let ((user-text (nonblank user-text)))
    (unless user-text
      (fail "nothing to remember: give the text, e.g. (memobase:remember \"the build is green on main\")"))
    (let* ((assistant (nonblank assistant))
           (messages (with-output-to-string (out)
                       (format out "[{\"role\":\"user\",\"content\":~a}"
                               (json-string (clipped user-text +blob-chars+)))
                       (when assistant
                         (format out ",{\"role\":\"assistant\",\"content\":~a}"
                                 (json-string (clipped assistant +blob-chars+))))
                       (write-char #\] out)))
           (body (format nil "{\"blob_type\":\"chat\",\"blob_data\":{\"messages\":~a}}" messages)))
      (call-json :post (insert-url) "the insert endpoint" :content body)
      (format nil "memobase: wrote ~d message~:p for user ~a; its server extracts the profile in the background~@[~%  (a user message, ~d characters)~]"
              (if assistant 2 1) (user-id) t (length user-text)))))

;;; --- the operator's own door -----------------------------------------------------------

(defun status-line ()
  "What /memobase alone answers: where this cell points and what it gives."
  (format nil "memobase: ~a as user ~a~%~{  ~a~^~%~}"
          (base) (user-id)
          (list "the model has memobase_search and memobase_profile"
                (format nil "max_tokens ~d, time_range_days ~d, event window ~:[off~;on~]"
                        (setting :max-tokens) (setting :time-range-days)
                        (setting :fill-window-events))
                "give a query to search, profile for the whole profile, or remember TEXT to write one")))

(defun run-slash (args session-id)
  "/memobase [status|profile|remember TEXT|QUERY]: the operator's own door on
the same store the tools read."
  (declare (ignore session-id))
  (let* ((raw (nlk:trimmed (or args "")))
         (space (position #\Space raw))
         (head (string-downcase (if space (subseq raw 0 space) raw)))
         (value (if space (nlk:trimmed (subseq raw (1+ space))) "")))
    (cond ((member head '("" "status") :test #'string=) (status-line))
          ((string= head "profile") (profile))
          ((string= head "remember") (remember value))
          (t (search raw)))))
