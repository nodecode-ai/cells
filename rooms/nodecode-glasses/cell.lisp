;;;; cell.lisp --- smart glasses as a channel: a wearer's words in, short replies out, a photo on request.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of 100yenadmin/glasses-platform (MIT). What the original gives
;;;; a wearer is kept: a pair of glasses authenticates with its own token, says
;;;; something to the agent, gets a short answer back on the lens (a long one
;;;; split, never a table), asks for a picture by saying "what is this", shares
;;;; a picture it took, and receives what the agent pushes without being asked
;;;; -- a reminder, a scheduled job's words. The original's answer cache holds
;;;; the last frames for a device that was away; so does this one.
;;;;
;;;; What changed is the transport, and only that. The original opened a
;;;; WebSocket listener of its own on 0.0.0.0:8765 beside a gateway; here the
;;;; device talks to four small HTTP verbs under one gateway route, and the
;;;; turn is the kernel's own in-process ingress, NLE:SUBMIT. A glasses client
;;;; polls for what the agent said, and a capture request waits for the POST
;;;; that answers it. Nothing here dials out.

(defpackage #:nodecode-glasses
  (:use #:cl)
  (:export #:*submit* #:*answer* #:*active-p* #:*await-photo* #:*pairs* #:*registered*
           #:reset! #:route-path #:route-answer #:parse-devices #:session-of
           #:strip-markdown #:split-text #:visual-query-p #:deictic-query-p #:decode-base64
           #:queue-frame #:drain-frames #:queued-count #:say #:capture-photo #:request-photo
           #:run-command #:status-line #:deliver-pending #:deliver-step #:save-photo))

(in-package #:nodecode-glasses)


(nlk:define-peripheral glasses :not-running t)

;;; --- the words the cell answers itself -----------------------------------------

(defparameter *no-device* "No glasses are connected. Pair one with the token in the channels.glasses section."
  "What the camera tool answers when nothing has ever spoken to the cell.")
(defparameter *empty* "Say something for the agent."
  "An empty query is not a turn.")
(defparameter *not-taken* "The agent did not take that; say it again."
  "NLE:SUBMIT refused or dropped the query.")
(defparameter *photo-missing* "The glasses did not send a photo in time."
  "A capture request the device never answered.")
(defparameter *photo-answer* "Photo taken with the glasses."
  "The tool's line before the file it saved.")
(defparameter *stopped* "The glasses cell is not running; turn it on in /cells."
  "Every verb says this while the route is off the gateway.")

(defparameter *max-body* 16384
  "The original's ceiling on one query body, before a photo's own allowance.")

(defparameter *markdown-rules*
  '(("#{1,6}[ \\t]*" "")            ; a lens has no headings
    ("```[a-zA-Z0-9]*" "")          ; and no fences
    ("\\*\\*(.+?)\\*\\*" "\\1")     ; bold to its words
    ("__(.+?)__" "\\1")
    ("~~(.+?)~~" "\\1")
    ("`([^`]+)`" "\\1")
    ("\\[([^]]+)\\]\\([^)]*\\)" "\\1") ; a link to its text
    ("(?m)^[ \\t]*[-*+][ \\t]+" "") ; bullets flattened
    ("[ \\t]+" " "))
  "What comes off an answer before it reaches a lens.")

(defparameter *visual-keywords*
  '("look at" "looking at" "see this" "what am i seeing" "what is this" "what's this"
    "read this" "in front of me" "take a picture" "take a photo" "snap a photo"
    "use the camera" "through the camera")
  "The original's words that always mean \"take a picture now\".")

(defparameter *deictic-stop-nouns*
  '("morning" "afternoon" "evening" "night" "time" "day" "week" "month" "year"
    "moment" "question" "answer" "idea" "point" "case" "way" "reason" "sense"
    "stuff" "conversation" "chat" "session" "app" "voice" "sound" "response"
    "reply" "much" "long" "short" "many" "far" "fast" "slow" "bit" "lot" "kind"
    "sort" "type" "is" "was" "are" "were" "be" "being" "been" "will" "would"
    "can" "could" "should" "might" "may" "must" "does" "did" "done" "has" "had"
    "have" "gets" "got" "goes" "went" "seems" "means" "works" "sounds" "feels"
    "happened")
  "The original's nouns that read deictic and point at nothing visible.")

;;; --- what the cell runs on --------------------------------------------------

(defstruct (pair (:copier nil))
  (user "owner" :type string)
  (device "default" :type string)
  (token "" :type string))

(defvar *pairs* '()
  "The PAIRs of the section, parsed at start: one PAIR per device token.")

(defvar *registered* nil
  "The path the running cell put on the gateway, or NIL while stopped.")

(defvar *outboxes* (make-hash-table :test #'equal)
  "Wearer key -> frames not yet polled, newest first: what the agent said
while the device was away, and what it was asked to capture.")

(defvar *asking* '()
  "Wearers owed an answer: (SESSION USER DEVICE) entries, newest first.")

(defvar *photos* (make-hash-table :test #'equal)
  "Capture request id -> what the device answered, while a request is open.")

(defvar *last-pair* nil
  "The device that spoke most recently: who a tool call asks for a picture.")

(defvar *sequence* 0
  "The counter the cell mints command ids from, so two asks are two admissions.")

(defvar *delivery-worker* nil)

(defvar *lock* (bt2:make-lock :name "glasses")
  "One lock over the outboxes and the open capture requests: the gateway
answers requests on several threads.")

(defun reset! ()
  "Forget every queued frame, ask and open request; the route is the cell's
start and stop, not this."
  (bt2:with-lock-held (*lock*)
    (clrhash *outboxes*)
    (clrhash *photos*)
    (setf *asking* '()))
  (setf *last-pair* nil *sequence* (get-universal-time)))

(defun wearer-key (pair)
  (format nil "~a/~a" (pair-user pair) (pair-device pair)))

(defun next-id (kind)
  (format nil "~a-~a-~a" kind (get-universal-time) (incf *sequence*)))

;;; --- device tokens -------------------------------------------------------------

(defun valid-name-p (string)
  "A user or device name: what the original's section pattern accepts."
  (and (stringp string) (plusp (length string)) (<= (length string) 64)
       (every (lambda (character)
                (or (alphanumericp character) (find character "_.-" :test #'char=)))
              string)))

(defun parse-devices (devices single-token)
  "DEVICES ($GLASSES_DEVICES' shape: user:device=token, comma separated) and
SINGLE-TOKEN (one wearer, owner:default) => the PAIRs, token unique. Signals
NLK:CONFIG-REFUSAL naming the entry that is wrong, as the original did."
  (let ((entries (if (and devices (plusp (length (nlk:trimmed devices))))
                     (uiop:split-string devices :separator ",")
                     '()))
        (pairs '())
        (seen (make-hash-table :test #'equal)))
    (when (and single-token (plusp (length (nlk:trimmed single-token))))
      (setf entries (append entries (list (format nil "owner:default=~a" (nlk:trimmed single-token))))))
    (loop for entry in entries
          for index from 1
          do (let* ((entry (nlk:trimmed entry))
                    (equals (position #\= entry))
                    (label (and equals (subseq entry 0 equals)))
                    (token (and equals (nlk:trimmed (subseq entry (1+ equals)))))
                    (colon (and label (position #\: label))))
               (unless (and equals colon token (valid-name-p (subseq label 0 colon))
                            (valid-name-p (subseq label (1+ colon))))
                 (nlk:config-error
                  (format nil "glasses: device entry ~d is not user:device=token" index)))
               (when (gethash token seen)
                 (nlk:config-error (format nil "glasses: the same token is given twice (entry ~d)" index)))
               (setf (gethash token seen) t)
               (push (make-pair :user (subseq label 0 colon)
                                :device (subseq label (1+ colon))
                                :token token)
                     pairs)))
    (nreverse pairs)))

(defun authenticate (token pairs)
  "TOKEN's PAIR, or NIL."
  (find token pairs :key #'pair-token :test #'string=))

;;; --- the queue the device polls ---------------------------------------------------

(defun queue-frame (pair frame)
  "Hand FRAME to PAIR's glasses at their next poll. The oldest is dropped
past the section's queue_length, as the original's outbox dropped it."
  (let ((key (wearer-key pair)))
    (bt2:with-lock-held (*lock*)
      (let ((items (cons frame (gethash key *outboxes*))))
        (setf (gethash key *outboxes*) (subseq items 0 (min (length items) (setting :queue-length)))))))
  frame)

(defun drain-frames (pair)
  "Every frame queued for PAIR, oldest first, and the queue emptied."
  (bt2:with-lock-held (*lock*)
    (prog1 (reverse (gethash (wearer-key pair) *outboxes*))
      (remhash (wearer-key pair) *outboxes*))))

(defun queued-count (pair)
  (bt2:with-lock-held (*lock*) (length (gethash (wearer-key pair) *outboxes*))))

(defun push-to (user text)
  "TEXT to every device USER wears, framed as the answer to nothing: a
reminder, a job's words, a note the agent sent unasked."
  (let ((sent 0))
    (dolist (pair *pairs*)
      (when (string-equal (pair-user pair) user)
        (dolist (part (split-text (strip-markdown text)))
          (queue-frame pair (nlk:json-object "type" "push" "text" part))
          (incf sent))))
    sent))

;;; --- what a lens can show ---------------------------------------------------------

(defun strip-markdown (text)
  "TEXT as a lens takes it: no markdown, one line."
  (let ((out (or text "")))
    (loop for (pattern replacement) in *markdown-rules*
          do (setf out (ppcre:regex-replace-all pattern out replacement)))
    (nlk:trimmed (substitute #\Space #\Newline (substitute #\Space #\Tab out)))))

(defun split-text (text)
  "TEXT cut into lens lines of at most the section's reply_length, in order."
  (let* ((text (or text ""))
         (limit (max 80 (or (setting :reply-length) 480))))
    (cond ((zerop (length text)) '(""))
          ((<= (length text) limit) (list text))
          (t (loop for start from 0 below (length text) by limit
                   collect (nlk:trimmed (subseq text start (min (length text) (+ start limit)))))))))

;;; --- when a query should take a picture ---------------------------------------------

(defun deictic-query-p (text)
  "Whether TEXT points at something visible: \"this drink\", \"the one\"."
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings "(?i)\\b(?:this|that|these|those)\\s+([a-z]+)|\\bthe one\\b" text)
    (declare (ignore match))
    (when groups
      (let ((noun (string-downcase (or (and (plusp (length groups)) (aref groups 0)) ""))))
        (or (zerop (length noun))
            (not (member noun *deictic-stop-nouns* :test #'string=)))))))

(defun visual-query-p (text)
  "Whether TEXT is asking about something the wearer is looking at: the
original's explicit words, or a deictic reference to a visible thing."
  (let ((lowered (string-downcase (or text ""))))
    (or (some (lambda (keyword) (search keyword lowered :test #'char=)) *visual-keywords*)
        (deictic-query-p lowered))))

;;; --- base64, for the photo a device sends -------------------------------------------

(defun decode-base64 (text max-bytes)
  "TEXT decoded from base64 => octets of at most MAX-BYTES, or NIL."
  (when (and (stringp text) (plusp (length text))
             (<= (length text) (* 4 (ceiling max-bytes 3))))
    (handler-case
        (let ((out (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
              (acc 0) (bits 0))
          (loop for character across (string-right-trim "=" text)
                for value = (cond ((char<= #\A character #\Z) (- (char-code character) 65))
                                  ((char<= #\a character #\z) (- (char-code character) 71))
                                  ((char<= #\0 character #\9) (+ (char-code character) 4))
                                  ((char= character #\+) 62)
                                  ((char= character #\/) 63)
                                  ((char= character #\=) 0)
                                  (t (error "not base64")))
                do (setf acc (logior (ash acc 6) value) bits (+ bits 6))
                   (when (>= bits 8)
                     (setf bits (- bits 8))
                     (vector-push-extend (logand #xff (ash acc (- bits))) out)
                     (setf acc (logand acc (1- (ash 1 bits))))))
          (let ((octets (coerce out '(simple-array (unsigned-byte 8) (*)))))
            (when (and (plusp (length octets)) (<= (length octets) max-bytes))
              octets)))
      (error () nil))))

(defun max-photo-bytes ()
  (* 1024 (setting :max-photo-kb)))

(defun photo-path (request)
  "Where a photo is kept: the original's image cache, here the home's own
glasses folder, so a later turn can look at it."
  (namestring (ensure-directories-exist
               (merge-pathnames (format nil "glasses/~a.jpg" request) (nlk:home "")))))

(defun save-photo (request octets)
  (let ((path (photo-path request)))
    (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede :if-does-not-exist :create)
      (write-sequence octets out))
    path))

;;; --- asking the glasses for a photo ---------------------------------------------------

(defun await-photo (request timeout)
  "Wait up to TIMEOUT seconds for REQUEST's photo => its path, or NIL."
  (let ((deadline (+ (get-internal-real-time)
                      (* timeout internal-time-units-per-second))))
    (loop
      (let ((entry (bt2:with-lock-held (*lock*) (gethash request *photos*))))
        (when (and entry (eq :done (getf entry :state)))
          (return (getf entry :path)))
        (when (> (get-internal-real-time) deadline) (return nil))
        (sleep 0.05)))))

(defvar *await-photo* #'await-photo
  "How a capture request waits for the device: (REQUEST TIMEOUT) => path or
NIL. A test binds this; nothing else does.")

(defun request-photo (pair reason &optional (timeout (setting :photo-timeout-seconds)))
  "Ask PAIR's glasses for a photo and wait for it => the file, or NIL."
  (let ((request (next-id "photo")))
    (bt2:with-lock-held (*lock*)
      (setf (gethash request *photos*) (list :state :waiting :at (get-universal-time))))
    (queue-frame pair (nlk:json-object "type" "capture_photo" "request" request "reason" reason))
    (unwind-protect
         (funcall *await-photo* request timeout)
      (bt2:with-lock-held (*lock*) (remhash request *photos*)))))

(defun resolve-photo (request data)
  "The device's answer to REQUEST: DATA decoded, saved, and handed to
whoever waits => the path, or NIL when the bytes were not a photo."
  (let ((octets (decode-base64 data (max-photo-bytes))))
    (when octets
      (let ((path (save-photo request octets)))
        (bt2:with-lock-held (*lock*)
          (when (gethash request *photos*)
            (setf (gethash request *photos*) (list :state :done :path path))))
        path))))

;;; --- a query becomes a turn ------------------------------------------------------------

(defun session-of (user)
  "The session a wearer's asks run in: one per user, so a conversation on the
glasses is a conversation in the agent's history."
  (format nil "~a-~a" (setting :session)
          (string-downcase (ppcre:regex-replace-all "[^A-Za-z0-9_]+" (or user "owner") "-"))))

(defun ensure-session (session)
  (when (nlk:store-open-p)
    (unless (nlk:session-exists-p session)
      (nlk:standby-session :id session :cwd (namestring (ensure-directories-exist (nlk:home "glasses/")))))))

(defvar *submit*
  (lambda (session text command-id source-id)
    (nlk:active-input-admission-disposition
     (nle:submit session text :command-id command-id :source "glasses" :source-id source-id)))
  "How a query reaches its session: (SESSION TEXT COMMAND-ID SOURCE-ID) => the
admission. A test binds this; nothing else does.")

(defun taken-p (disposition)
  (member disposition '(:started :queued :steered :duplicate)))

(defun ask-agent (pair text &key photo)
  "TEXT from PAIR's wearer into their session, and a note to answer them.
=> the command id when the kernel took it, NIL when it did not."
  (let* ((session (session-of (pair-user pair)))
         (command-id (next-id "glasses"))
         (prompt (if photo
                     (format nil "A photo from my glasses is saved at ~a. ~a" photo text)
                     text)))
    (ensure-session session)
    (let ((disposition (handler-case (funcall *submit* session prompt command-id (pair-device pair))
                          (error () nil))))
      (when (taken-p disposition)
        (unless (member session *asking* :test #'string= :key #'first)
          (push (list session (pair-user pair) (pair-device pair)) *asking*))
        command-id))))

(defun say (pair text image)
  "One query from PAIR: TEXT, and IMAGE when the device sent what its camera
can see; a visual question takes a fresh photo first when the section says so.
=> (values STATUS FRAME)."
  (let ((text (nlk:trimmed (or text ""))))
    (if (zerop (length text))
        (values 400 (nlk:json-object "type" "error" "message" *empty*))
        (let* ((sent (and (stringp image) (plusp (length image)) (resolve-photo (next-id "sent") image)))
               (photo (or sent
                          (and (null sent) (string-equal "keywords" (setting :auto-capture))
                               (visual-query-p text)
                               (request-photo pair "keyword"))))
               (command-id (ask-agent pair text :photo photo)))
          (if command-id
              (values 200 (nlk:json-object "type" "ack" "id" command-id
                                            "photo" (and photo t)))
              (values 503 (nlk:json-object "type" "error" "message" *not-taken*)))))))

;;; --- what the agent answers ---------------------------------------------------------

(defvar *answer* #'nlk:turn-answer
  "The finished answer of a session: (SESSION) => text. A test binds this.")

(defvar *active-p* #'nlk:active-turn-p
  "Whether a session is mid-turn: (SESSION) => boolean. A test binds this.")

(defun deliver-pending ()
  "Hand each finished answer to the glasses that asked, split for the lens."
  (dolist (entry (copy-list *asking*))
    (destructuring-bind (session user device) entry
      (unless (funcall *active-p* session)
        (setf *asking* (remove entry *asking* :test #'equal))
        (let ((answer (funcall *answer* session)))
          (when (and answer (plusp (length (nlk:trimmed answer))))
            (let ((pair (or (find-if (lambda (p) (and (string-equal (pair-user p) user)
                                                      (string-equal (pair-device p) device)))
                                     *pairs*)
                            (find user *pairs* :key #'pair-user :test #'string-equal))))
              (when pair
                (dolist (part (split-text (strip-markdown answer)))
                  (queue-frame pair (nlk:json-object "type" "response" "text" part "final" t))))))))))
  nil)

(defun deliver-step ()
  (handler-case (deliver-pending) (error (condition) (warn "glasses: delivery: ~a" condition)))
  nil)

;;; --- the route ------------------------------------------------------------------

(defun route-path ()
  "The one path the glasses server answers under, a token in front of every
verb: <path_prefix><token>/say, /poll, /photo, /hello."
  (let ((prefix (or (setting :path-prefix) "/glasses/")))
    (cond ((zerop (length prefix)) "/glasses/")
          ((char= #\/ (char prefix (1- (length prefix)))) prefix)
          (t (concatenate 'string prefix "/")))))

(defun split-path (path)
  "PATH after the prefix => (values TOKEN VERB), or NIL when it is not the
shape of a device request."
  (let* ((prefix (route-path))
         (tail (and (uiop:string-prefix-p prefix path) (subseq path (length prefix)))))
    (when tail
      (let ((slash (position #\/ tail)))
        (when (and slash (plusp slash) (< (1+ slash) (length tail)))
          (values (subseq tail 0 slash) (subseq tail (1+ slash))))))))

(defun header (env name)
  (let ((headers (getf env :headers)))
    (and (hash-table-p headers) (gethash name headers))))

(defun json-answer (status value)
  "VALUE as the response the gateway writes."
  (let ((octets (nlk:encode-json-octets value)))
    (list status (list :content-type "application/json; charset=utf-8"
                       :content-length (length octets))
          octets)))

(defun body-stream (env)
  (getf env :raw-body))

(defun body-length (env)
  (let ((length (getf env :content-length)))
    (and (integerp length) (not (minusp length)) length)))

(defun read-body (env limit)
  "ENV's body read as text => (values TEXT NIL), or (values NIL WHY) where
WHY is a keyword naming what was wrong."
  (let ((stream (body-stream env)) (length (body-length env)))
    (cond ((null stream) (values nil :absent))
          ((null length) (values nil :bad-length))
          ((> length limit) (values nil :too-large))
          ((not (search "application/json" (or (header env "content-type") "") :test #'char-equal))
           (values nil :not-json))
          (t
           (let* ((text (if (eq 'character (stream-element-type stream))
                            (make-string length)
                            (make-array length :element-type '(unsigned-byte 8))))
                  (read (read-sequence text stream)))
             (if (= read length)
                 (values text nil)
                 (values nil :truncated)))))))

(defun body-object (env limit)
  "ENV's body as one decoded JSON object => (values OBJECT NIL) or (values
NIL WHY)."
  (multiple-value-bind (text why) (read-body env limit)
    (if why
        (values nil why)
        (handler-case
            (let ((value (nlk:decode-json text :whole t)))
              (if (hash-table-p value) (values value nil) (values nil :unreadable)))
          (error () (values nil :unreadable))))))

(defun json-member (object key)
  (and (hash-table-p object) (gethash key object)))

(defun dispatch (env method verb pair)
  "One device request, already authenticated as PAIR => the three values the
gateway writes."
  (cond
    ((and (string-equal verb "hello") (eq method :get))
     (json-answer 200 (nlk:json-object "type" "hello_ack" "protocol" 2
                                        "user" (pair-user pair)
                                        "device" (pair-device pair))))
    ((and (string-equal verb "poll") (eq method :get))
     (json-answer 200 (nlk:json-object "type" "frames" "frames" (coerce (drain-frames pair) 'vector))))
    ((and (string-equal verb "say") (eq method :post))
     (multiple-value-bind (body why) (body-object env (+ *max-body* (max-photo-bytes)))
       (if why
           (json-answer (case why (:too-large 413) (:not-json 415) (t 400))
                         (nlk:json-object "type" "error" "message" (string-downcase (symbol-name why))))
           (multiple-value-bind (status frame) (say pair (json-member body "text") (json-member body "image"))
             (json-answer status frame)))))
    ((and (string-equal verb "photo") (eq method :post))
     (multiple-value-bind (body why) (body-object env (+ *max-body* (max-photo-bytes)))
       (if why
           (json-answer 400 (nlk:json-object "type" "error" "message" (string-downcase (symbol-name why))))
           (let ((request (json-member body "request")))
             (if (and (stringp request)
                      (bt2:with-lock-held (*lock*) (gethash request *photos*))
                      (resolve-photo request (json-member body "data")))
                 (json-answer 200 (nlk:json-object "type" "taken" "request" request))
                 (json-answer 400 (nlk:json-object "type" "error" "message" *photo-missing*)))))))
    ((and (string-equal verb "photo") (eq method :get))
     ;; The device's own way of asking whether it is wanted: a capture request
     ;; it has not answered yet.
     (let ((pending (bt2:with-lock-held (*lock*)
                      (loop for request being the hash-keys of *photos*
                              using (hash-value entry)
                            when (eq :waiting (getf entry :state))
                              return request))))
       (if pending
           (json-answer 200 (nlk:json-object "type" "capture_photo" "request" pending "reason" "tool"))
           (json-answer 200 (nlk:json-object "type" "idle")))))
    (t (json-answer 404 (nlk:json-object "type" "error" "message" "unknown verb")))))

(defun route-answer (env)
  "The glasses endpoint on the gateway's own port: <prefix><token>/<verb>.
Nothing is answered while the cell is stopped, and a token the section does
not carry is refused before its body is read."
  (let* ((raw (or (getf env :request-uri) ""))
         (path (subseq raw 0 (or (position #\? raw) (length raw))))
         (method (getf env :request-method)))
    (multiple-value-bind (token verb) (split-path path)
      (let ((pair (and token *pairs* (authenticate token *pairs*))))
        (cond
          ((null *registered*) (json-answer 503 (nlk:json-object "type" "error" "message" *stopped*)))
          ((null pair) (json-answer 403 (nlk:json-object "type" "error" "message" "unknown device token")))
          (t (progn
               (setf *last-pair* pair)
               (dispatch env method verb pair))))))))

;;; --- the camera tool ------------------------------------------------------------------

(defparameter +capture-schema+
  (list :description
        (concatenate 'string
                     "Take a photo with the wearer's smart glasses camera right now and look at it. "
                     "Use when the wearer asks about something they are looking at, or when the answer "
                     "needs what is in front of them. The photo is saved as a file the answer can name.")
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                       "reason" (nlk:json-object "type" "string"
                                                   "description" "one short sentence: what the photo is of"))
         "required" #()))
  "The one tool the cell offers the model.")

(defun capture-photo (arguments)
  "The tool's body: ARGUMENTS is the decoded call, a hash table. => the file
the photo was saved at, or a line naming why there is none."
  (declare (ignore arguments))
  (let ((pair *last-pair*))
    (cond
      ((null *registered*) *stopped*)
      ((null pair) *no-device*)
      (t (let ((path (request-photo pair "tool")))
           (if path
               (format nil "~a It is saved at ~a." *photo-answer* path)
               *photo-missing*))))))

;;; --- the operator's /glasses ---------------------------------------------------------

(defun status-line ()
  (format nil "Smart glasses: ~d pair~:p paired, ~d ask~:p owed an answer, ~d capture request~:p open.~
~%The devices talk to ~s<token>/say, /poll, /photo and /hello on the gateway's port; ~
each wearer's asks run in session ~a-<user>. The channels.glasses section names the tokens."
          (length *pairs*)
          (length *asking*)
          (bt2:with-lock-held (*lock*) (hash-table-count *photos*))
          (route-path)
          (setting :session)))

(defun run-command (args session-id)
  "One /glasses invocation: no argument shows the endpoint; \"say TEXT\"
sends TEXT down the same path a wearer's words take; \"photo\" takes one
picture from the last pair that spoke."
  (declare (ignore session-id))
  (let* ((line (nlk:trimmed (or args "")))
         (space (position #\Space line))
         (verb (if space (subseq line 0 space) line))
         (rest (if space (nlk:trimmed (subseq line (1+ space))) "")))
    (cond
      ((zerop (length line)) (if *registered* (status-line) *stopped*))
      ((string-equal verb "say")
       (if (zerop (length rest))
           "glasses: /glasses say <what you would say to the agent>"
           (let ((pair (or *last-pair* (first *pairs*))))
             (if (null pair)
                 *no-device*
                 (multiple-value-bind (status frame) (say pair rest nil)
                   (format nil "~d ~a" status (or (json-member frame "id") "")))))))
      ((string-equal verb "photo") (capture-photo nil))
      (t (format nil "glasses: ~a is not a verb; say <text>, or photo" verb)))))

;;; --- the manual --------------------------------------------------------------------

(defparameter +primer+
  "Smart glasses are a channel: a pair carries a token, the wearer speaks, and the agent's
answer comes back as short lines on the lens -- split at reply_length and stripped of markdown,
because a lens has no room for a table, and a reminder or a scheduled job's words are pushed the
same way.
A device talks to four verbs on the gateway's own port, under <path_prefix><token>/: /hello pairs
and echoes who it is; /say posts {\"text\": \"...\"} (and {\"image\": \"<base64 jpeg>\"} when the
glasses already took a picture) and answers {\"id\": ...}; /poll answers the frames the agent has
said since the last poll -- {type: response} for an answer, {type: push} for something unasked;
and /photo GETs a capture request, or POSTs {\"request\": ..., \"data\": \"<base64>\"} to answer one.
A visual question (\"what is this\", \"read this\", \"the one on the left\") makes /say ask for a photo
first when auto_capture is keywords; set it to off to capture only through the glasses_capture_photo
tool.
Every token in the section grants the wearer a chat with your agent, including approvals, so treat a
token as a key. The gateway refuses any Host but loopback, so put an HTTPS reverse proxy in front of
it (or `nodecode link`) before a device off this machine can reach it. /glasses shows the endpoint and
the pairs, /glasses say TEXT proves the whole path from the shell."
  "What (help :glasses) answers while the cell runs.")

;;; --- the cell ----------------------------------------------------------------------

(nle:define-cell glasses
  (:section ("channels" "glasses")
    (:guide "give each pair a token (user:device=token, or one token for owner:default); the device talks to <path_prefix><token>/say and /poll on the gateway's port; /glasses shows it, /glasses say TEXT proves the path")
    ("token" :secret
     :doc "one device token, for the single wearer owner:default")
    ("devices" :string
     :doc "several pairs: user:device=token, comma separated (the original's GLASSES_DEVICES)")
    (:one-of "token" "devices")
    ("path_prefix" :string :default "/glasses/"
     :doc "the gateway path the devices talk under; a token goes after it")
    ("session" :string :default "glasses"
     :doc "the session each wearer's asks run in: <session>-<user>")
    ("auto_capture" :choice :options '("keywords" "off") :default "keywords"
     :doc "keywords: a visual question takes a photo first; off: only the glasses_capture_photo tool captures")
    ("photo_timeout_seconds" :integer :default 25 :min 1
     :doc "how long a capture request waits for the device")
    ("reply_length" :integer :default 480 :min 80
     :doc "the longest line a lens shows; a longer answer is split")
    ("queue_length" :integer :default 50 :min 1
     :doc "frames held per device until it polls")
    ("max_photo_kb" :integer :default 2048 :min 16
     :doc "the largest photo a device may send"))
  (:tool "glasses_capture_photo" +capture-schema+ 'capture-photo)
  (:command "glasses" 'run-command
            :description "Show the smart glasses endpoint, or say something down it"
            :argument-hint "[say TEXT | photo]")
  (:help :glasses
         "glasses: smart glasses as a channel - a wearer's words and camera in, short replies out"
         +primer+)
  (:start (lambda ()
            (let ((devices (parse-devices (setting :devices) (setting :token))))
              (unless devices
                (nlk:config-error "glasses: no device token: set \"token\" for one pair, or \"devices\" for several"))
              (reset!)
              (setf *pairs* devices)
              (setf *registered* (nle:route (route-path) #'route-answer :public t))
              (nle:on-stop (lambda ()
                             (when *registered*
                               (nle:route *registered* nil)
                               (setf *registered* nil))
                             (setf *pairs* '())))
              (setf *delivery-worker* (nlk:worker-start "glasses" #'deliver-step :wake 1))
              (nle:on-stop (lambda ()
                             (when *delivery-worker*
                               (nlk:worker-stop *delivery-worker*)
                               (setf *delivery-worker* nil))))
              (nle:notice (format nil "glasses: ~d pair~:p paired; devices talk to ~s<token>/say"
                                  (length devices) (route-path))
                          :key "channels.glasses")
              (nle:on-stop (lambda () (handler-case (nle:notice nil :key "channels.glasses")
                                        (error () nil))))))))
