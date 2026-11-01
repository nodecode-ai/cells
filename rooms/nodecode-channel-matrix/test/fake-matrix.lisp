;;;; fake-matrix.lisp --- a Matrix homeserver that answers on 127.0.0.1, for tests.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The adapter's live executor and its /sync lap run unchanged against this:
;;;; the Client-Server API under /_matrix/ -- whoami, /sync (the first answer
;;;; every room's shape and its BACKLOG, later ones what a test pushes and the
;;;; echo of every event the lane sends, the way a homeserver echoes a
;;;; client's own events with their transaction id), sends, redactions,
;;;; typing, joins, the room reads the probe makes, a profile and an upload.
;;;; Every call is recorded. Shapes follow spec.matrix.org.

(in-package #:nodecode.test)

(define-test-slice "matrix" "CHANNEL-MATRIX-" :recipe "matrix")

(defparameter +fake-bot+ "@bot:fake")

(defstruct (fake-matrix (:copier nil))
  (lock (bt2:make-lock :name "fake-matrix"))
  (handler nil)
  (port 0 :type integer)
  ;; (METHOD SEGMENTS BODY) per call, newest first; SEGMENTS the path's
  ;; decoded segments after /_matrix/.
  (calls '())
  ;; room id -> (:members (ID ...) :encrypted BOOL :name TEXT)
  (rooms (make-hash-table :test #'equal))
  ;; (ROOM . EVENT) the first /sync's timeline carries.
  (backlog '())
  ;; (ROOM . EVENT) the next /sync delivers, and invites as (:invite ROOM SENDER).
  (queue '())
  ;; room -> ("<type>|<state key>" -> the state event the room holds there):
  ;; what /rooms/{room}/state answers and a state PUT sets.
  (room-state (make-hash-table :test #'equal))
  ;; How many /sync calls named a since token.
  (polls 0 :type integer)
  (counter 0 :type integer))

(nlk:access (fake fake-matrix))

(defun fake-matrix-room (fake room &key members encrypted name)
  (setf (gethash room fake.rooms) (list :members (cons +fake-bot+ members)
                                        :encrypted encrypted :name name)))

(defun fake-matrix-next (fake prefix)
  (format nil "~a~d" prefix (bt2:with-lock-held (fake.lock) (incf fake.counter))))

(defun matrix-event (sender type content &key id)
  (nlk:json-object "type" type "sender" sender "content" content
                   "event_id" (or id (format nil "$ev~d" (random 1000000000)))
                   "origin_server_ts" 1700000000000))

(defun matrix-text (sender text &key id html mentions thread reply-to)
  "An m.room.message as a client sends it."
  (let ((content (nlk:json-object "msgtype" "m.text" "body" text
                                  :when html "format" "org.matrix.custom.html"
                                  :opt "formatted_body" html
                                  :when mentions "m.mentions" (nlk:json-object "user_ids" (coerce mentions 'vector)))))
    (cond (thread (setf (gethash "m.relates_to" content)
                        (nlk:json-object "rel_type" "m.thread" "event_id" thread "is_falling_back" t
                                         "m.in_reply_to" (nlk:json-object "event_id" thread))))
          (reply-to (setf (gethash "m.relates_to" content)
                          (nlk:json-object "m.in_reply_to" (nlk:json-object "event_id" reply-to)))))
    (matrix-event sender "m.room.message" content :id id)))

(defun fake-matrix-state (fake room type state-key content &key (sender "@kim:fake"))
  "Set ROOM's state at TYPE and STATE-KEY to CONTENT as SENDER sent it.
=> the event."
  (let ((event (nlk:json-object "type" type "state_key" state-key "sender" sender "content" content
                                "event_id" (fake-matrix-next fake "$state"))))
    (bt2:with-lock-held (fake.lock)
      (setf (gethash (format nil "~a|~a" type state-key)
                     (or (gethash room fake.room-state)
                         (setf (gethash room fake.room-state) (make-hash-table :test #'equal))))
            event))
    event))

(defun fake-matrix-push (fake room event)
  (bt2:with-lock-held (fake.lock) (setf fake.queue (append fake.queue (list (cons room event))))))

(defun fake-matrix-invite (fake room sender &key direct)
  (bt2:with-lock-held (fake.lock)
    (setf fake.queue (append fake.queue (list (list :invite room sender direct))))))

(defun fake-matrix-body (env &aux (length (getf env :content-length)))
  (when (and length (plusp length))
    (let ((octets (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence octets (getf env :raw-body))
      (or (ignore-errors (nlk:decode-json (sb-ext:octets-to-string octets :external-format :utf-8)))
          octets))))

(defun room-section (fake room &key first-p)
  (let* ((spec (gethash room fake.rooms))
         (state (and first-p (getf spec :encrypted)
                     (list (nlk:json-object "type" "m.room.encryption" "state_key" "" "sender" "@kim:fake"
                                            "event_id" "$crypt" "content" (nlk:json-object "algorithm" "m.megolm.v1.aes-sha2"))))))
    (nlk:json-object "summary" (nlk:json-object "m.joined_member_count" (length (getf spec :members)))
                     "state" (nlk:json-object "events" (coerce state 'vector))
                     "timeline" (nlk:json-object "events" (vector)))))

(defun fake-matrix-sync (fake since)
  "The answer to one /sync: the first every room and the backlog, a later
one -- after waiting a little for something to say -- the queue."
  (let ((join (make-hash-table :test #'equal))
        (invite (make-hash-table :test #'equal))
        (items '()))
    (if (null since)
        (progn
          (maphash (lambda (room spec) (declare (ignore spec))
                     (setf (gethash room join) (room-section fake room :first-p t)))
                   fake.rooms)
          (setf items (bt2:with-lock-held (fake.lock) fake.backlog)))
        (progn
          (bt2:with-lock-held (fake.lock) (incf fake.polls))
          (loop repeat 20
                until (bt2:with-lock-held (fake.lock) fake.queue)
                do (sleep 0.05))
          (setf items (bt2:with-lock-held (fake.lock) (shiftf fake.queue '())))))
    (dolist (item items)
      (if (eq :invite (first item))
          (destructuring-bind (room sender direct) (rest item)
            (setf (gethash room invite)
                  (nlk:json-object "invite_state"
                                   (nlk:json-object "events"
                                                    (vector (nlk:json-object "type" "m.room.member" "state_key" +fake-bot+
                                                                             "sender" sender
                                                                             "content" (nlk:json-object "membership" "invite"
                                                                                                        :when direct "is_direct" t)))))))
          (let* ((room (car item))
                 (section (or (gethash room join)
                              (setf (gethash room join)
                                    (nlk:json-object "timeline" (nlk:json-object "events" (vector)))))))
            (setf (gethash "events" (gethash "timeline" section))
                  (concatenate 'vector (gethash "events" (gethash "timeline" section)) (vector (cdr item)))))))
    (nlk:json-object "next_batch" (fake-matrix-next fake "b")
                     "rooms" (nlk:json-object "join" join "invite" invite))))

(defun fake-matrix-answer (fake method segments body query)
  "(values STATUS OBJECT) for one Client-Server API call."
  (flet ((at (n) (nth n segments)))
    (cond
      ((equal (subseq segments 0 (min 3 (length segments))) '("client" "v3" "account"))
       (values 200 (nlk:json-object "user_id" +fake-bot+ "device_id" "DEV1")))
      ((equal (at 2) "sync")
       (values 200 (fake-matrix-sync fake (ppcre:register-groups-bind (since) ("since=([^&]+)" (or query ""))
                                            (quri:url-decode since)))))
      ((equal (at 2) "joined_rooms")
       (values 200 (nlk:json-object "joined_rooms" (coerce (sort (alexandria:hash-table-keys fake.rooms) #'string<)
                                                           'vector))))
      ((equal (at 2) "profile")
       (values 200 (nlk:json-object "displayname" (if (equal (at 3) "@kim:fake") "kim" ""))))
      ((equal (at 2) "join")
       (fake-matrix-room fake (at 3) :members (list "@kim:fake"))
       (values 200 (nlk:json-object "room_id" (at 3))))
      ((equal (at 0) "media")
       (values 200 (nlk:json-object "content_uri" (fake-matrix-next fake "mxc://fake/m"))))
      ((equal (at 2) "rooms")
       (let ((room (at 3)) (verb (at 4)))
         (cond
           ((equal verb "send")
            (let ((id (fake-matrix-next fake "$out")))
              ;; The homeserver echoes the client's own event, its txn id unsigned.
              (fake-matrix-push fake room (nlk:json-object "type" (at 5) "sender" +fake-bot+ "event_id" id
                                                           "content" body
                                                           "unsigned" (nlk:json-object "transaction_id" (at 6))))
              (values 200 (nlk:json-object "event_id" id))))
           ((equal verb "redact") (values 200 (nlk:json-object "event_id" (fake-matrix-next fake "$red"))))
           ((and (equal verb "state") (equal method "PUT"))
            ;; Set, and echoed in the next /sync's timeline as the homeserver does.
            (let ((event (fake-matrix-state fake room (at 5) (fake-matrix-state-key segments)
                                            (if (hash-table-p body) body (nlk:json-object))
                                            :sender +fake-bot+)))
              (fake-matrix-push fake room event)
              (values 200 (nlk:json-object "event_id" (gethash "event_id" event)))))
           ((and (equal verb "state") (null (at 5)))
            (values 200 (coerce (bt2:with-lock-held (fake.lock)
                                  (let ((held (gethash room fake.room-state)))
                                    (and held (alexandria:hash-table-values held))))
                                'vector)))
           ((equal verb "typing") (values 200 (nlk:json-object)))
           ((equal verb "joined_members")
            (let ((joined (make-hash-table :test #'equal)))
              (dolist (id (getf (gethash room fake.rooms) :members))
                (setf (gethash id joined) (nlk:json-object "display_name" (subseq id 1 (position #\: id)))))
              (values 200 (nlk:json-object "joined" joined))))
           ((and (equal verb "state") (equal (at 5) "m.room.name"))
            (let ((name (getf (gethash room fake.rooms) :name)))
              (if name
                  (values 200 (nlk:json-object "name" name))
                  (values 404 (nlk:json-object "errcode" "M_NOT_FOUND" "error" "no name")))))
           ((and (equal verb "state") (equal (at 5) "m.room.encryption"))
            (if (getf (gethash room fake.rooms) :encrypted)
                (values 200 (nlk:json-object "algorithm" "m.megolm.v1.aes-sha2"))
                (values 404 (nlk:json-object "errcode" "M_NOT_FOUND" "error" "not encrypted"))))
           (t (values 404 (nlk:json-object "errcode" "M_UNRECOGNIZED"))))))
      (t (values 404 (nlk:json-object "errcode" "M_UNRECOGNIZED"))))))

(defun fake-matrix-app (fake)
  (lambda (env &aux (path (getf env :path-info)))
    (if (uiop:string-prefix-p "/_matrix/" path)
        ;; Split the path as it came over the wire and decode each segment once,
        ;; as a homeserver does: the decoded PATH-INFO has lost which slash
        ;; and plus were a segment's own (a state key's base64 has both).
        (let* ((raw (let ((uri (or (getf env :request-uri) path)))
                      (subseq uri 0 (position #\? uri))))
               (segments (mapcar #'quri:url-decode
                                 (rest (rest (uiop:split-string raw :separator "/")))))
               (method (string-upcase (string (getf env :request-method))))
               (query (getf env :query-string))
               (body (fake-matrix-body env)))
          (bt2:with-lock-held (fake.lock) (push (list method segments body) fake.calls))
          (multiple-value-bind (status object) (fake-matrix-answer fake method segments body query)
            (list status '(:content-type "application/json") (list (nlk:encode-json-object object)))))
        (list 404 '(:content-type "text/plain") (list "fake matrix")))))

(defmacro with-fake-matrix ((var) &body body)
  "Run BODY with VAR a fake homeserver serving on 127.0.0.1, stopped on unwind."
  `(let ((,var (make-fake-matrix)))
     (multiple-value-bind (handler port) (nle::serve-local (fake-matrix-app ,var))
       (setf (fake-matrix-handler ,var) handler
             (fake-matrix-port ,var) port)
       (nlk:with-cleanup ((nle::stop-clack-handler handler))
         ,@body))))

(defmacro with-matrix-state-dir ((var) &body body)
  "Run BODY with VAR a fresh scratch folder, removed on unwind."
  `(let ((,var (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "matrix-state-~36r/" (random (expt 36 8)))
                                 (uiop:temporary-directory)))))
     (ensure-directories-exist ,var)
     (nlk:with-cleanup ((uiop:delete-directory-tree ,var :validate t :if-does-not-exist :ignore))
       ,@body)))

(defun fake-matrix-url (fake)
  (format nil "http://127.0.0.1:~d" fake.port))

(defun fake-matrix-state-key (segments)
  "The state key a state path's SEGMENTS end in, empty when it names none."
  (or (nth 6 segments) ""))

(defun matrix-state-puts (fake &key room type)
  "(ROOM TYPE STATE-KEY BODY) of every state PUT so far, oldest first."
  (bt2:with-lock-held (fake.lock)
    (reverse (loop for (method segments body) in fake.calls
                   when (and (equal method "PUT") (equal (nth 2 segments) "rooms")
                             (equal (nth 4 segments) "state")
                             (or (null room) (equal (nth 3 segments) room))
                             (or (null type) (equal (nth 5 segments) type)))
                     collect (list (nth 3 segments) (nth 5 segments) (fake-matrix-state-key segments) body)))))

(defun matrix-calls (fake verb &key type room)
  "The bodies of every call whose path's room VERB (send, redact, typing) or
last verb is VERB, oldest first; TYPE narrows a send to one event type."
  (bt2:with-lock-held (fake.lock)
    (reverse (loop for (nil segments body) in fake.calls
                   when (and (or (and (equal (nth 2 segments) "rooms") (equal (nth 4 segments) verb)
                                      (or (null type) (equal (nth 5 segments) type))
                                      (or (null room) (equal (nth 3 segments) room)))
                                 (and (not (equal (nth 2 segments) "rooms")) (equal (nth 2 segments) verb)
                                      (or (null room) (equal (nth 3 segments) room)))))
                     collect body))))
