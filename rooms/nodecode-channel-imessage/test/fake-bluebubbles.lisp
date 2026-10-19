;;;; fake-bluebubbles.lisp --- a BlueBubbles server that answers on 127.0.0.1, for tests.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The adapter's live executor and its poll lap run unchanged against this:
;;;; the REST API under /api/v1 behind its password -- the server's account of
;;;; itself, the message query by row (the Messages database a test writes
;;;; into, the bot's own sends landing in it the way Messages writes them),
;;;; sends of words and files, tapbacks, the typing bubble, contacts, chats and
;;;; an attachment's download. Every call is recorded. Shapes follow the
;;;; server's source (packages/server/src/server/api/http/api/v1).

(in-package #:nodecode.test)

(define-test-slice "imessage" "CHANNEL-IMESSAGE-" :recipe "imessage")

(defparameter +fake-bb-password+ "bb-secret-pass")

(defstruct (fake-bb (:copier nil))
  (lock (bt2:make-lock :name "fake-bluebubbles"))
  (handler nil)
  (port 0 :type integer)
  (private-api t)
  ;; (METHOD SEGMENTS BODY) per call, newest first; SEGMENTS the path's
  ;; decoded segments after /api/v1/.
  (calls '())
  ;; The Messages database: message objects, oldest first, each its row.
  (messages '())
  (row 1000 :type integer)
  ;; chat guid -> (:name TEXT :people (HANDLE ...))
  (chats (make-hash-table :test #'equal))
  ;; (HANDLE . NAME) the Mac's contacts know.
  (contacts '())
  ;; How many reads named a row to go on from.
  (polls 0 :type integer)
  (counter 0 :type integer))

(nlk:access (fake fake-bb))

(defun fake-bb-chat (fake guid &key people name)
  (setf (gethash guid fake.chats) (list :name name :people people)))

(defun fake-bb-next (fake prefix)
  (format nil "~a~d" prefix (bt2:with-lock-held (fake.lock) (incf fake.counter))))

(defun bb-chat-object (fake guid)
  (let ((spec (gethash guid fake.chats)))
    (nlk:json-object "guid" guid "displayName" (or (getf spec :name) "")
                     "style" (if (search ";-;" guid) 45 43)
                     "participants" (coerce (mapcar (lambda (handle) (nlk:json-object "address" handle))
                                                    (getf spec :people))
                                            'vector))))

(defun fake-bb-write (fake chat &rest members &key guid from-me sender text root reply-to tapback target
                                                attachments &allow-other-keys)
  "Write one message into the fake's Messages database, at the next row."
  (declare (ignore members))
  (bt2:with-lock-held (fake.lock)
    (let ((message (nlk:json-object
                    "guid" (or guid (format nil "G~d" (incf fake.counter)))
                    "originalROWID" (incf fake.row)
                    "text" text
                    "handle" (if from-me :null (nlk:json-object "address" sender "service" "iMessage"))
                    "isFromMe" (and from-me t)
                    "itemType" 0
                    "isSystemMessage" nil
                    "dateCreated" 1700000000000
                    "dateRetracted" :null
                    "associatedMessageGuid" (if target (format nil "p:0/~a" target) :null)
                    "associatedMessageType" (or tapback :null)
                    "threadOriginatorGuid" (or root :null)
                    "replyToGuid" (or reply-to :null)
                    "attachments" (coerce attachments 'vector)
                    "chats" (vector (bb-chat-object fake chat)))))
      (setf fake.messages (append fake.messages (list message)))
      message)))

(defun fake-bb-say (fake chat sender text &rest keys)
  "SENDER says TEXT in CHAT."
  (apply #'fake-bb-write fake chat :sender sender :text text keys))

(defun fake-bb-body (env &aux (length (getf env :content-length)))
  (when (and length (plusp length))
    (let ((octets (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence octets (getf env :raw-body))
      (or (ignore-errors (nlk:decode-json (sb-ext:octets-to-string octets :external-format :utf-8)))
          (sb-ext:octets-to-string octets :external-format :latin-1)))))

(defun form-field (body name)
  "The value of the form-data field NAME in the multipart BODY text."
  (ppcre:register-groups-bind (value)
      ((format nil "name=\"~a\"\\r?\\n(?:[^\\r\\n]+\\r?\\n)*\\r?\\n([^\\r\\n]*)" name) body)
    value))

(defun envelope (data &key (status 200) (message "Success"))
  (nlk:json-object "status" status "message" message "data" data))

(defun fake-bb-root (fake guid)
  (let ((message (find guid fake.messages :key (lambda (m) (gethash "guid" m)) :test #'equal)))
    (or (and message (stringp (gethash "threadOriginatorGuid" message)) (gethash "threadOriginatorGuid" message))
        guid)))

(defun fake-bb-query (fake body)
  (let ((where (nlk:json-array body "where")))
    (if (zerop (length where))
        (envelope (let ((last (car (last fake.messages)))) (if last (vector last) (vector))))
        (let ((after (nlk:json-value (aref where 0) :integer "args" "rowid"))
              (limit (or (nlk:json-value body :integer "limit") 100)))
          (bt2:with-lock-held (fake.lock) (incf fake.polls))
          (envelope (coerce (let ((rows (remove-if-not (lambda (m) (> (gethash "originalROWID" m) after))
                                                       (bt2:with-lock-held (fake.lock) fake.messages))))
                              (subseq rows 0 (min limit (length rows))))
                            'vector))))))

(defun fake-bb-answer (fake method segments body)
  "(values STATUS OBJECT-OR-OCTETS) for one REST call."
  (declare (ignore method))
  (flet ((at (n) (nth n segments)))
    (cond
      ((equal segments '("server" "info"))
       (values 200 (envelope (nlk:json-object "server_version" "1.9.9" "os_version" "14.5"
                                              "private_api" (and fake.private-api t)
                                              "helper_connected" (and fake.private-api t)
                                              "detected_imessage" "bot@icloud.com"))))
      ((equal segments '("message" "query")) (values 200 (fake-bb-query fake body)))
      ((equal segments '("message" "text"))
       (let ((selected (gethash "selectedMessageGuid" body)))
         (values 200 (envelope (fake-bb-write fake (gethash "chatGuid" body) :from-me t
                                                   :guid (fake-bb-next fake "OUT")
                                                   :text (gethash "message" body)
                                                   :root (and selected (fake-bb-root fake selected)))))))
      ((equal segments '("message" "attachment"))
       (values 200 (envelope (fake-bb-write fake (form-field body "chatGuid") :from-me t
                                                 :guid (fake-bb-next fake "FILE") :text ""))))
      ((equal segments '("message" "react"))
       (values 200 (envelope (fake-bb-write fake (gethash "chatGuid" body) :from-me t
                                                 :guid (fake-bb-next fake "TAP")
                                                 :tapback (gethash "reaction" body)
                                                 :target (gethash "selectedMessageGuid" body)))))
      ((and (equal (at 0) "chat") (equal (at 2) "typing")) (values 200 (envelope :null)))
      ((equal segments '("chat" "query"))
       (values 200 (envelope (coerce (mapcar (lambda (guid) (bb-chat-object fake guid))
                                             (sort (alexandria:hash-table-keys fake.chats) #'string<))
                                     'vector))))
      ((equal segments '("contact" "query"))
       (values 200 (envelope (coerce (loop for (handle . name) in fake.contacts
                                           when (find handle (nlk:json-array body "addresses") :test #'equal)
                                             collect (nlk:json-object "displayName" name
                                                                      "phoneNumbers" (vector (nlk:json-object "address" handle))
                                                                      "emails" (vector)))
                                     'vector))))
      ((and (equal (at 0) "attachment") (equal (at 2) "download"))
       (values 200 (sb-ext:string-to-octets (format nil "bytes of ~a" (at 1)) :external-format :utf-8)))
      (t (values 404 (nlk:json-object "status" 404 "message" "Not Found"
                                      "error" (nlk:json-object "type" "Not Found" "message" "no such route")))))))

(defun fake-bb-app (fake)
  (lambda (env &aux (path (getf env :path-info)) (query (or (getf env :query-string) "")))
    (cond
      ((not (uiop:string-prefix-p "/api/v1/" path))
       (list 404 '(:content-type "text/plain") (list "fake bluebubbles")))
      ((not (search (format nil "password=~a" (quri:url-encode +fake-bb-password+)) query))
       (list 401 '(:content-type "application/json")
             (list (nlk:encode-json-object
                    (nlk:json-object "status" 401 "message" "Unauthorized"
                                     "error" (nlk:json-object "type" "Unauthorized"
                                                              "message" "You are not authorized to access this resource"))))))
      (t
       (let* ((segments (mapcar (lambda (segment) (quri:url-decode (ppcre:regex-replace-all "\\+" segment "%2B")))
                               (rest (rest (rest (uiop:split-string path :separator "/"))))))
              (method (string-upcase (string (getf env :request-method))))
              (body (fake-bb-body env)))
         (bt2:with-lock-held (fake.lock) (push (list method segments body) fake.calls))
         (multiple-value-bind (status object) (fake-bb-answer fake method segments body)
           (if (typep object '(vector (unsigned-byte 8)))
               (list status '(:content-type "image/jpeg") object)
               (list status '(:content-type "application/json") (list (nlk:encode-json-object object))))))))))

(defmacro with-fake-bb ((var &key (private-api t)) &body body)
  "Run BODY with VAR a fake BlueBubbles server serving on 127.0.0.1, stopped on unwind."
  `(let ((,var (make-fake-bb :private-api ,private-api)))
     (multiple-value-bind (handler port) (nle::serve-local (fake-bb-app ,var))
       (setf (fake-bb-handler ,var) handler
             (fake-bb-port ,var) port)
       (nlk:with-cleanup ((nle::stop-clack-handler handler))
         ,@body))))

(defmacro with-imessage-state-dir ((var) &body body)
  "Run BODY with VAR a fresh scratch folder, removed on unwind."
  `(let ((,var (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "imessage-state-~36r/" (random (expt 36 8)))
                                 (uiop:temporary-directory)))))
     (ensure-directories-exist ,var)
     (nlk:with-cleanup ((uiop:delete-directory-tree ,var :validate t :if-does-not-exist :ignore))
       ,@body)))

(defun fake-bb-url (fake)
  (format nil "http://127.0.0.1:~d" fake.port))

(defun bb-calls (fake &rest segments)
  "The bodies of every call whose path begins with SEGMENTS, oldest first."
  (bt2:with-lock-held (fake.lock)
    (reverse (loop for (nil path body) in fake.calls
                   when (equal segments (subseq path 0 (min (length segments) (length path))))
                     collect body))))

(defun bb-call-methods (fake &rest segments)
  "The methods of every call whose path begins with SEGMENTS, oldest first."
  (bt2:with-lock-held (fake.lock)
    (reverse (loop for (method path) in fake.calls
                   when (equal segments (subseq path 0 (min (length segments) (length path))))
                     collect method))))

(defun bb-sends (fake needle)
  "Every message/text body so far whose words say NEEDLE, oldest first."
  (remove-if-not (lambda (body) (search needle (or (ignore-errors (gethash "message" body)) "")))
                 (bb-calls fake "message" "text")))
