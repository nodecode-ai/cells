;;;; fake-driver.lisp --- a WhatsApp driver that answers in-process, for tests.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The adapter's lap, its executor and the wire run unchanged against this:
;;;; every request is a JSON line the fake decodes, records and answers with a
;;;; JSON line, through the same DRIVER-ACCEPT-LINE the live reader thread
;;;; feeds; every event a test pushes is a JSON line too. What it answers
;;;; follows driver/main.go's DISPATCH: ids for sends, the account for status,
;;;; a group's info, a file for a download, a code for pair_phone.

(in-package #:nodecode.test)

(define-test-slice "whatsapp" "CHANNEL-WHATSAPP-" :recipe "whatsapp")

(defparameter +wa-bot+ "15550000000")
(defparameter +wa-kim+ "15551110000")
(defparameter +wa-lee+ "15552220000")
(defparameter +wa-group+ "120363000000000001@g.us")

(defun wa-jid (number) (format nil "~a@s.whatsapp.net" number))

(defstruct (fake-wa (:copier nil) (:constructor %make-fake-wa))
  (lock (bt2:make-lock :name "fake-wa"))
  (driver nil)
  ;; (OP . ARGS) per request, newest first
  (requests '())
  (counter 0 :type integer)
  (linked t)
  ;; op -> (STATUS . ERROR) the fake refuses it with
  (refusals '())
  ;; group jid -> its info, as group_info answers it
  (groups (make-hash-table :test #'equal))
  (opened 0 :type integer))

(nlk:access (fake fake-wa))

(defun fake-wa-identity (fake)
  (nlk:json-object "connected" t "logged_in" fake.linked "linked" fake.linked
                   "jid" (if fake.linked (wa-jid +wa-bot+) "")
                   "phone" (if fake.linked +wa-bot+ "")
                   "lid" (if fake.linked "99990000@lid" "")
                   "push_name" (if fake.linked "Nodecode" "")))

(defun fake-wa-group (fake chat name &rest members)
  "Put a group CHAT named NAME in FAKE, MEMBERS (NUMBER &key admin) lists."
  (setf (gethash chat fake.groups)
        (nlk:json-object "chat" chat "name" name
                         "participants"
                         (coerce (loop for (number . flags) in members
                                       collect (nlk:json-object "id" number "phone" number
                                                                "jid" (wa-jid number)
                                                                "name" (format nil "person ~a" (subseq number 7))
                                                                "admin" (and (getf flags :admin) t)
                                                                "superadmin" (and (getf flags :superadmin) t)))
                                 'vector))))

(defun fake-wa-answer (fake op args)
  "What FAKE answers OP with ARGS: (values OK STATUS RESULT ERROR)."
  (nlk:when-let (refusal (assoc op fake.refusals :test #'equal))
    (return-from fake-wa-answer (values nil (cadr refusal) nil (cddr refusal))))
  (flet ((id () (format nil "BOT~d" (bt2:with-lock-held (fake.lock) (incf fake.counter)))))
    (cond
      ((equal op "status") (values t 200 (fake-wa-identity fake)))
      ((member op '("send" "send_file" "poll") :test #'equal)
       (values t 200 (nlk:json-object "id" (id) "chat" (gethash "chat" args)
                                      "timestamp_ms" (ncw::unix-ms))))
      ((equal op "group_info")
       (let ((info (gethash (gethash "chat" args) fake.groups)))
         (if info (values t 200 info) (values nil 404 nil "no such group"))))
      ((equal op "groups")
       (values t 200 (coerce (loop for info being the hash-values of fake.groups
                                   collect (nlk:json-object "chat" (gethash "chat" info)
                                                            "name" (gethash "name" info)
                                                            "participants" (length (gethash "participants" info))))
                             'vector)))
      ((equal op "download")
       (let ((path (uiop:with-temporary-file (:pathname path :keep t :prefix "fake-wa-")
                     path)))
         (with-open-file (out path :direction :output :if-exists :supersede)
           (format out "bytes of ~a" (nlk:json-value args :string "media" "type")))
         (values t 200 (nlk:json-object "path" (namestring path) "size" 14))))
      ((equal op "pair_phone") (values t 200 (nlk:json-object "code" "ABCD-EFGH")))
      ((equal op "logout") (setf fake.linked nil) (values t 200 (nlk:json-object "linked" nil)))
      (t (values t 200 (nlk:json-object))))))

(defun make-fake-wa (&key (linked t))
  (let ((fake (%make-fake-wa :linked linked)))
    (setf fake.driver
          (ncw:make-driver
           :send (lambda (line)
                   (let* ((request (nlk:decode-json line))
                          (op (gethash "op" request))
                          (args (gethash "args" request)))
                     (bt2:with-lock-held (fake.lock) (push (cons op args) fake.requests))
                     (multiple-value-bind (ok status result error) (fake-wa-answer fake op args)
                       ;; What a send created is kept beside what it said.
                       (when (and ok (hash-table-p args) (hash-table-p result) (gethash "id" result))
                         (setf (gethash "__id" args) (gethash "id" result)))
                       (ncw:driver-accept-line
                        fake.driver
                        (nlk:encode-json-object
                         (if ok
                             (nlk:json-object "id" (gethash "id" request) "ok" t "status" status
                                              "result" result)
                             (nlk:json-object "id" (gethash "id" request) "ok" nil "status" status
                                              "error" error)))))))))
    fake))

(defun fake-wa-push (fake event)
  "EVENT, a JSON object, written by FAKE's driver as a line."
  (ncw:driver-accept-line fake.driver (nlk:encode-json-object event)))

(defun fake-wa-opener (fake)
  "An opener for the adapter's lap: FAKE's driver, announcing the account
as the live driver does when it starts and, linked, when it connects."
  (lambda (adapter)
    (declare (ignore adapter))
    (incf fake.opened)
    (let ((identity (fake-wa-identity fake)))
      (setf (gethash "event" identity) "started")
      (fake-wa-push fake identity)
      (when fake.linked
        (let ((ready (fake-wa-identity fake)))
          (setf (gethash "event" ready) "ready")
          (fake-wa-push fake ready))))
    fake.driver))

(defun fake-wa-calls (fake op)
  "The args of every OP request FAKE took, oldest first."
  (bt2:with-lock-held (fake.lock)
    (reverse (loop for (name . args) in fake.requests when (equal name op) collect args))))

(defun fake-wa-ops (fake)
  (bt2:with-lock-held (fake.lock) (reverse (mapcar #'car fake.requests))))

(defun wa-person (number)
  (nlk:json-object "id" number "jid" (wa-jid number) "phone" number "lid" ""))

(defun wa-message (from text &key (id (format nil "ASK~a" (random 1000000))) chat group quote mentions-me
                                  media (timestamp (ncw::unix-ms)) (push-name "kim") from-me
                                  (kind "text"))
  "A message event as the driver writes one: FROM a number saying TEXT in CHAT
(their direct chat unless GROUP names one), QUOTE a (ID FROM-ME TEXT) list."
  (nlk:json-object "event" "message" "id" id
                   "chat" (or group chat (wa-jid from))
                   "group" (and group t)
                   "from" (wa-person from)
                   "from_me" (and from-me t)
                   "push_name" push-name
                   "timestamp_ms" timestamp
                   "kind" kind "text" text
                   "mentions" #() "mentions_me" (and mentions-me t)
                   :when media "media" media
                   :when quote "quote" (destructuring-bind (qid qfrom-me qtext &optional qmedia) quote
                                         (nlk:json-object "id" qid "from_me" qfrom-me "text" qtext
                                                          "from" (wa-person (if qfrom-me +wa-bot+ from))
                                                          :when qmedia "media" qmedia))))

(defun wa-media (type &key (mime "image/jpeg") (filename "photo.jpg") (size 2048) seconds ptt)
  (nlk:json-object "type" type "direct_path" "/v/t62/fake" "media_key" "a2V5" "file_enc_sha256" "ZW5j"
                   "file_sha256" "c2hh" "file_length" size "mime" mime "filename" filename
                   :when seconds "seconds" seconds :when ptt "ptt" t))

(defun whatsapp-section (soul-path &rest members)
  "channels.whatsapp with a scratch soul, MEMBERS over it."
  (let ((section (nlk:json-object "request_timeout_seconds" 5 "soul_file" soul-path
                                  "status_update_ms" 0)))
    (loop for (key value) on members by #'cddr
          do (setf (gethash key section) value))
    section))

(defmacro with-whatsapp-lane ((fake &rest members) &body body)
  "BODY against a started WhatsApp lane over FAKE -- MEMBERS the section's
own -- once the lap has opened its driver; the provider answers `whatsapp
reply'."
  `(let ((ncw::*driver-opener* (fake-wa-opener ,fake)))
     (with-adapter-lane (:reply "whatsapp reply" :token "unused" :start ncw:start-channel
                         :bindings ((executor nil))
                         :section (whatsapp-section soul-path ,@members)
                         :plans nil
                         :after ((is (adapter-threads-stopped-p "channel-whatsapp"))))
       (is (await (:timeout 10) (plusp (fake-wa-opened ,fake))) "the lap opened the driver")
       (when (fake-wa-linked ,fake)
         (is (await (:timeout 10) (equal +wa-bot+ (ncw::own ncw::*whatsapp-adapter* "phone")))))
       ,@body)))

(defun wa-answers (fake needle)
  "Every send so far whose words say NEEDLE, oldest first."
  (remove-if-not (lambda (args) (search needle (or (gethash "text" args) "")))
                 (fake-wa-calls fake "send")))
