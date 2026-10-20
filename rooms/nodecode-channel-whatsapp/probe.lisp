;;;; probe.lisp --- what the linked device can see, and how it is linked.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The read-only half of onboarding: PROBE-CHANNEL (START-CHANNEL's twin;
;;;; NCK:PROBE is the door) asks the running driver who the account is and
;;;; which groups it is in -- their names, who is in them -- and answers a
;;;; text, so chats and people are picked by name. A device that is not linked
;;;; yet says how to link it (ADAPTER.LISP, LINK). The probe starts nothing:
;;;; one process per linked device, and the lane's is the one.
;;;;
;;;; QR-PNG draws a linking QR code as a PNG a chat can show, the code's
;;;; modules from the link cell's encoder when it is loaded. The section
;;;; declaration at the end is the adapter's one statement of what
;;;; channels.whatsapp is made of (NLK:DEFINE-SECTION): the kit's refusal, the
;;;; setup panel, the model's setup primer and the addons report all read it.

(in-package #:nodecode-channel-whatsapp)

(defparameter +probe-shown+ 30
  "How many chats or people one probe line names before it counts the rest.")

(defun live-driver ()
  (let ((adapter *whatsapp-adapter*))
    (and adapter (driver-alive-p (whatsapp-adapter-driver adapter))
         (whatsapp-adapter-driver adapter))))

(defun probe-groups (driver)
  "The groups the account is in, as (JID NAME COUNT), or NIL."
  (multiple-value-bind (status groups) (driver-call driver "groups" nil :timeout 20)
    (when (and (= status 200) (vectorp groups))
      (loop for group across groups
            collect (list (nlk:json-value group :string "chat")
                          (or (let ((name (nlk:json-value group :text "name")))
                                (and (plusp (length name)) name))
                              "unnamed group")
                          (or (nlk:json-value group :integer "participants") 0))))))

(defun probe-people (driver groups)
  "The people in the first of GROUPS, as (ID . NAME), the account itself
left out."
  (let ((people '())
        (me (nlk:json-value (nth-value 1 (driver-call driver "status" nil :timeout 5)) :string "phone")))
    (loop for (chat) in groups
          for index from 0
          while (< index 10)
          do (multiple-value-bind (status info) (driver-call driver "group_info" (nlk:json-object "chat" chat)
                                                             :timeout 15)
               (when (= status 200)
                 (loop for person across (nlk:json-array info "participants")
                       for id = (nlk:json-value person :string "id")
                       unless (or (null id) (equal id me))
                         do (pushnew (cons id (or (let ((name (nlk:json-value person :text "name")))
                                                    (and (plusp (length name)) name))
                                                  id))
                                     people :key #'car :test #'equal)))))
    (sort people #'string< :key #'car)))

(defparameter +not-running+
  "device: the channel is not running -- set channels.whatsapp (allowed_users with your own number, owner the same), restart the channels, then link: (ncw:link :phone \"+<number>\") answers a code to type in WhatsApp > Linked devices > Link a device > Link with phone number instead")

(defun probe-channel (section &key executor (timeout 15) &aux (lines '()))
  "What the linked device can see, as lines: the account, the groups it is
in, and the people in them -- or how to link it."
  ;; Read only, and only through the running lane's driver.
  (declare (ignore section executor timeout))
  (flet ((say (control &rest args) (push (apply #'format nil control args) lines)))
    (let ((driver (live-driver)))
      (if (null driver)
          (say "~a" +not-running+)
          (multiple-value-bind (status me error) (driver-call driver "status" nil :timeout 5)
            (cond ((/= status 200) (say "device: the driver did not answer (~a)" error))
                  ((not (gethash "linked" me))
                   (say "device: not linked yet -- ~a" +link-hint+))
                  (t
                   (say "device: linked as +~a~@[ (~a)~]~:[, reconnecting~;, connected~]"
                        (nlk:json-value me :string "phone") (nlk:json-value me :text "push_name")
                        (gethash "connected" me))
                   (let ((groups (probe-groups driver)))
                     (say "groups: ~:[none -- add the account to a group, or message it directly from an allowed number~;~:*~{~a~^, ~}~]"
                          (loop for (chat name count) in groups
                                for index from 0
                                while (< index +probe-shown+)
                                collect (format nil "~a ~a (~d)" name chat count)))
                     (nlk:when-let (people (probe-people driver groups))
                       (say "people: ~{~a~^, ~}"
                            (mapcar (lambda (pair) (format nil "~a ~a" (cdr pair) (car pair)))
                                    (subseq people 0 (min (length people) +probe-shown+)))))))))))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defun probe-chat-choices (section &key executor (timeout 15))
  "The groups the account is in as ((jid . name) ...) -- what a setup panel
offers for allowed_chats."
  (declare (ignore section executor timeout))
  (let ((driver (or (live-driver) (config-error "~a" +not-running+))))
    (loop for (chat name) in (probe-groups driver) collect (cons chat name))))

(defun probe-user-choices (section &key executor (timeout 15))
  "The people in those groups as ((number . name) ...) -- what a setup panel
offers for allowed_users and owner."
  (declare (ignore section executor timeout))
  (let ((driver (or (live-driver) (config-error "~a" +not-running+))))
    (probe-people driver (probe-groups driver))))

;;; --- the QR code as a picture ----------------------------------------------------
;;; A PNG with nothing in it a chat cannot show: 8-bit grey rows, each led by
;;; filter 0, in deflate's stored blocks (no compression is still a valid zlib
;;; stream), each chunk under its CRC.

(defparameter +crc-table+
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8)
          (setf c (if (logbitp 0 c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
        (setf (aref table n) c)))))

(defun crc32 (octets &optional (crc #xFFFFFFFF))
  (loop for octet across octets
        do (setf crc (logxor (aref +crc-table+ (logand (logxor crc octet) #xFF)) (ash crc -8))))
  (logxor crc #xFFFFFFFF))

(defun adler32 (octets &aux (a 1) (b 0))
  (loop for octet across octets
        do (setf a (mod (+ a octet) 65521) b (mod (+ b a) 65521)))
  (logior (ash b 16) a))

(defun be32 (n)
  (list (ldb (byte 8 24) n) (ldb (byte 8 16) n) (ldb (byte 8 8) n) (ldb (byte 8 0) n)))

(defun png-chunk (type data)
  (let ((body (concatenate '(vector (unsigned-byte 8)) (map 'vector #'char-code type) data)))
    (concatenate '(vector (unsigned-byte 8)) (be32 (length data)) body (be32 (crc32 body)))))

(defun png-octets (rows &key (scale 8))
  "ROWS -- strings of #\\1 dark and #\\0 light -- as a PNG's octets, each
module SCALE pixels square."
  (let* ((height (* scale (length rows)))
         (width (* scale (length (first rows))))
         (raw (make-array (* height (1+ width)) :element-type '(unsigned-byte 8)))
         (at 0))
    (dolist (row rows)
      (dotimes (repeat scale)
        (setf (aref raw at) 0) (incf at)
        (loop for module across row
              for grey = (if (char= module #\1) 0 255)
              do (dotimes (i scale) (setf (aref raw at) grey) (incf at)))))
    (let ((zlib (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
      (flet ((put (&rest octets) (dolist (octet octets) (vector-push-extend octet zlib))))
        (put #x78 #x01)
        (loop for start from 0 by 65535
              for end = (min (length raw) (+ start 65535))
              for final = (= end (length raw))
              for len = (- end start)
              do (put (if final 1 0) (ldb (byte 8 0) len) (ldb (byte 8 8) len)
                      (ldb (byte 8 0) (lognot len)) (ldb (byte 8 8) (lognot len)))
                 (loop for i from start below end do (vector-push-extend (aref raw i) zlib))
              until final)
        (apply #'put (be32 (adler32 raw))))
      (concatenate '(vector (unsigned-byte 8))
                   #(137 80 78 71 13 10 26 10)
                   (png-chunk "IHDR" (concatenate '(vector (unsigned-byte 8))
                                                  (be32 width) (be32 height) #(8 0 0 0 0)))
                   (png-chunk "IDAT" (coerce zlib '(vector (unsigned-byte 8))))
                   (png-chunk "IEND" #())))))

(defun qr-png (text path)
  "TEXT as a QR code in a PNG at PATH, the link cell's encoder drawing its
modules => PATH's name, or NIL when that encoder is not loaded."
  (nlk:when-let (rows-fn (uiop:find-symbol* "QR-ROWS" "NODECODE-LINK" nil))
    (when (fboundp rows-fn)
      (let ((octets (png-octets (funcall rows-fn text :margin 4))))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
          (write-sequence octets out))
        (uiop:native-namestring path)))))

;;; The declaration. Types and constraints are what START-CHANNEL reads and
;;; REQUIRE-NON-EMPTY-ALLOWLIST enforces; the kit refuses on SECTION-PROBLEMS
;;; before either runs, so the refusal, the panel and the primer say the same
;;; thing in the same words.
(nlk:define-section ("channels" "whatsapp")
  (:guide "WhatsApp is reached as a linked device: the account's phone stays its primary, and this machine becomes one of its Linked devices, as WhatsApp Web is. Give the bot a number of its own (a second SIM, an eSIM, WhatsApp Business on the same phone) -- linked to your own number, it speaks as you, and with self_chat on it answers you in your chat with yourself. Set allowed_users to the numbers that may talk to it (yours first) and owner to yours, restart the channels, then link it: (ncw:link :phone \"+<the bot's number>\") answers a code to type in WhatsApp > Settings > Linked devices > Link a device > Link with phone number instead; (ncw:link) answers a QR code to scan there. The first start builds the driver from this cell's driver/ folder, which needs Go 1.24 or newer. WhatsApp does not allow automated accounts; a number that misbehaves can be banned, so keep the bot to people who expect it.")
  (:check #'probe-channel)
  (:any-of "allowed_chats" "allowed_users")
  ("allowed_chats" :list :doc "chats the bot answers in: group JIDs (...@g.us), or numbers for those people's direct chats"
                   :choices #'probe-chat-choices)
  ("allowed_users" :list :doc "phone numbers, with country code, allowed to drive the bot"
                   :choices #'probe-user-choices)
  ("owner" :list :doc "the operator's phone number, whose word is standing policy in the chat"
           :choices #'probe-user-choices)
  ("pairing" :boolean :default t
             :doc "a direct message from a number not allowed is answered with a pairing code, and the operator is told; /channels pair CODE lets them in")
  ("allowed_roles" :list
                   :doc "a group member's standing that lets them drive the bot as a listed user does: admin (every admin of the group) or superadmin (its creator)")
  ("require_mention" :boolean :default t
                     :doc "in groups, answer when @mentioned or when a message quotes one of the bot's; a direct chat always answers")
  ("free_response_chats" :list :choices #'probe-chat-choices
                         :doc "group JIDs where every allowed message may open a turn, mention or not")
  ("require_mention_chats" :list :choices #'probe-chat-choices
                           :doc "group JIDs where a message must mention or quote the bot even when require_mention is false")
  ("ignored_chats" :list :choices #'probe-chat-choices
                   :doc "chats the bot reads neither as asks nor as context")
  ("mention_patterns" :list
                      :doc "case-insensitive wake words that address the bot as its @mention does, anywhere in a line")
  ("other_addressees" :list
                      :doc "names other than the bot's that a line may open by addressing (\"ada, ...\"); such a line is observed, never answered")
  ("allow_private_chats" :boolean :default t :doc "answer in direct chats")
  ("allow_group_chats" :boolean :default t :doc "answer in groups")
  ("self_chat" :boolean :default :false
               :doc "with the bot linked to your own number: what you write in your chat with yourself (Message yourself) is an ask, answered there")
  ("read_receipts" :boolean :default t :doc "mark an ask the bot takes as read (the blue ticks)")
  ("reject_calls" :boolean :default t
                  :doc "decline a voice or video call -- a linked device cannot answer one -- and tell an allowed caller to send a voice message instead")
  ("online" :boolean :default t
            :doc "show the account online while connected, so the typing indicator shows; the phone then holds its own notifications for that account")
  ("session" :string :default "default"
             :doc "the name the linked device's keys are kept under in the home (channels/whatsapp/<session>.db)")
  ("device_name" :string :default "Nodecode" :doc "the name the phone's Linked devices list shows")
  ("driver_path" :path :doc "a WhatsApp driver built elsewhere (cd driver && go build); unset, the cell builds its own")
  ("phone" :string :doc "the linked account's number, read from the device when unset")
  ("catch_up_minutes" :integer :default 60
                      :doc "after the device was away -- a restart, a machine that was off -- answer the asks WhatsApp kept from the last this-many minutes, each marked as late, and skip older ones and any already answered; 0 answers none of them")
  ("max_concurrent_turns" :integer :default 4
                          :doc "turns that may run at once across every chat; more wait their turn")
  ("status_update_ms" :integer :default 4000
                      :doc "how often a running turn's card is edited, in ms; every edit is a message to each of the chat's devices")
  ("text_chunk_limit" :integer :default 6000
                      :doc "the most characters one message carries; a longer answer comes in parts")
  ("delivery_workers" :integer :default 4 :doc "threads that post to WhatsApp at once")
  ("reactions" :boolean :default :false
               :doc "mark each ask on the message itself: 👀 from admission through the working turn, cleared when the turn ends")
  ("stream" :boolean :default :false
            :doc "the model's words reach the chat as it writes them, a message edited as it grows; off by default, since every edit is a message to each device")
  ("voice_replies" :choice :options '("off" "on" "tts") :default "off"
                   :doc "an answer comes as a voice message too, below its words: on, to an ask said in a voice message; tts, to every ask; off, never")
  ("turn_budget_minutes" :integer :default 0
                        :doc "minutes an ask's turn may run: past them its tool calls are refused and it answers with what it has; 0, the default, caps nothing")
  ("room_tokens" :integer :default 40000
                :doc "the most history the chat keeps, in estimated tokens; past it the older half is evicted; 0 keeps everything")
  ("soul_file" :path :doc "a SOUL.md whose text is the chat's standing persona"))
