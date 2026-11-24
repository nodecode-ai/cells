;;;; cell.lisp --- the shepherd: watch opencode sessions, and answer them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; opencode (https://opencode.ai) runs a server, and its sessions ask and
;;;; wait: a turn finishes and wants input, the agent puts a question form up,
;;;; or a permission request stands between it and the next command. This cell
;;;; is the one that notices, and that lets the person's agent answer.
;;;;
;;;; Seven tools over the server's v2 HTTP API, and a watch that stands one
;;;; notice while anything waits, so a waiting session is not silent:
;;;;
;;;;   opencode_sessions   the inventory: id, title, folder, running, age
;;;;   opencode_waiting    what needs the person now, and the call that answers it
;;;;   opencode_tail        the last messages of one session, one line each
;;;;   opencode_send        say something to a session, steering or queued
;;;;   opencode_permission  once / always / reject, on the first pending request
;;;;   opencode_form        answer the first pending question, field by field
;;;;   opencode_interrupt   stop the running turn
;;;;
;;;; Rewritten in Common Lisp from okazakee's opencode-shepherd, a Python CLI
;;;; a person ran from cron or systemd and piped over SSH; here the watch is a
;;;; worker in the organism and its findings stand where the model reads them.
;;;; See NOTICE.

(in-package #:nodecode-opencode-shepherd)

;;; --- small words ----------------------------------------------------------------

(defparameter +space+ '(#\space #\newline #\tab #\return)
  "What a one-line rendering folds away.")

(defun nonblank (value)
  "VALUE when it is a non-empty string, else NIL."
  (and (stringp value) (plusp (length value)) value))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying so; NIL for NIL."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (concatenate 'string (subseq text 0 limit) "..."))))

(defun one-line (text &key (cap 500))
  "TEXT as one line of at most CAP characters."
  (when (stringp text)
    (clip (string-trim +space+
                       (with-output-to-string (out)
                         (loop for char across text
                               do (write-char (if (member char +space+) #\space char)
                                              out))))
          cap)))

(defun age-text (seconds)
  "SECONDS ago, in words short enough for one line."
  (let ((minutes (floor (max 0 (or seconds 0)) 60)))
    (cond ((< minutes 1) "now")
          ((< minutes 60) (format nil "~dm" minutes))
          ((< minutes 2880) (format nil "~dh" (floor minutes 60)))
          (t (format nil "~dd" (floor minutes 1440))))))

(defun clock-text (universal)
  "UNIVERSAL time as HH:MM:SS."
  (multiple-value-bind (second minute hour) (decode-universal-time universal)
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

;;; --- the wire: one opencode server, one exchange ----------------------------------

(defun password ()
  "The password this server asks for, if any: the section's own, else the
environment variable the section names."
  (or (nonblank (setting :password))
      (let ((variable (setting :password-env)))
        (and (nonblank variable) (nonblank (uiop:getenv variable))))))

(defstruct (server (:constructor make-server) (:conc-name server-))
  "Where the person's opencode server listens, and how to reach it."
  url username password prefix timeout)

(defun server ()
  "The server the running settings describe."
  (let ((prefix (or (nonblank (setting :api-prefix)) "/api")))
    (make-server
     :url (string-right-trim "/" (setting :url))
     :username (or (nonblank (setting :username)) "opencode")
     :password (password)
     :prefix (if (string= (string-trim "/" prefix) "")
                 ""
                 (format nil "/~a" (string-trim "/" prefix)))
     :timeout (setting :timeout-seconds))))

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "Standard base64, the alphabet HTTP basic authentication means.")

(defun base64 (text)
  "TEXT's UTF-8 octets as base64, no line breaks: half of a Basic credential."
  (let ((octets (sb-ext:string-to-octets text :external-format :utf-8)))
    (with-output-to-string (out)
      (loop for index from 0 below (length octets) by 3
            for first-octet = (aref octets index)
            for second-octet = (if (< (+ index 1) (length octets))
                                   (aref octets (+ index 1))
                                   0)
            for third-octet = (if (< (+ index 2) (length octets))
                                  (aref octets (+ index 2))
                                  0)
            for triple = (+ (ash first-octet 16) (ash second-octet 8) third-octet)
            do (write-char (char +base64-alphabet+ (ldb (byte 6 18) triple)) out)
               (write-char (char +base64-alphabet+ (ldb (byte 6 12) triple)) out)
               (write-char (if (< (+ index 1) (length octets))
                               (char +base64-alphabet+ (ldb (byte 6 6) triple))
                               #\=)
                           out)
               (write-char (if (< (+ index 2) (length octets))
                               (char +base64-alphabet+ (ldb (byte 6 0) triple))
                               #\=)
                           out)))))

(defun url-encode (value)
  "VALUE as one URL path or query segment: unreserved characters as they are,
everything else percent-encoded from its UTF-8 octets."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets (princ-to-string value)
                                                    :external-format :utf-8)
          do (if (or (<= 48 octet 57) (<= 65 octet 90) (<= 97 octet 122)
                     (member octet '(45 46 95 126)))
                 (write-char (code-char octet) out)
                 (format out "%~2,'0X" octet)))))

(defun query-string (parameters)
  "PARAMETERS, an alist of (NAME . VALUE), as a query string without its ?."
  (format nil "~{~a~^&~}"
          (loop for (name . value) in parameters
                when (and value (not (equal value "")))
                  collect (format nil "~a=~a" (url-encode name) (url-encode value)))))

(defun request (method path &key parameters content)
  "One exchange with the opencode server: the decoded answer, or NIL when the
answer carries none. A server that cannot be reached, or that answers outside
2xx, is a refusal the model reads."
  (let* ((where (server))
         (url (format nil "~a~a~a~@[?~a~]"
                      (server-url where) (server-prefix where) path
                      (and parameters (query-string parameters))))
         (body (and content (nlk:encode-json-object content)))
         (credentials (and (server-password where)
                           (format nil "Basic ~a"
                                   (base64 (format nil "~a:~a"
                                                    (server-username where)
                                                    (server-password where))))))
         (headers (append (when credentials (list (cons "authorization" credentials)))
                          (when body (list (cons "content-type" "application/json"))))))
    (multiple-value-bind (text status)
        (handler-case (nlk:http method url :headers headers :content body
                                :timeout (server-timeout where))
          (error (condition)
            (fail "cannot reach the opencode server at ~a (~a ~a): ~a"
                  (server-url where) method path condition)))
      (unless (and (integerp status) (<= 200 status 299))
        (fail "the opencode server answered ~a to ~a ~a~@[: ~a~]"
              (if (integerp status) (format nil "HTTP ~a" status) "nothing")
              method path (clip (and (stringp text) (one-line text :cap 240)) 240)))
      (when (and (stringp text) (plusp (length text)))
        (handler-case (nlk:decode-json text)
          (error () nil))))))

;;; --- the server's shapes ---------------------------------------------------------

(defun json-string (object &rest path)
  "The string at PATH in decoded OBJECT, or NIL."
  (apply #'nlk:json-value object :string path))

(defun json-integer (object &rest path)
  "The integer at PATH in decoded OBJECT, or 0."
  (or (apply #'nlk:json-value object :integer path) 0))

(defun data-array (object)
  "The DATA array of an opencode answer, or an empty vector, so a LOOP ACROSS
needs no guard."
  (nlk:json-array object "data"))

(defun session-id (session)
  (json-string session "id"))

(defun session-title (session)
  (let ((title (json-string session "title")))
    (or (and title (string-trim +space+ title)) "")))

(defun session-parent (session)
  (nonblank (json-string session "parentID")))

(defun session-directory (session)
  (or (json-string session "location" "directory") ""))

(defun session-updated (session)
  "When SESSION last moved, in seconds since the epoch; 0 when it never did."
  (floor (json-integer session "time" "updated") 1000))

(defun session-age (session)
  (age-text (- (get-universal-time) (session-updated session))))

(defun active-ids ()
  "The ids of the sessions whose turn runs right now."
  (let ((data (nlk:json-value (request :get "/session/active") :object "data")))
    (when (hash-table-p data)
      (loop for id being the hash-keys of data collect id))))

(defun session-rows (&key (limit 60))
  "The server's recent sessions, most recently updated first."
  (coerce (data-array (request :get "/session"
                                :parameters (list (cons "limit" (princ-to-string limit))
                                                  (cons "order" "desc"))))
          'list))

(defun catalog (&key (limit 60))
  "Every recent session and the ids of the ones running. A running session the
listing did not carry is fetched, so a turn that just started is never
invisible. => (values SESSIONS ACTIVE)"
  (let* ((active (active-ids))
         (sessions (session-rows :limit limit)))
    (dolist (id active)
      (unless (find id sessions :key #'session-id :test #'equal)
        (let ((one (nlk:json-value (request :get (format nil "/session/~a" (url-encode id)))
                                    :object "data")))
          (when (hash-table-p one) (push one sessions)))))
    (values sessions active)))

(defun message-rows (id limit)
  "SESSION ID's most recent LIMIT messages, newest first."
  (coerce (data-array (request :get (format nil "/session/~a/message" (url-encode id))
                                :parameters (list (cons "limit" (princ-to-string limit))
                                                  (cons "order" "desc"))))
          'list))

(defun form-rows (id)
  "SESSION ID's pending questions."
  (coerce (data-array (request :get (format nil "/session/~a/form" (url-encode id)))) 'list))

(defun permission-rows (id)
  "SESSION ID's pending permission requests."
  (coerce (data-array (request :get (format nil "/session/~a/permission" (url-encode id))))
          'list))

(defun message-shell-ids (message)
  "The background shells MESSAGE started or names, by id."
  (let ((ids '()))
    (let ((own (json-string message "metadata" "shellID")))
      (when own (push own ids)))
    (loop for part across (nlk:json-array message "content")
          when (equal "tool" (json-string part "type"))
            do (dolist (from (list part (nlk:json-value part :object "state")))
                 (let ((id (and (hash-table-p from) (json-string from "metadata" "shellID"))))
                   (when id (push id ids)))))
    (remove-duplicates (nreverse ids) :test #'equal)))

(defun shell-running-p (id directory)
  "Whether background shell ID still runs. A shell the server cannot describe
is not running: it is the turn that matters, and an unanswered shell must not
silence one."
  (let ((info (handler-case
                  (nlk:json-value (request :get (format nil "/shell/~a" (url-encode id))
                                           :parameters (and (nonblank directory)
                                                            (list (cons "location[directory]"
                                                                        directory))))
                                  :object "data")
                (opencode-shepherd-error () nil))))
    (equal "running" (and (hash-table-p info) (json-string info "status")))))

(defun message-text (message)
  "The text of MESSAGE as one line: what the agent said, what the person typed,
or how a turn ended."
  (let ((type (json-string message "type")))
    (cond
      ((member type '("assistant" "system" "synthetic") :test #'equal)
       (let ((pieces '()))
         (loop for part across (nlk:json-array message "content")
               for kind = (json-string part "type")
               do (cond ((and (equal kind "text") (nonblank (json-string part "text")))
                         (push (one-line (json-string part "text") :cap 400) pieces))
                        ((equal kind "tool")
                         (let ((status (or (json-string part "state" "status")
                                           (json-string part "metadata" "status"))))
                           (push (format nil "[tool:~a~@[:~a~]]"
                                         (or (nonblank (json-string part "name")) "?")
                                         (nonblank status))
                                 pieces)))))
         (and pieces (format nil "~{~a~^ ~}" (nreverse pieces)))))
      ((equal type "user")
       (one-line (or (json-string message "payload" "text")
                     (json-string message "text")
                     (loop for part across (nlk:json-array message "content")
                           for text = (and (equal "text" (json-string part "type"))
                                           (nonblank (json-string part "text")))
                           when text return text))
                 :cap 400))
      ((equal type "idle")
       (format nil "turn ended~@[ (~a)~]" (nonblank (json-string message "outcome"))))
      (t nil))))

;;; --- resolving a session by name --------------------------------------------------

(defun match-session (sessions fragment)
  "=> (values SESSION CANDIDATES), exactly one of them: the one session
FRAGMENT names -- an exact id, then a fragment of an id, then a fragment of a
title -- else the near matches, or nothing."
  (let* ((needle (string-downcase fragment))
         (exact (find-if (lambda (session) (equal needle (string-downcase (or (session-id session) ""))))
                          sessions)))
    (flet ((named-by (reader)
             (remove-if-not (lambda (session) (search needle (string-downcase (or (funcall reader session) ""))))
                            sessions)))
      (cond
        (exact (values exact nil))
        (t (let ((by-id (named-by #'session-id)))
             (cond ((= 1 (length by-id)) (values (first by-id) nil))
                   ((> (length by-id) 1) (values nil by-id))
                   (t (let ((by-title (named-by #'session-title)))
                        (cond ((= 1 (length by-title)) (values (first by-title) nil))
                              ((> (length by-title) 1) (values nil by-title))
                              (t (values nil '()))))))))))))

(defun session-label (session)
  (format nil "~a ~s" (session-id session) (session-title session)))

(defun resolve-session (fragment)
  "The one session FRAGMENT names. Refuses with the candidates when it is
ambiguous, and with what there is to choose from when nothing matches."
  (multiple-value-bind (sessions active) (catalog :limit 80)
    (declare (ignore active))
    (multiple-value-bind (session candidates) (match-session sessions fragment)
      (cond
        (session session)
        (candidates
         (fail "~s names ~d opencode sessions; say which one: ~{~a~^, ~}"
               fragment (length candidates)
               (loop for candidate in (subseq candidates 0 (min 10 (length candidates)))
                     collect (session-label candidate))))
        ((null sessions)
         (fail "no opencode session matches ~s: the server has none" fragment))
        (t
         (fail "no opencode session matches ~s; the recent ones are: ~{~a~^, ~}"
               fragment
               (loop for candidate in (subseq sessions 0 (min 8 (length sessions)))
                     collect (session-label candidate))))))))

;;; --- what waits -------------------------------------------------------------------

(defparameter +idle-grace-seconds+ 60
  "How long a finished turn must sit before it counts as waiting: opencode
emits idle between two halves of one answer, and a turn that resumes by itself
should never be news.")

(defun form-item (session form)
  "A pending question as the item the tools and the watch carry."
  (list :kind :form
        :key (format nil "form:~a" (or (json-string form "id") "?"))
        :session (session-id session)
        :title (session-title session)
        :sub (and (session-parent session) t)
        :form form))

(defun permission-item (session permission)
  "A pending permission request as the item the tools and the watch carry."
  (list :kind :permission
        :key (format nil "perm:~a" (or (json-string permission "id") "?"))
        :session (session-id session)
        :title (session-title session)
        :sub (and (session-parent session) t)
        :permission permission))

(defun finished-turn-item (session messages)
  "SESSION's finished turn as an item, or NIL when it does not wait: it is
running, its last message is not an idle, or a background shell still runs and
will resume the turn by itself."
  (let ((last (first messages)))
    (when (and last (equal "idle" (json-string last "type")))
      (let ((shells (loop for message in (subseq messages 0 (min 5 (length messages)))
                          append (message-shell-ids message))))
        (unless (loop for id in (remove-duplicates shells :test #'equal)
                      thereis (shell-running-p id (session-directory session)))
          (let* ((outcome (or (nonblank (json-string last "outcome")) ""))
                 (created (json-integer last "time" "created"))
                 (text (loop for message in messages
                             for text = (and (equal "assistant" (json-string message "type"))
                                             (message-text message))
                             when text return text)))
            (list :kind (if (equal outcome "succeeded") :waiting :trouble)
                  :key (format nil "idle:~a:~a:~a" (session-id session) created outcome)
                  :session (session-id session)
                  :title (session-title session)
                  :sub nil
                  :outcome outcome
                  :created (floor created 1000)
                  :text (or text ""))))))))

(defun session-waits (session active now)
  "The items SESSION waits on: its pending questions and permissions, and --
top-level, and not waiting on a background shell -- a finished turn."
  (let ((id (session-id session))
        (items '()))
    (dolist (form (form-rows id)) (push (form-item session form) items))
    (dolist (permission (permission-rows id)) (push (permission-item session permission) items))
    (unless (or items (member id active :test #'equal) (session-parent session))
      (let ((item (finished-turn-item session (message-rows id 6))))
        (when (and item
                   (or (null +idle-grace-seconds+)
                       (<= (+ (getf item :created) +idle-grace-seconds+) now)))
          (push item items))))
    (nreverse items)))

(defun waiting-items (&key (window-hours 12) (max-sessions 20) (session ""))
  "What needs the person now: a pending question, a pending permission, or a
top-level turn that finished and waits for input -- across every recent
session, or the one SESSION names."
  (multiple-value-bind (sessions active) (catalog :limit 60)
    (let* ((now (get-universal-time))
           (window (* 3600 (max 1 window-hours)))
           (chosen (if (nonblank session)
                       (list (resolve-session session))
                       (let ((recent (remove-if-not
                                      (lambda (one)
                                        (or (member (session-id one) active :test #'equal)
                                            (<= (- now (session-updated one)) window)))
                                      sessions)))
                         (subseq (sort recent #'> :key #'session-updated)
                                 0 (min (max 1 max-sessions) (length recent)))))))
      (loop for one in chosen append (session-waits one active now)))))

;;; --- saying what waits ------------------------------------------------------------

(defun field-lines (form)
  "FORM's fields, one line each: its key, what it asks, its options."
  (loop for field across (nlk:json-array form "fields")
        collect (format nil "    ~a~@[: ~a~]~@[ -> ~{~a~^ / ~}~]~@[ (default ~a)~]"
                        (or (nonblank (json-string field "key")) "?")
                        (or (nonblank (json-string field "title"))
                            (nonblank (json-string field "description")))
                        (loop for option across (nlk:json-array field "options")
                              collect (or (nonblank (json-string option "label"))
                                          (nonblank (json-string option "value"))
                                          "?"))
                        (nlk:json-value field :any "default"))))

(defun item-lines (item)
  "ITEM as the lines a person and a model read: what it is, and the call that
answers it."
  (let ((session (getf item :session))
        (title (getf item :title))
        (sub (getf item :sub)))
    (case (getf item :kind)
      (:form
       (let ((form (getf item :form)))
         (append (list (format nil "? ~s~a asks:~@[ ~a~]"
                                title (if sub " (sub-session)" "")
                                (nonblank (json-string form "title"))))
                 (field-lines form)
                 (list (format nil "    answer: opencode_form session=~s answers={<field key>: <value>}"
                               session)))))
      (:permission
       (let* ((permission (getf item :permission))
              (resources (coerce (nlk:json-array permission "resources") 'list))
              (shown (subseq resources 0 (min 4 (length resources))))
              (message (nonblank (json-string permission "message"))))
         (append (list (format nil "! ~s~a requests permission: ~a~@[ (~{~a~^ | ~})~]"
                                title (if sub " (sub-session)" "")
                                (or (nonblank (json-string permission "action")) "?")
                                shown))
                 (when message (list (format nil "    it says: ~a" (one-line message :cap 300))))
                 (list (format nil "    answer: opencode_permission session=~s decision=\"once\" (or always, reject)"
                               session)))))
      ((:waiting :trouble)
       (let ((text (nonblank (one-line (getf item :text) :cap 220))))
         (append
          (list (format nil "~a ~s: ~a"
                        (if (eq (getf item :kind) :trouble) "!" "=")
                        title
                        (if (eq (getf item :kind) :trouble)
                            (format nil "its turn ended~@[ (~a)~]"
                                    (nonblank (getf item :outcome)))
                            "its turn finished and waits for input"))
                (format nil "    reply: opencode_send session=~s text=\"...\"" session))
          (when text (list (format nil "    last: ~s" text))))))
      (t (list (format nil "? ~s" title))))))

(defun waiting-text (items)
  "ITEMS as the report both the tool and the notice say, one block per item."
  (if (null items)
      "nothing in opencode waits on the person"
      (format nil "~d waiting in opencode:~%~{~a~%~}"
              (length items)
              (loop for item in items append (loop for line in (item-lines item)
                                                  collect (or line ""))))))

(defun waiting-report (&key (session "") (window-hours 12) (max-sessions 20))
  "The waiting report, as the tool and the /opencode command answer it."
  (waiting-text (waiting-items :window-hours window-hours
                                :max-sessions max-sessions
                                :session session)))

(defun session-report (fragment count)
  "One session's last COUNT messages, oldest first, then what still waits on it."
  (let* ((session (resolve-session fragment))
         (id (session-id session))
         (messages (reverse (message-rows id (max 1 count))))
         (lines (loop for message in messages
                      collect (format nil "[~a] [~a] ~a"
                                      (clock-text (floor (json-integer message "time" "created")
                                                         1000))
                                      (or (string-downcase (json-string message "type")) "?")
                                      (or (message-text message) ""))))
         (waits (session-waits session '() (get-universal-time))))
    (format nil "== ~a ~s ==  (moved ~a ago~@[, ~d waiting~])~%~{~a~%~}~@[~%~{~a~%~}~]"
            id (session-title session) (session-age session)
            (and waits (length waits))
            lines
            (and waits (loop for item in waits append (item-lines item))))))

;;; --- the tools --------------------------------------------------------------------

(define-verb list-sessions (call)
  "The person's opencode sessions, most recently updated first, as one line each: id, whether its turn runs, how long since it moved, whether it is a sub-session, and its title."
  (multiple-value-bind (sessions active) (catalog :limit (session-list-limit call))
    (if (null sessions)
        "no opencode sessions on the server yet"
        (format nil "~d opencode session~:p~@[, ~d running~]:~%~{~a~%~}"
                (length sessions) (and active (length active))
                (loop for session in (subseq sessions 0 (min (session-list-limit call)
                                                             (length sessions)))
                      collect (format nil "~a  ~a  ~a ago~@[  child~]  ~a"
                                      (session-id session)
                                      (if (member (session-id session) active :test #'equal)
                                          "RUNNING" "idle   ")
                                      (session-age session)
                                      (session-parent session)
                                      (session-title session)))))))

(define-verb waiting-sessions (call)
  "What needs the person now across their opencode sessions, and the call that answers each one."
  (waiting-report :session (session-waiting-session call)
                  :window-hours (session-waiting-window-hours call)
                  :max-sessions (session-waiting-max-sessions call)))

(define-verb tail-session (call)
  "One session's last messages, one line each, and whether it waits on anything."
  (session-report (session-tail-session call) (session-tail-messages call)))

(define-verb send-prompt (call)
  "Say something to a session: steer the turn that runs, or queue for the next."
  (let* ((session (resolve-session (session-send-session call)))
         (id (session-id session))
         (text (string-right-trim '(#\newline)
                                   (session-send-text call)))
         (answer (request :post (format nil "/session/~a/prompt" (url-encode id))
                          :content (nlk:json-object "text" text
                                                     :when (session-send-queue call)
                                                     "delivery" "queue"))))
    (format nil "sent to ~a (~s)~@[, delivered when the running turn ends~]~@[ as ~a~]"
            id (session-title session) (session-send-queue call)
            (json-string answer "id"))))

(define-verb reply-permission (call)
  "Answer the first pending permission request of a session: once, always or reject."
  (let* ((session (resolve-session (permission-reply-session call)))
         (id (session-id session))
         (pending (permission-rows id)))
    (when (null pending)
      (fail "no permission request waits on ~a (~s): opencode_waiting says what does"
            id (session-title session)))
    (let* ((permission (first pending))
           (decision (permission-reply-decision call))
           (message (nonblank (permission-reply-message call))))
      (request :post (format nil "/session/~a/permission/~a/reply"
                             (url-encode id)
                             (url-encode (or (json-string permission "id") "")))
               :content (nlk:json-object "decision" decision
                                          :opt "message" message))
      (format nil "answered ~a on ~a (~s) with ~a: the agent goes on"
              (or (nonblank (json-string permission "action")) "the request")
              id (session-title session) decision))))

(define-verb answer-form (call)
  "Answer the first pending question of a session, one value per field."
  (let* ((session (resolve-session (form-reply-session call)))
         (id (session-id session))
         (pending (form-rows id)))
    (when (null pending)
      (fail "no question waits on ~a (~s): opencode_waiting says what does"
            id (session-title session)))
    (let ((form (first pending))
          (answers (form-reply-answers call)))
      (request :post (format nil "/session/~a/form/~a/reply"
                             (url-encode id)
                             (url-encode (or (json-string form "id") "")))
               :content (nlk:json-object "answer" answers))
      (format nil "answered the question on ~a (~s): ~{~a~^, ~}"
              id (session-title session)
              (loop for key being the hash-keys of answers using (hash-value value)
                    collect (format nil "~a=~a" key value))))))

(define-verb interrupt-session (call)
  "Stop the turn a session runs, as its stop button does."
  (let ((session (resolve-session (session-interrupt-session call))))
    (request :post (format nil "/session/~a/interrupt" (url-encode (session-id session))))
    (format nil "interrupted ~a (~s)" (session-id session) (session-title session))))

;;; --- the watch: what is new since the last poll --------------------------------------

(defstruct (watching (:constructor make-watching) (:conc-name watching-))
  "What the watch remembers between polls: the keys already reported, whether
the first poll has seeded the finished turns, and the notice standing now."
  (seen (make-hash-table :test #'equal))
  (seeded nil)
  (fails 0)
  (standing nil))

(defvar *watching* nil
  "The running cell's watch state, or NIL while it is not started.")

(defparameter +notice-key+ "nodecode-opencode-shepherd"
  "The board key the shepherd's one standing notice rides: replaced on every
poll that changes it, cleared when nothing waits.")

(defun watch-seen-p (key)
  (gethash key (watching-seen *watching*)))

(defun watch-see (items)
  (let ((now (get-universal-time)))
    (dolist (item items)
      (setf (gethash (getf item :key) (watching-seen *watching*)) now))))

(defun watch-prune ()
  "Forget what was reported more than a week ago."
  (let ((cutoff (- (get-universal-time) (* 7 86400))))
    (loop for key being the hash-keys of (watching-seen *watching*)
            using (hash-value seen)
          when (< seen cutoff)
            do (remhash key (watching-seen *watching*)))))

(defun watch-step ()
  "One poll: what waits now, deduped against every poll before it, stood as
one notice while anything does. A server that cannot be reached is said after
three polls in a row, so one restart is not news."
  (let ((items nil)
        (trouble nil))
    (handler-case (setf items (waiting-items))
      (opencode-shepherd-error (condition) (setf trouble (princ-to-string condition)))
      (error (condition) (setf trouble (format nil "~a" condition))))
    (when *watching*
      (cond
        (trouble
         (incf (watching-fails *watching*))
         (when (>= (watching-fails *watching*) 3)
           (nle:notice (format nil "opencode-shepherd: ~a" (clip (one-line trouble :cap 240) 240))
                       :level :warning :key +notice-key+)))
        (t
         (setf (watching-fails *watching*) 0)
         (let ((fresh (loop for item in items
                            unless (watch-seen-p (getf item :key))
                              unless (and (not (watching-seeded *watching*))
                                          (member (getf item :kind) '(:waiting :trouble)))
                                collect item))
               (text (and items (waiting-text items))))
           (cond
             ((null items)
              (when (watching-standing *watching*)
                (nle:notice nil :key +notice-key+)
                (setf (watching-standing *watching*) nil)))
             (fresh
              (nle:notice text :level :warning :key +notice-key+)
              (setf (watching-standing *watching*) text))
             ((not (equal text (watching-standing *watching*)))
              (nle:notice text :level :quiet :key +notice-key+)
              (setf (watching-standing *watching*) text)))
           (watch-see items)
           (watch-prune)
           (setf (watching-seeded *watching*) t)))))))

(defun run-opencode (args session-id)
  "/opencode [session]: what waits, or one session's last messages."
  (declare (ignore session-id))
  (running-settings)
  (let ((fragment (and args (string-trim +space+ args))))
    (values (if (nonblank fragment)
                (session-report fragment 10)
                (waiting-report))
            nil)))

;;; --- the cell ------------------------------------------------------------------

(defun check-config ()
  "Refuse a section no watch could be started from, naming the member."
  (unless (nonblank (setting :url))
    (error 'nlk:config-refusal
           :detail (concatenate
                    'string
                    "opencode-shepherd: set \"url\" to the address your opencode server prints "
                    "(`opencode serve` says http://127.0.0.1:4096 unless told otherwise); "
                    "\"password_env\" names the environment variable holding its password, if it has one"))))

(nle:define-cell opencode-shepherd
  (:section ("opencode-shepherd")
    (:guide "run `opencode serve` or `opencode web` on this machine, then put the address it prints here (usually http://127.0.0.1:4096); if the server asks for a password, name the environment variable that holds it and start nodecode with it exported; nothing else is needed to watch")
    ("url" :string :required t
     :doc "the address the opencode server prints, e.g. http://127.0.0.1:4096")
    ("username" :string :default "opencode"
     :doc "the HTTP basic user the opencode server expects")
    ("password" :secret
     :doc "the server password itself; password_env is the tidier way to give it")
    ("password_env" :env :default "OPENCODE_SERVER_PASSWORD"
     :doc "the environment variable holding the server password")
    ("api_prefix" :string :default "/api"
     :doc "the path the v2 API sits under on that server")
    ("timeout_seconds" :integer :default 20 :min 1
     :doc "one exchange with the server's ceiling")
    ("poll_seconds" :integer :default 60 :min 0
     :doc "how often the watch asks what waits, in seconds; 0 watches nothing"))
  (:tool "opencode_sessions" (nlk:contract 'session-list) #'list-sessions)
  (:tool "opencode_waiting" (nlk:contract 'session-waiting) #'waiting-sessions)
  (:tool "opencode_tail" (nlk:contract 'session-tail) #'tail-session)
  (:tool "opencode_send" (nlk:contract 'session-send) #'send-prompt)
  (:tool "opencode_permission" (nlk:contract 'permission-reply) #'reply-permission)
  (:tool "opencode_form" (nlk:contract 'form-reply) #'answer-form)
  (:tool "opencode_interrupt" (nlk:contract 'session-interrupt) #'interrupt-session)
  (:command "opencode" #'run-opencode
            :description "What your opencode sessions wait on"
            :session nil)
  (:help :opencode
   "opencode: watch the person's opencode sessions and answer them"
   "opencode (https://opencode.ai) is a coding agent the person runs as a server on their machine; this cell watches it from Nodecode.\n\nCall opencode_waiting first when the person asks what their opencode is doing, or before they ask: it lists, per session, the pending question with its fields and options, the pending permission with its action and resources, and any top-level turn that finished and waits for input -- each with the call that answers it. A session waiting on a background shell is not listed (opencode resumes it by itself), and a sub-session's finished turn is not either (its parent reports it).\n\nAnswer with the three tools that decide: opencode_form for a question (one value per field key, exactly the options it offered), opencode_permission for a permission (once, always or reject, and why when rejecting), and opencode_send for anything else -- steering the running turn by default, or queueing with queue=1. opencode_tail reads a session's last messages before you steer it, opencode_sessions is the inventory when the title is unknown (a call takes an id or any fragment of an id or title), and opencode_interrupt stops a turn that ran away.\n\nA permission reply is a real approval the agent acts on: relay what the person decided, never what seems convenient. The watch stands a notice while anything waits, so silence means nothing does.\n\nSettings live in the `opencode-shepherd` section of the config: url (required: the address the server prints), username, password or password_env, timeout_seconds, and poll_seconds (0 turns the watch off).")
  (:start (lambda ()
            (check-config)
            (setf *watching* (make-watching))
            (let ((seconds (setting :poll-seconds)))
              (when (and (integerp seconds) (plusp seconds))
                (let ((worker (nlk:worker-start "opencode-shepherd" #'watch-step
                                                :wake seconds :prime t)))
                  (nle:on-stop (lambda ()
                                 (nlk:worker-stop worker)
                                 (when (and *watching* (watching-standing *watching*))
                                   (nle:notice nil :key +notice-key+))))))))))
