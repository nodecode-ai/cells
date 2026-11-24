;;;; cell.lisp --- a live, masked page of one session, for the people you invite.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Spectator plugin for Hermes, against Nodecode's own
;;;; seams instead of a host dashboard: the session's facts are already in the
;;;; event log, so the page reads them (NLK:EVENTS) rather than a host state
;;;; database; the page rides the gateway's own port (NLE:ROUTE :PUBLIC)
;;;; rather than a second server; an approved suggestion enters the session
;;;; through the one in-process ingress (NLE:SUBMIT) rather than a host
;;;; method. Nothing leaves this machine: the only address the cell ever
;;;; hands out is the one the operator puts in the config.
;;;;
;;;; Privacy is the point, so it is one function: MASK-TEXT runs over every
;;;; line the page carries -- paths, NAME=value assignments and long opaque
;;;; tokens are rewritten before a guest can read them. What travels is a
;;;; summary of the session, never a transcript: an assistant message's text,
;;;; a tool's name, a turn's start and end. A prompt the operator typed is
;;;; only ever counted ("a turn started"), never quoted.

(defpackage #:nodecode-spectator
  (:use #:cl)
  (:nicknames #:spectator)
  (:export
   ;; verbs, each a string or a refusal
   #:share #:pending #:approve #:reject #:revoke
   ;; the one condition
   #:spectator-error))

(in-package #:nodecode-spectator)

(nlk:define-peripheral spectator :not-running t :lock "spectator")

;;; --- what the cell keeps ---------------------------------------------------
;;; Live-only, like the page: an invite dies with the process, and so does
;;; what a guest left. Nothing here is durable, so nothing needs a schema.

(defvar *invites* '()
  "Open invites, newest first: (:token :session :expires :since).")

(defvar *suggestions* '()
  "What guests left, newest first: (:id :token :session :text :at :status).")

(defvar *submit* #'nle:submit
  "How an approved suggestion reaches its session: (FUNCTION SESSION TEXT
&KEY SOURCE SOURCE-ID). Tests rebind it; nothing else does.")

(defparameter *line-chars* 400
  "How much of one line the page carries.")

(defparameter *max-suggestion-chars* 2000
  "What one suggestion may carry, past which the route refuses it.")

(defparameter +manual+
  "spectator: a live page of this session for the people you invite, masked.
  (spectator:share \"SESSION\")   mint an invite and answer its address, which
      is SPECTATOR.PUBLIC_URL + /spectator/TOKEN/. The page shows the turn's
      start and end, each tool call by name, and each assistant message's
      text, with absolute paths, NAME=value assignments and long opaque
      tokens rewritten to ~/..., NAME=*** and ***. Invites last
      SPECTATOR.INVITE_MINUTES; nothing is stored past the process.
  (spectator:pending \"SESSION\")  what guests left, oldest first, each by id.
  (spectator:approve \"ID\")      put one guest suggestion into its session as
      a prompt beginning [spectator], and answer what was sent.
  (spectator:reject \"ID\")       drop one, and answer how many are left.
  (spectator:revoke)             close every open invite and answer how many.
/spectator in a shell does the same: share, suggestions, approve ID, reject
ID, close. A guest's page is public on the gateway's port, so put a tunnel
in front of it and set SPECTATOR.PUBLIC_URL to that address; anyone holding
the token can read the page, which is why the mask is not optional for them."
  "What (help :spectator) answers while the cell runs.")

;;; --- small words ----------------------------------------------------------

(defun now ()
  "Seconds since the epoch, as invites and suggestions are stamped."
  (get-universal-time))

(defun fresh-token ()
  "24 hex characters nothing can guess: the whole credential a link carries."
  (subseq (nlk:sha256-text (format nil "~a-~a-~a" (now) (random most-positive-fixnum)
                                   (gensym)))
          0 24))

(defun clip (text limit)
  "TEXT at most LIMIT characters, the ellipsis counted inside it."
  (if (<= (length text) limit)
      text
      (concatenate 'string (subseq text 0 (max 0 (- limit 3))) "...")))

(defun watermark (session)
  "The session's newest log position: where its invite starts reading, so a
page never replays what came before it was shared."
  (or (nlk:events :session-id session :as :value :columns '("max(log_position)")) 0))

;;; --- the mask ---------------------------------------------------------------
;;; One token at a time, in the order the original applied its rules: a path
;;; first, then a NAME=value assignment, then an opaque token. Reading a line
;;; back gives a line a person can read, not a transcript.

(defun path-token-p (token)
  "TOKEN looks like an absolute path, POSIX, home-relative or a drive letter."
  (or (uiop:string-prefix-p "/" token)
      (uiop:string-prefix-p "~/" token)
      (and (>= (length token) 3)
           (alpha-char-p (char token 0))
           (char= #\: (char token 1))
           (member (char token 2) '(#\/ #\\)))))

(defun run-together-p (token)
  "TOKEN is all of one alphabet: a name, not a sentence."
  (every (lambda (char) (or (alphanumericp char) (find char "_-."))) token))

(defun assignment-token (token)
  "TOKEN as NAME=*** when it is a NAME=value assignment, else NIL."
  (let ((equals (position #\= token)))
    (when (and equals (>= equals 3) (run-together-p (subseq token 0 equals))
               (not (digit-char-p (char token 0))))
      (concatenate 'string (subseq token 0 equals) "=***"))))

(defun opaque-token-p (token)
  "TOKEN is long and mixed enough to be a key, a digest or an id."
  (and (>= (length token) 24)
       (run-together-p token)
       (some #'alpha-char-p token)
       (some #'digit-char-p token)
       (not (path-token-p token))))

(defun mask-token (token)
  "TOKEN as the page may show it."
  (cond ((path-token-p token) "~/...")
        ((assignment-token token))
        ((opaque-token-p token) "***")
        (t token)))

(defun mask-text (text &key (mask t))
  "TEXT one line, MASK's rules applied, at most *LINE-CHARS* characters."
  (cond ((null mask) (clip text *line-chars*))
        (t (clip (format nil "~{~a~^ ~}"
                         (mapcar #'mask-token
                                 (uiop:split-string text :separator '(#\Space #\Tab #\Newline #\Return))))
                 *line-chars*))))

;;; --- what a fact reads as ----------------------------------------------------
;;; The session's own kinds, each as one line a guest can follow. A kind the
;;; page has nothing to say about answers NIL and is left out.

(defun content-text (message)
  "A wire message's text, its content a string or an array of typed parts."
  (or (nlk:json-value message :string "content")
      (let ((parts (nlk:json-value message :array "content")))
        (when parts
          (let ((texts (loop for part across parts
                             for text = (and (hash-table-p part)
                                             (nlk:json-value part :string "text"))
                             when text collect text)))
            (and texts (format nil "~{~a~^ ~}" texts)))))))

(defun message-text (payload)
  "The assistant text a turn.assistant_message_completed payload carries."
  (or (nlk:json-value payload :string "message")
      (let ((message (nlk:json-value payload :object "message")))
        (and message (content-text message)))))

(defun fact-line (kind payload mt)
  "One line for fact KIND with PAYLOAD, masked when MASK is true."
  (let ((line (cond ((string= kind "turn.started") "a turn started")
                    ((string= kind "turn.tool_call_started")
                     (let ((name (or (nlk:json-value payload :string "name")
                                     (nlk:json-value payload :string "tool")
                                     (nlk:json-value payload :string "tool_name"))))
                       (format nil "called ~a" (if name (mask-token name) "a tool"))))
                    ((string= kind "turn.tool_result") "a tool answered")
                    ((string= kind "turn.assistant_message_completed")
                     (or (message-text payload) "the model wrote a message"))
                    ((string= kind "turn.completed") "turn done")
                    ((string= kind "turn.failed") "turn failed")
                    ;; Any other fact that carries a line of its own -- a cell
                    ;; recording one for the page -- reads as that line.
                    (t (nlk:json-value payload :string "line")))))
    (and line (mask-text line :mask mt))))

(defun feed (invite &key since limit)
  "The newest LIMIT facts of INVITE's session after SINCE (its own start by
default), oldest first: (:seq :at :line) each."
  (let* ((rows (nlk:events :session-id (getf invite :session)
                            :after (or since (getf invite :since))
                            :order :newest
                            :limit (or limit (setting :max-events))
                            :as :rows
                            :columns '("log_position" "kind" "occurred_at" "payload")))
         (mask (setting :mask))
         (lines '()))
    (dolist (row rows)
      (destructuring-bind (position kind at payload) row
        (let ((line (handler-case (fact-line kind (nlk:decode-json payload) mask)
                      (error () nil))))
          (when line (push (list :seq position :at at :line line) lines)))))
    lines))

;;; --- invites and suggestions --------------------------------------------------

(defun live-invite (token)
  "The invite TOKEN names, while it lasts, or NIL."
  (with-spectator-lock
    (find-if (lambda (invite) (and (equal token (getf invite :token))
                                   (> (getf invite :expires) (now))))
             *invites*)))

(defun public-base ()
  "The address guests open, without its trailing slash, or a refusal."
  (let ((base (nlk:trimmed (or (setting :public-url) ""))))
    (when (zerop (length base))
      (fail "spectator: no public_url is set, so there is no address to hand out: set ~
spectator.public_url to the address that reaches this gateway's port (a tunnel's), then ~
(restart-cells \"nodecode-spectator\")"))
    (string-right-trim "/" base)))

(define-verb share (session &key minutes)
  "Mint an invite for SESSION and answer the address a guest opens."
  (unless (and (stringp session) (plusp (length (nlk:trimmed session))))
    (fail "spectator: no session to share"))
  (let* ((base (public-base))
         (minutes (or minutes (setting :invite-minutes)))
         (invite (list :token (fresh-token) :session session
                       :expires (+ (now) (* 60 minutes)) :since (watermark session))))
    (when (<= minutes 0)
      (fail "spectator: minutes must be positive"))
    (with-spectator-lock (push invite *invites*))
    (format nil "spectator: ~a/spectator/~a/ is open for ~d minute~:p -- masked, and the guest ~
sees only what came after it was shared. /spectator suggestions lists what they leave."
            base (getf invite :token) minutes)))

(defun suggestion (id)
  "The suggestion ID names, or a refusal."
  (or (with-spectator-lock
        (find id *suggestions* :test #'equal
                                :key (lambda (one) (getf one :id))))
      (fail "spectator: no suggestion ~a; /spectator suggestions lists them" id)))

(define-verb pending (&optional session)
  "What guests left on SESSION (every session when NIL), oldest first: a list
of (:id :token :session :text :at :status)."
  (with-spectator-lock
    (loop for one in (reverse *suggestions*)
          when (or (null session) (equal session (getf one :session)))
            collect one)))

(define-verb suggest (token text)
  "Take one guest suggestion from the page behind TOKEN."
  (let ((invite (live-invite token)))
    (unless invite (fail "spectator: this invite is closed or expired"))
    (let ((text (nlk:trimmed (or text ""))))
      (when (zerop (length text))
        (fail "spectator: an empty suggestion"))
      (when (> (length text) *max-suggestion-chars*)
        (fail "spectator: a suggestion is at most ~d characters" *max-suggestion-chars*))
      (let ((one (list :id (format nil "g-~a" (subseq (fresh-token) 0 6))
                       :token token :session (getf invite :session)
                       :text (clip text *max-suggestion-chars*) :at (now) :status "waiting")))
        (with-spectator-lock (push one *suggestions*))
        one))))

(define-verb approve (id)
  "Put the suggestion ID into its session as a prompt, and answer what was sent."
  (let* ((one (suggestion id))
         (session (getf one :session))
         (prompt (format nil "[spectator] a guest suggested: ~a" (getf one :text))))
    (when (equal "approved" (getf one :status))
      (fail "spectator: ~a was already approved" id))
    (let ((admission (handler-case (funcall *submit* session prompt
                                            :source "spectator" :source-id (getf one :id))
                        (error (condition)
                          (fail "spectator: ~a did not reach ~a: ~a" id session condition)))))
      (unless admission
        (fail "spectator: ~a did not reach ~a" id session))
      (setf (getf one :status) "approved")
      (format nil "spectator: approved ~a into ~a -- it now reads: ~a" id session prompt))))

(define-verb reject (id)
  "Drop the suggestion ID, and answer what is left."
  (let ((one (suggestion id)))
    (setf (getf one :status) "rejected")
    (format nil "spectator: ~a rejected; ~d still waiting"
            id (length (remove-if-not (lambda (other) (equal "waiting" (getf other :status)))
                                      (pending (getf one :session)))))))

(define-verb revoke ()
  "Close every open invite, and answer how many."
  (with-spectator-lock
    (let ((count (length *invites*)))
      (setf *invites* '())
      (format nil "spectator: ~d invite~:p closed" count))))

(defun status-text (session)
  "What /spectator says with no argument: the invites open on SESSION and
what waits behind them."
  (let ((invites (with-spectator-lock
                   (remove-if-not (lambda (invite) (and (equal session (getf invite :session))
                                                        (> (getf invite :expires) (now))))
                                  *invites*)))
        (waiting (pending session)))
    (format nil "spectator: ~d invite~:p open, ~d suggestion~:p waiting~%~@{~a~%~}"
            (length invites) (length waiting)
            (loop for invite in (reverse invites)
                  collect (format nil "  ~a/spectator/~a/ -- ~d minute~:p left"
                                  (or (ignore-errors (public-base)) "")
                                  (getf invite :token)
                                  (ceiling (- (getf invite :expires) (now)) 60))))))

(defun suggestions-text (session)
  "The waiting suggestions as lines a person reads, oldest first."
  (let ((waiting (remove-if-not (lambda (one) (equal "waiting" (getf one :status)))
                                 (pending session))))
    (if (null waiting)
        "spectator: no suggestions are waiting"
        (format nil "spectator: ~d suggestion~:p waiting~%~{  ~a~%~}"
                (length waiting)
                (loop for one in waiting
                      collect (format nil "~a  ~a" (getf one :id) (getf one :text)))))))

;;; --- the page ---------------------------------------------------------------
;;; One route under /spectator/, three answers: the page, its events, and a
;;; suggestion. The token in the path is the whole credential; the gateway
;;; has already refused any Host that is not loopback, and a tunnel in front
;;; carries it the rest of the way.

(defun percent-decode (text)
  "TEXT with %XX and + decoded, as a form or a fetch sends it."
  (let ((out (make-string-output-stream)))
    (loop with i = 0 and n = (length text)
          while (< i n)
          for char = (char text i)
          do (cond ((and (char= char #\%) (<= (+ i 3) n))
                    (let ((code (ignore-errors (parse-integer text :start (1+ i) :end (+ i 3)
                                                              :radix 16))))
                      (if code
                          (progn (write-char (code-char code) out) (incf i 3))
                          (progn (write-char char out) (incf i)))))
                   ((char= char #\+) (write-char #\Space out) (incf i))
                   (t (write-char char out) (incf i))))
    (get-output-stream-string out)))

(defun query-param (env name)
  "NAME's value in ENV's query string, or NIL."
  (let ((query (getf env :query-string)))
    (when query
      (loop for pair in (uiop:split-string query :separator "&")
            for equals = (position #\= pair)
            when (and equals (equal name (subseq pair 0 equals)))
              return (percent-decode (subseq pair (1+ equals)))))))

(defun html-escape (text)
  "TEXT as HTML body text: the five characters that end a tag or a quote."
  (with-output-to-string (out)
    (loop for char across text
          do (case char
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&#39;" out))
               (t (write-char char out))))))

(defun page (invite)
  "The guest's page: a title, the log, a box, and the poll that fills them."
  (format nil "<!doctype html>
<html lang=en><head><meta charset=utf-8><meta name=viewport content='width=device-width,initial-scale=1'>
<title>~a</title>
<style>body{font:15px/1.5 ui-sans-serif,system-ui,sans-serif;margin:2rem auto;max-width:46rem;padding:0 1rem;color:#111}h1{font-size:1.2rem;margin:0 0 .2rem}p.meta{color:#666;margin:0 0 1rem;font-size:.85rem}ol{padding-left:1.1rem}li{margin:.15rem 0}li time{color:#999;font-variant-numeric:tabular-nums;margin-right:.5rem}form{display:flex;gap:.5rem;margin-top:1.5rem}input{flex:1;padding:.5rem;font:inherit}button{padding:.5rem .9rem;font:inherit}p.state{color:#666;font-size:.85rem;min-height:1.2rem}</style>
</head><body>
<h1>~a</h1>
<p class=meta>a live, masked view of one session &middot; updates every two seconds</p>
<ol id=log></ol>
<p class=state id=state></p>
<form id=f><input id=t maxlength=2000 placeholder='a suggestion for the operator'><button>send</button></form>
<script>
var base='/spectator/~a', cursor=~d;
function add(event){var row=document.createElement('li'),time=document.createElement('time'),text=document.createTextNode(event.line);time.textContent=event.at?String(event.at).slice(11,19)+'Z':'';row.appendChild(time);row.appendChild(text);document.getElementById('log').appendChild(row);cursor=event.seq;}
async function tick(){try{var response=await fetch(base+'/events?since='+cursor,{cache:'no-store'});if(!response.ok){document.getElementById('state').textContent='this invite is closed or expired';return;}var data=await response.json();(data.events||[]).forEach(add);var log=document.getElementById('log');while(log.children.length>300){log.removeChild(log.firstChild);}document.getElementById('state').textContent=(data.suggestions||0)+' suggestion(s) waiting with the operator';}catch(error){document.getElementById('state').textContent='reconnecting...';}}
document.getElementById('f').addEventListener('submit',async function(event){event.preventDefault();var box=document.getElementById('t'),text=box.value.trim();if(!text){return;}box.value='';var response=await fetch(base+'/suggest?text='+encodeURIComponent(text),{method:'POST'});var data=await response.json();document.getElementById('state').textContent=data.answered||'sent';});
setInterval(tick,2000);tick();
</script>
</body></html>"
          (html-escape (setting :title)) (html-escape (setting :title))
          (getf invite :token) (getf invite :since)))

(defun html-response (body)
  "BODY as a page, never cached, never framed, nothing loaded from elsewhere."
  (list 200 (list :content-type "text/html; charset=utf-8"
                  :cache-control "no-store"
                  :referrer-policy "no-referrer"
                  :x-content-type-options "nosniff")
        (list body)))

(defun refusal-response (text)
  "TEXT as a JSON body a script reads, with 404's status."
  (list 404 (list :content-type "application/json; charset=utf-8" :cache-control "no-store")
        (list (nlk:encode-json-object (nlk:json-object "error" text)))))

(defun events-json (invite env)
  "What the page polls: the facts past SINCE, and how many suggestions wait."
  (let* ((since (ignore-errors (parse-integer (or (query-param env "since") "") :junk-allowed t)))
         (lines (feed invite :since (or since (getf invite :since))))
         (waiting (remove-if-not (lambda (one) (equal "waiting" (getf one :status)))
                                  (pending (getf invite :session)))))
    (nlk:json-object
     "events" (coerce (loop for line in lines
                             collect (nlk:json-object "seq" (getf line :seq)
                                                       "at" (getf line :at)
                                                       "line" (getf line :line)))
                       'vector)
     "cursor" (or (getf (car (last lines)) :seq) (or since (getf invite :since)))
     "suggestions" (length waiting))))

(defun suggest-json (invite env)
  "Take the page's suggestion and answer what became of it."
  (let ((one (suggest (getf invite :token) (query-param env "text"))))
    (nlk:json-object "id" (getf one :id)
                     "answered" (format nil "suggestion ~a is with the operator" (getf one :id)))))

(defun guest-route (env)
  "GET /spectator/TOKEN/ is the page, GET .../events its facts, POST
.../suggest one suggestion. Anything else under the token is a 404."
  (let* ((path (or (getf env :path-info) ""))
         (rest (if (uiop:string-prefix-p "/spectator/" path)
                   (subseq path (length "/spectator/"))
                   ""))
         (segments (uiop:split-string rest :separator "/"))
         (token (or (first segments) ""))
         (op (or (second segments) ""))
         (method (getf env :request-method))
         (invite (live-invite token)))
    (cond ((null invite) (refusal-response "spectator: this invite is closed or expired"))
          ((or (string= op "") (equal method :head)) (html-response (page invite)))
          ((string= op "events") (events-json invite env))
          ((and (string= op "suggest") (equal method :post))
           (handler-case (suggest-json invite env)
             (spectator-error (condition)
               (refusal-response (spectator-error-detail condition)))))
          (t (refusal-response "spectator: nothing here")))))

;;; --- the tool, the command, the entry ------------------------------------------

(defparameter *share-schema*
  (list :description
        "Open a live, masked page of this session for a guest, and answer the address to send them. The page shows the turn's start and end, each tool call by name and each assistant message's text, with absolute paths, NAME=value assignments and long opaque tokens masked. Nothing is stored beyond this process, and an approved suggestion from the page is put into this session as a prompt beginning [spectator]."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "minutes" (nlk:json-object
                                  "type" "integer"
                                  "description" "How long the invite stays open, in minutes (default: the section's invite_minutes)."))
         "required" (coerce '() 'vector)))
  "The tool the model calls to share this session.")

(defun session-now ()
  "The session this call runs in: the turn's, else the one the eval names."
  (or (getf (ignore-errors (nle:turn)) :session-id)
      (ignore-errors nlk:*scribe-session-id*)
      (fail "spectator: this call belongs to no session, so there is nothing to share")))

(defun share-tool (arguments)
  "SPECTATOR_SHARE: the model's way to hand a guest a link."
  (share (session-now) :minutes (nlk:json-value arguments :integer "minutes")))

(defun run-slash (args session-id)
  "/spectator [share [MINUTES] | suggestions | approve ID | reject ID | close | status]."
  (let* ((words (uiop:split-string (nlk:trimmed (or args "")) :separator " "))
         (head (string-downcase (or (first words) ""))))
    (values
     (cond ((member head '("" "status") :test #'string=) (status-text session-id))
           ((string= head "share")
            (share session-id
                   :minutes (ignore-errors (parse-integer (or (second words) "") :junk-allowed t))))
           ((string= head "suggestions") (suggestions-text session-id))
           ((string= head "approve")
            (if (second words)
                (approve (second words))
                (fail "spectator: /spectator approve ID")))
           ((string= head "reject")
            (if (second words)
                (reject (second words))
                (fail "spectator: /spectator reject ID")))
           ((string= head "close") (revoke))
           (t (fail "spectator: /spectator share | suggestions | approve ID | reject ID | close"))))))

(defun install ()
  "Clear what an earlier start left, and take the same clearing on stop."
  (reset)
  (nle:on-stop #'reset)
  (when (zerop (length (nlk:trimmed (or (setting :public-url) ""))))
    (nle:notice "spectator: set spectator.public_url to the address that reaches this gateway's port; /spectator share refuses until it is"
                :level :info :key "nodecode-spectator")))

(defun reset ()
  "Forget every invite and every suggestion."
  (with-spectator-lock (setf *invites* '() *suggestions* '())))

(nle:define-cell spectator
  (:section ("spectator")
    (:guide "public_url is the address guests open, which must reach this gateway's port (put a tunnel in front of it); invite_minutes bounds one invite; mask rewrites paths and NAME=value in what the page shows")
    ("public_url" :string :required t
     :doc "the address guests open, e.g. https://share.example.org -- a tunnel in front of the gateway's port; /spectator share refuses without it")
    ("title" :string :default "Spectator" :doc "the name the page wears")
    ("invite_minutes" :integer :default 120 :min 1
     :doc "how long one invite stays joinable")
    ("max_events" :integer :default 200 :min 10
     :doc "how many recent facts one poll of the page carries")
    ("mask" :boolean :default t
     :doc "rewrite absolute paths, NAME=value assignments and long opaque tokens"))
  (:tool "spectator_share" *share-schema* 'share-tool)
  (:command "spectator" 'run-slash
            :description "A live, masked page of this session for invited guests"
            :argument-hint "share | suggestions | approve ID | reject ID | close")
  (:help :spectator "spectator: a live, masked page of this session for the people you invite" +manual+)
  (:route "/spectator/" #'guest-route :public t)
  (:start #'install))
