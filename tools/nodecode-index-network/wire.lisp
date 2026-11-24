;;;; wire.lisp --- the Index REST API, one function per operation.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The same endpoints the original Hermes plugin calls, under the same
;;;; paths, so the cell reads and writes the same account:
;;;;
;;;;   POST  {api}/intents/list                {limit, page, q, archived}
;;;;   GET   {api}/networks/{id}/intents       ?limit=&page=
;;;;   POST  {api}/intents                      {description, networkIds}
;;;;   PATCH {api}/intents/{id}                 {description}
;;;;   GET   {api}/intents/{id}/networks
;;;;   POST  {api}/intents/{id}/networks        {networkId}
;;;;   GET   {api}/networks
;;;;   GET   {api}/networks/{id}/members
;;;;   POST  {api}/networks/{id}/join           {}
;;;;   GET   {api}/opportunities                ?status=&limit=
;;;;   PATCH {api}/opportunities/{id}/status    {status}
;;;;   GET   {api}/docs                         ?topic=
;;;;
;;;; One credential: an Index API key, pasted in the section or exported as
;;;; INDEX_API_KEY. The original also rides this device's browser session
;;;; (INDEX_SESSION_TOKEN) and mints a key from it; a key is what the
;;;; negotiator sidecar runs on anyway, and Index hands one out under
;;;; Settings, then advanced. A 401 is not retried: a key does not expire the
;;;; way a session does.
;;;;
;;;; *HTTP* is the one seam a test stubs. Every row the API answers with is
;;;; read leniently (NLK:JSON-VALUE), so a field the API renames costs a
;;;; column, never the answer.

(in-package #:nodecode-index-network)

(defparameter *http* #'nlk:http
  "The exchange this cell dials through: NLE's HTTP, stubbed by a test.")

(defparameter +default-api-url+ "https://protocol.index.network"
  "The Index protocol endpoint the original plugin defaults to.")

(defparameter +default-app-url+ "https://index.network"
  "Where a deep link to a signal, an opportunity or a person points.")

(defparameter +docs-topics+
  '("identity-context" "signals" "communities-networks" "opportunities"
    "negotiations" "workflows")
  "The topics GET /docs answers, as the original plugin lists them.")

(defparameter *row-chars* 160
  "How much of a signal's text one line carries before it is ellipsized.")

(defparameter *line-format* "~d. ~a~@[ (~a)~]~%~@[   id ~a~]"
  "One signal as two lines: its number and text, then its id.")

(defvar *last-error* nil
  "Why the last call that failed failed, for /index; NIL once one answers.")

;;; --- who and where ------------------------------------------------------------

(defun secret (name)
  "The environment variable NAME's value, trimmed, or NIL."
  (let ((value (uiop:getenv name)))
    (and value (plusp (length (nlk:trimmed value))) (nlk:trimmed value))))

(defun api-key ()
  "The key the section carries, else $INDEX_API_KEY."
  (or (let ((key (nlk:trimmed (or (setting :api-key) ""))))
        (and (plusp (length key)) key))
      (secret "INDEX_API_KEY")))

(defun api-url ()
  "The API endpoint, without a trailing slash."
  (string-right-trim "/" (or (setting :api-url) +default-api-url+)))

(defun app-url ()
  "Where deep links point, without a trailing slash."
  (string-right-trim "/" (or (setting :app-url) +default-app-url+)))

(defun configured-p ()
  "Whether there is a key to ask with at all."
  (and (api-key) t))

(defun unaware ()
  "Refuse: nothing can be asked without a key."
  (unless (configured-p)
    (fail "no Index API key: set index-network.api_key in /settings, or export ~
INDEX_API_KEY (Index makes one under Settings, then advanced)"))
  t)

(defun headers ()
  "The two headers every call carries."
  `(("authorization" . ,(format nil "Bearer ~a" (api-key)))
    ("content-type" . "application/json")
    ("accept" . "application/json")))

(defun endpoint (path &optional params)
  "PATH on the configured API, PARAMS an alist of query arguments."
  (format nil "~a~a~@[?~a~]" (api-url) path
          (and params (quri:url-encode-params params))))

(defun default-limit ()
  "How many rows a read answers when its call names none."
  (or (setting :limit) 20))

(defun segment (id)
  "ID as one path segment."
  (quri:url-encode (princ-to-string id)))

;;; --- one exchange ---------------------------------------------------------------

(defun sentence (body)
  "What a refused response said, one line, at most 300 characters, or NIL."
  (let ((text (typecase body
                (string body)
                (t (ignore-errors (nlk:encode-json-object body))))))
    (when (and text (plusp (length text)))
      (let ((flat (substitute #\Space #\Newline text)))
        (subseq flat 0 (min 300 (length flat)))))))

(defun refused (what status body)
  "Refuse: WHAT was answered STATUS, saying BODY where it said anything."
  (fail "~a answered HTTP ~d~@[: ~a~]" what status (sentence body)))

(defun decode-body (body)
  "BODY, decoded; an empty body reads as an empty object, and a body that
will not decode is handed back as the string it was."
  (if (and body (stringp body) (plusp (length body)))
      (or (ignore-errors (nlk:decode-json body)) body)
      (nlk:json-object)))

(defun exchange (method path &key object params)
  "METHOD PATH with OBJECT as JSON and PARAMS as the query. => (values
DECODED STATUS). OBJECT stays off the wire when it is NIL, so a GET carries
no body."
  (unaware)
  (let* ((timeout (or (setting :timeout) 20))
         (args (append (list method (endpoint path params)
                              :headers (headers)
                              :timeout timeout
                              :connect-timeout (min 10 timeout))
                       (when object (list :content (nlk:encode-json-object object))))))
    (multiple-value-bind (body status)
        (apply *http* args)
      (values (decode-body body) status))))

(defun ok-p (status)
  "Whether STATUS is one the API means as an answer."
  (and (integerp status) (<= 200 status 299)))

;;; --- reading rows ---------------------------------------------------------------

(defun text-of (row &rest keys)
  "The first non-empty string ROW carries under KEYS, or NIL."
  (loop for key in keys
        for value = (nlk:json-value row :text key)
        when value return value))

(defun id-of (row)
  "The id ROW carries, whichever of its names the API used."
  (text-of row "id" "intentId" "opportunityId" "networkId" "userId"))

(defun fit (text length)
  "TEXT on one line, at most LENGTH characters."
  (let* ((flat (substitute #\Space #\Newline (princ-to-string (or text ""))))
         (flat (substitute #\Space #\Tab flat))
         (clean (remove-if (lambda (c) (< (char-code c) 32)) flat)))
    (cond ((zerop (length clean)) "")
          ((<= (length clean) length) clean)
          (t (concatenate 'string (subseq clean 0 (max 1 (- length 3))) "...")))))

(defun link (kind id)
  "The web address of KIND (i, o or u) with ID, when there is one."
  (and id (format nil "~a/~a/~a" (app-url) kind id)))

(defun rows (value key)
  "The array VALUE carries under KEY, as a list; NIL when it carries none."
  (coerce (or (nlk:json-value value :array key) #()) 'list))

(defun list-text (lines empty)
  "LINES as one answer, or EMPTY when there are none."
  (if lines
      (with-output-to-string (out)
        (dolist (line lines) (format out "~a~%" line)))
      empty))

;;; --- signals -------------------------------------------------------------------

(defun signal-lines (rows &key (start 1))
  "ROWS as numbered lines: the signal, its status, its id."
  (loop for row in rows
        for n from start
        for id = (id-of row)
        for statement = (or (text-of row "description" "payload" "summary" "statement")
                            "(no description)")
        collect (format nil *line-format* n (fit statement *row-chars*)
                        (text-of row "status") id)))

(defun read-signals (&key network-id query archived limit page)
  "The caller's signals, or one community's, as lines."
  (multiple-value-bind (value status)
      (if (and network-id (plusp (length network-id)))
          (exchange "GET" (format nil "/networks/~a/intents" (segment network-id))
                    :params `(("limit" . ,(princ-to-string (or limit (default-limit))))
                              ,@(when page (list (cons "page" (princ-to-string page))))))
          (let ((body (nlk:json-object
                       "limit" (or limit (default-limit))
                       "page" (or page 1)
                       "archived" (if archived t :false))))
            (when (and query (plusp (length query)))
              (setf (gethash "q" body) query))
            (exchange "POST" "/intents/list" :object body)))
    (cond ((not (ok-p status)) (refused "listing signals" status value))
          (t (list-text (signal-lines (rows value "intents"))
                        "No signals here yet.")))))

(defun create-signal (description &optional network-ids)
  "Create one signal from DESCRIPTION, shared in NETWORK-IDS when given."
  (let ((body (nlk:json-object "description" description)))
    (when (and network-ids (plusp (length network-ids)))
      (setf (gethash "networkIds" body) (coerce network-ids 'vector)))
    (multiple-value-bind (value status) (exchange "POST" "/intents" :object body)
      (cond ((not (ok-p status)) (refused "creating the signal" status value))
            (t (let* ((intent (or (nlk:json-value value :object "intent") value))
                      (id (id-of intent)))
                 (format nil "Signal created~@[: ~a~]~@[ (~a)~]~@[~%  ~a~]"
                         id (text-of intent "status") (link "i" id))))))))

(defun update-signal (id description)
  "Rewrite the signal ID's text."
  (multiple-value-bind (value status)
      (exchange "PATCH" (format nil "/intents/~a" (segment id))
                :object (nlk:json-object "description" description))
    (cond ((not (ok-p status)) (refused (format nil "rewriting signal ~a" id) status value))
          (t (format nil "Signal ~a rewritten.~@[~%  ~a~]" id (link "i" id))))))

(defun share-in-network (intent-id network-id)
  "Share the signal INTENT-ID in the community NETWORK-ID."
  (multiple-value-bind (value status)
      (exchange "POST" (format nil "/intents/~a/networks" (segment intent-id))
                :object (nlk:json-object "networkId" network-id))
    (cond ((not (ok-p status))
           (refused (format nil "sharing signal ~a" intent-id) status value))
          (t (format nil "Signal ~a is shared in community ~a." intent-id network-id)))))

(defun signal-networks (intent-id)
  "The communities the signal INTENT-ID is shared in."
  (multiple-value-bind (value status)
      (exchange "GET" (format nil "/intents/~a/networks" (segment intent-id)))
    (cond ((not (ok-p status)) (refused (format nil "reading signal ~a's communities" intent-id) status value))
          (t (let ((ids (append (nlk:json-value value :array "networkIds")
                                 (mapcar (lambda (row) (id-of row))
                                         (rows value "networks")))))
               (list-text (loop for id in ids when id collect (format nil "- ~a" id))
                          (format nil "Signal ~a is in no community." intent-id)))))))

;;; --- opportunities -------------------------------------------------------------

(defun opportunity-lines (rows)
  "ROWS as numbered lines: who the match is, its status, its id."
  (loop for row in rows
        for n from 1
        for id = (id-of row)
        for who = (or (text-of row "counterpartName" "name")
                      (text-of (nlk:json-value row :object "counterpart") "name" "title"))
        for headline = (or (text-of row "headline" "mainText" "summary") "")
        collect (format nil "~d. ~a~@[ (~a)~]~@[ -- ~a~]~%~@[   id ~a~]"
                        n (or who "An introduction") (text-of row "status")
                        (and (plusp (length headline)) (fit headline *row-chars*)) id)))

(defun read-opportunities (&key status limit)
  "The introductions negotiated for the caller, as lines."
  (multiple-value-bind (value code)
      (exchange "GET" "/opportunities"
                :params `(("limit" . ,(princ-to-string (or limit (default-limit))))
                          ,@(when (and status (plusp (length status)))
                              (list (cons "status" status)))))
    (cond ((not (ok-p code)) (refused "listing opportunities" code value))
          (t (list-text (opportunity-lines (rows value "opportunities"))
                        "No opportunities yet: nothing has been negotiated for you.")))))

(defun set-opportunity-status (id status)
  "Mark the opportunity ID accepted or rejected."
  (multiple-value-bind (value code)
      (exchange "PATCH" (format nil "/opportunities/~a/status" (segment id))
                :object (nlk:json-object "status" status))
    (cond ((not (ok-p code))
           (refused (format nil "marking opportunity ~a ~a" id status) code value))
          (t (format nil "Opportunity ~a is ~a.~@[~%  ~a~]" id status (link "o" id))))))

;;; --- communities ---------------------------------------------------------------

(defun member-name (row)
  "Who ROW names, or the id it carries."
  (or (text-of row "name" "title" "displayName") (id-of row) "(unnamed)"))

(defun member-count (row)
  "How many members ROW says the community has, or NIL."
  (or (nlk:json-value row :integer "memberCount")
      (nlk:json-value row :integer "members")
      (nlk:json-value (nlk:json-value row :object "_count") :integer "members")))

(defun network-lines (rows)
  "ROWS as numbered lines: the community, its size, its id."
  (loop for row in rows
        for n from 1
        for id = (id-of row)
        for title = (or (text-of row "title" "name") "Untitled community")
        for count = (member-count row)
        collect (format nil "~d. ~a~@[ (~d member~:p)~]~%~@[   id ~a~]"
                        n (fit title *row-chars*) count id)))

(defun network-members (network-id)
  "One community's roster."
  (multiple-value-bind (value code)
      (exchange "GET" (format nil "/networks/~a/members" (segment network-id)))
    (cond ((not (ok-p code))
           (refused (format nil "reading community ~a members" network-id) code value))
          (t (let ((rows (or (rows value "members") (rows value "data"))))
               (list-text (loop for row in rows
                                for n from 1
                                collect (format nil "~d. ~a~@[   id ~a~]"
                                                n (fit (member-name row) *row-chars*) (id-of row)))
                          (format nil "Community ~a answers no public roster." network-id)))))))

(defun read-networks (&optional network-id)
  "The communities the caller belongs to; NETWORK-ID asks for one roster."
  (if (and network-id (plusp (length network-id)))
      (network-members network-id)
      (multiple-value-bind (value code) (exchange "GET" "/networks")
        (cond ((not (ok-p code)) (refused "listing communities" code value))
              (t (list-text (network-lines (rows value "networks"))
                            "No communities yet."))))))

(defun join-network (network-id)
  "Join the open community NETWORK-ID."
  (multiple-value-bind (value code)
      (exchange "POST" (format nil "/networks/~a/join" (segment network-id))
                :object (nlk:json-object))
    (cond ((not (ok-p code))
           (refused (format nil "joining community ~a" network-id) code value))
          (t (format nil "Joined community ~a." network-id)))))

;;; --- the protocol's own guidance ------------------------------------------------

(defun docs-listing (value)
  "VALUE's members as a list of topics, or NIL when it is not an object."
  (when (and (hash-table-p value) (plusp (hash-table-count value)))
    (with-output-to-string (out)
      (format out "The topics Index documents:~%")
      (maphash (lambda (key text)
                 (format out "- ~a~@[: ~a~]~%" key
                         (and (stringp text) (fit text 100))))
               value))))

(defun docs-text (value)
  "The guidance VALUE carries, whichever shape the API answered with."
  (if (stringp value)
      (fit value 20000)
      (or (text-of value "content" "markdown" "text" "doc" "body")
          (docs-listing (nlk:json-value value :object "data"))
          (docs-listing value)
          (fit (nlk:encode-json-object value) 2000))))

(defun fetch-docs (&optional topic)
  "The protocol's canonical guidance, all of it or one TOPIC's."
  (multiple-value-bind (value code)
      (exchange "GET" "/docs" :params (when (and topic (plusp (length topic)))
                                         (list (cons "topic" topic))))
    (cond ((not (ok-p code))
           (refused (format nil "reading the Index docs~@[ on ~a~]" topic) code value))
          (t (docs-text value)))))
