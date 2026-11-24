;;;; cell.lisp --- Freshdesk's eleven tools, over the account's own v2 API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :freshdesk) answers the primer below, and every request's help
;;;; section carries one line naming it. Nothing hooks the provider path: an
;;;; account speaks HTTP, and NLK:HTTP is the one seam.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "freshdesk": {"domain": "acme", "api_key_env": "FRESHDESK_API_KEY"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from aedelgod's Hermes Agent plugin (see NOTICE): the eleven
;;;; tools and their schemas, the bounded pagination, the account-origin and
;;;; attachment-origin checks, the plain-text writes and their readback, and
;;;; the write-may-have-succeeded state. Hermes' human-approval hook has no
;;;; counterpart here and is left out.

(in-package #:nodecode-freshdesk)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Freshdesk is available through the nodecode-freshdesk cell: eleven tools, which a person
can also call from eval in the freshdesk package.
  freshdesk_list_tickets  {updated_since, include_description}
      Tickets, 100 a page, bounded by the operator's max_pages. Without updated_since
      Freshdesk answers only the tickets created in the last 30 days. The description is
      left out unless include_description is true. A list cut by the page cap says
      truncated: narrow it with a date rather than assuming it is all of them.
  freshdesk_get_ticket  {ticket_id}
      One ticket by id: subject, description, status, priority, requester, tags and
      attachments. Status runs 2 open, 3 pending, 4 resolved, 5 closed; priority 1 low
      through 4 urgent.
  freshdesk_search_tickets  {status, priority, tag, type, agent_id, group_id,
                              created_after, created_before, updated_after, updated_before}
      Read-only search with at least one filter. Dates are YYYY-MM-DD. Subject,
      description and free-text keyword search are not supported here.
  freshdesk_list_conversations  {ticket_id}
      Every reply, note and forward of a ticket, oldest first, with its body and whether
      it is private. A list cut by the page cap is an error, not a short list.
  freshdesk_get_contact  {contact_id}
      One contact by id: name, email, phone, company.
  freshdesk_download_attachment  {ticket_id, attachment_id}
      Writes one attachment of a ticket (or of one of its conversations) under the
      operator's download directory and answers its path and size. The URL always comes
      from the ticket itself, and only Freshdesk and its CDN/S3 origins are accepted.
  freshdesk_add_private_note  {ticket_id, body}
      A private note, visible to agents only. The text is plain: it is HTML-escaped and
      newlines become line breaks, and it is read back before success is reported.
  freshdesk_create_ticket  {fields}
      A new ticket from subject, description, priority, status and exactly one of email or
      requester_id. Read back and compared field by field before success.
  freshdesk_reply_to_ticket  {ticket_id, body}
      A public reply to the requester - this emails them - read back before success.
  freshdesk_add_time_entry  {ticket_id, time_spent, note, billable, agent_id}
      A finished (non-timer) time entry, H:MM through HHHH:MM, read back and compared.
  freshdesk_update_ticket  {ticket_id, status, priority}
      Change status (2-5) and/or priority (1-4), read back and compared.
Every answer is a JSON object: reads answer {success, data}, and a write that Freshdesk
may have accepted but could not be verified answers state write_may_have_succeeded with the
created_id when known and retry_safe false - do not retry such a write blindly. A rejected
write answers the words Freshdesk gave."
  "What (help :freshdesk) answers while the cell runs.")

;;; --- what the wire constrains ------------------------------------------------

(defparameter +account-pattern+ "\\A[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?\\z"
  "A Freshdesk account name, as it stands before .freshdesk.com.")

(defparameter +attachment-host-pattern+ "\\A[a-z0-9-]+-attachment\\.freshdesk\\.com\\z"
  "The sharded attachment CDN hosts Freshdesk hands URLs from.")

(defparameter +cdn-s3-host-pattern+ "\\Acdn\\.freshdesk\\.com\\.s3(\\.[a-z0-9-]+)?\\.amazonaws\\.com\\z"
  "The S3 bucket Freshdesk's CDN lives in, and only that one.")

(defparameter +tag-pattern+ "\\A[A-Za-z0-9 _-]{1,64}\\z"
  "A tag or a ticket type: letters, digits, spaces, underscore and hyphen.")

(defparameter +date-pattern+ "\\A[0-9]{4}-[0-9]{2}-[0-9]{2}\\z"
  "A date, as the search takes it.")

(defparameter +time-spent-pattern+ "\\A([0-9]{1,4}):([0-5][0-9])\\z"
  "A duration from H:MM through HHHH:MM, minutes under 60.")

(defparameter +certain-failure-status+ '(400 401 403 404 409 422)
  "The statuses that mean Freshdesk answered and rejected the write: no ticket,
note or entry was created. Anything else - 408, 429, a 5xx, a transport
failure - leaves the write ambiguous, and the tool says so.")

(defparameter +per-page+ 100 "Freshdesk's largest page.")
(defparameter +max-search-pages+ 10 "How many search pages one call may read.")
(defparameter +max-response-bytes+ (* 4 1024 1024) "One JSON answer's ceiling.")
(defparameter +max-attachment-bytes+ (* 25 1024 1024) "One attachment's ceiling.")
(defparameter +detail-chars+ 500 "How much of Freshdesk's own error body a refusal quotes.")

;;; --- answers, and the two conditions -------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error, and an
UNCERTAIN-WRITE becomes the state object that says not to retry blindly."
  `(handler-case (progn ,@body)
     (uncertain-write (condition)
       (nlk:encode-json-object
        (nlk:json-object "success" nil
                         "state" "write_may_have_succeeded"
                         "created_id" (uncertain-write-created-id condition)
                         "error" (princ-to-string condition)
                         "retry_safe" nil)))
     (freshdesk-error (condition)
       (nle:failure (format nil "ERROR: ~a" (freshdesk-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Freshdesk: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun answer (data)
  "DATA as the answer shape the plugin's tools had: {success, data}."
  (nlk:encode-json-object (nlk:json-object "success" t "data" data)))

(defun detail (body)
  "Freshdesk's own words for a failed call, clipped, or NIL."
  (let ((text (nlk:trimmed (if (stringp body) body ""))))
    (and (plusp (length text)) (nlk:clip text +detail-chars+))))

(defun positive (value label)
  "VALUE as a positive integer, or a refusal naming LABEL."
  (unless (and (integerp value) (plusp value))
    (fail "~a must be a positive integer, not ~s" label value))
  value)

(defun canonical-time-spent (text)
  "TEXT as the HH:MM form Freshdesk stores, or a refusal."
  (let ((groups (and (stringp text)
                     (nth-value 1 (ppcre:scan-to-strings +time-spent-pattern+ text)))))
    (unless groups
      (fail "time_spent must be H:MM through HHHH:MM, minutes under 60, not ~s" text))
    (let ((hours (parse-integer (aref groups 0)))
          (minutes (parse-integer (aref groups 1))))
      (when (and (zerop hours) (zerop minutes))
        (fail "time_spent must not be zero"))
      (if (< hours 100)
          (format nil "~2,'0d:~2,'0d" hours minutes)
          (format nil "~d:~2,'0d" hours minutes)))))

;;; --- reading the section --------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun account-origin (domain)
  "DOMAIN - an account name, or the account's own HTTPS origin - as the
origin every request goes to. A custom support CNAME is refused: the API key
must only travel to a native Freshdesk origin."
  (let* ((raw (string-downcase (nonblank domain)))
         (uri (and raw (quri:uri raw))))
    (when (ppcre:scan +account-pattern+ (or raw ""))
      (return-from account-origin (format nil "https://~a.freshdesk.com" raw)))
    (let* ((scheme (and uri (string-downcase (or (quri:uri-scheme uri) ""))))
           (host (string-downcase (or (and uri (quri:uri-host uri)) "")))
           (account (and (uiop:string-suffix-p host ".freshdesk.com")
                         (subseq host 0 (- (length host) (length ".freshdesk.com"))))))
      (unless (and (string= scheme "https")
                   account
                   (ppcre:scan +account-pattern+ account)
                   (null (quri:uri-userinfo uri))
                   (member (quri:uri-port uri) '(nil 443) :test #'equal)
                   (member (or (quri:uri-path uri) "") '("" "/") :test #'string=)
                   (null (nonblank (quri:uri-query uri)))
                   (null (nonblank (quri:uri-fragment uri))))
        (nlk:config-error
         "domain ~s is neither an account name nor a native HTTPS Freshdesk origin: set it to the part before .freshdesk.com, or to https://<account>.freshdesk.com" domain))
      (format nil "https://~a" host))))

(defun read-settings (values table)
  "The declared VALUES, plus the origin and the API key the section names:
read once, here."
  (declare (ignore table))
  (let ((domain (nonblank (getf values :domain)))
        (env (nonblank (getf values :api-key-env)))
        (file (nonblank (getf values :api-key-file)))
        (plist (copy-list values)))
    (unless domain
      (nlk:config-error "no domain: set it to your Freshdesk account name, the part before .freshdesk.com"))
    (setf (getf plist :origin) (account-origin domain)
          (getf plist :api-key)
          (let ((key (cond (env (or (nonblank (nle::credential-env env))
                                    (nlk:config-error "api_key_env names ~a, which is unset or empty" env)))
                           (file (or (nonblank (ignore-errors
                                                (uiop:read-file-string (nlk:expand-home file))))
                                     (nlk:config-error "api_key_file ~a cannot be read, or is empty" file)))
                           (t (nlk:config-error
                               "no API key: set api_key_env to the environment variable holding one, or api_key_file to a file holding one (Freshdesk: Profile, API Keys)")))))
            (when (or (find #\Newline key) (find #\Return key))
              (nlk:config-error "the API key carries a newline, which no header can hold"))
            key))
    plist))

;;; --- one exchange ---------------------------------------------------------------

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The base64 alphabet, for the one Basic-auth header.")

(defun base64 (text)
  "TEXT as base64, one octet per character: an API key is ASCII."
  (let ((n (length text)))
    (with-output-to-string (out)
      (loop for i from 0 below n by 3
            for a = (char-code (char text i))
            for b = (if (< (+ i 1) n) (char-code (char text (+ i 1))) 0)
            for c = (if (< (+ i 2) n) (char-code (char text (+ i 2))) 0)
            for triple = (logior (ash a 16) (ash b 8) c)
            do (flet ((sextet (shift)
                        (char +base64-alphabet+ (ldb (byte 6 shift) triple))))
                 (write-char (sextet 18) out)
                 (write-char (sextet 12) out)
                 (write-char (if (< (+ i 1) n) (sextet 6) #\=) out)
                 (write-char (if (< (+ i 2) n) (sextet 0) #\=) out))))))

(defun query-string (pairs)
  "PAIRS - (NAME . VALUE) - as a query string, values URL-encoded, absent and
empty ones left out."
  (format nil "~{~a~^&~}"
          (loop for (name . value) in pairs
                when (and value (not (and (stringp value) (zerop (length value)))))
                  collect (format nil "~a=~a" name
                                  (quri:url-encode (princ-to-string value) :encoding :utf-8)))))

(defun api-url (path &optional query)
  "PATH under the account's origin, with QUERY when it names anything."
  (unless (and (uiop:string-prefix-p "/api/v2/" path)
               (not (find #\? path))
               (not (find #\# path)))
    (fail "Freshdesk API path ~s is invalid" path))
  (let ((text (query-string query)))
    (if (plusp (length text))
        (format nil "~a~a?~a" (setting :origin) path text)
        (format nil "~a~a" (setting :origin) path))))

(defun request-headers (authenticated payload)
  (append (when authenticated
            (list (cons "authorization"
                        (format nil "Basic ~a" (base64 (format nil "~a:X" (setting :api-key)))))))
          (list (cons "accept" (if authenticated "application/json" "application/octet-stream")))
          (when payload (list (cons "content-type" "application/json")))
          (list (cons "user-agent" "Nodecode-Freshdesk/0.1"))))

(defun dial (method url &key payload (authenticated t) binary max-bytes)
  "=> (values BODY STATUS). BODY is the text Freshdesk answered (octets with
BINARY); STATUS its status code, or NIL when the dial itself failed, with the
reason as BODY."
  (handler-case
      (let ((timeout (setting :timeout)))
        (multiple-value-bind (body status)
            (nlk:http method url
                      :headers (request-headers authenticated payload)
                      :content (and payload (nlk:encode-json-object payload))
                      :binary binary
                      :max-bytes max-bytes
                      :timeout timeout
                      :connect-timeout (min 10 timeout))
          (values body status)))
    (error (condition)
      (values (format nil "~(~a~) ~a did not answer: ~a"
                      method url (nlk:one-line (princ-to-string condition)))
              nil))))

(defun reading (method path &key query payload)
  "PATH's decoded JSON answer, or a refusal carrying Freshdesk's own words."
  (let ((url (api-url path query)))
    (multiple-value-bind (body status) (dial method url :payload payload)
      (cond ((and (integerp status) (< status 400))
             (or (ignore-errors (nlk:decode-json body))
                 (and (plusp (length (or body ""))) body)))
            ((integerp status)
             (fail "~(~a~) ~a answered HTTP ~a~@[: ~a~]" method path status (detail body)))
            (t (fail "~(~a~) ~a did not answer~@[: ~a~]" method path (nlk:one-line (or body ""))))))))

(defun sending (method path payload operation)
  "Freshdesk's decoded answer to a write. A definite rejection refuses; an
ambiguous one signals UNCERTAIN-WRITE, which the caller answers as
write_may_have_succeeded."
  (multiple-value-bind (body status) (dial method (api-url path) :payload payload)
    (cond ((and (integerp status) (< status 400))
           (or (ignore-errors (nlk:decode-json body))
               (error 'uncertain-write :operation operation
                                       :reason "the answer was not JSON")))
          ((and (integerp status) (member status +certain-failure-status+))
           (fail "~a rejected by Freshdesk: HTTP ~a~@[: ~a~]" operation status (detail body)))
          ((integerp status)
           (error 'uncertain-write :operation operation
                                   :reason (format nil "Freshdesk answered HTTP ~a" status)))
          (t (error 'uncertain-write :operation operation :reason (nlk:one-line (or body "")))))))

(defun created-id (value operation)
  "VALUE's own id, which a create answer must carry, or UNCERTAIN-WRITE."
  (let ((id (and (hash-table-p value) (nlk:json-value value :integer "id"))))
    (unless (and (integerp id) (plusp id))
      (error 'uncertain-write :operation operation :reason "the answer carried no id"))
    id))

;;; --- pages ---------------------------------------------------------------------

(defun paged (path &key query)
  "PATH's rows over the account's pages => (values ROWS TRUNCATED-P)."
  (let ((rows '())
        (pages (setting :max-pages))
        (complete nil))
    (dotimes (index pages)
      (let* ((value (reading :get path
                              :query (append query
                                              (list (cons "page" (1+ index))
                                                    (cons "per_page" +per-page+)))))
             (items (or (nlk:json-array value)
                        (fail "Freshdesk answered ~a page ~a as something other than a list"
                              path (1+ index)))))
        (loop for item across items do (push item rows))
        (when (< (length items) +per-page+)
          (setf complete t)
          (return))))
    (values (nreverse rows) (not complete))))

(defun conversations (ticket-id)
  "TICKET-ID's whole conversation list, or a refusal when the page cap cut it."
  (multiple-value-bind (rows truncated)
      (paged (format nil "/api/v2/tickets/~a/conversations" ticket-id))
    (when truncated
      (fail "Freshdesk's pagination limit was reached: ticket ~a has more conversations than max_pages reads" ticket-id))
    rows))

(defun find-conversation (ticket-id conversation-id)
  "CONVERSATION-ID's own record on TICKET-ID, read back page by page."
  (let ((path (format nil "/api/v2/tickets/~a/conversations" ticket-id)))
    (dotimes (index (setting :max-pages))
      (let* ((items (or (nlk:json-array
                         (reading :get path :query (list (cons "page" (1+ index))
                                                         (cons "per_page" +per-page+))))
                        (fail "Freshdesk answered ~a page ~a as something other than a list"
                              path (1+ index)))))
        (loop for item across items
              do (when (= (or (nlk:json-value item :integer "id") 0) conversation-id)
                   (return-from find-conversation item)))
        (when (< (length items) +per-page+) (return))))
    (fail "conversation readback is missing")))

;;; --- plain text, and Freshdesk's HTML ---------------------------------------------

(defun html-escape (text)
  (with-output-to-string (out)
    (loop for character across text
          do (case character
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&#x27;" out))
               (#\Newline (write-string "<br>" out))
               (t (write-char character out))))))

(defun html-div (text)
  "TEXT as the HTML div Freshdesk stores: escaped, newlines as line breaks."
  (format nil "<div>~a</div>" (html-escape text)))

(defun html-unescape (text)
  (let ((text (ppcre:regex-replace-all "&(?:amp|#38);" text "&")))
    (setf text (ppcre:regex-replace-all "&(?:lt|#60);" text "<"))
    (setf text (ppcre:regex-replace-all "&(?:gt|#62);" text ">"))
    (setf text (ppcre:regex-replace-all "&(?:quot|#34);" text "\""))
    (ppcre:regex-replace-all "&(?:apos|#39|#x27);" text "'")))

(defun plain-from-html (text)
  "TEXT with line breaks kept and every tag dropped: what a person would read."
  (let ((text (ppcre:regex-replace-all "(?i)<br\\s*/?>" (or text "") (string #\Newline))))
    (setf text (ppcre:regex-replace-all "(?i)</div>" text (string #\Newline)))
    (setf text (ppcre:regex-replace-all "<[^>]+>" text ""))
    (nlk:trimmed (html-unescape text))))

(defun conversation-plain (item)
  "ITEM's text, as Freshdesk's own body_text or its HTML read plainly."
  (let ((text (and (hash-table-p item) (nlk:json-value item :string "body_text"))))
    (if (and text (plusp (length (nlk:trimmed text))))
        (nlk:trimmed text)
        (plain-from-html (nlk:json-value item :string "body")))))

;;; --- freshdesk_list_tickets --------------------------------------------------------

(defun list-tickets (call)
  "The freshdesk_list_tickets tool: the account's tickets, page-bounded."
  (answering
    (multiple-value-bind (rows truncated)
        (paged "/api/v2/tickets"
               :query (append (list (cons "updated_since" (ticket-list-updated-since call)))
                              (when (ticket-list-include-description call)
                                (list (cons "include" "description")))))
      (answer (nlk:json-object "tickets" (coerce rows 'vector)
                                "truncated" truncated)))))

;;; --- freshdesk_get_ticket ---------------------------------------------------------

(defun fetch-ticket (ticket-id &key include)
  "TICKET-ID's own ticket, or a refusal when the readback is not it."
  (let ((ticket (reading :get (format nil "/api/v2/tickets/~a" ticket-id)
                          :query (and include (list (cons "include" include))))))
    (unless (and (hash-table-p ticket)
                 (= (or (nlk:json-value ticket :integer "id") 0) ticket-id))
      (fail "Freshdesk ticket ~a readback is invalid" ticket-id))
    ticket))

(defun get-ticket (call)
  "The freshdesk_get_ticket tool: one ticket, whole."
  (answering
    (answer (fetch-ticket (positive (ticket-read-ticket-id call) "ticket_id")))))

;;; --- freshdesk_search_tickets -------------------------------------------------------

(defun real-date-p (text)
  "TEXT as YYYY-MM-DD, and a date the calendar has."
  (and (ppcre:scan +date-pattern+ text)
       (let ((year (parse-integer text :start 0 :end 4))
             (month (parse-integer text :start 5 :end 7))
             (day (parse-integer text :start 8 :end 10)))
         (and (ignore-errors (encode-universal-time 0 0 0 day month year 0)) t))))

(defun search-clauses (call)
  "CALL's filters as Freshdesk's query clauses, or a refusal."
  (let ((clauses '()))
    (labels ((add (text) (setf clauses (append clauses (list text))))
             (date (value label)
               (when value
                 (unless (real-date-p value)
                   (fail "~a must be a real date, YYYY-MM-DD, not ~s" label value))
                 value)))
      (let ((status (ticket-search-status call))
            (priority (ticket-search-priority call))
            (tag (nonblank (ticket-search-tag call)))
            (kind (nonblank (ticket-search-type-of-ticket call)))
            (agent (ticket-search-agent-id call))
            (group (ticket-search-group-id call)))
        (when status
          (unless (<= 2 status 5) (fail "status must be between 2 and 5, not ~a" status))
          (add (format nil "status:~a" status)))
        (when priority
          (unless (<= 1 priority 4) (fail "priority must be between 1 and 4, not ~a" priority))
          (add (format nil "priority:~a" priority)))
        (when tag
          (unless (ppcre:scan +tag-pattern+ tag)
            (fail "tag ~s is not up to 64 letters, digits, spaces, _ and -" tag))
          (add (format nil "tag:'~a'" tag)))
        (when kind
          (unless (ppcre:scan +tag-pattern+ kind)
            (fail "type ~s is not up to 64 letters, digits, spaces, _ and -" kind))
          (add (format nil "type:'~a'" kind)))
        (when agent
          (add (format nil "agent_id:~a" (positive agent "agent_id"))))
        (when group
          (add (format nil "group_id:~a" (positive group "group_id")))))
      (let ((after (date (nonblank (ticket-search-created-after call)) "created_after"))
            (before (date (nonblank (ticket-search-created-before call)) "created_before"))
            (seen (date (nonblank (ticket-search-updated-after call)) "updated_after"))
            (until (date (nonblank (ticket-search-updated-before call)) "updated_before")))
        (when (and after before (not (string< after before)))
          (fail "created_after (~a) must be before created_before (~a)" after before))
        (when (and seen until (not (string< seen until)))
          (fail "updated_after (~a) must be before updated_before (~a)" seen until))
        (when after (add (format nil "created_at:>'~a'" after)))
        (when before (add (format nil "created_at:<'~a'" before)))
        (when seen (add (format nil "updated_at:>'~a'" seen)))
        (when until (add (format nil "updated_at:<'~a'" until))))
      (unless clauses
        (fail "search requires at least one filter: status, priority, tag, type, agent_id, group_id or a date range"))
      (format nil "\"~a\"" (format nil "~{~a~^ AND ~}" clauses)))))

(defun search-results (query)
  "QUERY's results over Freshdesk's search pages => (values RESULTS TRUNCATED-P)."
  (let ((results '())
        (total nil)
        (complete nil))
    (dotimes (index +max-search-pages+)
      (let* ((value (reading :get "/api/v2/search/tickets"
                              :query (list (cons "query" query) (cons "page" (1+ index)))))
             (items (and (hash-table-p value) (nlk:json-array value "results"))))
        (unless items
          (fail "Freshdesk answered search page ~a as an invalid search response" (1+ index)))
        (loop for item across items do (push item results))
        (let ((raw (and (hash-table-p value) (nlk:json-value value :integer "total"))))
          (when raw (setf total raw)))
        (cond ((and total (>= (length results) total)) (setf complete t) (return))
              ((zerop (length items)) (setf complete t) (return)))))
    (values (nreverse results) (not complete))))

(defun search-tickets (call)
  "The freshdesk_search_tickets tool: a read-only filtered search."
  (answering
    (multiple-value-bind (results truncated) (search-results (search-clauses call))
      (answer (nlk:json-object "results" (coerce results 'vector)
                                "truncated" truncated)))))

;;; --- freshdesk_list_conversations ----------------------------------------------------

(defun list-conversations (call)
  "The freshdesk_list_conversations tool: a ticket's whole conversation list."
  (answering
    (let ((ticket-id (positive (conversation-list-ticket-id call) "ticket_id")))
      (answer (coerce (conversations ticket-id) 'vector)))))

;;; --- freshdesk_get_contact -----------------------------------------------------------

(defun get-contact (call)
  "The freshdesk_get_contact tool: one contact, whole."
  (answering
    (let* ((contact-id (positive (contact-read-contact-id call) "contact_id"))
           (contact (reading :get (format nil "/api/v2/contacts/~a" contact-id))))
      (unless (and (hash-table-p contact)
                   (= (or (nlk:json-value contact :integer "id") 0) contact-id))
        (fail "Freshdesk contact ~a readback is invalid" contact-id))
      (answer contact))))

;;; --- freshdesk_download_attachment ----------------------------------------------------

(defun unsafe-path-p (path)
  "Whether PATH carries a dot segment, encoded or doubled, or a backslash: the
shapes a transport could normalize across origins."
  (or (find #\\ path)
      (let ((decoded (string-downcase path)))
        (dotimes (round 2)
          (setf decoded (ppcre:regex-replace-all "%2e" decoded ".")))
        (some (lambda (segment) (member segment '("." "..") :test #'string=))
              (ppcre:split "/" decoded)))))

(defun attachment-tier (url)
  "URL's trust tier: :account (the API key travels), :cdn or :s3 (it never
does). A URL outside Freshdesk's own origins is a refusal."
  (let* ((uri (ignore-errors (quri:uri url)))
         (scheme (and uri (string-downcase (or (quri:uri-scheme uri) ""))))
         (host (and uri (string-downcase (or (quri:uri-host uri) ""))))
         (path (or (and uri (quri:uri-path uri)) ""))
         (account (string-downcase (or (quri:uri-host (quri:uri (setting :origin))) ""))))
    (unless (and (string= scheme "https")
                 (null (quri:uri-userinfo uri))
                 (member (quri:uri-port uri) '(nil 443) :test #'equal)
                 (null (nonblank (quri:uri-fragment uri)))
                 (not (unsafe-path-p path)))
      (fail "attachment URL ~s is outside the permitted Freshdesk origins" url))
    (cond ((and (string= host account) (uiop:string-prefix-p "/helpdesk/attachments/" path))
           :account)
          ((or (string= host "attachment.freshdesk.com")
               (ppcre:scan +attachment-host-pattern+ host))
           :cdn)
          ((and (string= host "s3.amazonaws.com") (uiop:string-prefix-p "/cdn.freshdesk.com/" path))
           :s3)
          ((ppcre:scan +cdn-s3-host-pattern+ host) :s3)
          (t (fail "attachment URL ~s is outside the permitted Freshdesk origins" url)))))

(defun attachments-of (object)
  (let ((items (and (hash-table-p object) (nlk:json-array object "attachments"))))
    (and items (coerce items 'list))))

(defun find-attachment (ticket-id attachment-id)
  "ATTACHMENT-ID's own record: from the ticket first, then its conversations.
The URL always comes from Freshdesk, never from a caller."
  (labels ((match (items)
             (find attachment-id items :test #'eql
                                   :key (lambda (item)
                                          (nlk:json-value item :integer "id")))))
    (let ((found (match (attachments-of (fetch-ticket ticket-id)))))
      (unless found
        (dolist (conversation (conversations ticket-id))
          (setf found (match (attachments-of conversation)))
          (when found (return))))
      (or found
          (fail "attachment ~a is not on ticket ~a, on the ticket or in its conversations"
                attachment-id ticket-id)))))

(defun directory-of (value)
  "VALUE - the section's path setting - as the directory attachments go in."
  (uiop:ensure-directory-pathname
   (cond ((pathnamep value) value)
         ((stringp value) (nlk:expand-home value))
         (t (fail "download_dir is not a path: ~s" value)))))

(defun leaf-name (name)
  "NAME's last path component, or a stand-in when it names none."
  (let* ((slash (position #\/ name :from-end t))
         (leaf (if slash (subseq name (1+ slash)) name)))
    (if (member leaf '("" "." "..") :test #'string=) "attachment.bin" leaf)))

(defun write-octets (path octets)
  "OCTETS written to PATH, which must not already exist; mode 0600 where the
system keeps modes."
  (ensure-directories-exist path)
  (let ((out (open path :direction :output :element-type '(unsigned-byte 8)
                        :if-exists nil :if-does-not-exist :create)))
    (unless out
      (fail "~a already exists; the attachment was not written over it" (namestring path)))
    (unwind-protect
         (write-sequence octets out)
      (close out)))
  (ignore-errors (uiop:symbol-call '#:sb-posix '#:chmod (namestring path) #o600))
  (namestring path))

(defun download-attachment (call)
  "The freshdesk_download_attachment tool: a ticket's attachment, on disk."
  (answering
    (let* ((ticket-id (positive (attachment-download-ticket-id call) "ticket_id"))
           (attachment-id (positive (attachment-download-attachment-id call) "attachment_id"))
           (attachment (find-attachment ticket-id attachment-id))
           (url (nlk:json-value attachment :string "attachment_url"))
           (name (nonblank (nlk:json-value attachment :string "name")))
           (tier (attachment-tier (or url "")))
           (directory (directory-of (setting :download-dir)))
           (destination (merge-pathnames (format nil "~a-~a" attachment-id (leaf-name name))
                                         directory)))
      (unless name
        (fail "attachment ~a carries no name" attachment-id))
      (multiple-value-bind (body status headers truncated)
          (dial :get url :authenticated (eq tier :account)
                :binary t :max-bytes +max-attachment-bytes+)
        (declare (ignore headers))
        (cond ((null status)
               (fail "attachment ~a could not be fetched~@[: ~a~]" attachment-id
                     (nlk:one-line (or body ""))))
              ((>= status 400)
               (fail "attachment ~a answered HTTP ~a" attachment-id status))
              (truncated
               (fail "attachment ~a is larger than the ~a-byte ceiling" attachment-id
                     +max-attachment-bytes+)))
        (let ((octets (if (stringp body) (nlk:octets body) body)))
          (answer (nlk:json-object "path" (write-octets destination octets)
                                    "bytes" (length octets))))))))

;;; --- freshdesk_add_private_note --------------------------------------------------------

(defun add-private-note (call)
  "The freshdesk_add_private_note tool: a note, escaped and read back."
  (answering
    (let* ((ticket-id (positive (note-add-ticket-id call) "ticket_id"))
           (body (nlk:trimmed (note-add-body call)))
           (created (sending :post (format nil "/api/v2/tickets/~a/notes" ticket-id)
                             (nlk:json-object "body" (html-div body) "private" t)
                             "private-note write"))
           (note-id (created-id created "private-note write"))
           (match (find-conversation ticket-id note-id)))
      (unless (and (eq t (nlk:json-value match :boolean "private"))
                   (string= (conversation-plain match) body))
        (error 'uncertain-write :operation "private-note write" :created-id note-id
                                :reason "the note readback did not match"))
      (answer match))))

;;; --- freshdesk_create_ticket -----------------------------------------------------------

(defun verify-create (sent readback ticket-id)
  "Every field SENT is READBACK's, or UNCERTAIN-WRITE."
  (labels ((unverified (reason)
             (error 'uncertain-write :operation "ticket creation"
                                     :created-id ticket-id :reason reason)))
    (dolist (key '("subject" "priority" "status" "requester_id"))
      (multiple-value-bind (value present) (gethash key sent)
        (when (and present (not (equal value (gethash key readback))))
          (unverified (format nil "ticket readback field ~a did not match" key)))))
    (multiple-value-bind (email present) (gethash "email" sent)
      (when present
        (let* ((requester (gethash "requester" readback))
               (actual (and (hash-table-p requester) (gethash "email" requester))))
          (unless (and (stringp actual) (string-equal actual email))
            (unverified "the requester email did not match requester.email")))))
    (multiple-value-bind (description present) (gethash "description" sent)
      (when present
        (let ((text (gethash "description_text" readback)))
          (unless (and (stringp text) (string= (nlk:trimmed text) description))
            (unverified "the readback description did not match description_text")))))))

(defun create-ticket (call)
  "The freshdesk_create_ticket tool: a ticket from plain text, read back."
  (answering
    (let* ((fields (ticket-create-fields call))
           (subject (create-fields-subject fields))
           (description (nlk:trimmed (create-fields-description fields)))
           (priority (create-fields-priority fields))
           (status (create-fields-status fields))
           (email (nonblank (create-fields-email fields)))
           (requester (create-fields-requester-id fields)))
      (unless (<= 1 priority 4)
        (fail "priority must be between 1 and 4, not ~a" priority))
      (unless (<= 2 status 5)
        (fail "status must be between 2 and 5, not ~a" status))
      (when (eq (and email t) (and requester t))
        (fail "give exactly one of email and requester_id: ~a"
              (if email "both arrived" "neither arrived")))
      (let ((sent (nlk:json-object "subject" subject
                                    "description" (html-div description)
                                    "priority" priority
                                    "status" status))
            (plain (nlk:json-object "subject" subject
                                     "description" description
                                     "priority" priority
                                     "status" status)))
        (when email
          (setf (gethash "email" sent) email
                (gethash "email" plain) email))
        (when requester
          (setf (gethash "requester_id" sent) (positive requester "requester_id")
                (gethash "requester_id" plain) requester))
        (let* ((created (sending :post "/api/v2/tickets" sent "ticket creation"))
               (ticket-id (created-id created "ticket creation"))
               (readback (fetch-ticket ticket-id :include "requester")))
          (verify-create plain readback ticket-id)
          (answer readback))))))

;;; --- freshdesk_reply_to_ticket ----------------------------------------------------------

(defun reply-to-ticket (call)
  "The freshdesk_reply_to_ticket tool: a public reply, read back."
  (answering
    (let* ((ticket-id (positive (ticket-reply-ticket-id call) "ticket_id"))
           (body (nlk:trimmed (ticket-reply-body call)))
           (created (sending :post (format nil "/api/v2/tickets/~a/reply" ticket-id)
                             (nlk:json-object "body" (html-div body))
                             "reply write"))
           (conversation-id (created-id created "reply write"))
           (match (find-conversation ticket-id conversation-id)))
      (unless (and (null (nlk:json-value match :boolean "private"))
                   (null (nlk:json-value match :boolean "incoming"))
                   (string= (conversation-plain match) body))
        (error 'uncertain-write :operation "reply write" :created-id conversation-id
                                :reason "the reply readback did not match"))
      (answer match))))

;;; --- freshdesk_add_time_entry -------------------------------------------------------------

(defun find-time-entry (ticket-id entry-id)
  "ENTRY-ID's own record on TICKET-ID, read back page by page."
  (let ((path (format nil "/api/v2/tickets/~a/time_entries" ticket-id)))
    (dotimes (index (setting :max-pages))
      (let ((rows (nlk:json-array
                   (reading :get path :query (list (cons "page" (1+ index))
                                                   (cons "per_page" +per-page+))))))
        (unless rows (fail "time-entry readback is invalid"))
        (loop for item across rows
              do (when (= (or (nlk:json-value item :integer "id") 0) entry-id)
                   (return-from find-time-entry item)))
        (when (< (length rows) +per-page+) (return))))
    (fail "time-entry readback is missing")))

(defun add-time-entry (call)
  "The freshdesk_add_time_entry tool: a finished time entry, read back."
  (answering
    (let* ((ticket-id (positive (time-entry-add-ticket-id call) "ticket_id"))
           (duration (canonical-time-spent (time-entry-add-time-spent call)))
           (note (nlk:trimmed (time-entry-add-note call)))
           (billable (and (time-entry-add-billable call) t))
           (agent (time-entry-add-agent-id call))
           (payload (nlk:json-object "time_spent" duration "note" note
                                      "billable" billable "timer_running" nil)))
      (when agent
        (setf (gethash "agent_id" payload) (positive agent "agent_id")))
      (let* ((created (sending :post (format nil "/api/v2/tickets/~a/time_entries" ticket-id)
                                payload "time-entry write"))
             (entry-id (created-id created "time-entry write"))
             (found (find-time-entry ticket-id entry-id)))
        (unless (and (= (or (nlk:json-value found :integer "ticket_id") 0) ticket-id)
                     (string= (or (nlk:json-value found :string "time_spent") "") duration)
                     (string= (or (nlk:json-value found :string "note") "") note)
                     (eq (and (nlk:json-value found :boolean "billable") t) billable)
                     (null (nlk:json-value found :boolean "timer_running")))
          (error 'uncertain-write :operation "time-entry write" :created-id entry-id
                                  :reason "the time-entry readback did not match"))
        (answer found)))))

;;; --- freshdesk_update_ticket --------------------------------------------------------------

(defun update-ticket (call)
  "The freshdesk_update_ticket tool: status and/or priority, read back."
  (answering
    (let* ((ticket-id (positive (ticket-update-ticket-id call) "ticket_id"))
           (status (ticket-update-status call))
           (priority (ticket-update-priority call))
           (sent (nlk:make-json-object)))
      (unless (or status priority)
        (fail "update_ticket requires at least one of status and priority"))
      (when status
        (unless (<= 2 status 5)
          (fail "status must be between 2 and 5, not ~a" status))
        (setf (gethash "status" sent) status))
      (when priority
        (unless (<= 1 priority 4)
          (fail "priority must be between 1 and 4, not ~a" priority))
        (setf (gethash "priority" sent) priority))
      (sending :put (format nil "/api/v2/tickets/~a" ticket-id) sent "ticket update")
      (let ((readback (fetch-ticket ticket-id)))
        (maphash (lambda (key value)
                   (unless (equal value (gethash key readback))
                     (error 'uncertain-write :operation "ticket update" :created-id ticket-id
                                             :reason (format nil "ticket readback field ~a did not match" key))))
                 sent)
        (answer readback)))))

;;; --- the cell -------------------------------------------------------------------------

(nle:define-cell freshdesk
  (:section ("freshdesk")
    (:guide "your account name (the part before .freshdesk.com) and an API key from Freshdesk's Profile, API Keys, named in api_key_env or kept in api_key_file; next step: ask for the open tickets")
    (:one-of "api_key_env" "api_key_file")
    ("domain" :string :required t
     :doc "your Freshdesk account name, the part before .freshdesk.com, or https://<account>.freshdesk.com")
    ("api_key_env" :env
     :doc "the environment variable holding the API key (FRESHDESK_API_KEY)")
    ("api_key_file" :path
     :doc "a file holding the API key, which the file's own permissions guard")
    ("download_dir" :path :default "~/.nodecode/cache/freshdesk"
     :doc "where freshdesk_download_attachment writes the files it fetches")
    ("max_pages" :integer :default 10 :min 1
     :doc "how many pages of 100 a list or a search may read before it reports truncated")
    ("timeout" :integer :default 20 :min 1
     :doc "how long one Freshdesk request may take, in seconds"))
  (:settings #'read-settings)
  (:help :freshdesk
         "freshdesk: eleven tools that read and write the tickets of your own Freshdesk account"
         *manual*)
  (:tool "freshdesk_list_tickets" (nlk:contract 'ticket-list) #'list-tickets)
  (:tool "freshdesk_get_ticket" (nlk:contract 'ticket-read) #'get-ticket)
  (:tool "freshdesk_search_tickets" (nlk:contract 'ticket-search) #'search-tickets)
  (:tool "freshdesk_list_conversations" (nlk:contract 'conversation-list) #'list-conversations)
  (:tool "freshdesk_get_contact" (nlk:contract 'contact-read) #'get-contact)
  (:tool "freshdesk_download_attachment" (nlk:contract 'attachment-download) #'download-attachment)
  (:tool "freshdesk_add_private_note" (nlk:contract 'note-add) #'add-private-note)
  (:tool "freshdesk_create_ticket" (nlk:contract 'ticket-create) #'create-ticket)
  (:tool "freshdesk_reply_to_ticket" (nlk:contract 'ticket-reply) #'reply-to-ticket)
  (:tool "freshdesk_add_time_entry" (nlk:contract 'time-entry-add) #'add-time-entry)
  (:tool "freshdesk_update_ticket" (nlk:contract 'ticket-update) #'update-ticket))
