;;;; cell-test.lisp --- the freshdesk cell against a stubbed account.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Freshdesk endpoint is a stubbed NLK:HTTP and the API key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches an account, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it -
;;;; through the real dispatch, contract and all - so the schema the provider
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "freshdesk" "FRESHDESK-CELL-")

(defun freshdesk-config (&rest pairs)
  "A whole shared config holding the `freshdesk' section PAIRS make, with its
domain and API key variable defaulted."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "domain" section "acme")
    (alexandria:ensure-gethash "api_key_env" section "FRESHDESK_API_KEY")
    (nlk:json-object "freshdesk" section)))

(defun freshdesk-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-freshdesk:start-cell (apply #'freshdesk-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-fd-key (&body body)
  "BODY with the API key environment answering one fixed key."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "FRESHDESK_API_KEY") "test-key"))
     ,@body))

(defmacro with-fd-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL CONTENT BINARY), oldest
first, once BODY reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content)
                                               (getf options :binary))
                                         ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected Freshdesk request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defparameter +tickets+
  "{\"id\":1,\"subject\":\"Printer will not print\",\"status\":2,\"priority\":3,\"description_text\":\"The office printer is offline.\"}"
  "One ticket, as Freshdesk answers it.")

(defparameter +ticket-list+
  (concatenate 'string
               "[{\"id\":1,\"subject\":\"Printer will not print\",\"status\":2,\"priority\":3},"
               "{\"id\":2,\"subject\":\"VPN drops\",\"status\":3,\"priority\":2}]")
  "Two tickets, as GET /api/v2/tickets answers a page.")

(defun fd-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun fd-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (let ((text (fd-answer id tool arguments)))
    (ignore-errors (nlk:decode-json text))))

(defun octets-of (text)
  "TEXT as the octets a download answers with."
  (let ((octets (make-array (length text) :element-type '(unsigned-byte 8))))
    (loop for character across text
          for index from 0
          do (setf (aref octets index) (char-code character)))
    octets))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "freshdesk"
  (:config "domain" "acme" "api_key_env" "FRESHDESK_API_KEY")
  (:help :freshdesk)
  (:fixture with-fd-key)
  (:refused ("domain" 5) ("max_pages" 0)))

(deftest freshdesk-cell-without-a-key-refuses ()
  ;; The one setting no verb can work without: the section names an API key, and
  ;; a section that names none is refused at start, before anything installs.
  (with-fd-key
    (let ((condition (signals-error nlk:config-refusal
                                    (freshdesk-start "api_key_env" ""))))
      (is (search "no API key" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "freshdesk_list_tickets")) "and installs nothing"))))

(deftest freshdesk-cell-refuses-a-custom-cname ()
  ;; The API key may only travel to a native Freshdesk origin.
  (with-fd-key
    (is (signals-error nlk:config-refusal (freshdesk-start "domain" "help.example.com")))
    (is (signals-error nlk:config-refusal
                       (freshdesk-start "domain" "https://help.example.com/freshdesk")))))

;;; --- freshdesk_list_tickets -----------------------------------------------------------

(deftest freshdesk-cell-lists-tickets (with-fd-key)
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked ("/api/v2/tickets" (values +ticket-list+ 200)))
      (let* ((object (fd-object "fd-list-1" "freshdesk_list_tickets" "{}"))
             (data (nlk:json-value object :object "data"))
             (tickets (nlk:json-array data "tickets")))
        (is (= 2 (length tickets)) "both tickets come back")
        (is (equal "Printer will not print"
                   (nlk:json-value (aref tickets 0) :string "subject")))
        (is (null (nlk:json-value data :boolean "truncated")) "and the page was whole")
        (is (equal :get (first (first asked))) "a plain list is one GET")
        (is (search "per_page=100" (second (first asked))) "Freshdesk's largest page")))))

(deftest freshdesk-cell-says-when-a-list-is-cut (with-fd-key)
  ;; A full page at the operator's page cap is truncated, and says so rather
  ;; than pretending the list is complete.
  (let ((full (format nil "[~a]"
                       (format nil "~{~a~^,~}"
                               (loop for id from 1 to 100
                                     collect (format nil "{\"id\":~a,\"subject\":\"t~a\"}"
                                                     id id))))))
    (with-cell-stop ((freshdesk-start "max_pages" 1))
      (with-fd-wire (asked ("/api/v2/tickets" (values full 200)))
        (let* ((object (fd-object "fd-list-2" "freshdesk_list_tickets" "{}"))
               (data (nlk:json-value object :object "data")))
          (is (eq t (nlk:json-value data :boolean "truncated")))
          (is (= 100 (length (nlk:json-array data "tickets"))))
          (is (= 1 (length asked)) "one page, and no more"))))))

;;; --- freshdesk_get_ticket --------------------------------------------------------------

(deftest freshdesk-cell-reads-one-ticket (with-fd-key)
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked ("/api/v2/tickets/1" (values +tickets+ 200)))
      (let* ((object (fd-object "fd-get-1" "freshdesk_get_ticket" "{\"ticket_id\":1}"))
             (ticket (nlk:json-value object :object "data")))
        (is (equal "Printer will not print" (nlk:json-value ticket :string "subject")))
        (is (= 2 (nlk:json-value ticket :integer "status")))
        (is (equal "/api/v2/tickets/1" (subseq (second (first asked))
                                                (length "https://acme.freshdesk.com"))) "the id rides the path, under the account's own origin")))))

(deftest freshdesk-cell-refuses-a-call-with-no-ticket-id (with-fd-key)
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked)
      (let ((answer (fd-answer "fd-get-2" "freshdesk_get_ticket" "{}")))
        (is (search "preflight" answer) "the contract refuses it before the body")
        (is (search "ticket_id" answer))
        (is (null asked) "and nothing was asked of Freshdesk")))))

(deftest freshdesk-cell-refuses-a-readback-that-is-not-the-ticket (with-fd-key)
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked ("/api/v2/tickets/1" (values "{\"id\":2,\"subject\":\"other\"}" 200)))
      (is (search "readback is invalid"
                  (fd-answer "fd-get-3" "freshdesk_get_ticket" "{\"ticket_id\":1}"))))))

;;; --- freshdesk_search_tickets ------------------------------------------------------------

(deftest freshdesk-cell-searches-by-filter (with-fd-key)
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked ("/api/v2/search/tickets"
                          (values "{\"results\":[{\"id\":1,\"subject\":\"Printer will not print\"}],\"total\":1}"
                                  200)))
      (let* ((object (fd-object "fd-search-1" "freshdesk_search_tickets"
                                 "{\"status\":2,\"priority\":3}"))
             (data (nlk:json-value object :object "data")))
        (is (= 1 (length (nlk:json-array data "results"))))
        (is (null (nlk:json-value data :boolean "truncated")))
        (is (search "status" (second (first asked))) "the query is built here, not taken from the caller")))))

(deftest freshdesk-cell-refuses-a-search-with-no-filter (with-fd-key)
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked)
      (is (search "at least one filter"
                  (fd-answer "fd-search-2" "freshdesk_search_tickets" "{}")))
      (is (null asked)))
    (with-fd-wire (asked)
      (is (search "between 2 and 5"
                  (fd-answer "fd-search-3" "freshdesk_search_tickets" "{\"status\":9}")))
      (is (null asked)))))

;;; --- freshdesk_add_private_note ------------------------------------------------------------

(deftest freshdesk-cell-writes-a-note-and-reads-it-back (with-fd-key)
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked ("/api/v2/tickets/1/notes" (values "{\"id\":55}" 201))
                         ("/api/v2/tickets/1/conversations"
                          (values "[{\"id\":55,\"private\":true,\"body_text\":\"Please advise\"}]" 200)))
      (let ((object (fd-object "fd-note-1" "freshdesk_add_private_note"
                                "{\"ticket_id\":1,\"body\":\"Please advise\"}")))
        (is (eq t (nlk:json-value object :boolean "success")) "the note was verified")
        (let ((sent (nlk:decode-json (third (first asked)))))
          (is (eq t (nlk:json-value sent :boolean "private")) "it is private")
          (is (equal "<div>Please advise</div>" (nlk:json-value sent :string "body")) "the text is HTML-escaped and wrapped"))))))

(deftest freshdesk-cell-answers-a-write-it-could-not-verify (with-fd-key)
  ;; A 5xx leaves the write ambiguous: the answer says so, with retry_safe
  ;; false, rather than claiming a failure somebody might retry.
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked ("/api/v2/tickets/1/notes" (values "upstream" 500)))
      (let ((object (fd-object "fd-note-2" "freshdesk_add_private_note"
                                "{\"ticket_id\":1,\"body\":\"Please advise\"}")))
        (is (equal "write_may_have_succeeded" (nlk:json-value object :string "state")))
        (is (null (nlk:json-value object :boolean "retry_safe")) "and it is not retry-safe")))))

(deftest freshdesk-cell-answers-a-rejected-write (with-fd-key)
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked ("/api/v2/tickets/1/notes" (values "{\"description\":\"bad\"}" 403)))
      (let ((answer (fd-answer "fd-note-3" "freshdesk_add_private_note"
                                "{\"ticket_id\":1,\"body\":\"Please advise\"}")))
        (is (search "rejected by Freshdesk: HTTP 403" answer))
        (is (search "bad" answer) "Freshdesk's own words come with it")))))

;;; --- freshdesk_update_ticket ---------------------------------------------------------------

(deftest freshdesk-cell-updates-and-reads-back (with-fd-key)
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked ("/api/v2/tickets/1"
                          (if (eq method :put)
                              (values "{\"id\":1,\"status\":4}" 200)
                              (values "{\"id\":1,\"subject\":\"Printer will not print\",\"status\":4}" 200))))
      (let ((object (fd-object "fd-update-1" "freshdesk_update_ticket"
                                "{\"ticket_id\":1,\"status\":4}")))
        (is (eq t (nlk:json-value object :boolean "success")))
        (is (= 4 (nlk:json-value (nlk:json-value object :object "data") :integer "status")))
        (is (eq :put (first (first asked))) "the write is a PUT")
        (is (= 2 (length asked)) "and the readback follows it")))))

(deftest freshdesk-cell-refuses-an-update-with-nothing-to-change (with-fd-key)
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked)
      (is (search "at least one of status and priority"
                  (fd-answer "fd-update-2" "freshdesk_update_ticket" "{\"ticket_id\":1}")))
      (is (null asked)))))

;;; --- freshdesk_add_time_entry -----------------------------------------------------------

(deftest freshdesk-cell-canonicalizes-a-duration (with-fd-key)
  (is (equal "01:30" (nodecode-freshdesk:canonical-time-spent "1:30")))
  (is (equal "00:01" (nodecode-freshdesk:canonical-time-spent "000:01")))
  (is (equal "123:05" (nodecode-freshdesk:canonical-time-spent "123:05")))
  (is (signals-error nodecode-freshdesk:freshdesk-error
                     (nodecode-freshdesk:canonical-time-spent "0:00")) "a zero duration is refused"))

(deftest freshdesk-cell-adds-a-time-entry (with-fd-key)
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked ("/api/v2/tickets/1/time_entries"
                          (if (eq method :post)
                              (values "{\"id\":9}" 201)
                              (values "[{\"id\":9,\"ticket_id\":1,\"time_spent\":\"01:30\",\"note\":\"Call\",\"billable\":true,\"timer_running\":false}]" 200))))
      (let ((object (fd-object "fd-time-1" "freshdesk_add_time_entry"
                                "{\"ticket_id\":1,\"time_spent\":\"1:30\",\"note\":\"Call\",\"billable\":true}")))
        (is (eq t (nlk:json-value object :boolean "success")))
        (let ((sent (nlk:decode-json (third (first asked)))))
          (is (equal "01:30" (nlk:json-value sent :string "time_spent")) "the duration is sent in the form Freshdesk keeps")
          (is (null (nlk:json-value sent :boolean "timer_running")) "a finished entry"))))))

;;; --- freshdesk_download_attachment --------------------------------------------------------

(deftest freshdesk-cell-downloads-only-from-freshdesk (with-fd-key)
  (let ((directory (format nil "~a/freshdesk-~a/" (uiop:temporary-directory)
                           (get-universal-time))))
    (unwind-protect
         (with-cell-stop ((freshdesk-start "download_dir" directory))
           (with-fd-wire (asked ("/api/v2/tickets/1"
                                 (values "{\"id\":1,\"subject\":\"t\",\"attachments\":[{\"id\":7,\"name\":\"invoice.pdf\",\"attachment_url\":\"https://acme-attachment.freshdesk.com/helpdesk/attachments/1\"}]}" 200))
                                ("acme-attachment.freshdesk.com"
                                 (values (octets-of "PDF") 200))
                                ("/api/v2/tickets/1/conversations" (values "[]" 200)))
             (let* ((object (fd-object "fd-attach-1" "freshdesk_download_attachment"
                                        "{\"ticket_id\":1,\"attachment_id\":7}"))
                    (data (nlk:json-value object :object "data"))
                    (path (nlk:json-value data :string "path")))
               (is (equal 3 (nlk:json-value data :integer "bytes")))
               (is (search "7-invoice.pdf" path) "the file carries the id and its own name")
               (is (eq t (fourth (second asked))) "the CDN answer is read as bytes")
               (is (= 3 (with-open-file (in path :element-type '(unsigned-byte 8))
                          (file-length in))) "and the bytes are on disk"))))
      (ignore-errors (uiop:delete-directory-tree (uiop:ensure-directory-pathname directory)
                                                 :validate t :if-does-not-exist :ignore)))))

(deftest freshdesk-cell-refuses-an-attachment-outside-freshdesk (with-fd-key)
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked ("/api/v2/tickets/1"
                          (values "{\"id\":1,\"attachments\":[{\"id\":7,\"name\":\"a.pdf\",\"attachment_url\":\"https://evil.example.com/a.pdf\"}]}" 200)))
      (is (search "outside the permitted Freshdesk origins"
                  (fd-answer "fd-attach-2" "freshdesk_download_attachment"
                             "{\"ticket_id\":1,\"attachment_id\":7}")))
      (is (= 1 (length asked)) "nothing outside the account was fetched"))))

;;; --- freshdesk_list_conversations and freshdesk_get_contact ---------------------------------

(deftest freshdesk-cell-lists-conversations (with-fd-key)
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked ("/api/v2/tickets/1/conversations"
                          (values "[{\"id\":3,\"private\":false,\"incoming\":false,\"body_text\":\"Fixed.\"}]" 200)))
      (let ((object (fd-object "fd-conv-1" "freshdesk_list_conversations" "{\"ticket_id\":1}")))
        (is (= 1 (length (nlk:json-array object "data")))))
      (is (search "per_page=100" (second (first asked)))))))

(deftest freshdesk-cell-reads-a-contact (with-fd-key)
  (with-cell-stop ((freshdesk-start))
    (with-fd-wire (asked ("/api/v2/contacts/4"
                          (values "{\"id\":4,\"name\":\"Sam\",\"email\":\"sam@example.com\"}" 200)))
      (let* ((object (fd-object "fd-contact-1" "freshdesk_get_contact" "{\"contact_id\":4}"))
            (contact (nlk:json-value object :object "data")))
        (is (equal "Sam" (nlk:json-value contact :string "name")))))))
