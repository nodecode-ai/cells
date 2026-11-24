;;;; cell-test.lisp --- the cell against a stubbed EquipDash MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches EquipDash, no key
;;;; is needed and no shop is touched. What is proved is the cell's own
;;;; wiring: the section (a healthy start, and a missing key refused), the
;;;; three tools through the dispatch the model calls them by -- the MCP method
;;;; and the tool each one names, the bearer key and the session id on the
;;;; wire, the preview and the person's yes before anything that changes the
;;;; shop, the day in one call -- and the /equipdash command.

(in-package #:nodecode.test)

(define-test-slice "equipdash" "EQUIPDASH-CELL-" :start nodecode-equipdash:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defparameter *calls* nil
  "Every request the stub answered, newest first, as (METHOD PARAMS HEADERS).")

(defparameter *answer* nil
  "When set, (TEXT STATUS HEADERS): what the stub answers to tools/call.")

(defparameter *offered*
  (vector
   (nlk:json-object "name" "get_today"
                     "description" "The day as the Today screen shows it: sessions, pickups, returns."
                     "annotations" (nlk:json-object "readOnlyHint" t)
                     "inputSchema" (nlk:json-object
                                    "type" "object"
                                    "properties" (nlk:json-object
                                                  "date" (nlk:json-object "type" "string")
                                                  "section" (nlk:json-object "type" "string"))
                                    "required" (vector)))
   (nlk:json-object "name" "get_needs_you"
                     "description" "Booking requests, late returns and unpaid bookings waiting on the owner."
                     "annotations" (nlk:json-object "readOnlyHint" t))
   (nlk:json-object "name" "check_product_availability"
                     "description" "Whether an item is free over a period, for a quantity."
                     "annotations" (nlk:json-object "readOnlyHint" t)
                     "inputSchema" (nlk:json-object
                                    "type" "object"
                                    "properties" (nlk:json-object
                                                  "product_id" (nlk:json-object "type" "string")
                                                  "date_from" (nlk:json-object "type" "string")
                                                  "date_to" (nlk:json-object "type" "string")
                                                  "quantity" (nlk:json-object "type" "integer"))
                                    "required" (vector "product_id" "date_from")))
   (nlk:json-object "name" "create_booking"
                     "description" "Preview a booking, then make it with user_confirmed true."
                     "annotations" (nlk:json-object "readOnlyHint" nil)
                     "inputSchema" (nlk:json-object
                                    "type" "object"
                                    "properties" (nlk:json-object
                                                  "product_id" (nlk:json-object "type" "string"))
                                    "required" (vector "product_id")))
   (nlk:json-object "name" "record_payment"
                     "description" "Record a payment taken in person. Saves straight away."
                     "annotations" (nlk:json-object "readOnlyHint" nil))
   (nlk:json-object "name" "send_payment_link"
                     "description" "Email the customer a link for the balance; confirmed true sends it."
                     "annotations" (nlk:json-object "readOnlyHint" nil))
   (nlk:json-object "name" "process_refund"
                     "description" "Refund a card payment; confirmed true refunds it."
                     "annotations" (nlk:json-object "readOnlyHint" nil))
   (nlk:json-object "name" "update_booking"
                     "description" "Change a booking's details."))
  "The tools the stub server lists: three read-only, four that change the shop, one with no hint.")

(defun envelope (id result)
  "One JSON-RPC answer carrying RESULT, as the wire spells it."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun text-result (text &key errorp)
  "One tools/call result carrying TEXT."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                   "isError" (and errorp t)))

(defun call-result (params)
  "What the stub's tools/call answers: the day's own words, else the tool
and its arguments back."
  (let ((tool (nlk:json-value params :string "name"))
        (arguments (nlk:json-value params :object "arguments")))
    (text-result (cond ((equal tool "get_today") "Today: 2 rentals out, 1 due back")
                       ((equal tool "get_needs_you") "Needs you: 1 late return")
                       (t (format nil "~a answered~@[ ~a~]" tool
                                  (and arguments (nlk:encode-json-object arguments))))))))

(defun session-headers ()
  (let ((headers (make-hash-table :test #'equal)))
    (setf (gethash "mcp-session-id" headers) "sess-1")
    headers))

(defun stub-wire (options)
  "NLK:HTTP, answering from the request's own method; nothing leaves the machine."
  (let* ((body (nlk:decode-json (or (getf options :content) "{}")))
         (name (nlk:json-value body :text "method"))
         (params (nlk:json-value body :object "params"))
         (id (nlk:json-value body :integer "id")))
    (push (list name params (getf options :headers)) *calls*)
    (cond ((equal name "initialize")
           (values (envelope id (nlk:json-object "protocolVersion" "2025-06-18"
                                                   "capabilities" (nlk:make-json-object)))
                   200 (session-headers) nil))
          ((equal name "notifications/initialized")
           (values "" 202 nil nil))
          ((equal name "tools/list")
           (values (envelope id (nlk:json-object "tools" *offered*)) 200 nil nil))
          ((equal name "tools/call")
           (if *answer*
               (values (first *answer*) (second *answer*) (third *answer*) nil)
               (values (envelope id (call-result params)) 200 nil nil)))
          (t (values "{}" 404 nil nil)))))

(defmacro with-equipdash-wire (&body body)
  "BODY with NLK:HTTP stubbed: every request is logged and answered here."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (stub-wire options))
       ,@body)))

(defmacro with-equipdash-running (&body body)
  "BODY with the cell started on a literal section, stopped on unwind."
  `(with-cell-stop ((equipdash-start "token" "key-1"))
     ,@body))

(defun equipdash-request (method)
  "The newest stubbed request of JSON-RPC METHOD, or NIL."
  (find method *calls* :test #'equal :key #'first))

(defun equipdash-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun equipdash-tool-call (&optional (index 0))
  "The INDEX'th newest tools/call the cell sent, as (METHOD PARAMS HEADERS)."
  (nth index (remove-if-not (lambda (call) (equal (first call) "tools/call")) *calls*)))

(defun equipdash-sent-tool (&optional (index 0))
  "The MCP tool name the INDEX'th newest tools/call named."
  (let ((call (equipdash-tool-call index)))
    (and call (nlk:json-value (second call) :string "name"))))

(defun equipdash-sent-arguments (&optional (index 0))
  "The arguments object of the INDEX'th newest tools/call."
  (let ((call (equipdash-tool-call index)))
    (and call (nlk:json-value (second call) :object "arguments"))))

(defun equipdash-sent-tools ()
  "Every MCP tool name the cell called, newest first."
  (loop for call in (remove-if-not (lambda (call) (equal (first call) "tools/call")) *calls*)
        collect (nlk:json-value (second call) :string "name")))

(defun equipdash-called (tool arguments)
  "TOOL called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "equipdash-call" tool arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "equipdash"
  (:fixture with-equipdash-wire)
  (:config "token" "key-1")
  (:help :equipdash)
  (:command "equipdash")
  (:running
   (is (nle::find-tool "equipdash_tools") "the listing tool is registered")
   (is (nle::find-tool "equipdash_call") "and the one that runs a tool")
   (is (nle::find-tool "equipdash_day") "and the one that answers the day")
   (is (= 3 (count-if (lambda (entry) (eql 0 (search "equipdash_" (first entry))))
                      nle::*tools*)) "three of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "equipdash_tools")) "and stop takes every one back")
   (is (null (nle::find-tool "equipdash_day"))))
  (:refused ("token" "key-1" "timeout_seconds" 0)
            ("token" "key-1" "max_chars" 10)))

(deftest equipdash-cell-a-start-with-no-key-refuses (with-equipdash-wire)
  (with-stubbed-fdefinition (nodecode-equipdash::mcp-token (url)
                                (declare (ignore url))
                                nil)
    (with-stubbed-fdefinition (nodecode-equipdash::environment-token (name)
                                  (declare (ignore name))
                                  nil)
      (let ((text (refusal-text nlk:config-refusal (equipdash-start))))
        (is-carrying text "API key" "mcp:access" "restart-cells")
        (is (null (nle::find-tool "equipdash_tools")) "nothing was installed")))))

(deftest equipdash-cell-the-mcp-cells-sign-in-is-reused (with-equipdash-wire)
  ;; The shipped mcp cell keeps its OAuth sign-in under the home; this cell
  ;; reads that token rather than asking for a key of its own.
  (let ((path (nlk:home "mcp/oauth.json")))
    (unwind-protect
         (progn
           (ensure-directories-exist path)
           (with-open-file (out path :direction :output :if-exists :supersede
                                      :if-does-not-exist :create)
             (write-string (nlk:encode-json-object
                            (nlk:json-object "https://app.equipdash.com/mcp/v1"
                                             (nlk:json-object "tokens"
                                                              (nlk:json-object "access_token" "key-stored"))))
                           out))
           (with-stubbed-fdefinition (nodecode-equipdash::environment-token (name)
                                         (declare (ignore name))
                                         nil)
             (with-cell-stop ((equipdash-start))
               (equipdash-called "equipdash_day" "{}")
               (is (equal "Bearer key-stored"
                          (equipdash-request-header (equipdash-tool-call) "authorization")) "the token the mcp cell kept is the one sent"))))
      (when (probe-file path) (ignore-errors (delete-file path))))))

;;; --- the wire ---------------------------------------------------------------------

(deftest equipdash-cell-one-session-serves-every-call (with-equipdash-wire)
  (with-equipdash-running
    (equipdash-called "equipdash_tools" "{}")
    (equipdash-called "equipdash_call" "{\"name\": \"check_product_availability\"}")
    (is (= 1 (count-if (lambda (call) (equal (first call) "initialize")) *calls*)) "initialize happens once and is reused")
    (is (equal "Bearer key-1" (equipdash-request-header (equipdash-tool-call) "authorization")) "with this cell's own key")
    (is (equal "sess-1" (equipdash-request-header (equipdash-tool-call) "mcp-session-id")) "and the session id the server handed is echoed")))

(deftest equipdash-cell-an-error-the-server-reports-fails-the-call (with-equipdash-wire)
  (with-equipdash-running
    (setf *answer* (list (envelope 1 (text-result "product p-1 is not booked then" :errorp t))
                         200 nil))
    (let ((answer (equipdash-called "equipdash_call"
                                     "{\"name\": \"check_product_availability\", \"arguments\": {\"product_id\": \"p-1\"}}")))
      (is (search "ERROR:" answer) "the call is failed, not answered")
      (is (search "product p-1 is not booked then" answer) "and the server's words reach the model"))))

;;; --- the list says what the skill says ---------------------------------------------

(deftest equipdash-cell-the-list-marks-what-changes-the-shop (with-equipdash-wire)
  (with-equipdash-running
    (let ((answer (equipdash-called "equipdash_tools" "{}")))
      (is (search "get_today" answer) "every tool the server offers is named")
      (is (search "arguments: date, section" answer) "with the arguments the tool's own schema names")
      (is (search "check_product_availability [read-only]" answer) "a read-only tool says so")
      (is (search "create_booking [changes the shop]" answer) "a write says it changes the shop")
      (is (search "required: date_from, product_id" answer) "and its required arguments")
      (is (search "update_booking [changes the shop]" answer) "a tool with no readOnlyHint is treated as changing the shop"))))

(deftest equipdash-cell-the-list-takes-a-filter (with-equipdash-wire)
  (with-equipdash-running
    (let ((answer (equipdash-called "equipdash_tools" "{\"filter\": \"payment\"}")))
      (is (search "record_payment" answer) "the matching tools are listed")
      (is (not (search "get_today" answer)) "and the others are not"))))

(deftest equipdash-cell-a-read-only-tool-runs-without-a-yes (with-equipdash-wire)
  (with-equipdash-running
    (let ((answer (equipdash-called "equipdash_call" "{\"name\": \"get_today\"}")))
      (is (equal "get_today" (equipdash-sent-tool)) "the server's own tool is called")
      (is (search "2 rentals out" answer) "and its answer reaches the model"))))

;;; --- the preview, and the person's yes --------------------------------------------

(deftest equipdash-cell-a-write-without-preview-or-yes-is-refused (with-equipdash-wire)
  (with-equipdash-running
    (let ((answer (equipdash-called "equipdash_call"
                                     "{\"name\": \"create_booking\", \"arguments\": {\"product_id\": \"p-1\"}}")))
      (is (search "needs the person's yes" answer) "the refusal says what the call wants")
      (is (search "preview true" answer) "and names the preview that comes first")
      (is (search "confirmed true" answer) "and the yes that follows it")
      (is (null (equipdash-tool-call)) "nothing was sent"))))

(deftest equipdash-cell-a-preview-sends-no-yes-and-says-so (with-equipdash-wire)
  (with-equipdash-running
    (let ((answer (equipdash-called "equipdash_call"
                                     "{\"name\": \"create_booking\", \"arguments\": {\"product_id\": \"p-1\"}, \"preview\": true}")))
      (is (equal "create_booking" (equipdash-sent-tool)) "the tool is asked")
      (multiple-value-bind (value present) (gethash "user_confirmed" (equipdash-sent-arguments))
        (declare (ignore value))
        (is (not present) "with no yes attached"))
      (is (search "preview: create_booking has answered" answer) "and the model is told it is a preview")
      (is (search "record_payment" answer) "naming the one tool that saves before the yes"))))

(deftest equipdash-cell-a-confirmed-call-sends-the-yes (with-equipdash-wire)
  (with-equipdash-running
    (equipdash-called "equipdash_call"
                       "{\"name\": \"create_booking\", \"arguments\": {\"product_id\": \"p-1\"}, \"confirmed\": true}")
    (is (equal "create_booking" (equipdash-sent-tool)))
    (is (eq t (nlk:json-value (equipdash-sent-arguments) :boolean "user_confirmed")) "a booking takes user_confirmed true")
    (is (equal "p-1" (nlk:json-value (equipdash-sent-arguments) :text "product_id")) "and the tool's own arguments go through as the server names them")
    (equipdash-called "equipdash_call"
                       "{\"name\": \"process_refund\", \"arguments\": {\"booking_id\": \"b-9\", \"amount\": 40}, \"confirmed\": true}")
    (is (eq t (nlk:json-value (equipdash-sent-arguments) :boolean "confirmed")) "a refund takes confirmed true, as the skill spells it")))

(deftest equipdash-cell-an-unknown-name-lists-what-there-is (with-equipdash-wire)
  (with-equipdash-running
    (equipdash-called "equipdash_tools" "{}")
    (let ((answer (equipdash-called "equipdash_call" "{\"name\": \"cancel_everything\"}")))
      (is (search "equipdash_tools" answer) "the model is told where the names are")
      (is (null (equipdash-tool-call)) "and nothing was sent as a call")
      (is (= 2 (count-if (lambda (call) (equal (first call) "tools/list")) *calls*)) "the list is read again before the name is refused"))))

;;; --- the day ----------------------------------------------------------------------

(deftest equipdash-cell-the-day-is-today-and-what-needs-you (with-equipdash-wire)
  (with-equipdash-running
    (let ((answer (equipdash-called "equipdash_day" "{}")))
      (is (equal '("get_needs_you" "get_today") (equipdash-sent-tools)) "both reads, today first")
      (is-carrying answer "2 rentals out" "Needs you: 1 late return"))))

(deftest equipdash-cell-the-day-takes-a-day-and-a-section (with-equipdash-wire)
  (with-equipdash-running
    (let ((answer (equipdash-called "equipdash_day"
                                     "{\"date\": \"2026-11-08\", \"section\": \"out_now\"}")))
      (is (equal '("get_today") (equipdash-sent-tools)) "a section narrows it to one read")
      (is (equal "2026-11-08" (nlk:json-value (equipdash-sent-arguments) :text "date")) "the day is the shop's own, sent as written")
      (is (equal "out_now" (nlk:json-value (equipdash-sent-arguments) :text "section")))
      (is (search "2 rentals out" answer)))))

(deftest equipdash-cell-a-day-the-plan-does-not-offer-says-so (with-equipdash-wire)
  (let ((*offered* (vector (nlk:json-object "name" "get_needs_you"
                                              "description" "What needs the owner."
                                              "annotations" (nlk:json-object "readOnlyHint" t)))))
    (with-equipdash-running
      (is-carrying (answer (equipdash-called "equipdash_day" "{}"))
        "offers no get_today" "Needs you: 1 late return"))))

;;; --- the operator's command ---------------------------------------------------------

(deftest equipdash-cell-the-command-reads-the-day (with-equipdash-wire)
  (with-equipdash-running
    (let ((answer (nodecode-equipdash::run-command "" nil)))
      (is (search "2 rentals out" answer) "no argument answers the day")
      (is (equal '("get_needs_you" "get_today") (equipdash-sent-tools))))))

(deftest equipdash-cell-the-command-calls-a-tool-as-the-operators-own-yes (with-equipdash-wire)
  (with-equipdash-running
    (let ((answer (nodecode-equipdash::run-command "call create_booking {\"product_id\": \"p-1\"}" nil)))
      (is (equal "create_booking" (equipdash-sent-tool)) "the named tool is called")
      (is (eq t (nlk:json-value (equipdash-sent-arguments) :boolean "user_confirmed")) "with the operator's own yes attached")
      (is (search "create_booking answered" answer)))))

(deftest equipdash-cell-the-command-lists-the-tools (with-equipdash-wire)
  (with-equipdash-running
    (let ((answer (nodecode-equipdash::run-command "tools payment" nil)))
      (is (search "record_payment" answer) "a word lists the tools it matches")
      (is (not (search "get_today" answer)) "and no others"))))
