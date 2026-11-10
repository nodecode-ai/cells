;;;; cell.lisp --- NeuralWatt's read-only analytics API as Nodecode tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exchange is one GET to the NeuralWatt API carrying the account's
;;;; own key, and nothing is written anywhere. Nine tools, one /neuralwatt
;;;; command, one manual topic and a Lisp function each for eval: the
;;;; original Hermes plugin's nine tools, reimplemented natively, answering
;;;; formatted text where it answered JSON.

(defpackage #:nodecode-neuralwatt
  (:use #:cl)
  (:nicknames #:neuralwatt)
  (:export #:health #:model-catalog #:prices #:quota #:dashboard #:energy
           #:usage-view #:requests-view #:sessions-view #:neuralwatt-error))

(in-package #:nodecode-neuralwatt)

(nlk:define-peripheral neuralwatt :not-running t)

(defparameter +default-base-url+ "https://api.neuralwatt.com"
  "Where the NeuralWatt API answers unless the section names another host.")

(defparameter +page-size+ 200
  "The most request rows one page returns, as the original asked for.")

(defparameter +max-pages+ 20
  "The most pages one scan asks for, so a large account cannot pin a call.")

(defparameter +usage-actions+ '("summary" "by_model" "by_key" "compare")
  "The usage views: three aggregates the API answers and one this cell computes.")

(defparameter +request-actions+ '("list" "top_expensive")
  "The request views: one page of rows, or the costliest rows of a window.")

(defparameter +session-actions+ '("list" "detail" "families")
  "The session views: newest sessions, one session's turns, sub-agent fan-outs.")

(defparameter +views+
  '(("health" . health) ("models" . model-catalog) ("prices" . prices)
    ("quota" . quota) ("balance" . quota) ("dashboard" . dashboard)
    ("today" . dashboard) ("energy" . energy) ("carbon" . energy)
    ("usage" . usage-summary) ("requests" . requests-list)
    ("sessions" . sessions-list))
  "The words /neuralwatt takes, each with the view it answers.")

(defvar *http* #'nlk:http
  "The one seam that leaves this image: an HTTP exchange. Tests stub it.")

;;; --- what the section says --------------------------------------------------------

(defun nonblank (value)
  "VALUE as a trimmed string when it is one with a character in it, else NIL."
  (let ((text (and value (princ-to-string value))))
    (when (and text (stringp text))
      (let ((text (nlk:trimmed text)))
        (and (plusp (length text)) text)))))

(defun base-url ()
  (string-right-trim "/" (or (nonblank (setting :base-url)) +default-base-url+)))

(defun api-key ()
  "The account's key: the one pasted into the section, else the variable
api_key_env names, else NIL."
  (or (nonblank (setting :api-key))
      (let ((name (nonblank (setting :api-key-env))))
        (and name (nonblank (uiop:getenv name))))))

(defun read-settings (values table)
  "The declared VALUES, with the key and the base URL read here, once: the
inline key, else the variable api_key_env names, else a refusal naming what is
missing. The base URL must be https, since the key goes wherever it points."
  (declare (ignore table))
  (let* ((plist (copy-list values))
         (env (nonblank (getf values :api-key-env)))
         (key (or (nonblank (getf values :api-key))
                  (and env (nonblank (uiop:getenv env)))))
         (base (string-right-trim "/" (or (nonblank (getf values :base-url))
                                           +default-base-url+))))
    (unless key
      (nlk:config-error "no NeuralWatt API key: paste one into api_key, or name the variable ~
holding it in api_key_env~@[ (~a is unset or empty)~]. The key is made in the NeuralWatt ~
dashboard, and every view this cell reads needs it" env))
    (unless (uiop:string-prefix-p "https://" base)
      (nlk:config-error "base_url must be an https:// URL: the account's key goes wherever it ~
points, so a plain HTTP host would carry it in the clear"))
    (setf (getf plist :api-key) key
          (getf plist :base-url) base)
    plist))

;;; --- the exchange -----------------------------------------------------------------

(defun url-encode (value)
  "VALUE as one query value: everything but the unreserved characters escaped."
  (with-output-to-string (out)
    (loop for char across (princ-to-string value)
          do (if (or (alphanumericp char) (find char "-_.~" :test #'char=))
                 (write-char char out)
                 (format out "%~2,'0X" (char-code char))))))

(defun query-string (params)
  "PATH's query from PARAMS, an alist of (NAME . VALUE): each value encoded,
and a NIL or empty value left out, as the original sent them."
  (let ((pairs (loop for (name . value) in params
                     for text = (and value (princ-to-string value))
                     when (and text (plusp (length text)))
                       collect (format nil "~a=~a" name (url-encode text)))))
    (if pairs (format nil "?~{~a~^&~}" pairs) "")))

(defun headers ()
  "The headers every exchange carries, with the account's key as a bearer
token when the section holds one."
  (let ((key (api-key)))
    (append (list (cons "accept" "application/json")
                  (cons "user-agent" "nodecode-neuralwatt/1.0"))
            (and key (list (cons "authorization" (format nil "Bearer ~a" key)))))))

(defun excerpt (body)
  "BODY's first 300 characters on one line, for a refusal that says what the
API answered."
  (let ((text (and (stringp body) (plusp (length body))
                   (substitute #\Space #\Return (substitute #\Space #\Newline body)))))
    (when text
      (string-right-trim " " (subseq text 0 (min 300 (length text)))))))

(defun decode-body (body)
  "BODY as one decoded JSON object, or NIL: data a peer produced, read leniently."
  (let ((text (and (stringp body) (plusp (length body)))))
    (when text
      (let ((value (ignore-errors (nlk:decode-json body :whole t))))
        (and (hash-table-p value) value)))))

(defun request (path &optional params)
  "GET PATH on the NeuralWatt API => the decoded object. A status of 400 or
worse, and a body that is not one JSON object, is a refusal naming the
endpoint and what the API said of it."
  (let ((url (concatenate 'string (base-url) path (query-string params))))
    (multiple-value-bind (body status)
        (funcall *http* :get url :headers (headers)
                 :timeout (setting :timeout-seconds))
      (when (and (integerp status) (>= status 400))
        (fail "~a answered HTTP ~a~@[~a~]~@[ -- ~a~]"
              path status
              (cond ((= status 401) " (the key is unset, expired, or another workspace's)")
                    ((= status 404) " (with a valid key: an endpoint this account does not have)")
                    (t nil))
              (excerpt body)))
      (or (decode-body body)
          (fail "~a answered no JSON object (HTTP ~a)~@[ -- ~a~]" path status (excerpt body))))))

(defun fetch (path &optional params)
  "REQUEST PATH, answering (values OBJECT ERROR-TEXT): a refusal is the
error text rather than a condition, so one part of a view can fail alone."
  (handler-case (values (request path params) nil)
    (neuralwatt-error (condition) (values nil (princ-to-string condition)))))

;;; --- reading what came back -------------------------------------------------------

(defun number-text (value)
  "VALUE as one short number: an integer as it stands, a real at four decimals
with the trailing zeros gone."
  (cond ((not (realp value)) "n/a")
        ((integerp value) (format nil "~d" value))
        (t (let ((text (format nil "~,4f" (coerce value 'double-float))))
             (let ((text (string-right-trim "0" text)))
               (string-right-trim "." text))))))

(defun usd (value)
  "VALUE as dollars, or n/a when the row carried no number."
  (if (realp value) (format nil "$~a" (number-text value)) "n/a"))

(defun scalar-text (value)
  (cond ((or (null value) (eq value :null)) "null")
        ((eq value t) "true")
        ((stringp value) value)
        ((realp value) (number-text value))
        (t (princ-to-string value))))

(defun object-p (value)
  (hash-table-p value))

(defun array-p (value)
  (and (vectorp value) (not (stringp value))))

(defun render-to (out value indent)
  "VALUE as indented lines: an object one member a line, an array one element
a line, a scalar as it stands -- every field the API answered, named."
  (cond
    ((object-p value)
     (let ((keys (sort (loop for key being the hash-keys of value collect key)
                       #'string< :key #'princ-to-string)))
       (if (null keys)
           (format out "~vtx(none)~%" indent)
           (dolist (key keys)
             (let ((member (gethash key value)))
               (cond ((or (object-p member) (array-p member))
                      (format out "~vtx~a:~%" indent (princ-to-string key))
                      (render-to out member (+ indent 2)))
                     (t (format out "~vtx~a: ~a~%" indent (princ-to-string key)
                                (scalar-text member)))))))))
    ((array-p value)
     (if (zerop (length value))
         (format out "~vtx(none)~%" indent)
         (loop for element across value
               do (cond ((or (object-p element) (array-p element))
                         (format out "~vtx-~%" indent)
                         (render-to out element (+ indent 2)))
                        (t (format out "~vtx- ~a~%" indent (scalar-text element)))))))
    (t (format out "~vtx~a~%" indent (scalar-text value)))))

(defun render (value &optional (indent 0))
  "VALUE as the text RENDER-TO writes."
  (with-output-to-string (out) (render-to out value indent)))

(defun field (out label value indent)
  "One line for VALUE under LABEL: inline when it is a scalar, its own block
when it is an object or an array, and nothing at all when it is absent."
  (when value
    (cond ((or (object-p value) (array-p value))
           (format out "~vtx~a:~%" indent label)
           (render-to out value (+ indent 2)))
          (t (format out "~vtx~a: ~a~%" indent label (scalar-text value))))))

(defun number-value (object &rest path)
  "A number at PATH, or NIL: a string that names one reads as one."
  (let ((value (apply #'nlk:json-value object :any path)))
    (cond ((realp value) value)
          ((stringp value) (let ((read (ignore-errors (read-from-string value))))
                             (and (realp read) read)))
          (t nil))))

(defun average (numbers)
  (and numbers (/ (reduce #'+ numbers) (length numbers))))

;;; --- the dates the views ask for --------------------------------------------------

(defun utc-date (&optional (offset 0))
  "Today's date in UTC, OFFSET days away, as YYYY-MM-DD."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (+ (get-universal-time) (* offset 86400)) 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun now-iso ()
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

;;; --- the views -------------------------------------------------------------------

(defun health ()
  "Service health: the basic status and the detailed component checks."
  (with-output-to-string (out)
    (format out "NeuralWatt service health~%")
    (multiple-value-bind (payload error) (fetch "/health")
      (if error
          (format out "  status: ~a~%" error)
          (progn (format out "  status:~%") (render-to out payload 4))))
    (multiple-value-bind (payload error) (fetch "/health/detailed")
      (if error
          (format out "  components: ~a~%" error)
          (progn (format out "  components:~%") (render-to out payload 4))))))

(defun model-catalog ()
  "The model catalog: the public models, and the account's own when the key
reaches them."
  (let ((payload (request "/v1/models")))
    (let ((rows (nlk:json-value payload :array "data")))
      (with-output-to-string (out)
        (format out "NeuralWatt models~@[ (~a)~]~%" (nlk:json-value payload :string "scope"))
        (if rows
            (loop for row across rows
                  for id = (nlk:json-value row :string "id")
                  for name = (nlk:json-value row :string "metadata" "display_name")
                  do (format out "  ~a~@[ -- ~a~]~%" (or id "(unnamed)") name))
            (progn (format out "  no model list in the answer; the whole body:~%")
                   (render-to out payload 4)))))))

(defun pricing-row (model)
  "One catalog entry as a pricing row, or NIL when it carries no pricing."
  (let* ((metadata (nlk:json-value model :object "metadata"))
         (pricing (nlk:json-value metadata :object "pricing"))
         (tbd (nlk:json-value pricing :boolean "pricing_tbd"))
         (input (nlk:json-value pricing :number "input_per_million"))
         (output (nlk:json-value pricing :number "output_per_million"))
         (cached (nlk:json-value pricing :number "cached_input_per_million")))
    (when (or input output cached tbd)
      (list :model (nlk:json-value model :string "id")
            :display (nlk:json-value metadata :string "display_name")
            :input (unless tbd input)
            :output (unless tbd output)
            :cached (unless tbd cached)
            :currency (nlk:json-value pricing :string "currency")
            :tbd (and tbd t)
            :tier (nlk:json-value pricing :string "service_tier")
            :flex (number-value pricing "flex_discount_multiplier")))))

(defun prices ()
  "What a million tokens costs, input, output and cached input, per model."
  (let* ((payload (request "/v1/models"))
         (models (nlk:json-value payload :array "data"))
         (rows (loop for model across (or models #())
                     for row = (pricing-row model)
                     when row collect row)))
    (unless rows
      (fail "the model list carried no metadata.pricing: this account's API does not expose prices"))
    (with-output-to-string (out)
      (format out "NeuralWatt prices: USD per million tokens~%")
      (dolist (row rows)
        (format out "  ~a~@[ -- ~a~]: in ~a, out ~a~@[, cached ~a~]~@[ (~a)~]~@[ -- tier ~a~]~@[ -- tbd~]~@[ -- flex x~a~]~%"
                (or (getf row :model) "(unnamed)") (getf row :display)
                (usd (getf row :input)) (usd (getf row :output))
                (and (getf row :cached) (usd (getf row :cached)))
                (getf row :currency) (getf row :tier) (getf row :tbd) (getf row :flex)))
      (format out "  note: Flex prices are already discounted; do not apply the multiplier again.~%")
      (format out "  ~d model~:p read~%" (length rows)))))

(defun quota ()
  "The account's balance, usage totals, subscription allowance and key limits."
  (let ((payload (request "/v1/quota")))
    (with-output-to-string (out)
      (format out "NeuralWatt quota~%")
      (render-to out payload 2))))

(defun dashboard (&optional (days 7))
  "One check-in: balance and allowance, today's usage, recent energy."
  (unless (and (integerp days) (<= 1 days 30))
    (fail "days must be an integer from 1 to 30, not ~s" days))
  (let ((today (utc-date))
         (now (now-iso))
         (start (utc-date (- 1 days))))
    (with-output-to-string (out)
      (format out "NeuralWatt dashboard~%")
      (format out "account~%")
      (multiple-value-bind (payload error) (fetch "/v1/quota")
        (if error
            (format out "  not read: ~a~%" error)
            (progn
              (format out "  as of: ~a~%" (or (nlk:json-value payload :string "snapshot_at") "now"))
              (field out "balance" (nlk:json-value payload :any "balance") 2)
              (field out "subscription" (nlk:json-value payload :any "subscription") 2)
              (field out "key allowance" (nlk:json-value payload :any "key" "allowance") 2)
              (field out "rate-limit tier" (nlk:json-value payload :any "limits" "rate_limit_tier") 2))))
      (format out "today (~a)~%" today)
      (multiple-value-bind (payload error)
          (fetch "/v1/usage/summary" (list (cons "start_date" today) (cons "end_date" now)))
        (if error
            (format out "  not read: ~a~%" error)
            (render-to out payload 2)))
      (format out "recent energy (~a to ~a)~%" start today)
      (multiple-value-bind (payload error)
          (fetch "/v1/usage/energy" (list (cons "start_date" start) (cons "end_date" today)))
        (if error
            (format out "  not read: ~a~%" error)
            (render-to out payload 2))))))

(defun energy (&key start-date end-date)
  "Energy and carbon of a window, with its daily breakdown (default 30 days)."
  (let ((payload (request "/v1/usage/energy"
                          (list (cons "start_date" (or (nonblank start-date) (utc-date -29)))
                                (cons "end_date" (or (nonblank end-date) (utc-date)))))))
    (with-output-to-string (out)
      (format out "NeuralWatt energy~%")
      (render-to out payload 2))))

(defun check-paging (limit max-pages)
  "The bounds the original put on a scan, refused in its words."
  (unless (and (integerp limit) (<= 1 limit 200))
    (fail "limit must be an integer from 1 to 200, not ~s" limit))
  (unless (and (integerp max-pages) (<= 1 max-pages +max-pages+))
    (fail "max_pages must be an integer from 1 to ~a, not ~s" +max-pages+ max-pages))
  t)

(defun scan-requests (start-date end-date model max-pages)
  "The request rows of a window, a page at a time => a plist: :rows (oldest
page first), :period, :accounting-method, :pages, :complete, :next-cursor and
an :error when the walk stopped early."
  (let ((rows '()) (cursor "") (pages 0) (period nil) (accounting nil))
    (labels ((status (complete error)
               (list :rows (nreverse rows) :period period :accounting-method accounting
                     :pages pages :complete (and complete t)
                     :next-cursor (and (plusp (length cursor)) cursor) :error error)))
      (block scan
        (loop repeat max-pages
              do (multiple-value-bind (payload error)
                     (fetch "/v1/usage/requests"
                            (list (cons "start_date" start-date) (cons "end_date" end-date)
                                  (cons "model" model) (cons "limit" +page-size+)
                                  (cons "cursor" cursor)))
                   (when (null payload)
                     (return-from scan (status nil error)))
                   (let ((page (nlk:json-value payload :array "requests")))
                     (unless page
                       (return-from scan
                         (status nil "the requests answer carried no requests array")))
                     (loop for row across page when (hash-table-p row) do (push row rows))
                     (incf pages)
                     (when (null period)
                       (setf period (nlk:json-value payload :string "period")
                             accounting (nlk:json-value payload :string "accounting_method")))
                     (let ((next (nlk:json-value payload :string "next_cursor")))
                       (if (and next (plusp (length next)))
                           (setf cursor next)
                           (return-from scan (status t nil)))))))
        (status nil nil)))))

(defun scan-header (out scan subject)
  (format out "~a: ~d request~:p over ~d page~:p~a~%"
          subject (length (getf scan :rows)) (getf scan :pages)
          (if (getf scan :complete)
              ", the window complete"
              ", INCOMPLETE -- results cover only the pages scanned"))
  (when (getf scan :period) (format out "  period: ~a~%" (getf scan :period)))
  (when (getf scan :accounting-method)
    (format out "  accounting: ~a~%" (getf scan :accounting-method)))
  (when (getf scan :next-cursor)
    (format out "  next cursor: ~a~%" (getf scan :next-cursor)))
  (when (getf scan :error) (format out "  stopped: ~a~%" (getf scan :error))))

(defun compare-rows (scan)
  "The scanned rows grouped by requested model, costliest first."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (row (getf scan :rows))
      (let* ((requested (or (nlk:json-value row :string "requested_model")
                             (nlk:json-value row :string "model") "unknown"))
             (group (or (gethash requested groups)
                        (setf (gethash requested groups)
                              (list :requests 0 :costs '() :durations '() :ttfts '()
                                    :tokens '() :served '()))))
             (served (nlk:json-value row :string "model")))
        (incf (getf group :requests))
        (let ((cost (number-value row "cost_usd")))
          (when cost (push cost (getf group :costs))))
        (let ((duration (number-value row "duration")))
          (when duration (push duration (getf group :durations))))
        (let ((ttft (number-value row "ttft")))
          (when ttft (push ttft (getf group :ttfts))))
        (let ((tokens (number-value row "total_tokens")))
          (when tokens (push tokens (getf group :tokens))))
        (when served (pushnew served (getf group :served) :test #'equal))))
    (let ((rows '()))
      (maphash (lambda (name group)
                 (let ((total (and (getf group :costs) (reduce #'+ (getf group :costs)))))
                   (push (list :model name
                               :requests (getf group :requests)
                               :total total
                               :average (and total (average (getf group :costs)))
                               :counted (length (getf group :costs))
                               :tokens (reduce #'+ (getf group :tokens))
                               :duration (average (getf group :durations))
                               :ttft (average (getf group :ttfts))
                               :served (sort (copy-list (getf group :served)) #'string<))
                         rows)))
               groups)
      (sort rows (lambda (a b)
                   (let ((left (getf a :total)) (right (getf b :total)))
                     (cond ((and left right) (> left right))
                           (left t)
                           (t nil))))))))

(defun usage-view (action &key start-date end-date (limit 20) (max-pages 10))
  "A usage breakdown: summary, by_model, by_key, or a comparison this cell
computes from the request rows."
  (let ((action (or (nonblank action) "summary")))
    (cond
      ((equal action "compare")
       (check-paging limit max-pages)
       (let* ((scan (scan-requests start-date end-date "" max-pages))
              (rows (compare-rows scan))
              (shown (subseq rows 0 (min limit (length rows)))))
         (with-output-to-string (out)
           (scan-header out scan "NeuralWatt usage compare")
           (format out "  ~26a~5a~12a~11a~9a~10a~11a  ~a~%"
                   "model" "rows" "total cost" "$/row" "avg s" "ttft s" "tokens" "served")
           (dolist (row shown)
             (format out "  ~26a~5d~12a~11a~9a~10a~11d  ~a~%"
                     (getf row :model)
                     (getf row :requests) (usd (getf row :total)) (usd (getf row :average))
                     (number-text (getf row :duration)) (number-text (getf row :ttft))
                     (or (getf row :tokens) 0)
                     (if (getf row :served)
                         (format nil "~{~a~^,~}" (getf row :served))
                         "-")))
           (when (> (length rows) (length shown))
             (format out "  ~d more model~:p not shown (limit ~d)~%"
                     (- (length rows) (length shown)) limit))
           (when (null rows) (format out "  no request rows were scanned~%")))))
      ((member action '("summary" "by_model" "by_key") :test #'equal)
       (let ((path (cdr (assoc action '(("summary" . "/v1/usage/summary")
                                        ("by_model" . "/v1/usage/by-model")
                                        ("by_key" . "/v1/usage/by-key"))
                          :test #'equal))))
         (let ((payload (request path (list (cons "start_date" start-date)
                                            (cons "end_date" end-date)
                                            (cons "limit" limit)))))
           (with-output-to-string (out)
             (format out "NeuralWatt usage: ~a~%" action)
             (render-to out payload 2)))))
      (t (fail "~a is not a usage view: ~{~a~^, ~}" action +usage-actions+)))))

(defun request-line (row)
  "One request row on one line: its cost, its models, its tokens, its
latency and when it ran, for whichever of those the row carries."
  (let ((fields
          (remove nil
                  (list (let ((cost (number-value row "cost_usd")))
                          (and cost (format nil "cost ~a" (usd cost))))
                        (let ((model (or (nlk:json-value row :string "requested_model")
                                         (nlk:json-value row :string "model"))))
                          (and model (format nil "model ~a" model)))
                        (let ((served (nlk:json-value row :string "model")))
                          (and served
                               (let ((requested (nlk:json-value row :string "requested_model")))
                                 (and requested (not (equal requested served))
                                      (format nil "served by ~a" served)))))
                        (let ((tokens (number-value row "total_tokens")))
                          (and tokens (format nil "~d tokens" (round tokens))))
                        (let ((duration (number-value row "duration")))
                          (and duration (format nil "~as" (number-text duration))))
                        (let ((ttft (number-value row "ttft")))
                          (and ttft (format nil "ttft ~as" (number-text ttft))))
                        (let ((session (nlk:json-value row :string "session_id")))
                          (and session (format nil "session ~a" session)))
                        (let ((at (or (nlk:json-value row :string "created_at")
                                      (nlk:json-value row :string "started_at")
                                      (nlk:json-value row :string "timestamp"))))
                          (and at (princ-to-string at)))))))
    (if fields
        (format nil "~{~a~^ -- ~}" fields)
        (nlk:trimmed (render row 6)))))

(defun requests-view (action &key start-date end-date model limit cursor max-pages)
  "A page of request rows, or the costliest rows of a window."
  (let ((action (or (nonblank action) "list")))
    (cond
      ((equal action "list")
       (let ((payload (request "/v1/usage/requests"
                               (list (cons "start_date" start-date) (cons "end_date" end-date)
                                     (cons "model" model) (cons "limit" (or limit 20))
                                     (cons "cursor" cursor)))))
         (with-output-to-string (out)
           (format out "NeuralWatt requests~@[ (~a)~]~%" (nlk:json-value payload :string "period"))
           (render-to out payload 2))))
      ((equal action "top_expensive")
       (check-paging (or limit 20) (or max-pages 10))
       (let ((scan (scan-requests start-date end-date model (or max-pages 10))))
         (let ((priced (remove-if-not (lambda (row) (number-value row "cost_usd"))
                                       (getf scan :rows))))
           (setf priced (sort priced (lambda (a b)
                                       (> (number-value a "cost_usd")
                                          (number-value b "cost_usd")))))
           (with-output-to-string (out)
             (scan-header out scan "NeuralWatt costliest requests")
             (format out "  ~d request~:p carried a cost~%" (length priced))
             (loop for row in (subseq priced 0 (min (or limit 20) (length priced)))
                   for index from 1
                   do (format out "  ~d. ~a~%" index (request-line row)))))))
      (t (fail "~a is not a request view: ~{~a~^, ~}" action +request-actions)))))

(defun sessions-view (action &key session-id start-date end-date model limit offset)
  "Newest sessions, one session's turns, or the sub-agent fan-outs."
  (let ((action (or (nonblank action) "list")))
    (cond
      ((equal action "detail")
       (let ((id (nonblank session-id)))
         (unless id
           (fail "session_id is required for the detail view: name the session to read"))
         (let ((payload (request (concatenate 'string "/v1/usage/sessions/" (url-encode id))
                                 (list (cons "start_date" start-date) (cons "end_date" end-date)
                                       (cons "limit" (or limit 10))))))
           (with-output-to-string (out)
             (format out "NeuralWatt session ~a~%" id)
             (render-to out payload 2)))))
      ((member action '("list" "families") :test #'equal)
       (let ((path (if (equal action "families")
                       "/v1/usage/sessions/families"
                       "/v1/usage/sessions"))
             (payload nil))
         (setf payload (request path
                                (append (list (cons "start_date" start-date)
                                              (cons "end_date" end-date)
                                              (cons "model" model)
                                              (cons "limit" (or limit 10)))
                                        (and (equal action "list")
                                             (list (cons "offset" (or offset 0)))))))
         (with-output-to-string (out)
           (format out "NeuralWatt sessions: ~a~%" action)
           (render-to out payload 2))))
      (t (fail "~a is not a session view: ~{~a~^, ~}" action +session-actions)))))

(defun usage-summary () (usage-view "summary"))

(defun requests-list () (requests-view "list"))

(defun sessions-list () (sessions-view "list"))

;;; --- one word of the command ------------------------------------------------------

(defun command-view (args)
  "The /neuralwatt argument as a view: a word from +VIEWS+, or the dashboard
when none is typed."
  (let ((word (string-downcase (or (first-word args) "dashboard"))))
    (let ((view (cdr (assoc word +views+ :test #'equal))))
      (if view
          (funcall view)
          (fail "~a is not a view this cell reads: ~{~a~^, ~}" word (mapcar #'car +views+))))))

(defun first-word (text)
  (let ((text (nonblank text)))
    (and text (first (remove "" (uiop:split-string text :separator '(#\Space #\Tab))
                             :test #'equal)))))

;;; --- the tools -------------------------------------------------------------------

(defun guarded (body)
  "Run BODY, answering its text; a refusal becomes the tool's own failure line,
so the model reads what went wrong instead of a broken call."
  (handler-case (funcall body)
    (neuralwatt-error (condition)
      (nle:failure (format nil "ERROR: NEURALWATT-ERROR: ~a" condition)))
    (error (condition)
      (nle:failure (format nil "ERROR: NEURALWATT-ERROR: ~a" condition)))))

(defun no-argument-schema (description)
  "A tool that takes no argument: the plist REGISTER-TOOL reads by hand."
  (list :description description
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object))))

(defun health-tool (arguments)
  "The neuralwatt_health tool."
  (declare (ignore arguments))
  (guarded #'health))

(defun models-tool (arguments)
  "The neuralwatt_models tool."
  (declare (ignore arguments))
  (guarded #'model-catalog))

(defun prices-tool (arguments)
  "The neuralwatt_prices tool."
  (declare (ignore arguments))
  (guarded #'prices))

(defun quota-tool (arguments)
  "The neuralwatt_quota tool."
  (declare (ignore arguments))
  (guarded #'quota))

(defun dashboard-tool (call)
  "The neuralwatt_dashboard tool."
  (guarded (lambda () (dashboard (or (dashboard-call-days call) 7)))))

(defun energy-tool (call)
  "The neuralwatt_energy tool."
  (guarded (lambda () (energy :start-date (energy-call-start-date call)
                               :end-date (energy-call-end-date call)))))

(defun usage-tool (call)
  "The neuralwatt_usage tool."
  (guarded (lambda () (usage-view (usage-call-action call)
                                   :start-date (usage-call-start-date call)
                                   :end-date (usage-call-end-date call)
                                   :limit (or (usage-call-limit call) 20)
                                   :max-pages (or (usage-call-max-pages call) 10)))))

(defun requests-tool (call)
  "The neuralwatt_requests tool."
  (guarded (lambda () (requests-view (requests-call-action call)
                                      :start-date (requests-call-start-date call)
                                      :end-date (requests-call-end-date call)
                                      :model (requests-call-model call)
                                      :limit (or (requests-call-limit call) 20)
                                      :cursor (requests-call-cursor call)
                                      :max-pages (or (requests-call-max-pages call) 10)))))

(defun sessions-tool (call)
  "The neuralwatt_sessions tool."
  (guarded (lambda () (sessions-view (sessions-call-action call)
                                      :session-id (sessions-call-session-id call)
                                      :start-date (sessions-call-start-date call)
                                      :end-date (sessions-call-end-date call)
                                      :model (sessions-call-model call)
                                      :limit (or (sessions-call-limit call) 10)
                                      :offset (or (sessions-call-offset call) 0)))))

;;; --- the boundaries ---------------------------------------------------------------

(nlk:define-contract dashboard-call
  (:doc "NeuralWatt check-in: account balance and allowance, today's cost and token totals, and the last days of energy with its daily breakdown, from the account's own endpoints.")
  ("days" :integer :default 7
          :doc "days in the recent-energy window, today included (1-30, default 7)"))

(nlk:define-contract energy-call
  (:doc "NeuralWatt energy usage: kWh, joules and carbon, with a per-day breakdown. Dates are YYYY-MM-DD; the default window is the last 30 days.")
  ("start_date" :text :doc "window start, YYYY-MM-DD (default 29 days ago)")
  ("end_date" :text :doc "window end, YYYY-MM-DD (default today)"))

(nlk:define-contract usage-call
  (:doc "NeuralWatt usage: a summary, a breakdown by model or by key, or a comparison of cost and latency across the models asked for, computed from the per-request rows.")
  ("action" :text :default "summary" :options +usage-actions+
            :doc "which breakdown: summary, by_model, by_key or compare")
  ("start_date" :text :doc "window start: YYYY-MM-DD for the aggregates, ISO 8601 for compare (default 7 days ago)")
  ("end_date" :text :doc "window end: YYYY-MM-DD for the aggregates, ISO 8601 for compare (default now)")
  ("limit" :integer :default 20 :doc "rows or models to show (1-200 for compare)")
  ("max_pages" :integer :default 10 :doc "200-row pages to scan for compare (1-20)"))

(nlk:define-contract requests-call
  (:doc "NeuralWatt request rows, newest first, or the costliest requests of a window. Content-free: no prompt or completion text. The list view pages with the cursor the last answer carried.")
  ("action" :text :default "list" :options +request-actions+
            :doc "list newest rows, or top_expensive to rank them by cost")
  ("start_date" :text :doc "window start, ISO 8601 (default 7 days ago)")
  ("end_date" :text :doc "window end, ISO 8601, exclusive (default now)")
  ("model" :text :doc "filter to one model, requested or served (exact match)")
  ("limit" :integer :default 20 :doc "page size, or how many top rows to show (1-200)")
  ("cursor" :text :doc "the next_cursor a previous list answer carried")
  ("max_pages" :integer :default 10 :doc "pages to scan for top_expensive (1-20)"))

(nlk:define-contract sessions-call
  (:doc "NeuralWatt session analytics: the newest sessions with their cost, energy and cache figures; one session's per-turn timeline; or the sub-agent fan-outs.")
  ("action" :text :default "list" :options +session-actions+
            :doc "list, detail (needs session_id) or families")
  ("session_id" :text :doc "the session to read, for action detail")
  ("start_date" :text :doc "window start, ISO 8601 (default 7 days ago)")
  ("end_date" :text :doc "window end, ISO 8601 (default now)")
  ("model" :text :doc "filter to one model or alias (exact match on the requested model)")
  ("limit" :integer :default 10 :doc "rows to return")
  ("offset" :integer :default 0 :doc "paging offset for list: limit plus offset reads on"))

;;; --- the words the model and the operator read ------------------------------------

(defparameter +primer+
  "NeuralWatt's read-only analytics are available through the nodecode-neuralwatt
cell: service health, the model catalog and its prices, the account's balance and
limits, today's spend, energy and carbon, usage breakdowns, request rows and
session analytics. Every view is one or more GETs to the NeuralWatt API with the
account's own key; nothing is written anywhere, and no prompt or completion
text is stored by that API.
  neuralwatt_health      status, and the detailed component checks
  neuralwatt_models      the catalog: public models, and the account's own
  neuralwatt_prices      input, output and cached input, USD per million tokens
  neuralwatt_quota       balance, allowance, usage totals, rate-limit tier
  neuralwatt_dashboard   balance, allowance, today's spend, recent energy (days)
  neuralwatt_energy      kWh, joules and carbon with a daily breakdown
  neuralwatt_usage       summary | by_model | by_key | compare (cost and
      latency by requested model, scanned a page at a time; it says when the
      window is only the pages it read)
  neuralwatt_requests    list | top_expensive
  neuralwatt_sessions    list | detail (session_id) | families
  /neuralwatt WORD       the same views in the shell: health, models, prices,
      quota, dashboard, energy, usage, requests, sessions; no word is the
      dashboard.
  (neuralwatt:dashboard), (neuralwatt:prices), (neuralwatt:usage-view \"compare\")
      and the rest are the same text through eval.
The key is the neuralwatt section's api_key, else the variable api_key_env
names; base_url points at the API unless a gateway of your own stands before
it. A refusal is ERROR: NEURALWATT-ERROR and names what to set."
  "What (help :neuralwatt) answers while the cell runs.")

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell neuralwatt
  (:section ("neuralwatt")
    (:guide "paste a NeuralWatt API key into api_key, or name the variable holding it in api_key_env (NEURALWATT_API_KEY is the usual name); every view reads with it, from the account's own endpoints; base_url points at api.neuralwatt.com unless a gateway of your own stands in front of it, and that host then receives the key")
    ("api_key" :secret
               :doc "the NeuralWatt API key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "NEURALWATT_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("base_url" :string :default "https://api.neuralwatt.com"
                :doc "where the NeuralWatt API answers; change it only for a gateway of your own")
    ("timeout_seconds" :integer :default 30 :min 1
                       :doc "seconds one request may take"))
  (:settings #'read-settings)
  (:tool "neuralwatt_health"
         (no-argument-schema "NeuralWatt service health: the gateway's own status and the detailed component checks (database, cache, uptime). Use it to answer whether the provider is having API problems.")
         #'health-tool)
  (:tool "neuralwatt_models"
         (no-argument-schema "List the models available on the NeuralWatt API: the public catalog, and the account's own models too when the key reaches them.")
         #'models-tool)
  (:tool "neuralwatt_prices"
         (no-argument-schema "The NeuralWatt price table, from the catalog: input, output and cached input USD per million tokens. Flex prices are already discounted.")
         #'prices-tool)
  (:tool "neuralwatt_quota"
         (no-argument-schema "The NeuralWatt account's balance, usage totals, subscription allowance, rate-limit tier and key limits, as the quota endpoint answers them.")
         #'quota-tool)
  (:tool "neuralwatt_dashboard" (nlk:contract 'dashboard-call) #'dashboard-tool)
  (:tool "neuralwatt_energy" (nlk:contract 'energy-call) #'energy-tool)
  (:tool "neuralwatt_usage" (nlk:contract 'usage-call) #'usage-tool)
  (:tool "neuralwatt_requests" (nlk:contract 'requests-call) #'requests-tool)
  (:tool "neuralwatt_sessions" (nlk:contract 'sessions-call) #'sessions-tool)
  (:command "neuralwatt" 'run-command
            :description "Your NeuralWatt account: health, prices, balance, spend, energy, usage"
            :argument-hint "[health|models|prices|quota|dashboard|energy|usage|requests|sessions]")
  (:help :neuralwatt
         "neuralwatt: health, prices, balance, spend, energy, usage, requests and sessions - nine tools, /neuralwatt, (neuralwatt:dashboard)"
         +primer+))

(defun run-command (args session-id)
  "The /neuralwatt command: its argument names a view, and no argument is the
dashboard."
  (declare (ignore session-id))
  (guarded (lambda () (command-view args))))
