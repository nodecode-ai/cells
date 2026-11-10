;;;; api.lisp --- the fixed-origin VibeLeak client: prepare, start, read, compare.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One origin and nothing else (the `origin' setting, www.vibeleak.app by
;;;; default), one owner key out of the cell's own section, and three calls:
;;;; POST /api/v1/scans for an assessment the operator asked for, GET
;;;; /api/v1/scans/<id> for its evidence, GET /api/v1/reports/<id>/markdown
;;;; for the saved report. Nothing is written to disk, nothing is executed,
;;;; and a scan runs only when a tool call names the host and says so.

(in-package #:nodecode-vibeleak)

(defparameter +user-agent+ "nodecode-vibeleak/0.1.0 (+https://github.com/nodecode-ai/cells)"
  "Who the API sees asking: the cell, and where it comes from.")

(defparameter +max-response-bytes+ (* 2 1024 1024)
  "The most one answer may be. A peer that sends more is cut, not waited on.")

(defparameter +id-scanner+
  (cl-ppcre:create-scanner
   "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$")
  "A saved scan's identifier: the API's own UUID shape, and nothing else.")

;;; --- the one network call ----------------------------------------------------

(defun http (method url &key headers content timeout)
  "The cell's one network call: NLK:HTTP, the body as text under the byte
cap. => (values TEXT STATUS). Tests stub this name and never dial out."
  (multiple-value-bind (body status)
      (nlk:http method url :headers headers :content content :timeout (or timeout 30)
                     :max-bytes +max-response-bytes+)
    (values (cond ((stringp body) body)
                  ((streamp body) (ignore-errors (nlk:decode-octets body)))
                  (t (ignore-errors (nlk:decode-octets body))))
            status)))

(defun endpoint (path)
  (format nil "~a~a" (string-right-trim "/" (or (setting :origin) "https://www.vibeleak.app")) path))

(defun decode-answer (text what)
  "TEXT as a decoded JSON object, or a refusal naming WHAT answered
something unreadable."
  (let ((json (ignore-errors (nlk:decode-json text))))
    (unless (hash-table-p json)
      (fail "~a answered something that is not JSON" what))
    json))

;;; --- the hostname the API takes ----------------------------------------------

(defun normalize-domain (value)
  "VALUE as the public hostname the API takes: no scheme, no credentials,
no port, no trailing dot, lower case. A literal IP or a reserved suffix is
refused, because an assessment of one is not an assessment of a website."
  (let* ((raw (string-trim '(#\Space #\Tab) (or value "")))
         (text (or (multiple-value-bind (match groups)
                       (cl-ppcre:scan-to-strings "^[a-zA-Z][a-zA-Z0-9+.-]*://([^/?#]+)" raw)
                     (declare (ignore match))
                     (and groups (aref groups 0)))
                   raw)))
    (when (zerop (length text)) (fail "name the website to assess"))
    (when (cl-ppcre:scan "[@\\\\\\s]" text) (fail "~a is not a bare hostname" text))
    (when (cl-ppcre:scan ":[0-9]+$" text) (fail "custom ports are not part of an assessment"))
    (let ((host (string-downcase (string-right-trim "." text))))
      (when (cl-ppcre:scan "^www\\." host) (setf host (subseq host 4)))
      (when (cl-ppcre:scan "^[0-9.]+$" host) (fail "~a is an IP address, not a website" value))
      (when (or (< (length host) 3) (not (cl-ppcre:scan "\\." host))
                (cl-ppcre:scan "\\.(?:local|localhost|internal|test|invalid|example|onion)$" host))
        (fail "~a is not a public hostname" value))
      (unless (cl-ppcre:scan "^[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?(?:\\.[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?)+$" host)
        (fail "~a has an invalid label" value))
      host)))

(defun new-id ()
  "A version-4 UUID, as text: the idempotency key a prepared assessment
carries, so a retry reuses one approved request rather than starting another."
  (let ((hex (lambda (digits) (format nil (format nil "~~~d,'0x" digits)
                                      (random (expt 16 digits))))))
    (format nil "~a-~a-4~a-~a-~a"
            (funcall hex 8) (funcall hex 4) (funcall hex 3)
            (format nil "~(~3,'0x~)" (logior #x8000 (random #x1000)))
            (funcall hex 12))))

;;; --- the three calls ----------------------------------------------------------

(defun api-call (method path &key key content)
  "METHOD PATH against the fixed origin => (values BODY STATUS). The key,
when there is one, travels in the Authorization header and nowhere else."
  (http method (endpoint path)
        :headers (append (list (cons "user-agent" +user-agent+)
                               (cons "accept" "application/json"))
                         (when key (list (cons "authorization" (concatenate 'string "Bearer " key)))))
        :content content
        :timeout (setting :timeout-seconds)))

(defun scan-plist (raw)
  "One saved scan as the few values a report shows: identity, grade, the
findings worst first, and the coverage that says how much of the rubric ran."
  (let ((scan (or (nlk:json-value raw :object "data" "scan")
                  (nlk:json-value raw :object "scan")
                  raw)))
    (unless (hash-table-p scan) (fail "the answer carried no scan"))
    (list :domain (or (nlk:json-value scan :string "domain")
                      (nlk:json-value scan :string "url")
                      "")
          :grade (nlk:json-value scan :string "grade")
          :score (nlk:json-value scan :number "score")
          :scan-id (nlk:json-value scan :string "id")
          :failed (and (nlk:json-value scan :boolean "scanFailed") t)
          :coverage (scan-coverage scan)
          :findings (scan-findings scan))))

(defun scan-coverage (scan)
  "The coverage block when the report carries one, else NIL: a check that
did not run is never reported as passing."
  (let ((coverage (nlk:json-value scan :object "coverage")))
    (when (hash-table-p coverage)
      (list :status (nlk:json-value coverage :string "status")
            :completed (coerce (or (nlk:json-array coverage "completedModules") #()) 'list)
            :failed (coerce (or (nlk:json-array coverage "failedModules") #()) 'list)))))

(defun scan-findings (scan)
  "The scan's findings, worst first, each one bounded and with its evidence."
  (let ((rows (nlk:json-array scan "findings")))
    (sort (loop for row across (or rows #())
                when (hash-table-p row)
                  collect (list :title (truncate-text (nlk:json-value row :string "title") 300)
                                :description (truncate-text (nlk:json-value row :string "description") 2000)
                                :evidence (truncate-text (nlk:json-value row :string "evidence") 3000)
                                :remediation (truncate-text (nlk:json-value row :string "remediation") 4000)
                                :module (truncate-text (nlk:json-value row :string "module") 80)
                                :category (truncate-text (nlk:json-value row :string "category") 80)
                                :severity (scan-severity (nlk:json-value row :string "severity"))))
          (lambda (left right)
            (< (severity-rank (getf left :severity)) (severity-rank (getf right :severity)))))))

(defun scan-severity (text)
  (let ((severity (and text (intern (string-upcase text) :keyword))))
    (if (member severity '(:critical :high :medium :low :info)) severity :info)))

(defun truncate-text (text limit)
  (and (stringp text) (subseq text 0 (min limit (length text)))))

(defun start-scan (domain &key project key)
  "Ask VibeLeak for one assessment of DOMAIN, under one scan unit. KEY is the
owner key; PROJECT is an optional label kept beside the request. => a scan
plist, with :state :started or :completed."
  (declare (ignore project))
  (let* ((host (normalize-domain domain))
         (id (new-id)))
    (multiple-value-bind (text status)
        (api-call :post "/api/v1/scans" :key key
                  :content (nlk:encode-json-object
                            (nlk:make-json-object "url" (format nil "https://~a" host)
                                                   "maxUnits" 1
                                                   "idempotencyKey" id)))
      (unless (eql status 201)
        (fail "VibeLeak answered HTTP ~a for ~a~@[ (~a)~]" status host
              (case status (401 "check the owner key") (403 "the key lacks a scope or entitlement")
                (429 "rate limited; retry later") (t nil))))
      (let* ((json (decode-answer text "VibeLeak"))
             (ok (nlk:json-value json :boolean "ok")))
        (unless ok (fail "VibeLeak did not accept the request"))
        (let ((scan (scan-plist json)))
          (list :state "completed" :request-id id :domain host :scan scan
                :units 1 :message "One approved assessment of one host, under one scan unit."))))))

(defun get-scan (scan-id &key key)
  "One saved scan's evidence, by the identifier the API issued."
  (unless (and (stringp scan-id) (cl-ppcre:scan +id-scanner+ scan-id))
    (fail "~a is not a saved scan id" scan-id))
  (multiple-value-bind (text status)
      (api-call :get (format nil "/api/v1/scans/~a" scan-id) :key key)
    (unless (eql status 200)
      (fail "VibeLeak answered HTTP ~a for scan ~a~@[ (~a)~]" status scan-id
            (case status (401 "check the owner key") (403 "the key lacks a finding scope")
              (404 "no saved scan with that id") (t nil))))
    (scan-plist (decode-answer text "VibeLeak"))))

(defun report-markdown (scan-id &key key)
  "The saved report for SCAN-ID as the markdown VibeLeak renders."
  (unless (and (stringp scan-id) (cl-ppcre:scan +id-scanner+ scan-id))
    (fail "~a is not a saved scan id" scan-id))
  (multiple-value-bind (text status)
      (api-call :get (format nil "/api/v1/reports/~a/markdown" scan-id) :key key)
    (unless (eql status 200) (fail "VibeLeak answered HTTP ~a for the report of ~a" status scan-id))
    (or text "")))
