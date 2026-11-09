;;;; client.lisp --- the Yandex Disk REST API v1, and nothing above it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's client.py in Lisp: the same endpoints, the same query
;;;; parameters, the same OAuth header, and the same two shapes it handled
;;;; once here rather than in every caller --
;;;;
;;;;   * deferred operations. A bulk endpoint answers 202 with an
;;;;     operations/<id> link instead of doing the work inline; RUN-OPERATION
;;;;     polls it to completion, so a caller always sees a finished operation.
;;;;   * two-step transfers. An upload or a download first asks the API for a
;;;;     single-use URL on another host, then moves the bytes there -- without
;;;;     the OAuth token, which that host neither needs nor should see.
;;;;
;;;; Two things differ, deliberately. An answer is read field by field here
;;;; instead of being handed on as JSON, and a failure signals the cell's
;;;; condition carrying Yandex's own sentence, so a model gets the reason
;;;; rather than an "error" member to parse.
;;;;
;;;; Everything goes out through HTTP, the one seam a test stubs.

(in-package #:nodecode-yandex-disk)

;;; --- where Yandex answers, and how patient this is ---------------------------

(defparameter +api-base+ "https://cloud-api.yandex.net/v1/disk"
  "Where the Yandex Disk REST API answers, as the original named it.")

(defparameter +max-retries+ 3
  "Attempts past the first one for a transient status, as the original allowed.")

(defparameter +retry-statuses+ '(429 500 502 503 504)
  "Statuses worth another attempt: a rate limit, or a server that is briefly unwell.")

(defparameter +poll-interval+ 0.5
  "Seconds between two reads of a deferred operation.")

(defparameter +operation-timeout+ 60
  "How long one deferred operation may take before the call says so.")

(defparameter +detail-chars+ 400
  "How much of a refused answer's body a refusal quotes.")

;;; --- the one way out ---------------------------------------------------------

(defun http (method url &key headers content (timeout (setting :timeout-seconds)) binary max-bytes)
  "One request over the image's own HTTP call => (values BODY STATUS).

A transient status (a rate limit, a server that is briefly unwell) is tried
again, up to +MAX-RETRIES+ times, with the wait doubling from one second --
what the original's _send did. This is the cell's only way to the network, so
a test stubs exactly this function and nothing dials."
  (loop with attempt = 0
        do (handler-case
                   (multiple-value-bind (body status)
                       (nlk:http method url
                                 :headers headers
                                 :content content
                                 :timeout timeout
                                 :connect-timeout (min 10 timeout)
                                 :binary (and binary t)
                                 :max-bytes max-bytes)
                     (if (and (member status +retry-statuses+)
                              (< attempt +max-retries+))
                         (progn (incf attempt) (sleep (min (expt 2 attempt) 8)))
                         (return (values body status))))
             (error (condition)
               (fail "could not reach ~a: ~a"
                     (or (ignore-errors (quri:uri-host (quri:uri url))) url)
                     (nlk:one-line (princ-to-string condition)))))))

(defun url-enc (text)
  "TEXT as one query-string component."
  (quri:url-encode (princ-to-string text) :encoding :utf-8))

(defun query-string (params)
  "PARAMS -- an alist of (name . value) -- as `?a=1&b=2', or the empty
string when nothing is left. A NIL or empty value is dropped rather than
sent as `name=', as the original dropped it."
  (let ((pairs (loop for (name . value) in params
                     when (and value (not (and (stringp value) (zerop (length value)))))
                       collect (format nil "~a=~a" (url-enc name) (url-enc value)))))
    (if pairs (format nil "?~{~a~^&~}" pairs) "")))

(defun api-url (path &optional params)
  "PATH on the section's root, with PARAMS as its query string."
  (concatenate 'string
               (string-right-trim "/" (or (setting :base-url) +api-base+))
               path
               (query-string params)))

(defun auth-headers ()
  "The headers the original sent: the token as OAuth, JSON accepted."
  (list (cons "authorization" (format nil "OAuth ~a" (setting :token)))
        (cons "accept" "application/json")))

(defun failure-text (body status)
  "Yandex's own sentence for a refused call: the English `description' or the
account-language `message' of its {error, description, message} envelope,
else the first clipped line of the body."
  (let* ((payload (ignore-errors (nlk:decode-json (if (stringp body) body ""))) )
         (sentence (or (nlk:json-value payload :string "description")
                       (nlk:json-value payload :string "message")))
         (text (nlk:one-line (or (and (stringp body) body) ""))))
    (format nil "the Yandex Disk API answered HTTP ~a~@[: ~a~]~@[: ~a~]"
            status sentence
            (and (not sentence) (plusp (length text)) (nlk:clip text +detail-chars+)))))

(defun api-call (method path &key params json)
  "One authenticated call => (values BODY STATUS). A status of 400 or worse
signals the cell's condition carrying Yandex's own words."
  (multiple-value-bind (body status)
      (http method (api-url path params)
            :headers (if json
                         (append (auth-headers) (list (cons "content-type" "application/json")))
                         (auth-headers))
            :content json)
    (if (and (integerp status) (< status 400))
        (values body status)
        (fail "~a" (failure-text body status)))))

(defun json-of (body)
  "BODY -- an API answer -- as a hash table, or an empty one."
  (let ((value (and (stringp body)
                    (plusp (length (nlk:trimmed body)))
                    (ignore-errors (nlk:decode-json body)))))
    (if (hash-table-p value) value (make-hash-table :test #'equal))))

(defun api-json (method path &key params json)
  "PATH's answer as a hash table."
  (multiple-value-bind (body _status) (api-call method path :params params :json json)
    (declare (ignore _status))
    (json-of body)))

;;; --- deferred operations ------------------------------------------------------

(defun operation-status (href)
  "The status a deferred operation reports: success, failed, or in progress."
  (multiple-value-bind (body _status) (http :get href :headers (auth-headers))
    (declare (ignore _status))
    (or (nlk:json-value (json-of body) :string "status") "")))

(defun wait-for-operation (href)
  "Poll HREF until the operation succeeds, fails, or the wait runs out."
  (unless (and href (plusp (length href)))
    (return-from wait-for-operation t))
  (let ((deadline (+ (get-internal-real-time)
                     (* +operation-timeout+ internal-time-units-per-second))))
    (loop
      (let ((status (operation-status href)))
        (cond ((string= status "success") (return t))
              ((string= status "failed")
               (fail "Yandex Disk reported the operation as failed."))
              ((>= (get-internal-real-time) deadline)
               (fail "Yandex Disk is still processing the operation; it may finish on its own."))
              (t (sleep +poll-interval+)))))))

(defun api-run (method path &key params)
  "Call an endpoint that may defer: 202 carries the operations/<id> link to
poll, anything else means the work was done inline."
  (multiple-value-bind (body status) (api-call method path :params params)
    (if (eql status 202)
        (wait-for-operation (nlk:json-value (json-of body) :string "href"))
        (values body status))))

;;; --- the disk and its metadata ------------------------------------------------

(defun account-info ()
  "The account's quota and owner, as Yandex reports it."
  (api-json :get "/"))

(defun get-meta (path &key limit offset sort fields)
  "Metadata for a file or folder; a folder embeds its children."
  (api-json :get "/resources"
            :params (list (cons "path" path)
                          (cons "limit" limit)
                          (cons "offset" offset)
                          (cons "sort" sort)
                          (cons "fields" fields))))

(defun search-resources (query &key (limit 20) (offset 0) media-type sort)
  "One page of a server-side search of file names and contents, whole-disk.

Returns files only, ranked by relevance. Yandex gates this endpoint to some
applications rather than to a token scope, so a token without the grant gets
a 403 -- which the search tool turns into the sentence it deserves. The
endpoint ignores any folder scope, so confinement is the caller's job."
  (let ((items (nlk:json-array
                (api-json :get "/resources/search"
                          :params (list (cons "query" query)
                                        (cons "limit" limit)
                                        (cons "offset" offset)
                                        (cons "media_type" media-type)
                                        (cons "sort" sort)))
                "items")))
    (if (vectorp items) (coerce items 'list) '())))

(defun folder-exists-p (path)
  "Whether anything lives at PATH. Only a 404 counts as no: a refused status
other than that arrives as the condition it is."
  (handler-case
      (progn (get-meta path :limit 0 :fields "path") t)
    (yandex-disk-error (condition)
      (let ((text (princ-to-string condition)))
        (if (cl:search "HTTP 404" text) nil (error condition))))))

;;; --- structure ----------------------------------------------------------------

(defun ensure-folder (path)
  "PATH and every missing folder above it; => the folders it made, outermost
first, as the original's _ensure_dir answered them. A folder that is already
there (409) is left alone, and is not one of the made ones."
  (multiple-value-bind (scheme rest) (split-scheme path)
    (let ((segments (remove "" (uiop:split-string rest :separator "/") :test #'string=))
          (created '())
          (prefix ""))
      (dolist (segment segments)
        (setf prefix (concatenate 'string prefix "/" segment))
        (let ((candidate (concatenate 'string scheme prefix)))
          (handler-case
              (progn (api-json :put "/resources" :params (list (cons "path" candidate)))
                     (setf created (append created (list candidate))))
            (yandex-disk-error (condition)
              (unless (cl:search "HTTP 409" (princ-to-string condition))
                (error condition))))))
      created)))
(defun parent-of (path)
  "PATH with its last segment dropped: /a/b.txt answers /a, /a answers /."
  (multiple-value-bind (scheme rest) (split-scheme path)
    (let ((slash (position #\/ rest :from-end t)))
      (format nil "~a~a" scheme (if (and slash (plusp slash)) (subseq rest 0 slash) "/")))))

(defun transfer-resource (verb source destination &key overwrite)
  "Copy or move SOURCE to DESTINATION, waiting for the deferred half."
  (api-run :post (format nil "/resources/~a" verb)
           :params (list (cons "from" source)
                         (cons "path" destination)
                         (cons "overwrite" (if overwrite "true" "false")))))

(defun delete-resource (path &key permanently)
  (api-run :delete "/resources"
           :params (list (cons "path" path)
                         (cons "permanently" (if permanently "true" "false")))))

;;; --- transfers ----------------------------------------------------------------

(defvar *staging-counter* 0
  "Makes a staging name unique within one process, beside the clock.")

(defun staging-path (path)
  "A sibling of PATH to land a transfer at, so an interrupted one leaves the
original where it was."
  (format nil "~a.~8,'0x~4,'0x.part" path
          (logand (get-universal-time) #xffffffff) (incf *staging-counter*)))

(defun upload-link (path &key overwrite)
  (api-json :get "/resources/upload"
            :params (list (cons "path" path)
                          (cons "overwrite" (if overwrite "true" "false")))))

(defun upload-bytes (path octets &key overwrite)
  "Ask for a single-use upload URL, PUT the octets there, then wait for the
commit. The token stays on the API host: the transfer host needs none."
  (let ((link (upload-link path :overwrite overwrite)))
    (let ((href (nlk:json-value link :string "href")))
      (unless href (fail "Yandex Disk did not return an upload link for ~a." path))
      (multiple-value-bind (body status)
          (http :put href :content octets
                       :timeout (max 60 (setting :timeout-seconds)))
        (when (and (integerp status) (>= status 400))
          (fail "~a" (failure-text body status))))
      (let ((operation (nlk:json-value link :string "operation_id")))
        (when operation
          (wait-for-operation (format nil "~a/operations/~a"
                                      (string-right-trim
                                       "/" (or (setting :base-url) +api-base+))
                                      operation))))
      t)))

(defun upload-from-url (path url)
  "Have Yandex fetch a public URL straight into the disk -- never proxied here."
  (api-run :post "/resources/upload"
           :params (list (cons "path" path) (cons "url" url))))

(defun download-link (path)
  (let ((href (nlk:json-value (api-json :get "/resources/download"
                                         :params (list (cons "path" path)))
                               :string "href")))
    (or href (fail "Yandex Disk did not return a download link for ~a." path))))

(defun download-octets (path &key max-bytes)
  "A file's bytes, refusing anything past MAX-BYTES rather than buffering it
all and complaining afterwards."
  (multiple-value-bind (body status truncated)
      (http :get (download-link path) :binary t :max-bytes max-bytes)
    (when (and (integerp status) (>= status 400))
      (fail "~a" (failure-text body status)))
    (let ((octets (cond ((stringp body)
                         (sb-ext:string-to-octets body :external-format :utf-8))
                        ((vectorp body) body)
                        (t (make-array 0 :element-type '(unsigned-byte 8))))))
      (when (or truncated (> (length octets) max-bytes))
        (fail "File is larger than the ~a byte limit for this tool." max-bytes))
      octets)))

;;; --- sharing and the bin --------------------------------------------------------

(defun publish-resource (path)
  (api-json :put "/resources/publish" :params (list (cons "path" path)))
  (get-meta path :fields "path,name,type,public_url,public_key"))

(defun unpublish-resource (path)
  (api-json :put "/resources/unpublish" :params (list (cons "path" path)))
  (get-meta path :fields "path,name,type,public_url"))

(defun trash-page (&key (path "trash:/") (limit 50) (offset 0))
  (api-json :get "/trash/resources"
            :params (list (cons "path" path)
                          (cons "limit" limit)
                          (cons "offset" offset))))

(defun trash-meta (path)
  "One bin item's metadata -- notably the origin_path it was deleted from."
  (api-json :get "/trash/resources"
            :params (list (cons "path" path)
                          (cons "limit" 0)
                          (cons "fields" "path,name,type,origin_path,deleted"))))

(defun restore-resource (path &key name overwrite)
  (api-run :put "/trash/resources/restore"
           :params (list (cons "path" path)
                         (cons "name" name)
                         (cons "overwrite" (if overwrite "true" "false")))))

(defun destroy-trash (&optional path)
  "Destroy one bin item, or the whole bin when PATH is NIL."
  (api-run :delete "/trash/resources"
           :params (list (cons "path" path))))
