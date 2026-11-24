;;;; wire.lisp --- the one network seam, the two trust boundaries, and the reading of an answer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through REQUEST, so a test stubs NLK:HTTP
;;;; and never touches the network. The service answers its HTTP status as a
;;;; value; anything but a 2xx is MARKDOWN-NEW-ERROR carrying the service's own
;;;; words, which is what the model repairs from.
;;;;
;;;; The two boundaries are the plugin's own (src/markdown_new/safety.py):
;;;; a URL the model hands over must be public and carry no credential, and a
;;;; local file to upload must be a regular file, under the size cap and not a
;;;; secret store. They are checked here before any request leaves.

(in-package #:nodecode-markdown-new)

(defparameter +base-url+ "https://markdown.new"
  "The public Markdown.new service, where every call goes unless the section
names another address.")

(defparameter +user-agent+ "markdown-new/0.1.0 (nodecode-markdown-new)"
  "The plugin's own user agent, with this cell's name after it.")

(defparameter *retry-delay* 1
  "Seconds to wait before the one retry a 429 or a 5xx earns.")

(defvar *sleep* #'sleep
  "How this cell waits: tests bind it to something that does not.")

(defparameter +sensitive-names+
  '(".env" ".netrc" "credentials" "credentials.json" "id_dsa" "id_ed25519" "id_rsa"
    "known_hosts")
  "File names the plugin refused to upload, kept here.")

(defparameter +sensitive-suffixes+ '(".key" ".kdbx" ".p12" ".pem" ".pfx")
  "Extensions the plugin refused to upload.")

(defparameter +sensitive-parts+ '(".aws" ".gnupg" ".ssh")
  "Directories the plugin refused to upload out of.")

(defparameter +sensitive-parameters+
  '("token" "access_token" "api_key" "apikey" "key" "secret" "password" "passwd"
    "sig" "signature" "auth" "session" "cookie" "code")
  "Query-parameter names that carry a credential, which no URL here may carry.")

;;; --- small readers ---------------------------------------------------------------

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left, or NIL for no text."
  (cond ((null text) nil)
        ((not (stringp text)) (clip (princ-to-string text) limit))
        ((<= (length text) limit) text)
        (t (format nil "~a~%[... ~d more characters; raise max_chars in the markdown-new section to read them]"
                   (subseq text 0 limit) (- (length text) limit)))))

(defun object-text (object key)
  "KEY of the JSON OBJECT as text, or NIL."
  (and (hash-table-p object) (nlk:json-value object :text key)))

(defun object-of (object key)
  "KEY of the JSON OBJECT as an object, or NIL."
  (and (hash-table-p object) (nlk:json-value object :object key)))

(defun objects-of (object key)
  "KEY of the JSON OBJECT as a list of objects, dropping anything else."
  (let ((value (and (hash-table-p object) (nlk:json-value object :array key))))
    (and value (remove-if-not #'hash-table-p (coerce value 'list)))))

(defun bolt (&rest pairs)
  "A JSON object from PAIRS (KEY VALUE ...), leaving out every NIL or empty
VALUE: a request carries only what the call asked for."
  (loop for (key value) on pairs by #'cddr
        when (and value (not (and (stringp value) (zerop (length value)))))
          append (list key value) into kept
        finally (return (apply #'nlk:make-json-object kept))))

(defun strings (vector)
  "VECTOR — a contract's array of strings — as a JSON array, or NIL when empty."
  (when (and vector (plusp (length vector)))
    (coerce (remove-if-not #'stringp (coerce vector 'list)) 'vector)))

;;; --- the section this cell runs on -----------------------------------------------

(defun root ()
  "The service address this cell talks to, without a trailing slash."
  (string-right-trim "/" (or (setting :base-url) +base-url+)))

(defun check-config ()
  "Refuse a section that names no service address, naming the member."
  (unless (plusp (length (root)))
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "markdown-new: base_url is blank: this cell needs the service "
                                 "address, https://markdown.new unless you run your own"))))

;;; --- the URL boundary -------------------------------------------------------------

(defun url-parts (url)
  "URL as (values SCHEME AUTHORITY TAIL), or NIL when it is no absolute URL."
  (let ((mark (search "://" url)))
    (when (and mark (plusp mark))
      (let* ((scheme (string-downcase (subseq url 0 mark)))
             (rest (subseq url (+ mark 3)))
             (end (or (position-if (lambda (character) (find character "/?#")) rest)
                      (length rest))))
        (values scheme (subseq rest 0 end) (subseq rest end))))))

(defun authority-host (authority)
  "AUTHORITY as (values HOST PORT USERINFO), HOST lowercased without its root dot."
  (let* ((at (position #\@ authority :from-end t))
         (userinfo (and at (subseq authority 0 at)))
         (hostport (if at (subseq authority (1+ at)) authority))
         (colon (position #\: hostport :from-end t))
         (host (if colon (subseq hostport 0 colon) hostport))
         (port (and colon (subseq hostport (1+ colon)))))
    (values (string-downcase (string-right-trim "." (string-trim "[]" host))) port userinfo)))

(defun ipv4-p (host)
  "HOST when it reads as a dotted-quad address, else NIL."
  (let ((parts (and (plusp (length host))
                    (ignore-errors (mapcar #'parse-integer (uiop:split-string host :separator "."))))))
    (and parts
         (= 4 (length parts))
         (every (lambda (part) (<= 0 part 255)) parts)
         t)))

(defun private-ipv4-p (host)
  "HOST when it is an address outside the public ranges, else NIL."
  (let ((parts (ignore-errors (mapcar #'parse-integer (uiop:split-string host :separator ".")))))
    (when (= 4 (length parts))
      (destructuring-bind (a b c d) parts
        (declare (ignore c d))
        (and (every (lambda (part) (<= 0 part 255)) parts)
             (or (= a 0) (= a 10) (= a 127) (= a 100)
                 (and (= a 169) (= b 254))
                 (and (= a 172) (<= 16 b 31))
                 (and (= a 192) (= b 168))
                 (>= a 224)))))))

(defun private-host-p (host)
  "HOST when it names this machine or a network inside one."
  (or (string= host "localhost")
      (uiop:string-suffix-p host ".localhost")
      (uiop:string-suffix-p host ".local")
      (uiop:string-suffix-p host ".internal")
      (and (find #\: host) (or (uiop:string-prefix-p "::1" host)
                               (uiop:string-prefix-p "fe80" host)
                               (uiop:string-prefix-p "fc" host)
                               (uiop:string-prefix-p "fd" host)))
      (private-ipv4-p host)))

(defun sensitive-parameter-p (tail)
  "The credential-bearing query parameter TAIL carries, or NIL."
  (let* ((query (and (position #\? tail) (subseq tail (1+ (position #\? tail)))))
         (fragment (and query (position #\# query)))
         (query (if fragment (subseq query 0 fragment) query)))
    (loop for piece in (and query (uiop:split-string query :separator "&"))
          for name = (string-downcase (subseq piece 0 (or (position #\= piece) (length piece))))
          when (member name +sensitive-parameters+ :test #'string=)
            return name)))

(defun validate-public-url (url)
  "URL when it is an uncredentialed public http or https address; a
MARKDOWN-NEW-ERROR naming what is wrong otherwise."
  (let ((url (and (stringp url) (nlk:trimmed url))))
    (unless (plusp (length url))
      (fail "markdown_new: the url is empty; give an absolute public http or https url"))
    (when (some (lambda (character) (< (char-code character) 32)) url)
      (fail "markdown_new: the url ~s carries a control character" url))
    (multiple-value-bind (scheme authority tail) (url-parts url)
      (unless (member scheme '("http" "https") :test #'string=)
        (fail "markdown_new: the url ~s must use http or https" url))
      (multiple-value-bind (host port userinfo) (authority-host (or authority ""))
        (when (and port (plusp (length port))
                   (not (and (every #'digit-char-p port) (<= 1 (parse-integer port) 65535))))
          (fail "markdown_new: the url ~s names no usable port" url))
        (when userinfo
          (fail "markdown_new: the url ~s carries credentials; send the bare address" url))
        (unless (plusp (length host))
          (fail "markdown_new: the url ~s names no host" url))
        (when (private-host-p host)
          (fail "markdown_new: the url ~s points inside the network; Markdown.new reads public pages only" url))
        (when (and (not (find #\. host)) (not (ipv4-p host)))
          (fail "markdown_new: the url ~s names no public hostname" url))
        (let ((parameter (sensitive-parameter-p tail)))
          (when parameter
            (fail "markdown_new: the url ~s carries a credential in its ~a parameter; Markdown.new would read it, so it is refused" url parameter)))))
    url))

;;; --- the upload boundary ----------------------------------------------------------

(defun directory-parts (path)
  "PATH's directory names, lowercased."
  (mapcar #'string-downcase
          (remove-if-not #'stringp (rest (pathname-directory path)))))

(defun sensitive-file-p (path)
  "PATH when it is one of the secret stores the plugin refused to upload."
  (let ((name (string-downcase (or (file-namestring path) "")))
        (type (string-downcase (or (pathname-type path) ""))))
    (or (member name +sensitive-names+ :test #'string=)
        (member (concatenate 'string "." type) +sensitive-suffixes+ :test #'string=)
        (intersection (directory-parts path) +sensitive-parts+ :test #'string=))))

(defun file-size (path)
  "PATH's size in bytes."
  (with-open-file (stream path :element-type '(unsigned-byte 8) :direction :input)
    (file-length stream)))

(defun read-bytes (path)
  "PATH's bytes as one octet vector."
  (with-open-file (stream path :element-type '(unsigned-byte 8) :direction :input)
    (let ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(defun validate-upload (path)
  "PATH — a local file to upload — as a truename, or a MARKDOWN-NEW-ERROR
naming what disqualifies it: no regular file, a secret store, over the cap."
  (let* ((given (and (stringp path) (nlk:trimmed path)))
         (truename (and (plusp (length given)) (ignore-errors (probe-file given)))))
    (unless (plusp (length given))
      (fail "markdown_new: source is empty; give a public url or a path to a local file"))
    (unless (and truename (not (uiop:directory-pathname-p truename)))
      (fail "markdown_new: no local file at ~s; give a public url or a file that is there" given))
    (when (sensitive-file-p truename)
      (fail "markdown_new: ~s looks like a secret store (~{~a~^, ~}); convert a document instead"
            given (append +sensitive-parts+ +sensitive-suffixes+)))
    (let ((size (file-size truename))
          (cap (setting :max-upload-bytes)))
      (when (> size cap)
        (fail "markdown_new: ~s is ~d bytes, over the ~d byte upload cap" given size cap))
      (values truename size))))

;;; --- the wire ---------------------------------------------------------------------

(defun service-words (answer)
  "What a body says went wrong, in one line, or NIL."
  (let ((value (and (stringp answer)
                     (plusp (length (nlk:trimmed answer)))
                     (handler-case (nlk:decode-json answer) (error () nil)))))
    (when (hash-table-p value)
      (or (object-text value "error")
          (object-text value "message")
          (object-text value "detail")
          (let ((nested (object-of value "error")))
            (and nested (or (object-text nested "message") (object-text nested "detail"))))))))

(defun check-status (path answer status)
  "Fail unless STATUS is a 2xx, in the service's own words when it gave any."
  (cond ((or (null status) (and (integerp status) (< status 300))) t)
        ((= status 404)
         (fail "markdown_new ~a: HTTP 404, the service knows no such page or job~@[: ~a~]"
               path (service-words answer)))
        ((= status 429)
         (fail "markdown_new ~a: HTTP 429, rate limited; wait before asking again" path))
        ((and (<= 500 status) (<= status 599))
         (fail "markdown_new ~a: HTTP ~d, the service failed~@[: ~a~]" path status (service-words answer)))
        (t (fail "markdown_new ~a: HTTP ~d~@[: ~a~]" path status (service-words answer)))))

(defun decode-answer (path answer)
  "ANSWER — a body — as a decoded JSON object; a service-side failure signals."
  (let ((value (cond ((hash-table-p answer) answer)
                     ((and (stringp answer) (plusp (length (nlk:trimmed answer))))
                      (handler-case (nlk:decode-json answer) (error () nil)))
                     (t nil))))
    (unless (hash-table-p value)
      (fail "markdown_new ~a: the service answered no JSON~@[: ~a~]" path (clip answer 200)))
    (multiple-value-bind (success present) (gethash "success" value)
      (when (and present (null success))
        (fail "markdown_new ~a: the service refused~@[: ~a~]" path (service-words answer))))
    value))

(defun request (method path &key content headers)
  "One exchange with the service => (values BODY STATUS). A transport failure
signals; a 429 or a 5xx is retried once, as the plugin's client did."
  (let ((url (concatenate 'string (root) path))
        (limit (setting :timeout-seconds))
        (answer nil)
        (status nil))
    (loop for attempt from 1 to 2
          do (handler-case
                 (multiple-value-setq (answer status)
                   (nlk:http method url
                             :headers (append (list (cons "user-agent" +user-agent+)
                                                    (cons "accept" "application/json"))
                                              headers)
                             :content content
                             :timeout limit
                             :connect-timeout (min 10 limit)))
               ((or error sb-sys:deadline-timeout) (condition)
                 (fail "markdown_new ~a: ~a" path (nle:transport-failure-label condition url))))
             (unless (and (= attempt 1) (integerp status)
                          (or (= status 429) (<= 500 status 599)))
               (return)))
    (values answer status)))

(defun request-json (method path &key body headers)
  "METHOD PATH => the decoded JSON object; any other answer signals."
  (multiple-value-bind (answer status) (request method path :content body :headers headers)
    (check-status path answer status)
    (decode-answer path answer)))

(defun post-json (path object)
  "POST OBJECT — a JSON object — to PATH => the decoded answer."
  (request-json :post path
                :body (nlk:encode-json-object object)
                :headers '(("content-type" . "application/json"))))

(defun get-json (path)
  (request-json :get path))

(defun delete-json (path)
  (request-json :delete path))

(defun url-segment (text)
  "TEXT as one URL path segment, percent-encoding what is not unreserved."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets text :external-format :utf-8)
          for character = (code-char octet)
          do (if (or (and (alphanumericp character) (< octet 128))
                     (find character "-._~"))
                 (write-char character out)
                 (format out "%~2,'0X" octet)))))

(defun job-path (job-id)
  "The status path of JOB-ID, or a refusal when it names nothing."
  (let ((job-id (nonempty job-id)))
    (unless job-id
      (fail "markdown_new: job_id is empty; a crawl answers one when it starts"))
    (format nil "/crawl/status/~a" (url-segment (nlk:trimmed job-id)))))

;;; --- the endpoints ----------------------------------------------------------------

(defun api-convert (url &key method retain-images)
  "POST / — the plugin's url_convert => the service's answer."
  (post-json "/" (bolt "url" url
                        "method" (nonempty method)
                        "retain_images" (and retain-images t))))

(defun api-search (query &key n gl hl retain-images)
  "POST /search — the plugin's search_extract => the service's answer."
  (post-json "/search" (bolt "q" query
                              "n" (or n 3)
                              "gl" (or (nonempty gl) "us")
                              "hl" (or (nonempty hl) "en")
                              "retain_images" (and retain-images t)
                              "format" "json")))

(defun api-crawl-start (url &key limit depth render source max-age modified-since
                                 include-external-links include-subdomains
                                 include-patterns exclude-patterns)
  "POST /crawl — a bounded crawl of URL => the service's answer, holding jobId."
  (post-json "/crawl"
              (bolt "url" url
                    "limit" (or limit 10)
                    "depth" (or depth 1)
                    "render" (and render t)
                    "source" (nonempty source)
                    "maxAge" max-age
                    "modifiedSince" modified-since
                    "includeExternalLinks" (and include-external-links t)
                    "includeSubdomains" (and include-subdomains t)
                    "includePatterns" (strings include-patterns)
                    "excludePatterns" (strings exclude-patterns))))

(defun api-crawl-status (job-id)
  "GET /crawl/status/JOB?format=json => the service's answer."
  (get-json (concatenate 'string (job-path job-id) "?format=json")))

(defun crawl-status (body)
  "BODY's result.status, lowercased, or NIL."
  (let ((result (object-of body "result")))
    (let ((status (object-text (or result body) "status")))
      (and status (string-downcase status)))))

(defun api-crawl-markdown (job-id &key retain-images)
  "GET /crawl/status/JOB => the crawl's Markdown as text."
  (let ((path (concatenate 'string (job-path job-id)
                            (if retain-images "?retain_images=true" ""))))
    (multiple-value-bind (answer status) (request :get path)
      (check-status path answer status)
      (if (and (stringp answer) (plusp (length (nlk:trimmed answer))))
          answer
          (fail "markdown_new ~a: the service answered no Markdown" path)))))

(defun api-crawl-cancel (job-id)
  "DELETE /crawl/status/JOB => the service's answer."
  (delete-json (job-path job-id)))

(defparameter +content-types+
  '(("pdf" . "application/pdf")
    ("txt" . "text/plain")
    ("text" . "text/plain")
    ("md" . "text/markdown")
    ("markdown" . "text/markdown")
    ("html" . "text/html")
    ("htm" . "text/html")
    ("csv" . "text/csv")
    ("json" . "application/json")
    ("xml" . "application/xml")
    ("rtf" . "application/rtf")
    ("epub" . "application/epub+zip")
    ("doc" . "application/msword")
    ("docx" . "application/vnd.openxmlformats-officedocument.wordprocessingml.document"))
  "The document types an upload names, by extension; anything else is bytes.")

(defun content-type-of (path)
  "The content type of PATH, by extension."
  (or (cdr (assoc (string-downcase (or (pathname-type path) "")) +content-types+
                   :test #'string=))
      "application/octet-stream"))

(defun safe-filename (name)
  "NAME with what would break a multipart header replaced, as the plugin did."
  (map 'string (lambda (character)
                 (if (find character '(#\\ #\" #\Return #\Newline)) #\_ character))
       name))

(defun multipart-body (path boundary)
  "PATH's bytes as one multipart/form-data body under BOUNDARY."
  (let* ((head (format nil "--~a~c~cContent-Disposition: form-data; name=\"file\"; filename=\"~a\"~c~cContent-Type: ~a~c~c~c~c"
                        boundary #\Return #\Linefeed
                        (safe-filename (file-namestring path)) #\Return #\Linefeed
                        (content-type-of path) #\Return #\Linefeed #\Return #\Linefeed))
         (tail (format nil "~c~c--~a--~c~c" #\Return #\Linefeed boundary #\Return #\Linefeed)))
    (concatenate '(vector (unsigned-byte 8))
                 (sb-ext:string-to-octets head :external-format :utf-8)
                 (read-bytes path)
                 (sb-ext:string-to-octets tail :external-format :utf-8))))

(defun api-convert-file (path &key method retain-images boundary)
  "POST /convert with PATH's bytes => the service's answer, holding data.content."
  (request-json :post "/convert"
                :body (multipart-body path boundary)
                :headers (list (cons "accept" "application/json")
                               (cons "content-type"
                                     (format nil "multipart/form-data; boundary=~a" boundary)))))
