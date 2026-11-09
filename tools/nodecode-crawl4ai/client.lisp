;;;; client.lisp --- the Crawl4AI seam: the address, the token, the /md request.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Crawl4AI (https://github.com/unclecode/crawl4ai) is a self-hosted crawler
;;;; that renders a page in its own browser and answers it as LLM-friendly
;;;; markdown. Its /md route takes one page URL and answers
;;;; {"url": ..., "markdown": ..., "success": true}, the markdown sometimes
;;;; nested as {"raw_markdown": ...}. This file is the only place that talks
;;;; to it: the same route, the same bearer token, the same two answer shapes
;;;; and the same title rule (the page's first top-level heading) the original
;;;; plugin used.
;;;;
;;;; Every exchange goes through *HTTP* (NLK:HTTP by default), so a test binds
;;;; that one name and no socket is opened. A refused status is a VALUE here,
;;;; not a condition: (REQUEST-MD ...) answers the body and the status, and
;;;; EXTRACT-ONE decides what the status means. Only a transport failure (the
;;;; server is not running) signals, with the address and the hint.
;;;;
;;;; A page URL pointing into a private or loopback network is refused before
;;;; the request leaves, as the original's is_safe_url refused it: the point of
;;;; extraction is the public web, and the server renders whatever it is handed.

(in-package #:nodecode-crawl4ai)

;;; --- the seam -------------------------------------------------------------

(defparameter +server-help+
  "Crawl4AI is a server of your own: `docker run -d -p 11235:11235 unclecode/crawl4ai'
prints its JWT on startup."
  "Where the server comes from, said in every message that needs it.")

(defvar *http* #'nlk:http
  "The cell's one network seam: NLK:HTTP's own argument list, answering
(values BODY STATUS HEADERS TRUNCATED-P). A test binds this name.")

(defun http (method url &key headers content timeout)
  "One exchange through *HTTP*, connect and read alike under TIMEOUT."
  (funcall *http* method url :headers headers :content content
                    :timeout timeout :connect-timeout (min 10 timeout)))

;;; --- the server -----------------------------------------------------------

(defun base-url ()
  "The configured server address, trailing slashes off; \"\" when unset."
  (or (setting :url) ""))

(defun require-server ()
  "The address, or a refusal naming the member that is missing."
  (let ((url (base-url)))
    (if (plusp (length url))
        url
        (fail "crawl4ai.url is not set: put your Crawl4AI server's address in the crawl4ai ~
section of ~~/.nodecode/config.jsonc, e.g. {\"crawl4ai\": {\"url\": \"http://localhost:11235\"}}. ~
~a" +server-help+))))

(defun auth-headers ()
  "The bearer token the server was started with, when one was."
  (let ((token (setting :api-token)))
    (if (and token (plusp (length token)))
        (list (cons "authorization" (format nil "Bearer ~a" token)))
        '())))

(defun endpoint (path)
  "PATH on the configured server."
  (concatenate 'string (require-server) path))

(defun redact (text)
  "TEXT with the configured bearer token taken out of it."
  ;; Read off the settings plist, not through SETTING: FAIL runs this while
  ;; the cell may not be started at all.
  (let ((token (getf *crawl4ai* :api-token)))
    (if (and (stringp text) (stringp token) (plusp (length token)))
        (uiop:frob-substrings text (list token) "[redacted]")
        text)))

;;; --- the page URL policy ---------------------------------------------------
;;; The original asked is_safe_url before every request: a page on a private,
;;; loopback or link-local address was answered as an error, never fetched.
;;; The same literals are refused here, plus the cloud metadata endpoints,
;;; which are where the machine's own credentials answer.

(defparameter +private-hosts+
  '("localhost" "localhost.localdomain" "ip6-localhost" "ip6-loopback" "0.0.0.0")
  "Host names that name the machine the cell runs on.")

(defun url-host (url)
  "URL's host, lowercased and un-bracketed, or \"\" when it cannot be parsed."
  (let ((uri (ignore-errors (quri:uri url))))
    (string-downcase (string-trim "[]" (or (and uri (quri:uri-host uri)) "")))))

(defun url-scheme (url)
  "URL's scheme, lowercased, or \"\" when it cannot be parsed."
  (let ((uri (ignore-errors (quri:uri url))))
    (string-downcase (or (and uri (quri:uri-scheme uri)) ""))))

(defun ipv4-octets (host)
  "HOST as four numbers when it is a dotted IPv4 literal, else NIL."
  (let* ((parts (uiop:split-string host :separator "."))
         (numbers (and (= 4 (length parts))
                       (mapcar (lambda (part)
                                 (let ((number (ignore-errors (parse-integer part :junk-allowed t))))
                                   (and number (<= 0 number 255) number)))
                               parts))))
    (and numbers (every #'numberp numbers) numbers)))

(defun private-ipv4-p (octets)
  "Whether the address OCTETS names is loopback, private or link-local."
  (destructuring-bind (a b &rest rest) octets
    (declare (ignore rest))
    (or (= a 0) (= a 127) (= a 10)
        (and (= a 172) (<= 16 b 31))
        (and (= a 192) (= b 168))
        (and (= a 169) (= b 254)))))

(defun private-host-p (host)
  "Whether HOST names a private, loopback or link-local address."
  (or (member host +private-hosts+ :test #'string=)
      (some (lambda (suffix) (uiop:string-suffix-p suffix host))
            '(".localhost" ".local" ".internal" ".localdomain"))
      (let ((octets (ipv4-octets host)))
        (and octets (private-ipv4-p octets)))
      ;; An IPv6 literal: loopback, unique-local (fc00::/7) or link-local.
      (and (find #\: host)
           (or (string= host "::1")
               (uiop:string-prefix-p "fe80" host)
               (uiop:string-prefix-p "fc" host)
               (uiop:string-prefix-p "fd" host)))))

(defun policy-url (url)
  "URL normalized, or a refusal: a page address must be http or https and
must not point into the machine's own network."
  (let ((text (string-trim '(#\Space #\Tab #\Return #\Newline) (or url ""))))
    (when (zerop (length text))
      (fail "url is empty: say which page to extract, e.g. https://example.com"))
    (when (and (search "://" text)
               (not (or (uiop:string-prefix-p "http://" text)
                        (uiop:string-prefix-p "https://" text))))
      (fail "~a is not a page address: extract reads http and https pages" text))
    (unless (or (uiop:string-prefix-p "http://" text) (uiop:string-prefix-p "https://" text))
      (setf text (concatenate 'string "https://" text)))
    (let ((host (url-host text)))
      (when (zerop (length host))
        (fail "~a is not a page address: it names no host" text))
      (when (private-host-p host)
        (fail "Blocked: ~a is a private or loopback address, so the page would come from the ~
network this machine sits on." host)))
    text))

;;; --- the /md request ------------------------------------------------------

(defun answer-object (body)
  "BODY decoded, when it is a JSON object; NIL for an empty or other answer."
  (let ((object (and (stringp body)
                      (plusp (length (string-trim '(#\Space #\Return #\Newline) body)))
                      (ignore-errors (nlk:decode-json body)))))
    (and (hash-table-p object) object)))

(defun json-member (object key)
  "KEY's member of OBJECT — a JSON object — or NIL when it is absent, null,
or OBJECT is no object at all; the second value says it was present."
  (if (hash-table-p object)
      (gethash key object)
      (values nil nil)))

(defun text-member (object key)
  "KEY's member of OBJECT as text, \"\" when it is absent or not a string."
  (multiple-value-bind (value present) (json-member object key)
    (if (and present (stringp value)) value "")))

(defun markdown-of (object)
  "The markdown OBJECT carries, either as a string or as the object Crawl4AI
nests it in (raw_markdown, then markdown)."
  (multiple-value-bind (value present) (json-member object "markdown")
    (cond ((not present) "")
          ((stringp value) value)
          ((hash-table-p value)
           (let ((raw (text-member value "raw_markdown")))
             (if (plusp (length raw)) raw (text-member value "markdown"))))
          (t ""))))

(defun error-detail (body)
  "The server's own words for a refused exchange, when its body carries any."
  (let ((object (answer-object body)))
    (if object
        (let ((detail (text-member object "detail")))
          (if (plusp (length detail))
              detail
              (or (let ((error (text-member object "error"))) (and (plusp (length error)) error))
                  (let ((message (text-member object "message")))
                    (and (plusp (length message)) message)))))
        (let ((text (string-trim '(#\Space #\Tab #\Return #\Newline) (or body ""))))
          (and (plusp (length text)) (subseq text 0 (min 200 (length text))))))))

(defun request-md (url)
  "POST URL to the server's /md route => (values BODY STATUS).
A transport failure signals CRAWL4AI-ERROR naming the address and the hint."
  (handler-case
      (http "POST" (endpoint "/md")
            :headers (append '(("accept" . "application/json")
                               ("content-type" . "application/json"))
                             (auth-headers))
            :content (nlk:encode-json-object (nlk:make-json-object "url" url))
            :timeout (setting :timeout-seconds))
    (crawl4ai-error (condition) (error condition))
    (nlk:turn-cancelled-condition (condition) (error condition))
    (error (condition)
      (fail "cannot reach the Crawl4AI server at ~a (~a). Is it running? ~a"
            (base-url) condition +server-help+))))

(defun title-of (markdown fallback)
  "The page's title as the original read it: its first top-level heading."
  (loop for line in (uiop:split-string markdown :separator '(#\Newline))
        for text = (string-trim '(#\Space #\Tab #\Return) line)
        when (uiop:string-prefix-p "# " text)
          do (return (string-trim '(#\Space #\Tab) (subseq text 2)))
        finally (return fallback)))

(defun extract-one (url)
  "URL's markdown from the server, as (:URL :TITLE :MARKDOWN) or (:URL :ERROR)."
  (multiple-value-bind (body status) (request-md url)
    (if (>= status 300)
        (list :url url :error (let ((detail (error-detail body)))
                                 (format nil "the Crawl4AI server answered HTTP ~d~@[: ~a~]"
                                         status detail)))
        (let* ((object (answer-object body))
               (markdown (and object (markdown-of object)))
               (detail (and object (error-detail body))))
          (cond ((null object)
                 (list :url url :error "the Crawl4AI server answered no JSON object"))
                ((and (zerop (length markdown)) detail)
                 (list :url url :error detail))
                (t (list :url url :title (title-of (or markdown "") url)
                         :markdown markdown)))))))

(defun extract-one-safe (url)
  "EXTRACT-ONE behind the URL policy: a blocked URL is one error, not a
failed call, as the original answered it per URL."
  (handler-case (extract-one (policy-url url))
    (crawl4ai-error (condition)
      (list :url (or url "") :error (crawl4ai-error-detail condition)))))

(defun render (result)
  "One page of the answer: its title and address, then its markdown."
  (let ((url (getf result :url))
        (error (getf result :error)))
    (if error
        (format nil "## ~a~%ERROR: ~a" url error)
        (let ((markdown (getf result :markdown))
              (title (getf result :title)))
          (format nil "## ~a~%~a~%~%~a"
                  (if (and title (plusp (length title))) title url)
                  url
                  (if (and markdown (plusp (length markdown)))
                      markdown
                      "(the page answered no text)"))))))

(defun extract (urls)
  "Every URL rendered as one answer, in the order asked: the same request per
page the original's extract made, with the same per-page error lines."
  (let ((urls (remove-duplicates (remove "" urls :test #'string=) :test #'string=)))
    (when (null urls)
      (fail "no URL to extract: name at least one page, e.g. https://example.com"))
    (format nil "~{~a~^~%~%~}" (mapcar (lambda (url) (render (extract-one-safe url))) urls))))

