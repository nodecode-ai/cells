;;;; cloro.lisp --- one POST to cloro, one answer as lines, the tool body.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's provider.py, in Lisp: the same endpoint, the same two
;;;; members in the body (query and a 2-letter country), the key in the same
;;;; Authorization header, the same 120-second ceiling because cloro opens
;;;; the live results page during the request, and the same fields lifted out
;;;; of result.organicResults -- link, title, snippet, results without a
;;;; resolved http(s) link skipped. Two things differ, both deliberate.
;;;; cloro's answer is rendered as one block a result -- the fields the
;;;; original put in a JSON object are here lines a model reads -- and a
;;;; failure SIGNALS CLORO-ERROR carrying cloro's own sentence, so a
;;;; model gets the reason rather than a JSON "error" member it has to parse.
;;;;
;;;; Nothing here touches a file or the environment: the only way out is
;;;; HTTP, the one seam a test stubs.

(in-package #:nodecode-cloro)

;;; --- the boundary: what a model call carries --------------------------------

(nlk:define-contract cloro-search
  (:doc "Search the live web through cloro, which loads the Google results
page, and answer its organic results ranked. Use it for current facts,
recent pages, or when you need search-engine results.")
  ("query" :text :required t
   :doc "the search query sent to cloro")
  ("count" :integer :slot result-count
   :doc "how many results to return, 1 to 20; the section's count answers when this is absent")
  ("country" :text
   :doc "a 2-letter ISO 3166-1 code such as US or DE, the location of the results; the section's country answers when this is absent"))

(defparameter +search-url+ "https://api.cloro.dev/v1/monitor/google"
  "cloro's Google-search endpoint, as the original named it.")

(defparameter +key-url+ "https://dashboard.cloro.dev/api-keys"
  "Where cloro hands out the API key the section carries.")

(defparameter +result-ceiling+ 20
  "The most results one call asks cloro for; more is clamped.")

(defparameter +detail-chars+ 1000
  "How much of a failed answer's body a refusal quotes, as the original clipped it.")

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

;;; --- what cloro sends, made plain -------------------------------------------

(defun entity-character (name)
  "The character the HTML entity NAME stands for (&amp;, &#39;, &#x27;,
&nbsp;), or NIL for one this cell does not know."
  (cond ((string-equal name "amp") #\&)
        ((string-equal name "lt") #\<)
        ((string-equal name "gt") #\>)
        ((string-equal name "quot") #\")
        ((or (string-equal name "apos") (string= name "39")) #\')
        ((string-equal name "nbsp") #\Space)
        ((and (> (length name) 1) (char= (char name 0) #\#))
         (let ((rest (subseq name 1)))
           (if (and (plusp (length rest)) (char-equal (char rest 0) #\x))
               (ignore-errors (code-char (parse-integer rest :start 1 :radix 16)))
               (ignore-errors (code-char (parse-integer rest))))))
        (t nil)))

(defun decode-entities (text)
  "TEXT with the HTML entities Google escapes decoded; one this cell does
not know is left as it stands, never dropped."
  (with-output-to-string (out)
    (let ((index 0)
          (length (length text)))
      (loop while (< index length)
            do (let* ((character (char text index))
                      (end (and (char= character #\&)
                                (position #\; text :start index
                                              :end (min length (+ index 12)))))
                      (decoded (and end (entity-character (subseq text (1+ index) end)))))
                 (if decoded
                     (progn (write-char decoded out) (setf index (1+ end)))
                     (progn (write-char character out) (incf index))))))))

(defun strip-tags (text)
  "TEXT with every <...> span cut out of it: Google bolds the matched words
in a snippet, and the tags are not what the page says."
  (with-output-to-string (out)
    (let ((in-tag nil))
      (loop for character across text
            do (cond (in-tag (when (char= character #\>) (setf in-tag nil)))
                     ((char= character #\<) (setf in-tag t))
                     (t (write-char character out)))))))

(defun collapse-space (text)
  "TEXT with every run of whitespace as one space and both ends trimmed."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (with-output-to-string (out)
                 (let ((space nil))
                   (loop for character across text
                         do (if (member character '(#\Space #\Tab #\Newline #\Return #\Page))
                                (setf space t)
                                (progn (when space (write-char #\Space out))
                                       (setf space nil)
                                       (write-char character out))))))))

(defun plain-text (value)
  "VALUE as the model reads it: entities decoded, tags cut, whitespace
collapsed; NIL is the empty string."
  (if (null value)
      ""
      (collapse-space (strip-tags (decode-entities (princ-to-string value))))))

(defun clipped (text)
  "TEXT at most +DETAIL-CHARS+ characters, with a note when it was cut."
  (if (<= (length text) +detail-chars+)
      text
      (format nil "~a [~d more characters]" (subseq text 0 +detail-chars+)
              (- (length text) +detail-chars+))))

;;; --- the words the settings and a call carry --------------------------------

(defun country-code (value)
  "VALUE as the 2-letter ISO 3166-1 code cloro takes, upcased, or a refusal
naming what it wanted; the section's country answers when a call names none."
  (let ((text (string-upcase (or (nonblank value) "US"))))
    (unless (and (= 2 (length text))
                 (every (lambda (character) (<= (char-code #\A) (char-code character) (char-code #\Z)))
                        text))
      (fail "country is ~s: cloro takes a 2-letter ISO 3166-1 code, for example US or DE"
            value))
    text))

(defun clamped-count (count)
  "COUNT within 1 and +RESULT-CEILING+, as the original clamped it up from
zero; the section's own count answers when a call names none."
  (max 1 (min +result-ceiling+ (or count (setting :count) 5))))

(defun request-body (query country)
  "The JSON cloro takes for QUERY in COUNTRY, as the original built it."
  (nlk:encode-json-object (nlk:json-object "query" query "country" country)))

;;; --- the one seam ------------------------------------------------------------

(defun http (method url &key headers content timeout)
  "The cell's one network call; a test stubs this name. => (values BODY
STATUS), every status a value."
  (nlk:http method url :headers headers :content content :timeout (or timeout 120)))

(defun ask-cloro (url body key &key timeout)
  "cloro's answer to URL for BODY, with the key the original sent as a
bearer token. => (values BODY STATUS)."
  (http :post url
        :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                       (cons "content-type" "application/json")
                       (cons "accept" "application/json")
                       (cons "user-agent" "nodecode-cloro"))
        :content body
        :timeout timeout))

;;; --- the answer, or cloro's own reason --------------------------------------

(defun api-message (payload)
  "PAYLOAD's error sentence or code, as cloro writes a failure:
{\"success\": false, \"error\": {\"code\": \"...\", \"message\": \"...\"}}."
  (let ((error (and (hash-table-p payload) (nlk:json-value payload :object "error"))))
    (and error
         (or (nlk:json-value error :string "message")
             (nlk:json-value error :string "code")))))

(defun payload-of (body status &aux (text (plain-text (or body ""))))
  "BODY decoded as cloro's JSON answer, or a refusal naming the status and
cloro's own sentence, as the original read the error member."
  (let ((payload (unless (zerop (length (or body "")))
                   (ignore-errors (nlk:decode-json body)))))
    (when (>= status 400)
      (fail "cloro returned HTTP ~d~@[ - ~a~]"
            status (or (api-message payload)
                       (and (plusp (length text)) (clipped text)))))
    (unless (hash-table-p payload)
      (fail "cloro answered a non-JSON body: HTTP ~d~@[ - ~a~]"
            status (and (plusp (length text)) (clipped text))))
    payload))

(defun web-url-p (url)
  "Whether URL is one a model can open: absolute http or https, as the
original kept only those."
  (and (stringp url)
       (or (uiop:string-prefix-p "http://" url)
           (uiop:string-prefix-p "https://" url))))

(defun results-of (payload limit)
  "PAYLOAD's organic results, each the fields the original lifted -- title,
link and snippet -- in the order cloro ranked them, at most LIMIT of them; a
result with no resolved http(s) link is skipped, as the original skipped
it."
  (let ((items (nlk:json-value payload :array "result" "organicResults")))
    (unless (arrayp items)
      (fail "cloro answered without result.organicResults~@[ - ~a~]"
            (api-message payload)))
    (let ((rows '()))
      (loop for item across items
            while (< (length rows) limit)
            for url = (and (hash-table-p item) (nlk:json-value item :string "link"))
            when (web-url-p url)
              do (push (list :title (plain-text (nlk:json-value item :string "title"))
                             :url url
                             :description (plain-text (nlk:json-value item :string "snippet")))
                       rows))
      (nreverse rows))))

;;; --- the answer as lines -----------------------------------------------------

(defun result-block (result index)
  "One RESULT as the model reads it: a numbered title, its url, then the
snippet cloro carried."
  (destructuring-bind (&key title url description &allow-other-keys) result
    (with-output-to-string (out)
      (format out "[~d] ~a" index (if (plusp (length title)) title url))
      (format out "~%    ~a" url)
      (when (plusp (length description))
        (format out "~%    ~a" description)))))

(defun search-text (query country results)
  "What a search answers: one block a result, or a line saying cloro found
none."
  (if (null results)
      (format nil "cloro: no results for ~s (country ~a)" query country)
      (format nil "cloro: ~d result~:p for ~s (country ~a)~2%~{~a~^~2%~}"
              (length results) query country
              (loop for result in results
                    for index from 1
                    collect (result-block result index)))))

;;; --- the verb ---------------------------------------------------------------

(define-verb search (query &key count country)
  "Search the live web through cloro and answer Google's organic results.
QUERY is the search; COUNT 1-20 (the section's count when absent) and
COUNTRY a 2-letter code such as \"US\" (the section's country when absent).
=> the results as lines: title, url and snippet."
  (let ((query (nonblank query)))
    (unless query
      (fail "search needs a query: give one, e.g. (cloro:search \"latest Rust release notes\")"))
    (let ((country (country-code (or (nonblank country) (setting :country)))))
      (multiple-value-bind (body status)
          (ask-cloro +search-url+ (request-body query country) (setting :api-key)
                     :timeout (setting :timeout-seconds))
        (search-text query country (results-of (payload-of body status)
                                               (clamped-count count)))))))

;;; --- the tool the model calls ------------------------------------------------

(defun cloro-search-tool (request)
  "The cloro_search tool: the members one call carries, answered as the verb
answers them."
  (search (cloro-search-query request)
          :count (cloro-search-result-count request)
          :country (cloro-search-country request)))

;;; --- the operator's own door -------------------------------------------------

(defun run-slash (args session-id)
  "/cloro QUERY: the operator's own search, answered as the model's tool is."
  (declare (ignore session-id))
  (let ((query (nonblank args)))
    (if query
        (search query)
        (format nil "cloro: type a query, e.g. /cloro latest Rust release notes. This cell also gives the ~
model the cloro_search tool (~d results when it names no count)."
                (clamped-count nil)))))
