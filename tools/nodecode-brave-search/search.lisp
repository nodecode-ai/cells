;;;; search.lisp --- one request to Brave, one answer as lines, the tool body.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's tools.py, in Lisp: the same endpoint, the same six
;;;; parameters, the token in the same header, and the same fields lifted out
;;;; of the answer. Two things differ, both deliberate. Brave's answer is
;;;; rendered as one block a result -- the fields the original put in a JSON
;;;; object are here lines a model reads -- and a failure SIGNALS
;;;; BRAVE-SEARCH-ERROR carrying Brave's own sentence, so a model gets the
;;;; reason rather than a JSON "error" member it has to parse.
;;;;
;;;; Nothing here touches a file or the environment: the only way out is
;;;; HTTP, the one seam a test stubs.

(in-package #:nodecode-brave-search)

;;; --- the boundary: what a model call carries ---------------------------------------
;;; The original's schema, member for member: the same wire keys, the same
;;; range, the same safe-search enum.

(nlk:define-contract web-search-request
  (:doc "Search the live web with Brave Search and answer ranked results. Use it
for current facts, recent pages, or when you need search-engine results.")
  ("query" :text :required t
   :doc "the search query sent to Brave")
  ("count" :integer :slot result-count
   :doc "how many results to return, 1 to 20; the section's count answers when this is absent")
  ("country" :text
   :doc "a country code, e.g. us or in; the section's country answers when this is absent")
  ("search_lang" :text
   :doc "a result language code, e.g. en; the section's search_lang answers when this is absent")
  ("freshness" :text
   :doc "a recency window: pd (day), pw (week), pm (month) or py (year); the section's freshness answers when this is absent")
  ("safesearch" :text :options '("off" "moderate" "strict")
   :doc "the safe-search mode; the section's safesearch answers when this is absent"))

(defparameter +search-url+ "https://api.search.brave.com/res/v1/web/search"
  "Brave's web-search endpoint, as the original named it.")

(defparameter +result-ceiling+ 20
  "The most results Brave answers with in one call, as the original's schema said.")

(defparameter +safe-search-modes+ '("off" "moderate" "strict")
  "What an `X-Safesearch' parameter may say.")

(defparameter +detail-chars+ 1000
  "How much of a failed answer's body the refusal quotes, as the original clipped it.")

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

;;; --- what Brave sends, made plain --------------------------------------------------

(defun entity-character (name)
  "The character the HTML entity NAME stands for (&amp;, &#39;, &#x27;,
&nbsp;), or NIL for one this cell does not know."
  (cond ((or (string-equal name "amp")) #\&)
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
  "TEXT with the HTML entities Brave escapes decoded; one this cell does not
know is left as it stands, never dropped."
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
  "TEXT with every <...> span cut out of it, as the original's TAG_RE did."
  (with-output-to-string (out)
    (let ((in-tag nil))
      (loop for character across text
            do (cond (in-tag (when (char= character #\>) (setf in-tag nil)))
                     ((char= character #\<) (setf in-tag t))
                     (t (write-char character out)))))))

(defun collapse-space (text)
  "TEXT with every run of whitespace as one space and both ends trimmed, as
the original's WHITESPACE_RE did."
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

;;; --- the words the settings and a call carry ---------------------------------------

(defun safe-search-mode (value)
  "VALUE as one of the three modes, or a refusal naming them."
  (let ((value (string-downcase (or (nonblank value) "moderate"))))
    (or (find value +safe-search-modes+ :test #'string=)
        (fail "safesearch is ~s: Brave takes ~{~a~^, ~}" value +safe-search-modes+))))

(defun clamped-count (count)
  "COUNT within 1 and +RESULT-CEILING+, as the original clamped it; the
section's own count answers when a call names none."
  (max 1 (min +result-ceiling+ (or count (setting :count) 10))))

(defun request-url (query &key count country search-lang freshness safesearch)
  "Brave's address for QUERY with the parameters a call carries; a member
left empty falls back to its setting, and one empty there too is not sent."
  (let ((pairs (list (cons "q" query)
                      (cons "count" (princ-to-string (clamped-count count)))
                      (cons "safesearch" (safe-search-mode
                                           (or (nonblank safesearch) (setting :safesearch)))))))
    (dolist (pair (list (cons "country" (or (nonblank country) (setting :country)))
                         (cons "search_lang" (or (nonblank search-lang) (setting :search-lang)))
                         (cons "freshness" (or (nonblank freshness) (setting :freshness)))))
      (when (cdr pair) (setf pairs (append pairs (list pair)))))
    (format nil "~a?~{~a~^&~}"
            +search-url+
            (loop for (name . value) in pairs
                  collect (format nil "~a=~a" name
                                  (quri:url-encode value :encoding :utf-8))))))

;;; --- the one seam ---------------------------------------------------------------

(defun http (url &key headers timeout)
  "The cell's one network call; a test stubs this name. => (values BODY
STATUS), every status a value."
  (nlk:http :get url :headers headers :timeout (or timeout 20)))

(defun ask-brave (url key &key timeout)
  "Brave's answer to URL, with the subscription token the original sent in
X-Subscription-Token. => (values BODY STATUS)."
  (http url
        :headers (list (cons "accept" "application/json")
                       (cons "x-subscription-token" key))
        :timeout timeout))

(defun payload-of (body status)
  "BODY decoded as Brave's JSON answer, or a refusal naming the status and
Brave's own sentence."
  (when (>= status 400)
    (fail "Brave Search API returned an error: HTTP ~d~@[ ~a~]"
          status (let ((text (plain-text (or body ""))))
                   (and (plusp (length text)) (clipped text)))))
  (let ((payload (ignore-errors (nlk:decode-json (or body "")))))
    (unless (hash-table-p payload)
      (fail "Brave Search API returned non-JSON response: HTTP ~d~@[ ~a~]"
            status (let ((text (plain-text (or body ""))))
                     (and (plusp (length text)) (clipped text)))))
    payload))

;;; --- the answer as lines ----------------------------------------------------------

(defun results-of (payload)
  "PAYLOAD's web results, each the fields the original lifted: title, url,
description, age, language, source, display_url."
  (loop for item across (or (nlk:json-value payload :array "web" "results") #())
        for profile = (nlk:json-value item :object "profile")
        for meta = (nlk:json-value item :object "meta_url")
        collect (list :title (plain-text (nlk:json-value item :string "title"))
                      :url (or (nlk:json-value item :string "url") "")
                      :description (plain-text (nlk:json-value item :string "description"))
                      :age (nlk:json-value item :string "age")
                      :language (nlk:json-value item :string "language")
                      :source (and profile
                                    (or (nlk:json-value profile :string "long_name")
                                        (nlk:json-value profile :string "name")))
                      :display-url (and meta (nlk:json-value meta :string "display_url")))))

(defun result-block (result index)
  "One RESULT as the model reads it: a numbered title, its url, who and how
old when Brave says, then the description."
  (destructuring-bind (&key title url description age language source display-url &allow-other-keys)
      result
    (let ((traits (remove nil (list source age (and language (format nil "language ~a" language))))))
      (with-output-to-string (out)
        (format out "[~d] ~a" index (if (plusp (length title)) title url))
        (format out "~%    ~a" (if (plusp (length url)) url "(no url)"))
        (when (and display-url (plusp (length display-url)) (string/= display-url url))
          (format out " (shown as ~a)" display-url))
        (when traits (format out " - ~{~a~^, ~}" traits))
        (when (plusp (length description))
          (format out "~%    ~a" description))))))

(defun search-text (query results &key freshness)
  "What a search answers: one block a result, or a line saying Brave found
none."
  (if (null results)
      (format nil "brave: no results for ~s~@[ (freshness ~a)~]" query freshness)
      (format nil "brave: ~d result~:p for ~s~@[ (freshness ~a)~]~2%~{~a~^~2%~}"
              (length results) query freshness
              (loop for result in results
                    for index from 1
                    collect (result-block result index)))))

;;; --- the verb ------------------------------------------------------------------

(define-verb search (query &key count country search-lang freshness safesearch)
  "Search the live web through Brave Search and answer ranked results. QUERY
is the search; COUNT 1-20 (the section's count when absent); COUNTRY a code
such as \"us\"; SEARCH-LANG a code such as \"en\"; FRESHNESS \"pd\", \"pw\",
\"pm\" or \"py\"; SAFESEARCH \"off\", \"moderate\" or \"strict\". => the
results as lines: title, url, source and age, description."
  (let ((query (nonblank query)))
    (unless query
      (fail "search needs a query: give one, e.g. (brave:search \"latest Rust release notes\")"))
    (let ((freshness (or (nonblank freshness) (setting :freshness))))
      (multiple-value-bind (body status)
          (ask-brave (request-url query
                                   :count count
                                   :country country
                                   :search-lang search-lang
                                   :freshness freshness
                                   :safesearch safesearch)
                     (setting :api-key)
                     :timeout (setting :timeout-seconds))
        (search-text query (results-of (payload-of body status)) :freshness freshness)))))

;;; --- the tool the model calls -----------------------------------------------------

(defun web-search-tool (request)
  "The brave_web_search tool: the members one call carries, answered as the
verb answers them."
  (search (web-search-request-query request)
          :count (web-search-request-result-count request)
          :country (web-search-request-country request)
          :search-lang (web-search-request-search-lang request)
          :freshness (web-search-request-freshness request)
          :safesearch (web-search-request-safesearch request)))

;;; --- the operator's own door ------------------------------------------------------

(defun run-slash (args session-id)
  "/brave QUERY: the operator's own search, answered as the model's tool is."
  (declare (ignore session-id))
  (let ((query (nonblank args)))
    (if query
        (search query)
        (format nil "brave: type a query, e.g. /brave latest Rust release notes. This cell also gives the model the ~
brave_web_search tool (~d results when it names no count)."
                (clamped-count nil)))))
