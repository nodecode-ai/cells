;;;; client.lisp --- the Yandex Search API, both modes, and the tool bodies.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's client.py, provider.py and generative.py, in Lisp: the
;;;; same two endpoints, the same request bodies, the same Api-Key header,
;;;; and the same fields lifted out of each answer. Three things differ,
;;;; deliberately. The web report is read by a small scanner here instead of
;;;; a full XML parser -- the document is still refused whole on a DTD, a
;;;; byte that is not UTF-8, or more than 2 MiB, as the original refused it.
;;;; Each answer is rendered as lines a model reads instead of a JSON
;;;; envelope. And a failure SIGNALS YANDEX-ERROR carrying Yandex's own
;;;; sentence, so a model gets the reason rather than an "error" member to
;;;; parse.
;;;;
;;;; Nothing here touches a file or the environment: the only way out is
;;;; HTTP, the one seam a test stubs.

(in-package #:nodecode-yandex)

;;; --- the boundary: what a model call carries ---------------------------------------
;;; The original's two schemas, member for member: the same wire keys and the
;;; same ranges.

(nlk:define-contract web-search-request
  (:doc "Search the live web through the Yandex Search API and answer ranked
results, each with a title, a url and a snippet. Use it for current facts,
recent pages, or queries best answered by Yandex -- Russian-language ones
especially.")
  ("query" :text :required t
   :doc "the search query sent to Yandex")
  ("n" :integer :slot result-count
   :doc "how many results to return, 1 to 100; the section's max_results answers when this is absent"))

(nlk:define-contract generative-search-request
  (:doc "Answer a question with Yandex generative search: one short answer
synthesised from live web sources, with the source urls it cites. Prefer it
over plain web search when a direct answer to a factual question is wanted
rather than a list of links.")
  ("query" :text :required t
   :doc "the question to answer")
  ("sites" :array :of :string
   :doc "optional domains, at most five, that restrict the sources the answer is grounded on"))

;;; --- what Yandex is, and how much of it this cell will read -------------------------

(defparameter +default-base-url+ "https://searchapi.api.cloud.yandex.net"
  "Where the Yandex Search API answers, as the original named it.")

(defparameter +web-search-path+ "/v2/web/search"
  "The classic web search: Base64-encoded XML results.")

(defparameter +gen-search-path+ "/v2/gen/search"
  "The generative search: a JSON grounded answer with citations.")

(defparameter +search-types+
  '("SEARCH_TYPE_RU" "SEARCH_TYPE_TR" "SEARCH_TYPE_COM"
    "SEARCH_TYPE_KK" "SEARCH_TYPE_BE" "SEARCH_TYPE_UZ")
  "The market a query runs against, as the original's VALID_SEARCH_TYPES listed.")

(defparameter +result-ceiling+ 100
  "The most results one web search asks Yandex for, as the original clamped it.")

(defparameter +site-ceiling+ 5
  "The most domains one generative call may restrict its sources to, as the
original's tool description said.")

(defparameter +max-xml-bytes+ (* 2 1024 1024)
  "The largest XML report this cell reads, as the original capped it.")

(defparameter +max-raw-data-chars+ (* 4 (ceiling +max-xml-bytes+ 3))
  "The longest Base64 that can decode to +MAX-XML-BYTES+ or fewer.")

(defparameter +snippet-chars+ 1200
  "How much of one result's snippet a search answer carries.")

(defparameter +used-text-chars+ 300
  "How much of a cited source's used text an answer carries.")

(defparameter +detail-chars+ 500
  "How much of a failed answer's body a refusal quotes, as the original clipped it.")

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun quoted-text (body)
  "BODY -- an HTTP body -- as one clipped line, or the empty string."
  (let ((text (nlk:one-line (or body ""))))
    (if (plusp (length text)) (nlk:clip text +detail-chars+) "")))

;;; --- Base64: what rawData is, read strictly --------------------------------------
;;; The original decoded with validate=True, refusing whitespace and anything
;;; outside the alphabet; so does this. The octet count comes from the length
;;; before anything is built, so no header can make this allocate past the cap.

(defun base64-digit (character)
  "CHARACTER's six bits, or NIL when it is outside the Base64 alphabet."
  (cond ((char<= #\A character #\Z) (- (char-code character) 65))
        ((char<= #\a character #\z) (+ 26 (- (char-code character) 97)))
        ((char<= #\0 character #\9) (+ 52 (- (char-code character) 48)))
        ((char= character #\+) 62)
        ((char= character #\/) 63)
        (t nil)))

(defun base64-octets (text)
  "TEXT, standard Base64 with its padding, as octets. A character outside
the alphabet, a padding in the wrong place, or a length that decodes to
nothing is a refusal."
  (let* ((total (length text))
         (end (or (position #\= text) total))
         (digits (subseq text 0 end))
         (padding (- total end))
         (count (length digits)))
    (unless (plusp count)
      (fail "Yandex answered an empty rawData"))
    (when (> padding 2)
      (fail "Yandex rawData is not valid Base64: ~d padding characters" padding))
    (when (and (< end total) (find #\= text :start end :test-not #'char=))
      (fail "Yandex rawData is not valid Base64: a padding character before the end"))
    (when (= 1 (mod count 4))
      (fail "Yandex rawData is not valid Base64: ~d characters" total))
    (let ((out (make-array (+ (* 3 (floor count 4)) 2)
                            :element-type '(unsigned-byte 8) :fill-pointer 0)))
      (loop for at from 0 below count by 4
            for left = (- count at)
            for a = (base64-digit (char digits at))
            for b = (and (> left 1) (base64-digit (char digits (1+ at))))
            do (unless (and a b)
                 (fail "Yandex rawData is not valid Base64 near character ~d" at))
               (vector-push (logior (ash a 2) (ash b -4)) out)
               (when (>= left 3)
                 (let ((c (base64-digit (char digits (+ at 2)))))
                   (unless c
                     (fail "Yandex rawData is not valid Base64 near character ~d" (+ at 2)))
                   (vector-push (logior (ash (logand b 15) 4) (ash c -2)) out)
                   (when (>= left 4)
                     (let ((d (base64-digit (char digits (+ at 3)))))
                       (unless d
                         (fail "Yandex rawData is not valid Base64 near character ~d" (+ at 3)))
                       (vector-push (logior (ash (logand c 3) 6) d) out)))))
            finally (return (coerce out '(simple-array (unsigned-byte 8) (*))))))))

;;; --- the report's text, and the one shape of it this cell walks --------------------

(defun xml-report (octets)
  "OCTETS as the report's text: at most +MAX-XML-BYTES+, no NUL byte, UTF-8,
and no DTD -- the original's guards, in the same order."
  (when (> (length octets) +max-xml-bytes+)
    (fail "refusing the Yandex XML report: it is ~d bytes, more than the limit of ~d"
          (length octets) +max-xml-bytes+))
  (when (find 0 octets)
    (fail "rejected the Yandex XML report: it is not UTF-8 (a NUL byte)"))
  (let ((text (handler-case (sb-ext:octets-to-string octets :external-format :utf-8)
                (error ()
                  (fail "rejected the Yandex XML report: it is not UTF-8")))))
    (when (cl:search "<!doctype" text :test #'char-equal)
      (fail "rejected the Yandex XML report: it declares a DTD"))
    text))

(defun local-name (tag)
  "TAG without a namespace prefix, as a lowercase-free comparison reads it."
  (let ((colon (position #\: tag)))
    (if colon (subseq tag (1+ colon)) tag)))

(defun element-start (xml name &optional (from 0))
  "The position of the `<' opening the first element whose local name is NAME
at or after FROM, or NIL. A namespace prefix is ignored, as the original
stripped namespaces before walking the tree."
  (loop for open = (position #\< xml :start from)
        while open
        do (let* ((gt (or (position #\> xml :start open) (length xml)))
                  (stop (or (position-if
                             (lambda (character)
                               (member character '(#\Space #\Tab #\Newline #\Return #\/)))
                             xml :start (1+ open) :end gt)
                            gt)))
             (when (string-equal (local-name (subseq xml (1+ open) stop)) name)
               (return open)))
        do (setf from (1+ open))))

(defun closing-tag (xml name &optional (from 0))
  "=> (values TAG-START END) for the first `</NAME>` (a namespace prefix
ignored) at or after FROM: where the `<' stands, and just past its `>'; or
NIL when there is none."
  (loop for at = (cl:search "</" xml :start2 from)
        while at
        do (let* ((gt (or (position #\> xml :start at) (length xml)))
                  (tag (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (subseq xml (+ at 2) gt))))
             (when (string-equal (local-name tag) name)
               (return (values at (1+ gt)))))
        do (setf from (1+ at))))

(defun element-span (xml name &optional (from 0))
  "=> (values CONTENT-START CONTENT-END END) for the first element named NAME
at or after FROM: the span of its content, and the position just past its
closing tag; or NIL when there is none."
  (let* ((open (element-start xml name from))
         (gt (and open (position #\> xml :start open)))
         (content (and gt (1+ gt))))
    (when content
      (multiple-value-bind (close-start close-end) (closing-tag xml name content)
        (when close-start
          (values content close-start close-end))))))

(defun xml-entity (name)
  "The character the XML entity NAME stands for (&amp;, &#39;, &#x27;), or NIL
for one this cell does not know."
  (cond ((string-equal name "amp") #\&)
        ((string-equal name "lt") #\<)
        ((string-equal name "gt") #\>)
        ((string-equal name "quot") #\")
        ((string-equal name "apos") #\')
        ((and (plusp (length name)) (char= (char name 0) #\#))
         (let ((rest (subseq name 1)))
           (if (and (plusp (length rest)) (char-equal (char rest 0) #\x))
               (ignore-errors (code-char (parse-integer rest :start 1 :radix 16)))
               (ignore-errors (code-char (parse-integer rest))))))
        (t nil)))

(defun decode-entities (text)
  "TEXT with the XML entities it escapes decoded; one this cell does not know
is left as it stands, never dropped."
  (with-output-to-string (out)
    (let ((at 0)
          (total (length text)))
      (loop while (< at total)
            do (let* ((character (char text at))
                      (end (and (char= character #\&)
                                (position #\; text :start at :end (min total (+ at 12)))))
                      (decoded (and end (xml-entity (subseq text (1+ at) end)))))
                 (if decoded
                     (progn (write-char decoded out) (setf at (1+ end)))
                     (progn (write-char character out) (incf at))))))))

(defun strip-tags (text)
  "TEXT with every `<...>' span cut out of it -- Yandex's <hlword> highlight
inside a title or passage included -- as the original's itertext flattened it."
  (with-output-to-string (out)
    (let ((in-tag nil))
      (loop for character across text
            do (cond (in-tag (when (char= character #\>) (setf in-tag nil)))
                     ((char= character #\<) (setf in-tag t))
                     (t (write-char character out)))))))

(defun collapse-space (text)
  "TEXT with every run of blanks as one space and both ends trimmed."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (with-output-to-string (out)
                 (let ((blank nil))
                   (loop for character across text
                         do (if (member character '(#\Space #\Tab #\Newline #\Return #\Page))
                                (setf blank t)
                                (progn (when blank (write-char #\Space out))
                                       (setf blank nil)
                                       (write-char character out))))))))

(defun plain-text (text)
  "TEXT as the model reads it: inner tags dropped, entities decoded, blanks
collapsed; NIL is the empty string."
  (if (null text) "" (collapse-space (decode-entities (strip-tags text)))))

(defun element-text (xml name &optional (from 0))
  "The text of the first element named NAME at or after FROM, or NIL."
  (multiple-value-bind (start end) (element-span xml name from)
    (when start (plain-text (subseq xml start end)))))

(defun element-texts (xml name)
  "Every element named NAME in XML, in order, as plain text."
  (loop with from = 0
        for (start end stop) = (multiple-value-list (element-span xml name from))
        while start
        collect (plain-text (subseq xml start end))
        do (setf from stop)))

(defun documents (xml)
  "Each <doc> element's content in the report, in order."
  (loop with from = 0
        for (start end stop) = (multiple-value-list (element-span xml "doc" from))
        while start
        collect (subseq xml start end)
        do (setf from stop)))

(defun report-error (xml)
  "Signal the failure a report carries inside its own envelope, as the
original's _raise_for_xml_error did."
  (let ((text (nonblank (element-text xml "error"))))
    (when text
      (fail "Yandex search error: ~a" text))))

;;; --- the one seam ---------------------------------------------------------------

(defun http (url &key headers content timeout)
  "The cell's one network call; a test stubs this name. => (values BODY STATUS)."
  (let ((timeout (or timeout 30)))
    (nlk:http :post url :headers headers :content content
                      :timeout timeout :connect-timeout (min 10 timeout))))

(defun api-url (path)
  "PATH on the section's root, without a trailing slash on either side."
  (concatenate 'string (string-right-trim "/" (or (setting :base-url) +default-base-url+)) path))

(defun api-headers ()
  "The headers the original sent: its key as Api-Key, JSON in and out."
  (list (cons "authorization" (format nil "Api-Key ~a" (setting :api-key)))
        (cons "content-type" "application/json")
        (cons "accept" "application/json")))

(defun status-detail (body status)
  "Yandex's own sentence for a refused call: its message or error member, the
requestId a details entry carries, else the body's first clipped line."
  (let* ((payload (ignore-errors (nlk:decode-json (or body ""))))
         (message (or (nlk:json-value payload :string "message")
                      (nlk:json-value payload :string "error")))
         (request (loop for detail across (nlk:json-array payload "details")
                        for id = (nlk:json-value detail :string "requestId")
                        when id return id))
         (text (quoted-text body)))
    (format nil "the Yandex Search API answered HTTP ~a~@[: ~a~]~@[ (request ~a)~]~@[: ~a~]"
            status message request (and (not message) (plusp (length text)) text))))

(defun api (path payload)
  "PATH on the Yandex Search API with PAYLOAD and the section's folderId => the
answer's decoded JSON. A refused or unreadable call is a refusal carrying
Yandex's own sentence."
  (let ((body (nlk:copy-json-object payload "folderId" (setting :folder-id))))
    (multiple-value-bind (answer status)
        (handler-case
            (http (api-url path)
                  :headers (api-headers)
                  :content (nlk:encode-json-object body)
                  :timeout (setting :timeout-seconds))
          (error (condition)
            (fail "the Yandex Search API did not answer: ~a"
                  (nlk:one-line (princ-to-string condition)))))
      (unless (and (integerp status) (< status 400))
        (fail "~a" (status-detail answer status)))
      (or (ignore-errors (nlk:decode-json (or answer "")))
          (fail "the Yandex Search API answered non-JSON: HTTP ~a~@[: ~a~]"
                status (let ((text (quoted-text answer)))
                         (and (plusp (length text)) text)))))))

;;; --- classic web search ------------------------------------------------------------

(defun search-payload (query &key (n 5) (page 0))
  "The /v2/web/search body the original sent, member for member."
  (nlk:json-object
   "query" (nlk:json-object "searchType" (setting :search-type)
                             "queryText" query
                             "page" (max 0 page)
                             "familyMode" "FAMILY_MODE_MODERATE"
                             "fixTypoMode" "FIX_TYPO_MODE_ON")
   "groupSpec" (nlk:json-object "groupMode" "GROUP_MODE_DEEP"
                                 "groupsOnPage" n
                                 "docsInGroup" 1)
   "responseFormat" "FORMAT_XML"))

(defun web-results (query &key (n 5) (page 0))
  "QUERY's web results: each a plist of :title, :url, :domain and
:description, at most N of them."
  (let* ((answer (api +web-search-path+ (search-payload query :n n :page page)))
         (raw (nlk:json-value answer :string "rawData")))
    (unless (nonblank raw)
      (fail "the Yandex Search API answered without rawData"))
    (when (> (length raw) +max-raw-data-chars+)
      (fail "refusing the Yandex XML report: its Base64 is ~d characters, more than the limit of ~d"
            (length raw) +max-raw-data-chars+))
    (let* ((xml (xml-report (base64-octets raw)))
           (results (progn (report-error xml) (web-documents xml))))
      (if (> (length results) n) (subseq results 0 n) results))))

(defun web-documents (xml)
  "The report's documents as plists: :title, :url, :domain, :description."
  (loop for doc in (documents xml)
        for url = (or (element-text doc "url") "")
        when (plusp (length url))
          collect (list :url url
                        :title (let ((title (element-text doc "title")))
                                 (if (nonblank title) title url))
                        :domain (or (element-text doc "domain") "")
                        :description (document-description doc))))

(defun document-description (doc)
  "The snippet a document carries: its passages joined, else its headline."
  (let ((passages (remove-if-not #'nonblank (element-texts doc "passage"))))
    (if passages
        (format nil "~{~a~^ ~}" passages)
        (or (element-text doc "headline") ""))))

;;; --- generative search -------------------------------------------------------------

(defun clean-sites (sites)
  "SITES -- a list, a vector, or one string -- as up to +SITE-CEILING+
trimmed non-empty domains, or NIL."
  (let ((sites (remove-if-not #'nonblank
                               (mapcar (lambda (site)
                                         (if (stringp site) (nlk:trimmed site) nil))
                                       (cond ((null sites) '())
                                             ((vectorp sites) (coerce sites 'list))
                                             ((listp sites) sites)
                                             (t (list sites)))))))
    (and sites (subseq sites 0 (min +site-ceiling+ (length sites))))))

(defun generative-payload (query sites)
  "The /v2/gen/search body the original sent, member for member."
  (let ((payload (nlk:json-object
                  "messages" (vector (nlk:json-object "role" "ROLE_USER"
                                                       "content" query))
                  "searchType" (setting :search-type)
                  "fixMisspell" t)))
    (when sites
      (setf (gethash "site" payload) (nlk:json-object "site" (coerce sites 'vector))))
    payload))

(defun generative-answer (query &key sites)
  "QUERY's grounded answer as a plist: :text, :sources, :queries, :fixed,
:rejected and :bullet."
  (let* ((sites (clean-sites sites))
         (answer (api +gen-search-path+ (generative-payload query sites))))
    (answer-of answer)))

(defun answer-of (payload)
  "The generative answer in PAYLOAD, a GenSearchResponse or a one-element
array wrapping one, as a plist."
  (let ((payload (cond ((vectorp payload)
                        (let ((last (car (last (coerce payload 'list)))))
                          (or last (nlk:json-object))))
                       (t payload))))
    (unless (hash-table-p payload)
      (fail "the Yandex generative answer had an unexpected shape"))
    (let ((message (nlk:json-value payload :object "message")))
      (list :text (or (and message (nlk:json-value message :string "content")) "")
            :sources (loop for source across (nlk:json-array payload "sources")
                           when (hash-table-p source)
                             collect (list :url (or (nlk:json-value source :string "url") "")
                                           :title (or (nlk:json-value source :string "title") "")
                                           :used (or (nlk:json-value source :string "usedText") "")))
            :queries (loop for item across (nlk:json-array payload "searchQueries")
                           for text = (if (hash-table-p item)
                                          (nlk:json-value item :string "text")
                                          item)
                           when (and (stringp text) (nonblank text))
                             collect (nlk:trimmed text))
            :fixed (or (nlk:json-value payload :string "fixedMisspellQuery") "")
            :rejected (and (eq t (nlk:json-value payload :any "isAnswerRejected")) t)
            :bullet (and (eq t (nlk:json-value payload :any "isBulletAnswer")) t)))))

;;; --- the answers as lines ------------------------------------------------------------

(defun result-block (result index)
  "One RESULT as the model reads it: a numbered title, its url and domain,
then the snippet."
  (with-output-to-string (out)
    (format out "[~d] ~a~%    ~a" index (getf result :title) (getf result :url))
    (let ((domain (getf result :domain)))
      (when (nonblank domain) (format out " - ~a" domain)))
    (let ((description (getf result :description)))
      (when (nonblank description)
        (format out "~%    ~a" (nlk:clip description +snippet-chars+))))))

(defun web-text (query results)
  "What a web search answers: one block a result, or a line saying Yandex
found none."
  (if (null results)
      (format nil "yandex: no results for ~s" query)
      (format nil "yandex: ~d result~:p for ~s~2%~{~a~^~2%~}"
              (length results) query
              (loop for result in results
                    for index from 1
                    collect (result-block result index)))))

(defun answer-text (query answer)
  "What a generative search answers: the answer, the sources it cites, the
queries Yandex ran, and any correction or rejection it reported."
  (let ((text (nlk:trimmed (getf answer :text)))
        (sources (getf answer :sources))
        (queries (getf answer :queries))
        (fixed (nlk:trimmed (or (getf answer :fixed) ""))))
    (with-output-to-string (out)
      (format out "yandex: answer for ~s~@[ (flagged as rejected)~]~2%"
              query (getf answer :rejected))
      (format out "~a" (if (plusp (length text)) text "Yandex returned no answer text."))
      (when sources
        (format out "~2%Sources:")
        (loop for source in sources
              for index from 1
              for used = (nlk:trimmed (getf source :used))
              do (format out "~%[~d] ~a~%    ~a~@[~%    ~a~]"
                         index
                         (let ((title (nonblank (getf source :title))))
                           (if title title "(untitled)"))
                         (getf source :url)
                         (and (plusp (length used))
                              (nlk:clip used +used-text-chars+)))))
      (when queries
        (format out "~2%Yandex searched: ~{~a~^; ~}" queries))
      (when (plusp (length fixed))
        (format out "~2%Corrected query: ~a" fixed)))))

;;; --- the verbs ------------------------------------------------------------------

(define-verb search (query &key n page)
  "Search the live web through the Yandex Search API and answer ranked
results. QUERY is the search; N 1-100 (the section's max_results when
absent); PAGE the result page, zero-based. => the results as lines: a
numbered title, its url and domain, then the snippet."
  (let ((query (nonblank query)))
    (unless query
      (fail "search needs a query: give one, e.g. (yandex:search \"latest news\")"))
    (let* ((n (max 1 (min +result-ceiling+ (or n (setting :max-results) 5))))
           (page (max 0 (or page 0))))
      (web-text query (web-results query :n n :page page)))))

(define-verb answer (query &key sites)
  "Answer a question with Yandex generative search: one grounded answer
synthesised from live web sources, then the sources it cites. QUERY is the
question; SITES up to five domains to restrict the sources to. => the answer
as lines."
  (let ((query (nonblank query)))
    (unless query
      (fail "answer needs a question: give one, e.g. (yandex:answer \"when was the first metro opened\")"))
    (answer-text query (generative-answer query :sites sites))))

;;; --- the tools the model calls -----------------------------------------------------

(defun web-search-tool (request)
  "The yandex_web_search tool: the members one call carries, answered as the
verb answers them."
  (search (web-search-request-query request)
          :n (web-search-request-result-count request)))

(defun generative-search-tool (request)
  "The yandex_generative_search tool: the members one call carries, answered
as the verb answers them."
  (answer (generative-search-request-query request)
          :sites (generative-search-request-sites request)))

;;; --- the operator's own door ------------------------------------------------------

(defun run-slash (args session-id)
  "/yandex QUERY: the operator's own search, answered as the model's tool is."
  (declare (ignore session-id))
  (let ((query (nonblank args)))
    (if query
        (search query)
        (format nil "yandex: type a query, e.g. /yandex latest news. This cell also gives the model the ~
yandex_web_search and yandex_generative_search tools (~d results when a call names none)."
                (setting :max-results)))))
