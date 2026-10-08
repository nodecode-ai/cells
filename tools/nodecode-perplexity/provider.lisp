;;;; provider.lisp --- what Perplexity is: its endpoints, its credentials, its search.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/perplexity.kdl (the env name, the jwt-or-never expiry, no refresh),
;;;; coding-agent/src/web/search/providers/perplexity-auth.ts (the
;;;; credentials and their order) and providers/perplexity.ts (the search:
;;;; the ask endpoint's request and its stream, the API's request and its
;;;; stream, the filters), and catalog/src/compat/rules/providers/web.kdl.
;;;;
;;;; omp serves no chat model under this id: Perplexity is one of the engines
;;;; behind its web search tool (the `web' provider's `perplexity' model), and
;;;; the "Perplexity (Pro/Max)" login is the credential that engine spends.
;;;; The models it names are search models: the subscription one a signed-in
;;;; search asks the ask endpoint for (`experimental', which Perplexity calls
;;;; Sonar, unless the operator names another), and the API one a key search
;;;; asks api.perplexity.ai for (`sonar-pro'). So this cell is a search verb,
;;;; (perplexity:search ...), not a provider in the catalog.
;;;;
;;;; A search tries the credentials omp tries, in its order, and the first that
;;;; answers wins:
;;;;
;;;;   PERPLEXITY_COOKIES   a browser's Cookie header, sent to the ask endpoint
;;;;   the sign-in          the account's session token (signin.lisp), sent to
;;;;                        the ask endpoint as the session cookie: a bearer is
;;;;                        ignored there and the request falls back to the
;;;;                        anonymous model
;;;;   an API key           /connect's api_keys.perplexity, else
;;;;                        PERPLEXITY_API_KEY, sent to the chat API; never
;;;;                        while a sign-in is kept, as omp skips it then
;;;;   nobody               the ask endpoint anonymously, only when none of
;;;;                        the above is there

(in-package #:nodecode-perplexity)

(defparameter +site+ "https://www.perplexity.ai"
  "Perplexity's web app, whose endpoints the sign-in and the ask search use.")

(defparameter +ask-url+ "https://www.perplexity.ai/rest/sse/perplexity_ask"
  "The consumer ask endpoint: a streamed answer, for a session, cookies or nobody.")

(defparameter +api-url+ "https://api.perplexity.ai/chat/completions"
  "The API's chat endpoint, for a key.")

(defparameter +api-version+ "2.18"
  "The app API version the requests name (omp's API_VERSION).")

(defparameter +app-user-agent+ "Perplexity/641 CFNetwork/1568 Darwin/25.2.0"
  "The macOS app's user agent, which a signed-in request carries.")

(defparameter +browser-user-agent+
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36"
  "A browser's user agent, which an anonymous ask carries.")

(defparameter +session-cookies+ '("__Secure-next-auth.session-token" "next-auth.session-token")
  "The cookies a Perplexity session lives in, the first the one the ask endpoint reads.")

(defparameter +key-env+ "PERPLEXITY_API_KEY"
  "The variable an API key is read from when /connect saved none.")

(defparameter +cookies-env+ "PERPLEXITY_COOKIES"
  "The variable a browser's whole Cookie header is read from.")

(defparameter +exchange-seconds+ 15
  "How long one sign-in exchange may take.")

(defparameter +search-seconds+ 60
  "How long one search request may take, the answer streamed whole.")

(defparameter +recencies+ '("hour" "day" "week" "month" "year")
  "What a search's recency may name.")

(defun unix-seconds ()
  "Now, in seconds since 1970."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun uuid ()
  "A random UUID, version 4."
  (let ((bytes (nlk:random-bytes 16)))
    (setf (aref bytes 6) (logior #x40 (logand (aref bytes 6) #x0f))
          (aref bytes 8) (logior #x80 (logand (aref bytes 8) #x3f)))
    (flet ((hex (start end) (format nil "~(~{~2,'0x~}~)" (coerce (subseq bytes start end) 'list))))
      (format nil "~a-~a-~a-~a-~a" (hex 0 4) (hex 4 6) (hex 6 8) (hex 8 10) (hex 10 16)))))

;;; --- one exchange ------------------------------------------------------------------------

(defun body-string (body)
  "An HTTP BODY dexador answered, as text."
  (typecase body
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (null "")
    (t (princ-to-string body))))

(defun exchange (method url &key headers content cookie-jar (seconds +exchange-seconds+))
  "One METHOD (:post or :get) to URL within SECONDS, its cookies kept in
COOKIE-JAR when there is one: (values TEXT STATUS), any status answered as a
value; a transport failure signals PERPLEXITY-ERROR."
  (handler-case
      (sb-sys:with-deadline (:seconds seconds)
        (multiple-value-bind (body status)
            (handler-case (if (eq method :post)
                              (dex:post url :headers headers :content content :cookie-jar cookie-jar
                                            :connect-timeout seconds :read-timeout seconds
                                            :use-connection-pool nil)
                              (dex:get url :headers headers :cookie-jar cookie-jar
                                           :connect-timeout seconds :read-timeout seconds
                                           :use-connection-pool nil))
              (dex:http-request-failed (condition)
                (values (dex:response-body condition) (dex:response-status condition))))
          (values (body-string body) status)))
    (perplexity-error (condition) (error condition))
    ((or error sb-sys:deadline-timeout) (condition)
      (fail "~a did not answer: ~a" url (nle:transport-failure-label condition url)))))

(defun ok-p (status)
  "Whether STATUS is a 2xx."
  (and (integerp status) (<= 200 status 299)))

(defun decode (text)
  "TEXT as JSON, or NIL when it is not."
  (ignore-errors (nlk:decode-json text)))

(defun clip (text &optional (limit 300))
  "TEXT cut to LIMIT characters, for an error that quotes a body."
  (let ((text (string-trim '(#\Space #\Newline #\Return #\Tab) (or text ""))))
    (if (> (length text) limit) (subseq text 0 limit) text)))

;;; --- the session's token --------------------------------------------------------------

(defun jwt-expiry (token)
  "The epoch second the JWT TOKEN's exp claim names, or NIL: Perplexity's
session tokens usually carry none, and never expire from here."
  ;; Unverified, as omp reads it.
  (let* ((parts (and (stringp token) (uiop:split-string token :separator ".")))
         (text (and (= (length parts) 3) (second parts))))
    (when (plusp (length text))
      (let* ((claims (ignore-errors
                      (nlk:decode-json
                       (cl-base64:base64-string-to-usb8-array
                        ;; base64url, unpadded: cl-base64's URI alphabet pads with dots
                        (concatenate 'string (substitute #\- #\+ (substitute #\_ #\/ text))
                                     (make-string (mod (- (length text)) 4) :initial-element #\.))
                        :uri t))))
             (exp (and (hash-table-p claims) (nlk:json-value claims :number "exp"))))
        (and exp (floor exp))))))

;;; --- the store ---------------------------------------------------------------------
;;; A sign-in is kept in the shared auth.json under oauth_tokens.perplexity:
;;; access_token (the session token), refresh_token (empty: the rule says
;;; refresh none), expires_at (the token's exp less omp's five minutes, left
;;; out when it names none, which is the usual case), email.

(defvar *store-lock* (bt2:make-lock :name "perplexity store")
  "Held across a read and write of this cell's entry.")

(defun stored-entry (auth)
  "The oauth_tokens.perplexity entry of the parsed store AUTH, or NIL."
  (let ((entry (nlk:json-value auth :object "oauth_tokens" +provider+)))
    (and (nlk:json-value entry :text "access_token") entry)))

(defun save-entry (path entry)
  "Write ENTRY as oauth_tokens.perplexity of the auth.json at PATH, or take
it out when ENTRY is NIL; every other field of the file is kept."
  (let* ((path (merge-pathnames path))
         (auth (or (nle::read-auth-file path) (make-hash-table :test #'equal)))
         (tokens (or (nlk:json-value auth :object "oauth_tokens")
                     (setf (gethash "oauth_tokens" auth) (make-hash-table :test #'equal)))))
    (if entry
        (setf (gethash +provider+ tokens) entry)
        (remhash +provider+ tokens))
    (nlk:write-file-atomically path (shasht:write-json auth nil) :mode #o600 :directory-mode #o700)
    entry))

(defun token-entry (token &optional email)
  "The stored entry the session TOKEN makes, for the account EMAIL."
  (let ((expiry (jwt-expiry token)))
    (nlk:json-object "provider" +provider+
                     "access_token" token
                     "refresh_token" ""
                     ;; omp's getJwtExpiry: the exp claim less five minutes
                     :opt "expires_at" (and expiry (- expiry 300))
                     :opt "email" email)))

(defun usable-p (entry)
  "Whether ENTRY's session token may be sent: it names no expiry, or one
still ahead."
  (let ((expiry (nlk:json-value entry :integer "expires_at")))
    (or (null expiry) (> expiry (unix-seconds)))))

;;; --- which credentials a search tries ---------------------------------------------------

(defun auth-methods (&optional (auth-path nle::*auth-file-path*))
  "The credentials a search tries, in omp's order: (:cookies TEXT),
(:session TOKEN), (:api-key KEY), or (:anonymous) alone when there is none."
  (let* ((auth (ignore-errors (nle::read-auth-file auth-path)))
         (session (stored-entry auth))
         (cookies (nle::credential-env +cookies-env+))
         ;; a kept sign-in is the provider's credential: omp never sends the
         ;; API endpoint a key beside it
         (key (and (null session)
                   (or (nlk:json-value auth :text "api_keys" +provider+ "key")
                       (nle::credential-env +key-env+))))
         (methods (append (and cookies (list (list :cookies cookies)))
                          (and session (usable-p session)
                               (list (list :session (nlk:json-value session :text "access_token"))))
                          (and key (list (list :api-key key))))))
    (or methods (list (list :anonymous)))))

;;; --- the search's arguments ------------------------------------------------------------

(defstruct (filters (:copier nil))
  "What a search narrows to, in Perplexity's own request fields."
  (recency nil)
  (domains nil)
  (after nil)
  (before nil)
  (language nil))

(defun perplexity-date (iso)
  "The ISO date ISO (YYYY-MM-DD) as Perplexity's filters take it, M/D/YYYY."
  (let ((parts (and (stringp iso) (ppcre:scan "^\\d{4}-\\d{2}-\\d{2}$" iso)
                    (uiop:split-string iso :separator "-"))))
    (unless parts
      (fail "a date must be YYYY-MM-DD, got ~s" iso))
    (destructuring-bind (year month day) parts
      (format nil "~d/~d/~a" (parse-integer month) (parse-integer day) year))))

(defun site-host (site)
  "The bare host of SITE (github.com/anthropics -> github.com): the domain
filter takes hosts only."
  (subseq site 0 (or (position #\/ site) (length site))))

(defun domain-filter (domains)
  "DOMAINS, hosts to keep or \"-host\" to drop, as Perplexity's
search_domain_filter: bare hosts, each once, at most 20."
  (let ((entries (coerce (or domains #()) 'list)))
    (unless (every #'stringp entries)
      (fail "domains must be a list of hosts, \"-host\" to drop one"))
    (let ((hosts (remove-duplicates
                  (loop for entry in entries
                        for trimmed = (string-trim " " entry)
                        when (plusp (length trimmed))
                          collect (if (char= #\- (char trimmed 0))
                                      (concatenate 'string "-" (site-host (subseq trimmed 1)))
                                      (site-host trimmed)))
                  :test #'string= :from-end t)))
      (subseq hosts 0 (min 20 (length hosts))))))

(defun make-search-filters (&key recency domains after before language)
  "The FILTERS a search's keywords name, checked."
  (when (and recency (not (member recency +recencies+ :test #'equal)))
    (fail "recency must be one of ~{~a~^, ~}, got ~s" +recencies+ recency))
  (make-filters :recency recency
                :domains (domain-filter domains)
                :after (and after (perplexity-date after))
                :before (and before (perplexity-date before))
                ;; ISO 639-1: en-us is en; anything else is not narrowed
                :language (and (stringp language)
                               (ppcre:register-groups-bind (code)
                                   ("^([a-z]{2})(?:[-_]|$)" (string-downcase language))
                                 code))))

(defun dated-p (filters)
  "Whether FILTERS name a date bound, which outranks recency: the API refuses both."
  (or (filters-after filters) (filters-before filters)))

;;; --- the stream ---------------------------------------------------------------------

(defun sse-data (text)
  "The data payloads of the server-sent events in TEXT, in order."
  (let ((events '()) (data '()))
    (flet ((flush ()
             (when data
               (push (format nil "~{~a~^~%~}" (reverse data)) events)
               (setf data nil))))
      (dolist (line (uiop:split-string text :separator '(#\Newline)))
        (let ((line (string-right-trim '(#\Return) line)))
          (cond ((zerop (length line)) (flush))
                ((uiop:string-prefix-p "data:" line)
                 (push (string-left-trim " " (subseq line 5)) data)))))
      (flush))
    (nreverse events)))

(defun source (title url snippet date)
  "One source a search read, as the answer lists it."
  (list :title (or title url) :url url :snippet snippet :date date))

;;; --- the ask endpoint: a session, cookies, or nobody ---------------------------------------
;;; The ask endpoint streams snapshots of one answer: each event carries some
;;; of the answer's blocks, which merge by their intended_usage, a markdown
;;; block's chunks splicing in at chunk_starting_offset. The answer is the
;;; latest snapshot's markdown, else its ask_text block, else its text, which
;;; may itself be a JSON payload carrying the answer.

(defun merge-markdown (existing incoming)
  "The markdown block EXISTING with INCOMING's fields, its chunks spliced in."
  (if (null existing)
      (nlk:copy-json-object incoming)
      (let ((result (nlk:copy-json-object existing))
            (chunks (nlk:json-value incoming :array "chunks")))
        (maphash (lambda (key value) (setf (gethash key result) value)) incoming)
        (when (plusp (length chunks))
          (let ((offset (or (nlk:json-value incoming :integer "chunk_starting_offset") 0))
                (old (or (nlk:json-value existing :array "chunks") #())))
            (setf (gethash "chunks" result)
                  (if (zerop offset)
                      chunks
                      (concatenate 'vector (subseq old 0 (min offset (length old))) chunks)))))
        result)))

(defun merge-blocks (existing incoming)
  "The blocks EXISTING with INCOMING merged in by intended_usage, in first-seen order."
  (let ((order '()) (table (make-hash-table :test 'equal)))
    (flet ((put (usage block)
             (unless (nth-value 1 (gethash usage table)) (push usage order))
             (setf (gethash usage table) block)))
      (loop for block across existing
            for usage = (nlk:json-value block :text "intended_usage")
            when usage do (put usage block))
      (loop for block across incoming
            for usage = (nlk:json-value block :text "intended_usage")
            when usage
              do (let* ((prior (gethash usage table))
                        (merged (if prior (nlk:copy-json-object prior) (make-hash-table :test 'equal))))
                   (maphash (lambda (key value) (setf (gethash key merged) value)) block)
                   (alexandria:when-let (markdown (nlk:json-value block :object "markdown_block"))
                     (setf (gethash "markdown_block" merged)
                           (merge-markdown (nlk:json-value prior :object "markdown_block") markdown)))
                   (put usage merged))))
    (map 'vector (lambda (usage) (gethash usage table)) (reverse order))))

(defun merge-event (existing incoming)
  "The snapshot EXISTING with the event INCOMING merged in."
  (let ((merged (nlk:copy-json-object existing))
        (blocks (nlk:json-value incoming :array "blocks"))
        (prior-blocks (or (nlk:json-value existing :array "blocks") #())))
    (maphash (lambda (key value) (setf (gethash key merged) value)) incoming)
    (setf (gethash "blocks" merged)
          (if (plusp (length blocks)) (merge-blocks prior-blocks blocks) prior-blocks))
    (when (and (null (nlk:json-value merged :array "sources_list"))
               (nlk:json-value existing :array "sources_list"))
      (setf (gethash "sources_list" merged) (nlk:json-value existing :array "sources_list")))
    merged))

(defun chunks-text (value)
  "The strings of the array VALUE joined, or NIL when it is empty, holds
anything else, or joins to nothing."
  (when (and (vectorp value) (not (stringp value)) (plusp (length value)) (every #'stringp value))
    (let ((text (apply #'concatenate 'string (coerce value 'list))))
      (and (plusp (length text)) text))))

(defun payload-answer (payload)
  "The answer a text payload object carries: its structured answer, its
chunks, or its answer."
  (or (loop for item across (or (nlk:json-value payload :array "structured_answer") #())
            thereis (and (hash-table-p item)
                         (or (nlk:json-value item :text "text")
                             (chunks-text (gethash "chunks" item)))))
      (chunks-text (gethash "chunks" payload))
      (nlk:json-value payload :text "answer")))

(defun text-payload (text)
  "The payload object the event TEXT is, or the first step's answer that
decodes to one, or NIL."
  (let ((parsed (decode text)))
    (cond ((hash-table-p parsed) parsed)
          ((vectorp parsed)
           (loop for step across parsed
                 for answer = (nlk:json-value step :text "content" "answer")
                 for payload = (and answer (decode answer))
                 when (hash-table-p payload) return payload)))))

(defun text-answer (text)
  "The answer the event TEXT carries, else TEXT itself."
  (or (let ((payload (text-payload text))) (and payload (payload-answer payload)))
      (let ((parsed (decode text)))
        (and (vectorp parsed) (not (stringp parsed))
             (loop for step across parsed
                   thereis (nlk:json-value step :text "content" "answer"))))
      text))

(defun markdown-answer (markdown)
  "(KIND . VALUE) of the markdown block MARKDOWN's answer: (:chunks . ARRAY),
(:answer . TEXT), or NIL."
  (let ((chunks (nlk:json-value markdown :array "chunks"))
        (answer (nlk:json-value markdown :text "answer")))
    (cond ((plusp (length chunks)) (cons :chunks chunks))
          (answer (cons :answer answer)))))

(defun answer-source (event)
  "Where the snapshot EVENT's answer comes from, as (KIND . VALUE), or NIL."
  (let ((blocks (or (nlk:json-value event :array "blocks") #()))
        (text (gethash "text" event)))
    (flet ((block-markdown (predicate)
             (loop for block across blocks
                   for markdown = (nlk:json-value block :object "markdown_block")
                   when (and markdown (funcall predicate (or (nlk:json-value block :string "intended_usage") "")))
                     return markdown)))
      (if (zerop (length blocks))
          (and (stringp text) (cons :text text))
          (or (markdown-answer (block-markdown (lambda (usage) (cl:search "markdown" usage))))
              (markdown-answer (block-markdown (lambda (usage) (string= usage "ask_text"))))
              (and (stringp text) (plusp (length text)) (cons :text text)))))))

(defun answer-text (source)
  "The answer SOURCE, an ANSWER-SOURCE, says."
  (ecase (car source)
    (:chunks (format nil "~{~a~}" (map 'list (lambda (chunk) (if (stringp chunk) chunk "")) (cdr source))))
    (:answer (cdr source))
    (:text (text-answer (cdr source)))))

(defun event-sources (event)
  "The sources the snapshot EVENT names: its web results block, else its
sources list, else the web results its text payload carries."
  (let* ((blocks (or (nlk:json-value event :array "blocks") #()))
         (web (find "web_results" blocks :key (lambda (block) (nlk:json-value block :string "intended_usage"))
                                         :test #'equal))
         (results (nlk:json-value web :array "web_result_block" "web_results")))
    (flet ((rows (array title url snippet date)
             (loop for row across (or array #())
                   for link = (nlk:json-value row :text url)
                   when link
                     collect (source (nlk:json-value row :text title) link
                                     (nlk:json-value row :string snippet)
                                     (nlk:json-value row :string date)))))
      (or (rows results "name" "url" "snippet" "timestamp")
          (rows (nlk:json-value event :array "sources_list") "title" "url" "snippet" "date")
          (let ((payload (and (stringp (gethash "text" event)) (text-payload (gethash "text" event)))))
            (loop for row across (or (nlk:json-value payload :array "web_results") #())
                  for link = (nlk:json-value row :text "url")
                  when link
                    collect (source (or (nlk:json-value row :text "name") (nlk:json-value row :text "title"))
                                    link (nlk:json-value row :string "snippet")
                                    (nlk:json-value row :string "timestamp"))))))))

(defun ask-headers (kind credential request-id)
  "The headers an ask of KIND (:session, :cookies, :anonymous) carries."
  `(("content-type" . "application/json")
    ("accept" . "text/event-stream")
    ("origin" . ,+site+)
    ("referer" . ,(format nil "~a/" +site+))
    ("user-agent" . ,(if (eq kind :anonymous) +browser-user-agent+ +app-user-agent+))
    ("x-request-id" . ,request-id)
    ;; the ask endpoint authenticates by the session cookie, never a bearer
    ,@(case kind
        (:session `(("cookie" . ,(format nil "~a=~a" (first +session-cookies+) credential))))
        (:cookies `(("cookie" . ,credential))))
    ,@(unless (eq kind :anonymous)
        `(("x-app-apiclient" . "default")
          ("x-app-apiversion" . ,+api-version+)
          ("x-perplexity-request-reason" . "submit")))))

(defun ask-body (kind query filters)
  "The ask request for QUERY under FILTERS, as omp's callPerplexityAsk sends it."
  ;; The bare query: the ask endpoint has no system slot, and a prompt
  ;; prepended to the query reads to it as a meta-instruction.
  (let ((params (nlk:json-object
                 "query_str" query
                 "search_focus" "internet"
                 "mode" "copilot"
                 "model_preference" (setting :model)
                 "sources" (vector "web")
                 "attachments" (vector)
                 "frontend_uuid" (uuid)
                 "frontend_context_uuid" (uuid)
                 "version" +api-version+
                 "language" "en-US"
                 "timezone" "UTC"
                 ;; recency cannot sit beside a date bound; the bound wins
                 "search_recency_filter" (or (and (not (dated-p filters)) (filters-recency filters)) :null)
                 "is_incognito" t
                 "use_schematized_api" t
                 ;; a search tool always retrieves: the skip classifier off,
                 ;; and the override the web client sets when it fires anyway
                 "skip_search_enabled" :false
                 "always_search_override" t
                 "prompt_source" "user"
                 "source" "default"
                 "local_search_enabled" :false
                 ;; no approval UI and no local browser agent to wait on
                 "should_ask_for_mcp_tool_confirmation" :false
                 "supports_tool_approval_modal" :false
                 "force_enable_browser_agent" :false
                 "is_local_browser_available" :false
                 "is_local_browser_allowed" :false
                 :when (eq kind :anonymous) "send_back_text_in_streaming_api" t
                 :when (filters-domains filters) "search_domain_filter" (coerce (filters-domains filters) 'vector)
                 :opt "search_after_date_filter" (filters-after filters)
                 :opt "search_before_date_filter" (filters-before filters)
                 :when (filters-language filters) "search_language_filter" (vector (filters-language filters)))))
    (nlk:encode-json-object (nlk:json-object "query_str" query "params" params))))

(defun post-search (url headers content)
  "POST CONTENT to URL for a search: (values TEXT STATUS). A transport
failure is tried once more; an HTTP answer, a refusal included, never is."
  ;; The ask endpoint intermittently drops the socket before it answers (omp #5315).
  (handler-case (exchange :post url :headers headers :content content :seconds +search-seconds+)
    (perplexity-error ()
      (exchange :post url :headers headers :content content :seconds +search-seconds+))))

(defun ask (kind credential query filters)
  "Ask the ask endpoint QUERY as KIND with CREDENTIAL: a plist (:answer
:sources :model :request-id)."
  (let ((request-id (uuid)))
    (multiple-value-bind (text status)
        (post-search +ask-url+ (ask-headers kind credential request-id) (ask-body kind query filters))
      (unless (ok-p status)
        (fail "Perplexity ask API error (~a): ~a" status (clip text)))
      (let ((merged (make-hash-table :test 'equal))
            (answer nil) (sources '()) (model nil) (final-id nil))
        (dolist (data (sse-data text))
          (let ((event (decode data)))
            (when (hash-table-p event)
              (alexandria:when-let (code (nlk:json-value event :text "error_code"))
                (fail "Perplexity ask stream error: ~a" (or (nlk:json-value event :text "error_message") code)))
              (setf merged (merge-event merged event))
              (let ((source (answer-source merged)))
                (when (and source (plusp (length (answer-text source))))
                  (setf answer source)))
              ;; a source seen again keeps its place and takes the newer row
              (dolist (row (event-sources merged))
                (let* ((key (string-right-trim "/" (string-trim " " (getf row :url))))
                       (cell (assoc key sources :test #'string=)))
                  (if cell (setf (cdr cell) row) (setf sources (append sources (list (cons key row)))))))
              (alexandria:when-let (reported (find-if (lambda (name) (and name (string/= name "turbo")))
                                                      (list (nlk:json-value merged :text "user_selected_model")
                                                            (nlk:json-value merged :text "display_model"))))
                (setf model reported))
              (alexandria:when-let (id (nlk:json-value merged :text "uuid")) (setf final-id id))
              (when (or (eq t (gethash "final" merged))
                        (equal "COMPLETED" (nlk:json-value merged :string "status")))
                (return)))))
        ;; Anonymous quota exhaustion answers 200 with a short signup wall and
        ;; no sources; a grounded anonymous ask always has some.
        (when (and (eq kind :anonymous) (null sources))
          (fail "Perplexity anonymous ask returned no sources (likely signup wall or exhausted anonymous quota); sign in with /perplexity login or set ~a"
                +key-env+))
        (list :answer (and answer (answer-text answer))
              :sources (mapcar #'cdr sources)
              :model (or model (if (eq kind :anonymous)
                                   (nlk:json-value merged :text "display_model")
                                   (setting :model)))
              :request-id (or final-id request-id))))))

;;; --- the API: a key -------------------------------------------------------------------

(defun api-body (query filters)
  "The chat request for QUERY under FILTERS, with Perplexity's search fields
as omp sends them."
  (nlk:encode-json-object
   (nlk:json-object "model" (setting :api-model)
                    "messages" (vector (nlk:json-object "role" "user" "content" query))
                    "max_tokens" 8192
                    "temperature" 0.2
                    "stream" t
                    "stream_options" (nlk:json-object "include_usage" t)
                    "search_mode" "web"
                    "num_search_results" 20
                    "web_search_options" (nlk:json-object "search_type" "pro" "search_context_size" "high")
                    "enable_search_classifier" t
                    "reasoning_effort" "medium"
                    "language_preference" "en"
                    "return_related_questions" t
                    :when (and (filters-recency filters) (not (dated-p filters)))
                    "search_recency_filter" (filters-recency filters)
                    :when (filters-domains filters) "search_domain_filter" (coerce (filters-domains filters) 'vector)
                    :opt "search_after_date_filter" (filters-after filters)
                    :opt "search_before_date_filter" (filters-before filters)
                    :when (filters-language filters)
                    "search_language_filter" (vector (filters-language filters)))))

(defun api-search (key query filters)
  "Ask the API QUERY with KEY: a plist (:answer :sources :related :model :request-id)."
  (multiple-value-bind (text status)
      (post-search +api-url+ `(("content-type" . "application/json")
                               ("accept" . "text/event-stream")
                               ("authorization" . ,(format nil "Bearer ~a" key)))
                   (api-body query filters))
    (unless (ok-p status)
      (fail "Perplexity API error (~a): ~a" status (clip text)))
    (let ((answer (make-string-output-stream))
          (id nil) (model nil) (citations nil) (results nil) (related nil))
      (dolist (data (sse-data text))
        (when (equal data "[DONE]") (return))
        (let ((record (decode data)))
          (when (hash-table-p record)
            (alexandria:when-let (failure (nlk:json-value record :object "error"))
              (fail "Perplexity API stream error: ~a"
                    (or (nlk:json-value failure :text "message") (clip data))))
            (alexandria:when-let (value (nlk:json-value record :text "id")) (setf id value))
            (alexandria:when-let (value (nlk:json-value record :text "model")) (setf model value))
            (alexandria:when-let (value (nlk:json-value record :array "citations")) (setf citations value))
            (alexandria:when-let (value (or (nlk:json-value record :array "search_results")
                                            (nlk:json-value record :array "results")))
              (setf results value))
            (alexandria:when-let (value (nlk:json-value record :array "related_questions")) (setf related value))
            (let ((choice (let ((choices (nlk:json-value record :array "choices")))
                            (and (plusp (length choices)) (aref choices 0)))))
              (alexandria:when-let (delta (or (nlk:json-value choice :string "delta" "content")
                                              (nlk:json-value choice :string "message" "content")))
                (write-string delta answer))))))
      (flet ((result-row (row)
               (source (nlk:json-value row :text "title") (nlk:json-value row :text "url")
                       (nlk:json-value row :string "snippet") (nlk:json-value row :string "date"))))
        (let* ((rows (loop for row across (or results #())
                           when (nlk:json-value row :text "url") collect row))
               (urls (remove-if-not (lambda (url) (and (stringp url) (plusp (length url))))
                                    (coerce (or citations #()) 'list))))
          (list :answer (let ((text (get-output-stream-string answer))) (and (plusp (length text)) text))
                ;; the citations, in order, with what the results say of each;
                ;; with no citations, the results themselves
                :sources (if urls
                             (mapcar (lambda (url)
                                       (let ((row (find url rows :key (lambda (row) (nlk:json-value row :text "url"))
                                                                 :test #'string=)))
                                         (if row (result-row row) (source nil url nil nil))))
                                     urls)
                             (mapcar #'result-row rows))
                :related (remove-if-not (lambda (question) (and (stringp question)
                                                                 (plusp (length (string-trim " " question)))))
                                        (coerce (or related #()) 'list))
                :model model
                :request-id id))))))

;;; --- the answer the model reads ---------------------------------------------------------

(defparameter +modes+ '((:cookies . "PERPLEXITY_COOKIES") (:session . "the Perplexity sign-in")
                        (:api-key . "the Perplexity API key") (:anonymous . "Perplexity, anonymously"))
  "How the answer names the credential that served it.")

(defun render (result kind n)
  "RESULT, a search's plist, as the text the model reads: the answer, the
sources (at most N), the related questions, and who answered."
  (let ((sources (getf result :sources)))
    (with-output-to-string (out)
      (format out "~a~%" (or (getf result :answer) "(Perplexity gave no answer text)"))
      (when sources
        (format out "~%Sources:~%")
        (loop for row in sources
              for i from 1
              while (or (null n) (<= i n))
              do (format out "~d. ~a~%   ~a~@[~%   ~a~]~@[ (~a)~]~%"
                         i (getf row :title) (getf row :url)
                         (let ((snippet (getf row :snippet)))
                           (and snippet (plusp (length snippet)) (clip snippet 300)))
                         (getf row :date))))
      (alexandria:when-let (related (getf result :related))
        (format out "~%Related:~%~{- ~a~%~}" related))
      (format out "~%[answered by ~a~@[, model ~a~]]" (cdr (assoc kind +modes+)) (getf result :model)))))

(define-verb search (query &key n recency domains after before language)
  "Answer QUERY from a live Perplexity search: the answer, its sources, related questions."
  (unless (and (stringp query) (plusp (length (string-trim " " query))))
    (fail "query must be a non-empty string"))
  (unless (or (null n) (and (integerp n) (plusp n)))
    (fail "n must be a positive integer, got ~s" n))
  (let ((filters (make-search-filters :recency recency :domains domains :after after :before before
                                      :language language))
        (last nil))
    (dolist (method (auth-methods) (error last))
      (destructuring-bind (kind &optional credential) method
        (handler-case
            (return (render (if (eq kind :api-key)
                                (api-search credential query filters)
                                (ask kind credential query filters))
                            kind n))
          (perplexity-error (condition) (setf last condition)))))))
