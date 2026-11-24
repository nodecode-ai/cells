;;;; lines.lisp --- the search lines: the section read back, one line's request, its cards.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A line is one search backend the operator configured: an HTTP endpoint,
;;;; the field its query goes in, and the environment variables its keys are
;;;; read from. Every byte this cell moves goes through DIAL, so a test stubs
;;;; one name and never touches the network.
;;;;
;;;; Names only. A line's keys are read from the environment at request time
;;;; (CARD-VALUE); no value is ever written to the config, stored, or printed
;;;; in an answer -- a status text says `set' or `missing'.

(in-package #:nodecode-federated-search)

;;; --- the lines a name alone configures --------------------------------------

(defparameter +known-lines+
  '(("tavily" :url "https://api.tavily.com" :path "/search" :query "query"
              :count "max_results" :auth "bearer" :env "TAVILY_API_KEY")
    ("firecrawl" :url "https://api.firecrawl.dev" :path "/v1/search" :query "query"
                 :count "limit" :auth "bearer" :env "FIRECRAWL_API_KEY")
    ("searxng" :path "/search" :query "q" :count "" :auth "" :method "GET"
               :extra (("format" . "json"))))
  "The backends this cell knows by name, as the endpoint each one really is.
A line naming one of these needs no base_url, path or field names of its own:
the original's `use a registered provider' (tavily, firecrawl, searxng), read
here as the request that provider's own API answers. A SearXNG instance is
the operator's own address, so that line still names its base_url.")

(defparameter +count-cap+ 10
  "The most results one line is asked for: the original's cap on a custom
backend's request, kept so a fan-out spends no more of a key than it did.")

(defparameter +default-strategy+ "fill_first"
  "What a line without a pool_strategy of its own uses, when the section names none.")

;;; --- small readers -----------------------------------------------------------

(defun nonblank (value)
  "VALUE, trimmed, when it is a string with something in it; else NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun member-text (object key)
  "KEY of the JSON OBJECT, as text, or NIL."
  (and (hash-table-p object) (nlk:json-value object :text key)))

(defun member-strings (object key)
  "KEY of the JSON OBJECT as a list of the strings it holds; NIL when absent."
  (let ((array (and (hash-table-p object) (nlk:json-value object :array key))))
    (and array (remove-if-not #'stringp (coerce array 'list)))))

(defun env-name-p (text)
  "Whether TEXT can name an environment variable: A-Z, digits, underscore, a letter first."
  (and (stringp text)
       (plusp (length text))
       (<= (length text) 128)
       (char<= #\A (char text 0) #\Z)
       (every (lambda (char) (or (char<= #\A char #\Z) (char<= #\0 char #\9) (char= char #\_)))
              text)))

;;; --- the section read back ----------------------------------------------------

(defun line-or-refusal (entry index strategy)
  "ENTRY -- one object of the backends array -- as the line this cell asks, or
CONFIG-ERROR naming what the entry lacks. INDEX is its place in the array."
  (unless (hash-table-p entry)
    (nlk:config-error "backends[~d] is not an object: a line names at least its \"name\"" index))
  (let* ((name (or (nonblank (member-text entry "name"))
                   (nlk:config-error "backends[~d] has no \"name\": give every line the name it answers under" index)))
         (known (cdr (assoc name +known-lines+ :test #'string-equal)))
         (base (or (nonblank (member-text entry "base_url")) (getf known :url)))
         (path (or (nonblank (member-text entry "search_path")) (getf known :path)))
         (query (or (nonblank (member-text entry "query_param")) (getf known :query) "q"))
         (count (if (member "count_param" (object-keys entry) :test #'string=)
                    (or (member-text entry "count_param") "")
                    (or (getf known :count) "max_results")))
         (auth (if (member "auth_style" (object-keys entry) :test #'string=)
                   (or (member-text entry "auth_style") "")
                   (or (getf known :auth) "bearer")))
         (method (string-upcase (or (nonblank (member-text entry "method")) (getf known :method) "POST")))
         (cards (line-cards entry index known)))
    (unless base
      (nlk:config-error
       "the line ~s needs \"base_url\": the address of its search API ~
(only tavily, firecrawl and searxng are known by name, and a searxng line names its instance)"
       name))
    (unless (member method '("POST" "GET") :test #'string=)
      (nlk:config-error "the line ~s says method ~s: POST or GET" name method))
    (unless (member auth '("" "bearer" "x-api-key") :test #'string=)
      (nlk:config-error "the line ~s says auth_style ~s: bearer, x-api-key, or empty for no header"
                        name auth))
    (let ((own (nonblank (member-text entry "pool_strategy"))))
      (when (and own (not (member own '("fill_first" "round_robin" "random") :test #'string=)))
        (nlk:config-error "the line ~s says pool_strategy ~s: fill_first, round_robin or random"
                          name own))
      (list :name name
            :url (endpoint base path)
            :method method
            :query-param query
            :count-param count
            :auth auth
            :extra (getf known :extra)
            :required (and (eq t (nlk:json-value entry :boolean "required")) t)
            :strategy (or own strategy)
            :cards cards))))

(defun object-keys (object)
  "OBJECT's member names, as a list of strings."
  (when (hash-table-p object)
    (let ((keys '())) (maphash (lambda (key value) (declare (ignore value)) (push key keys)) object) keys)))

(defun line-cards (entry index known)
  "The environment variable names ENTRY's keys are read from, in the order it
lists them; a line with no key at all answers with the one empty card."
  (let* ((listed (member-strings entry "credentials"))
         (single (nonblank (member-text entry "api_key_env")))
         (cards (cond (listed (remove-duplicates listed :test #'string=))
                      (single (list single))
                      ((getf known :env) (list (getf known :env)))
                      (t (list "")))))
    (loop for card in cards
          for at from 0
          unless (or (string= card "") (env-name-p card))
            do (nlk:config-error "backends[~d].credentials[~d] is not an environment variable name: names only (A-Z, digits, _), never the key itself; a pasted value is refused here, not stored" index at))
    cards))

(defun endpoint (base path)
  "BASE and PATH joined at exactly one slash."
  (let ((base (string-right-trim "/" base))
        (path (string-left-trim "/" (or path ""))))
    (if (plusp (length path)) (format nil "~a/~a" base path) base)))

(defun read-lines (table strategy)
  "TABLE -- the section's own object -- as the list of lines; CONFIG-ERROR when
it names none, which is the one setting this cell cannot run without."
  (let ((array (and (hash-table-p table) (nlk:json-value table :array "backends"))))
    (unless (and array (plusp (length array)))
      (nlk:config-error
       "the federated-search section configures no search line: set \"backends\" to an array of ~
backends, e.g. [{\"name\": \"tavily\"}, {\"name\": \"minimax\", \"base_url\": ~
\"https://api.minimaxi.com\", \"search_path\": \"/v1/coding_plan/search\", ~
\"api_key_env\": \"MINIMAX_API_KEY\"}]"))
    (loop for entry across array
          for index from 0
          collect (line-or-refusal entry index strategy))))

(defun read-settings (values table)
  "The declared VALUES, plus the lines read out of the section's own object,
checked against each other once here rather than at every call."
  (let* ((strategy (or (getf values :pool-strategy) +default-strategy+))
         (lines (read-lines table strategy))
         (plist (copy-list values)))
    (when (> (getf values :min-backends) (length lines))
      (nlk:config-error
       "min_backends is ~d but only ~d search line~:p configured: lower min_backends, or add a line"
       (getf values :min-backends) (length lines)))
    (setf (getf plist :lines) lines)
    plist))

;;; --- cards: the keys a line's request may spend --------------------------------

(defvar *cooldowns* (make-hash-table :test #'equal)
  "A card that just failed, and the universal time it is usable again. Process-local,
cleared at every start, as the original's pool is.")

(defvar *rotation* (make-hash-table :test #'equal)
  "The round-robin cursor of each line, by name.")

(defun card-value (card)
  "The value CARD names in the environment: non-blank, one line, else NIL.
The empty card is no key at all, and is always usable."
  (cond ((string= card "") "")
        (t (let ((value (uiop:getenv card)))
             (and (stringp value)
                  (let ((value (nlk:trimmed value)))
                    (and (plusp (length value))
                         (not (find #\Newline value))
                         (not (find #\Return value))
                         value)))))))

(defun cooled-down-p (line card)
  "Whether CARD of LINE is still resting after a failure."
  (> (gethash (format nil "~a|~a" line card) *cooldowns* 0) (get-universal-time)))

(defun cooldown-seconds (status sole)
  "How long a card that failed with STATUS rests: the original's pool times,
with a sole card's transient failures held to a minute."
  (cond ((eql status 429) 3600)
        ((member status '(401 403)) 300)
        (t (if sole 60 300))))

(defun cool-down (line card status sole)
  (setf (gethash (format nil "~a|~a" line card) *cooldowns*)
        (+ (get-universal-time) (cooldown-seconds status sole))))

(defun dial-order (line)
  "LINE's cards, the one this request dials first first, by the line's strategy:
fill_first keeps the declared order, round_robin rotates one card per request,
random picks. A card still cooling is passed over while another is usable."
  (let* ((cards (getf line :cards))
         (usable (remove-if-not #'card-value cards))
         (pool (remove-if (lambda (card) (cooled-down-p (getf line :name) card)) usable))
         (strategy (or (getf line :strategy) +default-strategy+)))
    (cond ((null pool) '())
          ((string= strategy "round_robin")
           (let* ((line-name (getf line :name))
                  (at (mod (gethash line-name *rotation* 0) (length pool))))
             (setf (gethash line-name *rotation*) (1+ at))
             (append (nthcdr at pool) (subseq pool 0 at))))
          ((string= strategy "random")
           (let ((shuffled (copy-list pool)))
             (loop for index downfrom (1- (length shuffled)) to 1
                   for other = (random (1+ index))
                   do (rotatef (nth index shuffled) (nth other shuffled)))
             shuffled))
          (t pool))))

;;; --- one line's request -------------------------------------------------------

(defun auth-headers (line card)
  "LINE's headers: JSON, and the key CARD names in the place LINE says."
  (let ((value (card-value card)))
    (cons '("content-type" . "application/json")
          (cond ((null value) '())
                ((string= (getf line :auth) "x-api-key") (list (cons "x-api-key" value)))
                ((string= (getf line :auth) "bearer") (list (cons "authorization" (format nil "Bearer ~a" value))))
                (t '())))))

(defun query-string (line query limit)
  "LINE's query string: the query under the field it expects, the count, and
whatever the known backend's format asks for."
  (let ((pairs (list (cons (getf line :query-param) query))))
    (when (plusp (length (getf line :count-param)))
      (push (cons (getf line :count-param) (princ-to-string (min limit +count-cap+))) pairs))
    (dolist (pair (getf line :extra)) (push pair pairs))
    (format nil "~{~a=~a~^&~}"
            (loop for (key . value) in (nreverse pairs)
                  append (list (quri:url-encode key :encoding :utf-8)
                               (quri:url-encode value :encoding :utf-8))))))

(defun request-body (line query limit)
  "LINE's POST body: the query under the field it expects, and its count when it takes one."
  (let ((pairs (list (getf line :query-param) query))
        (count (getf line :count-param)))
    (when (plusp (length count))
      (setf pairs (list* count (min limit +count-cap+) pairs)))
    (apply #'nlk:make-json-object pairs)))

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded value; an empty or unreadable one is an empty object."
  (if (and (stringp answer)
           (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer))))
      (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
      (nlk:json-object)))

(defun results-of-array (items pick-title pick-url pick-description)
  "ITEMS read as results: each object through the three readers, non-objects dropped."
  (loop for item across items
        when (hash-table-p item)
          collect (list :title (or (funcall pick-title item) "")
                        :url (or (funcall pick-url item) "")
                        :description (or (funcall pick-description item) ""))))

(defun extract-results (body)
  "BODY's results, in whichever of the three shapes a search API answers with:
{\"organic\": [{title, link, snippet}]}, {\"data\": {\"web\": [{title, url, description}]}}
or {\"results\": [{title, url, content}]}."
  (when (hash-table-p body)
    (let ((organic (nlk:json-value body :array "organic")))
      (if organic
          (results-of-array organic
                             (lambda (item) (member-text item "title"))
                             (lambda (item) (or (member-text item "link") (member-text item "url")))
                             (lambda (item) (or (member-text item "snippet") (member-text item "content"))))
          (let* ((data (nlk:json-value body :object "data"))
                 (web (and data (nlk:json-value data :array "web"))))
            (if web
                (results-of-array web
                                   (lambda (item) (member-text item "title"))
                                   (lambda (item) (member-text item "url"))
                                   (lambda (item) (or (member-text item "description") (member-text item "content"))))
                (let ((raw (nlk:json-value body :array "results")))
                  (and raw
                       (results-of-array raw
                                         (lambda (item) (member-text item "title"))
                                         (lambda (item) (or (member-text item "url") (member-text item "link")))
                                         (lambda (item) (or (member-text item "content") (member-text item "snippet"))))))))))))

(defun refused-text (answer)
  "What a refused answer says, in the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :text "detail")
        (nlk:json-value body :text "message")
        (nlk:json-value body :text "error")
        (and (stringp answer) (plusp (length answer))
             (nlk:clip (nlk:one-line answer) 160 :ellipsis "...")))))

(defun dial (line card query limit timeout)
  "One request to LINE with CARD's key => (values RESULTS DONE STATUS DETAIL):
DONE is the service answering at all, STATUS its HTTP status when it was
refused, DETAIL one line of what went wrong."
  (let* ((get (string= (getf line :method) "GET"))
         (url (if get (format nil "~a?~a" (getf line :url) (query-string line query limit))
                  (getf line :url)))
         (content (and (not get) (nlk:encode-json-object (request-body line query limit)))))
    (handler-case
        (multiple-value-bind (answer status)
            (nlk:http (if get :get :post) url
                      :headers (auth-headers line card)
                      :content content
                      :timeout timeout
                      :connect-timeout (min 10 timeout))
          (cond ((and (integerp status) (< status 300))
                 (values (extract-results (decode-body answer)) t status nil))
                ((integerp status)
                 (values '() nil status (format nil "HTTP ~d~@[ - ~a~]" status (refused-text answer))))
                (t (values '() t status nil))))
      ((or error sb-sys:deadline-timeout) (condition)
        (values '() nil nil (nle:transport-failure-label condition url))))))

(defun ask-line (line query limit timeout)
  "Ask LINE, one card at a time until one answers; a card that fails rests in
*COOLDOWNS* and the request moves to the next, each card dialed at most once.
=> (values RESULTS NOTE): NOTE is the one line an answer carries about this line."
  (let* ((cards (dial-order line))
         (name (getf line :name))
         (sole (= 1 (length (getf line :cards))))
         (detail nil))
    (cond ((null cards)
           (values '() (if (some #'card-value (getf line :cards))
                           (format nil "line ~s: every key is resting after a failure" name)
                           (format nil "line ~s: no key set~@[ (~{~a~^, ~})~]"
                                   name (remove "" (getf line :cards))))))
          (t (dolist (card cards (values '() (and detail (format nil "line ~s: ~a" name detail))))
               (multiple-value-bind (results done status why) (dial line card query limit timeout)
                 (cond (done (return-from ask-line
                               (values results
                                       (and (null results)
                                            (format nil "line ~s answered no results" name)))))
                       (t (setf detail (or why "failed"))
                          (cool-down name card status sole)))))))))
