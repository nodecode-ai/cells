;;;; cell.lisp --- the URLs this session opened, kept in one list.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of aydnOktay's hermes-source-tray (MIT) against Nodecode's
;;;; seams. The original watched Hermes's post_tool_call for web_search,
;;;; web_extract, browser_navigate, browser_open and web_fetch, pulled every
;;;; URL out of the call's arguments and its answer, and kept them in one
;;;; state.json for the whole install, capped at 40.
;;;;
;;;; Here the same seam is Nodecode's :TOOL point: a watched call's answer,
;;;; read once, after the call ran, for the URLs it carries -- a web result's
;;;; JSON, a list of hits, a page's text. The rules are the original's: a
;;;; URL is http or https and one line, at most 500 characters, its host
;;;; lowercased, its fragment dropped; a URL already on the list keeps its
;;;; place and takes the newer title, tool, query and time; past max_items
;;;; the oldest leave. The list is per session, under the session state key
;;;; "source-tray.items", so two sessions never share one tray and a restart
;;;; keeps it. Both tools keep their original names and answers, and
;;;; /sources is the same words: bare or "list" and "clear".

(defpackage #:nodecode-source-tray
  (:use #:cl)
  (:nicknames #:source-tray)
  (:export #:tray-items #:record-sources #:clear-sources #:normalize-url
           #:source-tray-list #:source-tray-clear #:run-sources
           #:*primer* #:*usage*))

(in-package #:nodecode-source-tray)

(nlk:define-peripheral source-tray :not-running t)

;;; --- the words ---------------------------------------------------------------

(defparameter *primer*
  "The source tray is a list of the URLs this session opened through a web
or browser call, kept by the nodecode-source-tray cell: after a watched call
runs, the URLs its arguments and its answer carried are recorded, and
nothing here searches the web or fetches a page of its own.
  source_tray_list   the list as JSON, with a markdown bullet list: the tool
      to call when the operator asks for sources, citations, or what was just
      read. Use only the URLs it answers; never invent one.
  source_tray_clear  empty the list; call it when the operator wants a fresh
      citation list for a new topic, not on your own.
  /sources [list]   the same list for the operator; typed alone it also
      opens a panel with the one row that clears it.
  /sources clear    empty the list.
A URL appears once: opening it again updates its title, tool and time in
place, and the list keeps at most source-tray.max_items of them, the oldest
leaving first. Only calls named in source-tray.tools are read."
  "What (help :sources) answers while the cell runs.")

(defparameter *usage*
  "Usage:
  /sources          the URLs this session opened
  /sources list     the same
  /sources clear    empty the tray"
  "What /sources answers for a word it does not know.")

(defparameter *state-key* "source-tray.items"
  "The session-state key the tray's JSON array sits under: one list a session.")

(defparameter *max-url* 500
  "The longest URL kept, characters: the original's own limit.")

(defparameter *max-title* 160
  "The longest display title kept, characters.")

(defparameter *max-query* 200
  "The longest search query kept, characters.")

(defparameter *max-found* 40
  "The most URLs read out of one call, the original's own cap.")

(defparameter *fallback-max-items* 40
  "The cap when the cell is not running: the original's default.")

(defparameter *default-tools* #("web_search" "web_extract" "web_fetch"
                               "browser_navigate" "browser_open")
  "The calls the tray reads when the section names none: the original's set,
minus nothing, plus Nodecode's own host-fetch name.")

(defparameter +empty+ "(source-tray is empty)"
  "What the markdown list says while the tray holds nothing.")

(defparameter *url-argument-keys* '("url" "href") "A call's own URL arguments.")
(defparameter *url-list-keys* '("urls") "A call's own list of URL arguments.")
(defparameter *query-keys* '("query" "q" "search") "A call's own query arguments.")
(defparameter *url-keys* '("url" "href" "link" "uri") "An answer's own URL members.")
(defparameter *title-keys* '("title" "name" "heading") "An answer's own title members.")
(defparameter *skip-keys* '("content" "markdown" "text" "output" "body" "snippet")
  "An answer's members not walked: a page's own text is scanned, never every URL in it.")

;;; --- a URL, as the original cleaned it ---------------------------------------

(defun url-stops (char)
  "Whether CHAR ends a URL read out of free text: the original's own set."
  (member char '(#\Space #\Tab #\Newline #\Return #\< #\> #\" #\' #\) #\] #\( #\[)))

(defun normalize-url (raw)
  "RAW as a URL the tray keeps, or NIL: http or https, a host, one line, at
most 500 characters, its host lowercased, its fragment dropped and its path
"/" when it names none."
  (let ((text (and (stringp raw)
                   (string-trim '(#\Space #\Tab #\Newline #\Return) raw))))
    (when (and text (plusp (length text)))
      (setf text (string-right-trim ". , ) ; ]"  (string-trim '(#\Space #\Tab) text)))
      (let* ((mark (search "://" text))
             (scheme (and mark (string-downcase (subseq text 0 mark)))))
        (when (and mark (member scheme '("http" "https") :test #'string=))
          (let* ((rest (subseq text (+ mark 3)))
                 (hash (position #\# rest))
                 (rest (if hash (subseq rest 0 hash) rest))
                 (slash (position #\/ rest))
                 (host (string-downcase (if slash (subseq rest 0 slash) rest)))
                 (path (if slash (subseq rest slash) "/"))
                 (whole (concatenate 'string host path)))
            (when (and (plusp (length host))
                       (not (position-if #'url-stops whole))
                       (<= (length (concatenate 'string scheme "://" whole)) *max-url*))
              (concatenate 'string scheme "://" whole))))))))

(defun host-of (url)
  "URL's host, as the original titled a page it found in text."
  (let* ((mark (search "://" url))
         (rest (if mark (subseq url (+ mark 3)) url))
         (slash (position #\/ rest)))
    (if slash (subseq rest 0 slash) rest)))

(defun urls-in-text (text)
  "Every http or https URL in TEXT, in the order they appear."
  (let ((found '())
        (start 0))
    (loop
      (let ((pos (and (< start (length text))
                      (search "http" text :start2 start :test #'char-equal))))
        (unless pos (return))
        (let ((end (or (position-if #'url-stops text :start pos) (length text))))
          (push (subseq text pos end) found)
          (setf start (max (1+ pos) end)))))
    (nreverse found)))

(defun title-of (object url)
  "OBJECT's own title, name or heading, else URL's host: what the list shows."
  (let ((title (and (hash-table-p object)
                    (loop for key in *title-keys*
                          for value = (gethash key object)
                          when (and (stringp value) (plusp (length (string-trim " " value))))
                            return (string-trim " " value)))))
    (let ((title (or title (host-of url) url)))
      (subseq title 0 (min (length title) *max-title*)))))

;;; --- the URLs one call carried ------------------------------------------------

(defun add-hit (found url title)
  "FOUND with (URL . TITLE) at its end, once per URL, the first title standing."
  (if (or (>= (length found) *max-found*)
          (find url found :key #'car :test #'string-equal))
      found
      (append found (list (cons url title)))))

(defun collect-from-object (object found)
  "FOUND plus every URL OBJECT carries: a member's url, href, link or uri,
or a URL in a short string; a page's own text and content are not walked."
  (cond
    ((hash-table-p object)
     (let ((url (loop for key in *url-keys*
                      for value = (gethash key object)
                      when (and (stringp value) (normalize-url value))
                        return (normalize-url value))))
       (when url (setf found (add-hit found url (title-of object url))))
       (loop for key being the hash-keys of object using (hash-value value)
             unless (member key *skip-keys* :test #'string-equal)
               do (setf found
                        (if (and (stringp value) (string= key "_text"))
                            (collect-from-text value found)
                            (collect-from-object value found))))
       found))
    ((and (vectorp object) (not (stringp object)))
     (loop for item across object do (setf found (collect-from-object item found)))
     found)
    ((consp object)
     (loop for item in object do (setf found (collect-from-object item found)))
     found)
    ((and (stringp object) (< (length object) 400)) (collect-from-text object found))
    (t found)))

(defun collect-from-text (text found)
  "FOUND plus every URL in TEXT, each titled by its host."
  (dolist (raw (urls-in-text text))
    (let ((url (normalize-url raw)))
      (when url (setf found (add-hit found url (host-of url))))))
  found)

(defun parse-answer (answer)
  "ANSWER, a call's return, as a value to walk: its JSON when the string
is JSON, else its text."
  (if (stringp answer)
      (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
        (if (plusp (length trimmed))
            (handler-case (nlk:decode-json trimmed)
              (t () (list :text answer)))
            '()))
      answer))

(defun argument-hits (arguments)
  "The URLs a call's own ARGUMENTS name, each titled by its host."
  (let ((hits '()))
    (when (hash-table-p arguments)
      (dolist (key *url-argument-keys*)
        (let ((value (gethash key arguments)))
          (when (and (stringp value) (normalize-url value))
            (setf hits (add-hit hits (normalize-url value) (host-of (normalize-url value)))))))
      (dolist (key *url-list-keys*)
        (let ((value (gethash key arguments)))
          (when (and (vectorp value) (not (stringp value)))
            (loop for item across value
                  when (and (stringp item) (normalize-url item))
                    do (setf hits (add-hit hits (normalize-url item) (host-of (normalize-url item)))))))))
    hits))

(defun query-of (arguments)
  "The search query a call's ARGUMENTS carry, trimmed and clipped."
  (let ((query (and (hash-table-p arguments)
                    (loop for key in *query-keys*
                          for value = (gethash key arguments)
                          when (and (stringp value) (plusp (length (string-trim " " value))))
                            return (string-trim " " value)))))
    (if query (subseq query 0 (min (length query) *max-query*)) "")))

;;; --- the tray ------------------------------------------------------------------

(defun max-items ()
  "The tray's cap: the running cell's max_items, the original's 40 when the
cell is not running."
  (if (running-settings) (setting :max-items) *fallback-max-items*))

(defun watched-tools ()
  "The call names the tray reads: the section's tools, else the original's set."
  (let ((tools (and (running-settings) (setting :tools))))
    (if tools tools *default-tools*)))

(defun watched-p (tool)
  "Whether a call named TOOL is one the tray reads."
  (and (stringp tool)
       (member tool (watched-tools) :test (lambda (a b) (string-equal a b)))))

(defun tray-session ()
  "The session this call belongs to, or NIL: the eval's own, else the turn's."
  (or nlk:*scribe-session-id*
      (getf (nle:turn) :session-id)))

(defun tray-items (session)
  "The tray SESSION holds, oldest first, each (:url :title :tool :query :at)."
  (let ((json (and session (nlk:store-open-p)
                    (ignore-errors (nlk:session-state-get session *state-key*)))))
    (loop for item across (or (and json
                                    (nlk:json-value (nlk:decode-json json) :array))
                               #())
          for url = (nlk:json-value item :string "url")
          when url
            collect (list :url url
                          :title (or (nlk:json-value item :string "title") "")
                          :tool (or (nlk:json-value item :string "tool") "")
                          :query (or (nlk:json-value item :string "query") "")
                          :at (or (nlk:json-value item :string "at") "")))))

(defun (setf tray-items) (items session)
  "Write ITEMS as SESSION's tray."
  (when (and session (nlk:store-open-p))
    (nlk:session-state-put
     session *state-key*
     (nlk:encode-json-object
      (coerce (loop for item in items
                    collect (nlk:make-json-object
                             "url" (getf item :url)
                             "title" (or (getf item :title) "")
                             "tool" (or (getf item :tool) "")
                             "query" (or (getf item :query) "")
                             "at" (or (getf item :at) "")))
              'vector))))
  items)

(defun record-sources (session tool query hits)
  "Record HITS, each (URL . TITLE), under TOOL and QUERY in SESSION's tray: a
URL already on it keeps its place and takes the newer entry, and past
max_items the oldest leave. => the tray."
  (let ((items (tray-items session))
        (at (nlk:iso-now)))
    (dolist (hit hits)
      (let ((url (car hit)))
        (when url
          (let ((entry (find url items :key (lambda (item) (getf item :url))
                                  :test #'string-equal)))
            (if entry
                (setf (getf entry :title) (or (cdr hit) url)
                      (getf entry :tool) (or tool "")
                      (getf entry :query) (or query "")
                      (getf entry :at) at)
                (setf items (append items (list (list :url url
                                                      :title (or (cdr hit) url)
                                                      :tool (or tool "")
                                                      :query (or query "")
                                                      :at at)))))))))
    (let ((limit (max-items)))
      (when (> (length items) limit)
        (setf items (last items limit))))
    (setf (tray-items session) items)))

(defun clear-sources (session)
  "Empty SESSION's tray. => how many URLs it held."
  (let ((count (length (tray-items session))))
    (setf (tray-items session) '())
    count))

(defun tray-markdown (items)
  "ITEMS as the list the operator and the model are shown."
  (if (null items)
      +empty+
      (format nil "~{~a~^~%~}"
              (loop for item in items
                    for index from 1
                    collect (format nil "~d. [~a](~a)"
                                    index (getf item :title) (getf item :url))))))

(defun item-json (item)
  "One tray entry as JSON."
  (nlk:make-json-object "url" (getf item :url)
                         "title" (or (getf item :title) "")
                         "tool" (or (getf item :tool) "")
                         "query" (or (getf item :query) "")
                         "ts" (or (getf item :at) "")))

(defun tray-json (items &key markdown)
  "The tray as the JSON string both tools answer."
  (nlk:encode-json-object
   (apply #'nlk:make-json-object
          "ok" t
          "count" (length items)
          "items" (coerce (loop for item in items collect (item-json item)) 'vector)
          (when markdown (list "markdown" (tray-markdown items))))))

;;; --- the record, at the one seam -----------------------------------------------

(defun note-call (op answer)
  "Record the URLs OP's call carried: its own arguments and its ANSWER.
Nothing here may raise."
  (let ((tool (getf op :name)))
    (when (watched-p tool)
      (let* ((arguments (getf op :arguments))
             (hits (argument-hits arguments))
             (hits (collect-from-object (parse-answer answer) hits))
             (session (tray-session)))
        (when (and hits session)
          (record-sources session tool (query-of arguments) hits))))))

(defun observe-tool (op next)
  "(next OP): the :TOOL point -- a watched call's arguments and answer leave
their URLs on the tray after it ran; every other call passes through
untouched, this cell's own two included."
  (let ((values (multiple-value-list (funcall next op))))
    (ignore-errors (note-call op (first values)))
    (values-list values)))

;;; --- the tools the model calls --------------------------------------------------

(defparameter +markdown-default+ t
  "Whether source_tray_list answers a markdown list unless told otherwise.")

(defun markdown-wanted-p (arguments)
  "Whether a source_tray_list call asked for the markdown list."
  (multiple-value-bind (value present)
      (if (hash-table-p arguments)
          (gethash "markdown" arguments)
          (values nil nil))
    (if present (not (null value)) +markdown-default+)))

(defun source-tray-list (arguments)
  "The source tray as JSON: ok, count, items, and a markdown list unless
the call said \"markdown\": false."
  (let ((items (tray-items (tray-session))))
    (tray-json items :markdown (markdown-wanted-p arguments))))

(defun source-tray-clear (arguments)
  "Empty the source tray: the JSON answer source_tray_clear gives."
  (declare (ignore arguments))
  (clear-sources (tray-session))
  "{\"ok\":true,\"count\":0,\"items\":[]}")

(defparameter +list-schema+
  (list :description
        (concatenate 'string
                     "List the URLs this session opened through a web or browser call "
                     "(search, extract, fetch, navigate), in the order they were first "
                     "opened, each with its title, the call that opened it and the query. "
                     "Use it when the operator asks for sources, citations, or what you "
                     "just read. It searches nothing and fetches nothing: a URL on it is a "
                     "string a call's answer carried. Past the cell's max_items the oldest "
                     "have left; do not invent URLs that are not on it.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "markdown" (nlk:make-json-object
                                   "type" "boolean"
                                   "description" "Also include a markdown bullet list. Default true."
                                   "default" t))
         "required" #()))
  "What the provider is shown for source_tray_list.")

(defparameter +clear-schema+
  (list :description
        "Empty the session's source tray. Use it when the operator wants a fresh citation list for a new topic, not on your own."
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object)
                                           "required" #()))
  "What the provider is shown for source_tray_clear.")

;;; --- /sources -------------------------------------------------------------------

(defun run-sources (args session-id)
  "/sources: the tray as the original's command answered it -- bare or
\"list\" the list (bare also opens the panel whose one row clears it),
\"clear\" to empty it, anything else the usage."
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (char) (member char '(#\Space #\Tab))) text))
         (head (string-downcase (subseq text 0 space)))
         (session (or session-id (tray-session)))
         (items (tray-items session)))
    (cond
      ((string= head "clear")
       (clear-sources session)
       (format nil "source tray cleared~@[ (~d URL~:p went)~]"
               (and (plusp (length items)) (length items))))
      ((member head '("" "list" "show" "md" "markdown") :test #'string=)
       (let ((list (tray-markdown items)))
         (if (string= head "")
             (values list
                     (nle:list-dialog
                      :title "Source tray"
                      :summary (format nil "~d source~:p this session" (length items))
                      :rows (list (nle:run-row "Clear the tray" "/sources clear"
                                                :detail "the session's list starts again"))))
             list)))
      (t *usage*))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell source-tray
  (:section ("source-tray")
    (:guide "nothing here is required: max_items bounds the list; tools names the calls the tray reads, and it records itself from their answers")
    ("max_items" :integer :default 40 :min 1
                 :doc "the most URLs one session's tray keeps; past it the oldest leave")
    ("tools" :list :default (vector "web_search" "web_extract" "web_fetch"
                                     "browser_navigate" "browser_open")
             :doc "the tool calls whose arguments and answers the tray reads; empty reads none"))
  (:tool "source_tray_list" +list-schema+ 'source-tray-list)
  (:tool "source_tray_clear" +clear-schema+ 'source-tray-clear)
  (:hook :tool #'observe-tool)
  (:help :sources "the source tray: the URLs this session opened" *primer*)
  (:command "sources" 'run-sources
            :description "URLs this session opened (list | clear)"))
