;;;; search.lisp --- one request to Grounder, one answer as lines, the tool body.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original's provider.py, in Lisp: the same endpoint
;;;; (POST https://api.grounder.net/v1/search), the same body (query, top_k,
;;;; text_max_chars), the key in the same bearer header, and the same rows out
;;;; -- title, url, and a description that is the result's summary else its
;;;; text, on one line, at most the section's budget. Two things differ, both
;;;; deliberate. The rows are rendered as one block each, which is what a model
;;;; reads, and a failure SIGNALS GROUNDER-ERROR carrying Grounder's own
;;;; sentence, so the model gets the reason rather than a {"success": false}
;;;; object to parse.
;;;;
;;;; Nothing here touches a file or the environment beyond the key the section
;;;; read at start: the only way out is HTTP, the one seam a test stubs.

(in-package #:nodecode-grounder)

;;; --- what Grounder is, as the original named it ------------------------------------

(defparameter +search-path+ "/v1/search"
  "Grounder's search path, appended to the section's base_url.")

(defparameter +result-ceiling+ 20
  "The most results one search asks Grounder for, as the original's MAX_RESULTS.")

(defparameter +query-chars+ 2000
  "Grounder's own limit on a query, as the original's MAX_QUERY_CHARS.")

(defparameter +text-budget+ 800
  "How much text a result may carry unless the section says otherwise, as the
original's DESCRIPTION_CHARS, which it also sent as text_max_chars.")

(defparameter +user-agent+ "nodecode-grounder/0.1.0"
  "The user agent every search names: this cell, at its own version.")

;;; --- the boundary: what a model call carries ---------------------------------------
;;; The original's search(QUERY, LIMIT) over the same two members it sent:
;;; the query, and how many rows to ask for.

(nlk:define-contract search-request
  (:doc "Search the live web through Grounder and answer licensed results from
the providers the query needs. Use it for current facts, recent pages, prices,
scores, odds or flights.")
  ("query" :text :required t
   :doc "the search query sent to Grounder, cut to its first 2000 characters")
  ("limit" :integer
   :doc "how many results to return, 1 to 20; the section's max_results answers when this is absent"))

;;; --- words the section and a call share --------------------------------------------

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun collapse-space (text)
  "TEXT with every run of whitespace as one space and both ends trimmed, as
the original joined a result's text with \" \".split()."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (with-output-to-string (out)
                 (let ((space nil))
                   (loop for character across text
                         do (if (member character '(#\Space #\Tab #\Newline #\Return #\Page))
                                (setf space t)
                                (progn (when space (write-char #\Space out))
                                       (setf space nil)
                                       (write-char character out))))))))

(defun budget ()
  "The section's text budget for one result, never below one character."
  (max 1 (or (setting :description-chars) +text-budget+)))

(defun clamped-limit (limit)
  "LIMIT within 1 and +RESULT-CEILING+, as the original clamped it; the
section's max_results answers when a call names none."
  (let ((wanted (if (integerp limit) limit (or (setting :max-results) 5))))
    (max 1 (min +result-ceiling+ wanted))))

(defun clipped-query (query)
  "QUERY's first +QUERY-CHARS+ characters, as the original sent it."
  (if (> (length query) +query-chars+) (subseq query 0 +query-chars+) query))

(defun endpoint ()
  "The search address: the section's base_url with Grounder's search path."
  (format nil "~a~a" (string-right-trim "/" (or (setting :base-url) "https://api.grounder.net"))
          +search-path+))

;;; --- the one seam ----------------------------------------------------------------

(defun http (method url &key headers content timeout)
  "The cell's one network call; a test stubs this name. => (values BODY
STATUS), every status a value."
  (nlk:http method url :headers headers :content content :timeout (or timeout 45)))

(defun request-body (query limit text-chars)
  "The JSON body one search sends: the query, the rows wanted, the text budget."
  (nlk:encode-json-object
   (nlk:json-object "query" query
                     "top_k" limit
                     "text_max_chars" text-chars)))

(defun ask-grounder (url key body &key timeout)
  "POST BODY to URL with Grounder's search KEY; => (values BODY STATUS).
A request that never reached an answer is a refusal naming the transport fact."
  (handler-case
      (http :post url
            :headers (list (cons "accept" "application/json")
                           (cons "content-type" "application/json")
                           (cons "user-agent" +user-agent+)
                           (cons "authorization" (format nil "Bearer ~a" key)))
            :content body
            :timeout timeout)
    (grounder-error (condition) (error condition))
    (error (condition)
      (fail "Could not reach Grounder: ~a" (nle:transport-failure-label condition url)))))

;;; --- reading Grounder's answer ---------------------------------------------------

(defun decode (text)
  "TEXT as a JSON object, or NIL when it is not one."
  (let ((payload (ignore-errors (nlk:decode-json (or text "")))))
    (and (hash-table-p payload) payload)))

(defun error-message (payload)
  "The message Grounder's own error object carries, or NIL."
  (let ((text (nlk:json-value payload :string "error" "message")))
    (and text (nonblank text))))

(defun payload-of (body status key-env)
  "BODY decoded as Grounder's answer, or a refusal naming what went wrong:
the status, Grounder's own sentence, and the one thing to fix for a key."
  (let ((payload (decode body)))
    (cond
      ((= status 200)
       (or payload (fail "Could not parse the Grounder response as JSON")))
      ((= status 401)
       (fail "Grounder rejected the search key in ~a: check the key on your Search keys page at ~
grounder.net/account/keys, or paste a new one" key-env))
      ((= status 429)
       (fail "Grounder rate limit reached: ~a" (or (and payload (error-message payload))
                                                   "try again later")))
      (t (fail "Grounder returned HTTP ~d~@[: ~a~]" status (and payload (error-message payload)))))))

;;; --- the answer as lines ---------------------------------------------------------

(defun description-of (result)
  "RESULT's summary, else its text, on one line and within the section's
budget: the passages Grounder chose, cut only when a response ignored the
budget the search asked for."
  (let* ((raw (or (nlk:json-value result :string "summary")
                   (nlk:json-value result :string "text")
                   ""))
         (text (collapse-space raw))
         (limit (budget)))
    (if (> (length text) limit)
        (concatenate 'string (string-right-trim " " (subseq text 0 (1- limit))) "…")
        text)))

(defun results-of (payload)
  "PAYLOAD's results, each the fields the original answered: title, url,
description."
  (loop for item across (or (nlk:json-value payload :array "results") #())
        collect (list :title (or (nlk:json-value item :string "title") "")
                      :url (or (nlk:json-value item :string "url") "")
                      :description (description-of item))))

(defun result-block (result index)
  "One RESULT as the model reads it: a numbered title, its url, its description."
  (destructuring-bind (&key title url description &allow-other-keys) result
    (with-output-to-string (out)
      (format out "[~d] ~a" index (if (plusp (length title)) title url))
      (format out "~%    ~a" (if (plusp (length url)) url "(no url)"))
      (when (plusp (length description))
        (format out "~%    ~a" description)))))

(defun search-text (query results &key limit)
  "What a search answers: one block a result, or a line saying Grounder found none."
  (if (null results)
      (format nil "grounder: no results for ~s~@[ (asked for ~d)~]" query limit)
      (format nil "grounder: ~d result~:p for ~s~2%~{~a~^~2%~}"
              (length results) query
              (loop for result in results
                    for index from 1
                    collect (result-block result index)))))

;;; --- the verb ------------------------------------------------------------------

(define-verb search (query &key limit)
  "Search the live web through Grounder and answer licensed results. QUERY is
the search, cut to 2000 characters; LIMIT 1-20 (the section's max_results when
absent). => the results as lines: a numbered title, its url, its description."
  (let ((query (nonblank query)))
    (unless query
      (fail "search needs a query: give one, e.g. (grounder:search \"offshore wind capacity 2026\")"))
    (let* ((limit (clamped-limit limit))
           (query (clipped-query query))
           (env (or (nonblank (setting :api-key-env)) "GROUNDER_API_KEY")))
      (multiple-value-bind (body status)
          (ask-grounder (endpoint) (setting :api-key)
                        (request-body query limit (budget))
                        :timeout (setting :timeout-seconds))
        (search-text query (results-of (payload-of body status env)))))))

;;; --- the tool the model calls -----------------------------------------------------

(defun search-tool (request)
  "The grounder_search tool: the members one call carries, answered as the
verb answers them."
  (search (search-request-query request)
          :limit (search-request-limit request)))

;;; --- the operator's own door ------------------------------------------------------

(defun run-slash (args session-id)
  "/grounder QUERY: the operator's own search, answered as the model's tool is."
  (declare (ignore session-id))
  (let ((query (nonblank args)))
    (if query
        (search query)
        (format nil "grounder: type a query, e.g. /grounder offshore wind capacity 2026. This cell also ~
gives the model the grounder_search tool (~d results when it names no limit)."
                (clamped-limit nil)))))
