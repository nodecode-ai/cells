;;;; mcp.lisp --- the GBrain client: one JSON-RPC call, one ranked page list.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A GBrain server speaks MCP over streamable HTTP: one POST of JSON-RPC,
;;;; and a body that is either plain JSON or one `data:' line per event. Two
;;;; of its tools are called -- volunteer_context (named entities, over a
;;;; short multi-turn window) and query (hybrid topical) -- and their pages
;;;; are merged, de-duplicated and ranked, entity hits first, capped at the
;;;; section's max_pages. Nothing here decides WHY a page is wanted: cell.lisp
;;;; asks, this file answers.
;;;;
;;;; Every function below reads the running cell's settings, so it is called
;;;; under (RUNNING-SETTINGS) from a verb, never at load time.

(in-package #:nodecode-gbrain)

;;; --- reading a decoded JSON value ---------------------------------------------

(defun field (object key)
  "KEY's value in OBJECT, a decoded JSON object, or NIL."
  (when (hash-table-p object)
    (gethash key object)))

(defun elements (value)
  "VALUE, a JSON array, as a list; NIL for anything else."
  (cond ((vectorp value) (coerce value 'list))
        ((consp value) value)
        (t nil)))

(defun trimmed (value)
  (and (stringp value)
       (string-trim '(#\Space #\Tab #\Newline #\Return) value)))

(defun valued (value)
  "VALUE as a non-empty string, or NIL."
  (let ((text (trimmed value)))
    (and text (plusp (length text)) text)))

(defun compact (value)
  "VALUE as one line, clipped, for a refusal."
  (let ((text (cond ((stringp value) value)
                    (t (ignore-errors (nlk:encode-json-object value))))))
    (when (valued text)
      (subseq text 0 (min 200 (length text))))))

;;; --- the credential and the endpoint ------------------------------------------

(defun expand-path (value)
  "VALUE, a path the section names, as a pathname; NIL when it names none."
  (let ((text (valued value)))
    (when text
      (handler-case
          (if (uiop:string-prefix-p "~/" text)
              (merge-pathnames (subseq text 2) (user-homedir-pathname))
              (uiop:parse-native-namestring text))
        (error () (merge-pathnames text (user-homedir-pathname)))))))

(defun token ()
  "The bearer token: the inline setting, else the environment variable the
section names, else the token file GBrain mints."
  (or (valued (setting :token))
      (let ((name (valued (setting :token-env))))
        (and name (valued (uiop:getenv name))))
      (let ((file (expand-path (setting :token-file))))
        (when (and file (probe-file file))
          (valued (ignore-errors (uiop:read-file-string file)))))))

(defun min-confidence ()
  "The section's entity gate, a number, 0.6 when it does not read as one."
  (let ((value (ignore-errors (read-from-string (setting :min-confidence) nil nil))))
    (if (realp value) (coerce value 'single-float) 0.6)))

;;; --- one exchange -------------------------------------------------------------

(defun parse-payload (raw)
  "The first JSON-RPC object in RAW: a JSON body, or the `data:' lines of a
streamable-HTTP event stream. NIL when nothing in it parses."
  (let ((text (trimmed raw)))
    (when text
      (or (loop for line in (uiop:split-string text :separator '(#\Newline))
                for one = (trimmed line)
                for payload = (and one (> (length one) 5)
                                   (uiop:string-prefix-p "data:" (string-downcase one))
                                   (trimmed (subseq one 5)))
                for decoded = (and payload (not (string= payload "[DONE]"))
                                   (ignore-errors (nlk:decode-json payload)))
                when decoded return decoded)
          (ignore-errors (nlk:decode-json text))))))

(defun post (method params)
  "One JSON-RPC exchange with the brain, its reply decoded. Signals
GBRAIN-ERROR when there is no token or nothing usable came back."
  (let ((token (token)))
    (unless token
      (fail "no bearer token: set gbrain.token, gbrain.token_env, or gbrain.token_file to a token the GBrain server minted"))
    (multiple-value-bind (body status)
        (handler-case
            (nlk:http :post (setting :url)
                      :headers (list (cons "Authorization" (format nil "Bearer ~a" token))
                                     (cons "Content-Type" "application/json")
                                     (cons "Accept" "application/json, text/event-stream"))
                      :content (nlk:encode-json-object
                                (nlk:json-object "jsonrpc" "2.0" "id" 1
                                                 "method" method "params" params))
                      :timeout (setting :timeout-seconds))
          (error (condition)
            (fail "GBrain at ~a did not answer: ~a" (setting :url) condition)))
      (cond ((and (integerp status) (>= status 400))
             (fail "GBrain at ~a answered HTTP ~d~@[ - ~a~]"
                   (setting :url) status (compact body)))
            ((null (valued body))
             (fail "GBrain at ~a answered an empty body" (setting :url)))
            (t (or (parse-payload body)
                   (fail "GBrain at ~a answered something that is not JSON-RPC"
                         (setting :url))))))))

(defun tool-text (name arguments)
  "The text a brain tool answered, or NIL when it answered no text."
  (let* ((reply (post "tools/call" (nlk:json-object "name" name "arguments" arguments)))
         (error (field reply "error")))
    (when error
      (fail "GBrain's ~a refused: ~a" name (compact error)))
    (let ((content (field (field reply "result") "content")))
      (let ((parts (loop for part in (elements content)
                         for text = (valued (field part "text"))
                         when text collect text)))
        (and parts (format nil "~{~a~}" parts))))))

;;; --- the two arms ---------------------------------------------------------------

(defun volunteer-pages (window)
  "The pages the brain volunteers for WINDOW: named entities, precision first."
  (let* ((text (tool-text "volunteer_context"
                           (nlk:json-object "window" window
                                            "max_pages" (setting :max-pages)
                                            "min_confidence" (min-confidence))))
         (data (and text (ignore-errors (nlk:decode-json text)))))
    (elements (field data "pages"))))

(defun query-pages (text)
  "The pages a hybrid topical query for TEXT answers: subject matter when the
turn names no entity."
  (let ((query (clipped (strip-injected text) 240)))
    (when (plusp (length query))
      (let* ((text (tool-text "query" (nlk:json-object "query" query
                                                        "limit" (max (setting :max-pages) 8))))
             (data (and text (ignore-errors (nlk:decode-json text)))))
        (or (elements data)
            (elements (field data "results"))
            (elements (field data "hits"))
            (elements (field data "pages")))))))

;;; --- the page, and the block ----------------------------------------------------

(defun clipped (text limit)
  (if (> (length text) limit) (subseq text 0 limit) text))

(defun score (value)
  "VALUE as a float, 0.0 when it is not a number."
  (let ((number (typecase value
                  (number value)
                  (string (ignore-errors (read-from-string value nil nil)))
                  (t nil))))
    (if (realp number) (coerce number 'single-float) 0.0)))

(defun strength (value)
  "A page's score as the block shows it, an em dash for none."
  (let ((number (score value)))
    (if (zerop number) "-" (format nil "~,2f" number))))

(defun clean-synopsis (value &optional (limit nil))
  "VALUE as one line of prose: no frontmatter, headings, tables or markup."
  (let ((text (trimmed value)))
    (when text
      (let ((limit (or limit (setting :synopsis-chars))))
        (when (uiop:string-prefix-p "---" text)
          (let ((parts (uiop:split-string text :separator "---" :limit 3)))
            (when (third parts) (setf text (third parts)))))
        (let ((lines '()) (chars 0))
          (dolist (line (uiop:split-string text :separator '(#\Newline)))
            (let ((one (trimmed line)))
              (unless (or (null one) (uiop:string-prefix-p "#" one)
                          (uiop:string-prefix-p "<!--" one)
                          (uiop:string-prefix-p "|" one)
                          (member one '("---" "***" "```") :test #'string=))
                (setf one (strip-markup one))
                (when (plusp (length one))
                  (push one lines)
                  (incf chars (length one))))))
          (let ((prose (trimmed (format nil "~{~a~^ ~}" (nreverse lines)))))
            (when (and prose (plusp (length prose)))
              (if (<= (length prose) limit)
                  prose
                  (let ((cut (clipped prose (max 1 (1- limit)))))
                    (format nil "~a…" (string-right-trim " ,;:" cut)))))))))))

(defun strip-markup (line)
  "LINE without wiki links, markdown links, emphasis and a bullet."
  (let ((out line))
    (loop for start = (search "[[" out)
          while start
          for end = (search "]]" out :start2 start)
          while end
          do (setf out (concatenate 'string (subseq out 0 start)
                                     (subseq out (+ start 2) end)
                                     (subseq out (+ end 2)))))
    (setf out (cl-ppcre:regex-replace-all "\\[([^]]+)\\]\\([^)]+\\)" out "\\1"))
    (setf out (cl-ppcre:regex-replace-all "[*_`]+" out ""))
    (let ((bare (string-left-trim "-* " out)))
      (trimmed bare))))

;;; --- the window ---------------------------------------------------------------

(defparameter +injected-markers+
  '("## Brain pages (ambient push)"
    "## Brain pages mentioned this turn"
    "## MEMORY budget")
  "Where a block this cell injected starts: a window never re-reads its own output.")

(defun strip-injected (text)
  "TEXT without a block this cell injected earlier, trimmed."
  (let ((out (or text "")))
    (dolist (marker +injected-markers+)
      (let ((at (search marker out)))
        (when at (setf out (subseq out 0 at)))))
    (or (trimmed out) "")))

(defparameter +trivial+
  '("ok" "okay" "k" "yes" "y" "no" "n" "thanks" "thank you" "thx" "ty" "ping" "pong"
    "hi" "hello" "hey" "sup" "gm" "gn" "test" "yo" "sure" "yep" "nope" "cool"
    "got it" "ack" "lgtm" "+1" "-1" "👍" "🙏" "✅" "❌")
  "A reply this short names nothing: the turn pays for no retrieval.")

(defun trivial-p (text)
  "Whether TEXT is a bare acknowledgement, or too short to name anything."
  (let ((bare (string-downcase (string-right-trim " .!?" (trimmed text)))))
    (or (< (length bare) 4)
        (member bare +trivial+ :test #'string=))))

;;; --- ranking -------------------------------------------------------------------

(defun page (raw source)
  "RAW, a decoded brain page, as a plist; NIL when it names no slug."
  (let ((slug (valued (field raw "slug"))))
    (when slug
      (list :display (or (valued (field raw "display")) (valued (field raw "title")) slug)
            :slug slug
            :synopsis (or (clean-synopsis (field raw "synopsis"))
                          (clean-synopsis (field raw "summary"))
                          (clean-synopsis (field raw "description"))
                          (clean-synopsis (field raw "chunk_text"))
                          (clean-synopsis (field raw "snippet"))
                          "")
            :confidence (or (field raw "confidence") (field raw "score")
                            (field raw "rerank_score"))
            :source (or (valued (field raw "source")) source)))))

(defun merge-pages (volunteered queried)
  "Entity hits first, then query hits by score; one page per slug, capped."
  (let ((out '()) (seen (make-hash-table :test #'equal)) (cap (setting :max-pages)))
    (dolist (raw (elements volunteered))
      (let ((one (page raw "volunteer")))
        (when (and one (< (length out) cap) (not (gethash (getf one :slug) seen)))
          (setf (gethash (getf one :slug) seen) t)
          (push one out))))
    (let ((ranked (stable-sort
                   (loop for raw in (elements queried)
                         for one = (page raw "query")
                         when one collect one)
                   #'> :key (lambda (one) (score (getf one :confidence))))))
      (dolist (one ranked)
        (when (and (< (length out) cap) (not (gethash (getf one :slug) seen)))
          (setf (gethash (getf one :slug) seen) t)
          (push one out))))
    (nreverse out)))

(defun recall-pages (text &key (window nil))
  "The brain's pages for TEXT and WINDOW: volunteered entities, then query
hits, merged and ranked. A GBRAIN error reaches the caller."
  (let ((window (or (valued window) (strip-injected text))))
    (merge-pages (and (plusp (length window)) (volunteer-pages window))
                 (query-pages text))))

;;; --- the block -----------------------------------------------------------------

(defun octets (text)
  (length (sb-ext:string-to-octets text :external-format :utf-8)))

(defparameter +framing+
  "## Brain pages (ambient push)~%Top matches by relevance (strength-ordered). Open one with a GBrain page read before relying on its details."
  "What an injected block opens with; the manual quotes it, the model reads it.")

(defun render (pages synopsis-cap)
  "PAGES as the block, each preview cut at SYNOPSIS-CAP characters."
  (let ((lines (list +framing+ "")))
    (loop for one in pages
          for index from 1
          for preview = (if synopsis-cap
                            (let ((text (getf one :synopsis)))
                              (if (> (length text) synopsis-cap)
                                  (format nil "~a…" (string-right-trim " ,;:" (clipped text (1- synopsis-cap))))
                                  text))
                            "")
          do (push (format nil "~d. **~a** -> `~a` (~a · ~a)~@[ - ~a~]"
                           index (getf one :display) (getf one :slug)
                           (strength (getf one :confidence)) (getf one :source) preview)
                   lines))
    (format nil "~{~a~%~}" (nreverse lines))))

(defun fit-context (pages)
  "PAGES as one block under the section's byte ceiling: previews shrink
before any page is dropped."
  (loop for cap in (list (setting :synopsis-chars) 220 120 80 0)
        for text = (render pages cap)
        when (<= (octets text) (setting :max-context-bytes))
          return text
        finally (return (render pages 0))))

(defun format-pages (pages)
  "PAGES as the block the model reads, or NIL when there is none."
  (when pages (fit-context pages)))
