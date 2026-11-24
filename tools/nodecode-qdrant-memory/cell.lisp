;;;; cell.lisp --- the four qdrant_memory_* tools, the recall, the capture.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :qdrant-memory) answers the primer below, and every request's help
;;;; section carries one line naming it. Everything the cell says to Qdrant is
;;;; NLK:HTTP against the store's REST API, and the vector of a memory comes
;;;; from the embedding endpoint in the section -- nothing else is spoken to.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "qdrant_memory": {"url": "http://127.0.0.1:6333",
;;;;                     "collection": "nodecode_memory",
;;;;                     "embedding_url": "http://127.0.0.1:11434/api/embed",
;;;;                     "embedding_model": "nomic-embed-text"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from cnkang's Hermes Agent plugin (see NOTICE): the four scoped
;;;; tools and their argument schemas, the canonical-text deduplication, the
;;;; namespace scoping, the prefetch of what the store holds about the newest
;;;; ask, and the background capture of finished turns. The Python runtime of
;;;; the original -- an embedded Qdrant client, an ONNX model, an LLM
;;;; extractor, a SQLite admission ledger and an optional shared owner
;;;; service -- is not carried: a cell speaks the REST API of a Qdrant the
;;;; operator already runs, and takes its vectors from an embedding endpoint.

(in-package #:nodecode-qdrant-memory)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Qdrant memory is available through the nodecode-qdrant-memory cell: a Qdrant collection of the
user's durable facts, recalled by meaning. Four tools, which a person can also call from eval in the
qdrant-memory package:
  qdrant_memory_search {query, limit?, namespace?}
      The stored memories nearest the query, with their ids and scores, newest first. Search
      before answering anything that depends on what the user said before (preferences, names,
      decisions, projects), and before adding a fact so it is not stored twice. Nothing found is
      not a failure: it is one sentence saying the store holds nothing about the query.
  qdrant_memory_add {text, tags?, source?}
      Store one durable fact as one self-contained sentence (\"Prefers green tea over coffee\"),
      with the name it is about. The answer carries the id update and delete take; an identical
      fact already stored answers stored=false with that id instead of a second copy.
  qdrant_memory_update {id, text?}
      Correct a stored memory in place: new text is re-embedded, so it is found by the new
      wording; no text stamps it as reviewed. An id the store does not hold is a refusal.
  qdrant_memory_delete {id}
      Forget a memory for good. Read it with qdrant_memory_search first so the id is the memory
      meant.
Doctrine: one fact per memory, self-contained, no pronouns. Recall rides every request already
(the store is searched for the newest ask), so a search is for a question the prefetched notes did
not answer. ERROR: QDRANT-MEMORY-ERROR names the problem."
  "What (help :qdrant-memory) answers while the cell runs.")

;;; --- small helpers -------------------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, or NIL when it is blank."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun clipped (text chars)
  (nlk:clip (or text "") chars))

(defun canonical-text (text)
  "TEXT as the key a duplicate is found by: lowercased, its runs of
whitespace folded to one space, its edges trimmed."
  (string-downcase
   (with-output-to-string (out)
     (let ((space nil))
       (loop for character across (nlk:trimmed (or text ""))
             do (if (member character '(#\Space #\Tab #\Newline #\Return))
                    (setf space t)
                    (progn (when space (write-char #\Space out))
                           (setf space nil)
                           (write-char character out))))))))

(defun uuid ()
  "A random UUID, version 4: the point id a memory is written and read by."
  (let ((bytes (nlk:random-bytes 16)))
    (setf (aref bytes 6) (logior #x40 (logand (aref bytes 6) #x0f))
          (aref bytes 8) (logior #x80 (logand (aref bytes 8) #x3f)))
    (flet ((hex (start end)
             (format nil "~(~{~2,'0x~}~)" (coerce (subseq bytes start end) 'list))))
      (format nil "~a-~a-~a-~a-~a" (hex 0 4) (hex 4 6) (hex 6 8) (hex 8 10) (hex 10 16)))))

(defun answer-failure (format-control &rest args)
  "The model's answer when a call cannot go on, as the call's failure."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (qdrant-memory-error (condition)
       (nle:failure (format nil "ERROR: ~a" (qdrant-memory-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Qdrant memory: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

;;; --- reading the section --------------------------------------------------------

(defun base-url (text what)
  "TEXT without a trailing slash, refused unless it carries a scheme."
  (let ((url (string-right-trim "/" (or text ""))))
    (unless (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
      (nlk:config-error "~a ~s has no http:// or https:// scheme: set the full base URL, e.g. \"http://127.0.0.1:6333\"" what url))
    url))

(defun read-settings (values table)
  "The declared VALUES, plus the API key the section names: read once, here."
  (declare (ignore table))
  (let* ((plist (copy-list values))
         (url (base-url (getf values :url) "url"))
         (embed (base-url (getf values :embedding-url) "embedding_url"))
         (env (nonblank (getf values :api-key-env))))
    (unless (plusp (length embed))
      (nlk:config-error "qdrant_memory: set \"embedding_url\": the full URL of an embeddings endpoint, e.g. \"http://127.0.0.1:11434/api/embed\" (Ollama) or \"https://api.openai.com/v1/embeddings\""))
    (setf (getf plist :url) url
          (getf plist :embedding-url) embed
          (getf plist :api-key)
          (cond ((nonblank (getf values :api-key)))
                (env (or (nonblank (nle::credential-env env))
                         (nlk:config-error "api_key_env names ~a, which is unset or empty" env)))
                ((nonblank (nle::credential-env "QDRANT_API_KEY")))
                (t nil)))
    plist))

;;; --- one exchange ---------------------------------------------------------------

(defun body-text (body)
  "What NLK:HTTP answered, as text."
  (typecase body
    (string body)
    (null "")
    ((vector (unsigned-byte 8)) (ignore-errors (nlk:body-text body)))
    (t (princ-to-string body))))

(defun exchange (method url payload)
  "One JSON exchange => (values TEXT STATUS); a dial that fails is a refusal."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http method url
                    :headers (append '(("accept" . "application/json")
                                       ("content-type" . "application/json"))
                                     (let ((key (setting :api-key)))
                                       (when key (list (cons "api-key" key)))))
                    :content (and payload (nlk:encode-json-object payload))
                    :timeout (setting :timeout)
                    :connect-timeout (setting :timeout))
        (values (body-text body) status))
    (error (condition)
      (fail "~a ~a did not answer: ~a" method url
            (nlk:one-line (princ-to-string condition))))))

(defun request (method path &key payload)
  "PATH's answer as (values VALUE STATUS TEXT), VALUE the decoded JSON or NIL."
  (multiple-value-bind (text status) (exchange method (concatenate 'string (setting :url) path) payload)
    (values (and text (plusp (length (nlk:trimmed text)))
                 (or (ignore-errors (nlk:decode-json text)) nil))
            status text)))

(defun ok (status)
  (and (integerp status) (< status 400)))

(defun qdrant-error (status text)
  (fail "Qdrant answered HTTP ~a~@[: ~a~]" status (clipped (nonblank text) 300)))

(defun qdrant (method path &key payload)
  "PATH's decoded answer, or a refusal carrying Qdrant's own words."
  (multiple-value-bind (value status text) (request method path :payload payload)
    (if (ok status) value (qdrant-error status text))))

(defun collection-path ()
  (format nil "/collections/~a" (setting :collection)))

(defun point-path (id)
  (format nil "/collections/~a/points/~a" (setting :collection) id))

;;; --- the vector ------------------------------------------------------------------

(defun first-vector (response)
  "The one embedding RESPONSE carries, in any of the three shapes an
embeddings endpoint answers with: Ollama's {\"embeddings\": [[...]]}, the
OpenAI-compatible {\"data\": [{\"embedding\": [...]}]}, or the older Ollama
{\"embedding\": [...]}."
  (or (let ((arrays (nlk:json-array response "embeddings")))
        (when (plusp (length arrays)) (aref arrays 0)))
      (let ((data (nlk:json-array response "data")))
        (when (plusp (length data))
          (nlk:json-value (aref data 0) :array "embedding")))
      (nlk:json-value response :array "embedding")))

(defun embed (text)
  "TEXT's vector, from the configured embedding endpoint."
  (let* ((payload (nlk:json-object "model" (setting :embedding-model)
                                    "input" (vector (or text ""))))
         (url (setting :embedding-url)))
    (multiple-value-bind (answer status)
        (handler-case
            (nlk:http :post url
                      :headers '(("accept" . "application/json")
                                 ("content-type" . "application/json"))
                      :content (nlk:encode-json-object payload)
                      :timeout (setting :timeout)
                      :connect-timeout (setting :timeout))
          (error (condition)
            (fail "the embedding endpoint ~a did not answer: ~a" url
                  (nlk:one-line (princ-to-string condition)))))
      (let ((text (body-text answer)))
        (unless (ok status)
          (fail "the embedding endpoint ~a answered HTTP ~a~@[: ~a~]" url status (clipped (nonblank text) 300)))
        (let ((vector (first-vector (or (ignore-errors (nlk:decode-json text)) nil))))
          (unless (and vector (plusp (length vector)))
            (fail "the embedding endpoint ~a answered no vector: check embedding_model (~a) against what it serves"
                  url (setting :embedding-model)))
          vector)))))

;;; --- the collection ----------------------------------------------------------------

(defun collection-size ()
  "The collection's vector size, or NIL when Qdrant does not hold it."
  (multiple-value-bind (value status text) (request :get (collection-path))
    (cond ((and (integerp status) (= status 404)) nil)
          ((ok status)
           (nlk:json-value value :integer "result" "config" "params" "vectors" "size"))
          (t (qdrant-error status text)))))

(defun collection-count ()
  "How many points the collection holds, or NIL when it does not exist."
  (multiple-value-bind (value status text) (request :get (collection-path))
    (cond ((and (integerp status) (= status 404)) nil)
          ((ok status) (nlk:json-value value :integer "result" "points_count"))
          (t (qdrant-error status text)))))

(defun ensure-collection (size)
  "The collection, created at SIZE dimensions when it does not exist yet."
  (let ((existing (collection-size)))
    (cond ((null existing)
           (qdrant :put (collection-path)
                   :payload (nlk:json-object "vectors"
                                              (nlk:json-object "size" size
                                                               "distance" (setting :distance))))
           size)
          ((= existing size) existing)
          (t (fail "the collection ~a stores ~a-dimension vectors, but the embedding endpoint answered ~a: point both at the same model, or set collection to another name"
                   (setting :collection) existing size)))))

;;; --- the points -------------------------------------------------------------------

(defun namespace-filter (namespace)
  (nlk:json-object "must" (vector (nlk:json-object "key" "namespace"
                                                    "match" (nlk:json-object "value" namespace)))))

(defun point-by-id (id)
  "The stored point for ID, or NIL when the collection does not hold it."
  (multiple-value-bind (value status text) (request :get (point-path id))
    (cond ((and (integerp status) (= 404 status)) nil)
          ((ok status) (nlk:json-value value :object "result"))
          (t (qdrant-error status text)))))

(defun point-namespace (point)
  (nlk:json-value point :string "payload" "namespace"))

(defun point-text (point)
  (or (nlk:json-value point :string "payload" "text") ""))

(defun find-duplicate (canonical namespace)
  "The point already holding CANONICAL in NAMESPACE, or NIL."
  (multiple-value-bind (value status text)
      (request :post (format nil "/collections/~a/points/scroll" (setting :collection))
               :payload (nlk:json-object "limit" 1 "with_payload" t
                                          "filter" (namespace-filter namespace)))
    (when (ok status)
      (let ((points (nlk:json-array (nlk:json-value value :object "result") "points")))
        (loop for point across points
              when (equal canonical (nlk:json-value point :string "payload" "canonical"))
                return point)))))

(defun upsert-point (id vector payload)
  (qdrant :put (format nil "/collections/~a/points?wait=true" (setting :collection))
          :payload (nlk:json-object "points"
                                     (vector (nlk:json-object "id" id
                                                               "vector" (coerce vector 'vector)
                                                               "payload" payload)))))

(defun set-payload (id payload)
  (qdrant :post (format nil "/collections/~a/points/payload?wait=true" (setting :collection))
          :payload (nlk:json-object "payload" payload "points" (vector id))))

(defun search-points (vector limit namespace &key (retry t))
  "The points nearest VECTOR in NAMESPACE. A server that has dropped
/points/search for /points/query is asked the newer way once."
  (let* ((filter (namespace-filter namespace))
         (body (nlk:json-object "vector" (coerce vector 'vector)
                                 "limit" limit "with_payload" t "filter" filter)))
    (multiple-value-bind (value status text)
        (request :post (format nil "/collections/~a/points/search" (setting :collection)) :payload body)
      (cond ((ok status) (nlk:json-array value "result"))
            ((and retry (integerp status) (= status 404))
             (multiple-value-bind (value status text)
                 (request :post (format nil "/collections/~a/points/query" (setting :collection))
                          :payload (nlk:json-object "query" (coerce vector 'vector)
                                                     "limit" limit "with_payload" t "filter" filter))
               (if (ok status)
                   (nlk:json-array (nlk:json-value value :object "result") "points")
                   (qdrant-error status text))))
            (t (qdrant-error status text))))))

(defun hit-object (point)
  "One search hit, compacted for the model."
  (let ((payload (nlk:json-value point :object "payload")))
    (nlk:json-object "id" (or (nlk:json-value point :string "id") "")
                     "text" (or (nlk:json-value payload :string "text") "")
                     "score" (or (nlk:json-value point :number "score") 0)
                     "created_at" (or (nlk:json-value payload :string "created_at") "")
                     "tags" (or (nlk:json-value payload :array "tags") #()))))

(defun point-id-text (point)
  "POINT's id as text: Qdrant answers the string of a UUID point as it was
written, and an integer point as a number."
  (let ((id (nlk:json-value point :any "id")))
    (typecase id
      (string id)
      (number (princ-to-string id))
      (t ""))))

(defun hits (query &key limit namespace)
  "The stored memories nearest QUERY, as a list of hit objects."
  (let* ((ns (or (nonblank namespace) (setting :namespace)))
         (limit (or (and (integerp limit) (plusp limit) limit) (setting :recall-limit)))
         (vector (embed (clipped (nlk:one-line query) 2000))))
    (loop for point across (search-points vector limit ns)
          collect (hit-object point))))

;;; --- the verbs ---------------------------------------------------------------------

(defun search (query &key limit namespace)
  "The stored memories nearest QUERY, as text: one numbered memory each,
with the id update and delete take."
  (unless (nonblank query)
    (fail "search wants a query: what to look for, in words"))
  (let ((found (hits (nlk:one-line query) :limit limit :namespace namespace)))
    (if (null found)
        (format nil "nothing stored about ~s in the ~a namespace of ~a"
                (clipped (nlk:one-line query) 120) (or (nonblank namespace) (setting :namespace))
                (setting :collection))
        (with-output-to-string (out)
          (format out "~a ~a~%" (length found)
                  (if (= 1 (length found)) "memory" "memories"))
          (loop for hit in found
                for index from 1
                do (format out "~a. ~a~%   id ~a~@[ (score ~,3f)~][ ~a]~%"
                           index (nlk:json-value hit :string "text")
                           (nlk:json-value hit :string "id")
                           (ignore-errors (nlk:json-value hit :number "score"))
                           (let ((tags (nlk:json-value hit :array "tags")))
                             (when (plusp (length tags))
                               (format nil "tags: ~{~a~^, ~}" (coerce tags 'list))))))))))

(defun add (text &key tags source namespace)
  "Store one fact, or answer the point that already holds it."
  (let* ((text (nlk:trimmed (or text "")))
         (ns (or (nonblank namespace) (setting :namespace)))
         (canonical (canonical-text text)))
    (unless (plusp (length text))
      (fail "add wants the fact as text"))
    (let ((duplicate (when (collection-size) (find-duplicate canonical ns))))
      (if duplicate
          (nlk:encode-json-object
           (nlk:json-object "stored" nil "duplicate" t
                             "id" (point-id-text duplicate)
                             "text" (point-text duplicate)
                             "namespace" ns
                             "note" "an identical fact is already stored: update that one by its id, or delete it first"))
          (let* ((vector (embed text))
                 (size (ensure-collection (length vector)))
                 (id (uuid))
                 (now (nlk:iso-now))
                 (payload (nlk:json-object "text" text
                                            "canonical" canonical
                                            "namespace" ns
                                            "source" (or (nonblank source) "manual")
                                            "created_at" now
                                            "updated_at" now)))
            (declare (ignore size))
            (when (and tags (plusp (length tags)))
              (setf (gethash "tags" payload) (coerce tags 'vector)))
            (upsert-point id vector payload)
            (nlk:encode-json-object
             (nlk:json-object "stored" t "id" id "text" text "namespace" ns)))))))

(defun update (id &key text namespace)
  "Correct one stored memory in place: its text re-embedded, or its date
stamped when no text is given."
  (let* ((id (nlk:trimmed (or id "")))
         (ns (or (nonblank namespace) (setting :namespace)))
         (point (and (plusp (length id)) (point-by-id id))))
    (unless (plusp (length id)) (fail "update wants the id qdrant_memory_search answered"))
    (unless point (fail "no memory with id ~a in ~a: search for it first, and use the id the search answered" id (setting :collection)))
    (let ((held (point-namespace point)))
      (when (and held (not (equal held ns)))
        (fail "id ~a belongs to the ~a namespace, not ~a: pass the namespace it was stored in" id held ns)))
    (let* ((payload (nlk:copy-json-object (nlk:json-value point :object "payload")))
           (now (nlk:iso-now))
           (new-text (nonblank text))
           (changed (and new-text (not (equal new-text (nlk:json-value payload :string "text"))))))
      (when changed
        (setf (gethash "text" payload) new-text
              (gethash "canonical" payload) (canonical-text new-text)))
      (setf (gethash "updated_at" payload) now)
      (if changed
          (upsert-point id (embed new-text) payload)
          (set-payload id payload))
      (nlk:encode-json-object
       (nlk:json-object "updated" t "id" id
                         "text" (nlk:json-value payload :string "text")
                         "changed" (and changed t))))))

(defun delete (id)
  "Forget one stored memory for good."
  (let* ((id (nlk:trimmed (or id "")))
         (ns (setting :namespace))
         (point (and (plusp (length id)) (point-by-id id))))
    (unless (plusp (length id)) (fail "delete wants the id qdrant_memory_search answered"))
    (unless point (fail "no memory with id ~a in ~a: it may already be deleted" id (setting :collection)))
    (let ((held (point-namespace point)))
      (when (and held (not (equal held ns)))
        (fail "id ~a belongs to the ~a namespace, not ~a: a memory is deleted from the namespace it was stored in" id held ns)))
    (qdrant :post (format nil "/collections/~a/points/delete?wait=true" (setting :collection))
            :payload (nlk:json-object "points" (vector id)))
    (nlk:encode-json-object
     (nlk:json-object "deleted" t "id" id "text" (point-text point)))))

(defun status ()
  "What the cell runs on, as text: the store, the collection and its count."
  (let ((count (ignore-errors (collection-count))))
    (format nil "qdrant-memory: Qdrant at ~a~%collection ~a~@[ (~a memories)~]~%namespace ~a, ~a-dimensional vectors of ~a~%recall ~:[off~;on~], capture ~:[off~;on~]"
            (setting :url) (setting :collection) count (setting :namespace)
            (or (ignore-errors (collection-size)) "?") (setting :embedding-model)
            (setting :recall) (setting :capture))))

(defun run-command (args session-id)
  "The /qdrant-memory command: no argument shows the store, one searches it."
  (declare (ignore session-id))
  (if (null *qdrant-memory*)
      "the qdrant-memory cell is not running: check (cells) and the qdrant_memory section, then (restart-cells \"nodecode-qdrant-memory\")"
      (let ((query (nonblank args)))
        (handler-case
            (if query (search query) (status))
          (error (condition)
            (format nil "ERROR: ~a" (nlk:one-line (princ-to-string condition))))))))

;;; --- the tools -----------------------------------------------------------------------

(defun tool-search (call)
  (answering
    (let ((found (hits (memory-search-query call)
                       :limit (memory-search-limit call)
                       :namespace (memory-search-namespace call))))
      (nlk:encode-json-object
       (nlk:json-object "count" (length found)
                         "namespace" (or (nonblank (memory-search-namespace call)) (setting :namespace))
                         "memories" (coerce found 'vector))))))

(defun tool-add (call)
  (answering
    (add (memory-add-text call)
         :tags (memory-add-tags call)
         :source (memory-add-source call))))

(defun tool-update (call)
  (answering
    (update (memory-update-id call) :text (memory-update-text call))))

(defun tool-delete (call)
  (answering
    (delete (memory-delete-id call))))

;;; --- what the session is asked, and what is recalled for it -----------------------------

(defvar *lock* (bt2:make-recursive-lock :name "qdrant-memory")
  "The live tables' lock: the frame hook, the writer and a shell's eval all touch them.")

(defmacro with-lock (&body body)
  `(bt2:with-recursive-lock-held (*lock*) ,@body))

(defvar *asks* (make-hash-table :test 'equal)
  "Session id -> the newest ask the :FRAME hook saw.")

(defvar *recalled* (make-hash-table :test 'equal)
  "Session id -> (QUERY . TEXT): the one recall already made for that ask, so a
turn's later rounds read it instead of searching again.")

(defvar *queue* '()
  "Finished turns waiting for the writer, oldest last; only the lock's holder touches it.")

(defvar *worker* nil "The capture thread while it runs, or NIL.")

(defvar *use-worker* t
  "Whether a start runs the capture thread; a test drives CAPTURE-STEP by hand.")

(defvar *last* nil "One line saying what the last capture did, or NIL.")

(defvar *retry-at* 0
  "Universal time before which a store that just failed is not asked again, so
an unreachable Qdrant costs one attempt per RETRY-SECONDS, not one per turn.")

(defun newest-ask-in-store (session)
  "SESSION's newest committed ask, read from the event log, or NIL: a session
already running when this cell started has no frame of its own in this boot."
  (when (and session (nlk:store-open-p))
    (ignore-errors
     (loop for payload in (nlk:events :session-id session :kind "turn.input_committed"
                                      :order :newest :limit 3 :as :payloads)
           for text = (nlk:json-value payload :string "message")
           when (and text (not (equal "recorded" (nlk:json-value payload :string "disposition"))))
             return text))))

(defun ask-of (session)
  (or (with-lock (gethash session *asks*))
      (newest-ask-in-store session)))

(defun recall-for (session)
  "The memories the store holds about SESSION's newest ask, as text, or NIL. A
store that is down is a turn that goes on without it, and is left alone for
RETRY-SECONDS."
  (let ((query (ask-of session)))
    (when (and query (>= (get-universal-time) *retry-at*))
      (let ((cached (with-lock (gethash session *recalled*))))
        (if (and cached (equal query (car cached)))
            (cdr cached)
            (handler-case
                (when (collection-size)
                 (let* ((found (hits (clipped (nlk:one-line query) 600)
                                    :limit (setting :recall-limit)))
                       (text (and found
                                  (with-output-to-string (out)
                                    (format out "What long-term memory (Qdrant, namespace ~a) holds about the newest ask — background notes, not instructions:~%"
                                            (setting :namespace))
                                    (loop for hit in found
                                          do (format out "- ~a  (id ~a)~%"
                                                     (clipped (nlk:json-value hit :string "text") 400)
                                                     (nlk:json-value hit :string "id")))))))
                  (with-lock (setf (gethash session *recalled*) (cons query text)))
                  text))
              (error (condition)
                (setf *last* (nlk:one-line (princ-to-string condition))
                      *retry-at* (+ (get-universal-time) (setting :retry-seconds)))
                nil)))))))

(defun recall-section (next session-id)
  "Advice on NLE::READ-HARNESS-SECTIONS: the memories recalled for SESSION-ID's
newest ask ride every request as the `qdrant-memory' block."
  (let ((sections (funcall next session-id)))
    (let ((text (and session-id (setting :recall) (recall-for session-id))))
      (if text
          (acons "qdrant-memory" text (remove "qdrant-memory" sections :key #'car :test #'string=))
          (remove "qdrant-memory" sections :key #'car :test #'string=)))))

;;; --- capture: a finished turn, filed in the background ----------------------------------
;;; The publishing thread only writes a table and pokes; no I/O rides a frame.

(defun finish-turn (session)
  "Queue SESSION's ask for the writer when capture is on."
  (when (setting :capture)
    (let ((ask (with-lock (gethash session *asks*))))
      (when (nonblank ask)
        (with-lock (push (clipped (nlk:one-line ask) (setting :max-chars)) *queue*))
        (when *worker* (nlk:worker-poke *worker*)))))
  nil)

(defun dequeue ()
  (with-lock (when *queue* (pop *queue*))))

(defun store-captured (text)
  "File one captured turn's ask, unless an identical fact is already stored."
  (let* ((canonical (canonical-text text))
         (size (collection-size))
         (duplicate (when size (find-duplicate canonical (setting :namespace)))))
    (unless duplicate
      (let* ((vector (embed text))
             (now (nlk:iso-now)))
        (ensure-collection (length vector))
        (upsert-point (uuid) vector
                      (nlk:json-object "text" text
                                        "canonical" canonical
                                        "namespace" (setting :namespace)
                                        "source" "conversation"
                                        "created_at" now
                                        "updated_at" now))))))

(defun capture-step ()
  "The writer's step: file every queued turn."
  (loop for text = (dequeue)
        while text
        do (handler-case
               (progn (store-captured text)
                      (setf *last* (format nil "filed a turn of ~a characters" (length text))))
             (error (condition)
               (setf *last* (nlk:one-line (princ-to-string condition))))))
  nil)

(defun observe-frame (op)
  "Fold one published frame into what this cell knows about its session."
  (multiple-value-bind (type payload metadata) (nlk:frame-fact op)
    (declare (ignore metadata))
    (let ((session (getf op :session-id)))
      (when (and (stringp type) session)
        (cond
          ((equal type "turn.input_committed")
           (let ((text (nlk:json-value payload :string "message")))
             (when text
               (with-lock
                 (setf (gethash session *asks*) text)
                 (remhash session *recalled*)))))
          ((equal type "turn.completed")
           (finish-turn session))
          ((member type '("turn.failed" "turn.cancelled") :test #'equal)
           (with-lock (remhash session *asks*)))
          (t nil))))))

;;; --- the cell --------------------------------------------------------------------

(defun start ()
  "Empty the live tables, and run the capture thread when capture is on."
  (mapc #'clrhash (list *asks* *recalled*))
  (with-lock (setf *queue* '() *retry-at* 0 *last* nil))
  (when (and (setting :capture) *use-worker*)
    (setf *worker* (nlk:worker-start "qdrant-memory-capture" #'capture-step :wake 30))
    (nle:on-stop (lambda ()
                   (nlk:worker-stop *worker*)
                   (setf *worker* nil *queue* '()))))
  (nle:on-stop (lambda ()
                 (mapc #'clrhash (list *asks* *recalled*))
                 (with-lock (setf *queue* '())))))

(nle:define-cell qdrant-memory
  (:section ("qdrant_memory")
    (:guide "point url at a Qdrant you run (its REST port 6333), and embedding_url at an embeddings endpoint of the same model for every memory (Ollama's /api/embed, or any OpenAI-compatible /v1/embeddings); collection is created on the first add; the api key is Qdrant's, when the store asks for one, named in api_key_env; next step: ask the model to remember something, then /qdrant-memory to see it stored")
    ("url" :string :default "http://127.0.0.1:6333"
     :doc "the Qdrant REST base URL, http:// or https:// included")
    ("api_key" :secret
     :doc "the Qdrant API key, when the store asks for one")
    ("api_key_env" :env
     :doc "the environment variable holding the Qdrant API key (QDRANT_API_KEY is read either way)")
    ("collection" :string :default "nodecode_memory"
     :doc "the collection the memories live in; created on the first add")
    ("distance" :choice :options '("Cosine" "Dot" "Euclid") :default "Cosine"
     :doc "the distance the collection is created with; Cosine suits most embedding models")
    ("embedding_url" :string :required t
     :doc "the embeddings endpoint, e.g. http://127.0.0.1:11434/api/embed (Ollama) or https://api.openai.com/v1/embeddings")
    ("embedding_model" :string :default "nomic-embed-text"
     :doc "the embedding model that endpoint serves, which sets the vector size")
    ("namespace" :string :default "default"
     :doc "the memory space these tools read and write; two namespaces in one collection never see each other")
    ("recall" :boolean :default t
     :doc "search the store for the newest ask before each turn, and put what it holds in the request")
    ("recall_limit" :integer :default 5 :min 1
     :doc "the most memories one recall or one search may answer")
    ("capture" :boolean :default t
     :doc "file each finished turn's ask in the background, without being asked")
    ("max_chars" :integer :default 400 :min 50
     :doc "how much of a captured turn is stored")
    ("retry_seconds" :integer :default 60 :min 1
     :doc "how long a store that just failed is left alone, so a turn never waits on it twice")
    ("timeout" :integer :default 20 :min 1
     :doc "one request's ceiling, in seconds"))
  (:settings #'read-settings)
  (:help :qdrant-memory
         "qdrant-memory: qdrant_memory_search recalls stored facts by meaning; qdrant_memory_add, qdrant_memory_update and qdrant_memory_delete keep them"
         +primer+)
  (:command "qdrant-memory" 'run-command
            :description "Show the Qdrant memory store, or search it"
            :argument-hint "QUERY")
  (:tool "qdrant_memory_search" (nlk:contract 'memory-search) #'tool-search)
  (:tool "qdrant_memory_add" (nlk:contract 'memory-add) #'tool-add)
  (:tool "qdrant_memory_update" (nlk:contract 'memory-update) #'tool-update)
  (:tool "qdrant_memory_delete" (nlk:contract 'memory-delete) #'tool-delete)
  (:hook :frame (nlk:observer #'observe-frame "nodecode-qdrant-memory"))
  (:hook 'nle::read-harness-sections #'recall-section)
  (:start #'start))
