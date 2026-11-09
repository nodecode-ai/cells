;;;; api.lisp --- the origin, the two requests, and the page they answer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Meilisearch's HTTP surface, as the original plugin used it: GET /health
;;;; with no key, and POST /indexes/<uid>/search with a bearer token and a
;;;; body of {"q": ..., "limit": 20}. Every status is a value, and a page
;;;; that is not exactly what the search call promises is a sentence the
;;;; model can act on, never a condition thrown at the turn.

(in-package #:nodecode-index-search)

(defparameter +hit-limit+ 20
  "The one page size: the original reports at most this many hits and says so.")

(defparameter +title-cap+ 200
  "How much of a hit's title field is copied into an answer.")

(defparameter +max-body+ 2000000
  "The longest answer body read; past it the call is incomplete, not half-parsed.")

(defparameter +redirect-codes+ '(301 302 303 307 308)
  "The statuses that mean the server moved the request; the answer says so.")

;;; --- what the model is told when a call cannot run -----------------------------

(defparameter +msg-host-required+
  "The host is not set: put the origin of the search server you run in host, the index-search section's member, such as http://127.0.0.1:7700.")

(defparameter +msg-host-bad+
  "The host is not a usable origin: write the scheme, the host and an optional port, and no path, query, fragment or credentials.")

(defparameter +msg-key-required+
  "The API key is not set: paste a search-only key into api_key, or name the variable holding it in api_key_env.")

(defparameter +msg-key-in-url+ "The API key is part of the request URL, so nothing was sent.")

(defparameter +msg-redirect+ "The server redirected the request, and it was not followed.")

(defparameter +msg-incomplete+ "The search did not complete, and seen ids were not advanced.")

(defparameter +msg-bad-page+ "The search returned an unusable page, and seen ids were not advanced.")

(defparameter +msg-no-id+ "A hit has no usable id field, so seen ids were not advanced.")

(defparameter +msg-dup+ "This page has duplicate ids, so seen ids were not advanced.")

(defparameter +msg-too-many+ "The page has more than 20 hits, so seen ids were not advanced.")

(defparameter +msg-zero+ "This page has 0 hits. Seen ids were kept.")

(defparameter +msg-gap+
  " This page shares no saved id, so matches between the saved ids and this page may have been missed.")

(defparameter +msg-query+ "The query must be a non-empty string of 400 characters or fewer.")

(defparameter +msg-uid+ "The index uid must be 1 to 64 letters, digits, hyphens, or underscores.")

(defparameter +msg-id+
  "The id field must be a non-empty string of 64 characters or fewer, without a dot, a slash, or a control character.")

(defparameter +msg-title+
  "The title field must be a non-empty string of 64 characters or fewer, without a dot, a slash, or a control character.")

;;; --- what a call may carry ----------------------------------------------------

(defun nonblank (value)
  "VALUE as a string holding anything but whitespace, trimmed; else NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun fold-digits (value)
  "VALUE trimmed, with full-width digits folded to the ASCII ones it means."
  (when (stringp value)
    (map 'string (lambda (character)
                   (let ((code (char-code character)))
                     (if (<= #xFF10 code #xFF19)
                         (digit-char (- code #xFF10))
                         character)))
         (nlk:trimmed value))))

(defun uid-char-p (character)
  (or (char<= #\a character #\z)
      (char<= #\A character #\Z)
      (char<= #\0 character #\9)
      (member character '(#\- #\_))))

(defun uid-ok-p (value)
  "VALUE as an index uid: 1 to 64 letters, digits, hyphens or underscores."
  (and (stringp value) (<= 1 (length value) 64) (every #'uid-char-p value)))

(defun field-ok-p (value)
  "VALUE as a hit field name: 1 to 64 characters, no dot, slash or control."
  (and (stringp value)
       (<= 1 (length value) 64)
       (not (find #\. value))
       (not (find #\/ value))
       (not (find-if (lambda (character) (or (< (char-code character) 32)
                                             (= (char-code character) 127)))
                      value))))

(defun query-ok-p (value)
  "VALUE as a query: non-empty once trimmed, 400 characters or fewer."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (<= (length value) 400)))

(defun query-problem (index query id-field title-field)
  "The first thing wrong with a query about to be sent, or NIL."
  (cond
    ((not (uid-ok-p index)) +msg-uid+)
    ((not (query-ok-p query)) +msg-query+)
    ((not (field-ok-p id-field)) +msg-id+)
    ((and (plusp (length title-field)) (not (field-ok-p title-field))) +msg-title+)
    (t nil)))

;;; --- the origin ----------------------------------------------------------------

(defun origin-character-p (character)
  "The characters a scheme, a host, a port and an IPv6 literal are written with."
  (or (alphanumericp character)
      (member character '(#\. #\- #\_ #\[ #\] #\:))))

(defun origin-of (host)
  "HOST as the origin requests are built from.
=> (values ORIGIN NIL), or (values NIL MESSAGE) when it is not one."
  (let ((text (nonblank host)))
    (cond
      ((null text) (values nil +msg-host-required+))
      ((find-if (lambda (character) (or (< (char-code character) 32)
                                        (= (char-code character) 127)
                                        (member character '(#\Space #\\))))
                text)
       (values nil +msg-host-bad+))
      (t
       (let* ((lowered (string-downcase text))
              (scheme (cond ((uiop:string-prefix-p "https://" lowered) "https")
                            ((uiop:string-prefix-p "http://" lowered) "http")
                            (t nil))))
         (if (null scheme)
             (values nil +msg-host-bad+)
             (let ((rest (subseq text (+ 3 (length scheme)))))
               (when (and (plusp (length rest)) (char= #\/ (char rest (1- (length rest)))))
                 (setf rest (subseq rest 0 (1- (length rest)))))
               (host-tail (format nil "~a://" scheme) rest))))))))

(defun host-tail (prefix rest)
  "PREFIX and REST, the host and port it carries, as the origin requests are built from."
  (cond
    ((zerop (length rest)) (values nil +msg-host-bad+))
    ((not (every #'origin-character-p rest)) (values nil +msg-host-bad+))
    ((char= #\[ (char rest 0))
     (let ((close (position #\] rest)))
       (cond
         ((null close) (values nil +msg-host-bad+))
         ((= (1+ close) (length rest))
          (values (concatenate 'string prefix rest) nil))
         ((and (char= #\: (char rest (1+ close)))
               (plusp (length (subseq rest (+ 2 close))))
               (every #'digit-char-p (subseq rest (+ 2 close)))
               (<= 1 (parse-integer (subseq rest (+ 2 close))) 65535))
          (values (format nil "~a~a:~d" prefix (subseq rest 0 (1+ close))
                          (parse-integer (subseq rest (+ 2 close))))
                  nil))
         (t (values nil +msg-host-bad+)))))
    (t
     (let* ((colon (position #\: rest :from-end t))
            (name (if colon (subseq rest 0 colon) rest))
            (port (and colon (subseq rest (1+ colon)))))
       (cond
         ((zerop (length name)) (values nil +msg-host-bad+))
         ((and port (or (zerop (length port)) (not (every #'digit-char-p port))))
          (values nil +msg-host-bad+))
         ((and port (not (<= 1 (parse-integer port) 65535))) (values nil +msg-host-bad+))
         (port (values (format nil "~a~a:~d" prefix name (parse-integer port)) nil))
         (t (values (concatenate 'string prefix name) nil)))))))

;;; --- the two URLs ---------------------------------------------------------------

(defun search-url (origin uid)
  "The search endpoint of index UID on ORIGIN; UID is already checked."
  (format nil "~a/indexes/~a/search" origin uid))

(defun health-url (origin)
  "The health endpoint of the server at ORIGIN."
  (format nil "~a/health" origin))

;;; --- one exchange -----------------------------------------------------------------

(defun slurp (stream)
  "STREAM read to its end as one string."
  (with-output-to-string (out)
    (loop for line = (read-line stream nil nil)
          while line
          do (write-string line out)
             (write-char #\Newline out))))

(defun exchange (method url &key headers content timeout)
  "The cell's one network call; a test stubs this name. => (values BODY STATUS),
a body that is a string and a status that is 0 when nothing came back."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http method url :headers headers :content content
                          :timeout (or timeout 10))
        (let ((text (cond ((stringp body) body)
                          ((streamp body) (ignore-errors (slurp body)))
                          (t (ignore-errors (sb-ext:octets-to-string body :external-format :utf-8))))))
          (values (or text "") (or status 0))))
    (error () (values "" 0))))

(defun http-timeout-seconds ()
  "The section's ceiling on one request."
  (setting :timeout-seconds))

;;; --- the page ---------------------------------------------------------------------

(defun page-json (body)
  "BODY decoded as one JSON value, or NIL when it is not JSON."
  (let ((value (and (stringp body) (ignore-errors (nlk:decode-json body :whole t)))))
    (if (eq value :null) nil value)))

(defun safe-code (body)
  "Meilisearch's own error code in BODY, when it is a short identifier."
  (let* ((object (page-json body))
         (code (and (hash-table-p object) (nlk:json-value object :string "code"))))
    (and code (<= 1 (length code) 64)
         (every (lambda (character) (or (alphanumericp character) (char= character #\_))) code)
         code)))

(defun failure-line (status code)
  "The sentence a refused search gets, naming what the server said."
  (if code
      (format nil "The search failed (HTTP ~d, ~a). Seen ids were not advanced." status code)
      (format nil "The search failed (HTTP ~d). Seen ids were not advanced." status)))

(defun total-suffix (total)
  "How many matched, and the one thing that follows from the page size."
  (if (> total +hit-limit+)
      (format nil " estimatedTotalHits is ~d. Matches past the first 20 are never reported." total)
      (format nil " estimatedTotalHits is ~d." total)))

(defun parse-page (status body)
  "STATUS and BODY as a page: (:HITS HITS TOTAL), (:ZERO), or (:BAD MESSAGE CAUSE)."
  (cond
    ((member status +redirect-codes+) (list :bad +msg-redirect+ "redirect"))
    ((or (>= status 500) (< status 200))
     (list :bad +msg-incomplete+ (format nil "http_~d" status)))
    ((/= status 200) (list :bad (failure-line status (safe-code body))
                           (format nil "http_~d" status)))
    ((zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) body)))
     (list :bad +msg-incomplete+ "incomplete"))
    (t
     (let ((object (page-json body)))
       (if (null object)
           (list :bad +msg-bad-page+ "bad_page")
           (page-object object))))))

(defun page-object (object)
  "OBJECT, a decoded page, checked for the shape a search promises."
  (let ((hits (nlk:json-value object :array "hits"))
        (total (nlk:json-value object :integer "estimatedTotalHits")))
    (cond
      ((null hits) (list :bad +msg-bad-page+ "bad_page"))
      ((null total) (list :bad +msg-bad-page+ "bad_page"))
      ((> (length hits) +hit-limit+) (list :bad +msg-too-many+ "too_many"))
      ((and (zerop total) (plusp (length hits))) (list :bad +msg-bad-page+ "bad_page"))
      ((< total (length hits)) (list :bad +msg-bad-page+ "bad_page"))
      ((and (zerop (length hits)) (plusp total)) (list :bad +msg-bad-page+ "bad_page"))
      ((and (zerop total) (zerop (length hits))) (list :zero))
      (t (check-hits hits total)))))

(defun check-hits (hits total)
  "HITS as a (:HITS ...) page, when every one of them is an object."
  (if (every #'hash-table-p hits)
      (list :hits (coerce hits 'list) total)
      (list :bad +msg-no-id+ "no_id")))

;;; --- the ids and titles a page carries --------------------------------------------

(defun usable-id (value)
  "VALUE as the seen list keeps an id: (\"i\" . 7), (\"s\" . \"abc\"), or NIL."
  (cond
    ((eq value t) nil)
    ((eq value :null) nil)
    ((null value) nil)
    ((integerp value) (cons "i" value))
    ((and (stringp value) (plusp (length value))) (cons "s" value))
    (t nil)))

(defun usable-title (value)
  "VALUE as a title of at most +TITLE-CAP+ characters, or NIL."
  (let ((text (cond
                ((stringp value) value)
                ((and (realp value) value) (princ-to-string value))
                (t nil))))
    (if (and text (plusp (length text)))
        (subseq text 0 (min +title-cap+ (length text)))
        nil)))

(defun ids-of (hits id-field title-field)
  "The page's HITS as (ITEM . PLIST) pairs, each plist holding :ID and :TITLE.
=> (values LIST \"\" \"\"), or (values NIL MESSAGE CAUSE) when the page cannot
be trusted to say what is new."
  (let ((found '()))
    (dolist (hit hits (values (nreverse found) "" ""))
      (let ((item (usable-id (nlk:json-value hit :any id-field))))
        (when (null item) (return (values nil +msg-no-id+ "no_id")))
        (when (assoc item found :test #'equal) (return (values nil +msg-dup+ "duplicate")))
        (let ((title (and (plusp (length title-field))
                           (usable-title (nlk:json-value hit :any title-field)))))
          (push (cons item (list :id (cdr item) :title title)) found))))))

(defun fmt-id (item)
  "ITEM as the answer prints it: a string id quoted, a number bare."
  (if (equal "s" (car item))
      (nlk:encode-json-object (cdr item))
      (princ-to-string (cdr item))))

(defun public-text (found)
  "FOUND as one comma-separated line of ids, each with its title when it has one."
  (format nil "~{~a~^, ~}"
          (loop for (item . public) in found
                collect (let ((title (getf public :title)))
                          (if title
                              (format nil "~a (~a)" (fmt-id item) title)
                              (fmt-id item))))))

;;; --- the calls ---------------------------------------------------------------------

(defun call-search (origin uid query key)
  "The page POST /indexes/UID/search answers on ORIGIN. => PARSE-PAGE's value."
  (let ((url (search-url origin uid)))
    (if (and (plusp (length key)) (search key url))
        (list :bad +msg-key-in-url+ "key_in_url")
        (multiple-value-bind (body status)
            (exchange :post url
                      :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                                     (cons "content-type" "application/json")
                                     (cons "accept" "application/json"))
                      :content (nlk:encode-json-object
                                (nlk:json-object "q" query "limit" +hit-limit+))
                      :timeout (http-timeout-seconds))
          (parse-page status body)))))

(defun ask-health (origin)
  "The server's own health. => (values HEALTHY MESSAGE)."
  (multiple-value-bind (body status)
      (exchange :get (health-url origin)
                :headers (list (cons "accept" "application/json"))
                :timeout (http-timeout-seconds))
    (let ((object (and (= status 200) (plusp (length body)) (page-json body))))
      (cond
        ((member status +redirect-codes+) (values nil +msg-redirect+))
        ((zerop status) (values nil "The health check did not complete."))
        ((and (hash-table-p object)
              (equal "available" (nlk:json-value object :string "status")))
         (values t "The server status is available."))
        (t (values nil "The server is not healthy."))))))

;;; --- keeping the key out of what the model reads -------------------------------------

(defun scrub (text key)
  "TEXT with every occurrence of KEY, the bearer token, replaced by [redacted]."
  (if (or (null (nonblank text)) (null (nonblank key)) (null (search key text)))
      text
      (with-output-to-string (out)
        (loop with start = 0
              for at = (search key text :start2 start)
              while at
              do (write-string text out :start start :end at)
                 (write-string "[redacted]" out)
                 (setf start (+ at (length key)))
              finally (write-string text out :start start)))))
