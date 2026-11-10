;;;; news.lisp --- an interest phrase into a query feed, the feed into items.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An interest is a phrase, not a subscription: it is expanded into the
;;;; provider's query address, fetched once, and what comes back becomes that
;;;; interest's source of the edition. The default provider is a public,
;;;; keyless news search feed; the section may name another with the same
;;;; {query} placeholder.
;;;;
;;;; Every byte this cell fetches moves through HTTP-GET, so a test stubs that
;;;; one name (through *HTTP*) and never touches the network.

(in-package #:nodecode-daily-edition)

(defparameter +provider+
  "https://news.google.com/rss/search?q={query}&hl=en-US&gl=US&ceid=US:en"
  "The query feed an interest is expanded into; the section may name another.")

(defparameter *http* nil
  "When bound, the seam a test stubs: (funcall *HTTP* URL) => (values BODY STATUS).")

;;; --- the query address --------------------------------------------------------------

(defun url-encode (text)
  "TEXT percent-encoded for a query, a space as +."
  (with-output-to-string (out)
    (loop for byte across (sb-ext:string-to-octets (or text "") :external-format :utf-8)
          do (let ((char (code-char byte)))
               (cond ((or (and (<= (char-code char) 127) (alphanumericp char))
                          (find char "-_.~"))
                      (write-char char out))
                     ((char= char #\Space) (write-char #\+ out))
                     (t (format out "%~2,'0X" byte)))))))

(defun topic-url (phrase &optional (provider +provider+))
  "PHRASE expanded into the provider's query address."
  (let ((phrase (nlk:trimmed (or phrase ""))))
    (when (zerop (length phrase))
      (fail "an interest needs some words"))
    (when (> (length phrase) 120)
      (fail "interest ~s is too long: at most 120 characters" phrase))
    (let ((address (or provider +provider+)))
      (unless (search "{query}" address)
        (fail "the provider address carries no {query} placeholder: ~a" (nlk:clip address 80)))
      (with-output-to-string (out)
        (let ((start 0))
          (loop
            (let ((at (search "{query}" address :start2 start)))
              (unless at (write-string (subseq address start) out) (return))
              (write-string (subseq address start at) out)
              (write-string (url-encode phrase) out)
              (setf start (+ at (length "{query}"))))))))))

;;; --- the wire ----------------------------------------------------------------------

(defun http-get (url)
  "The cell's one network call; a test binds *HTTP* to stub it.
=> (values BODY STATUS)."
  (if *http*
      (funcall *http* url)
      (nlk:http :get url
                :headers (list (cons "user-agent" "nodecode-daily-edition/0.1 (+https://nodecode.ai)")
                               (cons "accept" "application/rss+xml, application/xml;q=0.9, */*;q=0.8"))
                :timeout (setting :timeout-seconds)
                :max-bytes (* 2 1024 1024))))

(defun body-text (body)
  "BODY as a string: what the wire sent, decoded as UTF-8 when it is octets."
  (cond ((stringp body) body)
        ((typep body '(vector (unsigned-byte 8)))
         (handler-case (sb-ext:octets-to-string body :external-format :utf-8)
           (error () "")))
        (t (format nil "~a" body))))

;;; --- the feed -----------------------------------------------------------------------

(defparameter *entities*
  '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"") ("&apos;" . "'")
    ("&nbsp;" . " ") ("&#39;" . "'") ("&#8217;" . "'") ("&#8216;" . "'")
    ("&#8220;" . "\"") ("&#8221;" . "\"") ("&hellip;" . "...") ("&mdash;" . "-")
    ("&ndash;" . "-"))
  "The references a feed's text carries, decoded in one pass; anything else
is left as it was written.")

(defun decode-entities (text)
  (let ((out (make-string-output-stream)))
    (loop with start = 0
          for amp = (position #\& text :start start)
          do (cond ((null amp)
                    (write-string (subseq text start) out)
                    (return (get-output-stream-string out)))
                   (t
                    (write-string (subseq text start amp) out)
                    (let ((matched (find-if (lambda (pair) (nodecode-daily-edition::prefix-p (car pair) (subseq text amp)))
                                            *entities*)))
                      (if matched
                          (progn (write-string (cdr matched) out)
                                 (setf start (+ amp (length (car matched)))))
                          (progn (write-char #\& out)
                                 (setf start (1+ amp))))))))))

(defun strip-markup (text)
  "TEXT with every <tag ...> taken out and its references spelled."
  (with-output-to-string (out)
    (let ((start 0))
      (loop
        (let ((lt (position #\< text :start start)))
          (when (null lt)
            (write-string (subseq text start) out)
            (return))
          (write-string (subseq text start lt) out)
          (let ((gt (position #\> text :start lt)))
            (cond (gt (write-char #\Space out)
                      (setf start (1+ gt)))
                  (t (write-string (subseq text lt) out)
                     (return)))))))))

(defun unwrap-cdata (text)
  "TEXT with every <![CDATA[ ... ]]> replaced by what it wraps."
  (let ((open "<![CDATA[") (close "]]>") (start 0) (out (make-string-output-stream)))
    (loop
      (let ((a (search open text :start2 start)))
        (when (null a)
          (write-string (subseq text start) out)
          (return (get-output-stream-string out)))
        (write-string (subseq text start a) out)
        (let ((b (search close text :start2 (+ a (length open)))))
          (if b
              (progn (write-string (subseq text (+ a (length open)) b) out)
                     (setf start (+ b (length close))))
              (progn (write-string (subseq text a) out)
                     (setf start (length text))
                     (return (get-output-stream-string out)))))))))

(defun clean (text &key (limit 400))
  "TEXT as one line of prose, cut to LIMIT."
  (let* ((text (decode-entities (unwrap-cdata (or text ""))))
         (text (strip-markup text)))
    (nlk:clip (nlk:trimmed (format nil "~{~a~^ ~}"
                                   (remove "" (uiop:split-string text
                                                                 :separator '(#\Space #\Tab #\Newline #\Return))
                                           :test #'equal)))
              limit)))

(defun text-between (text open close &key (start 0))
  "What TEXT carries between OPEN and CLOSE at or after START, or NIL."
  (let* ((a (search open text :start2 start :test #'char-equal)))
    (when a
      (let* ((from (+ a (length open)))
             (b (search close text :start2 from :test #'char-equal)))
        (when b (subseq text from b))))))

(defun blocks (text name)
  "Every <NAME ...> ... </NAME> block of TEXT, tags left in."
  (let ((open (format nil "<~a" name)) (close (format nil "</~a>" name))
        (start 0) (out '()))
    (loop
      (let ((a (search open text :start2 start :test #'char-equal)))
        (unless a (return (nreverse out)))
        (let ((b (search close text :start2 a :test #'char-equal)))
          (unless b (return (nreverse out)))
          (let ((end (+ b (length close))))
            (push (subseq text a end) out)
            (setf start end)))))))

(defun tag-text (block name &key (start 0))
  "The text of BLOCK's first <NAME> element, tags and all."
  (let ((a (search (format nil "<~a" name) block :start2 start :test #'char-equal)))
    (when a
      (let ((open-end (position #\> block :start a)))
        (when open-end
          (let* ((from (1+ open-end))
                 (b (search (format nil "</~a>" name) block :start2 from :test #'char-equal)))
            (subseq block from (or b (length block)))))))))

;;; --- the date -------------------------------------------------------------------------

(defun month-number (name)
  (1+ (or (position (string-downcase (subseq name 0 (min 3 (length name)))) +months+
                    :test (lambda (a b) (nodecode-daily-edition::prefix-p a b)))
          0)))

(defun parse-zone (zone)
  "ZONE (\"GMT\", \"+0200\", \"-05:00\", nil) in hours east of UTC."
  (let ((zone (nlk:trimmed (or zone ""))))
    (cond ((or (zerop (length zone)) (member (string-upcase zone) '("GMT" "UTC" "Z") :test #'equal)) 0)
          ((find (char zone 0) "+-")
           (let* ((sign (if (char= (char zone 0) #\-) -1 1))
                  (digits (remove-if-not #'digit-char-p (subseq zone 1)))
                  (hours (or (parse-integer digits :end (min 2 (length digits)) :junk-allowed t) 0))
                  (minutes (or (and (> (length digits) 2)
                                    (parse-integer digits :start 2 :junk-allowed t))
                               0)))
             (* sign (+ hours (/ minutes 60)))))
          (t 0))))

(defun parse-date (text)
  "TEXT, an RFC 822 or an ISO date, as universal time; NIL when it is
neither."
  (let ((text (nlk:trimmed (or text ""))))
    (when (plusp (length text))
      (handler-case
          (let* ((comma (position #\, text))
                 (rest (nlk:trimmed (if comma (subseq text (1+ comma)) text)))
                 (words (remove "" (uiop:split-string rest :separator '(#\Space #\Tab))
                                :test #'equal)))
            (cond
              ((>= (length words) 4)
               (destructuring-bind (day month year clock &optional zone) words
                 (let ((day (or (parse-integer day :junk-allowed t) 0))
                       (month (month-number month))
                       (year (or (parse-integer year :junk-allowed t) 1970))
                       (hour 0) (minute 0) (second 0))
                   (when clock
                     (let ((parts (uiop:split-string clock :separator '(#\:))))
                       (setf hour (or (parse-integer (or (first parts) "") :junk-allowed t) 0)
                             minute (or (parse-integer (or (second parts) "") :junk-allowed t) 0)
                             second (or (parse-integer (or (third parts) "") :junk-allowed t) 0))))
                   (when (and (plusp day) (<= 1 day 31) (plusp year))
                     (encode-universal-time second minute hour day month year (parse-zone zone))))))
              ((search "-" text)
               ;; 2026-11-03T09:00:00Z
               (let* ((date (subseq text 0 (min 10 (length text))))
                      (parts (uiop:split-string date :separator '(#\-))))
                 (when (= (length parts) 3)
                   (encode-universal-time 0 0 0
                                          (parse-integer (third parts))
                                          (parse-integer (second parts))
                                          (parse-integer (first parts))
                                          0))))))
        (error () nil)))))

;;; --- one interest's feed -------------------------------------------------------------

(defun split-publisher (title)
  "TITLE and the masthead a query feed appended to it, as two values."
  (let ((at (search " - " title :from-end t)))
    (cond
      ((null at) (values title ""))
      (t (let ((head (nlk:trimmed (subseq title 0 at)))
               (tail (nlk:trimmed (subseq title (+ at 3)))))
           (if (and (>= (length head) 12) (<= 2 (length tail) 60) (not (search " - " tail)))
               (values head tail)
               (values title "")))))))

(defun parse-topic-feed (text)
  "TEXT, a query feed's XML, as (:source NAME :items ITEMS)."
  (when (or (search "<!DOCTYPE" (string-upcase text))
            (search "<!ENTITY" (string-upcase text)))
    (fail "unsupported XML declaration"))
  (let ((channel (text-between text "<channel" "</channel>")))
    (unless channel
      (fail "not an RSS feed"))
    (let ((name (clean (or (tag-text channel "title") "") :limit 120))
          (items '())
          (seen (make-hash-table :test #'equal)))
      (let ((blocks (blocks channel "item")))
        (dolist (block (subseq blocks 0 (min 100 (length blocks))))
          (let ((link (nlk:trimmed (or (tag-text block "link") ""))))
            (when (and (plusp (length link))
                       (or (nodecode-daily-edition::prefix-p "http://" (string-downcase link))
                           (nodecode-daily-edition::prefix-p "https://" (string-downcase link)))
                       (not (gethash link seen)))
              (setf (gethash link seen) t)
              (let ((raw (clean (or (tag-text block "title") "") :limit 300)))
                (unless (zerop (length raw))
                  (let ((source (clean (or (tag-text block "source") "") :limit 80)))
                    (multiple-value-bind (title suffix) (split-publisher raw)
                      (push (list :title title :url link
                                  :snippet (clean (or (tag-text block "description") "") :limit 400)
                                  :date (nlk:trimmed (or (tag-text block "pubDate") ""))
                                  :at (parse-date (tag-text block "pubDate"))
                                  :publisher (if (plusp (length source)) source suffix))
                            items)))))))))
      (list :source (if (plusp (length name)) name "feed")
            :items (nreverse items)))))

(defun fetch-topic (phrase)
  "PHRASE's items from the provider: a source plist, or a refusal in words."
  (multiple-value-bind (body status) (http-get (topic-url phrase (setting :provider)))
    (unless (and status (<= 200 status 299))
      (fail "the news provider answered ~a" (or status "nothing")))
    (parse-topic-feed (body-text body))))
