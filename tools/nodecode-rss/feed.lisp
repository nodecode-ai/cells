;;;; feed.lisp --- the network seam, the XML shapes, the library on disk.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through one HTTP GET, so a test stubs
;;;; that one name (HTTP-GET) and never touches the network. Feeds are read
;;;; by a small tolerant scanner rather than a real XML parser: an RSS 2.0
;;;; <item> and an Atom <entry> carry the same six things this cell keeps --
;;;; title, link, summary, date -- and a feed that is a little malformed
;;;; should still list its stories.

(in-package #:nodecode-rss)

;;; --- the library -----------------------------------------------------------------

(defparameter *library* nil
  "The library in memory: a plist (see EMPTY-LIBRARY). NIL until the cell starts.")

(defun empty-library ()
  "A library with no subscription, no article and no mute rule yet."
  (list :seq 0 :feeds '() :articles '() :mutes '()))

(defun library-path ()
  "The one file the library lives in, under the Nodecode home."
  (nlk:home "rss/library.sexp"))

(defun load-library ()
  "Read the library off disk, or start empty; a damaged file never stops the cell."
  (let ((path (library-path)))
    (setf *library*
          (if (probe-file path)
              (handler-case
                  (let* ((*read-eval* nil)
                         (form (read-from-string (uiop:read-file-string path) nil nil)))
                    (if (and (consp form) (listp (getf form :feeds)) (listp (getf form :articles)))
                        form
                        (empty-library)))
                (error () (empty-library)))
              (empty-library)))))

(defun save-library ()
  "Write the library where LOAD-LIBRARY reads it: one readable form."
  (let* ((path (library-path))
         (tmp (merge-pathnames "library.sexp.tmp" (uiop:pathname-directory-pathname path))))
    (ensure-directories-exist path)
    (with-open-file (out tmp :direction :output :if-exists :supersede :external-format :utf-8)
      (let ((*print-pretty* t) (*print-length* nil) (*print-level* nil) (*print-circle* nil))
        (write *library* :stream out :readably t)))
    (uiop:rename-file-overwriting-target tmp path)))

(defun keep ()
  "Save the library, and let a full disk cost an answer nothing."
  (ignore-errors (save-library)))

(defun feeds () (getf *library* :feeds))
(defun articles () (getf *library* :articles))
(defun mutes () (getf *library* :mutes))

(defun (setf feeds) (value) (setf (getf *library* :feeds) value))
(defun (setf articles) (value) (setf (getf *library* :articles) value))
(defun (setf mutes) (value) (setf (getf *library* :mutes) value))

(defun field-set (plist key value)
  "Set KEY in PLIST in place, where the entry already exists."
  (let ((cell (member key plist :test #'eq)))
    (when cell (setf (cadr cell) value)))
  value)

(defun next-id (prefix)
  "A fresh id, PREFIX and a number, numbered once per library."
  (let ((n (1+ (or (getf *library* :seq) 0))))
    (setf (getf *library* :seq) n)
    (format nil "~a~d" prefix n)))

;;; --- subscriptions ----------------------------------------------------------------

(defun find-feed (id)
  "The subscription with ID, or NIL."
  (find id (feeds) :key (lambda (feed) (getf feed :id)) :test #'equal))

(defun feed-by-url (url)
  "The subscription to URL, or NIL."
  (find url (feeds) :key (lambda (feed) (getf feed :url)) :test #'equal))

(defun feed-name (feed)
  "What a person calls FEED: its title once known, its url until then."
  (let ((title (getf feed :title)))
    (if (and (stringp title) (plusp (length title)))
        (nlk:clip title 60)
        (nlk:clip (getf feed :url) 60))))

(defun http-url-p (url)
  "URL is a non-empty http:// or https:// address."
  (and (stringp url)
       (plusp (length url))
       (let ((url (string-downcase url)))
         (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url)))))

(defun normalize-url (url)
  "URL as typed, trimmed, or a refusal saying what a feed address looks like."
  (let ((url (nlk:trimmed (or url ""))))
    (unless (http-url-p url)
      (fail "~s is not a feed address: give the http:// or https:// url of an RSS or Atom feed" url))
    url))

(defun add-feed (url &optional title)
  "Subscribe to URL (and NAME it): the existing subscription, or the new one."
  (let* ((url (normalize-url url))
         (existing (feed-by-url url)))
    (or existing
        (let ((feed (list :id (next-id "f") :url url :title (or title "") :fetched 0 :error "")))
          (setf (feeds) (append (feeds) (list feed)))
          feed))))

(defun drop-feed (id)
  "Unsubscribe ID, and drop its articles: the subscription that went."
  (let ((feed (find-feed (or id ""))))
    (unless feed
      (fail "no subscription ~s: the list action names every id" (or id "")))
    (setf (feeds) (remove feed (feeds) :test #'eq))
    (setf (articles) (remove (getf feed :id) (articles)
                              :key (lambda (article) (getf article :feed)) :test #'equal))
    feed))

(defun seed-feeds (spec)
  "Subscribe to the URLs SPEC names, comma-, newline- or space-separated."
  (dolist (url (uiop:split-string (or spec "") :separator '(#\, #\Space #\Tab #\Newline #\Return)))
    (let ((url (nlk:trimmed url)))
      (when (and (plusp (length url)) (http-url-p url))
        (add-feed url)))))

;;; --- articles ---------------------------------------------------------------------

(defun find-article (id)
  "The article with ID, or NIL."
  (find id (articles) :key (lambda (article) (getf article :id)) :test #'equal))

(defun feed-articles (feed-id)
  "FEED-ID's articles, newest first."
  (remove-if-not (lambda (article) (equal feed-id (getf article :feed))) (articles)))

(defun merge-entries (feed entries)
  "Add what ENTRIES carries that FEED does not hold yet; => how many were new."
  (let* ((id (getf feed :id))
         (seen (make-hash-table :test #'equal))
         (fresh '()))
    (dolist (article (feed-articles id)) (setf (gethash (getf article :key) seen) t))
    (dolist (entry entries)
      (let ((key (or (let ((url (getf entry :url))) (and url (plusp (length url)) url))
                     (format nil "~a|~a" (getf entry :title) (getf entry :date)))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push (list :id (next-id "a") :feed id :key key
                      :title (or (getf entry :title) "")
                      :url (or (getf entry :url) "")
                      :summary (or (getf entry :summary) "")
                      :date (or (getf entry :date) "")
                      :read nil :star nil)
                fresh))))
    (let ((count (length fresh)))
      (when fresh (setf (articles) (append (nreverse fresh) (articles))))
      count)))

(defun prune-feed (feed-id)
  "Keep FEED-ID's newest MAX-ENTRIES unstarred articles, and all its starred ones."
  (let ((ceiling (setting :max-entries)) (kept '()) (unstarred 0))
    (dolist (article (articles))
      (if (equal feed-id (getf article :feed))
          (cond ((getf article :star) (push article kept))
                ((< unstarred ceiling) (incf unstarred) (push article kept)))
          (push article kept)))
    (setf (articles) (nreverse kept))))

(defun refresh-feed (feed)
  "Fetch FEED once and merge what is new in it; => how many articles were new."
  (handler-case
      (multiple-value-bind (body status) (http-get (getf feed :url))
        (cond
          ((not (and body (integerp status) (<= 200 status 299)))
           (field-set feed :error (format nil "HTTP ~a" (or status "failure")))
           0)
          (t
           (multiple-value-bind (title entries) (parse-feed (body-text body))
             (when (and title (plusp (length title)))
               (field-set feed :title title))
             (field-set feed :fetched (get-universal-time))
             (field-set feed :error "")
             (prog1 (merge-entries feed entries)
               (prune-feed (getf feed :id)))))))
    (error (condition)
      (field-set feed :error (nlk:clip (format nil "~a" condition) 160))
      0)))

(defun muted-p (article)
  "ARTICLE carries one of the library's mute phrases."
  (let ((haystack (string-downcase (format nil "~a ~a" (or (getf article :title) "")
                                           (or (getf article :summary) "")))))
    (some (lambda (phrase) (search (string-downcase phrase) haystack)) (mutes))))

(defun unread-articles (&optional feed-id)
  "What is unread, unmuted and, with FEED-ID, that feed's alone; newest first."
  (remove-if (lambda (article)
               (or (getf article :read)
                   (muted-p article)
                   (and feed-id (not (equal feed-id (getf article :feed))))))
             (articles)))

;;; --- the network seam ---------------------------------------------------------------

(defvar *http* nil
  "When bound, the seam a test stubs: (funcall *HTTP* URL) => (values BODY STATUS).")

(defun http-get (url)
  "The cell's one network call; a test binds *HTTP* to stub it.
=> (values BODY STATUS), BODY a string or the octets the cap left."
  (when *http* (return-from http-get (funcall *http* url)))
  (nlk:http :get url
            :headers (list (cons "user-agent" (setting :user-agent))
                           (cons "accept" "application/rss+xml, application/atom+xml, application/xml;q=0.9, text/xml;q=0.9, */*;q=0.8"))
            :timeout (setting :timeout-seconds)
            :max-bytes (* 2 1024 1024)))

(defun body-text (body)
  "BODY as a string: what the wire sent, decoded as UTF-8 when it is octets."
  (cond ((stringp body) body)
        ((typep body '(vector (unsigned-byte 8)))
         (handler-case (sb-ext:octets-to-string body :external-format :utf-8)
           (error () "")))
        (t (format nil "~a" body))))

;;; --- the XML reader ------------------------------------------------------------------

(defparameter *entities*
  '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"") ("&apos;" . "'")
    ("&nbsp;" . " ") ("&#39;" . "'") ("&#8217;" . "'") ("&#8216;" . "'")
    ("&#8220;" . "\"") ("&#8221;" . "\"") ("&hellip;" . "...") ("&mdash;" . "-") ("&ndash;" . "-"))
  "The named and numeric references a feed's text carries, decoded in one pass.")

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
          (cond (b (write-string (subseq text (+ a (length open)) b) out)
                   (setf start (+ b (length close))))
                (t (write-string (subseq text a) out)
                   (setf start (length text))
                   (return (get-output-stream-string out)))))))))

(defun strip-markup (text)
  "TEXT with every <tag ...> taken out."
  (with-output-to-string (out)
    (let ((start 0))
      (loop
        (let ((a (position #\< text :start start)))
          (when (null a)
            (write-string (subseq text start) out)
            (return))
          (write-string (subseq text start a) out)
          (let ((b (position #\> text :start a)))
            (when (null b) (return))
            (setf start (1+ b))))))))

(defun decode-entities (text)
  "TEXT with its HTML references spelled."
  (let ((text (reduce (lambda (text pair)
                        (uiop:frob-substrings text (list (car pair)) (cdr pair)))
                      *entities* :initial-value text)))
    (with-output-to-string (out)
      (let ((start 0))
        (loop
          (let ((a (search "&#" text :start2 start)))
            (when (null a) (write-string (subseq text start) out) (return))
            (write-string (subseq text start a) out)
            (let* ((end (position #\; text :start a))
                   (digits (and end (subseq text (+ a 2) end)))
                   (code (and digits (plusp (length digits)) (ignore-errors (parse-integer digits)))))
              (cond ((and code (> code 0) (< code 1114112))
                     (write-char (code-char code) out)
                     (setf start (1+ end)))
                    (t (write-string "&#" out) (setf start (+ a 2)))))))))))

(defun collapse-space (text)
  "TEXT as one line: every run of whitespace one space."
  (with-output-to-string (out)
    (let ((space nil))
      (loop for character across text
            do (if (member character '(#\Space #\Tab #\Newline #\Return #\Page))
                   (setf space t)
                   (progn (when space (write-char #\Space out))
                          (setf space nil)
                          (write-char character out)))))))

(defun clean (text &key (limit 600))
  "TEXT as reading text: CDATA unwrapped, markup stripped, references spelled,
whitespace collapsed, then cut to LIMIT characters."
  (let* ((text (or text ""))
         (text (unwrap-cdata text))
         (text (decode-entities text))
         (text (strip-markup text))
         (text (collapse-space text)))
    (nlk:clip (nlk:trimmed text) limit :ellipsis "...")))

(defun opening-tags (text name)
  "The opening tag of every <NAME ...> in TEXT, in order."
  (let ((open (format nil "<~a" name)) (start 0) (out '()))
    (loop
      (let ((a (search open text :start2 start :test #'char-equal)))
        (when (null a) (return (nreverse out)))
        (let ((gt (position #\> text :start a)))
          (when (null gt) (return (nreverse out)))
          (push (subseq text a (1+ gt)) out)
          (setf start (1+ gt)))))))

(defun tag-attr (tag name)
  "NAME=\"...\" inside TAG, or NIL."
  (let* ((needle (format nil "~a=\"" name))
         (a (search needle tag :test #'char-equal)))
    (when a
      (let* ((start (+ a (length needle)))
             (end (position #\" tag :start start)))
        (and end (subseq tag start end))))))

(defun xml-blocks (text name)
  "The inner text of every <NAME ...>...</NAME> block in TEXT, in order."
  (let ((open (format nil "<~a" name))
        (close (format nil "</~a>" name))
        (start 0)
        (out '()))
    (loop
      (let ((a (search open text :start2 start :test #'char-equal)))
        (when (null a) (return (nreverse out)))
        (let ((gt (position #\> text :start a)))
          (when (null gt) (return (nreverse out)))
          (cond
            ((and (plusp gt) (char= (char text (1- gt)) #\/))
             (push "" out)
             (setf start (1+ gt)))
            (t (let ((b (search close text :start2 (1+ gt) :test #'char-equal)))
                 (cond (b (push (subseq text (1+ gt) b) out)
                          (setf start (+ b (length close))))
                       (t (return (nreverse out))))))))))))

(defun entry-link (block)
  "The address BLOCK points at: an Atom <link href>, else an RSS <link> or <guid>."
  (dolist (tag (opening-tags block "link"))
    (let ((href (tag-attr tag "href")) (rel (tag-attr tag "rel")))
      (when (and href (not (and rel (string-equal rel "self"))))
        (return-from entry-link href))))
  (let ((text (first (xml-blocks block "link"))))
    (cond ((and text (http-url-p (nlk:trimmed (strip-markup text))))
           (nlk:trimmed (strip-markup text)))
          (t (let ((guid (first (xml-blocks block "guid"))))
               (and guid (nlk:trimmed (strip-markup guid))))))))

(defun entry-of (block)
  "One entry of BLOCK as (:title :url :summary :date), or NIL when it says nothing."
  (let* ((title (clean (or (first (xml-blocks block "title")) "") :limit 300))
         (summary (clean (or (first (xml-blocks block "description"))
                             (first (xml-blocks block "summary"))
                             (first (xml-blocks block "content"))
                             "")
                         :limit 500))
         (date (clean (or (first (xml-blocks block "pubdate"))
                          (first (xml-blocks block "published"))
                          (first (xml-blocks block "updated"))
                          (first (xml-blocks block "date"))
                          "")
                     :limit 40))
         (link (entry-link block))
         (url (if (http-url-p link) link "")))
    (when (or (plusp (length title)) (plusp (length url)))
      (list :title title :url url :summary summary :date date))))

(defun parse-feed (text)
  "Read a feed: => (values FEED-TITLE ENTRIES), ENTRIES each (:title :url :summary :date)."
  (let* ((items (xml-blocks text "item"))
         (blocks (if items items (xml-blocks text "entry")))
         (title (clean (or (first (xml-blocks text "title")) "") :limit 300))
         (entries (loop for block in blocks
                        for entry = (entry-of block)
                        when entry collect entry)))
    (values (and (plusp (length title)) title) entries)))
