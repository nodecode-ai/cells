;;;; cell.lisp --- one or more pages read here, as markdown, with no key.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is the Hermes plugin enzo-adami/hermes-plugin-web-local: a
;;;; web-extract provider Hermes picked for its web_extract tool. It fetched
;;;; each URL from the agent's own machine with httpx and reduced the page with
;;;; trafilatura (readability extraction, Apache-2.0), answering readable text,
;;;; a title, the page's metadata and one result per URL -- an error item, not
;;;; an exception, for a URL that could not be read. Nodecode ships a websearch
;;;; cell whose web:fetch reads a page itself and has no extract registry a cell
;;;; may join, so the same reader is here a call the model makes and one the
;;;; operator makes: (web-local:extract URL), the local_extract tool (one
;;;; call, several URLs), and /web-local.
;;;;
;;;; No HTML parser is in the serving image, so the readability reduction is
;;;; cl-ppcre's, as the websearch cell's is: drop what is never prose, narrow
;;;; to <main> or <article> when the page names one, then write headings, list
;;;; items, links, table cells and code fences as markdown. The metadata is the
;;;; page's own <title> and <meta> tags.
;;;;
;;;; Config, a section of its own:
;;;;   "web-local": {"timeout_seconds": 30, "max_bytes": 10485760,
;;;;                 "budget_seconds": 120}

(in-package #:nodecode-web-local)

;;; --- the words --------------------------------------------------------------------

(defparameter *primer*
  "web-local reads web pages from this machine and answers them as markdown: the
article's text, with navigation, sidebars, ads and scripts dropped, under the page's
own title, address and one line of what it says about itself (site, byline, publish
date, words). No key, no service: the request leaves from here and nothing else is
contacted. Two calls, both through eval, both strings:
  (web-local:extract \"https://example.org/an-article\")
      one page, as the model reads it. :format \"text\" answers it without markdown
      markers; :max-chars N clips it.
  (web-local:extract-many (list \"https://a\" \"https://b\"))
      several pages in one answer, each under its own address; a page that could not
      be read is an ERROR line of its own and never takes the others with it.
The local_extract tool is the same call from the model's side, and /web-local URL...
is the operator's own read. It reads a page, it does not find one: search first
(web:search), then read here the pages worth reading whole. ERROR: WEB-LOCAL-ERROR
names why a call was refused -- no address, one that is not http(s), one that points
into a private network, or more URLs than one call takes."
  "What (help :web-local) answers while the cell runs.")

(defparameter +user-agent+ "Nodecode-LocalExtract/1.0"
  "An honest product token, as the plugin sent: a host that refuses library user
agents still sees a token that names what is asking.")

(defparameter +accept+
  "text/html,application/xhtml+xml,application/xml;q=0.9,text/plain;q=0.8,*/*;q=0.5"
  "What the plugin asked for, so a host negotiates the page and not an image.")

(defparameter +max-urls+ 8
  "How many addresses one call takes: the plugin read a list, and this bounds one
answer's cost.")

(defparameter +default-format+ "markdown"
  "What a call answers when it names no format.")

(defparameter +formats+ '("markdown" "text")
  "The two renderings of a page: markdown, or plain text with no markers.")

;;; --- the network seam ----------------------------------------------------------------

(defun http-read (url &key timeout max-bytes)
  "URL fetched by the image's own HTTP exchange: => (values OCTETS STATUS
HEADERS TRUNCATED-P). Every test stubs this name; nothing else does."
  (nlk:http :get url
            :headers (list (cons "user-agent" +user-agent+)
                           (cons "accept" +accept+))
            :timeout timeout
            :max-bytes max-bytes))

(defvar *http* #'http-read
  "The cell's one network call, a DEFVAR so a reload keeps it and a test stubs
it: (URL &key TIMEOUT MAX-BYTES) => (values OCTETS STATUS HEADERS TRUNCATED-P).")

;;; --- addresses ---------------------------------------------------------------------

(defun nonblank (text)
  "TEXT without its surrounding whitespace, or NIL when nothing is left."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun host-of (url)
  "The host URL names, lower case and without its brackets, or NIL."
  (cl-ppcre:register-groups-bind (host) ("(?i)^https?://(\\[[^\\]]+\\]|[^/:?#]+)" url)
    (string-downcase (string-trim "[]" host))))

(defun private-host-p (host)
  "T when HOST is this machine, a private network, or a name only it resolves."
  (or (string= host "localhost")
      (string= host "::1")
      (string= host "0.0.0.0")
      (uiop:string-suffix-p host ".localhost")
      (uiop:string-suffix-p host ".local")
      (uiop:string-suffix-p host ".internal")
      (uiop:string-prefix-p "127." host)
      (uiop:string-prefix-p "10." host)
      (uiop:string-prefix-p "192.168." host)
      (uiop:string-prefix-p "169.254." host)
      (uiop:string-prefix-p "fe80" host)
      (and (cl-ppcre:scan "^172\\.(1[6-9]|2[0-9]|3[01])\\." host) t)
      (and (cl-ppcre:scan "^f[cd][0-9a-f]{0,2}:" host) t)))

(defun safe-address (url)
  "URL as the address to read, checked before any request goes out.
=> (values URL NIL), or (values NIL WHY)."
  (let ((url (nonblank url)))
    (cond ((null url) (values nil "url must be a non-empty string"))
          ((not (or (uiop:string-prefix-p "http://" (string-downcase url))
                    (uiop:string-prefix-p "https://" (string-downcase url))))
           (values nil (format nil "~s is not an http:// or https:// address" url)))
          ((null (host-of url)) (values nil (format nil "~s names no host" url)))
          ((private-host-p (host-of url))
           (values nil "Blocked: URL targets a private or internal network address"))
          (t (values url nil)))))

(defun origin-of (url)
  "The scheme and host of URL, which a relative link resolves against."
  (or (cl-ppcre:register-groups-bind (origin) ("(?i)^(https?://[^/?#]+)" url) origin) ""))

(defun absolute-url (href base)
  "HREF as an address: itself when it names a scheme, else resolved against
BASE's origin."
  (let ((href (nlk:trimmed href)))
    (cond ((cl-ppcre:scan "^[a-zA-Z][a-zA-Z0-9+.-]*:" href) href)
          ((uiop:string-prefix-p "//" href)
           (format nil "~a:~a"
                   (if (uiop:string-prefix-p "https://" (string-downcase base)) "https" "http")
                   href))
          ((uiop:string-prefix-p "/" href) (format nil "~a~a" (origin-of base) href))
          (t (format nil "~a/~a" (origin-of base) href)))))

;;; --- the page's own markup ------------------------------------------------------------

(defparameter *comment-scanner* (cl-ppcre:create-scanner "(?s)<!--.*?-->")
  "A comment never reads as prose.")

(defparameter *drop-element-scanner*
  (cl-ppcre:create-scanner
   "(?is)<(script|style|noscript|svg|iframe|template|head)\\b[^>]*>.*?</\\1\\s*>")
  "Elements whose text is never the page's prose. <head> is read for its title
and meta tags before this runs.")

(defparameter *scope-scanners*
  (mapcar (lambda (tag)
            (cl-ppcre:create-scanner (format nil "<~a\\b[^>]*>(.*?)</~a\\s*>" tag tag)
                                      :case-insensitive-mode t :single-line-mode t))
          '("main" "article" "body"))
  "Narrowing, in preference order: the first tag a page has wins.")

(defparameter *named-entities+
  '(("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\"") ("apos" . "'")
    ("nbsp" . " ") ("copy" . "©") ("reg" . "®") ("ndash" . "–") ("mdash" . "—")
    ("hellip" . "…") ("lsquo" . "‘") ("rsquo" . "’") ("ldquo" . "“") ("rdquo" . "”")
    ("laquo" . "«") ("raquo" . "»") ("middot" . "·") ("bull" . "•") ("trade" . "™")
    ("eacute" . "é") ("egrave" . "è") ("uuml" . "ü") ("ouml" . "ö") ("auml" . "ä")
    ("szlig" . "ß") ("ccedil" . "ç") ("ntilde" . "ñ"))
  "The named entities a page writes when its own charset is not asked.")

(defparameter *tag-scanner* (cl-ppcre:create-scanner "<[^>]+>" :single-line-mode t)
  "Any remaining tag, stripped once the ones that mean something are written.")

(defparameter *inline-tag-scanner*
  (cl-ppcre:create-scanner
   "</?(a|b|i|u|s|em|strong|span|code|small|sup|sub|abbr|cite|q|mark|time|label|font|wbr)\\b[^>]*>"
   :case-insensitive-mode t)
  "Tags that wrap a run of text inside a line: they vanish, where a block tag
becomes a space, so `<b>M</b>.' stays `M.'.")

(defparameter *link-scanner*
  (cl-ppcre:create-scanner
   "(?is)<a\\b[^>]*?\\bhref\\s*=\\s*[\"']?([^\"'\\s>]+)[\"']?[^>]*>(.*?)</a\\s*>")
  "A link, whose label and address become markup of their own.")

(defparameter *block-break-scanner*
  (cl-ppcre:create-scanner
   "</(p|div|h[1-6]|section|blockquote|pre|table|ul|ol|figure|figcaption)\\s*>"
   :case-insensitive-mode t)
  "A block that ends: paragraphs are separated by a blank line.")

(defparameter *line-break-scanner*
  (cl-ppcre:create-scanner "</(li|tr|dd|dt)\\s*>|<br\\s*/?>" :case-insensitive-mode t)
  "A line that ends, inside a list, a table or a paragraph.")

(defparameter *cell-scanner* (cl-ppcre:create-scanner "</t[dh]\\s*>" :case-insensitive-mode t)
  "A table cell ends where a markdown cell's bar goes.")

(defparameter *pre-scanner*
  (cl-ppcre:create-scanner "(?is)<pre\\b[^>]*>(.*?)</pre\\s*>")
  "A code block, kept whole and fenced.")

(defun decode-entities (text)
  "TEXT with the common named and every numeric entity decoded; an unknown name
survives as written."
  (cl-ppcre:regex-replace-all
   "&(#x[0-9a-fA-F]+|#[0-9]+|[a-zA-Z]+);" text
   (lambda (match name)
     (cond ((char= (char name 0) #\#)
            (let ((code (if (char-equal (char name 1) #\x)
                            (parse-integer name :start 2 :radix 16 :junk-allowed t)
                            (parse-integer name :start 1 :junk-allowed t))))
              (if (and code (< 0 code char-code-limit))
                  (string (code-char code))
                  match)))
           (t (or (cdr (assoc name *named-entities+ :test #'string-equal)) match))))
   :simple-calls t))

(defun one-space (text)
  "TEXT with every run of whitespace inside a line made one space, trimmed."
  (nlk:trimmed (cl-ppcre:regex-replace-all "[ \\t\\n\\r]+" text " ")))

(defun write-link (href inner base format)
  "One anchor as FORMAT writes it: a markdown link, or the label and then its
address."
  (let ((label (one-space (cl-ppcre:regex-replace-all *tag-scanner* inner " ")))
        (url (absolute-url (decode-entities href) base)))
    (cond ((zerop (length label)) url)
          ((string= format "text") (format nil "~a (~a)" label url))
          (t (format nil "[~a](~a)" label url)))))

(defun inline-of (fragment base format)
  "FRAGMENT -- the inside of a heading or a list item -- as one line of text: its
links as FORMAT writes them, its tags gone, its entities decoded."
  (let* ((text (cl-ppcre:regex-replace-all
                 *link-scanner* fragment
                 (lambda (match href inner)
                   (declare (ignore match))
                   (write-link href inner base format))
                 :simple-calls t))
         (text (cl-ppcre:regex-replace-all *inline-tag-scanner* text ""))
         (text (cl-ppcre:regex-replace-all *tag-scanner* text " ")))
    (one-space (decode-entities text))))

(defun narrow (html)
  "The inner HTML of the first of <main>, <article>, <body> HTML has; HTML
itself when it names none of them."
  (dolist (scanner *scope-scanners* html)
    (cl-ppcre:register-groups-bind (inner) (scanner html)
      (when (nonblank inner) (return inner)))))

(defun reduce-html (html base format)
  "HTML as the page's prose: headings, list items, links, table cells and code
blocks as FORMAT writes them, everything that is not prose dropped."
  (let* ((text (cl-ppcre:regex-replace-all *comment-scanner* html ""))
         (text (cl-ppcre:regex-replace-all *drop-element-scanner* text ""))
         (text (narrow text))
         ;; Code first: its markup is not prose and its whitespace is its own.
         (text (cl-ppcre:regex-replace-all
                *pre-scanner* text
                (lambda (match inner)
                  (declare (ignore match))
                  (let ((code (string-right-trim
                               '(#\Space #\Tab #\Newline #\Return)
                               (decode-entities
                                (string-left-trim
                                 '(#\Newline #\Return #\Space)
                                 (cl-ppcre:regex-replace-all *tag-scanner* inner ""))))))
                    (if (zerop (length code))
                        ""
                        (format nil "~%~%```~%~a~%```~%~%" code))))
                :simple-calls t))
         ;; Headings, by their own level.
         (text (loop for level from 1 to 6
                      do (setf text
                               (cl-ppcre:regex-replace-all
                                (format nil "(?is)<h~d\\b[^>]*>(.*?)</h~d\\s*>" level level)
                                text
                                (lambda (match inner)
                                  (declare (ignore match))
                                  (let ((line (inline-of inner base format)))
                                    (if (zerop (length line))
                                        ""
                                        (format nil "~%~%~a ~a~%"
                                                (if (string= format "text")
                                                    ""
                                                    (make-string level :initial-element #\#))
                                                line))))
                                :simple-calls t))
                      finally (return text)))
         ;; A list item is a line of its own, its link already written.
         (text (cl-ppcre:regex-replace-all
                "(?is)<li\\b[^>]*>(.*?)</li\\s*>" text
                (lambda (match inner)
                  (declare (ignore match))
                  (let ((line (inline-of inner base format)))
                    (if (zerop (length line)) "" (format nil "~%- ~a~%" line))))
                :simple-calls t))
         ;; The rest of the links.
         (text (cl-ppcre:regex-replace-all
                *link-scanner* text
                (lambda (match href inner)
                  (declare (ignore match))
                  (write-link href inner base format))
                :simple-calls t))
         (text (cl-ppcre:regex-replace-all *cell-scanner* text " | "))
         (text (cl-ppcre:regex-replace-all *block-break-scanner* text (format nil "~%~%")))
         (text (cl-ppcre:regex-replace-all *line-break-scanner* text (string #\Newline)))
         (text (cl-ppcre:regex-replace-all *inline-tag-scanner* text ""))
         (text (cl-ppcre:regex-replace-all *tag-scanner* text " "))
         (text (decode-entities text))
         (text (cl-ppcre:regex-replace-all "[ \\t]+" text " "))
         (text (cl-ppcre:regex-replace-all "[ \\t]*\\n[ \\t]*" text (string #\Newline)))
         (text (cl-ppcre:regex-replace-all "\\n{3,}" text (format nil "~%~%"))))
    (string-trim '(#\Space #\Tab #\Newline #\Return) text)))

;;; --- what the page says about itself ---------------------------------------------------

(defun attribute-of (attributes name)
  "ATTRIBUTES -- one tag's attribute text -- and NAME's value, or NIL."
  (cl-ppcre:register-groups-bind (value)
      ((format nil "(?i)(?:^|\\s)~a\\s*=\\s*[\"']?([^\"'>]*)" name) attributes)
    (nonblank (decode-entities (nlk:trimmed value)))))

(defun meta-pairs (html)
  "The page's <meta> tags as (NAME-OR-PROPERTY . CONTENT), lower case."
  (let ((pairs '()))
    (dolist (tag (cl-ppcre:all-matches-as-strings "(?is)<meta\\b[^>]*>" html)
                 (nreverse pairs))
      (let ((key (or (attribute-of tag "property") (attribute-of tag "name")))
            (value (attribute-of tag "content")))
        (when (and key value) (push (cons (string-downcase key) value) pairs))))))

(defun meta-value (pairs &rest keys)
  "The content of the first of KEYS the page carries."
  (loop for key in keys
        for pair = (assoc key pairs :test #'string=)
        when pair return (cdr pair)))

(defun text-of-element (html tag)
  "The text of the first TAG element HTML carries, tags gone, entities decoded."
  (cl-ppcre:register-groups-bind (inner)
      ((format nil "(?is)<~a\\b[^>]*>(.*?)</~a\\s*>" tag tag) html)
    (nonblank (one-space (decode-entities (cl-ppcre:regex-replace-all *tag-scanner* inner " "))))))

(defun page-title (html pairs)
  "The page's own title: <title>, else the social tags, else its first <h1>."
  (or (text-of-element html "title")
      (meta-value pairs "og:title" "twitter:title")
      (text-of-element html "h1")))

(defun page-metadata (html base)
  "The page's own title and <meta> tags, as the plugin's metadata mapping."
  (let ((pairs (meta-pairs html)))
    (list :title (or (page-title html pairs) "")
          :description (or (meta-value pairs "description" "og:description"
                                        "twitter:description")
                           "")
          :author (meta-value pairs "author" "article:author" "og:article:author")
          :sitename (or (meta-value pairs "og:site_name" "application-name") (host-of base))
          :date (meta-value pairs "article:published_time" "date" "pubdate"))))

(defun word-count (text)
  "How many words TEXT carries."
  (count-if (lambda (word) (plusp (length word))) (cl-ppcre:split "\\s+" text)))

;;; --- one page -----------------------------------------------------------------------

(defun content-type-of (headers)
  "The media type HEADERS name, lower case and without its parameters."
  (let ((value (and (hash-table-p headers)
                    (or (gethash "content-type" headers)
                        (gethash "Content-Type" headers)))))
    (string-downcase (nlk:trimmed
                      (or (first (cl-ppcre:split ";" (if (stringp value) value ""))) "")))))

(defun html-page-p (content-type head)
  "T when CONTENT-TYPE -- or, untyped, the document's own first bytes -- is a page
this reader reduces."
  (cond ((zerop (length content-type))
         (and (or (search "<html" head) (search "<!doctype" head)) t))
        (t (and (or (search "text/html" content-type)
                    (search "application/xhtml" content-type)
                    (search "application/xml" content-type)
                    (search "text/xml" content-type))
                t))))

(defun page-of-octets (octets content-type url format)
  "OCTETS as the PAGE the plugin read, or why they are no page."
  ;; => (values PAGE NIL) or (values NIL WHY). A page is a plist of :URL,
  ;; :TITLE, :TEXT, :FORMAT and :METADATA; TEXT is what the model reads.
  (let* ((body (nlk:decode-octets octets content-type))
         (head (string-downcase (subseq body 0 (min 1024 (length body))))))
    (cond
      ((search "text/plain" content-type)
       (if (nonblank body)
           (values (list :url url :title "" :format format :text (nlk:trimmed body)
                         :metadata (list :title "" :description "" :author nil
                                         :sitename (host-of url) :date nil))
                   nil)
           (values nil "No readable content could be extracted from this page")))
      ((html-page-p content-type head)
       (let* ((metadata (page-metadata body url))
              (text (reduce-html body url format)))
         (if (null (nonblank text))
             (values nil "No readable content could be extracted from this page — it may be script-rendered")
             (values (list :url url :title (getf metadata :title) :format format
                           :text text :metadata metadata)
                     nil))))
      (t (values nil (format nil "Unsupported content type '~a' — the local extract backend handles HTML/XML/plain text"
                             content-type))))))

(defun read-page (url format timeout)
  "URL fetched here and reduced, with at most TIMEOUT seconds on the exchange.
=> (values PAGE NIL), or (values NIL WHY): a page that could not be read is a
fact the caller shows, never an exception."
  (multiple-value-bind (address why) (safe-address url)
    (if (null address)
        (values nil why)
        (handler-case
            (multiple-value-bind (octets status headers truncated)
                (funcall *http* address
                         :timeout timeout
                         :max-bytes (getf (running-settings) :max-bytes))
              (cond
                ((not (eql 200 status))
                 (values nil (format nil "HTTP ~a fetching ~a" status address)))
                (truncated
                 (values nil (format nil "Response exceeds the ~:dMB local extract limit"
                                     (truncate (getf (running-settings) :max-bytes)
                                               (* 1024 1024)))))
                (t (page-of-octets octets (content-type-of headers) address format))))
          (nlk:turn-cancelled-condition (condition) (error condition))
          (error (condition)
            (values nil (format nil "Fetch failed: ~a"
                                 (nlk:one-line (princ-to-string condition)))))))))

;;; --- what the model reads -------------------------------------------------------------

(defun quoted (text)
  "TEXT as one blockquote: every line carries its own '> ', as the plugin wrote
a page's description."
  (with-output-to-string (out)
    (loop for line in (uiop:split-string text :separator '(#\Newline))
          for first = t then nil
          do (unless first (format out "~%"))
             (format out "> ~a" line))))

(defun metadata-line (metadata text)
  "One line naming what the page says about itself, only the parts it says."
  (let ((parts (remove nil
                        (list (nonblank (getf metadata :sitename))
                              (let ((author (nonblank (getf metadata :author))))
                                (and author (format nil "by ~a" author)))
                              (let ((date (nonblank (getf metadata :date))))
                                (and date (format nil "published ~a" date)))
                              (let ((words (word-count text)))
                                (and (plusp words) (format nil "~:d words" words)))))))
    (when parts (format nil "~{~a~^ · ~}" parts))))

(defun rendered (page max-chars)
  "PAGE as the text the model reads, clipped at MAX-CHARS when that is a
positive number, with the cut said out loud."
  (let* ((title (nonblank (getf page :title)))
         (text (getf page :text))
         (metadata (getf page :metadata))
         (description (nonblank (getf metadata :description)))
         (answer (with-output-to-string (out)
                   (when title (format out "~a~%" title))
                   (format out "~a~%" (getf page :url))
                   (let ((line (metadata-line metadata text)))
                     (when line (format out "~a~%" line)))
                   (when (and description (not (equal description title)))
                     (format out "~%~a~%" (quoted description)))
                   (format out "~%~a"
                           (string-right-trim '(#\Space #\Tab #\Newline #\Return)
                                              text)))))
    (if (and (integerp max-chars) (plusp max-chars) (> (length answer) max-chars))
        (format nil "~a~%[web-local: clipped at ~:d of ~:d characters; call again with max_chars 0 for all of it]"
                (subseq answer 0 max-chars) max-chars (length answer))
        answer)))

(defun failure-item (url why)
  "URL that could not be read, as the line the model reads."
  (format nil "~a~%ERROR: ~a" url why))

(defun joined (parts separator)
  "PARTS as one string, SEPARATOR between them and nowhere else."
  (with-output-to-string (out)
    (loop for (part . rest) on parts
          do (write-string part out)
             (when rest (write-string separator out)))))

;;; --- the batch --------------------------------------------------------------------

(defun checked-urls (urls)
  "URLS as the list of addresses one call takes, or a refusal."
  (let ((urls (cond ((stringp urls) (list urls))
                     ((vectorp urls) (coerce urls 'list))
                     ((listp urls) urls)
                     (t (fail "urls must be a list of addresses, got ~s" urls)))))
    (unless urls (fail "urls needs at least one address"))
    (when (> (length urls) +max-urls+)
      (fail "one call takes at most ~d addresses, got ~d" +max-urls+ (length urls)))
    (dolist (url urls)
      (unless (stringp url) (fail "every address must be a string, got ~s" url)))
    urls))

(defun extract-many (urls &key format max-chars)
  "URLS read in one answer, inside one batch deadline; a page that could not be
read is an ERROR line of its own and never takes the others with it."
  (let* ((settings (running-settings))
         (urls (checked-urls urls))
         (format (or (nonblank format) +default-format+))
         (budget (getf settings :budget-seconds))
         (limit (getf settings :timeout-seconds))
         (spent 0)
         (parts '()))
    (unless (member format +formats+ :test #'string=)
      (fail "format must be one of ~{~a~^, ~}, got ~s" +formats+ format))
    (dolist (url urls)
      (let* ((left (- budget spent))
             (started (get-internal-real-time))
             (item (if (<= left 0)
                       (failure-item
                        url
                        (format nil "Extract batch time budget exhausted (~:d seconds) — page may be too large or unresponsive"
                                budget))
                       (multiple-value-bind (page why)
                           (read-page url format (min limit left))
                         (if page (rendered page max-chars) (failure-item url why))))))
        (setf spent (ceiling (+ (* spent internal-time-units-per-second)
                                (- (get-internal-real-time) started))
                             internal-time-units-per-second))
        (push item parts)))
    (joined (nreverse parts) (format nil "~%~%---~%~%"))))

;;; --- the verb --------------------------------------------------------------------

(define-verb extract (url &key format max-chars)
  "Read URL here as markdown (or, with FORMAT \"text\", as plain text): its
title, address, one line of what the page says about itself, then the page's
prose. MAX-CHARS, when positive, clips the answer. No key and no service: the
request leaves from this machine. ERROR: WEB-LOCAL-ERROR says why a call was
refused; a page that could not be read answers an ERROR line instead."
  (extract-many (list url) :format format :max-chars max-chars))

;;; --- the boundaries ---------------------------------------------------------------

(nlk:define-contract local-extract
  (:doc "Read one or more web pages from this machine as markdown: the article's text with navigation, sidebars and ads dropped, under the page's own title, address, byline and publish date. No key and no service beyond the page itself; pair it with web:search to find addresses. A page that cannot be read is an ERROR line of its own.")
  ("urls" :array :of :string :required t
                :doc "the addresses to read, http:// or https://, at most eight in one call")
  ("format" :string :options '("markdown" "text") :default "markdown"
            :slot answer-format
            :doc "markdown, or plain text with no markers")
  ("max_chars" :integer :default 0
               :doc "clip each page's answer at this many characters; 0, the default, keeps all of it"))

(defun local-extract-tool (call)
  "The local_extract tool: the pages CALL names, read here."
  (extract-many (local-extract-urls call)
                 :format (local-extract-answer-format call)
                 :max-chars (local-extract-max-chars call)))

;;; --- /web-local ------------------------------------------------------------------

(defun run-slash (args session-id)
  "/web-local URL...: the operator's own read of one or more pages, answered as
the text the model reads."
  (declare (ignore session-id))
  (let ((urls (remove "" (uiop:split-string (or args "")
                                             :separator '(#\Space #\Tab #\Newline))
                       :test #'string=)))
    (if (null urls)
        (format nil "web-local: type one or more URLs -- /web-local https://example.org/an-article. ~
                     The cell also gives the model (web-local:extract ...) and the local_extract tool.")
        (extract-many urls))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell web-local
  (:section ("web-local")
    (:guide "nothing here is required: the request leaves from this machine and no key is asked for; timeout_seconds bounds one page's exchange, budget_seconds the whole call, and max_bytes caps a page")
    ("timeout_seconds" :integer :default 30 :min 1
                       :doc "how many seconds one page's exchange may take")
    ("budget_seconds" :integer :default 120 :min 1
                      :doc "how many seconds one call may take over all its pages")
    ("max_bytes" :integer :default 10485760 :min 1024
                 :doc "the most one page may weigh, in bytes"))
  (:tool "local_extract" (nlk:contract 'local-extract) 'local-extract-tool)
  (:command "web-local" 'run-slash
            :catches 'web-local-error
            :description "Read pages here as markdown, with no key"
            :argument-hint "URL...")
  (:help :web-local "web-local: read one or more URLs here as markdown, no key - (web-local:extract \"https://…\")"
         *primer*))
