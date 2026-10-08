;;;; engines.lisp --- the keyless engines behind omp's web search, and their merge.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi's coding-agent/src/web/search/providers/: google.ts,
;;;; startpage.ts, ecosia.ts, mojeek.ts, duckduckgo.ts, searxng.ts, public.ts
;;;; (the merge), browser-headers.ts (the fixed Chrome identity) and utils.ts.
;;;;
;;;;   google      GET  www.google.com/search?q=&num=&hl=en&gl=us&udm=14&pws=0[&tbs=qdr:X]
;;;;               each <h3> inside a result link; /url?q= unwrapped; the snippet
;;;;               the result's VwiC3b (else IsZvec, s3v9rd) block
;;;;   startpage   GET  www.startpage.com/ for the search form's hidden inputs (its
;;;;               `sc' token), then POST /sp/search with them and the query; a
;;;;               GET /sp/search?query= when the form cannot be read
;;;;               div.result > a.result-link (h2 title), p.description
;;;;   duckduckgo  POST html.duckduckgo.com/html/ q=&kl=us-en[&df=]&b=
;;;;               div.result > a.result__a (uddg redirect unwrapped), .result__snippet,
;;;;               the date in result__extras__url
;;;;   ecosia      GET  www.ecosia.org/search?q=
;;;;               article[data-test-id=organic-result], its result-title link,
;;;;               its web-result-description
;;;;   mojeek      GET  www.mojeek.de/search?q=&t=&arc=none&lang=en&lb=en&theme=dark[&since=]
;;;;               ul.results-standard > li: a.title, p.s
;;;;   searxng     GET  <endpoint>/search?q=&format=json[...]  an instance of your own
;;;;               results[].{title,url,content,publishedDate}, answers, suggestions
;;;;   public      the five scrapers above at once, merged: one url (host without
;;;;               www, path without a trailing slash, query kept) counted once,
;;;;               ranked by how many engines named it, then its best rank, then
;;;;               the engine order startpage, google, duckduckgo, ecosia, mojeek;
;;;;               it answers once every engine has, or 5 s in with one answer,
;;;;               or at 30 s with whatever it has
;;;;
;;;; A blocked page (a bot challenge, a CAPTCHA, an automated-queries wall) is
;;;; an engine failure with omp's words for it, a 429. omp falls back to a
;;;; headless browser for Google, Ecosia and Mojeek when a plain fetch is
;;;; blocked; this cell has none (README, Gaps).

(in-package #:nodecode-web-provider)

(defparameter +engines+ '("public" "google" "startpage" "duckduckgo" "ecosia" "mojeek" "searxng")
  "What :engine and web-provider.engine may name.")

(defparameter +public-engines+ '("startpage" "google" "duckduckgo" "ecosia" "mojeek")
  "The scrapers the public merge asks, in its tiebreak order (PUBLIC_ENGINE_IDS).")

(defparameter +recencies+ '("day" "week" "month" "year")
  "What :recency may name: a pure time filter, ignored by an engine that has none.")

(defparameter *soft-seconds* 5
  "Past this the public merge answers as soon as one engine has (SOFT_DEADLINE_MS).")

(defparameter *hard-seconds* 30
  "At this the public merge answers with whatever it has (HARD_DEADLINE_MS).")

(defparameter +timeout+ 20
  "Seconds one engine request may take.")

;;; --- the condition an engine fails with -----------------------------------------------

(defun nonblank (value)
  "VALUE trimmed when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

(define-condition engine-failure (error)
  ((engine :initarg :engine :reader failure-engine)
   (status :initarg :status :initform nil :reader failure-status)
   (detail :initarg :detail :reader failure-detail))
  (:report (lambda (condition stream) (write-string (failure-detail condition) stream))))

(defun engine-fail (engine status format-control &rest arguments)
  (error 'engine-failure :engine engine :status status
                         :detail (apply #'format nil format-control arguments)))

;;; --- the one network call ---------------------------------------------------------------

(defun http (method url &rest arguments)
  "The cell's one network call, NLK:HTTP; tests stub DEX:REQUEST under it."
  (apply #'nlk:http method url arguments))

(defparameter +chrome-headers+
  '(("accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7")
    ;; omp's set asks br and zstd too; the client here decodes neither
    ("accept-encoding" . "gzip, deflate")
    ("accept-language" . "en-US,en;q=0.9")
    ("cache-control" . "max-age=0")
    ("priority" . "u=0, i")
    ("sec-ch-ua" . "\"Google Chrome\";v=\"149\", \"Chromium\";v=\"149\", \";Not A Brand\";v=\"99\"")
    ("sec-ch-ua-mobile" . "?0")
    ("sec-ch-ua-platform" . "\"macOS\"")
    ("sec-fetch-dest" . "document")
    ("sec-fetch-mode" . "navigate")
    ("sec-fetch-site" . "none")
    ("sec-fetch-user" . "?1")
    ("upgrade-insecure-requests" . "1")
    ("user-agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36"))
  "A desktop Mac Chrome navigation (CHROME_FALLBACK_HEADERS).")

(defun browser-headers (&key referer more)
  "The Chrome navigation, a REFERER making it a same-origin one, then MORE."
  (append (if referer
              (cons (cons "referer" referer)
                    (remove "sec-fetch-site" +chrome-headers+ :key #'car :test #'string=))
              +chrome-headers+)
          (and referer '(("sec-fetch-site" . "same-origin")))
          more))

(defun fetch-page (engine method url &key headers content)
  "(values HTML STATUS) of one page; a transport failure is ENGINE's failure."
  (multiple-value-bind (body status)
      (handler-case (http method url :headers headers :content content :timeout +timeout+)
        (error (condition)
          (engine-fail engine 503 "~a search failed: ~a" (engine-label engine) condition)))
    (values (or (and body (nlk:body-text body)) "") status)))

(defun engine-label (engine)
  (cdr (assoc engine '(("google" . "Google") ("startpage" . "Startpage") ("duckduckgo" . "DuckDuckGo")
                       ("ecosia" . "Ecosia") ("mojeek" . "Mojeek") ("searxng" . "SearXNG") ("public" . "Public Web"))
              :test #'equal)))

(defun query-string (&rest pairs)
  "PAIRS (name value ...) as a URL query, a NIL value left out."
  (format nil "~{~a~^&~}"
          (loop for (name value) on pairs by #'cddr
                when value collect (format nil "~a=~a" name (quri:url-encode (princ-to-string value) :encoding :utf-8)))))

(defun classify-status (engine status body)
  "omp's quota and auth reading of a refused page (classifyProviderHttpError), or NIL."
  (cond ((ppcre:scan "(?i)credits?\\s*(?:exhausted|exceeded)|quota|insufficient" body)
         (engine-fail engine status "~a: credits exhausted" engine))
        ((eql status 402) (engine-fail engine status "~a: 402 credits exhausted" engine))
        ((eql status 401) (engine-fail engine status "~a: 401 unauthorized" engine))
        ((eql status 403) (engine-fail engine status "~a: 403 forbidden" engine))))

(defun require-ok (engine status body)
  (unless (and (integerp status) (<= 200 status 299))
    (classify-status engine status body)
    (engine-fail engine status "~a HTML error (~a)" (engine-label engine) status)))

;;; --- HTML, read with regular expressions ---------------------------------------------------

(defun html-text (value)
  "VALUE, an HTML fragment, as text: tags out, entities decoded, whitespace
collapsed (decodeHtmlText, normalizeSearchText)."
  (let* ((text (ppcre:regex-replace-all "<[^>]*>" (or value "") " "))
         (text (ppcre:regex-replace-all "&#(\\d+);" text
                                        (lambda (match code)
                                          (declare (ignore match))
                                          (string (code-char (parse-integer code))))
                                        :simple-calls t))
         (text (ppcre:regex-replace-all "(?i)&#x([0-9a-f]+);" text
                                        (lambda (match code)
                                          (declare (ignore match))
                                          (string (code-char (parse-integer code :radix 16))))
                                        :simple-calls t)))
    (loop for (entity . char) in '(("&nbsp;" . " ") ("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"")
                                   ("&#39;" . "'") ("&apos;" . "'") ("&amp;" . "&"))
          do (setf text (ppcre:regex-replace-all (format nil "(?i)~a" entity) text char)))
    (string-trim " " (ppcre:regex-replace-all "\\s+" text " "))))

(defun attribute (tag name)
  "The value of attribute NAME in the opening TAG, entities decoded, or NIL."
  (ppcre:register-groups-bind (nil value)
      ((format nil "(?i)\\s~a\\s*=\\s*([\"'])(.*?)\\1" name) tag)
    (ppcre:regex-replace-all "(?i)&amp;" value "&")))

(defun class-p (tag class)
  "Whether the opening TAG's class list holds CLASS as a whole token."
  (let ((classes (attribute tag "class")))
    (and classes (member class (ppcre:split "\\s+" classes) :test #'string=) t)))

(defun opening-tags (html element)
  "Each opening tag of ELEMENT in HTML: (values TAG START END) per tag, as a list of (TAG START END)."
  (let ((found '()))
    (ppcre:do-matches (start end (format nil "(?i)<~a\\b[^>]*>" element) html)
      (push (list (subseq html start end) start end) found))
    (nreverse found)))

(defun inner (html element start)
  "The inner HTML of the ELEMENT whose opening tag ends at START, to its
first closing tag."
  (let ((close (ppcre:scan (format nil "(?i)</~a\\s*>" element) html :start start)))
    (subseq html start (or close (length html)))))

(defun http-url-p (url)
  (and (stringp url) (ppcre:scan "^https?://" url)))

(defun host-of (url)
  (ignore-errors (string-downcase (quri:uri-host (quri:uri url)))))

(defun absolute (href base)
  "HREF resolved against BASE."
  (ignore-errors (quri:render-uri (quri:merge-uris (quri:uri href) (quri:uri base)))))

(defun hit (title url &optional snippet date)
  "One result: (:title :url :snippet :date)."
  (list :title title :url url :snippet (and snippet (plusp (length snippet)) snippet) :date date))

(defun first-hits (hits n)
  "The first N of HITS, one per url."
  (let ((seen (make-hash-table :test 'equal)) (out '()))
    (dolist (hit hits)
      (unless (gethash (getf hit :url) seen)
        (setf (gethash (getf hit :url) seen) t)
        (push hit out)
        (when (>= (length out) n) (return))))
    (nreverse out)))

;;; --- google ---------------------------------------------------------------------------------

(defparameter +google-home+ "https://www.google.com/")

(defun google-unwrap (href)
  "The target a Google result HREF points at: a /url?q= redirect unwrapped,
Google's own pages refused."
  (let* ((absolute (absolute href +google-home+))
         (host (host-of absolute)))
    (when absolute
      (if (and (member host '("google.com" "www.google.com") :test #'equal)
               (equal "/url" (quri:uri-path (quri:uri absolute))))
          (let* ((query (quri:uri-query-params (quri:uri absolute)))
                 (target (or (cdr (assoc "q" query :test #'equal)) (cdr (assoc "url" query :test #'equal)))))
            (and (http-url-p target) (not (member (host-of target) '("google.com" "www.google.com") :test #'equal))
                 target))
          (and (http-url-p absolute) (not (member host '("google.com" "www.google.com") :test #'equal))
               absolute)))))

(defun google-results (html)
  (let ((hits '()))
    (ppcre:do-scans (start end starts ends
                     "(?is)<a\\b([^>]*)>(?:(?!</a>).)*?<h3\\b[^>]*>(.*?)</h3>" html)
      (let* ((tag (subseq html (aref starts 0) (aref ends 0)))
             (href (attribute (format nil " ~a" tag) "href"))
             (url (and href (google-unwrap href)))
             (title (html-text (subseq html (aref starts 1) (aref ends 1))))
             (after (subseq html end (min (length html) (or (ppcre:scan "(?i)<h3\\b" html :start end)
                                                           (+ end 4000)))))
             (snippet (loop for class in '("VwiC3b" "IsZvec" "s3v9rd")
                            for text = (ppcre:register-groups-bind (body)
                                           ((format nil "(?is)<(?:div|span)\\b[^>]*class=\"[^\"]*\\b~a\\b[^\"]*\"[^>]*>(.*?)</(?:div|span)>" class)
                                            after)
                                         (ppcre:regex-replace "\\s*Read more$" (html-text body) ""))
                            when (plusp (length (or text ""))) return text)))
        (when (and url (plusp (length title)))
          (push (hit title url snippet) hits))))
    (nreverse hits)))

(defun search-google (query n recency)
  (let ((url (format nil "https://www.google.com/search?~a"
                     (query-string "q" query "num" n "hl" "en" "gl" "us" "udm" "14" "pws" "0"
                                   "tbs" (cdr (assoc recency '(("day" . "qdr:d") ("week" . "qdr:w")
                                                               ("month" . "qdr:m") ("year" . "qdr:y"))
                                                     :test #'equal))))))
    (multiple-value-bind (html status) (fetch-page "google" :get url :headers (browser-headers :referer +google-home+))
      (when (or (eql status 403) (eql status 429)
                (ppcre:scan "(?i)unusual traffic|detected unusual traffic|g-recaptcha|/sorry/" html))
        (engine-fail "google" 429 "Google blocked the search with an automated-traffic challenge. Try another engine or retry later."))
      (require-ok "google" status html)
      (when (and (cl:search "/httpservice/retry/enablejs" html) (not (ppcre:scan "(?i)<h3\\b" html)))
        (engine-fail "google" 429 "Google returned its JavaScript challenge instead of rendered search results."))
      (first-hits (google-results html) n))))

;;; --- startpage --------------------------------------------------------------------------------

(defparameter +startpage-home+ "https://www.startpage.com/")
(defparameter +startpage-search+ "https://www.startpage.com/sp/search")

(defun startpage-challenge-p (html)
  (or (cl:search "component---src-pages-captcha" html) (cl:search "/sp/captcha" html)))

(defun startpage-form (html)
  "The hidden inputs of the homepage's /sp/search form as an alist, or NIL
without its `sc' token."
  (ppcre:register-groups-bind (form) ("(?is)<form\\b[^>]*action=\"/sp/search\"[^>]*>(.*?)</form>" html)
    (let ((inputs (loop for (tag) in (opening-tags form "input")
                        when (equalp "hidden" (attribute tag "type"))
                          collect (cons (attribute tag "name") (or (attribute tag "value") "")))))
      (and (cdr (assoc "sc" inputs :test #'equal)) (remove nil inputs :key #'car)))))

(defun startpage-results (html)
  "Each div.result's link, title and description, in document order."
  (let ((hits '())
        (blocks (remove-if-not (lambda (entry) (class-p (first entry) "result")) (opening-tags html "div"))))
    (loop for (entry . more) on blocks
          for block = (subseq html (third entry) (if more (second (first more)) (length html)))
          do (let ((anchor (find-if (lambda (tag) (class-p (first tag) "result-link")) (opening-tags block "a"))))
               (when anchor
                 (let* ((href (attribute (first anchor) "href"))
                        (url (and href (absolute href +startpage-home+)))
                        (body (inner block "a" (third anchor)))
                        (title (html-text (or (ppcre:register-groups-bind (heading) ("(?is)<h[23]\\b[^>]*>(.*?)</h[23]>" body) heading)
                                              body)))
                        (description (find-if (lambda (tag) (class-p (first tag) "description")) (opening-tags block "p"))))
                   (when (and (http-url-p url) (plusp (length title))
                              (not (ppcre:scan "(^|\\.)startpage\\.com$" (or (host-of url) ""))))
                     (push (hit title url (and description (html-text (inner block "p" (third description)))))
                           hits))))))
    (nreverse hits)))

(defun search-startpage (query n recency)
  (let* ((with-date (cdr (assoc recency '(("day" . "d") ("week" . "w") ("month" . "m") ("year" . "y")) :test #'equal)))
         (inputs (handler-case
                     (multiple-value-bind (html status) (fetch-page "startpage" :get +startpage-home+ :headers (browser-headers))
                       (and (integerp status) (<= 200 status 299) (not (startpage-challenge-p html))
                            (startpage-form html)))
                   (engine-failure () nil))))
    (multiple-value-bind (html status)
        (if inputs
            (fetch-page "startpage" :post +startpage-search+
                        :headers (browser-headers :referer +startpage-home+
                                                  :more '(("content-type" . "application/x-www-form-urlencoded")))
                        :content (apply #'query-string
                                        (append (loop for (name . value) in (remove "query" inputs :key #'car :test #'equal)
                                                      collect name collect value)
                                                (list "query" query "with_date" with-date))))
            (fetch-page "startpage" :get (format nil "~a?~a" +startpage-search+ (query-string "query" query "with_date" with-date))
                        :headers (browser-headers :referer +startpage-home+)))
      (when (startpage-challenge-p html)
        (engine-fail "startpage" 429 "Startpage blocked the request with a CAPTCHA challenge. Startpage rate-limits automated searches from datacenter and shared-egress addresses; try another engine such as duckduckgo or mojeek, or retry later."))
      (require-ok "startpage" status html)
      (first-hits (startpage-results html) n))))

;;; --- duckduckgo -------------------------------------------------------------------------------

(defun ddg-unwrap (href)
  "The target of a DuckDuckGo result HREF: its uddg redirect unwrapped."
  (let ((decoded (ppcre:regex-replace-all "(?i)&amp;" href "&")))
    (alexandria:if-let (wrapped (ppcre:register-groups-bind (value) ("[?&]uddg=([^&]+)" decoded) value))
      (ignore-errors (quri:url-decode wrapped :encoding :utf-8))
      (cond ((uiop:string-prefix-p "//" decoded) (concatenate 'string "https:" decoded))
            ((http-url-p decoded) decoded)))))

(defun ddg-date (block)
  "The date a DuckDuckGo result BLOCK's result__extras__url carries, or NIL."
  (ppcre:register-groups-bind (extras)
      ("(?is)<div\\b[^>]*\\bclass=\"[^\"]*\\bresult__extras__url\\b[^\"]*\"[^>]*>(.*?)</div>" block)
    (let ((found nil))
      (ppcre:do-register-groups (span) ("(?is)<span\\b[^>]*>(.*?)</span>" extras)
        (let ((text (html-text span)))
          (when (and (not found) (ppcre:scan "^\\d{4}-\\d{2}-\\d{2}(?:[T ]\\d{2}:\\d{2}|$)" text))
            (setf found text))))
      found)))

(defun ddg-results (html)
  (let ((hits '()))
    (ppcre:do-register-groups (block)
        ("(?is)<div\\b[^>]*\\bclass=\"[^\"]*\\bresult\\b[^\"]*\"[^>]*>(.*?)(?=<div\\b[^>]*\\bclass=\"[^\"]*\\bresult\\b|<div\\b[^>]*\\bclass=\"[^\"]*\\bnav-link\\b|$)"
         html)
      (ppcre:register-groups-bind (href title)
          ("(?is)<a\\b[^>]*\\bclass=\"[^\"]*\\bresult__a\\b[^\"]*\"[^>]*\\bhref=\"([^\"]+)\"[^>]*>(.*?)</a>" block)
        (let ((url (ddg-unwrap href))
              (title (html-text title))
              (snippet (ppcre:register-groups-bind (body)
                           ("(?is)<(?:a|div|span)\\b[^>]*\\bclass=\"[^\"]*\\bresult__snippet\\b[^\"]*\"[^>]*>(.*?)</(?:a|div|span)>" block)
                         (html-text body))))
          (when (and url (plusp (length title)))
            (push (hit title url snippet (let ((date (ddg-date block))) (and date (subseq date 0 10)))) hits)))))
    (nreverse hits)))

(defun search-duckduckgo (query n recency)
  (multiple-value-bind (html status)
      (fetch-page "duckduckgo" :post "https://html.duckduckgo.com/html/"
                  :headers (browser-headers :referer "https://html.duckduckgo.com/"
                                            :more '(("content-type" . "application/x-www-form-urlencoded")))
                  :content (query-string "q" query "kl" "us-en"
                                         "df" (cdr (assoc recency '(("day" . "d") ("week" . "w") ("month" . "m") ("year" . "y"))
                                                          :test #'equal))
                                         "b" ""))
    (require-ok "duckduckgo" status html)
    (when (or (cl:search "anomaly-modal" html) (cl:search "anomaly.js" html))
      (engine-fail "duckduckgo" 429 "DuckDuckGo blocked the request with a bot-detection challenge; it throttles automated HTML searches from datacenter and shared-egress addresses."))
    (first-hits (ddg-results html) n)))

;;; --- ecosia ----------------------------------------------------------------------------------

(defun ecosia-results (html)
  (let ((hits '()))
    (ppcre:do-register-groups (article)
        ("(?is)<article\\b[^>]*data-test-id=\"organic-result\"[^>]*>(.*?)</article>" html)
      (ppcre:register-groups-bind (href heading)
          ("(?is)<a\\b[^>]*\\bhref=\"([^\"]+)\"[^>]*>(?:(?!</a>).)*?data-test-id=\"result-title\"[^>]*>(.*?)</(?:h2|h3|span|div)>" article)
        (let* ((url (absolute (ppcre:regex-replace-all "(?i)&amp;" href "&") "https://www.ecosia.org/"))
               (title (html-text heading))
               (snippet (or (ppcre:register-groups-bind (body)
                                ("(?is)data-test-id=\"web-result-description\"[^>]*>(.*?)</p>" article)
                              (html-text body))
                            (ppcre:register-groups-bind (body)
                                ("(?is)data-test-id=\"result-description\"[^>]*>(.*?)</(?:div|p)>" article)
                              (html-text body)))))
          (when (and (http-url-p url) (plusp (length title))
                     (not (member (host-of url) '("ecosia.org" "www.ecosia.org") :test #'equal)))
            (push (hit title url snippet) hits)))))
    (nreverse hits)))

(defun search-ecosia (query n recency)
  (declare (ignore recency))           ; Ecosia's results carry no date filter
  (multiple-value-bind (html status)
      (fetch-page "ecosia" :get (format nil "https://www.ecosia.org/search?~a" (query-string "q" query))
                  :headers (browser-headers :referer "https://www.ecosia.org/"))
    (when (or (eql status 403) (eql status 429) (cl:search "Ecosia Firewall" html) (cl:search "_cf_chl_opt" html)
              (cl:search "/cdn-cgi/challenge-platform/" html) (ppcre:scan "(?i)confirm you.{0,3}re not a robot" html))
      (engine-fail "ecosia" 429 "Ecosia blocked the request with a Cloudflare bot challenge; its firewall throttles automated searches from datacenter and shared-egress addresses."))
    (require-ok "ecosia" status html)
    (first-hits (ecosia-results html) n)))

;;; --- mojeek -----------------------------------------------------------------------------------

(defparameter +mojeek-home+ "https://www.mojeek.de/?arc=none&lang=en&lb=en&theme=dark")

(defun mojeek-own-p (url)
  (let ((host (or (host-of url) "")))
    (some (lambda (domain) (or (string= host domain) (uiop:string-suffix-p host (concatenate 'string "." domain))))
          '("mojeek.com" "mojeek.co.uk" "mojeek.fr" "mojeek.de"))))

(defun mojeek-results (html)
  (let ((hits '()))
    (ppcre:do-register-groups (list)
        ("(?is)<ul\\b[^>]*\\bclass=\"[^\"]*\\bresults-standard\\b[^\"]*\"[^>]*>(.*?)</ul>" html)
      (ppcre:do-register-groups (item) ("(?is)<li\\b[^>]*>(.*?)</li>" list)
        (let ((anchor (find-if (lambda (entry) (class-p (first entry) "title")) (opening-tags item "a"))))
          (when anchor
            (let* ((href (attribute (first anchor) "href"))
                   (url (and href (absolute href +mojeek-home+)))
                   (title (html-text (inner item "a" (third anchor))))
                   (snippet (let ((p (find-if (lambda (entry) (class-p (first entry) "s")) (opening-tags item "p"))))
                              (and p (html-text (inner item "p" (third p)))))))
              (when (and (http-url-p url) (plusp (length title)) (not (mojeek-own-p url)))
                (push (hit title url snippet) hits)))))))
    (nreverse hits)))

(defun search-mojeek (query n recency)
  (multiple-value-bind (html status)
      (fetch-page "mojeek" :get (format nil "https://www.mojeek.de/search?~a"
                                        (query-string "q" query "t" n "arc" "none" "lang" "en" "lb" "en"
                                                      "theme" "dark" "since" recency))
                  :headers (browser-headers :referer +mojeek-home+))
    (when (and (or (cl:search "altcha-widget" html) (cl:search "captcha-wrap" html)
                   (ppcre:scan "(?i)sending automated queries" html))
               (not (cl:search "results-standard" html)))
      (engine-fail "mojeek" 429 "Mojeek blocked the request with its automated-queries wall; it rate-limits scripted searches from datacenter and shared-egress addresses. Retry later or try another engine."))
    (require-ok "mojeek" status html)
    (first-hits (mojeek-results html) n)))

;;; --- searxng ---------------------------------------------------------------------------------

(defun searxng-endpoint ()
  (or (nonblank (setting :searxng-endpoint)) (nle::credential-env "SEARXNG_ENDPOINT")))

(defun searxng-auth ()
  "The Authorization header value for the instance: Basic auth first, then a
bearer token, from the section or the environment; or NIL."
  (let ((user (or (nonblank (setting :searxng-basic-username)) (nle::credential-env "SEARXNG_BASIC_USERNAME")))
        (password (or (nonblank (setting :searxng-basic-password)) (nle::credential-env "SEARXNG_BASIC_PASSWORD")))
        (token (or (nonblank (setting :searxng-token)) (nle::credential-env "SEARXNG_TOKEN"))))
    (cond ((or user password)
           (unless (and user password)
             (fail "SearXNG Basic auth needs both searxng_basic_username and searxng_basic_password"))
           (when (find #\: user) (fail "a SearXNG Basic auth username cannot contain ':'"))
           (format nil "Basic ~a" (cl-base64:string-to-base64-string
                                   (sb-ext:octets-to-string
                                    (sb-ext:string-to-octets (format nil "~a:~a" user password) :external-format :utf-8)
                                    :external-format :latin-1))))
          (token (format nil "Bearer ~a" token)))))

(defun secrets ()
  "What a refusal's text is cleaned of: the SearXNG token and password. None
while the cell is not running: reading the settings then refuses, and the
refusal is itself cleaned."
  (and *web-provider*
       (remove nil (list (or (nonblank (setting :searxng-token)) (nle::credential-env "SEARXNG_TOKEN"))
                         (or (nonblank (setting :searxng-basic-password)) (nle::credential-env "SEARXNG_BASIC_PASSWORD"))))))

(defun searxng-headers ()
  (let ((auth (searxng-auth)))
    `(("accept" . "application/json") ,@(and auth `(("authorization" . ,auth))))))

(defvar *engine-names* (make-hash-table :test 'equal :synchronized t)
  "Per instance, its /config's lowercased engine names and shortcuts -> names.")

(defun engine-names (base)
  "BASE's engine name map from GET /config, kept for the process; NIL on any failure (not kept)."
  (or (gethash base *engine-names*)
      (let ((map (ignore-errors
                  (multiple-value-bind (body status) (http :get (format nil "~a/config" base)
                                                           :headers (searxng-headers) :timeout +timeout+)
                    (when (eql status 200)
                      (let ((map (make-hash-table :test 'equal)))
                        (loop for engine across (nlk:json-array (nlk:decode-json (nlk:body-text body)) "engines")
                              for name = (nlk:json-value engine :text "name")
                              for shortcut = (nlk:json-value engine :text "shortcut")
                              when name
                                do (setf (gethash (string-downcase name) map) name)
                                   (when shortcut (setf (gethash (string-downcase shortcut) map) name)))
                        (and (plusp (hash-table-count map)) map)))))))
        (when map (setf (gethash base *engine-names*) map))
        map)))

(defun searxng-engines (base)
  "The section's engines, names or shortcuts, as the names engines= takes."
  (alexandria:when-let (raw (nonblank (setting :searxng-engines)))
    (let ((entries (remove "" (mapcar #'nlk:trimmed (uiop:split-string raw :separator ",")) :test #'equal)))
      (when entries
        (let ((map (engine-names base)))
          (format nil "~{~a~^,~}" (mapcar (lambda (entry) (or (and map (gethash (string-downcase entry) map)) entry))
                                          entries)))))))

(defun answer-text (answer)
  "Displayable text of one SearXNG answer: a string, {answer}, translations, or weather."
  (cond ((stringp answer) (nonblank answer))
        ((not (hash-table-p answer)) nil)
        ((nonblank (nlk:json-value answer :string "answer")))
        ((nlk:json-value answer :array "translations")
         (let ((texts (loop for item across (nlk:json-value answer :array "translations")
                            for text = (nonblank (nlk:json-value item :string "text"))
                            when text collect text)))
           (and texts (format nil "~{~a~^~%~}" (subseq texts 0 (min 3 (length texts)))))))
        ((hash-table-p (gethash "current" answer))
         (let ((current (gethash "current" answer)))
           (or (nonblank (nlk:json-value current :string "summary"))
               (let ((parts (remove nil (list (nonblank (nlk:json-value current :string "location" "name"))
                                              (let ((value (nlk:json-value current :any "temperature" "val")))
                                                (and value (format nil "~a~@[~a~]" value
                                                                   (nlk:json-value current :string "temperature" "unit"))))
                                              (nonblank (nlk:json-value current :string "condition"))))))
                 (and parts (format nil "~{~a~^: ~}" parts))))))))

(defun search-searxng (query n recency)
  "=> (values HITS ANSWER SUGGESTIONS)."
  (let ((endpoint (searxng-endpoint)))
    (unless endpoint
      (engine-fail "searxng" nil "SearXNG endpoint not configured: set web-provider.searxng_endpoint or SEARXNG_ENDPOINT"))
    (let* ((base (string-right-trim "/" endpoint))
           (safesearch (nonblank (setting :searxng-safesearch)))
           (url (format nil "~a/search?~a" base
                        (query-string "q" (format nil "~{~a~^ ~}" (remove-if (lambda (part) (uiop:string-prefix-p "!!" part))
                                                                             (ppcre:split "\\s+" query)))
                                      "format" "json"
                                      "pageno" "1"
                                      "time_range" (cdr (assoc recency '(("day" . "day") ("week" . "month")
                                                                         ("month" . "month") ("year" . "year"))
                                                               :test #'equal))
                                      "categories" (nonblank (setting :searxng-categories))
                                      "engines" (searxng-engines base)
                                      "safesearch" safesearch
                                      "language" (nonblank (setting :searxng-language))))))
      (multiple-value-bind (body status)
          (handler-case (http :get url :headers (searxng-headers) :timeout +timeout+)
            (error (condition) (engine-fail "searxng" 503 "SearXNG search failed: ~a" condition)))
        (let ((text (or (and body (nlk:body-text body)) "")))
          (unless (eql status 200)
            (classify-status "searxng" status text)
            (engine-fail "searxng" status "SearXNG API error (~a): ~a" status (nlk:clip (nlk:one-line text) 300 :ellipsis "…")))
          (let* ((answer (ignore-errors (nlk:decode-json text)))
                 (hits (loop for result across (nlk:json-array answer "results")
                             for url = (nlk:json-value result :text "url")
                             when url
                               collect (hit (or (nlk:json-value result :text "title") url) url
                                            (nonblank (or (nlk:json-value result :string "content")
                                                          (nlk:json-value result :string "snippet")))
                                            (let ((date (or (nlk:json-value result :text "publishedDate")
                                                            (nlk:json-value result :text "published_date"))))
                                              (and date (ppcre:scan "^\\d{4}-\\d{2}-\\d{2}" date) (subseq date 0 10))))))
                 (failed (nlk:json-array answer "unresponsive_engines")))
            (when (and (null hits) (plusp (length failed)))
              (engine-fail "searxng" 503 "SearXNG returned no usable results; upstream engines failed: ~{~a~^; ~}"
                           (loop for pair across failed
                                 collect (if (vectorp pair) (format nil "~{~a~^: ~}" (coerce pair 'list)) pair))))
            (values (subseq hits 0 (min n (length hits)))
                    (let ((texts (loop for item across (nlk:json-array answer "answers")
                                       for text = (answer-text item)
                                       when text collect text)))
                      (and texts (format nil "~{~a~^~%~%~}" (subseq texts 0 (min 3 (length texts))))))
                    (coerce (nlk:json-array answer "suggestions") 'list))))))))

;;; --- the public merge -------------------------------------------------------------------------

(defun dedup-key (url)
  "URL as the merge counts it: host lowercased without www., path without a
trailing slash, the query kept, the fragment dropped."
  (or (ignore-errors
       (let* ((uri (quri:uri url))
              (host (ppcre:regex-replace "^www\\." (string-downcase (quri:uri-host uri)) ""))
              (path (or (quri:uri-path uri) "")))
         (when (and (> (length path) 1) (uiop:string-suffix-p path "/"))
           (setf path (subseq path 0 (1- (length path)))))
         (format nil "~a~a~@[?~a~]" host path (quri:uri-query uri))))
      url))

(defun merge-hits (lists n)
  "LISTS, each an engine's ranked hits in engine order, merged (searchPublicWeb)."
  (let ((merged (make-hash-table :test 'equal)) (order 0))
    (dolist (hits lists)
      (loop for hit in hits
            for rank from 0
            for key = (dedup-key (getf hit :url))
            for entry = (gethash key merged)
            do (if (null entry)
                   (setf (gethash key merged) (list :hit (copy-list hit) :engines 1 :rank rank :order (incf order)))
                   (let ((kept (getf entry :hit)))
                     (incf (getf entry :engines))
                     (when (< rank (getf entry :rank))
                       (setf (getf entry :rank) rank
                             (getf kept :title) (getf hit :title)
                             (getf kept :url) (getf hit :url)))
                     ;; the longest snippet, whoever ranked it best
                     (when (> (length (or (getf hit :snippet) "")) (length (or (getf kept :snippet) "")))
                       (setf (getf kept :snippet) (getf hit :snippet)))
                     (unless (getf kept :date) (setf (getf kept :date) (getf hit :date)))
                     (setf (getf entry :hit) kept (gethash key merged) entry)))))
    (let ((entries (loop for entry being the hash-values of merged collect entry)))
      (mapcar (lambda (entry) (getf entry :hit))
              (subseq (sort entries (lambda (a b)
                                      (or (> (getf a :engines) (getf b :engines))
                                          (and (= (getf a :engines) (getf b :engines))
                                               (or (< (getf a :rank) (getf b :rank))
                                                   (and (= (getf a :rank) (getf b :rank))
                                                        (< (getf a :order) (getf b :order))))))))
                      0 (min n (length entries)))))))

(defun one-engine (engine query n recency)
  "ENGINE's hits for QUERY. => (values HITS ANSWER SUGGESTIONS)"
  (cond ((equal engine "google") (search-google query n recency))
        ((equal engine "startpage") (search-startpage query n recency))
        ((equal engine "duckduckgo") (search-duckduckgo query n recency))
        ((equal engine "ecosia") (search-ecosia query n recency))
        ((equal engine "mojeek") (search-mojeek query n recency))
        ((equal engine "searxng") (search-searxng query n recency))))

(defun search-public (query n recency)
  "Every keyless scraper at once, merged. => (values HITS NOTES)"
  (let* ((count (length +public-engines+))
         (slots (make-array count :initial-element nil))
         (lock (bt2:make-lock :name "web-provider public"))
         (settings *web-provider*)
         (started (get-internal-real-time)))
    (loop for engine in +public-engines+
          for index from 0
          do (let ((engine engine) (index index))
               (bt2:make-thread
                (lambda ()
                  (let* ((*web-provider* settings)
                         (outcome (handler-case (list :hits (one-engine engine query n recency))
                                    (engine-failure (condition) (list :failure (failure-detail condition)))
                                    (error (condition) (list :failure (princ-to-string condition))))))
                    (bt2:with-lock-held (lock) (setf (aref slots index) outcome))))
                :name (format nil "web-provider ~a" engine))))
    (flet ((elapsed () (/ (- (get-internal-real-time) started) internal-time-units-per-second))
           (settled () (bt2:with-lock-held (lock) (count-if #'identity slots)))
           ;; an engine that answered, with results or none (firstSuccess)
           (answered () (bt2:with-lock-held (lock) (count-if (lambda (slot) (eq :hits (first slot))) slots))))
      (loop until (or (= (settled) count)
                      (and (>= (elapsed) *soft-seconds*) (plusp (answered)))
                      (>= (elapsed) *hard-seconds*))
            do (sleep 0.05)))
    ;; stragglers finish on their own threads; what they bring is not read
    (let ((outcomes (bt2:with-lock-held (lock) (copy-seq slots))))
      (let ((lists (loop for outcome across outcomes collect (getf outcome :hits)))
            (notes (loop for engine in +public-engines+
                         for outcome across outcomes
                         collect (cond ((null outcome) (format nil "~a: no answer in time" engine))
                                       ((getf outcome :failure) (format nil "~a: ~a" engine (getf outcome :failure)))))))
        ;; only every engine failing is a failure; one that is still out is not
        (when (every (lambda (outcome) (getf outcome :failure)) outcomes)
          (fail "All public engines failed: ~{~a~^; ~}" (remove nil notes)))
        (values (merge-hits lists n) (remove nil notes))))))
