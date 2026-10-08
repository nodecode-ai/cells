;;;; cell-test.lisp --- the web-provider cell against canned engine pages.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every engine is a stubbed dex:request (NLK:HTTP's own call) answering a
;;;; small page shaped like the engine's, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing touches the network or the environment.

(in-package #:nodecode.test)

(define-test-slice "web-provider" "WEB-PROVIDER-CELL-" :start nodecode-web-provider:start-cell)

(define-cell-lifecycle-tests "web-provider"
  (:help :engines)
  (:refused ("engine" "bing") ("max_results" 0))
  (:idle engines:web-provider-error (engines:search "x")))

;;; --- fixtures -------------------------------------------------------------------

(defparameter +wp-google+
  "<html><body><div class=\"MjjYud\"><div class=\"tF2Cxc\"><a href=\"/url?q=https://example.com/a&amp;sa=U\"><h3 class=\"LC20lb\">Example <b>A</b></h3></a><div class=\"VwiC3b\">About A things. Read more</div></div></div>
<div class=\"MjjYud\"><a href=\"https://example.net/c\"><h3>Example C</h3></a><span class=\"VwiC3b\">C &amp; more</span></div>
<a href=\"https://www.google.com/preferences\"><h3>Search settings</h3></a></body></html>")

(defparameter +wp-startpage-home+
  "<form action=\"/sp/search\" method=\"post\" id=\"search\"><input type=\"hidden\" name=\"sc\" value=\"SC-TOKEN\"><input type=\"hidden\" name=\"t\" value=\"device\"><input type=\"text\" name=\"query\"></form>")

(defparameter +wp-startpage+
  "<div class=\"a-bg-result\"><a class=\"result-link\" href=\"https://honeypot.example/\"><h2>Trap</h2></a></div>
<div class=\"w-gl result\"><a class=\"result-link\" href=\"https://example.com/a\"><h2 class=\"wgl-title\">Example A</h2></a><p class=\"description\">A from Startpage, the longest snippet of all the engines</p></div>
<div class=\"w-gl result\"><a class=\"result-title result-link\" href=\"https://www.example.net/c/\"><h2>Example C</h2></a><p class=\"description\">C</p></div>")

(defparameter +wp-duckduckgo+
  "<div class=\"result results_links web-result\"><div class=\"links_main\"><h2 class=\"result__title\"><a rel=\"nofollow\" class=\"result__a\" href=\"//duckduckgo.com/l/?uddg=https%3A%2F%2Fexample.com%2Fa&amp;rut=x\">Example <b>A</b></a></h2><div class=\"result__extras\"><div class=\"result__extras__url\"><a class=\"result__url\" href=\"x\">example.com</a><span>&nbsp; &nbsp; 2026-07-30T20:19:00.0000000</span></div></div><a class=\"result__snippet\" href=\"x\">A from <b>DuckDuckGo</b></a></div></div>
<div class=\"result results_links web-result\"><div class=\"links_main\"><a class=\"result__a\" href=\"https://d.example/\">Example D</a><div class=\"result__snippet\">D</div></div></div>
<div class=\"nav-link\"><form><input name=\"s\" value=\"30\"></form></div>")

(defparameter +wp-ecosia+
  "<article class=\"result\" data-test-id=\"organic-result\"><div><a href=\"https://example.com/a\" class=\"result__link\"><h2 data-test-id=\"result-title\">Example A</h2></a><div data-test-id=\"result-description\"><p data-test-id=\"web-result-description\">A from Ecosia</p></div></div></article>
<article data-test-id=\"organic-result\"><a href=\"https://e.example/page\"><h2 data-test-id=\"result-title\">Example E</h2></a></article>")

(defparameter +wp-mojeek+
  "<ul class=\"results-standard\"><li class=\"r1\"><a class=\"ob\" href=\"https://example.com/a\">example.com</a><h2><a class=\"title\" href=\"https://example.com/a\">Example A</a></h2><p class=\"s\">A from Mojeek</p></li><li><h2><a class=\"title\" href=\"https://www.mojeek.com/about\">About Mojeek</a></h2></li><li><h2><a class=\"title\" href=\"https://m.example/\">Example M</a></h2><p class=\"s\">M</p></li></ul>")

(defvar *wp-lock* (bt2:make-lock :name "wp test"))

(defmacro with-web ((requests &key pages config env) &body forms)
  "FORMS with the cell started on CONFIG and the engines answering PAGES,
each (MATCH . ANSWER): the first whose MATCH is in the url (or, for a POST,
`POST ' and the url) answers, ANSWER a page, (BODY STATUS), or a function of
the url answering one of those; held lexically, so the merge's engine
threads see it. REQUESTS each (METHOD URL HEADERS CONTENT) sent, oldest first."
  `(with-cell-stop ((web-provider-start ,@config))
     (let ((,requests '()) (pages ,pages))
       (with-stubbed-fdefinitions
           ((nle::credential-env (name) (cdr (assoc name ',env :test #'equal)))
            (dex:request (url &rest args)
                         (let ((method (getf args :method)))
                           (bt2:with-lock-held (*wp-lock*)
                             (setf ,requests (append ,requests (list (list method url (getf args :headers) (getf args :content))))))
                           (let* ((key (if (eq method :post) (format nil "POST ~a" url) url))
                                  (answer (cdr (find-if (lambda (entry) (cl:search (car entry) key)) pages)))
                                  (answer (if (functionp answer) (funcall answer url) answer)))
                             (cond ((stringp answer) (values answer 200 (make-hash-table :test 'equal)))
                                   (answer (values (first answer) (second answer) (make-hash-table :test 'equal)))
                                   (t (values "not here" 404 (make-hash-table :test 'equal))))))))
         ,@forms))))

(defun wp-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defun wp-params (url)
  "URL's query parameters as an alist."
  (quri:uri-query-params (quri:uri url)))

(defun wp-urls (text)
  "The result urls in a rendered TEXT, in order."
  (let ((urls '()))
    (ppcre:do-register-groups (url) ("(?m)^   (\\S+)$" text) (push url urls))
    (nreverse urls)))

;;; --- one engine at a time --------------------------------------------------------------

(deftest web-provider-cell-asks-google ()
  (with-web (requests :pages (list (cons "www.google.com/search" +wp-google+)))
    (let ((text (engines:search "example a" :engine "google" :recency "week" :n 5)))
      (is (equal '("https://example.com/a" "https://example.net/c") (wp-urls text))
          "the /url redirect unwrapped, Google's own pages left out")
      (is (search "1. Example A - About A things." text) "the snippet without its Read more")
      (is (search "C & more" text) "entities decoded"))
    (destructuring-bind (method url headers content) (first requests)
      (declare (ignore content))
      (is (eq :get method))
      (let ((params (wp-params url)))
        (is (equal "example a" (cdr (assoc "q" params :test #'equal))))
        (is (equal "14" (cdr (assoc "udm" params :test #'equal))) "the web results tab")
        (is (equal "qdr:w" (cdr (assoc "tbs" params :test #'equal))) "a week is qdr:w")
        (is (equal "5" (cdr (assoc "num" params :test #'equal)))))
      (is (search "Chrome/149" (wp-header headers "user-agent")) "a browser's navigation")
      (is (equal "https://www.google.com/" (wp-header headers "referer")))
      (is (equal "same-origin" (wp-header headers "sec-fetch-site"))))))

(deftest web-provider-cell-says-a-blocked-engine ()
  (with-web (requests :pages (list (cons "www.google.com" "<html><form id=\"captcha-form\"><div class=\"g-recaptcha\"></div></form></html>")))
    (is (search "automated-traffic challenge"
                (refusal-text engines:web-provider-error (engines:search "x" :engine "google")))))
  (with-web (requests :pages (list (cons "www.ecosia.org" '("<title>Ecosia Firewall</title>" 403))))
    (is (search "Cloudflare bot challenge" (refusal-text engines:web-provider-error (engines:search "x" :engine "ecosia")))))
  (with-web (requests :pages (list (cons "www.mojeek.de" "<title>Captcha</title><altcha-widget></altcha-widget>")))
    (is (search "automated-queries wall" (refusal-text engines:web-provider-error (engines:search "x" :engine "mojeek")))))
  (with-web (requests :pages (list (cons "html.duckduckgo.com" "<div class=\"anomaly-modal\"></div>")))
    (is (search "bot-detection challenge" (refusal-text engines:web-provider-error (engines:search "x" :engine "duckduckgo"))))))

(deftest web-provider-cell-asks-startpage-through-its-form ()
  (with-web (requests :pages (list (cons "POST https://www.startpage.com/sp/search" +wp-startpage+)
                                   (cons "https://www.startpage.com/" +wp-startpage-home+)))
    (let ((text (engines:search "example" :engine "startpage" :recency "day")))
      (is (equal '("https://example.com/a" "https://www.example.net/c/") (wp-urls text)) "the honeypot is no result"))
    (is (equal '(:get :post) (mapcar #'first requests)) "the homepage first, then the form")
    (let ((form (quri:url-decode-params (fourth (second requests)))))
      (is (equal "SC-TOKEN" (cdr (assoc "sc" form :test #'equal))) "the session token goes back")
      (is (equal "device" (cdr (assoc "t" form :test #'equal))))
      (is (equal "example" (cdr (assoc "query" form :test #'equal))))
      (is (equal "d" (cdr (assoc "with_date" form :test #'equal)))))
    (is (equal "application/x-www-form-urlencoded" (wp-header (third (second requests)) "content-type"))))
  (with-web (requests :pages (list (cons "/sp/search?query=" +wp-startpage+)
                                   (cons "https://www.startpage.com/" "<html>no form</html>")))
    (is (search "Example A" (engines:search "example" :engine "startpage")) "no form: a plain GET")))

(deftest web-provider-cell-asks-duckduckgo ()
  (with-web (requests :pages (list (cons "html.duckduckgo.com" +wp-duckduckgo+)))
    (let ((text (engines:search "example" :engine "duckduckgo" :recency "month")))
      (is (equal '("https://example.com/a" "https://d.example/") (wp-urls text)) "the uddg redirect unwrapped")
      (is (search "Example A - A from DuckDuckGo (2026-07-30)" text) "the row's date"))
    (let ((form (quri:url-decode-params (fourth (first requests)))))
      (is (equal "example" (cdr (assoc "q" form :test #'equal))))
      (is (equal "us-en" (cdr (assoc "kl" form :test #'equal))))
      (is (equal "m" (cdr (assoc "df" form :test #'equal)))))))

(deftest web-provider-cell-asks-ecosia-and-mojeek ()
  (with-web (requests :pages (list (cons "www.ecosia.org/search" +wp-ecosia+)))
    (let ((text (engines:search "example" :engine :ecosia)))
      (is (equal '("https://example.com/a" "https://e.example/page") (wp-urls text)))
      (is (search "A from Ecosia" text))))
  (with-web (requests :pages (list (cons "www.mojeek.de/search" +wp-mojeek+)))
    (let ((text (engines:search "example" :engine "mojeek" :recency "year" :n 3)))
      (is (equal '("https://example.com/a" "https://m.example/") (wp-urls text)) "Mojeek's own pages left out"))
    (let ((params (wp-params (second (first requests)))))
      (is (equal "year" (cdr (assoc "since" params :test #'equal))))
      (is (equal "3" (cdr (assoc "t" params :test #'equal)))))))

;;; --- searxng ---------------------------------------------------------------------------

(deftest web-provider-cell-asks-a-searxng-instance ()
  (with-web (requests :pages (list (cons "searx.example/config"
                                         "{\"engines\":[{\"name\":\"duckduckgo\",\"shortcut\":\"ddg\"},{\"name\":\"brave\",\"shortcut\":\"br\"}]}")
                                   (cons "searx.example/search"
                                         "{\"results\":[{\"title\":\"Example A\",\"url\":\"https://example.com/a\",\"content\":\"A from SearXNG\",\"publishedDate\":\"2026-08-01T00:00:00\"}],\"answers\":[{\"answer\":\"42\"}],\"suggestions\":[\"example b\"]}"))
                      :config ("searxng_endpoint" "https://searx.example/" "searxng_engines" "ddg, br, custom"
                               "searxng_safesearch" "1")
                      :env (("SEARXNG_TOKEN" . "sx-token")))
    (let ((text (engines:search "example !!g" :engine "searxng" :recency "week")))
      (is (search "42" text) "the instance's own answer on top")
      (is (search "1. Example A - A from SearXNG (2026-08-01)" text))
      (is (search "Related: example b" text)))
    (let* ((search (find-if (lambda (r) (cl:search "/search" (second r))) requests))
           (params (wp-params (second search))))
      (is (equal "example" (cdr (assoc "q" params :test #'equal))) "an external bang is stripped")
      (is (equal "json" (cdr (assoc "format" params :test #'equal))))
      (is (equal "month" (cdr (assoc "time_range" params :test #'equal))) "SearXNG has no week")
      (is (equal "duckduckgo,brave,custom" (cdr (assoc "engines" params :test #'equal))) "shortcuts named by /config")
      (is (equal "1" (cdr (assoc "safesearch" params :test #'equal))))
      (is (equal "Bearer sx-token" (wp-header (third search) "authorization")))))
  (with-web (requests)
    (is (search "endpoint not configured" (refusal-text engines:web-provider-error (engines:search "x" :engine "searxng"))))))

(deftest web-provider-cell-hides-the-searxng-token ()
  (with-web (requests :pages (list (cons "searx.example/search" '("bad token sx-secret" 500)))
                      :config ("searxng_endpoint" "https://searx.example" "searxng_token" "sx-secret"))
    (let ((text (refusal-text engines:web-provider-error (engines:search "x" :engine "searxng"))))
      (is (search "SearXNG API error (500)" text))
      (is (null (search "sx-secret" text)) "the token never reaches the model"))))

;;; --- the public merge -------------------------------------------------------------------

(defun wp-all-engines ()
  (list (cons "POST https://www.startpage.com/sp/search" +wp-startpage+)
        (cons "https://www.startpage.com/" +wp-startpage-home+)
        (cons "www.google.com/search" +wp-google+)
        (cons "html.duckduckgo.com" +wp-duckduckgo+)
        (cons "www.ecosia.org/search" +wp-ecosia+)
        (cons "www.mojeek.de/search" +wp-mojeek+)))

(deftest web-provider-cell-merges-the-public-engines ()
  (with-web (requests :pages (wp-all-engines))
    (let* ((text (engines:search "example"))
           (urls (wp-urls text)))
      (is (equal "https://example.com/a" (first urls)) "named by all five, ranked first")
      (is (= 1 (count "https://example.com/a" urls :test #'equal)) "and listed once")
      (is (equal "https://www.example.net/c/" (second urls))
          "www. and a trailing slash are one page, named by two")
      (is (= 1 (count-if (lambda (url) (cl:search "example.net" url)) urls)))
      (is (search "the longest snippet" text) "the most informative snippet kept")
      (is (null (search "[" text)) "no engine failed, no note"))
    (is (= 6 (length requests)) "each engine asked once (Startpage twice: its form)")))

(deftest web-provider-cell-notes-a-failed-engine-and-fails-when-all-do ()
  (with-web (requests :pages (cons (cons "www.google.com" '("blocked" 429)) (wp-all-engines)))
    (let ((text (engines:search "example")))
      (is (search "[google: " text) "a failed engine is a note")
      (is (equal "https://example.com/a" (first (wp-urls text))) "the others still answer")))
  (with-web (requests :pages (list (cons "" '("down" 503))))
    (is (search "All public engines failed"
                (refusal-text engines:web-provider-error (engines:search "example"))))))

(deftest web-provider-cell-answers-without-a-slow-engine ()
  (let ((nodecode-web-provider::*soft-seconds* 0.3))
    (with-web (requests :pages (cons (cons "www.mojeek.de" (lambda (url) (declare (ignore url)) (sleep 3) +wp-mojeek+))
                                     (wp-all-engines)))
      (let* ((started (get-internal-real-time))
             (text (engines:search "example"))
             (seconds (/ (- (get-internal-real-time) started) internal-time-units-per-second)))
        (is (< seconds 2.5) "past the soft deadline with answers in hand, the merge answers")
        (is (search "[mojeek: no answer in time]" text))
        (is (null (search "m.example" text)) "and the late engine's results are not read")))))

(deftest web-provider-cell-refuses-what-it-cannot-ask ()
  (with-web (requests)
    (is (search "engine must be one of" (refusal-text engines:web-provider-error (engines:search "x" :engine "bing"))))
    (is (search "recency must be one of" (refusal-text engines:web-provider-error (engines:search "x" :recency "hour"))))
    (is (search "non-empty" (refusal-text engines:web-provider-error (engines:search ""))))
    (is (null requests) "nothing was asked")))
