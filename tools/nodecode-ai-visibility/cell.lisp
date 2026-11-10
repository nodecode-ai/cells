;;;; cell.lisp --- the verbs behind the tools, and the one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The manual: (help :ai-visibility) answers the primer below while the cell
;;;; runs. The vocabulary: AI-VISIBILITY:CHECK, :REPORT and :INFO from eval,
;;;; the three tools of the same names for a turn, and /ai-visibility for the
;;;; operator. All of them read the same server the same way.
;;;;
;;;; Config, a top-level key beside `websearch' and `osv':
;;;;   "ai-visibility": {"enabled": true, "endpoint": "https://qqq.ee/mcp",
;;;;                     "language": "en", "timeout_seconds": 20}
;;;; No key and no account: the checker is free and read-only, and the server
;;;; caps the crawler check at five a day for one calling address.

(in-package #:nodecode-ai-visibility)

;;; --- the manual -------------------------------------------------------------------

(defparameter +primer+
  "AI visibility of one website, through the qqq-ai-visibility cell: the tools
check_ai_visibility, get_public_report and get_tool_info, the Lisp verbs
(ai-visibility:check \"example.com\") from eval, and /ai-visibility at the prompt. All
answer the server's own text, links included.
  check_ai_visibility domain=\"example.com\"   the technical readiness of a site for AI search:
      whether AI crawlers (GPTBot, OAI-SearchBot, ClaudeBot, PerplexityBot, Google-Extended) may
      read it, and robots.txt, sitemap, title, meta description, one H1, canonical, noindex,
      Organization or Person structured data, content in HTML, key pages and llms.txt, with the
      three most important fixes. It asks no AI model.
  get_public_report domain=\"example.com\"     the site's latest public report: in how many
      search-grounded AI answers (ChatGPT, Gemini, Perplexity) the brand was named, how often its
      own site was cited, the questions asked, the sources the answers used and the other
      companies named, with the report's date. It fetches nothing and asks no model.
  get_tool_info                                what the three measure, and their limits.
  language=\"et\" asks for the Estonian reading; the default is \"en\".
Doctrine: mentions, citations and technical readiness are three separate results -- never add
them into one score, and never invent AI answers or numbers. Quote the text as it came, keeping
its links clickable, and do not call the same domain twice in one conversation: the crawler check
allows five a day per address and the answer says so when they are spent. A refusal is an answer
naming the domain, not something to retry in a loop. The checker reads public websites only."
  "What (help :ai-visibility) answers while the cell runs.")

(defparameter +usage+
  (format nil "usage: /ai-visibility <domain> for the AI crawler and readiness check, ~
/ai-visibility report <domain> for the latest public report of AI mentions and citations, ~
/ai-visibility info for what the checks measure.~%The tools check_ai_visibility, ~
get_public_report and get_tool_info do the same mid-turn, and so do ~
(ai-visibility:check \"example.com\"), (ai-visibility:report \"example.com\") and ~
(ai-visibility:info) from eval.")
  "What /ai-visibility answers with no argument.")

;;; --- the domain, as the server wants it ----------------------------------------------

(defun strip-prefix (text prefix)
  "TEXT without PREFIX, compared without case, when it starts with it."
  (if (and (>= (length text) (length prefix))
           (string-equal prefix (subseq text 0 (length prefix))))
      (subseq text (length prefix))
      text))

(defun strip-scheme (text)
  "TEXT without a leading http:// or https://."
  (if (uiop:string-prefix-p "https://" text)
      (subseq text 8)
      (if (uiop:string-prefix-p "http://" text) (subseq text 7) text)))

(defun strip-to-first (text characters)
  "TEXT cut where the first of CHARACTERS stands."
  (let ((cut (position-if (lambda (character) (member character characters)) text)))
    (if cut (subseq text 0 cut) text)))

(defun strip-port (text)
  "TEXT without a trailing :PORT, when what follows the colon is only digits."
  (let ((colon (position #\: text)))
    (if (and colon (plusp (- (length text) colon 1))
             (every #'digit-char-p (subseq text (1+ colon))))
        (subseq text 0 colon)
        text)))

(defun domain-label-p (label)
  "LABEL is one DNS label: letters, digits and inner dashes, at most 63 long."
  (and (plusp (length label))
       (<= (length label) 63)
       (alphanumericp (char label 0))
       (alphanumericp (char label (1- (length label))))
       (every (lambda (character) (or (alphanumericp character) (char= character #\-))) label)))

(defun ip-address-p (text)
  "TEXT is four dot-separated groups of digits and nothing else."
  (let ((parts (uiop:split-string text :separator '(#\.))))
    (and (= 4 (length parts))
         (every (lambda (part) (and (plusp (length part)) (every #'digit-char-p part))) parts))))

(defun clean-domain (value)
  "VALUE as the bare domain the server is asked about: no scheme, user name,
port, path, query or trailing dot, and a shape a domain can have. Refuses
anything else before a request is made, so nothing typed reaches the wire as
something other than a host name."
  (let* ((given (nlk:trimmed (or value "")))
         (text (string-downcase given))
         (text (strip-scheme text))
         (text (strip-to-first text '(#\/ #\? #\#)))
         (text (let ((at (position #\@ text :from-end t)))
                 (if at (subseq text (1+ at)) text)))
         (text (strip-port text))
         (text (string-right-trim "." (nlk:trimmed text)))
         (labels (uiop:split-string text :separator '(#\.))))
    (cond ((zerop (length text))
           (fail "a domain is required: /ai-visibility example.com, or ask for the check of a site"))
          ((string= text "localhost")
           (fail "~s is this machine; the checker reads public websites only" text))
          ((ip-address-p text)
           (fail "~s is an IP address; the checker wants a public domain such as example.com" text))
          ((> (length text) 253)
           (fail "~s is longer than a domain can be" text))
          ((< (length labels) 2)
           (fail "~s is not a domain: write it as example.com" text))
          ((not (every #'domain-label-p labels))
           (fail "~s is not a domain: write it as example.com, letters, digits and dashes"
                 text))
          ((let ((last (car (last labels))))
             (or (< (length last) 2) (not (every #'alpha-char-p last))))
           (fail "~s does not end in a top-level domain such as .com or .ee" text)))
    text))

(defun language-of (value)
  "VALUE as en or et; absent, the section's language."
  (let ((text (string-downcase (nlk:trimmed (or value "")))))
    (cond ((zerop (length text)) (setting :language))
          ((member text +languages+ :test #'string=) text)
          (t (fail "language ~s is not en or et" text)))))

;;; --- the verbs ----------------------------------------------------------------------

(define-verb check (domain &key language)
  "The AI visibility check of DOMAIN: whether AI crawlers may read the site,
its technical readiness for AI search, and the three most important fixes, as
the server writes it, links included."
  (call-tool "check_ai_visibility"
              (nlk:make-json-object "domain" (clean-domain domain)
                                     "language" (language-of language))))

(define-verb report (domain &key language)
  "DOMAIN's latest public report: in how many AI answers the brand was named,
how often the site was cited, the questions asked and the sources used, as
the server writes it. It reads the stored report and asks no model."
  (call-tool "get_public_report"
              (nlk:make-json-object "domain" (clean-domain domain)
                                     "language" (language-of language))))

(define-verb info (&key language)
  "What the checker measures, and its limits, as the server writes it."
  (call-tool "get_tool_info"
              (nlk:make-json-object "language" (language-of language))))

;;; --- what the model calls -------------------------------------------------------------

(nlk:define-contract check-call
  (:doc "Check a website's AI visibility: whether the AI crawlers (GPTBot, OAI-SearchBot, ClaudeBot, PerplexityBot, Google-Extended) may read it, and its technical readiness for AI search (robots.txt, sitemap, title, meta description, one H1, canonical, noindex, structured data, content in HTML, llms.txt), with the three most important fixes. Free, read-only, no key. Use it when someone asks whether a site is ready for AI search or for a GEO/AEO check of a domain.")
  ("domain" :text :required t
            :doc "the public domain or URL to check, for example example.com")
  ("language" :string :options '("en" "et")
              :doc "the reading's language: en (default) or et (Estonian)"))

(nlk:define-contract report-call
  (:doc "Read a domain's latest public report of AI answers: in how many search-grounded answers from ChatGPT, Gemini and Perplexity the brand was named, how often its own site was cited, the questions asked, the sources the answers used and the other companies named, with the report's date. It fetches nothing and asks no model, and it has no daily limit. Use it when someone asks whether AI assistants mention or cite a brand.")
  ("domain" :text :required t
            :doc "the public domain whose stored report to read, for example example.com")
  ("language" :string :options '("en" "et")
              :doc "the reading's language: en (default) or et (Estonian)"))

(nlk:define-contract info-call
  (:doc "What the AI visibility checker measures and what its limits are: the checks of the site's readiness for AI search, the mentions and citations of the public reports, and the daily cap on checks.")
  ("language" :string :options '("en" "et")
              :doc "the reading's language: en (default) or et (Estonian)"))

(defun check-tool (arguments)
  "The check_ai_visibility tool: the readiness check of one domain."
  (check (check-call-domain arguments) :language (check-call-language arguments)))

(defun report-tool (arguments)
  "The get_public_report tool: the latest public AI answer report of one domain."
  (report (report-call-domain arguments) :language (report-call-language arguments)))

(defun info-tool (arguments)
  "The get_tool_info tool: what the checker measures and its limits."
  (info :language (info-call-language arguments)))

;;; --- /ai-visibility ------------------------------------------------------------------------

(defun run-slash (args session-id)
  "One /ai-visibility invocation: a domain for the check, `report DOMAIN' for
the stored report, `info' for what is measured."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (head (string-downcase (if space (subseq text 0 space) text)))
         (rest (if space (nlk:trimmed (subseq text (1+ space))) ""))
         (named (plusp (length rest))))
    (cond ((zerop (length text)) +usage+)
          ((member head '("info" "about" "limits") :test #'string=) (info))
          ((string= head "report")
           (if named
               (report rest)
               (fail "which domain's report? /ai-visibility report example.com")))
          ((string= head "check")
           (if named
               (check rest)
               (fail "which domain? /ai-visibility check example.com")))
          (t (check text)))))

;;; --- the entry -----------------------------------------------------------------------------

(nle:define-cell ai-visibility
  (:section ("ai-visibility")
    (:guide "nothing here is required and there is no key: the hosted checker is free and read-only; language picks the reading's language; timeout_seconds bounds one call; endpoint is the server the cell talks to")
    ("endpoint" :string :default "https://qqq.ee/mcp"
                :doc "the tools/call endpoint; the hosted checker answers at https://qqq.ee/mcp")
    ("language" :choice :options '("en" "et") :default "en"
                :doc "the language of the readings: en, or et for an Estonian site")
    ("timeout_seconds" :integer :default 20 :min 1
                       :doc "how long one call may wait for the checker, in seconds"))
  (:help :ai-visibility "AI visibility: can AI crawlers read a site, and do AI answers mention it - the check_ai_visibility tool, or /ai-visibility example.com" +primer+)
  (:tool "check_ai_visibility" (nlk:contract 'check-call) 'check-tool)
  (:tool "get_public_report" (nlk:contract 'report-call) 'report-tool)
  (:tool "get_tool_info" (nlk:contract 'info-call) 'info-tool)
  (:command "ai-visibility" 'run-slash
            :description "AI visibility: crawler access, readiness for AI search, and the latest AI mention report"
            :argument-hint "DOMAIN | report DOMAIN | info"))
