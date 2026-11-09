;;;; cell.lisp --- the manual, the section, the three tools, /browserless, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model finds the tools in its own catalog, listed with the schema
;;;; each contract in contracts.lisp projects; (help :browserless) carries
;;;; the rules for using them, and every request carries the one line the
;;;; :HELP clause gives. Nothing here starts a thread: Browserless is the
;;;; operator's account, and this cell only talks to the endpoint it names.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "browserless": {"token_env": "BROWSERLESS_TOKEN",
;;;;                   "base_url": "https://production-sfo.browserless.io"}
;;;; A vetoed section ("enabled": false) registers no tool at all.

(in-package #:nodecode-browserless)

(defparameter +primer+
  "Browserless renders pages in a cloud browser of your account: one request, one page, no Chrome on
this machine. The tools are browserless_fetch, browserless_screenshot and browserless_pdf.

  browserless_fetch url          the page as its words, or as its rendered html (format). The
                                 cheapest way to read what a script-heavy or blocking page shows.
  browserless_screenshot url     a PNG of the page, answered as the path to look at with (look PATH).
  browserless_pdf url            the page printed to a PDF, answered as the path.

Rules. Reach for browserless_fetch when a plain fetch answers an empty shell, a login wall of a
script, or a block: the page is rendered by a real browser with stealth on, and the operator's
residential proxy and captcha settings are what get past a site that blocks datacenter traffic.
Ask for format html only when the structure matters; the words are cheaper to read. A page cut at
browserless.content_chars says so in its answer, with the count: narrow what you ask of it rather
than repeating the call. Look at a screenshot or a PDF path with (look PATH) before answering
about it. A local address (localhost, a LAN host) is not reachable from Browserless: use a fetch
tool for this machine. An error is ERROR: BROWSERLESS-ERROR with the reason: a rejected token
names the setting, an HTTP 429 is the plan's concurrency limit, and anything else is Browserless'
own words."
  "What (help :browserless) answers while the cell runs.")

(defun start ()
  "Nothing to hold open: a start is the tools registered and the section read."
  (nle:on-stop (lambda () nil)))

(nle:define-cell browserless
  (:section ("browserless")
    (:guide "get an API token from your Browserless account (browserless.io/account) and name it in token_env (BROWSERLESS_TOKEN) or token_file; base_url is the region endpoint your account answers on; stealth and proxy=residential are what make a blocked page load; - /browserless shows what the cell will do")
    (:one-of "token_env" "token_file")
    ("token_env" :env
     :doc "the environment variable holding the Browserless API token (BROWSERLESS_TOKEN)")
    ("token_file" :path
     :doc "a file holding the token, which the file's own permissions guard")
    ("base_url" :string :default "https://production-sfo.browserless.io"
     :doc "your Browserless region endpoint, https:// included and no trailing slash, e.g. https://production-lon.browserless.io")
    ("stealth" :boolean :default t
     :doc "the stealth browser: it hides navigator.webdriver and the HeadlessChrome user agent")
    ("proxy" :string :default ""
     :doc "residential to render through a residential IP, which uses more plan units; empty for Browserless' own IPs")
    ("proxy_country" :string :default ""
     :doc "the residential proxy's exit country, two letters (us, gb, de); only with proxy residential")
    ("proxy_sticky" :boolean :default t
     :doc "keep one proxy IP for a whole render instead of rotating per request")
    ("solve_captchas" :boolean :default nil
     :doc "let Browserless solve the captchas it detects; it works best with the residential proxy")
    ("block_ads" :boolean :default nil
     :doc "block ads and trackers, which also makes a page load faster")
    ("timeout_seconds" :integer :default 30 :min 5
     :doc "how long one page may take to load, and one request may take")
    ("content_chars" :integer :default 20000 :min 500
     :doc "the longest page text browserless_fetch answers; the rest is cut with a count"))
  (:settings #'read-settings)
  (:tool "browserless_fetch" (nlk:contract 'page-fetch) #'run-fetch)
  (:tool "browserless_screenshot" (nlk:contract 'page-shot) #'run-screenshot)
  (:tool "browserless_pdf" (nlk:contract 'page-pdf) #'run-pdf)
  (:help :browserless
         "browserless: browserless_fetch, browserless_screenshot and browserless_pdf render pages in a Browserless cloud browser"
         +primer+)
  (:command "browserless" 'run-command
            :description "What Browserless will do, or one page as its words"
            :argument-hint "URL")
  (:start #'start))
