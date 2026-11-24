;;;; cell.lisp --- the manual, the section, the tool, the /crawl command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model finds the tool in its own catalog, with the schema the
;;;; declaration below projects; (help :crawl4ai) carries the rules for using
;;;; it, and every request carries the one line the :HELP clause gives. The
;;;; operator gets /crawl for one page without asking the model.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "crawl4ai": {"url": "http://localhost:11235", "api_token": "", "timeout_seconds": 60}
;;;; A vetoed section ("enabled": false) registers nothing at all.

(in-package #:nodecode-crawl4ai)

(defparameter +primer+
  "Crawl4AI turns a page into clean markdown for you: a server of the operator's own renders the
page in its own browser (so a JavaScript-heavy site extracts without a second tool) and answers
the text and the page's title.

  crawl4ai_extract urls   one or more page addresses; each answer is the page's first heading,
                           its URL and its markdown, or one ERROR line for that page alone.

Use it when you want a page's whole readable text: documentation, an article, a changelog, a page
the plain fetch tool reduced to noise. Give it the http or https address the reader would use; a
private, loopback or link-local address is refused before it is sent, because the server renders
whatever it is handed. Prefer a search tool to find pages and this to read one. A very long page
is long: read what the answer carries rather than calling again for the same address. An error is
ERROR: with the reason — a server that is not running, an HTTP status, or a page this server could
not render; fix that, do not repeat the call."
  "What (help :crawl4ai) answers while the cell runs.")

(nlk:define-contract extract-arguments
  (:doc "Extract pages as clean markdown through the operator's self-hosted Crawl4AI server. Give the http or https address of each page; the answer carries the page title, its URL and its whole markdown, or one ERROR line for a page the server could not render. A private or loopback address is refused before the request goes out.")
  ("urls" :array :of :string :required t
   :doc "the pages to extract, e.g. [\"https://example.com/docs\"]"))

(defun run-extract (arguments)
  "The crawl4ai_extract tool: every URL asked, rendered as one answer."
  (extract (coerce (extract-arguments-urls arguments) 'list)))

(defun run-crawl (args session-id)
  "One /crawl invocation => the answer's text, and typed alone the dialog
every command answers then: a row that asks for the page to extract."
  (declare (ignore session-id))
  (let ((url (nlk:trimmed (or args ""))))
    (if (plusp (length url))
        (extract (list url))
        (let ((text "Crawl4AI: give a page address, e.g. /crawl https://example.com"))
          (values text
                  (nle:list-dialog
                   :title "Crawl4AI" :summary text
                   :rows (list (nle:ask-row "Extract a page" "Page address" "/crawl"
                                             :placeholder "https://example.com"
                                             :verb "extract"))))))))

(nle:define-cell crawl4ai
  (:section ("crawl4ai")
    (:guide "run the Crawl4AI server yourself (docker run -d -p 11235:11235 unclecode/crawl4ai), then put its address in url; api_token is the JWT it prints at startup, when it was started with auth on; - /cells shows whether the folder started")
    ("url" :string :required t
     :doc "the Crawl4AI server's address, e.g. http://localhost:11235; this cell never installs or starts it")
    ("api_token" :secret
     :doc "the JWT the server logs at startup, sent as the bearer token; leave it empty when the server runs with auth off")
    ("timeout_seconds" :integer :default 60 :min 5
     :doc "how long one page may take: a JavaScript-heavy page renders in the server's browser first"))
  (:settings
   (lambda (values table)
     (declare (ignore table))
     (let ((url (string-right-trim "/" (or (getf values :url) ""))))
       (when (plusp (length url))
         (unless (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
           (nlk:config-error "crawl4ai.url is ~s; it is the server's web address, http:// or https://"
                             url)))
       (setf (getf values :url) url)
       values)))
  (:tool "crawl4ai_extract" (nlk:contract 'extract-arguments) 'run-extract)
  (:help :crawl4ai "crawl4ai: crawl4ai_extract turns pages into clean markdown through the operator's own Crawl4AI server"
         +primer+)
  (:command "crawl" 'run-crawl
            :description "Extract a page as markdown through your Crawl4AI server"
            :argument-hint "URL"))

