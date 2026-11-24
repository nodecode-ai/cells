;;;; tools.lisp --- the eight tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin exposed eight tools from Distro's hosted MCP server. The
;;;; names, the argument names and the descriptions are the server's, read
;;;; from its own tools/list and unchanged here, so the model is shown the
;;;; same contract it was shown in Hermes; what changed is the transport, from
;;;; MCP-over-HTTP to one JSON-RPC POST per call (see NOTICE).
;;;;
;;;; Each tool is one NLK:DEFINE-CONTRACT, so the shape the model is shown
;;;; and the shape the body reads are one declaration, and a member the
;;;; declaration does not carry has no reader.
;;;;
;;;; Every tool answers text: the reader formats its own answers as markdown
;;;; (headline, byline, preview, canonical link), and the model reads them,
;;;  cites them and never has to parse JSON.

(in-package #:nodecode-distro-reader)

;;; --- distro_feed_latest -------------------------------------------------------

(nlk:define-contract feed-call
  (:doc "Browse the latest published stories on Distro, optionally one publication. Answers each
story's headline, byline, date, preview and canonical link. Use it for headlines and discovery;
use distro_story_read to read one story.")
  ("publication" :string
   :doc "publication slug to filter by, e.g. deai-news; not with publicationId")
  ("publicationId" :integer :slot publication-id
   :doc "numeric publication id to filter by; not with publication")
  ("limit" :integer
   :doc "how many stories (default 10, max 50)"))

(defun latest (&key publication publication-id limit)
  "distro_feed_latest: the latest published stories, one publication or all."
  (when (and (nonempty publication) publication-id)
    (fail "distro-reader: give publication or publicationId, not both"))
  (call-tool "distro_feed_latest"
             (arguments "publication" (nonempty publication)
                        "publicationId" publication-id
                        "limit" (or limit (setting :page-limit)))))

(defun tool-feed (call)
  "distro_feed_latest called the way the model calls it."
  (latest :publication (feed-call-publication call)
          :publication-id (feed-call-publication-id call)
          :limit (feed-call-limit call)))

;;; --- distro_search -------------------------------------------------------------

(nlk:define-contract search-call
  (:doc "Full-text search across every published Distro story, ranked by relevance, with the
canonical link of each match. Use it for a topic, a person, a phrase, or to resolve which
publication the user means.")
  ("query" :text :required t
   :doc "search query text")
  ("limit" :integer
   :doc "how many results (default 10, max 50)"))

(defun find-stories (query &key limit)
  "distro_search: stories matching QUERY, ranked by relevance."
  (call-tool "distro_search"
              (arguments "query" query
                         "limit" (or limit (setting :page-limit)))))

(defun tool-search (call)
  "distro_search called the way the model calls it."
  (find-stories (search-call-query call) :limit (search-call-limit call)))

;;; --- distro_story_read ---------------------------------------------------------

(nlk:define-contract story-call
  (:doc "Read one published story's full text. Identify it by exactly one of storyId, slug
(\"<publication-slug>/<story-slug>\") or url. A free story answers its body; a premium one
answers the payment instructions instead of the body, which is not a failure.")
  ("storyId" :integer :slot story-id
   :doc "numeric story id; give exactly one of storyId, slug or url")
  ("slug" :string
   :doc "slug pair \"<publication-slug>/<story-slug>\"")
  ("url" :string
   :doc "a full or relative Distro url")
  ("accessToken" :string :slot access-token
   :doc "an X-Distro-Access-Token from an x402 payment or a wallet recovery, when the user has one")
  ("inlineImages" :boolean :slot inline-images
   :doc "embed Distro-hosted images as data URIs (~50 KB each, ~200 KB a request)"))

(defun story (&key story-id slug url access-token inline-images)
  "distro_story_read: one story's text, or its premium-access instructions."
  (unless (or story-id (nonempty slug) (nonempty url))
    (fail "distro-reader: give exactly one of storyId, slug or url to read a story"))
  (call-tool "distro_story_read"
             (arguments "storyId" story-id
                        "slug" (nonempty slug)
                        "url" (nonempty url)
                        "accessToken" (nonempty access-token)
                        "inlineImages" (plain inline-images))))

(defun tool-story (call)
  "distro_story_read called the way the model calls it."
  (story :story-id (story-call-story-id call)
         :slug (story-call-slug call)
         :url (story-call-url call)
         :access-token (story-call-access-token call)
         :inline-images (story-call-inline-images call)))

;;; --- distro_media_series -------------------------------------------------------

(nlk:define-contract series-call
  (:doc "A podcast or video series published through Distro, with its canonical metadata and its
episodes. Use it for anything about a podcast, a video series, its episodes, guests or
transcripts.")
  ("publication" :string :required t
   :doc "publication slug, e.g. deai-news")
  ("series" :string :required t
   :doc "media-series slug, e.g. deai-journal"))

(defun series (publication &key series)
  "distro_media_series: one podcast or video series and its episodes."
  ;; SERIES is both the function and the argument's name; the body keeps the
  ;; wire's own word for it, so neither shadows the other.
  (let ((slug (or (nonempty series) (fail "distro-reader: name the series slug"))))
    (call-tool "distro_media_series"
               (arguments "publication" publication
                          "series" slug))))

(defun tool-series (call)
  "distro_media_series called the way the model calls it."
  (series (series-call-publication call) :series (series-call-series call)))

;;; --- distro_story_render_hud ---------------------------------------------------

(nlk:define-contract story-render-call
  (:doc "Render a story as Distro's own reading page, the website-faithful article style. With
htmlDelivery \"link\" (the default here) it answers the page and a renderUrl to open; \"inline\"
returns the whole HTML, which is large. Identify the story by exactly one of storyId, slug or
url.")
  ("storyId" :integer :slot story-id
   :doc "numeric story id; give exactly one of storyId, slug or url")
  ("slug" :string :doc "slug pair \"<publication-slug>/<story-slug>\"")
  ("url" :string :doc "a full or relative Distro url")
  ("accessToken" :string :slot access-token
   :doc "an X-Distro-Access-Token from an x402 payment or a wallet recovery")
  ("htmlDelivery" :string :slot html-delivery :options '("link" "inline")
   :doc "how the page is delivered: link (default here) or inline")
  ("selfContained" :boolean :slot self-contained
   :doc "inline the mermaid runtime and the images, for a sandbox with no network")
  ("includeSponsor" :boolean :slot include-sponsor :default t
   :doc "include the publication's sponsor block (default true)"))

(defun render-story (&key story-id slug url access-token html-delivery self-contained include-sponsor)
  "distro_story_render_hud: a story as the page Distro itself renders."
  (unless (or story-id (nonempty slug) (nonempty url))
    (fail "distro-reader: give exactly one of storyId, slug or url to render a story"))
  (clip (call-tool "distro_story_render_hud"
                    (arguments "storyId" story-id
                               "slug" (nonempty slug)
                               "url" (nonempty url)
                               "accessToken" (nonempty access-token)
                               "htmlDelivery" (or (nonempty html-delivery) (setting :html-delivery))
                               "selfContained" (plain self-contained)
                               "includeSponsor" (plain include-sponsor)))
        (setting :max-chars)))

(defun tool-render-story (call)
  "distro_story_render_hud called the way the model calls it."
  (render-story :story-id (story-render-call-story-id call)
                :slug (story-render-call-slug call)
                :url (story-render-call-url call)
                :access-token (story-render-call-access-token call)
                :html-delivery (story-render-call-html-delivery call)
                :self-contained (story-render-call-self-contained call)
                :include-sponsor (story-render-call-include-sponsor call)))

;;; --- distro_book_render_hud ----------------------------------------------------

(nlk:define-contract book-render-call
  (:doc "A public Distro book: cover, metadata, ordered table of contents and every available
free chapter, as the page Distro renders. Name the book by publication slug, publicationId or
newslode url. Compare a chapter's access with the whole-book offer before any purchase.")
  ("publication" :string :doc "book publication slug")
  ("publicationId" :integer :slot publication-id :doc "numeric book publication id")
  ("url" :string :doc "a full or relative Distro newslode url")
  ("accessToken" :string :slot access-token
   :doc "a whole-book X-Distro-Access-Token; a valid one renders every chapter")
  ("htmlDelivery" :string :slot html-delivery :options '("link" "inline")
   :doc "how the page is delivered: link (default here) or inline")
  ("selfContained" :boolean :slot self-contained
   :doc "inline the mermaid runtime, for a sandbox with no network"))

(defun book (&key publication publication-id url access-token html-delivery self-contained)
  "distro_book_render_hud: a book's cover, contents and free chapters."
  (unless (or (nonempty publication) publication-id (nonempty url))
    (fail "distro-reader: give publication, publicationId or url to open a book"))
  (clip (call-tool "distro_book_render_hud"
                    (arguments "publication" (nonempty publication)
                               "publicationId" publication-id
                               "url" (nonempty url)
                               "accessToken" (nonempty access-token)
                               "htmlDelivery" (or (nonempty html-delivery) (setting :html-delivery))
                               "selfContained" (plain self-contained)))
        (setting :max-chars)))

(defun tool-book (call)
  "distro_book_render_hud called the way the model calls it."
  (book :publication (book-render-call-publication call)
        :publication-id (book-render-call-publication-id call)
        :url (book-render-call-url call)
        :access-token (book-render-call-access-token call)
        :html-delivery (book-render-call-html-delivery call)
        :self-contained (book-render-call-self-contained call)))

;;; --- distro_payment_info -------------------------------------------------------

(nlk:define-contract payment-call
  (:doc "What a premium item costs and how it can be paid: price, accepted rails (x402, Visa CLI),
the network and the resource. Read-only -- it authorizes no charge. Call it before telling the
user anything about paying.")
  ("contentId" :integer :slot content-id :required t
   :doc "the story or news id to price")
  ("clientType" :string :slot client-type
   :options '("mcp_agent" "visa_cli_agent" "bot_verified" "bot_unverified" "human" "anonymous")
   :doc "which pricing tier is asked for; mcp_agent by default"))

(defun payment-info (content-id &key client-type)
  "distro_payment_info: what one premium item costs, without paying."
  (call-tool "distro_payment_info"
              (arguments "contentId" content-id
                         "clientType" (or (nonempty client-type) "mcp_agent"))))

(defun tool-payment (call)
  "distro_payment_info called the way the model calls it."
  (payment-info (payment-call-content-id call)
                :client-type (payment-call-client-type call)))

;;; --- distro_tip ----------------------------------------------------------------

(nlk:define-contract tip-call
  (:doc "A one-time tip to the publisher of a story or a publication, paid in USDC on Base over
x402. Call it with no amount to get the suggested amounts; call it again with the exact amount
only after the user approved that amount. A tip buys no content access.")
  ("storyId" :integer :slot story-id
   :doc "the story to tip on; the tip goes to its publisher")
  ("newslodeSlug" :string :slot newslode-slug
   :doc "a publication slug, when not tipping from a story")
  ("amount" :number
   :doc "USD amount, 0.5 to 500, up to two decimals; omit for the menu"))

(defun tip (&key story-id newslode-slug amount)
  "distro_tip: the tip menu, or the x402 handoff for one approved amount."
  (unless (or story-id (nonempty newslode-slug))
    (fail "distro-reader: name the story (storyId) or the publication (newslodeSlug) to tip"))
  (call-tool "distro_tip"
             (arguments "storyId" story-id
                        "newslodeSlug" (nonempty newslode-slug)
                        "amount" amount)))

(defun tool-tip (call)
  "distro_tip called the way the model calls it."
  (tip :story-id (tip-call-story-id call)
       :newslode-slug (tip-call-newslode-slug call)
       :amount (tip-call-amount call)))

;;; --- what the server itself offers ---------------------------------------------

(defun first-line (text)
  "TEXT's first line, cut to 140 characters."
  (when text
    (let ((line (first (uiop:split-string text :separator '(#\Newline)))))
      (if (> (length line) 140)
          (concatenate 'string (subseq line 0 140) "...")
          line))))

(defun server-tools ()
  "The reader's own tools/list: what the endpoint advertises right now, one
line each. Answers drift in the server's contract without a cell change."
  (let ((tools (nlk:json-value (rpc "tools/list") :array "tools")))
    (if (or (null tools) (zerop (length tools)))
        "(the reader listed no tools)"
        (with-output-to-string (out)
          (format out "distro-reader: ~d tools~%" (length tools))
          (loop for tool across tools
                for name = (or (nlk:json-value tool :text "name") "(unnamed)")
                do (format out "  ~a~@[ - ~a~]~%"
                           name
                           (first-line (nlk:json-value tool :text "description"))))))))
