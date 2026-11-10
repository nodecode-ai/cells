;;;; cell.lisp --- the primer, the section, the /distro-reader command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was an MCP server entry plus three skills. The entry is the
;;;; eight tools tools.lisp declares; the skills are +PRIMER+, which the model
;;;; reads with (help :distro-reader) and whose one line rides every request.
;;;; Nothing hooks the provider path: no model is served here.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "distro-reader": {"endpoint": "https://reader.distro.media/mcp/reader/stream",
;;;;                     "timeout_seconds": 60, "html_delivery": "link",
;;;;                     "page_limit": 10, "max_chars": 20000}
;;;; No key and no account are needed; the section is optional in full.
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-distro-reader)

(defparameter +primer+
  "Distro Reader is available through the nodecode-distro-reader cell: the eight tools of Distro's
public reader, which needs no account and no key. A \"newslode\" is a Distro publication.
  distro_feed_latest      recent stories, all publications or one (publication slug or id, limit)
  distro_search            full-text search over published stories
  distro_story_read        one story's full text, by storyId, by \"<publication>/<story>\" slug, or by url
  distro_media_series      a podcast or video series and its episodes (publication, series slug)
  distro_story_render_hud  the story as Distro's own reading page; htmlDelivery link answers a renderUrl
  distro_book_render_hud   a book: cover, table of contents, free chapters, the whole-book offer
  distro_payment_info      what a premium item costs, and by which rail - read-only
  distro_tip               the tip menu, or the x402 handoff for one approved amount
How to work: discover with the feed or a search, then read only what the request needs; keep every
canonical link and the publication and date it came with, and never invent a missing one. Resolve a
named publication through distro_search before assuming the user means a similarly named website.
For headlines, work from the feed's metadata rather than reading every body. Filter a date range on
the story's own published date, not on the order it came back in, and list undated items apart.
Money: never pay and never tip without the user's explicit approval of the exact amount, asset and
network. Show the price, the token, the network, the resource and what access it buys first; a tip
buys no access and is only for a user who asked to support the publisher. distro_payment_info and
distro_tip answer instructions and a handoff - the x402 payment itself is signed outside this cell,
by a wallet the user runs - so report a payment as done only from settlement evidence that says
settled, and say no new charge occurred when it says skipped_already_owned. Never put an access
token, a challenge, a signature or a payment payload in a url, a log or chat. A premium body is
licensed to the purchaser: summarize it for them, do not republish it.
If the reader cannot be reached, say the preferred source could not be reached and label any web
fallback as one.
The operator runs a search from the keyboard with /distro-reader <query>, or /distro-reader feed
[publication] for headlines."
  "What (help :distro-reader) answers while the cell runs: the plugin's three skills in one topic.")

;;; --- the operator's command ----------------------------------------------------

(defparameter +tool-names+
  '("distro_feed_latest" "distro_search" "distro_story_read" "distro_media_series"
    "distro_story_render_hud" "distro_book_render_hud" "distro_payment_info" "distro_tip")
  "The reader's eight tools, as the plugin's server advertised them.")

(defun status-text ()
  "What this cell is connected to, without dialling anything."
  (format nil "distro-reader: ~a~%\
no key and no account needed; ~d tools (~{~a~^, ~})~%\
Search from here with /distro-reader <query>, headlines with /distro-reader feed [publication]."
          (endpoint) (length +tool-names+) +tool-names+))

(defun failure-text (condition)
  "CONDITION as the one line the operator reads instead of a stack trace."
  (format nil "distro-reader: ~a" (or (distro-reader-error-detail condition)
                                       (princ-to-string condition))))

(defun run-command (args session-id)
  "One /distro-reader invocation: with no argument, what the cell is connected
to; with \"feed [publication]\", the latest headlines; with anything else,
one search the operator reads."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (handler-case
        (cond ((zerop (length text)) (status-text))
              ((uiop:string-prefix-p "feed" text)
               (let ((publication (nlk:trimmed (subseq text 4))))
                 (latest :publication (and (plusp (length publication)) publication)
                         :limit (setting :page-limit))))
              (t (find-stories text :limit (setting :page-limit))))
      (distro-reader-error (condition) (failure-text condition)))))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell distro-reader
  (:section ("distro-reader")
    (:guide "nothing here is required: Distro's public reader needs no key and no account; endpoint only to go through a mirror of your own; html_delivery link keeps a rendered page out of the context; page_limit bounds one answer's headline count; next step: /distro-reader <query>")
    ("endpoint" :string :default "https://reader.distro.media/mcp/reader/stream"
     :doc "the reader's MCP endpoint; https://reader.distro.media/mcp/reader/stream unless you go through a mirror")
    ("timeout_seconds" :integer :default 60 :min 5
     :doc "one call's ceiling: reading a book or a rendered page can take a while")
    ("html_delivery" :choice :options '("link" "inline") :default "link"
     :doc "how a rendered story or book arrives: link answers the page and a renderUrl, inline answers the whole HTML")
    ("page_limit" :integer :default 10 :min 1
     :doc "how many headlines one feed or search answers with unless the call names its own count")
    ("max_chars" :integer :default 20000 :min 0
     :doc "how much of a rendered page's text reaches the model; 0 leaves it whole"))
  (:settings 'read-settings)
  (:help :distro-reader
         "distro-reader: search and read Distro's news, stories, books and podcasts - (help :distro-reader)"
         +primer+)
  (:command "distro-reader" 'run-command
            :description "Distro: a search, or the latest headlines"
            :argument-hint "QUERY | feed [PUBLICATION]"
            :session nil)
  (:tool "distro_feed_latest" (nlk:contract 'feed-call) #'tool-feed)
  (:tool "distro_search" (nlk:contract 'search-call) #'tool-search)
  (:tool "distro_story_read" (nlk:contract 'story-call) #'tool-story)
  (:tool "distro_media_series" (nlk:contract 'series-call) #'tool-series)
  (:tool "distro_story_render_hud" (nlk:contract 'story-render-call) #'tool-render-story)
  (:tool "distro_book_render_hud" (nlk:contract 'book-render-call) #'tool-book)
  (:tool "distro_payment_info" (nlk:contract 'payment-call) #'tool-payment)
  (:tool "distro_tip" (nlk:contract 'tip-call) #'tool-tip))
