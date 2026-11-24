;;;; contracts.lisp --- the three tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp and tools.lisp, whose bodies
;;;; read the records.

(in-package #:nodecode-browserless)

(defparameter +formats+ '("text" "html")
  "What browserless_fetch answers: the page's words, or its HTML.")

(nlk:define-contract page-fetch
  (:doc "Render one page in a Browserless cloud browser and answer what is on it. The browser runs on Browserless, not on this machine: scripts run, the page loads as a visitor sees it, and stealth, the residential proxy and captcha solving are what make a site that blocks datacenter traffic load. Use it for a page a plain fetch cannot render, or one that blocks this machine. format text answers the page's words (the cheapest to read); html answers the rendered HTML when the structure matters. A page longer than the operator's content_chars is cut, and the answer says so.")
  ("url" :text :required t
   :doc "the page to render, scheme included (https://example.com/pricing)")
  ("format" :string :slot page-format :default "text" :options +formats+
   :doc "text (the page's words) or html (the rendered HTML); text unless the structure matters")
  ("wait_for_selector" :string :default ""
   :doc "a CSS selector to wait for before reading the page, for content a script adds late (#results, main article)")
  ("wait_ms" :integer
   :doc "extra milliseconds to wait after load, for a page that fills itself in on a timer; over the operator's timeout the render is cut")
  ("timeout_seconds" :integer
   :doc "how long this page may take, over the operator's timeout_seconds"))

(nlk:define-contract page-shot
  (:doc "Screenshot one page in a Browserless cloud browser and answer where the PNG was saved. Look at it with (look PATH) and answer from what you see. Use it to see a layout, a chart or a captcha; for words, browserless_fetch is cheaper.")
  ("url" :text :required t
   :doc "the page to screenshot, scheme included")
  ("full_page" :boolean :default nil
   :doc "the whole page, scrolled from top to bottom, rather than the visible viewport")
  ("width" :integer :doc "the viewport width in pixels; the browser's own when unset (a page laid out for a desktop is 1280)")
  ("height" :integer :doc "the viewport height in pixels; the browser's own when unset (a viewport is 800)")
  ("wait_for_selector" :string :default ""
   :doc "a CSS selector to wait for before the shot, for content a script adds late"))

(nlk:define-contract page-pdf
  (:doc "Print one page in a Browserless cloud browser to a PDF and answer where the file was saved. Use it for a document worth keeping: an invoice, a receipt, a report, a page a person will read offline.")
  ("url" :text :required t
   :doc "the page to print, scheme included")
  ("landscape" :boolean :default nil
   :doc "a landscape page instead of portrait, for a wide table")
  ("wait_for_selector" :string :default ""
   :doc "a CSS selector to wait for before printing, for content a script adds late"))
