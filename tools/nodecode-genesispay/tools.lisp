;;;; tools.lisp --- the four tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin's four tools, under the names the plugin's skill tells the
;;;; model to call (see NOTICE). Each is one NLK:DEFINE-CONTRACT, so the
;;;; shape the model is shown and the shape the body sends are one
;;;; declaration, and each body is one call-tool: the answer is the
;;;; catalog's own text, cut at the section's ceiling.
;;;;
;;;; The catalog is public and read-only: no tool buys, orders, reserves or
;;;; changes anything, and nothing here sends a credential.

(in-package #:nodecode-genesispay)

;;; --- what a call sends ------------------------------------------------------------

(defun arguments (&rest pairs)
  "PAIRS, (WIRE-NAME VALUE)..., as the object a tools/call carries: only the
values that are present, under the server's own key names."
  (let ((object (nlk:json-object)))
    (loop for (name value) on pairs by #'cddr
          when value do (setf (gethash name object) value))
    object))

(defun trimmed-or-nil (text)
  "TEXT trimmed, or NIL when it is empty or not a string."
  (and (stringp text) (let ((text (nlk:trimmed text)))
                         (and (plusp (length text)) text))))

(defun clip (text)
  "TEXT, cut at the section's ceiling with one line saying so: the catalog's
own text is what the model reads, and a whole page of it is not a paragraph."
  (let ((cap (setting :max-result-chars)))
    (if (> (length text) cap)
        (format nil "~a~%... one answer of ~d characters, cut at ~d: narrow the query or the limit, ~
or raise max_result_chars in the genesispay section~%"
                (subseq text 0 cap) (length text) cap)
        text)))

;;; --- genesispay_catalog_search ----------------------------------------------------

(nlk:define-contract search-call
  (:doc "Search the products of the stores in the GenesisPay catalog, or the
stores themselves. Read-only: nothing here buys or reserves. Ask with words
first; the filters narrow a search that is too broad.")
  ("query" :string
   :doc "words matched against product titles, descriptions and the store's host, e.g. 'wool runner' or 'allbirds'; a size such as 120 ml or 3.4 oz selects a variant instead of being matched as words, and with capability set it is matched against store hosts and names")
  ("capability" :string :options '("checkout" "discount")
   :doc "answer stores, not products, whose published profile declares this: 'checkout' (an agent can check out) or 'discount' (it carries discount codes)")
  ("storeHost" :string :slot store-host
   :doc "only the products of this store, by the host a store result names (allbirds.com); the bare host and its www. form match each other")
  ("ucpOnly" :boolean :slot ucp-only
   :doc "true: only stores whose catalog endpoint was observed; omit for every indexed store")
  ("currency" :string
   :doc "only products priced in this ISO 4217 currency, e.g. USD or EUR; required with the two price bounds and the price sorts, and prices are compared inside one currency only")
  ("minPriceMinor" :integer :slot min-price-minor
   :doc "only products whose lowest price is at least this, in minor units of currency: 5000 is 50.00. Needs currency")
  ("maxPriceMinor" :integer :slot max-price-minor
   :doc "only products whose lowest price is at most this, in minor units of currency. Needs currency")
  ("availableOnly" :boolean :slot available-only
   :doc "true: only products the store says are available; an unknown availability is then left out")
  ("sort" :string :slot sort-by :options '("relevance" "price_asc" "price_desc" "newest")
   :doc "relevance (the default); price_asc or price_desc (lowest listed price; needs currency); newest (first seen by the index, not the store's launch date)")
  ("kind" :string :options '("physical" "digital")
   :doc "a legacy filter the catalog still accepts: physical is shipped goods, digital is everything else; it does not apply to store products")
  ("shop" :string
   :doc "an existing shop id (shop_…) to list only its listings; the shops tool answers stores, not these ids")
  ("category" :string
   :doc "one category name, matched against a store product's whole categories, not a substring of one")
  ("limit" :integer
   :doc "how many results, 1 to 50; the catalog defaults to 10")
  ("cursor" :string
   :doc "the nextCursor of the previous page, unchanged, with the same query, filters and sort"))

(defun tool-search (call)
  "genesispay_catalog_search: the products, or with capability set the stores."
  (let ((text (trimmed-or-nil (search-call-query call))))
    (unless (or text
                (search-call-capability call)
                (search-call-store-host call)
                (search-call-shop call)
                (search-call-category call))
      (fail "genesispay_catalog_search: give words to search for (query), a storeHost, a shop, a category, ~
or a capability to list stores by; nothing was sent"))
    (clip (call-tool "genesispay_catalog_search"
                      (arguments "query" text
                                 "capability" (search-call-capability call)
                                 "storeHost" (search-call-store-host call)
                                 "ucpOnly" (and (search-call-ucp-only call) t)
                                 "currency" (search-call-currency call)
                                 "minPriceMinor" (search-call-min-price-minor call)
                                 "maxPriceMinor" (search-call-max-price-minor call)
                                 "availableOnly" (and (search-call-available-only call) t)
                                 "sort" (search-call-sort-by call)
                                 "kind" (search-call-kind call)
                                 "shop" (search-call-shop call)
                                 "category" (search-call-category call)
                                 "limit" (search-call-limit call)
                                 "cursor" (search-call-cursor call))))))

;;; --- genesispay_catalog_shops -----------------------------------------------------

(nlk:define-contract shops-call
  (:doc "List the stores the GenesisPay catalog indexes: their host, their own
address, how many products the index holds for them, and what the index
observed of their catalog endpoint. Read-only.")
  ("query" :string
   :doc "keywords matched against store hosts and names, e.g. 'shoes'")
  ("ucpOnly" :boolean :slot ucp-only
   :doc "true: only stores whose catalog endpoint was observed; omit for every indexed store")
  ("limit" :integer
   :doc "how many stores, 1 to 50; the catalog defaults to 10")
  ("cursor" :string
   :doc "the nextCursor of the previous page, unchanged"))

(defun tool-shops (call)
  "genesispay_catalog_shops: the stores, with what the index observed of each."
  (clip (call-tool "genesispay_catalog_shops"
                    (arguments "query" (trimmed-or-nil (shops-call-query call))
                               "ucpOnly" (and (shops-call-ucp-only call) t)
                               "limit" (shops-call-limit call)
                               "cursor" (shops-call-cursor call)))))

;;; --- genesispay_catalog_trending ---------------------------------------------------

(nlk:define-contract recent-call
  (:doc "What the catalog saw most recently: the store products first seen in
the last seven days, newest first, at most three per store. Read-only. What
is new is not what is popular, and neither is a rating.")
  ("limit" :integer
   :doc "how many products, 1 to 20; the catalog defaults to 10"))

(defun tool-trending (call)
  "genesispay_catalog_trending: the catalog's newest products."
  (let ((sent (arguments "limit" (recent-call-limit call))))
    (clip (handler-case (call-tool "genesispay_catalog_recent" sent)
            (genesispay-error (condition)
              ;; The endpoint this plugin was written against named this tool
              ;; genesispay_catalog_trending; the catalog now names it
              ;; genesispay_catalog_recent. Whichever name is missing, the
              ;; other is tried once before the failure reaches the model.
              (if (search "unknown" (princ-to-string condition) :test #'char-equal)
                  (call-tool "genesispay_catalog_trending" sent)
                  (error condition)))))))

;;; --- genesispay_catalog_reviews ----------------------------------------------------

(nlk:define-contract reviews-call
  (:doc "Read the published reviews and the rating of one catalog result: an
affiliate product (prod_…) or a service (ext_…). Read-only. Every review
comes from a verified purchase, which proves a purchase, not that the opinion
is right; a review count is not a sales count.")
  ("id" :text :required t
   :doc "the id of one result, exactly as search or trending answered it: prod_… or ext_…")
  ("limit" :integer
   :doc "how many reviews, 1 to 10; the catalog defaults to 5")
  ("cursor" :string
   :doc "the nextCursor of the previous page, unchanged"))

(defun tool-reviews (call)
  "genesispay_catalog_reviews: the reviews and rating of one result."
  (let ((id (trimmed-or-nil (reviews-call-id call))))
    (unless id
      (fail "genesispay_catalog_reviews: give the id of one result from search or trending (prod_… or ext_…); ~
nothing was sent"))
    (clip (call-tool "genesispay_catalog_reviews"
                      (arguments "id" id
                                 "limit" (reviews-call-limit call)
                                 "cursor" (reviews-call-cursor call))))))
