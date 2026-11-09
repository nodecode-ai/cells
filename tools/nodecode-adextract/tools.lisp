;;;; tools.lisp --- the five tools, what each sends, and what it answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin published thirteen MCP tools: four ad searches, three
;;;; advertiser searches, two advertiser lookups, three single-ad lookups and
;;;; ad_reader. Each tool answers the library it names, with the same envelope
;;;; and the same credit price, so this cell groups them by what a caller
;;;; wants -- find ads, find an advertiser, look one advertiser up, look one ad
;;;; up, read a creative -- and routes on the platform the call names. The
;;;; server's own tool names and arguments are what ``wire.lisp'' sends; a
;;;; member this cell does not name goes through ``extra'', because the
;;;; server serves its own schema at runtime and the plugin pins no copy of it.
;;;;
;;;; Every tool answers text: the model reads it, and the urls, ids and
;;;; tokens stay where it can cite and use them.

(in-package #:nodecode-adextract)

(defparameter +platforms+ '("meta" "google" "linkedin" "tiktok")
  "The four ad libraries the server searches.")

(defun bolt (&rest pairs)
  "A JSON object from PAIRS (KEY VALUE ...), leaving out every NIL VALUE: a
request carries only what the call asked for."
  (loop for (key value) on pairs by #'cddr
        when value append (list key value) into kept
        finally (return (apply #'nlk:make-json-object kept))))

(defun merge-extra (object extra)
  "OBJECT with EXTRA's members set on it, EXTRA a decoded JSON object or NIL."
  (when (hash-table-p extra)
    (maphash (lambda (key value)
               (unless (eq value :null) (setf (gethash key object) value)))
             extra))
  object)

(defun array-p (value)
  "Whether VALUE is a JSON array (a vector that is not a string)."
  (and (vectorp value) (not (stringp value))))

(defun objects (value)
  "VALUE — a JSON array — as a list of its objects, dropping anything else."
  (and (array-p value) (remove-if-not #'hash-table-p (coerce value 'list))))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (format nil "~a ... (~d more characters)"
                   (subseq text 0 limit) (- (length text) limit)))))

(defun scalar-text (value limit)
  "VALUE — a JSON scalar — as one line."
  (cond ((stringp value) (clip value limit))
        ((null value) "false")
        ((eq :null value) "null")
        ((eq t value) "true")
        ((integerp value) (format nil "~d" value))
        ((numberp value) (format nil "~f" value))
        (t (format nil "~a" value))))

(defun nested-p (value)
  "Whether VALUE is a JSON object or a non-empty array: it gets its own lines."
  (or (hash-table-p value) (and (array-p value) (plusp (length value)))))

(defun format-value (value stream indent limit)
  "VALUE — any decoded JSON — laid out as indented text."
  (let ((pad (make-string indent :initial-element #\Space)))
    (cond
      ((hash-table-p value)
       (let ((members '()))
         (maphash (lambda (key entry) (push (cons key entry) members)) value)
         (if (null members)
             (format stream "~a{}~%" pad)
             (dolist (member (nreverse members))
               (format stream "~a~a: " pad (car member))
               (if (nested-p (cdr member))
                   (progn (terpri stream) (format-value (cdr member) stream (+ indent 2) limit))
                   (format stream "~a~%" (scalar-text (cdr member) limit)))))))
      ((array-p value)
       (if (zerop (length value))
           (format stream "~a(none)~%" pad)
           (loop for entry across value
                 for index from 1
                 do (format stream "~a~d. " pad index)
                    (if (nested-p entry)
                        (progn (terpri stream) (format-value entry stream (+ indent 3) limit))
                        (format stream "~a~%" (scalar-text entry limit))))))
      (t (format stream "~a~a~%" pad (scalar-text value limit))))))

(defun format-item (item stream index limit)
  "One ad of a search answer."
  (format stream "~d. ~a~@[ - ~a~]~%"
          index
          (or (nlk:json-value item :text "advertiser") "(unknown advertiser)")
          (nlk:json-value item :text "headline"))
  (let ((body (nlk:json-value item :text "body")))
    (when body (format stream "   ~a~%" (clip body limit))))
  (let* ((platforms (nlk:json-value item :array "platforms"))
         (names (and (array-p platforms) (remove-if-not #'stringp (coerce platforms 'list))))
         (state (multiple-value-bind (value present)
                    (nlk:json-value item :any "is_active")
                  (and present (if value "active" "not active"))))
         (id (nlk:json-value item :text "id")))
    (format stream "   ~@[~{~a~^, ~}~]~@[ - ~a~]~@[ - id ~a~]~%" names state id))
  (let ((media (objects (nlk:json-value item :array "media"))))
    (dolist (entry (subseq media 0 (min 2 (length media))))
      (format stream "   ~a: ~a~%"
              (or (nlk:json-value entry :text "type") "media")
              (or (nlk:json-value entry :text "url") "")))))

(defun format-search (object)
  "OBJECT — a search answer's envelope — as the text the model reads."
  (with-output-to-string (stream)
    (let* ((items (objects (nlk:json-value object :array "items")))
           (total (nlk:json-value object :integer "search_information" "total_results"))
           (engine (or (nlk:json-value object :text "engine") "search"))
           (limit (setting :max-chars))
           (shown (subseq items 0 (min (length items) (setting :max-items)))))
      (format stream "~a: ~d ad~:p in this answer~@[, ~d reported in total~]~%"
              engine (length items) total)
      (if (null items)
          (format stream "no ads matched~%")
          (loop for item in shown
                for index from 1
                do (format-item item stream index limit)))
      (when (> (length items) (length shown))
        (format stream "~%and ~d more, left out to keep this answer readable~%"
                (- (length items) (length shown))))
      (let ((token (nlk:json-value object :text "pagination" "next_page_token")))
        (cond (token (format stream "~%next page: ~a (pass it as next_page_token)~%" token))
              (items (format stream "~%no next page~%")))))))

(defun result-text (result)
  "RESULT — an MCP tool result — as its text content blocks joined."
  (let ((content (nlk:json-value result :array "content")))
    (and (array-p content)
         (format nil "~{~a~^~%~}"
                 (loop for block across content
                       when (and (hash-table-p block)
                                 (equal "text" (nlk:json-value block :text "type")))
                         collect (or (nlk:json-value block :text "text") ""))))))

(defun result-object (result)
  "RESULT's structured content, else its text when that text is one JSON object."
  (or (nlk:json-value result :object "structuredContent")
      (let ((text (result-text result)))
        (let ((value (and text
                          (plusp (length (nlk:trimmed text)))
                          (handler-case (nlk:decode-json text) (error () nil)))))
          (and (hash-table-p value) value)))))

(defun answer (result)
  "RESULT — an MCP tool result — as what the model reads: a search envelope
laid out as ads, any other JSON as indented members, plain text as itself."
  (let ((object (result-object result))
        (text (result-text result)))
    (concatenate 'string
                 (if (nlk:json-value result :boolean "isError") "ERROR: " "")
                 (cond
                   ((and object (array-p (nlk:json-value object :array "items")))
                    (format-search object))
                   (object (with-output-to-string (stream)
                             (format-value object stream 0 (setting :max-chars))))
                   (text (clip text (* 8 (setting :max-chars))))
                   (t "the adextract server answered nothing")))))

(defun platform-of (value tool allowed)
  "VALUE — the call's platform — lowercased and checked against ALLOWED."
  (let ((platform (string-downcase (or value ""))))
    (unless (member platform allowed :test #'equal)
      (fail "~a takes platform ~{~a~^, ~}; the call said ~s"
            tool allowed (or value "nothing")))
    platform))

;;; --- adextract_search ---------------------------------------------------------

(nlk:define-contract search-call
  (:doc "Search one public ad library: meta (Facebook and Instagram), google (the Ads Transparency
Center), linkedin, or tiktok. Answers the ads it found -- advertiser, headline, body, platforms,
active, media urls -- and a next_page_token for the page after it. One credit per successful
call. The server also names its arguments by library, so an argument this tool does not list goes
in extra.")
  ("platform" :string :required t :options '("meta" "google" "linkedin" "tiktok")
   :doc "which library: meta, google, linkedin or tiktok")
  ("q" :text :doc "what to search for: the brand, product or category phrase")
  ("advertiser" :string
   :doc "the brand, page or website the ads belong to; for google a domain such as nike.com, since the Transparency Center searches by advertiser or domain")
  ("country" :string
   :doc "the market, as an ISO code: US, GB, DE. Google calls it region")
  ("active_only" :boolean :doc "only ads running right now (meta)")
  ("media_type" :string :options '("image" "video" "meme" "all")
   :doc "keep one creative format (meta)")
  ("next_page_token" :string :doc "the token the last answer of the same search ended with")
  ("extra" :object
   :doc "any other argument the library's own tool takes, by its own name: sort_by, platforms, region, ad_format, time_period, limit, and so on"))

(defun search-tool (platform)
  "The server's search tool for PLATFORM."
  (cond ((string= platform "meta") "search_meta_ads")
        ((string= platform "google") "search_google_ads")
        ((string= platform "linkedin") "search_linkedin_ads")
        (t "search_tiktok_ads")))

(defun search-arguments (call platform)
  "CALL's members as the server's tool for PLATFORM spells them."
  (let ((q (nonempty (search-call-q call)))
        (advertiser (nonempty (search-call-advertiser call)))
        (country (nonempty (search-call-country call)))
        (token (nonempty (search-call-next-page-token call)))
        (media (nonempty (search-call-media-type call))))
    (cond
      ((string= platform "google")
       (unless advertiser
         (fail "adextract_search on google: the Transparency Center searches by advertiser or domain, so name one in advertiser (nike.com, or an advertiser id from adextract_advertisers)"))
       (bolt "domain" (and (find #\. advertiser) advertiser)
             "advertiser_id" (unless (find #\. advertiser) advertiser)
             "region" country "next_page_token" token))
      ((string= platform "meta")
       (unless (or q advertiser)
         (fail "adextract_search on meta: name what to look for in q, or the page in advertiser"))
       (bolt "q" (or q advertiser) "country" country "media_type" media
             "active_status" (and (search-call-active-only call) "active")
             "next_page_token" token))
      (t
       (unless (or q advertiser)
         (fail "adextract_search on ~a: name the brand, product or category in q" platform))
       (bolt "q" (or q advertiser) "country" country "next_page_token" token)))))

(defun tool-search (call)
  "adextract_search: the library's own search tool."
  (let* ((platform (platform-of (search-call-platform call) "adextract_search" +platforms+))
         (arguments (merge-extra (search-arguments call platform) (search-call-extra call))))
    (answer (call-tool (search-tool platform) arguments))))

;;; --- adextract_advertisers ----------------------------------------------------

(nlk:define-contract advertisers-call
  (:doc "Find an advertiser before searching its ads: answers the pages, advertisers or domains
the library knows by that name, with the ids a search takes. One credit per call.")
  ("platform" :string :required t :options '("meta" "google" "tiktok")
   :doc "which library: meta, google or tiktok (linkedin has no advertiser search)")
  ("q" :text :required t :doc "the brand, company or website to find")
  ("extra" :object :doc "any other argument the library's advertiser tool takes, by its own name"))

(defun tool-advertisers (call)
  "adextract_advertisers: the library's advertiser search."
  (let* ((platform (platform-of (advertisers-call-platform call) "adextract_advertisers"
                                '("meta" "google" "tiktok")))
         (tool (cond ((string= platform "meta") "search_meta_pages")
                     ((string= platform "google") "search_google_ads_advertisers")
                     (t "search_tiktok_advertisers")))
         (arguments (merge-extra (bolt "q" (nonempty (advertisers-call-q call)))
                                  (advertisers-call-extra call))))
    (answer (call-tool tool arguments))))

;;; --- adextract_advertiser_info ------------------------------------------------

(nlk:define-contract advertiser-info-call
  (:doc "One advertiser's own record: followers, verification, transparency and spend on meta, or
an advertiser's details from Google's own tokens. One credit per call.")
  ("platform" :string :required t :options '("meta" "google")
   :doc "which library: meta or google")
  ("advertiser" :text :required t
   :doc "the page id (meta) or the advertiser id or token (google) an earlier answer carried")
  ("extra" :object :doc "any other argument the library's own tool takes, by its own name"))

(defun tool-advertiser-info (call)
  "adextract_advertiser_info: get_meta_page_info or get_google_ads_advertiser_info."
  (let* ((platform (platform-of (advertiser-info-call-platform call) "adextract_advertiser_info"
                                '("meta" "google")))
         (advertiser (nonempty (advertiser-info-call-advertiser call)))
         (arguments
           (merge-extra
            (if (string= platform "meta")
                (bolt "page_id" advertiser "q" advertiser)
                (bolt "advertiser_id" advertiser "q" advertiser))
            (advertiser-info-call-extra call))))
    (answer (call-tool (if (string= platform "meta")
                            "get_meta_page_info"
                            "get_google_ads_advertiser_info")
                        arguments))))

;;; --- adextract_ad -------------------------------------------------------------

(nlk:define-contract ad-call
  (:doc "One ad's full record: an EU/UK transparency record on meta, a creative's variations on
google, targeting and audience size on tiktok. Take the id from a search answer. One credit per
call.")
  ("platform" :string :required t :options '("meta" "google" "tiktok")
   :doc "which library the ad is from")
  ("ad" :text :required t
   :doc "the ad's id or token, as a search answer carried it: ad_details_token or ad_archive_id on meta, creative_id on google, ad_id on tiktok")
  ("advertiser" :string :doc "google only: the advertiser_id the creative belongs to")
  ("extra" :object :doc "any other argument the library's own tool takes, by its own name"))

(defun tool-ad (call)
  "adextract_ad: the library's single-ad tool."
  (let* ((platform (platform-of (ad-call-platform call) "adextract_ad" '("meta" "google" "tiktok")))
         (ad (nonempty (ad-call-ad call)))
         (advertiser (nonempty (ad-call-advertiser call)))
         (arguments
           (merge-extra
            (cond
              ((string= platform "meta")
               (if (every #'digit-char-p ad)
                   (bolt "ad_archive_id" ad)
                   (bolt "ad_details_token" ad)))
              ((string= platform "google")
               (unless advertiser
                 (fail "adextract_ad on google: a creative is read with its advertiser, so name advertiser_id (or the domain) in advertiser"))
               (bolt "advertiser_id" advertiser "creative_id" ad))
              (t (bolt "ad_id" ad)))
            (ad-call-extra call))))
    (answer (call-tool (cond ((string= platform "meta") "get_meta_ad_details")
                             ((string= platform "google") "get_google_ad_details")
                             (t "get_tiktok_ad_details"))
                        arguments))))

;;; --- adextract_read_creative --------------------------------------------------

(nlk:define-contract read-creative-call
  (:doc "Read one ad creative from its media url: format, audience, emotion and archetypes, as
structured analysis. Take the url from a search answer's media, or pass any public image or video
url. One credit per call.")
  ("url" :text :required t :doc "an image or video url, absolute and publicly reachable")
  ("extra" :object :doc "any other argument ad_reader takes, by its own name"))

(defun tool-read-creative (call)
  "adextract_read_creative: ad_reader."
  (answer (call-tool "ad_reader"
                      (merge-extra (bolt "url" (nonempty (read-creative-call-url call)))
                                   (read-creative-call-extra call)))))
