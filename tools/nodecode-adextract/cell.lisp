;;;; cell.lisp --- the primer, the section, the /adextract command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was one hosted MCP server plus five skills. The server is the
;;;; five tools tools.lisp calls over MCP; the skills are +PRIMER+, which the
;;;; model reads with (help :adextract) and whose one line rides every
;;;; request. Nothing hooks the provider path: no model is served here.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "adextract": {"api_key": "...", "api_key_env": "ADEXTRACT_API_KEY",
;;;;                 "endpoint": "https://mcp.adextract.co/mcp",
;;;;                 "timeout_seconds": 60, "max_items": 10, "max_chars": 400}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-adextract)

(defparameter +primer+
  "Ad research is available through the nodecode-adextract cell: five tools over adextract's
hosted server, which searches the public ad libraries of Meta, Google, LinkedIn and TikTok and
reads ad creatives. The account is the key's, and every successful call spends one credit of it.
  adextract_search          find ads in one library. platform (meta, google, linkedin, tiktok),
                             q (the brand, product or category), advertiser (the brand or, for
                             google, the domain), country (US, GB, DE), next_page_token,
                             active_only, media_type. Answers advertiser, headline, body,
                             platforms, active, media urls, id, and a next_page_token.
  adextract_advertisers      find the advertiser first: the pages, advertisers or domains the
                             library knows by name, with the ids a search takes. meta, google
                             or tiktok.
  adextract_advertiser_info  one advertiser's own record: followers, verification, spend on
                             meta; google's advertiser details from its tokens.
  adextract_ad              one ad whole: EU/UK transparency on meta, creative variations on
                             google, targeting and audience size on tiktok. Take the id from a
                             search answer.
  adextract_read_creative    read one image or video url: format, audience, emotion, archetypes.
How to work: search first, analyze second. Run one search per platform when the question spans
libraries, then adextract_read_creative on the two or three best creatives before saying anything
about strategy. Synthesize format, emotion and funnel role across what you found, cite the
advertiser and the ad id, and name the filters you used. Google searches by advertiser or domain,
not by phrase: resolve the brand with adextract_advertisers first. TikTok signed media urls
expire: use the thumbnail when one is all that is left. A search with no results proves no
absence, only that this library had none for that filter. Every call's price is one credit; on a
refused key or an empty balance, stop and tell the operator instead of retrying.
When a call is refused for an argument, repeat it with that argument inside extra, which passes
any member of the library's own tool through unchanged. The operator runs one search from the
keyboard with /adextract PLATFORM QUERY."
  "What (help :adextract) answers while the cell runs: the plugin's five skills in one topic.")

(defparameter +platforms-text+ "meta, google, linkedin, tiktok")

(defun status-text ()
  "What this cell is connected to, without ever printing the key."
  (format nil "adextract: ~a, key ~a; ~d tools (~{~a~^, ~}); one credit per successful call~%\
Search from here with /adextract PLATFORM QUERY (~a)."
          (endpoint)
          (if (api-key) "set" "MISSING")
          (count-if (lambda (entry) (eql 0 (search "adextract_" (first entry)))) nle::*tools*)
          '("adextract_search" "adextract_advertisers" "adextract_advertiser_info"
            "adextract_ad" "adextract_read_creative")
          +platforms-text+))

(defun run-command (args session-id)
  "One /adextract invocation: with no argument, what the cell is connected to;
with a platform and a query, that library's search."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (head (string-downcase (if space (subseq text 0 space) text)))
         (rest (if space (nlk:trimmed (subseq text (1+ space))) "")))
    (cond
      ((zerop (length text)) (status-text))
      ((and (member head +platforms+ :test #'equal) (plusp (length rest)))
       (tool-search (decode-search-call
                     (nlk:make-json-object "platform" head "q" rest))))
      (t (format nil "usage: /adextract ~a QUERY~%       /adextract, alone, says what the cell is connected to"
                 +platforms-text+)))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell adextract
  (:section ("adextract")
    (:guide "make a key at app.adextract.co (account, then API keys) and paste it in api_key, or export ADEXTRACT_API_KEY (or name the variable in api_key_env); endpoint only for a server of your own; max_items and max_chars bound one answer's size; next step: /adextract shows the connection")
    ("api_key" :secret
     :doc "the adextract API key")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (ADEXTRACT_API_KEY is read either way)")
    ("endpoint" :string
     :doc "the MCP endpoint: https://mcp.adextract.co/mcp unless you run a server of your own")
    ("timeout_seconds" :integer :default 60 :min 5
     :doc "one tool call's ceiling, handshake and search together")
    ("max_items" :integer :default 10 :min 1
     :doc "how many ads of one answer are laid out before the rest are counted only")
    ("max_chars" :integer :default 400 :min 40
     :doc "how much of one ad's body or one field's text an answer carries"))
  (:help :adextract
         "adextract: competitor ad research across Meta, Google, LinkedIn and TikTok, and creative analysis - (help :adextract)"
         +primer+)
  (:command "adextract" 'run-command
            :description "Ad research: one library's search, or the connection"
            :argument-hint "PLATFORM QUERY"
            :session nil)
  (:tool "adextract_search" (nlk:contract 'search-call) #'tool-search)
  (:tool "adextract_advertisers" (nlk:contract 'advertisers-call) #'tool-advertisers)
  (:tool "adextract_advertiser_info" (nlk:contract 'advertiser-info-call) #'tool-advertiser-info)
  (:tool "adextract_ad" (nlk:contract 'ad-call) #'tool-ad)
  (:tool "adextract_read_creative" (nlk:contract 'read-creative-call) #'tool-read-creative)
  (:start (lambda ()
            (check-config))))
