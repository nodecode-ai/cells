;;;; cell.lisp --- the primer, the section, the /jp-corporate command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was two tools over the gBizINFO API; this cell is the same two
;;;; tools (tools.lisp) plus the operator's own way in: /jp-corporate NAME
;;;; runs one search from the keyboard, and /jp-corporate alone says what the
;;;; cell is connected to. The model learns the tools from their schemas and
;;;; the primer below, which (help :jp-corporate) answers and whose one line
;;;; rides every request. Nothing hooks the provider path: no model is served.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "jp-corporate": {"api_key": "...", "api_key_env": "GBIZINFO_API_TOKEN",
;;;;                    "endpoint": "https://api.info.gbiz.go.jp/hojin/v2/hojin",
;;;;                    "timeout_seconds": 20}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-jp-corporate)

(defparameter +primer+
  "Japanese corporate lookup is available through the nodecode-jp-corporate cell: two tools over
gBizINFO, the corporate information service of Japan's Ministry of Economy, Trade and Industry.
  jp_corp_search   find a corporation by name, or by its 13-digit 法人番号 (an invoice number such
                   as T1180301018771 also works), and check an invoice against the registry.
                   name, corporate_number, address, limit. Answers name, head-office address,
                   status, the gBizINFO page url, and -- when you give a number and a name or
                   address -- whether they match the official record.
  jp_corp_profile  one corporation's record, section by section (each section is one request):
                   basic, subsidy, procurement, commendation, certification, finance, patent,
                   workplace, offices. corporate_number is required; sections defaults to basic.
How to work: find the number first with jp_corp_search, then ask jp_corp_profile only for the
sections the question needs. 'registered' means the registration is not closed, not that the
business is operating; 0 results is not proof that a company does not exist; the registry holds
corporations, not sole proprietors. Representative, capital and headcount figures can be years
old, so quote gbizinfo_last_updated with them. The data is free but the API's terms ask for
restraint: one lookup at a time, not a loop over a list of companies.
Every answer carries the attribution line gBizINFO's terms ask for: show it when you show the data.
ERROR: JP-CORPORATE-ERROR names the problem (a malformed number, a refused token, a rate limit),
and says what to do next. The operator runs one search with /jp-corporate NAME, and /jp-corporate
alone says what the cell is connected to."
  "What (help :jp-corporate) answers while the cell runs.")

(defun status-text ()
  "What this cell is connected to, without ever printing the token."
  (format nil "jp-corporate: ~a, token ~a; tools jp_corp_search and jp_corp_profile (gBizINFO, METI Japan).~%
A free token is applied for once at ~a; set it as \"api_key\" in the jp-corporate section or export ~a.~%
Search from here with /jp-corporate NAME or /jp-corporate 13-DIGIT-NUMBER."
          (endpoint)
          (if (api-token) "set" "MISSING")
          +apply-url+
          +token-variable+))

(defun run-command (args session-id)
  "One /jp-corporate invocation: alone, what the cell is connected to; with a
name or a 13-digit number, one search."
  (declare (ignore session-id))
  (let ((text (string-trim '(#\Space #\Tab) (or args ""))))
    (cond
      ((zerop (length text)) (status-text))
      (t
       (let* ((digits (remove-if (lambda (character) (member character +number-junk+)) text))
              (numberp (and (plusp (length digits))
                            (every #'digit-char-p (subseq digits (if (member (char digits 0) '(#\T #\t)) 1 0))))))
         (redacted
          (render (if numberp
                      (lookup-number text "" "")
                      (search-object text "" 5)))))))))

(defun start ()
  "On start, refuse a section that could not reach the API."
  (check-config))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell jp-corporate
  (:section ("jp-corporate")
    (:guide "apply once for the free gBizINFO token (申請区分「トークン利用(WebAPI・データダウンロード)」), then paste it in api_key or export GBIZINFO_API_TOKEN (or name the variable in api_key_env); endpoint only for a mirror of the API; next step: /jp-corporate alone says whether the token is set")
    ("api_key" :secret
     :doc "the free gBizINFO REST API token")
    ("api_key_env" :env
     :doc "the environment variable holding the token, when it is not written here (GBIZINFO_API_TOKEN is read either way)")
    ("endpoint" :string
     :doc "the API base: https://api.info.gbiz.go.jp/hojin/v2/hojin unless you point at a mirror")
    ("timeout_seconds" :integer :default 20 :min 5
     :doc "one request's ceiling"))
  (:help :jp-corporate
         "jp-corporate: look a Japanese corporation up in gBizINFO by name or 法人番号, and read its registry, subsidy, contract, award, certification, finance, patent and workplace record"
         +primer+)
  (:command "jp-corporate" 'run-command
            :description "Japanese corporate lookup: one search, or the connection"
            :argument-hint "NAME or 13-DIGIT-NUMBER"
            :session nil)
  (:tool "jp_corp_search" (nlk:contract 'corp-search-call) #'corp-search)
  (:tool "jp_corp_profile" (nlk:contract 'corp-profile-call) #'corp-profile)
  (:start #'start))
