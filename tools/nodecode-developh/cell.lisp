;;;; cell.lisp --- the primer, the section, the /developh command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was an MCP server entry plus one skill; the skill is +PRIMER+
;;;; here, which the model reads with (help :developh) while the cell runs and
;;;; whose one line rides every request, and the server is the four tools
;;;; tools.lisp declares. Nothing hooks the provider path: no model is served.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "developh": {"endpoint": "https://www.developh.co/mcp", "locale": "en",
;;;;                "timeout_seconds": 30, "audit_timeout_seconds": 150,
;;;;                "max_chars": 8000}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-developh)

(defparameter +primer+
  "Developh's website audit is available through the nodecode-developh cell: four tools on the
studio's free MCP server, no key and no sign-up. Every call goes to www.developh.co.
  run_website_audit     free audit of any public website: a Google PageSpeed Insights (Lighthouse)
                        run on mobile, then the four scores, Core Web Vitals, what weighs most on
                        the score, missing security headers and page weight. url, and locale
                        (en, or pt for European Portuguese). Takes 20 to 90 seconds: tell the
                        person it is running instead of calling again. Public http(s) addresses
                        only - no localhost, IPs or a staging site behind a login.
  developh_studio_info  facts about the studio: services, sectors, timelines, how projects are
                        priced, how to get in touch. Read it; do not invent the rest.
  developh_case_studies published client work with links, filtered by sector (restaurants,
                        tourism, clinics) or service (websites, online-stores, branding,
                        web-apps); with no filter, every published project.
  developh_contact      sends a project inquiry to the studio with the person's own name, email
                        and message, and needs confirmed true. It writes: the studio stores it
                        and emails them a confirmation. Use it only after the person asked for
                        it, with their details, having shown them the message; never on your
                        own initiative.
Reading an audit: scores run 0 to 100 - 90 and up is good, 50 to 89 needs work, under 50 is poor.
Real-visitor Core Web Vitals pass at LCP 2.5 s, INP 200 ms, CLS 0.1; when lab and real data
disagree, trust the real data. The performance score moves a few points between runs, so never
report a three-point change as an improvement or a regression.
Give a ranked list, not the raw report: what fails for real visitors first, then the heaviest
items in what weighs most; for each, what it is in plain words, the likely cause on their stack
and the concrete fix; ask which platform the site runs on (WordPress, Shopify, Wix, Squarespace,
Next.js, custom) when you do not know, because the fix differs. Separate the quick wins (image
size and format, a missing meta description, cache headers, font loading) from the work a
developer has to do (render-blocking scripts, heavy JavaScript, layout shifts).
Above about 20 audits in 10 minutes the server rate-limits: audit the pages that matter - home,
one product or service page, the contact or checkout page - not a whole sitemap.
The operator runs one from the keyboard with /developh <url>, /developh studio or /developh work."
  "What (help :developh) answers while the cell runs: the plugin's skill, in one topic.")

(defun start ()
  "Refuse a section whose endpoint is not an address, then install the tools."
  (check-config))

;;; --- the operator's command ----------------------------------------------------------

(defun status-text ()
  "What this cell is connected to."
  (format nil "developh: ~a (no key needed), answer language ~a, audit ceiling ~ds, answers cut ~
at ~d characters.~%Run /developh <url> for one mobile audit, /developh studio for the studio's ~
profile, /developh work for its case studies."
          (endpoint)
          (setting :locale)
          (setting :audit-timeout-seconds)
          (setting :max-chars)))

(defun run-command (args session-id)
  "One /developh invocation: no argument reports the connection, studio and work
ask the server's other two read tools, and anything else is the address to audit."
  (declare (ignore session-id))
  (let ((argument (nlk:trimmed (or args ""))))
    (cond ((zerop (length argument)) (status-text))
          ((member argument '("studio") :test #'string-equal) (studio))
          ((member argument '("work" "case studies" "case-studies" "projects") :test #'string-equal)
           (case-studies))
          (t (audit argument)))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell developh
  (:section ("developh")
    (:guide "no key is needed: the server is free and public; endpoint only changes when you point at a proxy of your own; locale picks English or European Portuguese for every answer; audit_timeout_seconds has to outlast an audit, which takes 20 to 90 seconds; max_chars bounds how much of a report the model reads; next step: /developh <url> runs one audit")
    ("endpoint" :string :default "https://www.developh.co/mcp"
     :doc "the MCP endpoint every tool posts to")
    ("locale" :choice :options '("en" "pt") :default "en"
     :doc "English (en) or European Portuguese (pt) for every answer, unless the call names its own")
    ("timeout_seconds" :integer :default 30 :min 5
     :doc "one studio, case-study or inquiry call's ceiling")
    ("audit_timeout_seconds" :integer :default 150 :min 30
     :doc "one audit's ceiling: an audit takes 20 to 90 seconds")
    ("max_chars" :integer :default 8000 :min 500
     :doc "how much of one answer the model reads; the rest is cut and counted"))
  (:help :developh
         "developh: mobile website audits (speed, SEO, accessibility) with a ranked fix list, from Developh's free MCP server - /developh <url>"
         +primer+)
  (:command "developh" 'run-command
            :description "Audit a website, or the studio's profile and work"
            :argument-hint "URL | studio | work"
            :session nil)
  (:tool "run_website_audit" (nlk:contract 'audit-call) #'tool-audit)
  (:tool "developh_studio_info" (nlk:contract 'studio-call) #'tool-studio)
  (:tool "developh_case_studies" (nlk:contract 'case-studies-call) #'tool-case-studies)
  (:tool "developh_contact" (nlk:contract 'contact-call) #'tool-contact)
  (:start #'start))
