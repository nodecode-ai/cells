;;;; tools.lisp --- the four tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin exposed four tools from Developh's hosted MCP server; the
;;;; names, the arguments and the service are Developh's, unchanged, and the
;;;; transport is the part that changed, from an MCP client the harness had to
;;;; run to one JSON-RPC POST per call (see NOTICE). Three of the four are
;;;; namespaced with the studio's name here because every cell shares one tool
;;;; registry; the audit keeps the name the plugin's skill teaches.
;;;;
;;;; Each tool is one NLK:DEFINE-CONTRACT, so the shape the model is shown and
;;;; the shape the body reads are one declaration, and a member the declaration
;;;; does not carry has no reader. Every tool answers text: the server renders
;;;; the report, and the model reads it.

(in-package #:nodecode-developh)

(defun locale-of (given)
  "The language an answer comes in: the call's own, else the section's."
  (or (nonempty given) (setting :locale) "en"))

(defun host-of (address)
  "ADDRESS's host, lowercased, without the scheme, the path or a port."
  (let* ((lowered (string-downcase address))
         (after-scheme (or (let ((at (search "://" lowered)))
                             (and at (subseq lowered (+ at 3))))
                           lowered))
         (end (or (position-if (lambda (character) (find character "/?#: ")) after-scheme)
                  (length after-scheme))))
    (subseq after-scheme 0 end)))

(defun private-host-p (host)
  "Whether HOST is one only this machine or its network can reach, which a
mobile audit of a public site cannot use."
  (or (string= host "localhost")
      (uiop:string-prefix-p "127." host)
      (string= host "0.0.0.0")
      (uiop:string-prefix-p "10." host)
      (uiop:string-prefix-p "192.168." host)
      (uiop:string-prefix-p "169.254." host)
      (uiop:string-suffix-p ".local" host)
      (uiop:string-suffix-p ".localhost" host)))

;;; --- run_website_audit -----------------------------------------------------------

(nlk:define-contract audit-call
  (:doc "Free audit of a public website, for questions like \"why is my website slow?\" or \"is
my SEO any good?\": Google PageSpeed Insights on mobile, then performance, accessibility, best
practices and SEO scores, Core Web Vitals, the issues that weigh most on the score, missing
security headers and page weight. Takes 20 to 90 seconds — say it is running instead of
retrying. Only public http(s) addresses.")
  ("url" :text :required t
   :doc "public website address, e.g. example.pt or https://www.example.com/page")
  ("locale" :string :options '("en" "pt")
   :doc "answer language: English (en) or European Portuguese (pt); the section's when the call names none"))

(defun audit (url &optional locale)
  "One mobile audit of URL, as the report the model reads."
  (let ((address (nlk:trimmed (or url ""))))
    (when (zerop (length address))
      (fail "run_website_audit: url is empty; name a public http(s) address, e.g. example.com"))
    (when (private-host-p (host-of address))
      (fail "run_website_audit: ~a is a local address; the audit only reaches public http(s) ~
addresses, so name the live domain" address))
    (clip (developh-call "run_website_audit"
                          (bolt "url" address "locale" (locale-of locale))
                          :timeout (setting :audit-timeout-seconds))
          (setting :max-chars))))

(defun tool-audit (call)
  "run_website_audit: the server's PageSpeed Insights audit."
  (audit (audit-call-url call) (audit-call-locale call)))

;;; --- developh_studio_info ----------------------------------------------------------

(nlk:define-contract studio-call
  (:doc "Facts about Developh, a design and development studio in Lisbon, Portugal: address,
team, services, sectors with case studies, timelines, how projects are priced and how to get in
touch. Answer from it when someone asks who they are or what they do; do not embellish it.")
  ("locale" :string :options '("en" "pt")
   :doc "answer language: English (en) or European Portuguese (pt); the section's when the call names none"))

(defun studio (&optional locale)
  "The studio's profile, as the report the model reads."
  (clip (developh-call "studio_info" (bolt "locale" (locale-of locale))
                        :timeout (setting :timeout-seconds))
        (setting :max-chars)))

(defun tool-studio (call)
  "developh_studio_info: the server's studio profile."
  (studio (studio-call-locale call)))

;;; --- developh_case_studies ---------------------------------------------------------

(nlk:define-contract case-studies-call
  (:doc "Published client work by Developh with links to each case study, filtered by sector or
service; with no filter, every published project. Use it when someone asks for examples of the
studio's work, or to point at the closest one to their own project.")
  ("sector" :string :options '("restaurants" "tourism" "clinics")
   :doc "sector to keep: restaurants, tourism or clinics")
  ("service" :string :options '("websites" "online-stores" "branding" "web-apps")
   :doc "service to keep: websites, online-stores, branding or web-apps")
  ("locale" :string :options '("en" "pt")
   :doc "answer language: English (en) or European Portuguese (pt); the section's when the call names none"))

(defun case-studies (&key sector service locale)
  "The studio's published work, as the report the model reads."
  (clip (developh-call "list_case_studies"
                        (bolt "sector" (nonempty sector)
                              "service" (nonempty service)
                              "locale" (locale-of locale))
                        :timeout (setting :timeout-seconds))
        (setting :max-chars)))

(defun tool-case-studies (call)
  "developh_case_studies: the server's list of published projects."
  (case-studies :sector (case-studies-call-sector call)
                :service (case-studies-call-service call)
                :locale (case-studies-call-locale call)))

;;; --- developh_contact ---------------------------------------------------------------

(nlk:define-contract contact-call
  (:doc "Send a project inquiry or a question to the studio, with the person's own name, email
and message. This one writes: the studio stores the inquiry and emails the person a
confirmation, and usually answers within 2 to 8 business hours. Call it only after the person
asked you to, with the details they gave, having shown them the message; then pass confirmed
true. Never send one on your own initiative.")
  ("name" :text :required t :doc "the person's name, as they gave it (2 to 100 characters)")
  ("email" :text :required t
   :doc "the person's own email address: the confirmation and the reply go there")
  ("message" :text :required t
   :doc "what they need, in their words: the project, goals, timeline and budget if known (10 to 4000 characters)")
  ("company" :string :doc "company or business name, if they gave one")
  ("phone" :string :doc "phone number, only if they gave it")
  ("availability" :string :doc "when they can take a first call, e.g. weekday mornings, Lisbon time")
  ("locale" :string :options '("en" "pt")
   :doc "answer language: English (en) or European Portuguese (pt); the section's when the call names none")
  ("confirmed" :boolean :required t
   :doc "true records that the person reviewed the message and asked for it to be sent; the studio refuses an inquiry without it"))

(defun contact (&key name email message company phone availability locale confirmed)
  "One inquiry to the studio. Refuses before sending unless the person confirmed it."
  (unless confirmed
    (fail "developh_contact: confirmed is not true; show the person the message you are about to ~
send, and call again with confirmed true only once they asked for it to go"))
  (clip (developh-call "request_contact"
                        (bolt "name" (nonempty name)
                              "email" (nonempty email)
                              "message" (nonempty message)
                              "company" (nonempty company)
                              "phone" (nonempty phone)
                              "availability" (nonempty availability)
                              "locale" (locale-of locale)
                              "confirmed" t)
                        :timeout (setting :timeout-seconds))
        (setting :max-chars)))

(defun tool-contact (call)
  "developh_contact: the server's request_contact, the one tool that writes."
  (contact :name (contact-call-name call)
           :email (contact-call-email call)
           :message (contact-call-message call)
           :company (contact-call-company call)
           :phone (contact-call-phone call)
           :availability (contact-call-availability call)
           :locale (contact-call-locale call)
           :confirmed (contact-call-confirmed call)))
