;;;; cell.lisp --- where a session's data actually goes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of MadeByBrain's privacy-xray plugin for the Hermes Agent, on
;;;; Nodecode's own seams: a hook on :ROUND writes one metadata line per model
;;;; request, a hook on :CREDENTIAL records the host a provider's key was
;;;; resolved for, the config audit reads ~/.nodecode/config.jsonc, a canary
;;;; person is searched for in this home's own stores, and the leak map is a
;;;; self-contained HTML file. Local only: this cell dials nothing, never
;;;; stores message content, and never deletes anything.

(defpackage #:nodecode-privacy-xray
  (:use #:cl)
  (:nicknames #:privacy-xray)
  (:export #:xray-status #:xray-check #:xray-canary #:xray-trace #:xray-find
           #:xray-report #:xray-fix #:ledger-path #:report-path #:start-cell))

(in-package #:nodecode-privacy-xray)

(nlk:define-peripheral privacy-xray :not-running t)

(defvar *config* #'nle:config-get
  "The one seam over the shared config: a path, answering a keyword plist.
Tests stub it.")

(defvar *config-set* #'nle:config-set
  "The one seam that writes the shared config. Tests stub it.")

(defparameter +tab+ (string (code-char 9)))

(defparameter +actions+ '("status" "check" "canary" "trace" "find" "report" "fix")
  "What the privacy_xray tool answers, and what /xray takes as its first word.")

;;; --- what a destination is ------------------------------------------------------
;;; Data, not code: a host's vendor, and the country its headquarters is in.
;;; Only what the audit needs to say "this left for a company here, or there".

(defparameter +vendors+
  '(("anthropic.com" "Anthropic" "US")
    ("openai.com" "OpenAI" "US")
    ("googleapis.com" "Google" "US")
    ("google.com" "Google" "US")
    ("openrouter.ai" "OpenRouter" "US")
    ("deepseek.com" "DeepSeek" "CN")
    ("moonshot.ai" "Moonshot" "CN")
    ("moonshot.cn" "Moonshot" "CN")
    ("z.ai" "Z.AI" "CN")
    ("bigmodel.cn" "Z.AI" "CN")
    ("minimax.io" "MiniMax" "CN")
    ("minimaxi.com" "MiniMax" "CN")
    ("aliyuncs.com" "Alibaba" "CN")
    ("x.ai" "xAI" "US")
    ("mistral.ai" "Mistral" "FR")
    ("groq.com" "Groq" "US")
    ("together.xyz" "Together" "US")
    ("fireworks.ai" "Fireworks" "US")
    ("cerebras.ai" "Cerebras" "US")
    ("githubcopilot.com" "GitHub" "US")
    ("githubusercontent.com" "GitHub" "US")
    ("tavily.com" "Tavily" "US")
    ("exa.ai" "Exa" "US")
    ("brave.com" "Brave" "US")
    ("perplexity.ai" "Perplexity" "US")
    ("serpapi.com" "SerpApi" "US")
    ("ollama.com" "Ollama" "US")
    ("amazonaws.com" "Amazon" "US")
    ("microsoft.com" "Microsoft" "US")
    ("azure.com" "Microsoft" "US")
    ("discord.com" "Discord" "US")
    ("telegram.org" "Telegram" "AE")
    ("slack.com" "Slack" "US")
    ("matrix.org" "Matrix" "GB")
    ("cloudflare.com" "Cloudflare" "US"))
  "Host suffix -> (vendor, headquarters). A host this table does not know is
reported as unknown, never guessed.")

(defparameter +provider-hosts+
  '(("anthropic" . "api.anthropic.com")
    ("openai" . "api.openai.com")
    ("google" . "generativelanguage.googleapis.com")
    ("openrouter" . "openrouter.ai")
    ("deepseek" . "api.deepseek.com")
    ("moonshot" . "api.moonshot.ai")
    ("kimi" . "api.moonshot.ai")
    ("zai" . "api.z.ai")
    ("minimax" . "api.minimax.io")
    ("xai" . "api.x.ai")
    ("mistral" . "api.mistral.ai")
    ("groq" . "api.groq.com")
    ("together" . "api.together.xyz")
    ("fireworks" . "api.fireworks.ai")
    ("cerebras" . "api.cerebras.ai")
    ("ollama" . "localhost"))
  "Provider id -> the host its dialect defaults to, when no base_url is set.")

(defun host-of (url)
  "The host in URL, lowercased, without scheme, userinfo or port; NIL when
URL names none."
  (when (and (stringp url) (plusp (length url)))
    (let* ((text (string-downcase (nlk:trimmed url)))
           (scheme (search "://" text))
           (rest (subseq text (if scheme (+ scheme 3) 0))))
      (dolist (cut (list (code-char 47) (code-char 63) (code-char 35)))
        (let ((at (position cut rest))) (when at (setf rest (subseq rest 0 at)))))
      (let ((at (position (code-char 64) rest))) (when at (setf rest (subseq rest (1+ at)))))
      (let ((at (position (code-char 58) rest))) (when at (setf rest (subseq rest 0 at))))
      (when (plusp (length rest)) rest))))

(defun vendor-of (host)
  "(vendor headquarters) for HOST, or NIL when the table does not know it."
  (when host
    (loop for (suffix vendor hq) in +vendors+
          when (or (string= host suffix)
                   (and (> (length host) (length suffix))
                        (uiop:string-suffix-p host (concatenate 'string "." suffix))))
            return (list vendor hq))))

(defun vendor-text (host)
  "HOST with what is known about it, in one clause."
  (let ((vendor (vendor-of host)))
    (cond ((null host) "an unknown destination")
          (vendor (format nil "~a, headquarters ~a" (first vendor) (second vendor)))
          (t "a host this table does not know"))))

(defun inside-region-p (host)
  "Whether HOST's vendor sits in the home region the section names; true when
the section names none, or the vendor is unknown."
  (let* ((region (string-upcase (nlk:trimmed (or (setting :home-region) ""))))
         (vendor (vendor-of host)))
    (or (zerop (length region)) (null host) (null vendor)
        (member (second vendor) (uiop:split-string region :separator (list (code-char 44) (code-char 32)))
                :test #'string=))))

;;; --- the ledger ----------------------------------------------------------------
;;; One tab-separated line per record, in this home's own data folder:
;;;   t=2026-11-08T12:00:00.000Z  kind=request  session=.. turn=.. round=1 ...
;;; Metadata only: no prompt, no answer, no path of the session.

(defun data-dir () (nlk:home "privacy-xray/"))
(defun ledger-path () (nlk:home "privacy-xray/ledger.tsv"))
(defun canary-path () (nlk:home "privacy-xray/canaries.tsv"))
(defun report-path () (nlk:home "privacy-xray/report.html"))

(defun ensure-data-dir () (ensure-directories-exist (data-dir)))

(defun field-text (value)
  "VALUE as one ledger field: no tab, no newline."
  (let ((text (princ-to-string (or value ""))))
    (string-trim " " (substitute (code-char 32) #\Newline
                                  (substitute (code-char 32) (code-char 9) text)))))

(defun join (strings separator)
  (with-output-to-string (out)
    (loop for text in strings for first = t then nil
          do (unless first (write-string separator out))
             (write-string text out))))

(defun ledger-append (kind &rest pairs)
  "One metadata line: KIND, then PAIRS of member name and value."
  (ensure-data-dir)
  (rotate-ledger)
  (with-open-file (out (ledger-path) :direction :output :if-exists :append
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
    (write-line (join (cons (format nil "t=~a" (nlk:iso-now))
                            (cons (format nil "kind=~a" kind)
                                  (loop for (key value) on pairs by #'cddr
                                        collect (format nil "~(~a~)=~a" key
                                                        (field-text value)))))
                      +tab+)
                out)))

(defun rotate-ledger ()
  "When the ledger is at its ceiling, keep the last one beside it under
.1.tsv; older lines are dropped."
  (let ((path (ledger-path)))
    (when (and (probe-file path)
               (>= (or (ignore-errors (with-open-file (in path) (file-length in))) 0)
                   (* 1024 1024 (setting :ledger-max-mb))))
      (let ((old (nlk:home "privacy-xray/ledger.1.tsv")))
        (when (probe-file old) (ignore-errors (delete-file old)))
        (ignore-errors (rename-file path old))))))

(defun parse-line (line)
  "One line as an alist of (name . value) strings."
  (loop for part in (uiop:split-string line :separator (list (code-char 9)))
        for at = (position #\= part)
        when (and at (plusp at)) collect (cons (subseq part 0 at) (subseq part (1+ at)))))

(defun entries (path)
  "Every record in the line-oriented file at PATH, oldest first."
  (when (probe-file path)
    (with-open-file (in path :external-format :utf-8 :if-does-not-exist nil)
      (when in
        (loop for line = (read-line in nil nil) while line
              for entry = (parse-line line)
              when entry collect entry)))))

(defun value-of (entry name)
  (cdr (assoc name entry :test #'string=)))

(defun request-entries ()
  (remove-if-not (lambda (entry) (string= "request" (or (value-of entry "kind") "")))
                 (entries (ledger-path))))

(defun destination-entries ()
  (remove-if-not (lambda (entry) (string= "destination" (or (value-of entry "kind") "")))
                 (entries (ledger-path))))

;;; --- what the config says -------------------------------------------------------
;;; NLE:CONFIG-GET is the one reader: the shared ~/.nodecode/config.jsonc at a
;;; path, as keyword plists and vectors, every secret already replaced.

(defun spec-of (path)
  "The config object at PATH (a list of member names) as a keyword plist, or NIL."
  (let ((value (ignore-errors (funcall *config* path))))
    (and (consp value) (listp value) value)))

(defun array-of (path)
  "The config array at PATH as a list: a vector as it is, a plist as one
object, nothing else as no array."
  (let ((value (ignore-errors (funcall *config* path))))
    (cond ((vectorp value) (coerce value 'list))
          ((and (consp value) (keywordp (first value))) (list value))
          ((consp value) value)
          (t '()))))

(defun provider-host (provider)
  "The host PROVIDER's requests go to: its base_url's, else the dialect's own."
  (let* ((section (and (stringp provider) (spec-of (list "providers" provider))))
         (base (and section (getf section :base-url)))
         (host (and (stringp base) (host-of base))))
    (or host (cdr (assoc (string-downcase (or provider "")) +provider-hosts+
                         :test #'string=)))))

(defun route (label spec)
  "A route as a plist: :LABEL, :PROVIDER, :MODEL, :HOST, :VENDOR."
  (let* ((provider (and spec (getf spec :provider)))
         (model (and spec (getf spec :model)))
         (host (provider-host provider)))
    (list :label label :provider provider :model model :host host
          :vendor (vendor-of host))))

(defun main-route () (route "main model" (spec-of '("general" "default_model"))))

(defun route-text (route)
  "One route as the sentence the audit prints."
  (cond ((null (getf route :provider))
         (format nil "~a: the build default, nothing set" (getf route :label)))
        ((null (getf route :host))
         (format nil "~a: ~a/~a, and no base_url, so the host is unknown"
                 (getf route :label) (getf route :provider) (getf route :model)))
        (t (format nil "~a: ~a/~a -> ~a (~a)"
                   (getf route :label) (getf route :provider) (getf route :model)
                   (getf route :host) (vendor-text (getf route :host))))))

(defun main-route-label ()
  "The main model as the config names it, for a ledger line."
  (let ((route (main-route)))
    (if (getf route :provider)
        (format nil "~a/~a" (getf route :provider) (getf route :model))
        "build default")))

;;; --- the two hooks ---------------------------------------------------------------

(defvar *destinations-seen* (make-hash-table :test #'equal)
  "(PROVIDER . HOST) already written while this cell runs: a credential is
resolved once a turn, the ledger needs the destination once.")

(defun on-round (op next)
  "A :ROUND hook: one line per model request, its first round included. The
parts are the session, its turn, the round number and the main model the
config names. Never the prompt."
  (ignore-errors
   (ledger-append "request"
                   :session (or (getf op :session-id) "?")
                   :turn (or (getf op :turn) "")
                   :round (or (getf op :round) 0)
                   :route (ignore-errors (main-route-label))))
  (funcall next op))

(defun on-credential (op next)
  "A :CREDENTIAL hook: which host a provider's key was resolved for. Runs on
whichever thread froze the config, and answers what NEXT answers."
  (ignore-errors
   (let* ((provider (getf op :provider))
          (host (host-of (getf op :endpoint)))
          (key (cons (princ-to-string provider) host)))
     (unless (gethash key *destinations-seen*)
       (setf (gethash key *destinations-seen*) t)
       (ledger-append "destination" :provider provider :family (getf op :family)
                      :host (or host "unknown")))))
  (funcall next op))

(defun destination-rows ()
  "((HOST PROVIDER) ...) in the order a credential was first resolved for it."
  (let ((seen (make-hash-table :test #'equal)) (rows '()))
    (dolist (entry (destination-entries))
      (let ((host (value-of entry "host")))
        (unless (gethash host seen)
          (setf (gethash host seen) t)
          (push (list host (value-of entry "provider")) rows))))
    (nreverse rows)))

(defun requests-by (predicate)
  (count-if predicate (request-entries)))

(defun request-time (entry) (value-of entry "t"))

;;; --- the config audit ------------------------------------------------------------
;;; Each finding: :ID, :SEVERITY (:green, :amber, :red), :TITLE, :DETAIL and a
;;; :FIX the operator can evaluate. Nothing here writes the config.

(defun finding (id severity title detail &key fix)
  (list :id id :severity severity :title title :detail detail :fix fix))

(defun fix-form (path value)
  (format nil "(config-set '~s '~s)" path value))

(defun side-task-fix (main)
  (fix-form '("general" "auxiliary_model")
            (list :provider (getf main :provider) :model (getf main :model))))

(defun check-findings ()
  (let* ((main (main-route))
         (findings '()))
    (flet ((add (finding) (push finding findings)))
      (add (finding "main"
                    (if (and (getf main :host) (not (inside-region-p (getf main :host))))
                        :amber :green)
                    "Main model" (route-text main)))
      (let ((aux (route "auxiliary model" (spec-of '("general" "auxiliary_model")))))
        (cond ((null (getf aux :provider))
               (add (finding "aux" :green "Auxiliary model (session names, recaps, summaries)"
                             "unset: they run on the session's own model, where the conversation already goes")))
              ((equal (getf aux :host) (getf main :host))
               (add (finding "aux" :green "Auxiliary model" (route-text aux))))
              (t (add (finding "aux" :red "Auxiliary model goes to another vendor"
                               (format nil "~a -- a second recipient that gets the conversation itself"
                                       (route-text aux))
                               :fix (side-task-fix main))))))
      (let ((chain (array-of '("failover" "chain"))))
        (loop for spec in chain for index from 1
              for route = (route (format nil "failover ~d" index) spec)
              when (and (getf route :host) (not (equal (getf route :host) (getf main :host))))
                do (add (finding (format nil "failover.~d" index) :amber
                                 (format nil "Backup model ~d" index) (route-text route)))))
      (let ((decider (route "decider model" (spec-of '("general" "decider_model")))))
        (when (and (getf decider :host) (not (equal (getf decider :host) (getf main :host))))
          (add (finding "decider" :amber "Decider model on another host" (route-text decider)))))
      (when (spec-of '("request_bodies"))
        (add (finding "bodies" :red "Request bodies on disk"
                      "every prompt and its headers is written to this home as a file")))
      (let ((channels (spec-of '("channels"))))
        (loop for (name value) on channels by #'cddr
              unless (and (consp value) (eq (getf value :enabled) :false))
                do (add (finding (format nil "channel.~(~a~)" name) :amber
                                 (format nil "Channel: ~(~a~)" name)
                                 "a chat surface: what is said there reaches that company's servers"))))
      (when (spec-of '("websearch"))
        (add (finding "web" :amber "Web search and page fetch"
                      "queries and fetched pages leave for the search provider's host")))
      (let* ((qa (spec-of '("qa")))
             (share (and qa (getf qa :share))))
        (cond ((and (stringp share) (string= (string-downcase share) "never"))
               (add (finding "qa" :green "Usage page" "shared never")))
              ((stringp share)
               (add (finding "qa" :amber "Usage page"
                             (format nil "anonymous counts leave every week for the share url (~a)" share))))
              (t (add (finding "qa" :green "Usage page" "not set: nothing is sent until somebody answers")))))
      (let ((servers (spec-of '("mcp" "servers"))))
        (when servers
          (add (finding "mcp" :amber "MCP servers"
                        "every server this home lists runs with this machine's access"))))
      (add (finding "retention" :amber "chats on this machine"
                    "sessions stay in this home until you delete them; prune them yourself, this cell deletes nothing")))
    (nreverse findings)))

(defun severity-rank (severity)
  (case severity (:red 0) (:amber 1) (t 2)))

(defun severity-marker (severity)
  (case severity (:red "red  ") (:amber "amber") (t "green")))

(defun findings-text (&key all)
  (let* ((findings (stable-sort (check-findings) (lambda (a b)
                                                   (< (severity-rank (getf a :severity))
                                                      (severity-rank (getf b :severity))))))
         (shown (if all findings
                    (remove :green findings :key (lambda (f) (getf f :severity)))))
         (red (count :red findings :key (lambda (f) (getf f :severity))))
         (amber (count :amber findings :key (lambda (f) (getf f :severity)))))
    (with-output-to-string (out)
      (format out "Privacy x-ray: config audit, ~a~%" (nlk:iso-now))
      (if (null shown)
          (format out "Nothing leaves for a second recipient.~%")
          (dolist (finding shown)
            (format out "  ~a  ~a: ~a~%"
                    (severity-marker (getf finding :severity))
                    (getf finding :id) (getf finding :title))
            (format out "           ~a~%" (getf finding :detail))
            (when (getf finding :fix)
              (format out "           fix: ~a~%" (getf finding :fix)))))
      (format out "~%~d finding~:p: ~d red, ~d amber. Nothing here changes a setting; /xray fix <id> shows how.~%"
              (length findings) red amber))))

(defun entry-for (id)
  (find id (check-findings) :key (lambda (f) (getf f :id)) :test #'string=))

(defun xray-fix (id &key apply)
  "Show the fix for finding ID; with APPLY, write it through CONFIG-SET."
  (let ((finding (and (stringp id) (plusp (length id)) (entry-for id))))
    (unless finding
      (fail "no finding is called ~s; /xray check lists the ones there are" (or id "")))
    (let ((fix (getf finding :fix)))
      (unless fix (fail "~a has no automatic fix; the check says what to do instead" id))
      (if (not apply)
          (format nil "Privacy x-ray: ~a~%  ~a~%  fix: ~a~%Apply it with /xray fix ~a yes.~%"
                  (getf finding :title) (getf finding :detail) fix id)
          (let ((main (main-route)))
            (unless (getf main :provider)
              (fail "no main model is set, so there is nothing to point the side tasks at"))
            (funcall *config-set* '("general" "auxiliary_model")
                     (list :provider (getf main :provider) :model (getf main :model)))
            (format nil "Privacy x-ray: the auxiliary model is now ~a/~a; it applies from the next session (/new).~%"
                    (getf main :provider) (getf main :model)))))))

;;; --- the canary -----------------------------------------------------------------
;;; A fictitious person, every value invented: the e-mail domain example.org is
;;; reserved for documentation, the phone range +44 7700 900000-900999 is the
;;; one reserved for fiction. Mention it in a message, then trace where it went.

(defparameter +first-names+
  '("Ida" "Tomas" "Ruth" "Emil" "Greta" "Hugo" "Lena" "Otto" "Mira" "Jonas" "Klara" "Anton"))

(defparameter +last-names+
  '("Holtingham" "Brackenridge" "Marsden" "Thurston" "Vellacott" "Dunmore"
    "Ashcombe" "Pemberly" "Rothwell" "Wyndham" "Fairbanks" "Lindqvist"))

(defun group4 (text)
  (with-output-to-string (out)
    (loop for start from 0 below (length text) by 4
          do (when (plusp start) (write-char (code-char 32) out))
             (write-string (subseq text start (min (length text) (+ start 4))) out))))

(defun make-canary (&aux (stamp (get-universal-time)))
  "A new fictitious person as an alist of the ledger's own field names."
  (let* ((first (nth (mod stamp (length +first-names+)) +first-names+))
         (last (nth (mod (floor stamp 7) (length +last-names+)) +last-names+))
         (digits (format nil "~4,'0d" (mod (* stamp 37) 10000)))
         (email (format nil "~(~a~).~(~a~).~a@example.org" first last digits))
         (phone (format nil "+44 7700 900~3,'0d" (mod (* stamp 11) 1000)))
         (number (format nil "~20,'0d" (mod (* stamp 7919) (expt 10 20))))
         (iban (concatenate 'string "DE" (group4 number))))
    (list (cons "t" (nlk:iso-now))
          (cons "id" (subseq (nlk:sha256-text email) 0 6))
          (cons "name" (format nil "~a ~a" first last))
          (cons "email" email)
          (cons "phone" phone)
          (cons "iban" iban))))

(defun canary-rows () (entries (canary-path)))

(defun field-of (row name) (cdr (assoc name row :test #'string=)))

(defun active-canary ()
  "The newest canary this home has made, or NIL."
  (let ((rows (canary-rows))) (and rows (car (last rows)))))

(defun write-rows (path rows)
  (ensure-data-dir)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
    (dolist (row rows)
      (write-line (join (loop for (name . value) in row
                              collect (format nil "~a=~a" name (field-text value)))
                        +tab+)
                  out))))

(defun keep-canary (canary)
  "CANARY, and the four newest before it: the last five are kept."
  (let* ((rows (canary-rows))
         (kept (last rows (min 4 (length rows)))))
    (write-rows (canary-path) (append kept (list canary)))))

(defun xray-canary ()
  "Make a fictitious person and keep it beside the last four."
  (let ((canary (make-canary)))
    (keep-canary canary)
    (format nil "Privacy x-ray: new canary, every value invented.~%  id      ~a~%  name    ~a~%  e-mail  ~a~%  phone   ~a~%  iban    ~a~%~%Mention it in a normal message, then /xray trace shows where it went.~%"
            (field-of canary "id") (field-of canary "name") (field-of canary "email")
            (field-of canary "phone") (field-of canary "iban"))))

;;; --- searching this home's own stores -------------------------------------------
;;; Read-only, counts and a masked snippet only, and never this cell's own folder.

(defparameter +search-roots+ '("sessions/" "logs/" "memories/" "boards/"))

(defun search-files ()
  "The files one search reads: the home's stores up to the section's ceiling."
  (let ((files '()) (budget (setting :scan-files)))
    (labels ((walk (dir depth)
               (when (and (plusp depth) (plusp budget) (uiop:directory-exists-p dir))
                 (dolist (file (uiop:directory-files dir))
                   (when (plusp budget) (decf budget) (push file files)))
                 (dolist (sub (uiop:subdirectories dir))
                   (walk sub (1- depth))))))
      (dolist (root +search-roots+) (walk (nlk:home root) 3))
      (let ((store (nlk:home "store.db")))
        (when (and store (probe-file store)) (push store files))))
    (nreverse (remove-if (lambda (file) (search "privacy-xray" (namestring file))) files))))

(defun file-text (file)
  "FILE's bytes as a latin-1 string, when it is under the size ceiling."
  (let* ((ceiling (* 1024 (setting :scan-kb)))
         (size (ignore-errors (with-open-file (in file :element-type '(unsigned-byte 8))
                                (file-length in)))))
    (when (and size (<= size ceiling))
      (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
        (with-open-file (in file :element-type '(unsigned-byte 8) :if-does-not-exist nil)
          (when in (read-sequence bytes in)))
        (sb-ext:octets-to-string bytes :external-format :latin-1)))))

(defun count-occurrences (text needle)
  (loop with start = 0 and count = 0
        for at = (search needle text :start2 start)
        while at do (incf count) (setf start (+ at (length needle)))
        finally (return count)))

(defun first-occurrence (text needle)
  (search needle text))

(defun replace-all (text needle replacement)
  (with-output-to-string (out)
    (loop with start = 0
          for at = (search needle text :start2 start)
          while at
          do (write-string (subseq text start at) out)
             (write-string replacement out)
             (setf start (+ at (length needle)))
          finally (write-string (subseq text start) out))))

(defun snippet (text at needle)
  "A short, masked look at what is around AT: the needle keeps two characters."
  (let* ((start (max 0 (- at 30)))
         (end (min (length text) (+ at (length needle) 40)))
         (chunk (subseq text start end))
         (label (if (> (length needle) 2) (concatenate 'string (subseq needle 0 2) "***") "***")))
    (nlk:trimmed
     (substitute (code-char 32) #\Newline
                 (substitute (code-char 32) (code-char 9) (replace-all chunk needle label))))))

(defun search-stores (needles)
  "((FILE COUNT SNIPPET) ...) for every file holding one of NEEDLES."
  (let ((needles (remove-if-not (lambda (needle) (and (stringp needle) (>= (length needle) 3))) needles))
        (hits '()))
    (when needles
      (dolist (file (search-files))
        (let ((text (file-text file)))
          (when text
            (let ((count (reduce #'+ needles :key (lambda (needle) (count-occurrences text needle)))))
              (when (plusp count)
                (let ((at (some (lambda (needle) (first-occurrence text needle)) needles)))
                  (push (list file count
                              (and at (snippet text at (or (find-if (lambda (needle)
                                                                      (first-occurrence text needle))
                                                                    needles)
                                                            ""))))
                        hits))))))))
    (nreverse hits)))

(defun hits-text (hits &key (indent "  "))
  (with-output-to-string (out)
    (if (null hits)
        (format out "~aSearched, nothing found.~%" indent)
        (dolist (hit hits)
          (format out "~a~a: ~d hit(s)~@[ -- ~a~]~%"
                  indent (nlk:home-abbreviated (namestring (first hit))) (second hit) (third hit))))))

;;; --- the answers ----------------------------------------------------------------

(defun iso-of (universal)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(defun requests-since (since)
  (remove-if (lambda (entry)
               (and since (string< (or (request-time entry) "") since)))
             (request-entries)))

(defun xray-trace (&key hours)
  "Where the newest canary went: the requests since it was made, and every
local file that carries one of its values."
  (let ((canary (active-canary)))
    (unless canary
      (fail "no canary yet: /xray canary makes one, then talk normally"))
    (let* ((since (if (and hours (plusp hours))
                      (iso-of (- (get-universal-time) (* 3600 hours)))
                      (field-of canary "t")))
           (requests (requests-since since))
           (hits (search-stores (list (field-of canary "email") (field-of canary "name")))))
      (with-output-to-string (out)
        (format out "Privacy x-ray: canary ~a (~a), made ~a~%"
                (field-of canary "name") (field-of canary "id") (field-of canary "t"))
        (format out "Sent out: ~d model request(s) since ~a~%" (length requests) since)
        (dolist (row (destination-rows))
          (format out "  ~a -- ~a~%" (first row) (vendor-text (first row))))
        (format out "Stored on this machine (~d file(s) with one of its values):~%" (length hits))
        (format out "~a" (hits-text hits))
        (format out "~%The ledger records metadata only, so the counts above are the requests since it was made; the hits are where its own values sit in this home.~%")))))

(defun xray-status ()
  (let* ((requests (request-entries))
         (rows (destination-rows))
         (canary (active-canary))
         (path (ledger-path))
         (size (or (ignore-errors (with-open-file (in path :element-type '(unsigned-byte 8))
                                     (file-length in))) 0)))
    (with-output-to-string (out)
      (format out "Privacy x-ray, ~a~%" (nlk:iso-now))
      (format out "Ledger: ~d model request(s), ~d destination(s), ~d KB at ~a~%"
              (length requests) (length rows) (ceiling size 1024)
              (nlk:home-abbreviated (namestring path)))
      (when rows
        (dolist (row rows)
          (format out "  ~a -- ~a~@[ (provider ~a)~]~%"
                  (first row) (vendor-text (first row)) (second row))))
      (format out "Canary: ~a~%"
              (if canary
                  (format nil "~a (~a), made ~a, ~d request(s) since"
                          (field-of canary "name") (field-of canary "id") (field-of canary "t")
                          (length (requests-since (field-of canary "t"))))
                  "none yet -- /xray canary makes one"))
      (format out "Recording: ~a; home region: ~a; watch terms: ~a~%"
              (if (setting :record) "on" "off")
              (let ((region (nlk:trimmed (or (setting :home-region) ""))))
                (if (plusp (length region)) region "unset"))
              (let ((terms (setting :watch-terms)))
                (if terms (join terms ", ") "none")))
      (format out "Run /xray check for the config audit, /xray report for the leak map.~%"))))

(defun request-hosts ()
  "((HOST COUNT) ...) over the ledger's request lines, biggest first."
  (let ((counts (make-hash-table :test #'equal)) (order '()))
    (dolist (entry (request-entries))
      (let ((host (value-of entry "host")))
        (unless (gethash host counts) (push host order))
        (incf (gethash host counts 0))))
    (stable-sort (loop for host in order collect (list host (gethash host counts)))
                 #'> :key #'second)))

(defun html-escape (text)
  (with-output-to-string (out)
    (loop for character across (princ-to-string (or text ""))
          do (case character
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out))
               (t (write-char character out))))))

(defun verdict-line ()
  (let* ((hosts (request-hosts))
         (total (length (request-entries))))
    (cond ((zerop total) "No model request recorded yet; the ledger starts filling with the next turn.")
          ((null hosts) (format nil "~d request(s) recorded; their destination was not resolved in this run." total))
          (t (format nil "~d model request(s) to ~d destination(s): ~{~a~^, ~}."
                     total (length hosts)
                     (loop for host in hosts collect (or (first host) "unknown")))))))

(defun xray-report ()
  "Write the leak map as one self-contained HTML file, and name it."
  (let* ((hosts (request-hosts))
         (rows (destination-rows))
         (canary (active-canary))
         (findings (check-findings))
         (most (max 1 (or (and hosts (second (first hosts))) 1))))
    (ensure-data-dir)
    (with-open-file (out (report-path) :direction :output :if-exists :supersede
                                    :if-does-not-exist :create :external-format :utf-8)
      (format out "<!doctype html>~%<html lang=\"en\"><head><meta charset=\"utf-8\">~%")
      (format out "<title>Privacy x-ray</title>~%")
      (format out "<style>body{font:15px/1.5 system-ui,sans-serif;margin:2rem;max-width:52rem;color:#111}~%")
      (format out "h1{font-size:1.4rem} .bar{background:#4b6;height:1.1rem;border-radius:3px}~%")
      (format out ".row{margin:.8rem 0} .name{font-family:ui-monospace,monospace}~%")
      (format out ".red{color:#b21} .amber{color:#a60} .green{color:#260}~%")
      (format out "ul{padding-left:1.2rem} li{margin:.2rem 0}</style></head><body>~%")
      (format out "<h1>Privacy x-ray</h1>~%<p>~a</p>~%" (html-escape (nlk:iso-now)))
      (format out "<p><strong>~a</strong></p>~%" (html-escape (verdict-line)))
      (format out "<h2>Where the requests went</h2>~%")
      (if (null hosts) (format out "<p>Nothing recorded yet.</p>~%")
          (dolist (host hosts)
            (format out "<div class=\"row\"><span class=\"name\">~a</span> -- ~a, ~d request(s)~%"
                    (html-escape (first host)) (html-escape (vendor-text (first host))) (second host))
            (format out "<div class=\"bar\" style=\"width:~,1f%\"></div></div>~%"
                    (* 100 (/ (second host) most)))))
      (format out "<h2>Credentials resolved</h2><ul>~%")
      (if (null rows) (format out "<li>none in this run</li>~%")
          (dolist (row rows)
            (format out "<li><span class=\"name\">~a</span> -- ~a~@[ (provider ~a)~]</li>~%"
                    (html-escape (first row)) (html-escape (vendor-text (first row)))
                    (html-escape (second row)))))
      (format out "</ul>~%<h2>Canary</h2>~%")
      (format out "<p>~a</p>~%"
              (if canary
                  (html-escape (format nil "~a (~a), made ~a, ~d request(s) since."
                                        (field-of canary "name") (field-of canary "id")
                                        (field-of canary "t")
                                        (length (requests-since (field-of canary "t")))))
                  "None yet."))
      (format out "<h2>Config audit</h2><ul>~%")
      (dolist (finding findings)
        (format out "<li class=\"~(~a~)\"><strong>~a</strong> -- ~a: ~a</li>~%"
                (getf finding :severity) (html-escape (getf finding :id))
                (html-escape (getf finding :title)) (html-escape (getf finding :detail))))
      (format out "</ul>~%<p>Local only: this page was written on this machine and reads nothing from the network.</p>~%")
      (format out "</body></html>~%"))
    (format nil "Privacy x-ray: the leak map is at ~a -- one file, no scripts, no network.~%"
            (nlk:home-abbreviated (namestring (report-path))))))

(defun xray-find (subject)
  "Search this home's stores for SUBJECT, read-only: counts and a masked snippet."
  (let ((subject (nlk:trimmed (or subject ""))))
    (when (< (length subject) 3)
      (fail "say what to look for, at least three characters: /xray find \"a name or an address\""))
    (let ((hits (search-stores (list subject))))
      (with-output-to-string (out)
        (format out "Privacy x-ray: ~s in this home's stores (~d file(s) read)~%"
                subject (length (search-files)))
        (format out "~a" (hits-text hits))
        (format out "~%Read-only, this machine only, and no search of any kind left it.~%")))))

;;; --- the seams -----------------------------------------------------------------

(defun answer-action (action subject hours)
  (let ((action (string-downcase (nlk:trimmed (or action "status")))))
    (cond ((string= action "check") (findings-text :all t))
          ((string= action "canary") (xray-canary))
          ((string= action "trace") (xray-trace :hours hours))
          ((string= action "find") (xray-find subject))
          ((string= action "report") (xray-report))
          ((string= action "fix") (xray-fix subject))
          (t (xray-status)))))

(defun guarded (body)
  "Run BODY, answering its text; a refusal becomes the tool answer ERROR: ..."
  (handler-case (funcall body)
    (error (condition) (nle:failure (format nil "ERROR: PRIVACY-XRAY-ERROR: ~a" condition)))))

(nlk:define-contract xray-call
  (:doc "Where this session's data goes. Reads this machine only and changes nothing: status counts the model requests the cell's own ledger recorded and the hosts a provider key was resolved for; check audits the shared config for the settings on which conversation content leaves for a second vendor, each with the fix to evaluate; canary makes a fictitious person to mention in a message; trace says how many requests were made since that canary was made and which files in this home carry its values; find searches this home's stores for a name or an address, read-only, answering counts and a masked snippet; report writes a self-contained HTML leak map.")
  ("action" :text :required t :options +actions+
            :doc "status, check, canary, trace, find, report or fix")
  ("subject" :text :default ""
             :doc "for find: the text to look for, at least three characters; for fix: the finding id, such as aux")
  ("hours" :integer :default 0
           :doc "for trace: look at the last N hours only; 0 means since the canary was made"))

(defun xray-tool (call)
  "The privacy_xray tool: the contract's call, answered as text."
  (guarded (lambda ()
             (answer-action (xray-call-action call) (xray-call-subject call)
                            (xray-call-hours call)))))

(defun run-command (args session-id)
  "The /xray command: a word from the action list, then its subject."
  (declare (ignore session-id))
  (guarded (lambda ()
             (let* ((words (and args
                                (remove "" (uiop:split-string (nlk:trimmed args)
                                                               :separator (list (code-char 32)
                                                                                (code-char 9))))))
                    (action (string-downcase (or (first words) "status"))))
               (cond ((string= action "find") (xray-find (join (rest words) " ")))
                     ((string= action "fix")
                      (xray-fix (or (second words) "")
                                :apply (let ((word (third words)))
                                         (and word (string= (string-downcase word) "yes")))))
                     ((string= action "trace")
                      (xray-trace :hours (and (second words)
                                              (ignore-errors (parse-integer (second words))))))
                     (t (answer-action action "" 0)))))))

(defparameter +primer+
  "Privacy x-ray answers one question, from this machine only: where does what
I type actually go?

  /xray            what the local ledger holds: requests, destinations, canary
  /xray check      the config audit: settings that send the conversation to a
                   second vendor, each with the fix form for it
  /xray fix <id>   the fix, shown; `fix <id> yes` writes it through config-set
  /xray canary     a fictitious person to mention in a normal message
  /xray trace      how many requests were made since that canary, and which
                   files in this home carry its values
  /xray find ...   a read-only search of this home's stores, counts only
  /xray report     a self-contained HTML leak map, written to the data folder

The ledger records metadata only -- time, session, turn, round, the main model
and the host a credential was resolved for -- never a prompt, an answer or a
file path. Nothing here dials: no request leaves because of this cell.

Say what the audit found in plain words, name the finding id, and let the
operator run the fix: a setting changes only on an explicit `fix <id> yes`.
A green finding is no promise that a transfer is lawful; contracts and vendor
behaviour are not visible here.")

(defun start ()
  (ensure-data-dir)
  (clrhash *destinations-seen*)
  (nle:on-stop (lambda () (clrhash *destinations-seen*))))

(nle:define-cell privacy-xray
  (:section ("privacy-xray")
    (:guide "nothing here is required; the cell records metadata only, in this home's data folder; home_region is a comma-separated list of the countries you consider inside, such as EU,DE,CH; check, find and fix are the only verbs that read or write anything, and a fix is written only when the operator says yes")
    ("record" :boolean :default t
              :doc "write one metadata line per model request (never content)")
    ("watch_terms" :list
                    :doc "your own names or project code names, counted in a trace")
    ("home_region" :string :default ""
                   :doc "EU, EEA, DE, or a comma-separated list; a vendor outside it is amber")
    ("ledger_max_mb" :integer :default 5 :min 1
                     :doc "when the ledger reaches this size it is rotated once, to ledger.1.tsv")
    ("scan_files" :integer :default 300 :min 1
                  :doc "how many files one search reads")
    ("scan_kb" :integer :default 2048 :min 1
               :doc "the largest file one search reads"))
  (:tool "privacy_xray" (nlk:contract 'xray-call) 'xray-tool)
  (:command "xray" 'run-command
            :description "Where your data goes: ledger, audit, canary, leak map"
            :argument-hint "[status|check|fix <id> [yes]|canary|trace [hours]|find <text>|report]")
  (:help :privacy-xray
         "privacy-xray: where your data goes - requests, config audit, canary, leak map"
         +primer+)
  (:hook :round #'on-round)
  (:hook :credential #'on-credential)
  (:start #'start))
