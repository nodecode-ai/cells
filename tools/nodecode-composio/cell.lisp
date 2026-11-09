;;;; cell.lisp --- Composio's connected accounts, its catalog, and the CLI that owns them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of luisced/hermes-composio-connections (see NOTICE). The
;;;; original is a dashboard page over two authorities: the `composio' CLI,
;;;; which owns the user's login and their connected accounts, and Composio's
;;;; toolkit catalog at backend.composio.dev, which the page reads with the
;;;; COMPOSIO_USER_API_KEY the profile already holds. Both are what this cell
;;;; reaches: list_connections, link and connections remove through the CLI,
;;;; GET /api/v3/toolkits over HTTP. The page itself -- its two tabs, its
;;;; paging, its disconnect dialog and its pty prompt dance -- is not here;
;;;; what it guaranteed is: a link is only ever connect.composio.dev's, a
;;;; removal re-reads the list and refuses if the account is still there,
;;;; a second account of the same toolkit needs an alias the toolkit does
;;;; not already use, and the catalog is served whole or not at all.

(defpackage #:nodecode-composio
  (:use #:cl)
  (:export #:composio-error #:composio-error-detail
           #:accounts #:services #:connect #:disconnect))

(in-package #:nodecode-composio)

(nlk:define-peripheral composio :not-running t)

;;; --- the manual ------------------------------------------------------------------

(defparameter +primer+
  "Composio connects an account to outside services -- Gmail, Slack, GitHub and 1,600 more -- and the
`composio' CLI on this machine owns that login. Call these four tools; they answer text.
  composio_accounts {toolkit?}                 the connected accounts: toolkit, alias, status, and the
                                               selector that disconnects one
  composio_services {query?, linked?, limit?}   the public toolkit catalog to connect to; needs the
                                               Composio API key in the composio section (linked: any,
                                               connected or unconnected)
  composio_connect {toolkit, alias?}            answers a https://connect.composio.dev/link/... URL: open
                                               it, the person authorizes in a browser; a second account of
                                               a toolkit you already have needs a new alias
  composio_disconnect {selector, confirm}       confirm:true removes exactly that account and re-reads the
                                               list to prove it is gone; a selector two rows share is
                                               never removable
The account list is the CLI's own, so a service connected elsewhere in that Composio account shows here too.
Composio's status is its word: ACTIVE does not prove every permission was used, and disconnecting from
Composio does not revoke the grant at Google or GitHub."
  "What (help :composio) answers while the cell runs.")

;;; --- what a name may be -----------------------------------------------------------

(defparameter +token-chars+ "_.-"
  "The characters a toolkit slug, an alias and a selector may carry besides alphanumerics.")

(defun token-p (text)
  "TEXT as a toolkit slug, an alias or a selector: a letter, a digit or _ first, then
alphanumerics and +TOKEN-CHARS+, at most 200. The original's one validation rule."
  (and (stringp text)
       (<= 1 (length text) 200)
       (let ((first (char text 0)))
         (or (alphanumericp first) (char= first #\_)))
       (every (lambda (character)
                (or (alphanumericp character) (find character +token-chars+)))
              text)))

(defun nonblank (text)
  "TEXT trimmed, when it is a non-blank string; else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

;;; --- the section, read back -------------------------------------------------------

(defparameter +default-origin+ "https://backend.composio.dev"
  "Composio's own API root: the one the original reads the catalog from.")

(defun read-settings (values table)
  "The declared VALUES, plus the API key the catalog authenticates with, read once.
The key is api_key written inline, else the variable api_key_env names, else
COMPOSIO_USER_API_KEY -- the name the original's manifest requires. None is
required: the account tools run on the CLI alone, and only the catalog asks."
  (declare (ignore table))
  (let* ((env (nonblank (getf values :api-key-env)))
         (inline (nonblank (getf values :api-key)))
         (key (or inline
                  (and env (nonblank (uiop:getenv env)))
                  (nonblank (uiop:getenv "COMPOSIO_USER_API_KEY"))))
         (root (string-right-trim "/" (or (getf values :base-url) "")))
         (cli (or (nonblank (getf values :cli)) "composio"))
         (plist (copy-list values)))
    (when (and env (null key))
      (nlk:config-error "api_key_env names ~a, but nothing is set there: put a Composio API key in it (composio.dev, Settings, API keys), or clear api_key_env" env))
    (unless (or (uiop:string-prefix-p "https://" root) (uiop:string-prefix-p "http://" root))
      (nlk:config-error "base_url ~s has no http:// or https:// scheme: set it to ~a, or leave it out" root +default-origin+))
    (setf (getf plist :key) key
          (getf plist :base-url) root
          (getf plist :cli) cli)
    plist))

;;; --- the CLI, never a shell ------------------------------------------------------

(defparameter +detail-chars+ 300
  "How much of a CLI's own failure text a refusal repeats.")

(defun auth-failure-p (text)
  "TEXT reads as the CLI's signed-out answer."
  (let ((text (string-downcase text)))
    (or (search "not logged in" text)
        (search "please login" text)
        (search "not authenticated" text)
        (search "run 'composio login'" text))))

(defun cli-run (args &key input)
  "Run the configured composio CLI with ARGS -- an argv, never a shell line -- and
INPUT on its stdin; => its stdout. A missing CLI, a signed-out one, a non-zero
exit or the section's timeout is a refusal saying which."
  (let ((seconds (setting :timeout-seconds))
        (cli (setting :cli)))
    (handler-case
        (multiple-value-bind (output error status)
            (nlk:run-bounded (cons cli args) :seconds seconds :input (or input ""))
          (cond
            ((eq status :timeout)
             (fail "the ~a CLI did not answer within ~d second~:p: run `~a ~{~a~^ ~}` yourself, then try again"
                   cli seconds cli args))
            ((and (integerp status) (plusp status))
             (let ((said (string-downcase (format nil "~a ~a" output error))))
               (if (auth-failure-p said)
                   (fail "the ~a CLI is not signed in on this machine: run `~a login` in a terminal, then try again" cli cli)
                   (fail "the ~a CLI exited ~d~@[: ~a~]" cli status
                         (nonblank (nlk:clip (nlk:one-line (format nil "~a" error)) +detail-chars+))))))
            (t (or output ""))))
      (composio-error (condition) (error condition))
      (error (condition)
        (fail "the ~a CLI could not run: ~a" cli (nlk:one-line (princ-to-string condition)))))))

;;; --- the accounts the CLI owns ----------------------------------------------------

(defstruct (account (:constructor make-account))
  "One row of `composio connections list`: the toolkit it is for, the alias the CLI
holds for it, its Composio status, and the selector this cell disconnects it by."
  toolkit alias alias-known-p status selector index removable key)

(defun item-selector (item)
  "The identifier the CLI knows ITEM's account by: its id, else its connected
account id, else its word id -- the original's order."
  (or (nonblank (nlk:json-value item :string "id"))
      (nonblank (nlk:json-value item :string "connected_account_id"))
      (nonblank (nlk:json-value item :string "word_id"))))

(defun parse-connections (text)
  "TEXT, the JSON `composio connections list' printed (toolkit -> accounts) => its
ACCOUNT rows. A shape this cell does not know is a refusal, never a partial list."
  (let ((body (ignore-errors (nlk:decode-json text))))
    (unless (hash-table-p body)
      (fail "the composio CLI's `connections list' did not answer JSON: run `composio connections list' yourself -- an empty answer means no account, a sign-in prompt means `composio login'"))
    (let ((rows '()))
      (maphash
       (lambda (toolkit accounts)
         (unless (and (stringp toolkit) (vectorp accounts))
           (fail "the composio CLI's connections answer is not a map of toolkit to accounts: update the CLI, or run `composio connections list' yourself"))
         (loop for item across accounts
               for index from 0
               do (unless (hash-table-p item)
                    (fail "one ~a account in the composio CLI's answer is not an object" toolkit))
                  (let ((status (nonblank (nlk:json-value item :string "status"))))
                    (unless status
                      (fail "one ~a account in the composio CLI's answer has no status" toolkit))
                    (push (make-account :toolkit toolkit :status status :index index
                                        :alias (nonblank (nlk:json-value item :string "alias"))
                                        :alias-known-p (nth-value 1 (gethash "alias" item))
                                        :selector (item-selector item))
                          rows))))
       body)
      rows)))

(defun list-connections ()
  "The connected accounts as the CLI lists them, each with what disconnects it:
a selector is removable only when exactly one row carries it, and a row without
one is never removable. Sorted by toolkit, then alias."
  (let ((rows (parse-connections (cli-run '("connections" "list"))))
        (counts (make-hash-table :test #'equal)))
    (dolist (row rows)
      (when (account-selector row)
        (incf (gethash (account-selector row) counts 0))))
    (dolist (row rows)
      (let ((selector (account-selector row)))
        (setf (account-removable row) (and selector (= 1 (gethash selector counts 0)))
              (account-key row) (if (account-removable row)
                                    (format nil "~a:~a" (account-toolkit row) selector)
                                    (format nil "~a:~a:~d" (account-toolkit row)
                                            (or selector "unidentified") (account-index row))))))
    (sort rows (lambda (a b)
                 (let ((x (format nil "~a ~a" (account-toolkit a) (or (account-alias a) "")))
                       (y (format nil "~a ~a" (account-toolkit b) (or (account-alias b) ""))))
                   (string-lessp x y))))))

(defun format-accounts (rows)
  "ROWS as the lines the model reads."
  (if (null rows)
      "No Composio account is connected on this machine. Call composio_services to find a toolkit, then composio_connect for its link."
      (with-output-to-string (out)
        (format out "~d connected Composio account~:p (`composio connections list`):" (length rows))
        (dolist (row rows)
          (format out "~% ~a~@[ (~a)~] -- ~a~a"
                  (account-toolkit row)
                  (account-alias row)
                  (account-status row)
                  (if (account-removable row)
                      (format nil " -- selector ~a" (account-selector row))
                      " -- no single selector of its own, so not removable from here"))))))

;;; --- connecting -------------------------------------------------------------------

(defparameter +link-origin+ "https://connect.composio.dev/link/"
  "The only host a connect link may be on: the original refuses any other.")

(defun link-url-p (text)
  "TEXT is exactly a Composio connect link: that origin, and one path segment of
letters, digits, _ and -."
  (and (stringp text)
       (uiop:string-prefix-p +link-origin+ text)
       (let ((tail (subseq text (length +link-origin+))))
         (and (plusp (length tail))
              (every (lambda (character)
                       (or (alphanumericp character)
                           (find character "_-")))
                     tail)))))

(defun link-in-text (text)
  "The one connect link TEXT carries, or NIL when it carries none or more than one.
The CLI prints prose around the link on some versions, JSON on others."
  (let ((found '())
        (start 0))
    (loop
      (let ((at (search +link-origin+ text :start2 start)))
        (unless at (return))
        (let* ((end (or (position-if-not (lambda (character)
                                            (or (alphanumericp character)
                                                (find character "_-")))
                                          text :start (+ at (length +link-origin+)))
                         (length text)))
               (url (subseq text at end)))
          (unless (member url found :test #'equal)
            (push url found))
          (setf start end))))
    (and (= 1 (length found)) (first found))))

(defun authorization-url (raw)
  "RAW, the CLI's answer to `link' => the connect link it printed or encoded, or NIL."
  (let ((payload (ignore-errors (nlk:decode-json raw))))
    (when (hash-table-p payload)
      (let ((url (or (nonblank (nlk:json-value payload :string "redirect_url"))
                     (nonblank (nlk:json-value payload :string "authorization_url")))))
        (when (and url (link-url-p url))
          (return-from authorization-url url)))))
  (let ((url (link-in-text (or raw ""))))
    (and (link-url-p url) url)))

(defun connect-service (toolkit alias)
  "TOOLKIT's OAuth link, as the CLI answers it. A toolkit this account already has
needs an alias no other account of that toolkit holds -- the original's two rules."
  (unless (token-p toolkit)
    (fail "~s is no toolkit slug: lowercase letters, digits, _ . - (gmail, google_drive, github)" toolkit))
  (when alias
    (unless (token-p alias)
      (fail "~s is no alias: letters, digits, _ . -, at most 200" alias)))
  (let* ((rows (list-connections))
         (existing (remove-if-not (lambda (row) (equal toolkit (account-toolkit row))) rows)))
    (when (and existing (null alias))
      (fail "~a already has ~d account~:p here: name this one with an alias" toolkit (length existing)))
    (when (and alias
               (find-if (lambda (row)
                          (and (account-alias row) (string-equal alias (account-alias row))))
                        existing))
      (fail "~a already has an account aliased ~a: pick another alias" toolkit alias))
    (let* ((args (append (list "link" toolkit "--no-browser" "--no-wait")
                         (when alias (list "--alias" alias))))
           (url (authorization-url (cli-run args))))
      (unless url
        (fail "the composio CLI answered no https://connect.composio.dev/link/... URL for ~a: run `composio link ~a` yourself and open what it prints" toolkit toolkit))
      (format nil "Open this link to authorize ~a~@[ as ~a~]:~%  ~a~%~%Then call composio_accounts: the new account appears as ACTIVE when the browser flow finishes."
              toolkit alias url))))

;;; --- disconnecting ----------------------------------------------------------------

(defun remove-account (selector confirm)
  "Remove the one account SELECTOR names, then read the list again: an account still
listed there is a refusal, so a removal is only ever claimed once it is seen gone."
  (unless (token-p selector)
    (fail "~s is no selector: copy one from composio_accounts (the id after `selector`)" selector))
  (unless (eq confirm t)
    (fail "set confirm to true to disconnect ~a: the account is removed from Composio, not from the provider" selector))
  (let* ((rows (list-connections))
         (targets (remove-if-not (lambda (row) (equal selector (account-selector row))) rows)))
    (unless (= 1 (length targets))
      (fail "~a names ~d account~:p in the CLI's list; copy exactly one selector from composio_accounts" selector (length targets)))
    (unless (account-removable (first targets))
      (fail "~a is not removable from here: the CLI lists no single id of its own for it" selector))
    (let ((toolkit (account-toolkit (first targets))))
      ;; The CLI asks "Remove TOOLKIT connection?" on a terminal; a pipe gets the same answer.
      (cli-run (list "connections" "remove" selector) :input (format nil "y~%"))
      (when (find-if (lambda (row) (equal selector (account-selector row))) (list-connections))
        (fail "the composio CLI ran, but ~a still appears in `composio connections list`: the removal is not verified, so refresh with composio_accounts" selector))
      (format nil "Disconnected ~a (~a). Composio's connection is gone; the grant at the provider may still stand." toolkit selector))))

;;; --- the toolkit catalog, over HTTP -----------------------------------------------

(defparameter +catalog-pages+ 50
  "The most pages one catalog read follows: the original's bound.")

(defvar *catalog-cache* nil
  "The last whole catalog read: (KEY READ-AT ITEMS TOTAL).")

(defstruct (service (:constructor make-service))
  "One toolkit in Composio's public catalog."
  slug name description categories managed-p no-auth-p)

(defun answer-detail (answer)
  "Composio's own words for a refused call, clipped."
  (let* ((body (ignore-errors (nlk:decode-json answer)))
         (text (and (hash-table-p body)
                    (or (nonblank (nlk:json-value body :string "message"))
                        (nonblank (nlk:json-value body :string "error"))))))
    (and text (nlk:clip (nlk:trimmed text) +detail-chars+))))

(defun catalog-key ()
  "The API key the catalog authenticates with, or a refusal naming where to put one."
  (or (setting :key)
      (fail "the toolkit catalog needs a Composio API key: write it as api_key in the composio section, or set COMPOSIO_USER_API_KEY (composio.dev, Settings, API keys), then (restart-cells \"nodecode-composio\")")))

(defun catalog-headers ()
  "The catalog's request headers: the key Composio reads, JSON out, and a name of its own."
  (list (cons "x-user-api-key" (catalog-key))
        (cons "accept" "application/json")
        (cons "user-agent" "Nodecode-Composio-Connections")))

(defun catalog-request (cursor)
  "One page of the catalog at CURSOR (NIL for the first) => its decoded object."
  (let* ((url (format nil "~a/api/v3/toolkits?limit=~d~@[&cursor=~a~]"
                       (setting :base-url) (setting :page-limit)
                       (nonblank (and cursor (quri:url-encode cursor)))))
         (answer nil)
         (status 0))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :get url :headers (catalog-headers)
                    :timeout (setting :catalog-timeout-seconds)))
      (error (condition)
        (fail "Composio's catalog did not answer: ~a" (nlk:one-line (princ-to-string condition)))))
    (cond
      ((member status '(401 403))
       (fail "Composio refused the API key (HTTP ~d): check api_key, or COMPOSIO_USER_API_KEY" status))
      ((not (and (integerp status) (<= 200 status 299)))
       (fail "Composio's catalog answered HTTP ~a~@[: ~a~]" status (answer-detail answer)))
      (t (let ((page (ignore-errors (nlk:decode-json answer))))
           (unless (hash-table-p page)
             (fail "Composio's catalog answered no JSON object: try again in a minute"))
           page)))))

(defun normalize-service (item)
  "ITEM, one catalog row (v3 toolkit metadata) => a SERVICE. A row this cell cannot
read is a refusal: the catalog is served whole or not at all."
  (unless (hash-table-p item) (fail "one catalog row is not an object"))
  (let ((slug (nonblank (nlk:json-value item :string "slug")))
        (name (nonblank (nlk:json-value item :string "name"))))
    (unless (and slug (token-p slug))
      (fail "a catalog row's slug ~s is not a toolkit slug" (nlk:json-value item :string "slug")))
    (unless name
      (fail "the catalog row ~a has no name" slug))
    (let* ((meta (nlk:json-value item :object "meta"))
           (categories (or (nlk:json-array meta "categories") #()))
           (managed (or (nlk:json-array item "composio_managed_auth_schemes") #())))
      (make-service :slug slug :name name
                    :description (or (nonblank (nlk:json-value meta :string "description")) "")
                    :categories (loop for entry across categories
                                      when (hash-table-p entry)
                                        collect (or (nlk:json-value entry :string "name") ""))
                    :managed-p (plusp (length managed))
                    :no-auth-p (eq t (nlk:json-value item :boolean "no_auth"))))))

(defun fetch-catalog ()
  "Every toolkit Composio publishes, one page after another => (values SERVICES TOTAL)."
  (let ((items '())
        (seen (make-hash-table :test #'equal))
        (cursors (make-hash-table :test #'equal))
        (cursor nil)
        (total nil))
    (dotimes (page +catalog-pages+)
      (let* ((body (catalog-request cursor))
             (rows (nlk:json-array body "items"))
             (count (nlk:json-value body :integer "total_items")))
        (unless (vectorp rows)
          (fail "Composio's catalog answered no item list: try again in a minute"))
        (unless (and (integerp count) (not (minusp count)) (or (null total) (= total count)))
          (fail "Composio's catalog answered an inconsistent total (~s): try again in a minute" count))
        (setf total count)
        (loop for raw across rows
              do (let* ((service (normalize-service raw))
                        (slug (service-slug service)))
                   (when (gethash slug seen)
                     (fail "Composio's catalog names ~a twice" slug))
                   (setf (gethash slug seen) t)
                   (push service items)))
        (let ((next (nonblank (nlk:json-value body :string "next_cursor"))))
          (cond
            ((null next)
             (unless (= (length items) total)
               (fail "Composio's catalog ended with ~d of ~d toolkits: try again, a partial catalog is never served" (length items) total))
             (return-from fetch-catalog
               (values (sort items (lambda (a b) (string-lessp (service-name a) (service-name b)))) total)))
            ((or (equal next cursor) (gethash next cursors) (zerop (length rows)))
             (fail "Composio's catalog paged in a circle at ~a: try again in a minute" next))
            (t
             (setf (gethash next cursors) t
                   cursor next))))))
    (fail "Composio's catalog did not finish within ~d pages" +catalog-pages+)))

(defun catalog ()
  "Every toolkit, from the cache while it is younger than cache_seconds."
  (let ((key (catalog-key))
        (now (get-universal-time)))
    (if (and *catalog-cache*
             (equal key (first *catalog-cache*))
             (< (- now (second *catalog-cache*)) (setting :cache-seconds)))
        (third *catalog-cache*)
        (let ((services (fetch-catalog)))
          (setf *catalog-cache* (list key now services))
          services))))

(defun connected-toolkits ()
  "A set of the toolkit slugs this Composio account already has, from the CLI."
  (let ((set (make-hash-table :test #'equal)))
    (dolist (row (list-connections)) (setf (gethash (account-toolkit row) set) t))
    set))

(defun search-services (query linked limit)
  "The catalog rows matching QUERY -- slug, name, description or category -- kept to
LINKED (any, connected, unconnected) and cut at LIMIT. => (values ROWS MATCHED)."
  (let* ((term (string-downcase (or query "")))
         (wanted (and (member linked '("connected" "unconnected") :test #'equal)
                      (connected-toolkits)))
         (matches
           (remove-if-not
            (lambda (service)
              (and (or (zerop (length term))
                       (search term (string-downcase
                                     (format nil "~a ~a ~a ~{~a~^ ~}"
                                             (service-slug service)
                                             (service-name service)
                                             (service-description service)
                                             (service-categories service)))))
                   (cond ((null wanted) t)
                         ((equal linked "connected") (gethash (service-slug service) wanted))
                         (t (not (gethash (service-slug service) wanted))))))
            (catalog))))
    (values (if (and limit (> (length matches) limit))
                (subseq matches 0 limit)
                matches)
            (length matches))))

(defun format-services (rows matched linked)
  "ROWS as the lines the model reads, and a note when MATCHED overran them."
  (if (null rows)
      (format nil "No Composio toolkit matches (~a).~@[ Try composio_services without the ~a filter.~]"
              (if (equal linked "any") "the catalog" linked)
              (and (not (equal linked "any")) linked))
      (with-output-to-string (out)
        (format out "~d of ~d Composio toolkits~@[ (~a)~]:"
                (length rows) matched
                (and (not (equal linked "any")) linked))
        (dolist (service rows)
          (format out "~% ~a (~a)~@[ -- ~{~a~^, ~}~]~@[ -- no auth~]~a~@[~%    ~a~]"
                  (service-name service)
                  (service-slug service)
                  (remove "" (service-categories service) :test #'equal)
                  (service-no-auth-p service)
                  (if (service-managed-p service) "" " -- needs your own OAuth app")
                  (let ((description (nonblank (service-description service))))
                    (and description (nlk:clip description 200))))))))

;;; --- the verbs tools and /composio call -------------------------------------------

(define-verb accounts (&optional toolkit)
  "The Composio accounts the CLI lists, as text; TOOLKIT when given keeps one slug."
  (let ((rows (list-connections)))
    (when (nonblank toolkit)
      (setf rows (remove-if-not (lambda (row) (equal toolkit (account-toolkit row))) rows)))
    (format-accounts rows)))

(define-verb services (&key query linked limit)
  "Composio's toolkit catalog, searched through the API key. => text."
  (multiple-value-bind (rows matched) (search-services query (or linked "any") limit)
    (format-services rows matched (or linked "any"))))

(define-verb connect (toolkit &optional alias)
  "TOOLKIT's OAuth link from the CLI. => text with the link."
  (connect-service toolkit alias))

(define-verb disconnect (selector &optional confirm)
  "Remove the account SELECTOR names once CONFIRM is true. => text, or a refusal."
  (remove-account selector confirm))

;;; --- a refusal is text the model repairs from -------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (composio-error (condition)
       (nle:failure (format nil "ERROR: ~a" (composio-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Composio: ~a" (nlk:one-line (princ-to-string condition)))))))

;;; --- the four boundaries ----------------------------------------------------------

(nlk:define-contract composio-accounts
  (:doc "The Composio accounts connected on this machine, from the composio CLI: toolkit, alias, Composio status, and the selector composio_disconnect takes. An expired account still counts as added; ACTIVE does not prove every permission was used.")
  ("toolkit" :text
             :doc "only the accounts of this toolkit slug (gmail, github); unset, every account"))

(nlk:define-contract composio-services
  (:doc "Search Composio's public toolkit catalog -- the 1,600 services an account can be connected to -- by name, slug, category or description. Needs the Composio API key; call it before composio_connect to find the slug.")
  ("query" :text
           :doc "words to match against slug, name, category and description")
  ("linked" :string :default "any" :options '("any" "connected" "unconnected")
            :doc "any service, only the ones already connected, or only the ones not yet")
  ("limit" :integer
           :doc "how many rows to answer with; unset, every match"))

(nlk:define-contract composio-connect
  (:doc "Ask Composio for a toolkit's authorization link, which a person opens in a browser to grant access. A toolkit this account already has needs a new alias.")
  ("toolkit" :text :required t
             :doc "the toolkit slug to connect, as composio_services names it")
  ("alias" :text
           :doc "a name for this account, needed for a second account of a toolkit already connected"))

(nlk:define-contract composio-disconnect
  (:doc "Remove one connected Composio account and prove it is gone by reading the list again. The provider's own grant may still stand; only Composio's connection is removed.")
  ("selector" :text :required t
              :doc "the exact selector composio_accounts printed for that one account")
  ("confirm" :boolean :required t
             :doc "true removes it; anything else is refused"))

;;; --- the tools --------------------------------------------------------------------

(defun accounts-tool (call)
  "The composio_accounts tool: CALL's optional toolkit filter."
  (answering
    (accounts (composio-accounts-toolkit call))))

(defun services-tool (call)
  "The composio_services tool: the catalog matched, filtered by whether it is connected."
  (answering
    (services :query (composio-services-query call)
              :linked (composio-services-linked call)
              :limit (composio-services-limit call))))

(defun connect-tool (call)
  "The composio_connect tool: CALL's toolkit through the CLI's link."
  (answering
    (connect-service (composio-connect-toolkit call) (composio-connect-alias call))))

(defun disconnect-tool (call)
  "The composio_disconnect tool: CALL's selector removed once confirmed."
  (answering
    (remove-account (composio-disconnect-selector call) (composio-disconnect-confirm call))))

;;; --- /composio --------------------------------------------------------------------

(defun run-command (args session-id)
  "/composio: the connected accounts; /composio TOOLKIT: the link that connects it."
  (declare (ignore session-id))
  (let ((argument (nonblank (or args ""))))
    (if argument
        (connect-service argument nil)
        (accounts))))

;;; --- the declaration --------------------------------------------------------------

(nle:define-cell composio
  (:section ("composio")
    (:guide "the `composio' CLI on this machine owns the login and the connected accounts -- run `composio login` once if composio_accounts says it is signed out; the toolkit catalog also needs a Composio API key (composio.dev, Settings, API keys) as COMPOSIO_USER_API_KEY or written here as api_key; next step: /composio")
    ("api_key" :secret
     :doc "the Composio API key the toolkit catalog reads with, written here (optional)")
    ("api_key_env" :env
     :doc "the environment variable holding that key; COMPOSIO_USER_API_KEY is read either way")
    ("base_url" :string :default "https://backend.composio.dev"
     :doc "Composio's API root, for the toolkit catalog")
    ("cli" :string :default "composio"
     :doc "the composio CLI to run: its name on PATH, or a path to it")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one CLI call's ceiling, in seconds")
    ("catalog_timeout_seconds" :integer :default 45 :min 1
     :doc "one catalog page's ceiling, in seconds")
    ("cache_seconds" :integer :default 300 :min 0
     :doc "how long a whole catalog read is reused before it is fetched again")
    ("page_limit" :integer :default 1000 :min 1
     :doc "how many toolkits one catalog page asks for"))
  (:settings #'read-settings)
  (:help :composio
         "composio: Composio connected accounts and the toolkit catalog (composio_accounts, composio_services, composio_connect, composio_disconnect)"
         +primer+)
  (:tool "composio_accounts" (nlk:contract 'composio-accounts) #'accounts-tool)
  (:tool "composio_services" (nlk:contract 'composio-services) #'services-tool)
  (:tool "composio_connect" (nlk:contract 'composio-connect) #'connect-tool)
  (:tool "composio_disconnect" (nlk:contract 'composio-disconnect) #'disconnect-tool)
  (:command "composio" 'run-command
            :description "Composio accounts, or the link that connects a toolkit"
            :argument-hint "TOOLKIT"))
