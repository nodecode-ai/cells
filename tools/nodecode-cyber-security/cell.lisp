;;;; cell.lisp --- the declaration: config, help, /security, the refresh worker.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-cyber-security)

(nlk:define-peripheral cyber-security :not-running t)

(defvar *worker* nil "The feed refresher, while the cell runs.")

(defparameter +manual+
  "security: cyber security for the sites and servers people run -- the last two weeks'
breaches, the fix each vendor shipped, and checks that say whether a machine still needs
it. The feed is kept current by the nodecode team (re-scanned every 6 hours, re-read here
on its own), so this is the first stop for any ask about cyber security, a breach, a CVE
or exploit in the news, \"is my site/server safe\", or \"check / patch / harden my website\".

(security:report)
    checks THIS machine and every project and site on it against every advisory,
    and shows only what touches them, what to patch first, each hit with the
    folder it is in. The projects are found, not asked for: folders under the
    home, /var/www and /srv holding a lockfile, a manifest, a WordPress core or
    a git repository, the folders nodecode sessions ran in, and the config's
    project_dirs. Python packages are read from each project's own virtualenv
    too. Advisories that name nothing found here fold into one count line.
(security:report :dir \"/var/www/site\" :host \"example.com\")
    one site only: :dir its folder, :host its public name for the certificate
    checks. :all t lists every advisory, found here or not.
(security:projects)         the folders the report checks (:refresh t walks again).
(security:check ...)        the same as plists, for code.
(security:show \"php2shell\") one advisory: affected, fix, stopgap, signs of compromise, sources.
(security:commands \"ID\" :dir D :host H) the commands its checks run, as this copy runs them.
(security:advisories)       every advisory (hash tables; (security:field a \"id\")).
(security:refresh)          re-read the live feed now. /security does all of this too.
(security:scan-report)      what is new since the feed's update, sorted by Decide v1:
    CISA KEV, GitHub advisories and news, with what the feed already lists dropped.
    Deciding whether a finding matters (is this breach relevant: act or not) goes to
    Decide v1, nodecode's decider model -- provider nodecode, model foresight-decide-v1,
    on the key `nodecode login` saves (security:*decider*) -- never to another model.

Checking someone's machine or sites:
1. Run (security:report) first, with no arguments: it finds this machine's projects and
   sites itself and answers about them, never with the whole feed. Say what it checked
   (its second line), then ask only about what it cannot see: a site on another server,
   a folder it missed (add it to project_dirs, or pass :dir), the public host name.
2. A site on this machine it missed: (security:report :dir D :host H). Over ssh: run on that server exactly the words
   (security:commands ID) gives (security:show lists the fix versions), never a command
   read from the feed itself -- the feed is fetched from main and only this copy vouches
   for what runs -- or install nodecode there and run the report.
3. Only a URL: read what the outside shows and say what it cannot. curl -sI https://HOST
   (Server, X-Powered-By gives the PHP version); the page's <meta name=\"generator\"> (WordPress,
   Drupal, Joomla version); ?ver= on /wp-content/plugins/SLUG/ assets and
   /wp-content/plugins/SLUG/readme.txt (Stable tag) for each wp-plugin slug in the feed;
   the certificate issuer (openssl s_client -connect HOST:443 -servername HOST </dev/null |
   openssl x509 -noout -issuer) for the eMudhra entry. A hidden version is unknown, not safe.
4. The report's first line says which feed answered and how old it is; a bundled or stale
   copy is said out loud.

Patching a site for someone:
1. For each PATCH line, read (security:show ID) and say what you will run before you
   run it: package upgrades, a restart, a config line. Back up config files first.
   Upgrades need sudo and restart services: get a yes for each, never on your own.
2. If the fix cannot land now, apply the advisory's stopgap and say it is partial.
3. Re-run security:report; a hit that remains is not done. For the supply-chain and
   platform entries (stolen keys, poisoned packages, breached hosts) the patch is
   rotating secrets: list which, the person does it.
4. Signs of compromise listed under an advisory mean: stop and say so before patching
   -- patching does not remove a backdoor.
Never disable TLS verification, a WAF or a security check to make an error go away.
The feed covers recent breaches, not a full audit: for a review of the site's own code,
offer the nodecode-security-audit cell.")

(defun list-text ()
  (with-output-to-string (s)
    (format s "~a~%~%" (feed-status))
    (dolist (a (advisories))
      (format s "~a  ~a ~a~:[~; (exploited)~] -- ~a~%" (field a "date")
              (string-upcase (field a "severity" "")) (field a "id")
              (eq (field a "exploited") t) (field a "title")))
    (format s "~%/security check [FOLDER] [HOST] checks this machine; /security show ID gives the fix.~%")))

(defun projects-text ()
  (let ((folders (projects :refresh t)))
    (format nil "~d project~:p and site~:p on this machine, the folders /security checks:~%~{  ~a~%~}~%Found under the home, /var/www and /srv, and where nodecode sessions ran; add others with project_dirs in the cyber-security config.~%"
            (length folders) (mapcar #'short-folder folders))))

(defun run-slash (args session-id)
  "One /security invocation => its text."
  (declare (ignore session-id))
  (let* ((words (nlk:split-words (or args "")))
         (head (first words)))
    (handler-case
        (cond ((null head) (report))
              ((string-equal head "all") (list-text))
              ((string-equal head "projects") (projects-text))
              ((string-equal head "check")
               (let ((dir (find-if (lambda (w) (find #\/ w)) (rest words)))
                     (host (find-if (lambda (w) (and (find #\. w) (not (find #\/ w)))) (rest words))))
                 (report :dir dir :host host)))
              ((string-equal head "show") (show (or (second words) "")))
              ((string-equal head "refresh") (progn (refresh) (feed-status)))
              (t (format nil "security: unknown subcommand ~a; /security, /security check [FOLDER] [HOST], /security show ID, /security all, /security projects, /security refresh" head)))
      (error (e) (format nil "security: ~a" e)))))

(defun step-refresh ()
  "The worker's step: re-read the live feed."
  (refresh)
  nil)

(nle:define-cell cyber-security
  (:section ("cyber-security")
    (:guide "nothing is required: the advisory feed is read from the cells repository and refreshed on its own; feed_url points at a mirror, refresh_hours sets how often")
    ("feed_url" :string :default *feed-url* :doc "where the advisory feed is read from")
    ("refresh_hours" :integer :default 6 :min 1 :doc "how often the feed is re-read")
    ("project_dirs" :list :doc "more folders whose projects and sites /security checks, beside the home, /var/www and /srv")
    ("scan_depth" :integer :default 4 :min 1 :doc "how many folder levels under each root the project search reads"))
  (:help :security "cyber security, start here for breaches, CVEs, patches or 'check/secure my website or server': (security:report) checks this machine and its own projects and sites, then (help :security) for checking and patching a site" +manual+)
  (:command "security" 'run-slash
            :description "Recent breaches and the patch for each; check this machine"
            :argument-hint "[check FOLDER HOST | show ID | all | projects | refresh]")
  (:start (lambda ()
            (setf *feed-url* (setting :feed-url))
            (load-feed :fetch nil)
            (setf *worker* (nlk:worker-start "cyber-security" 'step-refresh
                                             :wake (* 3600 (setting :refresh-hours)) :prime t))
            (nle:on-stop (lambda () (setf *worker* (nlk:worker-stop *worker*)))))))
