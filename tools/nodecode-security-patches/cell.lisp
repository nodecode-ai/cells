;;;; cell.lisp --- the declaration: config, help, /security, the refresh worker.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-security-patches)

(nlk:define-peripheral security-patches :not-running t)

(defvar *worker* nil "The feed refresher, while the cell runs.")

(defparameter +manual+
  "security: the last two weeks' breaches and the fix each vendor shipped,
from a feed the nodecode team keeps current (it refreshes itself; an
installed copy never goes stale).

(security:report :dir \"/var/www/site\" :host \"example.com\")
    checks THIS machine against every advisory and says what to patch first.
    :dir is the site's folder (WordPress root, Node/PHP project) and :host its
    public name; both optional -- checks that need one are skipped without it.
(security:check ...)        the same as plists, for code.
(security:show \"php2shell\") one advisory: affected, fix, stopgap, signs of compromise, sources.
(security:advisories)       every advisory (hash tables; (security:field a \"id\")).
(security:refresh)          re-read the live feed now. /security does all of this too.

Patching a site for someone:
1. Ask where the site lives (its folder, and its host name), run security:report there.
   A site on another server: run the same detect commands over ssh, or ask them to
   install nodecode there.
2. For each PATCH line, read (security:show ID) and say what you will run before you
   run it: package upgrades, a restart, a config line. Back up config files first.
   Upgrades need sudo and restart services: get a yes for each, never on your own.
3. If the fix cannot land now, apply the advisory's stopgap and say it is partial.
4. Re-run security:report; a hit that remains is not done. For the supply-chain and
   platform entries (LiteLLM, Railway, OpenCode, Zeabur...) the patch is rotating
   secrets: list which, the person does it.
5. Signs of compromise listed under an advisory mean: stop and say so before patching
   -- patching does not remove a backdoor.
Never disable TLS verification, a WAF or a security check to make an error go away.")

(defun list-text ()
  (with-output-to-string (s)
    (format s "~a~%~%" (feed-status))
    (dolist (a (advisories))
      (format s "~a  ~a ~a~:[~; (exploited)~] -- ~a~%" (field a "date")
              (string-upcase (field a "severity" "")) (field a "id")
              (eq (field a "exploited") t) (field a "title")))
    (format s "~%/security check [FOLDER] [HOST] checks this machine; /security show ID gives the fix.~%")))

(defun run-slash (args session-id)
  "One /security invocation => its text."
  (declare (ignore session-id))
  (let* ((words (nlk:split-words (or args "")))
         (head (first words)))
    (handler-case
        (cond ((null head) (list-text))
              ((string-equal head "check")
               (let ((dir (find-if (lambda (w) (find #\/ w)) (rest words)))
                     (host (find-if (lambda (w) (and (find #\. w) (not (find #\/ w)))) (rest words))))
                 (report :dir dir :host host)))
              ((string-equal head "show") (show (or (second words) "")))
              ((string-equal head "refresh") (progn (refresh) (feed-status)))
              (t (format nil "security: unknown subcommand ~a; /security, /security check [FOLDER] [HOST], /security show ID, /security refresh" head)))
      (error (e) (format nil "security: ~a" e)))))

(defun step-refresh ()
  "The worker's step: re-read the live feed."
  (refresh)
  nil)

(nle:define-cell security-patches
  (:section ("security-patches")
    (:guide "nothing is required: the advisory feed is read from the cells repository and refreshed on its own; feed_url points at a mirror, refresh_hours sets how often")
    ("feed_url" :string :default *feed-url* :doc "where the advisory feed is read from")
    ("refresh_hours" :integer :default 6 :min 1 :doc "how often the feed is re-read"))
  (:help :security "security: the last two weeks' breaches and their patches - (security:report :dir \"/var/www/site\" :host \"example.com\")" +manual+)
  (:command "security" 'run-slash
            :description "Recent breaches and the patch for each; check this machine"
            :argument-hint "[check FOLDER HOST | show ID | refresh]")
  (:start (lambda ()
            (setf *feed-url* (setting :feed-url))
            (load-feed :fetch nil)
            (setf *worker* (nlk:worker-start "security-patches" 'step-refresh
                                             :wake (* 3600 (setting :refresh-hours)) :prime t))
            (nle:on-stop (lambda () (setf *worker* (nlk:worker-stop *worker*)))))))
