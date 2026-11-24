;;;; cell.lisp --- the section, read_settings, and the one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Config, a sibling top-level key next to `websearch':
;;;;   "caldav": {"url": "http://127.0.0.1:5232/user/calendar/",
;;;;              "user": "...", "password": "...",
;;;;              "password_env": "CALDAV_PASSWORD",
;;;;              "days": 7, "max_events": 50, "timeout_seconds": 20}
;;;; The original read three environment variables, CALDAV_URL, CALDAV_USER
;;;; and CALDAV_PASSWORD; the first two are typed into the section here, and
;;;; the password is pasted or named by a variable. A section that names no
;;;; collection, no user or no password refuses the start and says which, so
;;;; a tool is never offered on a calendar the cell cannot read.

(in-package #:nodecode-caldav)

(defun validate-url (url)
  "True when URL is one http or https collection with a host and a non-empty
path, and nothing else: no user name, query or fragment, no surrounding
space, as the original refused them before a connection."
  (let ((text (nonblank url)))
    (and text
         (string= text (nlk:trimmed text))
         (notany (lambda (character) (member character '(#\Space #\Tab #\Newline #\Return #\Page)))
                 text)
         (null (find #\? text))
         (null (find #\# text))
         (let* ((rest (cond ((uiop:string-prefix-p "https://" text) (subseq text 8))
                             ((uiop:string-prefix-p "http://" text) (subseq text 7))
                             (t nil)))
                (slash (and rest (position #\/ rest)))
                (authority (and slash (subseq rest 0 slash)))
                (path (and slash (subseq rest slash))))
           (and rest
                (plusp (length authority))
                (plusp (length path))
                (null (find #\@ authority))
                (let* ((colon (position #\: authority :from-end t))
                       (host (if colon (subseq authority 0 colon) authority))
                       (port (and colon (subseq authority (1+ colon)))))
                  (and (plusp (length host))
                       (or (null port)
                           (and (plusp (length port)) (every #'digit-char-p port))))))))))

(defun read-settings (values table)
  "The declared VALUES with the password resolved here, once: the inline
secret, else the variable password_env names, else a refusal. The collection
and the user are refused here too, so the sentence names what is missing and
a start on a half-filled section leaves nothing installed."
  (declare (ignore table))
  (let* ((url (nonblank (getf values :url)))
         (user (nonblank (getf values :user)))
         (inline (nonblank (getf values :password)))
         (env (nonblank (getf values :password-env)))
         (plist (copy-list values)))
    (unless url
      (nlk:config-error "no collection: set url to the http or https URL of one CalDAV collection, such as http://127.0.0.1:5232/user/calendar/"))
    (unless (validate-url url)
      (nlk:config-error "url is ~s: it must be http or https with a host and a non-empty path, and carry no user name, query or fragment" url))
    (unless user
      (nlk:config-error "no user: set user to the name the calendar server knows"))
    (setf (getf plist :url) url
          (getf plist :user) user
          (getf plist :password)
          (or inline
              (and env (nonblank (nle::credential-env env)))
              (nlk:config-error "no password: paste the calendar password into password, or name the variable holding it in password_env~@[ (~a is unset or empty)~]" env)))
    plist))

(nle:define-cell caldav
  (:section ("caldav")
    (:guide "put the collection's own URL in url, the account name in user, and the password in password or name the variable holding it in password_env; a CalDAV server such as Nextcloud, Radicale or Baikal must already be running; days is the window a call with no arguments reads, and max_events how many rows one answer shows")
    ("url" :string :required t
           :doc "http or https URL of one CalDAV collection, e.g. http://127.0.0.1:5232/user/calendar/")
    ("user" :string :required t
            :doc "the account name sent as HTTP Basic on every request")
    ("password" :secret
                :doc "the password sent as HTTP Basic; left empty, the variable password_env names is read")
    ("password_env" :env :default "CALDAV_PASSWORD"
                    :doc "the environment variable holding the password, read when password is empty")
    ("days" :integer :default 7 :min 1
            :doc "how many days a list_events call with no arguments reads")
    ("max_events" :integer :default 50 :min 1
                  :doc "how many events one answer shows before it says the list is cut (the original's 50)")
    ("timeout_seconds" :integer :default 20 :min 1
                       :doc "seconds one request may take"))
  (:settings #'read-settings)
  (:tool "list_events" (nlk:contract 'list-events-request) #'list-events)
  (:tool "read_event" (nlk:contract 'read-event-request) #'read-event)
  (:command "caldav" 'run-slash
            :catches 'caldav-error
            :description "Read your own CalDAV calendar in the next days"
            :argument-hint "[DAYS|today|tomorrow|week]")
  (:help :caldav
   "caldav: list and read the events of your own CalDAV calendar, read-only - the list_events and read_event tools"
   *primer*))
