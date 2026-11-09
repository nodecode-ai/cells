;;;; cell.lisp --- the section, the seven tools, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The cell talks to Yandex Calendar's CalDAV server as the account owner,
;;;; with an app password. Which of the seven verbs exist is decided at start
;;;; from `actions', the way the Hermes plugin decided it at registration:
;;;; an action that is not allowed is not merely refused, the model never
;;;; sees the tool. So the tools are registered in INSTALL rather than as
;;;; :TOOL clauses, and each registration's inverse goes on the same scope.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "yandex_calendar": {"login": "you@yandex.ru", "app_password": "...",
;;;;                        "base_url": "https://caldav.yandex.ru",
;;;;                        "calendars": ["Home"], "actions": ["read"]}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-yandex-calendar)

(nlk:define-peripheral yandex-calendar :not-running t)

(defparameter +primer+
  "Yandex Calendar is reachable through this cell's seven tools, each answering JSON
with an \"error\" member when the call fails: yandex_calendar_list_calendars,
yandex_calendar_list_events, yandex_calendar_create_event, yandex_calendar_update_event,
yandex_calendar_respond_event, yandex_calendar_move_event, yandex_calendar_delete_event.
List first: every event comes back with an href, and update, respond, move and delete
take that href. A recurring event is listed once, as stored, with its first occurrence's
time; a series whose occurrences were edited cannot be updated, only moved or deleted.
Times are ISO 8601; one without an offset is read as UTC. The settings live in the
yandex_calendar section: login, app_password (create one at id.yandex.ru), base_url,
calendars (an ordered allow-list, the first being the default) and actions (all, read,
write, delete, or single verb names) which decides which tools exist."
  "What (help :yandex-calendar) answers while the cell runs.")

(defparameter +actions+
  '(:list-calendars :list-events :create-event :update-event :respond-event
    :move-event :delete-event)
  "Every action the cell can expose, in the order the tools are registered.")

(defparameter +action-groups+
  '(("all" . (:list-calendars :list-events :create-event :update-event :respond-event
               :move-event :delete-event))
    ("read" . (:list-calendars :list-events))
    ("write" . (:create-event :update-event :respond-event :move-event))
    ("delete" . (:delete-event)))
  "The shorthands `actions' accepts beside a single verb's name.")

(defparameter *tool-table*
  '((:list-calendars "yandex_calendar_list_calendars" list-calendars-arguments
     list-calendars)
    (:list-events "yandex_calendar_list_events" list-events-arguments list-events)
    (:create-event "yandex_calendar_create_event" create-event-arguments create-event)
    (:update-event "yandex_calendar_update_event" update-event-arguments update-event)
    (:respond-event "yandex_calendar_respond_event" respond-event-arguments respond-event)
    (:move-event "yandex_calendar_move_event" move-event-arguments move-event)
    (:delete-event "yandex_calendar_delete_event" delete-event-arguments delete-event))
  "One entry per verb: the action name, the tool name the model sees, its
arguments declaration, and the function that answers the call.")

(defun action-name (entry)
  "The ACTION an `actions' ENTRY names, or NIL for one that names nothing.
A misspelled entry can only ever withhold a tool, never grant one."
  (let* ((text (string-downcase (string-trim '(#\Space #\Tab) (or entry ""))))
         (text (substitute #\- #\_ text))
         (text (if (uiop:string-prefix-p "yandex-calendar-" text)
                   (subseq text (length "yandex-calendar-"))
                   text)))
    (or (cdr (assoc text +action-groups+ :test #'string=))
        (let ((action (intern (string-upcase text) :keyword)))
          (and (member action +actions+) action)))))

(defun parse-actions (entries)
  "The actions ENTRIES allow; empty or unset means every one of them."
  (if (null entries)
      +actions+
      (let ((allowed '()))
        (dolist (entry entries)
          (let ((action (action-name entry)))
            (cond ((listp action) (setf allowed (union allowed action)))
                  (action (pushnew action allowed)))))
        allowed)))

(defun read-settings (values table)
  "The typed values the cell runs on: the declaration's own, plus what it cannot say.
`actions' is a list of tool names, and becomes the list of actions they allow;
`base_url' must be an HTTPS origin before a credential ever reaches it."
  (declare (ignore table))
  (let ((base-url (string-right-trim "/" (or (getf values :base-url) *default-base-url*))))
    (handler-case (https-origin base-url :what "base_url")
      (caldav-error (condition)
        (error 'nlk:config-refusal :detail (princ-to-string condition))))
    (list :login (or (getf values :login) "")
          :app-password (or (getf values :app-password) "")
          :base-url base-url
          :calendars (getf values :calendars)
          :actions (parse-actions (getf values :actions)))))

(defun install ()
  "Register the tools the section's actions allow, each with its inverse."
  (let ((allowed (setting :actions)))
    (dolist (entry *tool-table*)
      (destructuring-bind (action name declaration function) entry
        (when (member action allowed)
          (let ((tool name))
            (nle:register-tool tool (nlk:contract declaration) function)
            (nle:on-stop (lambda () (nle:unregister-tool tool)))))))))

(nle:define-cell yandex-calendar
  (:section ("yandex_calendar")
    (:guide "create an app password at id.yandex.ru/security/app-passwords and put it in app_password; login is the account address; calendars names the ones this cell may use, in the order they are offered, and the first is the default; actions decides which tools exist: all, read, write, delete, or single verb names")
    ("login" :string :required t :doc "the Yandex account's address, e.g. you@yandex.ru")
    ("app_password" :secret :required t
     :doc "a Yandex app password for CalDAV, from id.yandex.ru/security/app-passwords")
    ("base_url" :string :default "https://caldav.yandex.ru"
     :doc "the CalDAV server; only an HTTPS origin is accepted, and an event on
another one is refused rather than dialled")
    ("calendars" :list
     :doc "an allow-list of calendars by name or href segment, the first being the
default; empty means every calendar the account holds")
    ("actions" :list
     :doc "which verbs to offer: all, read, write, delete, or single verb names
such as create_event; empty means all of them"))
  (:settings #'read-settings)
  (:start #'install)
  (:help :yandex-calendar
         "Yandex Calendar over CalDAV: list, create, update, answer, move and delete events"
         +primer+))
