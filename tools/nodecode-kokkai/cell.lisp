;;;; cell.lisp --- the three tools, the /kokkai command and the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model reads the Diet with `kokkai_search', `kokkai_speech' and
;;;; `kokkai_watch'; the operator with /kokkai. WIRE.LISP holds the API and
;;;; the block each answer is; WATCH.LISP holds the saved watches and their
;;;; daily check. The section of the config is `kokkai':
;;;;   "kokkai": {"enabled": true, "hour": 9, "lookback_days": 30,
;;;;              "timeout_seconds": 30, "interval_minutes": 15}
;;;; No key: the API is open.

(in-package #:nodecode-kokkai)

;;; --- the primer ----------------------------------------------------------------

(defparameter +primer+
  "Japan's National Diet proceedings are reachable through the nodecode-kokkai cell: three
tools and one slash command.
  kokkai_search(any, speaker, house, from, until, maximum_records)   A Diet meeting and its
      official meetingURL: the house, the meeting, the session and issue, the date, its speech
      count and up to 30 speaker names. any is space-separated AND, speaker is space-separated
      OR, house is 衆議院 / 参議院 / 両院 / 両院協議会, from and until go together as YYYY-MM-DD.
      maximum_records is 1 to 30 (10 by default). A total larger than the page is an ERROR
      asking to narrow, never 0 meetings.
  kokkai_speech(any, speaker, house, from, until, maximum_records)   Who said what: an excerpt
      (the first 240 characters), the speaker, the meeting, the date and the speechURL.
      At most 10 rows (5 by default). A total larger than the page still answers, with the
      total and a truncation mark.
  kokkai_watch(action, any, speaker, house, lookback_days, watch_id)   add saves one query
      (any or speaker required) and marks what the window already holds as seen; list shows
      the saved ones; run checks now; remove takes one away by watch_id. A saved watch is
      checked once a day at the hour in the config, and says what is new under a notice.
  /kokkai 予算        the same search in the conversation, for the operator.
Doctrine: name a meeting by its own meetingURL, never by the first URL of a page, and copy the
credit line the answer ends with. Quote only from kokkai_speech, and say so when you did not
call it. Speech text is not stored; the excerpt is what came back."
  "What (help :kokkai) answers while the cell runs.")

;;; --- the settings --------------------------------------------------------------

(defun read-settings (values table)
  "The settings the cell runs on: the hour is 0 through 23, which the
declaration's :min cannot say on its own."
  (declare (ignore table))
  (let ((hour (getf values :hour)))
    (unless (and (integerp hour) (<= 0 hour 23))
      (nlk:config-error "hour is ~s; it is 0 through 23" hour))
    (list :hour hour
          :lookback-days (getf values :lookback-days)
          :timeout-seconds (getf values :timeout-seconds)
          :interval-minutes (getf values :interval-minutes)
          :watch (getf values :watch))))

;;; --- the contracts -------------------------------------------------------------

(defparameter +houses+ '("衆議院" "参議院" "両院" "両院協議会")
  "What house may be, as the declaration advertises it.")

(nlk:define-contract kokkai-search-arguments
  (:doc "Search Japan's National Diet proceedings: the meetings that match, each with its official meetingURL, and the credit line. Speech text is not returned; call kokkai_speech for who said what.")
  ("any" :string :default "" :doc "space-separated words that must all appear, in Japanese or English")
  ("speaker" :string :default "" :doc "space-separated names, any of which may match, as the record spells them")
  ("house" :string :default "" :options +houses+ :doc "衆議院, 参議院, 両院 or 両院協議会; empty for any")
  ("from" :string :default "" :doc "start date YYYY-MM-DD; passed together with until")
  ("until" :string :default "" :doc "end date YYYY-MM-DD; passed together with from")
  ("maximum_records" :integer :default 10 :doc "1 to 30 meetings; 10 by default"))

(nlk:define-contract kokkai-speech-arguments
  (:doc "Read what was said in Japan's National Diet: an excerpt of each matching speech, the speaker, the meeting, the date and the speechURL. Speech text is not stored.")
  ("any" :string :default "" :doc "space-separated words that must all appear in the speech")
  ("speaker" :string :default "" :doc "space-separated names, any of which may match")
  ("house" :string :default "" :options +houses+ :doc "衆議院, 参議院, 両院 or 両院協議会; empty for any")
  ("from" :string :default "" :doc "start date YYYY-MM-DD; passed together with until")
  ("until" :string :default "" :doc "end date YYYY-MM-DD; passed together with from")
  ("maximum_records" :integer :default 5 :doc "1 to 10 speeches; 5 by default"))

(nlk:define-contract kokkai-watch-arguments
  (:doc "A saved Diet query, checked once a day: add one with any or speaker, list what is saved, run one now, or remove one by its watch_id.")
  ("action" :string :required t :options '("add" "list" "run" "remove") :doc "add, list, run or remove")
  ("any" :string :default "" :doc "for add: space-separated words that must all appear")
  ("speaker" :string :default "" :doc "for add: space-separated names, any of which may match")
  ("house" :string :default "" :options +houses+ :doc "for add: 衆議院, 参議院, 両院 or 両院協議会")
  ("lookback_days" :integer :default 30 :doc "for add: 1 to 30 days back from today in Tokyo")
  ("watch_id" :string :default "" :doc "for run and remove: the id list shows"))

;;; --- the tools -----------------------------------------------------------------

(defun search-tool (arguments)
  "The `kokkai_search' tool: the meeting block, or a line saying why there
is none."
  (handler-case
      (meetings (kokkai-search-arguments-any arguments)
                (kokkai-search-arguments-speaker arguments)
                (kokkai-search-arguments-house arguments)
                (kokkai-search-arguments-from arguments)
                (kokkai-search-arguments-until arguments)
                (kokkai-search-arguments-maximum-records arguments))
    (kokkai-error (condition) (format nil "ERROR: ~a" condition))))

(defun speech-tool (arguments)
  "The `kokkai_speech' tool: the excerpts, or a line saying why there are
none."
  (handler-case
      (speeches (kokkai-speech-arguments-any arguments)
                (kokkai-speech-arguments-speaker arguments)
                (kokkai-speech-arguments-house arguments)
                (kokkai-speech-arguments-from arguments)
                (kokkai-speech-arguments-until arguments)
                (kokkai-speech-arguments-maximum-records arguments))
    (kokkai-error (condition) (format nil "ERROR: ~a" condition))))

(defun watch-tool (arguments)
  "The `kokkai_watch' tool: one action, one answer."
  (handler-case
      (let ((action (kokkai-watch-arguments-action arguments)))
        (cond ((string= action "add")
               (add-watch (kokkai-watch-arguments-any arguments)
                          (kokkai-watch-arguments-speaker arguments)
                          (kokkai-watch-arguments-house arguments)
                          (kokkai-watch-arguments-lookback-days arguments)))
              ((string= action "list") (list-watches))
              ((string= action "run")
               (multiple-value-bind (text news)
                   (run-watches :id (kokkai-watch-arguments-watch-id arguments))
                 (declare (ignore news))
                 (format nil "~a~%~%(A manual run does not mark what it saw as seen; the daily check does.)" text)))
              ((string= action "remove")
               (remove-watch (kokkai-watch-arguments-watch-id arguments)))
              (t (format nil "ERROR: ~s is not an action; it is add, list, run or remove" action))))
    (kokkai-error (condition) (format nil "ERROR: ~a" condition))))

;;; --- the command ---------------------------------------------------------------

(defun run-kokkai (args session-id)
  "The /kokkai command: the same search in the conversation. Bare, it asks
for the words instead of searching nothing."
  (declare (ignore session-id))
  (let ((args (nlk:trimmed (or args ""))))
    (if (zerop (length args))
        (values "Japan Diet meetings and speeches"
                (nle:list-dialog
                 :title "Kokkai"
                 :summary "search Japan's National Diet proceedings by word"
                 :rows (list (nle:ask-row "Search the Diet" "Words" "/kokkai"
                                           :placeholder "予算, 憲法, or a member's name"
                                           :verb "search"))))
        (handler-case (meetings args "" "" "" "" 10)
          (kokkai-error (condition) (format nil "~a" condition))))))

;;; --- the declaration ------------------------------------------------------------

(nle:define-cell kokkai
  (:section ("kokkai")
    (:guide "no key is needed; hour is when the daily watch checks; lookback_days bounds a watch with no lookback of its own")
    ("hour" :integer :default 9 :min 0
            :doc "the hour the saved watches are checked, on this machine's clock")
    ("lookback_days" :integer :default 30 :min 1
                     :doc "the days back a saved watch reads, at most 30")
    ("timeout_seconds" :integer :default 30 :min 1
                       :doc "how long one request may wait for kokkai.ndl.go.jp")
    ("interval_minutes" :integer :default 15 :min 1
                        :doc "how often the worker looks to see whether the watch hour has come")
    ("watch" :boolean :default t
             :doc "whether the daily check runs at all"))
  (:settings 'read-settings)
  (:tool "kokkai_search" (nlk:contract 'kokkai-search-arguments) 'search-tool)
  (:tool "kokkai_speech" (nlk:contract 'kokkai-speech-arguments) 'speech-tool)
  (:tool "kokkai_watch" (nlk:contract 'kokkai-watch-arguments) 'watch-tool)
  (:command "kokkai" 'run-kokkai
            :description "Search Japan's National Diet meetings"
            :argument-hint "WORDS")
  (:help :kokkai "kokkai: Japan Diet meetings, speech excerpts, and a daily watch" +primer+)
  (:start (lambda ()
            (setf *state* nil *last-done* nil)
            (nle:on-stop (lambda () (setf *state* nil)))
            (when (setting :watch)
              (let ((worker (nlk:worker-start "kokkai-watch" #'watch-step
                                              :wake (lambda () (* 60 (setting :interval-minutes))))))
                (nle:on-stop (lambda () (nlk:worker-stop worker))))))))
