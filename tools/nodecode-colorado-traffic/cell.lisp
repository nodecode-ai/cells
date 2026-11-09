;;;; cell.lisp --- the verb, the tool, /traffic, and the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of drkpxl/colorado-traffic, a Hermes Agent skill whose whole
;;;; program was one Python script the agent ran through its terminal. A cell
;;;; runs in the image the model already talks to, so the same query is one
;;;; Lisp verb: (cotrip:events) asks CDOT and answers the text, the
;;;; cotrip_events tool is that verb for a model that would rather call it
;;;; than write Lisp, and /traffic is it for the operator.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "colorado-traffic": {"corridor": "i70-golden-denver",
;;;;                         "layers": [], "max_items": 12, "timeout_seconds": 40}

(in-package #:nodecode-colorado-traffic)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Live Colorado highway events come from CDOT's public COtrip feed, through
the nodecode-colorado-traffic cell: one Lisp function, called through eval.
  (cotrip:events &key corridor route min-mp max-mp layers include-ended)
      The events on one corridor, as text: incidents, closures, scheduled work and
      Waze reports, each tagged [now], [upcoming] or [ended], with direction, mile
      points, the schedule in US Mountain time and the COtrip link.
  corridors (the default is i70-golden-denver, I-70 MP 259-279, C-470 at Golden
  through Quebec Street in east Denver):
      i70-golden-denver   I-70 Golden to east Denver, MP 259-279
      i70-golden-i25      I-70 Golden to I-25, MP 259-274.5
  route, min-mp and max-mp ask about another stretch instead: (cotrip:events
  :route \"I-25\" :min-mp 210 :max-mp 216). layers names what to fetch, from
  incidents, closures, work, waze, winter, chains; include-ended keeps events
  whose schedule is over. The cotrip_events tool is the same query.
No key, no account, one host: api-511x-co.carsprogram.org.
Three rules when you answer from it. CDOT sets active:true on future night work,
so trust the [now]/[upcoming] tag and never the flag. Waze reports are CDOT's
republication of unverified crowd reports: say \"Waze via COtrip\", not \"CDOT
confirmed\". Direction comes from the route's positive bearing -- positive is
eastbound on I-70 and northbound on I-25 -- so never call every positive
direction eastbound. COtrip publishes no speeds: say travel time is unavailable
rather than guessing minutes from a jam report. Floyd Hill, the Eisenhower
Tunnel and mountain-weekend forecasts are west of this corridor; do not mix them
into a Golden-to-Denver answer unless asked."
  "What (help :colorado-traffic) answers while the cell runs.")

;;; --- the verb -------------------------------------------------------------------

(defun layer-names (value)
  "VALUE, a comma string or a list of layer names, or NIL."
  (let ((items (cond ((null value) nil)
                     ((stringp value) (cl-ppcre:split "," value))
                     ((vectorp value) (coerce value 'list))
                     ((listp value) value))))
    (setf items (remove "" (mapcar (lambda (item) (nlk:trimmed (princ-to-string item))) items)
                        :test #'string=))
    (and items (mapcar #'string-downcase items))))

(defun spec-label (spec overridden)
  "The heading SPEC's answer opens with."
  ;; The shipped label while the corridor is the shipped box, and the box
  ;; said out loud once the caller moves it.
  (if overridden
      (format nil "~a MP ~,1f-~,1f" (getf spec :route) (getf spec :min) (getf spec :max))
      (getf spec :label)))

(define-verb events (&key corridor route min-mp max-mp layers (include-ended nil))
  "The COtrip events on one corridor, and the heading that says which."
  (let* ((named (and (stringp corridor) (plusp (length (nlk:trimmed corridor)))
                      (nlk:trimmed corridor)))
         (shipped (corridor-spec (or named (setting :corridor))))
         (asked-route (and (stringp route) (plusp (length (nlk:trimmed route)))
                           (nlk:trimmed route)))
         (spec (list :route (or asked-route (getf shipped :route))
                     :min (or min-mp (getf shipped :min))
                     :max (or max-mp (getf shipped :max))
                     :label (getf shipped :label)))
         (names (or (layer-names layers) (layer-names (setting :layers)) +default-layers+))
         (now (now-ms)))
    (render (list* :label (spec-label spec (or asked-route min-mp max-mp))
                   spec)
            (collect spec names :include-ended include-ended :now now)
            now)))

;;; --- the tool -------------------------------------------------------------------

(nlk:define-contract cotrip-events
  (:doc "Live Colorado highway events from CDOT's public COtrip feed: incidents, closures, scheduled work and Waze reports on one Colorado corridor, each tagged [now], [upcoming] or [ended] with its direction, mile points, schedule and COtrip link. No key. The default corridor is I-70 from Golden through east Denver; pass route, min_mp and max_mp for another stretch. CDOT's active flag is true for future scheduled work, so answer from the tag, not the flag, and call Waze reports \"Waze via COtrip\", never CDOT-confirmed.")
  ("corridor" :string
              :doc "the corridor to ask about: i70-golden-denver (the default, I-70 MP 259-279, Golden through Quebec Street) or i70-golden-i25 (I-70 MP 259-274.5)")
  ("route" :string
           :doc "a route designator to override the corridor's, such as I-25 or US 6")
  ("min_mp" :number
            :doc "the low end of the mile-point box, with max_mp, to ask about another stretch")
  ("max_mp" :number
            :doc "the high end of the mile-point box, with min_mp")
  ("layers" :string
            :doc "comma list of what to fetch: incidents, closures, work, waze, winter, chains. Empty asks for incidents, closures, work and waze")
  ("include_ended" :boolean
                   :doc "keep the events whose schedule is already over"))

(defun tool-events (args)
  "The cotrip_events tool: one corridor query, its answer as text."
  (events :corridor (cotrip-events-corridor args)
          :route (cotrip-events-route args)
          :min-mp (cotrip-events-min-mp args)
          :max-mp (cotrip-events-max-mp args)
          :layers (cotrip-events-layers args)
          :include-ended (cotrip-events-include-ended args)))

;;; --- /traffic -------------------------------------------------------------------

(defun number-word (word)
  "WORD as a mile point, or a refusal."
  (or (ignore-errors
        (let ((*read-eval* nil))
          (multiple-value-bind (value end) (read-from-string word)
            (and (realp value) (= end (length word)) (coerce value 'double-float)))))
      (fail "~s is not a mile point" word)))

(defun run-slash (args session-id)
  "The answer to one /traffic invocation."
  (declare (ignore session-id))
  (let ((words (nlk:split-words (nlk:trimmed (or args "")))))
    (handler-case
        (cond ((null words) (events))
              ((assoc (string-downcase (first words)) +corridors+ :test #'string=)
               (events :corridor (string-downcase (first words))))
              ((null (cdr words)) (events :corridor (string-downcase (first words))))
              ((= 3 (length words))
               (events :route (first words)
                       :min-mp (number-word (second words))
                       :max-mp (number-word (third words))))
              ((= 2 (length words))
               (events :route (first words)))
              (t (format nil "usage: /traffic [~{~a~^ | ~}] or /traffic ROUTE MIN-MP MAX-MP"
                         (mapcar #'car +corridors+))))
      (colorado-traffic-error (condition) (princ-to-string condition)))))

;;; --- the entry ------------------------------------------------------------------

(nle:define-cell colorado-traffic
  (:section ("colorado-traffic")
    (:guide "no key and nothing is required: corridor picks the stretch a bare /traffic asks about; layers and max_items bound what one answer lists; timeout_seconds bounds one COtrip query")
    ("corridor" :choice :options '("i70-golden-denver" "i70-golden-i25")
                :default "i70-golden-denver"
                :doc "the stretch a query asks about when the caller names none")
    ("layers" :list
              :doc "what a query fetches by default: incidents, closures, work, waze, winter, chains; empty takes incidents, closures, work and waze")
    ("max_items" :integer :default 12 :min 1
                 :doc "events listed per layer")
    ("timeout_seconds" :integer :default 40 :min 1
                        :doc "one COtrip query's ceiling, in seconds"))
  (:tool "cotrip_events" (nlk:contract 'cotrip-events) 'tool-events)
  (:help :colorado-traffic
         "cotrip:events answers live Colorado highway events from CDOT COtrip, by corridor: now, upcoming and Waze"
         +primer+)
  (:command "traffic" #'run-slash
            :description "Live Colorado highway events from CDOT COtrip"))
