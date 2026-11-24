;;;; watch.lisp --- what a watch is, the registry, and the file it survives in.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A watch is a keyword plist, not a record: it is written to a JSON file
;;;; and read back, and NLK:PLIST-JSON / NLK:JSON-PLIST already carry a
;;;; plist of scalars, strings and vectors both ways. The fields are all
;;;; there is to know:
;;;;
;;;;   :id :title :note :kind :every :path :url :command :fire-on
;;;;   :session :next-at :last-value :last-ok :last-state :fires
;;;;   :max-fires :paused :created-at
;;;;
;;;; :kind is "every" or "after" (a wake on a clock) or "file", "url" or
;;;; "command" (a probe, checked out of band and free until it fires).
;;;; :every is seconds. :command is a vector of strings, run without a
;;;; shell. :session is the session the watch wakes: the one that armed it.

(in-package #:nodecode-heartbeat)

(defvar *watches* '()
  "The standing watches, newest first. Read and written under WITH-HEARTBEAT-LOCK.")

(defvar *worker* nil
  "The ticker thread, or NIL while the cell runs without one (a test).")

(defvar *tick-p* t
  "Whether START-CELL starts the ticker. A test binds it to NIL and drives TICK by hand.")

(defvar *save-p* t
  "Whether the registry is written to its state file. A test binds it to NIL.")

(defvar *http* #'nlk:http
  "The cell's one network call, NLK:HTTP. A test binds it to a stub: the test run has no network.")

(defparameter +state-file+ "heartbeat/watches.json"
  "The registry's file under the home: what outlives a restart.")

(defparameter +kinds+ '("every" "after" "file" "url" "command")
  "The five triggers: a clock, a one-shot, or a probe.")

(defparameter +probe-kinds+ '("file" "url" "command")
  "The kinds that cost no model turn until they fire.")

(defparameter +fire-on+ '("change" "failure" "success")
  "What a probe speaks up about: anything new, a break, or a recovery.")

;;; --- the clock ---------------------------------------------------------------

(defun parse-duration (text)
  "Seconds the duration TEXT names -- \"30s\", \"10m\", \"2h\", \"1d\", \"1h30m\",
or a bare count of seconds -- or NIL when it names none."
  (when (stringp text)
    (let ((text (string-downcase (string-trim '(#\Space #\Tab) text))))
      (when (plusp (length text))
        (let ((total 0) (number 0) (digits 0))
          (loop for char across text
                do (cond ((digit-char-p char)
                          (setf number (+ (* number 10) (digit-char-p char)))
                          (incf digits))
                         ((find char "smhd" :test #'char=)
                          (unless (plusp digits) (return-from parse-duration nil))
                          (incf total (* number (case char
                                                   (#\s 1) (#\m 60) (#\h 3600) (#\d 86400))))
                          (setf number 0 digits 0))
                         (t (return-from parse-duration nil))))
          (cond ((zerop digits) (and (plusp total) total))
                ((zerop total) (and (plusp number) number))
                (t nil)))))))

(defun duration-text (seconds)
  "SECONDS in the shortest unit that says it without becoming a lie."
  (multiple-value-bind (days rest) (floor (round seconds) 86400)
    (multiple-value-bind (hours rest) (floor rest 3600)
      (multiple-value-bind (minutes secs) (floor rest 60)
        (cond ((plusp days) (if (plusp hours)
                                (format nil "~dd~dh" days hours)
                                (format nil "~dd" days)))
              ((plusp hours) (if (plusp minutes)
                                 (format nil "~dh~dm" hours minutes)
                                 (format nil "~dh" hours)))
              ((plusp minutes) (format nil "~dm" minutes))
              (t (format nil "~ds" secs)))))))

;;; --- the registry ---------------------------------------------------------------

(defun watch-get (watch key &optional default)
  "WATCH's member KEY, or DEFAULT."
  (getf watch key default))

(defun (setf watch-get) (value watch key &optional default)
  (declare (ignore default))
  (setf (getf watch key) value))

(defun find-watch (id)
  "The standing watch called ID, or NIL."
  (and (stringp id)
       (find id *watches* :key (lambda (watch) (watch-get watch :id)) :test #'string=)))

(defun id-slug (text)
  "TEXT as an id: lowercase words joined by single dashes."
  (let ((slug (string-downcase
               (cl-ppcre:regex-replace-all "[^a-z0-9]+" (string-downcase (or text "")) "-"))))
    (string-trim "-" slug)))

(defun unused-id (base)
  "BASE, or BASE-2, BASE-3, ... the first id no standing watch has."
  (let ((base (if (plusp (length base)) base "watch")))
    (loop for n from 1
          for candidate = (if (= n 1) base (format nil "~a-~d" base n))
          unless (find-watch candidate) return candidate)))

(defun watch-trigger-text (watch)
  "What WATCH watches, in the words the manual and the agent both use."
  (let ((kind (watch-get watch :kind "every"))
        (every (duration-text (watch-get watch :every 0)))
        (fire-on (watch-get watch :fire-on "failure")))
    (cond ((equal kind "every") (format nil "every ~a" every))
          ((equal kind "after") (format nil "once, ~a after it was armed" every))
          ((equal kind "file") (format nil "the file ~a every ~a, on ~a"
                                       (watch-get watch :path) every fire-on))
          ((equal kind "url") (format nil "the url ~a every ~a, on ~a"
                                      (watch-get watch :url) every fire-on))
          ((equal kind "command")
           (format nil "the command ~{~a~^ ~} every ~a, on ~a"
                   (coerce (watch-get watch :command #()) 'list) every fire-on))
          (t kind))))

(defun watch-when-text (watch &optional (now (get-universal-time)))
  "When WATCH looks next, as a word."
  (let ((next (watch-get watch :next-at)))
    (cond ((watch-get watch :paused) "paused")
          ((null next) "never")
          ((<= next now) "due")
          (t (format nil "in ~a" (duration-text (- next now)))))))

(defun watch-line (watch &optional (now (get-universal-time)))
  "One line for the report: what WATCH is, when it looks next, what it last saw."
  (format nil "~a  ~a (~a)  ~a  last ~a  fired ~d~@[ of ~d~]~:[~; PAUSED~]"
          (watch-get watch :id)
          (or (watch-get watch :title) (watch-get watch :id))
          (watch-trigger-text watch)
          (watch-when-text watch now)
          (or (watch-get watch :last-state) "never looked")
          (watch-get watch :fires 0)
          (let ((max (watch-get watch :max-fires 0))) (and (plusp max) max))
          (and (watch-get watch :paused) t)))

;;; --- the file it survives in -----------------------------------------------------

(defun state-path ()
  "The registry's file under this home."
  (nlk:home +state-file+))

(defun save-watches ()
  "Write the registry, so a restart wakes the same watches."
  (when (and *save-p* *watches*)
    (nlk:write-file-atomically
     (state-path)
     (shasht:write-json (nlk:plist-json (coerce *watches* 'vector)) nil)
     :mode #o600 :directory-mode #o700)))

(defun load-watches ()
  "Read the registry back, or leave it empty when the file is not there. => how many stood."
  (let ((path (probe-file (state-path))))
    (when path
      (let ((data (ignore-errors (shasht:read-json (uiop:read-file-string path)))))
        (setf *watches* (coerce (or (nlk:json-plist data) #()) 'list))))
    (length *watches*)))

;;; --- the session that armed it ----------------------------------------------------

(defun arming-session ()
  "The session a watch armed now wakes: the one whose turn or eval this is."
  (or nlk:*scribe-session-id*
      (getf (nle:turn) :session-id)))
