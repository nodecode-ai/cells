;;;; xml.lisp --- a scheduled task's definition, read as data.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every task the Scheduler knows has a definition: XML that names its
;;;; triggers, its actions and the account it runs as, in a schema that does not
;;;; change with the display language. Export-ScheduledTask hands that document
;;;; back as text, and this file reads it -- a reader for elements, text and
;;;; entities, which is all a task definition uses. Attributes are skipped and
;;;; so is a namespace part (a name is its part after the colon): the schema is
;;;; the same with one and without.

(in-package #:nodecode-windows-tasks)

;;; --- the nodes ---------------------------------------------------------------------

(defstruct (node (:constructor make-node (tag children)) (:copier nil))
  "One element: its TAG (local name, no namespace) and its CHILDREN, each a
NODE or a string of text."
  (tag "" :type string :read-only t)
  (children '() :type list :read-only t))

(defun xml-fail (control &rest arguments)
  "A definition that cannot be read, as the cell's one refusal."
  (fail "the definition could not be read: ~?" control arguments))

(defun local-name (name)
  "NAME with any namespace part dropped: p:Task reads Task."
  (let ((colon (position #\: name)))
    (if colon (subseq name (1+ colon)) name)))

;;; --- the reader --------------------------------------------------------------------

(defun parse-xml (text)
  "TEXT's root element as a NODE. The declaration, comments and doctypes are
skipped; anything that is not one well-formed element refuses."
  (let ((at 0)
        (length (length text)))
    (labels
        ((spacep (char) (member char '(#\Space #\Tab #\Newline #\Return)))
         (skip-space ()
           (loop while (and (< at length) (spacep (char text at))) do (incf at)))
         (prefix-p (marker)
           (let ((end (+ at (length marker))))
             (and (<= end length) (string= marker text :start2 at :end2 end))))
         (skip-past (marker)
           (let ((end (search marker text :start2 at)))
             (unless end (xml-fail "it ends inside a declaration or comment"))
             (setf at (+ end (length marker)))))
         (read-name ()
           (let ((from at))
             (loop while (and (< at length)
                              (not (spacep (char text at)))
                              (not (member (char text at) '(#\> #\/))))
                   do (incf at))
             (subseq text from at)))
         (skip-tag-end ()
           "AT is after a tag's name: skip its attributes. => T when it closes itself."
           (loop
             (cond
               ((>= at length) (xml-fail "it ends inside a tag"))
               ((char= (char text at) #\>) (incf at) (return nil))
               ((and (char= (char text at) #\/) (< (1+ at) length)
                     (char= (char text (1+ at)) #\>))
                (incf at 2) (return t))
               ((member (char text at) '(#\" #\'))
                (let* ((quote (char text at))
                       (end (position quote text :start (1+ at))))
                  (unless end (xml-fail "an attribute's quote is never closed"))
                  (setf at (1+ end))))
               (t (incf at)))))
         (read-element ()
           ;; AT is just past the '<' of an opening tag.
           (let* ((tag (local-name (read-name))))
             (when (zerop (length tag)) (xml-fail "a tag with no name"))
             (skip-space)
             (if (skip-tag-end)
                 (make-node tag '())
                 (make-node tag (read-children tag)))))
         (read-children (tag)
           (let ((children '()))
             (loop
               (cond
                 ((>= at length) (xml-fail "it ends inside <~a>" tag))
                 ((char/= (char text at) #\<)
                  (let ((end (or (position #\< text :start at) length)))
                    (push (decode-entities (subseq text at end)) children)
                    (setf at end)))
                 ((prefix-p "<!--") (skip-past "-->"))
                 ((prefix-p "<![CDATA[")
                  (let ((end (search "]]>" text :start2 (+ at 9))))
                    (unless end (xml-fail "a CDATA section is never closed"))
                    (push (subseq text (+ at 9) end) children)
                    (setf at (+ end 3))))
                 ((prefix-p "</")
                  (incf at 2)
                  (let ((name (local-name (read-name))))
                    (skip-space)
                    (unless (and (< at length) (char= (char text at) #\>))
                      (xml-fail "a closing tag ends without >"))
                    (incf at)
                    (unless (string= name tag)
                      (xml-fail "the closing tag is </~a> where </~a> belongs" name tag))
                    (return (nreverse children))))
                 (t (incf at) (push (read-element) children)))))))
      (loop
        (when (>= at length) (xml-fail "it holds no element"))
        (cond
          ((char/= (char text at) #\<)
           (let ((end (or (position #\< text :start at) length))) (setf at end)))
          ((prefix-p "<?") (skip-past "?>"))
          ((prefix-p "<!--") (skip-past "-->"))
          ((prefix-p "<!") (skip-past ">"))
          (t (incf at) (return (read-element))))))))

(defun decode-entities (text)
  "TEXT with XML's entities and character references as the characters they name."
  (if (null (search "&" text))
      text
      (with-output-to-string (out)
        (loop with from = 0
              for amp = (position #\& text :start from)
              do (cond
                   ((null amp) (write-string text out :start from) (return))
                   (t
                    (write-string text out :start from :end amp)
                    (let ((semi (position #\; text :start amp)))
                      (if (null semi)
                          (progn (write-string text out :start amp) (return))
                          (let* ((entity (subseq text (1+ amp) semi))
                                 (char (entity-char entity)))
                            (write-string (if char (string char)
                                              (subseq text amp (1+ semi)))
                                          out)
                            (setf from (1+ semi)))))))))))

(defun entity-char (entity)
  "The character XML's ENTITY stands for, or NIL when it is none."
  (cond ((string= entity "amp") #\&)
        ((string= entity "lt") #\<)
        ((string= entity "gt") #\>)
        ((string= entity "quot") #\")
        ((string= entity "apos") #\')
        ((and (> (length entity) 1) (char= (char entity 0) #\#))
         (let* ((hex (member (char entity 1) '(#\x #\X)))
                (code (ignore-errors
                       (parse-integer (subseq entity (if hex 2 1)) :radix (if hex 16 10)))))
           (and code (code-char code))))
        (t nil)))

;;; --- reading one node --------------------------------------------------------------

(defun node-text (node)
  "NODE's own text, trimmed, or NIL when it holds none."
  (when (node-p node)
    (let ((text (nlk:trimmed (with-output-to-string (out)
                               (dolist (child (node-children node))
                                 (when (stringp child) (write-string child out)))))))
      (and (plusp (length text)) text))))

(defun elements (node)
  "NODE's child elements."
  (when (node-p node)
    (remove-if-not #'node-p (node-children node))))

(defun child (node name)
  "NODE's first child element named NAME."
  (find-if (lambda (candidate) (string= (node-tag candidate) name)) (elements node)))

(defun child-text (node name)
  "The text of NODE's child NAME, or NIL."
  (let ((found (child node name)))
    (and found (node-text found))))

(defun child-bool (node name &optional default)
  "NODE's child NAME as a boolean, or DEFAULT when it says nothing."
  (let ((text (child-text node name)))
    (if (null text)
        default
        (and (member (string-downcase text) '("true" "1" "yes") :test #'string=) t))))

(defun keyword-of (name)
  "A definition's element NAME as the keyword it names: DaysOfWeek reads
:DAYS-OF-WEEK, WorkingDirectory :WORKING-DIRECTORY."
  (let ((out (make-string-output-stream)))
    (loop for char across name
          for before = nil then previous
          for previous = char
          do (when (and before (upper-case-p char) (not (upper-case-p before)))
               (write-char #\- out))
             (write-char (char-upcase char) out))
    (intern (get-output-stream-string out) :keyword)))

(defun flat (node)
  "NODE's children one level down as a plist, a child with elements of its own
as a plist of its own: a schedule, as it is."
  (loop for candidate in (elements node)
        append (list (keyword-of (node-tag candidate))
                     (if (elements candidate) (flat candidate) (node-text candidate)))))

;;; --- the definition ----------------------------------------------------------------

(defun parse-definition (text)
  "TEXT (a definition document) as a plist of :REGISTRATION, :PRINCIPAL,
:SETTINGS, :TRIGGERS and :ACTIONS."
  (let ((root (parse-xml text)))
    (unless (string= (node-tag root) "Task")
      (xml-fail "its root is <~a>, not <Task>" (node-tag root)))
    (let* ((registration (child root "RegistrationInfo"))
           (principals (child root "Principals"))
           (principal (first (elements principals)))
           (settings (child root "Settings"))
           (maintenance (child settings "MaintenanceSettings")))
      (list :registration (list :author (child-text registration "Author")
                                :description (child-text registration "Description")
                                :uri (child-text registration "URI")
                                :date (child-text registration "Date"))
            :principal (list :user-id (child-text principal "UserId")
                             :group-id (child-text principal "GroupId")
                             :logon-type (child-text principal "LogonType")
                             :run-level (child-text principal "RunLevel"))
            :settings (list :enabled (child-bool settings "Enabled" t)
                            :hidden (child-bool settings "Hidden" nil)
                            :disallow-start-if-on-batteries
                            (child-bool settings "DisallowStartIfOnBatteries")
                            :stop-if-going-on-batteries
                            (child-bool settings "StopIfGoingOnBatteries")
                            :start-when-available (child-bool settings "StartWhenAvailable")
                            :multiple-instances (child-text settings "MultipleInstancesPolicy")
                            :execution-time-limit (child-text settings "ExecutionTimeLimit")
                            ;; no triggers with this element: the task runs during
                            ;; automatic maintenance, and the notes say so
                            :maintenance (and maintenance
                                             (list :period (child-text maintenance "Period")
                                                   :deadline (child-text maintenance "Deadline"))))
            :triggers (mapcar #'read-trigger (elements (child root "Triggers")))
            :actions (mapcar #'read-action (elements (child root "Actions")))))))

(defun read-trigger (element)
  "One trigger element as a plist of what a person needs to read it."
  (let ((trigger (list :kind (node-tag element)
                       :enabled (child-bool element "Enabled" t)
                       :start-boundary (child-text element "StartBoundary")
                       :end-boundary (child-text element "EndBoundary")
                       :delay (child-text element "Delay")
                       :user-id (child-text element "UserId")
                       :state-change (child-text element "StateChange")
                       :repetition (let ((repetition (child element "Repetition")))
                                     (and repetition (flat repetition)))
                       :subscription (or (child-text element "EventTrigger")
                                          (child-text element "Subscription")))))
    ;; the schedule is kept under its own name: a daily and a weekly trigger
    ;; both have a schedule, and which one it is says what the trigger does
    (or (loop for name in '("ScheduleByDay" "ScheduleByWeek" "ScheduleByMonth"
                            "ScheduleByMonthDayOfWeek")
              for schedule = (child element name)
              when schedule
                return (append trigger
                               (list :schedule (list (keyword-of name) (flat schedule)))))
        trigger)))

(defun read-action (element)
  "One action element as a plist: its kind and the fields it carries."
  (let ((action (list :kind (node-tag element))))
    (dolist (name '("Command" "Arguments" "WorkingDirectory" "ClassId" "Data"))
      (let ((text (child-text element name)))
        (when text (setf action (append action (list (keyword-of name) text))))))
    action))

;;; --- telling it in words ------------------------------------------------------------

(defun clock (start-boundary)
  "The time of day inside a start boundary: ...T03:00:00 reads 03:00."
  (let ((t-at (and start-boundary (position #\T start-boundary))))
    (when t-at
      (let ((time (subseq start-boundary (1+ t-at))))
        (when (and (<= 3 (length time))
                   (string= ":00" time :start2 (- (length time) 3)))
          (setf time (subseq time 0 (- (length time) 3))))
        (and (plusp (length time)) time)))))

(defun names-of (plist)
  "The member names of one flat element, comma-joined, or NIL."
  (let ((names (loop for (key nil) on plist by #'cddr
                     collect (string-downcase (symbol-name key)))))
    (and names (format nil "~{~a~^, ~}" names))))

(defun describe-trigger (trigger)
  "One trigger in a sentence, for a listing that has no room for the schema."
  (let* ((kind (or (getf trigger :kind) "Trigger"))
         (at (clock (getf trigger :start-boundary)))
         (schedule (getf trigger :schedule))
         (text
           (cond
             ((string= kind "BootTrigger")
              (format nil "at startup~@[ after ~a~]" (getf trigger :delay)))
             ((string= kind "LogonTrigger")
              (format nil "at logon~@[ of ~a~]" (getf trigger :user-id)))
             ((string= kind "SessionStateChangeTrigger")
              (format nil "on session ~a" (or (getf trigger :state-change) "state change")))
             ((string= kind "EventTrigger") "on event")
             ((getf schedule :schedule-by-day)
              (format nil "every ~a day(s)~@[ at ~a~]"
                      (or (getf (getf schedule :schedule-by-day) :days-interval) "1") at))
             ((getf schedule :schedule-by-week)
              (let ((week (getf schedule :schedule-by-week)))
                (format nil "every ~a week(s) on ~a~@[ at ~a~]"
                        (or (getf week :weeks-interval) "1")
                        (or (names-of (getf week :days-of-week)) "no day named")
                        at)))
             ((getf schedule :schedule-by-month)
              (let ((month (getf schedule :schedule-by-month)))
                (format nil "~a on ~a~@[ at ~a~]"
                        (or (names-of (getf month :months)) "every month")
                        (or (names-of (getf month :days-of-month)) "no day named")
                        at)))
             ((string= kind "TimeTrigger")
              (format nil "once at ~a" (or (getf trigger :start-boundary) "once")))
             (t (format nil "~a~@[ at ~a~]" kind at))))
         (repetition (getf trigger :repetition)))
    (when (getf repetition :interval)
      (setf text (format nil "~a, repeating every ~a~@[ for ~a~]"
                          text (getf repetition :interval) (getf repetition :duration))))
    (concatenate 'string text (if (getf trigger :enabled t) "" ", disabled"))))

(defun describe-action (action)
  "One action as the command line a person would run."
  (if (string= (or (getf action :kind) "") "Exec")
      (let ((command (or (getf action :command) ""))
            (arguments (getf action :arguments)))
        (if arguments (format nil "~a ~a" command arguments) command))
      (format nil "~a~@[ ~a~]" (or (getf action :kind) "Action")
              (or (getf action :class-id) (getf action :data)))))
