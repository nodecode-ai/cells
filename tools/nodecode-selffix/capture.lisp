;;;; capture.lisp --- a failed turn's condition, caught live, kept as a case.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two advices, both on while the cell runs. NOTE-FRAME rides AGENT-LOOP: a
;;;; HANDLER-BIND that, when an error leaves the loop and the stack still
;;;; stands, notes the frame that signalled it - the first function of the
;;;; organism's own past the condition system - with its arguments. It
;;;; declines, so the turn fails exactly as it did. CAPTURE-FAILURE rides
;;;; FAIL-TURN: once the failure is a fact, the condition becomes a case, one
;;;; readable form per fault in cases/<fp>.lisp, counted:
;;;;
;;;;   (:type TYPE :function F :args ARGS-or-:unreadable :count N
;;;;    :first ISO :last ISO :generation N :turn ID :session ID
;;;;    [:name SYMBOL] [:classes (CLASS ...)] [:caller F] [:detail TEXT])
;;;;
;;;; The fault is the pair (TYPE . F), F the function the condition names
;;;; when it names one (a generic function with no method, a function that is
;;;; not defined), else the function that signalled. A provider's failure,
;;;; which the engine hands FAIL-TURN with evidence, is no fault of the code
;;;; and makes no case; neither does a turn of the cron wake's own session.

(in-package #:nodecode-selffix)

(defparameter *args-cap* 2000
  "Characters an argument list may print to and still be kept; past it the case says :UNREADABLE.")

(defvar *frames* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Condition => (FUNCTION ARGS): the frame that signalled it, noted while the stack stood.")

(defvar *cases-lock* (sb-thread:make-mutex :name "selffix cases"))

(defun cases-dir () (selffix-path "cases/"))

(defun case-path (fp) (selffix-path (format nil "cases/~a.lisp" fp)))

;;; --- the frame --------------------------------------------------------------

(defun frame-function (name)
  "The function name a backtrace frame NAME stands for, or NIL: a symbol or
(SETF symbol) as it is, a method's or a local function's enclosing name."
  (flet ((fname-p (x) (or (and x (symbolp x)) (and (consp x) (eq (first x) 'setf) (symbolp (second x))))))
    (cond ((fname-p name) name)
          ((not (consp name)) nil)
          ((member (symbol-name (first name)) '("FAST-METHOD" "SLOW-METHOD" "METHOD") :test #'string=)
           (and (fname-p (second name)) (second name)))
          (t (let ((in (member :in name))) (and in (fname-p (second in)) (second in)))))))

(defun own-function-p (fn)
  "Whether FN is a function of the organism's own: not Lisp's, not SBCL's, not this cell's."
  (let* ((symbol (if (consp fn) (second fn) fn))
         (package (and symbol (symbol-package symbol)))
         (name (and package (package-name package))))
    (and name
         (not (member name '("COMMON-LISP" "KEYWORD" "UIOP" "ASDF") :test #'string=))
         (not (uiop:string-prefix-p "SB-" name))
         (not (uiop:string-prefix-p "UIOP/" name))
         (not (eq package (find-package '#:nodecode-selffix))))))

(defun signalling-frame ()
  "(FUNCTION ARGS) of the innermost frame of the organism's own on the
current stack, or NIL. Arguments the compiler dropped are left as they are;
READABLE-TEXT refuses them."
  (let ((found nil))
    (block walk
      (sb-debug:map-backtrace
       (lambda (frame)
         (multiple-value-bind (name args) (ignore-errors (sb-debug::frame-call frame))
           (let ((fn (frame-function name)))
             (when (and fn (own-function-p fn))
               (setf found (list fn args))
               (return-from walk)))))
       :count 60))
    found))

(defun note-frame (next &rest args)
  "AGENT-LOOP advice: note the frame of an error that leaves the loop, then
decline it, so the turn fails as before. Never signals on its own."
  (handler-bind ((error (lambda (condition)
                          (ignore-errors
                           (unless (gethash condition *frames*)
                             (let ((frame (signalling-frame)))
                               (when frame (setf (gethash condition *frames*) frame))))))))
    (apply next args)))

;;; --- readable text ----------------------------------------------------------

(defmacro with-case-syntax (&body body)
  "BODY under standard syntax, every symbol printed with its package, no #. read."
  `(with-standard-io-syntax
     (let ((*package* (find-package :keyword)) (*read-eval* nil) (*print-circle* t))
       ,@body)))

(defun readable-text (object)
  "OBJECT's printed text when it reads back EQUALP to itself and fits
*ARGS-CAP*, else NIL."
  (handler-case
      (let ((text (with-case-syntax (let ((*print-readably* t)) (prin1-to-string object)))))
        (and (<= (length text) *args-cap*)
             (equalp (with-case-syntax (read-from-string text)) object)
             text))
    (error () nil)))

;;; --- the case ---------------------------------------------------------------

(defun slot-of (condition name &optional (package "COMMON-LISP"))
  "CONDITION's slot NAME (a symbol of PACKAGE), or NIL when it has none."
  (let ((slot (find-symbol name package)))
    (and slot (slot-exists-p condition slot) (slot-boundp condition slot) (slot-value condition slot))))

(defun class-name-of (object) (class-name (class-of object)))

(defun case-facts (condition frame)
  "The fault CONDITION is, with FRAME (FUNCTION ARGS) as noted => a plist of
:type :function :args and, when known, :name :classes :caller :detail."
  (let ((type (type-of condition))
        (caller (first frame))
        (frame-args (second frame)))
    (cond ((typep condition 'sb-pcl::no-applicable-method-error)
           (let ((gf (slot-of condition "GENERIC-FUNCTION"))
                 (args (slot-of condition "ARGS" "SB-PCL")))
             (list :type type
                   :function (and gf (sb-mop:generic-function-name gf))
                   :args (if (readable-text args) args :unreadable)
                   :classes (mapcar #'class-name-of args)
                   :caller caller)))
          ((typep condition 'undefined-function)
           (list :type type :function (cell-error-name condition) :args :unreadable :caller caller))
          ((typep condition 'unbound-variable)
           (list :type type :function caller :name (cell-error-name condition) :args :unreadable))
          (t
           (list :type type :function caller
                 :args (if (and caller (readable-text frame-args)) frame-args :unreadable)
                 :detail (nlk:one-line (let ((sb-int:*print-condition-references* nil))
                                         (princ-to-string condition))
                                       :cap 300))))))

(defun case-fingerprint (facts)
  "16 hex digits over the fault: its type, its function and the name it misses."
  (let ((digest (nlk:sha256-text (with-case-syntax
                                   (prin1-to-string (list (getf facts :type) (getf facts :function) (getf facts :name)))))))
    (subseq digest (1+ (position #\: digest)) (+ 17 (position #\: digest)))))

(defun read-case (path)
  "The case at PATH, or NIL when there is none or it no longer reads."
  (ignore-errors
   (with-open-file (in path :if-does-not-exist nil)
     (and in (with-case-syntax (read in nil nil))))))

(defun write-case (path case)
  "Write CASE to PATH whole, one key a line, through a temporary file."
  (ensure-directories-exist path)
  (let ((tmp (make-pathname :type "tmp" :defaults path)))
    (with-open-file (out tmp :direction :output :if-exists :supersede)
      (with-case-syntax
        (let ((*print-readably* t) (*print-right-margin* 100))
          (format out ";;;; selffix case: one fault, as the turns that met it left it~%(~{~s ~s~^~% ~})~%" case))))
    (rename-file tmp path)
    path))

(defun boot-generation ()
  "The generation this process booted as, or NIL."
  (ignore-errors
   (let* ((label (cdr nlk::*generation-label*))
          (start (position-if #'digit-char-p label)))
     (and start (parse-integer label :start start :junk-allowed t)))))

(defun note-case (turn condition &key (now (get-universal-time)))
  "Count CONDITION, TURN's failure, into its case => the case's path, or NIL
when the failure makes none."
  (let ((session (ignore-errors (nlk::durable-turn-session-id turn))))
    (unless (and session (ignore-errors (funcall *excluded-session-p* session)))
      (let* ((facts (case-facts condition (gethash condition *frames*)))
             (fp (case-fingerprint facts))
             (path (case-path fp)))
        (when (getf facts :function)
          (sb-thread:with-mutex (*cases-lock*)
            (let* ((old (read-case path))
                   (old-args (getf old :args :none))
                   (args (if (and (eq (getf facts :args) :unreadable) (listp old-args))
                             old-args
                             (getf facts :args))))
              (write-case path
                          (append (list :type (getf facts :type)
                                        :function (getf facts :function)
                                        :args args
                                        :count (1+ (or (getf old :count) 0))
                                        :first (or (getf old :first) (iso now))
                                        :last (iso now)
                                        :generation (boot-generation)
                                        :turn (ignore-errors (nlk::durable-turn-turn-id turn))
                                        :session session)
                                  (loop for key in '(:name :classes :caller :detail)
                                        when (getf facts key) append (list key (getf facts key))))))))))))

(defun capture-failure (next turn condition &rest keys &key evidence &allow-other-keys)
  "FAIL-TURN advice: the failure is recorded first, then counted into its
case. A provider's failure (EVIDENCE) makes none. Never fails the turn's record."
  (multiple-value-prog1 (apply next turn condition keys)
    (unless evidence
      (ignore-errors (note-case turn condition)))
    (remhash condition *frames*)))

(defun cases ()
  "Every case on disk, most frequent first."
  (sort (remove nil (mapcar #'read-case (directory (merge-pathnames "*.lisp" (cases-dir)))))
        #'> :key (lambda (case) (or (getf case :count) 0))))
