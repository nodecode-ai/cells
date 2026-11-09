;;;; cell.lisp --- the tools, /computer, and the one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-alans-way)

(defparameter +primer+
  "Alan's Way computer use is on through the nodecode-alans-way cell: the computer_* tools drive
an app on the user's own Mac through their Alan's Workspace router, and the window of the VM's
desktop when that Mac is offline.
Start with computer_apps (what is running, on which host), then computer_capture with app or pid:
its numbered control list is what every later number means. computer_click, computer_drag,
computer_scroll, computer_set_value (prefer it for a field), computer_type, computer_key and
computer_focus act on those numbers or on an x, y point; delivery_mode \"foreground\" brings the
window forward first, and is what the user sees happen.
Numbered controls are the point of a capture: a control's number survives only until the next
capture or a switch between the Mac and the VM, and the reply says which. Keychains, password
managers and password fields stay off limits; a lock screen or a permission prompt needs the
user's own hand, so say so instead of retrying. Scroll amounts are wheel ticks, a quarter page
each. (help :computer) repeats this while the cell runs."
  "The primer the model reads; the tools also say it in their descriptions.")

;;; --- what the section has to say ---------------------------------------------

(defun check-config ()
  "Refuse a section nothing could ever start the router from, naming the member."
  (when (null (setting :router-args))
    (error 'nlk:config-refusal
           :detail (concatenate
                    'string
                    "alans-way: set \"router_args\" to the arguments that start the workspace router "
                    "(the workspace-router.mjs path Alan's Way's workspace setup wrote, and anything it "
                    "needs after it); \"router_command\" is node unless it is another launcher")))
  (when (zerop (length (or (setting :router-command) "")))
    (error 'nlk:config-refusal
           :detail "alans-way: \"router_command\" is the command that starts the workspace router, node by default")))

;;; --- the tools ---------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *apps-schema*
  (tool-schema
   "List the apps the user's computer is running, with their pids, whether each is frontmost and the host that answered (the user's own Mac, or the VM's desktop when the Mac is offline). Start here, and again after a switch between the two: a pid from the other host means nothing."
   '())
  "computer_apps.")

(defparameter *capture-schema*
  (tool-schema
   "Read an app's window as a numbered list of its controls, and target that app for the actions that follow. Every element number the other computer_* tools take is a number from the last capture. Numbers are of this window until the next capture or a host switch. mode \"som\" (the default) lists the controls and attaches the screenshot only when the named ones cover little of the window; \"vision\" always attaches it; \"ax\" never does. Use vision for a canvas, a game or a web area where the list says little."
   '( ("app" "string" "The app's name, or part of it, as computer_apps lists it." nil)
      ("pid" "integer" "The app's pid, when the name is ambiguous or already known." nil)
      ("window_id" "integer" "A window number an earlier reply named, to stay on that window." nil)
      ("mode" "string" "som (default), vision or ax." nil)))
  "computer_capture.")

(defparameter *click-schema*
  (tool-schema
   "Click a numbered control from the last capture, or a screen point. Only the left button, and the left button's right-click and double-click, are available: middle clicks and chorded clicks come back unsupported."
   '( ("element" "integer" "The control's number in the last capture." nil)
      ("x" "integer" "Screen x, when clicking a point instead." nil)
      ("y" "integer" "Screen y, when clicking a point instead." nil)
      ("button" "string" "left (default) or right." nil)
      ("count" "integer" "1 (default) or 2 for a double click." nil)
      ("delivery_mode" "string" "\"background\" (default) leaves the user's keyboard where it is; \"foreground\" brings the window forward first and is what the user sees." nil)))
  "computer_click.")

(defparameter *type-schema*
  (tool-schema
   "Type text into whatever has the keyboard focus. Free typing depends on the host's support; computer_set_value on a field's number is the reliable way to fill one."
   '( ("text" "string" "The text to type." t)
      ("delivery_mode" "string" "background (default) or foreground." nil)))
  "computer_type.")

(defparameter *key-schema*
  (tool-schema
   "Press one key, or a chord such as cmd+s, cmd+shift+g or ctrl+alt+left."
   '( ("keys" "string" "One key (return, escape, tab) or a chord joined by +." t)
      ("delivery_mode" "string" "background (default) or foreground." nil)))
  "computer_key.")

(defparameter *set-value-schema*
  (tool-schema
   "Replace a text field's value: pass the field's number from the last capture. This is the dependable way to fill a field, and the only one on a host that refuses free typing."
   '( ("element" "integer" "The field's number in the last capture." t)
      ("value" "string" "The text the field should hold." t)))
  "computer_set_value.")

(defparameter *scroll-schema*
  (tool-schema
   "Scroll in the targeted app: at a control or a point when one is given, otherwise in the targeted window."
   '( ("direction" "string" "up, down, left or right." t)
      ("amount" "number" "Wheel ticks (default 3); a tick is a quarter page." nil)
      ("element" "integer" "The control number to scroll over." nil)
      ("x" "integer" "Screen x to scroll over instead." nil)
      ("y" "integer" "Screen y to scroll over instead." nil)
      ("delivery_mode" "string" "background (default) or foreground." nil)))
  "computer_scroll.")

(defparameter *drag-schema*
  (tool-schema
   "Drag with the left button from one control or point to another, as both controls' centres."
   '( ("from_element" "integer" "The control to start at, by number." nil)
      ("to_element" "integer" "The control to end at, by number." nil)
      ("from_x" "integer" "Screen x to start at instead." nil)
      ("from_y" "integer" "Screen y to start at instead." nil)
      ("to_x" "integer" "Screen x to end at instead." nil)
      ("to_y" "integer" "Screen y to end at instead." nil)
      ("delivery_mode" "string" "background (default) or foreground." nil)))
  "computer_drag.")

(defparameter *focus-schema*
  (tool-schema
   "Aim the cell at an app, launching it when it is not running (on a host that can launch by name). With raise, the app's window is brought forward first, which is foreground input the user sees."
   '( ("app" "string" "The app's name, or part of it." t)
      ("raise" "boolean" "Bring the app's window to the front now." nil)))
  "computer_focus.")

(defun tool-apps (args)
  "computer_apps: every app the workspace can drive."
  (declare (ignore args))
  (app-lines (list-apps)))

(defun tool-capture (args)
  "computer_capture: the numbered controls of one app's window."
  (capture (gethash "app" args) (gethash "pid" args) (gethash "window_id" args)
           (or (gethash "mode" args) "som")))

(defun answering-tool (function)
  "FUNCTION as a tool body: its refusal, a dead router included, becomes the
one `ERROR:' line the model repairs from."
  (lambda (args)
    (handler-case (progn (running-settings) (funcall function args))
      ((or error sb-sys:deadline-timeout) (condition)
        (nle:failure (format nil "ERROR: alans-way: ~a" condition))))))

;;; --- /computer ---------------------------------------------------------------

(defun run-command (args session-id)
  "One /computer invocation: no argument shows what the cell is driving, apps lists them."
  (declare (ignore session-id))
  (let ((what (string-downcase (nlk:trimmed (or args "")))))
    (handler-case
        (cond ((search "apps" what) (app-lines (list-apps)))
              ((current-pid)
               (format nil "Driving ~a (pid ~a)~@[, window ~a~] on ~a: ~d controls from the last capture. /computer apps lists what else is running."
                       (current-app) (current-pid) (current-window) (or (current :host) "the workspace")
                       (length (current-elements))))
              (t "Driving nothing yet: /computer apps lists the apps, then say \"capture <app>\" to the model."))
      (alans-way-error (condition) (format nil "~a" condition)))))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell alans-way
  (:section ("alans-way")
    (:guide "router_args is the workspace router Alan's Way's setup wrote (a .mjs run by node); router_command is node unless you launch it another way; next step: /computer apps")
    ("router_command" :string :default "node" :required t
     :doc "the command that starts the Alan's Workspace router")
    ("router_args" :list :required t
     :doc "its arguments: the router script's path, and anything it needs after it")
    ("call_timeout" :integer :default 60 :min 1
     :doc "seconds one router call may take, the start-up included")
    ("screenshot_dir" :path
     :doc "where a vision capture writes its PNG; under the home's computer directory by default"))
  (:help :computer "computer: computer_apps, computer_capture and their gestures drive a Mac app through Alan's Workspace" +primer+)
  (:command "computer" 'run-command
            :description "What the cell is driving, or the running apps"
            :argument-hint "APPS")
  (:tool "computer_apps" *apps-schema* (answering-tool #'tool-apps))
  (:tool "computer_capture" *capture-schema* (answering-tool #'tool-capture))
  (:tool "computer_click" *click-schema* (answering-tool #'tool-click))
  (:tool "computer_drag" *drag-schema* (answering-tool #'tool-drag))
  (:tool "computer_scroll" *scroll-schema* (answering-tool #'tool-scroll))
  (:tool "computer_type" *type-schema* (answering-tool #'tool-type))
  (:tool "computer_key" *key-schema* (answering-tool #'tool-key))
  (:tool "computer_set_value" *set-value-schema* (answering-tool #'tool-set-value))
  (:tool "computer_focus" *focus-schema* (answering-tool #'tool-focus))
  (:start (lambda ()
            (check-config)
            (setf *target* nil)
            (nle:on-stop (lambda () (setf *target* nil))))))
