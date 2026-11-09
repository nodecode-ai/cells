;;;; actions.lisp --- the gestures: click, drag, scroll, type, key, set_value, focus.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One call to the router's workspace_computer_action per gesture, aimed
;;;; either at a numbered control of the last capture (its ref, which the
;;;; helper resolves) or at a screen point. delivery_mode "foreground"
;;;; has the router activate the window first (a batch of activate then the
;;;; step), as the original's bring_to_front does; the background keeps the
;;;; user's own typing where it is.

(in-package #:nodecode-alans-way)

(defparameter +pages-per-tick+ 0.25
  "The stock tool scrolls in wheel ticks, the app in pages: a quarter page
per tick, as the original's PAGES_PER_TICK.")

(defun host-binding ()
  "What pins an action to the host the element list was read on."
  (when (current :host)
    (list "expect_host" (if (member (current :host) '("vps" "vm") :test #'equal)
                              "vm" "computer"))))

(defun body-notes (body)
  "The router's own notes on BODY's steps."
  (loop for result across (or (nlk:json-value body :array "results") #())
        for note = (nlk:json-value result :string "note")
        when (and note (plusp (length note))) collect note))

(defun describe-action (action body notes)
  "What the model reads after ACTION ran."
  (with-output-to-string (out)
    (format out "~a sent to ~a (pid ~a)." action (current :app) (current :pid))
    (dolist (note (append (body-notes body) notes))
      (format out " ~a" note))
    (when (nlk:json-value body :boolean "unchanged")
      (format out " The app's element tree did not change; a vision capture shows the pixels."))
    (format out " ~a"
            (if (equal action "focus_app")
                "Capture before acting."
                "Capture again before acting by element number."))))

(defun act (action step &key bring)
  "Send STEP as ACTION to the targeted app; answer the text the model reads.
With BRING, the router activates the window first."
  (let ((pid (current-pid)))
    (unless pid
      (fail "no app is targeted: call computer_capture with an app first"))
    (let* ((binding (append (when (current-window) (list "windowId" (current-window)))
                            (when (current :generation) (list "generation" (current :generation)))
                            (host-binding)))
           (arguments (if bring
                          (apply #'nlk:make-json-object
                                 (append (list "pid" pid "action" "batch"
                                               "steps" (vector (append (list "action" "activate")
                                                                        (when (current-window)
                                                                          (list "windowId" (current-window))))
                                                               step))
                                         binding))
                          (apply #'nlk:make-json-object (append (list "pid" pid) step binding)))))
      (multiple-value-bind (body host notes) (ask "workspace_computer_action" arguments)
        (seen-host host)
        (describe-action action body notes)))))

(defun center-of (index)
  "The screen point at the middle of the control numbered INDEX."
  (let ((element (element-at index)))
    (when element
      (let ((x (nlk:json-value element :any "x"))
            (y (nlk:json-value element :any "y"))
            (w (nlk:json-value element :any "width"))
            (h (nlk:json-value element :any "height")))
        (when (and (numberp x) (numberp y) (numberp w) (numberp h) (plusp w) (plusp h))
          (values (round (+ x (/ w 2))) (round (+ y (/ h 2)))))))))

(defun gesture (action step element x y &key bring)
  "ACT with STEP at ELEMENT — a number from the last capture — or at X, Y."
  (cond ((and (integerp element) (element-at element))
         (act action (append step (list "ref" (nlk:json-value (element-at element) :any "ref")))
              :bring bring))
        ((and (numberp x) (numberp y))
         (act action (append step (list "x" (round x) "y" (round y))) :bring bring))
        (element
         (fail "element ~a is not in the last capture: capture again" element))
        (t (fail "~a needs an element number from the last capture, or x and y" action))))

(defun foreground-p (args)
  (and (gethash "delivery_mode" args) (equal "foreground" (gethash "delivery_mode" args))))

(defun tool-click (args)
  "computer_click: a press at a control or a point."
  (let* ((element (gethash "element" args))
         (button (or (gethash "button" args) "left"))
         (count (or (gethash "count" args) 1)))
    (unless (member button '("left" "right") :test #'equal)
      (fail "button is left or right, not ~s" button))
    (unless (member count '(1 2))
      (fail "count is 1 or 2, not ~s" count))
    (let* ((action (cond ((= count 2) "double_click")
                         ((equal button "right") "right_click")
                         (t "click")))
           (verb (if (and element (equal action "click")) "press" action)))
      (gesture action (list "action" verb) element (gethash "x" args) (gethash "y" args)
               :bring (and (foreground-p args) t)))))

(defun tool-type (args)
  "computer_type: free typing into whatever has the keyboard."
  (let ((text (gethash "text" args)))
    (unless (and (stringp text) (plusp (length text)))
      (fail "text is required"))
    (handler-case
        (act "type" (list "action" "type" "text" text)
             :bring (and (foreground-p args) t))
      (alans-way-error (condition)
        (if (search "needs a ref" (alans-way-error-detail condition))
            (fail "~a Use computer_set_value(element=<field number>, value=<text>) for a field, or computer_key for a shortcut."
                  (alans-way-error-detail condition))
            (error condition))))))

(defun tool-set-value (args)
  "computer_set_value: replace a named field's text."
  (let ((element (gethash "element" args))
        (value (gethash "value" args)))
    (unless (integerp element)
      (fail "set_value needs element, the number of a text field in the last capture"))
    (unless (stringp value)
      (fail "value is required"))
    (gesture "set_value" (list "action" "type" "text" value) element nil nil)))

(defun tool-key (args)
  "computer_key: one key, or a chord like cmd+shift+g."
  (let ((keys (gethash "keys" args)))
    (unless (and (stringp keys) (plusp (length keys)))
      (fail "keys is required, for example cmd+s or return"))
    (act "key" (if (and (search "+" keys) (> (length keys) 1))
                    (list "action" "hotkey" "keys" keys)
                    (list "action" "key" "key" keys))
         :bring (and (foreground-p args) t))))

(defun tool-scroll (args)
  "computer_scroll: wheel ticks, a quarter page each, at a point or the window."
  (let* ((direction (or (gethash "direction" args) "down"))
         (amount (or (gethash "amount" args) 3)))
    (unless (member direction '("up" "down" "left" "right") :test #'equal)
      (fail "direction is up, down, left or right, not ~s" direction))
    (unless (numberp amount)
      (fail "amount is a number of wheel ticks, not ~s" amount))
    (let ((step (list "action" "scroll" "direction" direction
                      "amount" (round (* (min (max (* amount +pages-per-tick+) 0.1) 10) 100) 100))))
      (if (or (gethash "element" args) (gethash "x" args))
          (gesture "scroll" step (gethash "element" args) (gethash "x" args) (gethash "y" args))
          (act "scroll" step :bring (and (foreground-p args) t))))))

(defun tool-drag (args)
  "computer_drag: from one control or point to another."
  (let ((from (gethash "from_element" args))
        (to (gethash "to_element" args)))
    (multiple-value-bind (x y)
        (if (integerp from)
            (center-of from)
            (values (gethash "from_x" args) (gethash "from_y" args)))
      (multiple-value-bind (x2 y2)
          (if (integerp to)
              (center-of to)
              (values (gethash "to_x" args) (gethash "to_y" args)))
        (unless (and (numberp x) (numberp y) (numberp x2) (numberp y2))
          (fail "drag needs two controls from the last capture, or two points"))
        (act "drag" (list "action" "drag" "x" (round x) "y" (round y)
                           "x2" (round x2) "y2" (round y2))
             :bring (and (foreground-p args) t))))))

(defun tool-focus (args)
  "computer_focus: aim the cell at an app, launching it when it is not running."
  (let ((app (gethash "app" args)))
    (unless (and (stringp app) (plusp (length (string-trim '(#\Space) app))))
      (fail "app is required, a name from computer_apps"))
    (multiple-value-bind (pid name) (resolve-app app :launch t)
      (target pid name nil)
      (if (gethash "raise" args)
          (act "focus_app" (list "action" "activate"))
          (format nil "Targeting ~a (pid ~a). Capture before acting." name pid)))))
