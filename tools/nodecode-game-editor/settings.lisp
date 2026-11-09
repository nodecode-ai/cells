;;;; settings.lisp --- project.godot as a user of the project reads it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three views of one file: the autoloads a scene can name by a bare Name,
;;;; the input map an action maps to keys, and a summary -- name, main scene,
;;;; counts -- that answers a first question and the /game-editor panel.
;;;; Godot writes every value as a Lisp-looking literal (Vector2(...),
;;;; Object(InputEventKey,...)); nothing is evaluated, the literals are read
;;;; as text, which is all a reader needs and all this cell will do.

(in-package #:nodecode-game-editor)

(defun json-integer (field text)
  "FIELD's integer in TEXT, as in an input event's physical_keycode:65, or NIL."
  (let* ((key (format nil "\"~a\":" field))
         (at (search key text)))
    (when at
      (let* ((start (+ at (length key)))
             (end (or (position-if-not #'digit-char-p text :start start) (length text))))
        (and (> end start) (parse-integer text :start start :end end))))))

(defun json-string (field text)
  "FIELD's string in TEXT, as in an input event's action, or NIL."
  (let* ((key (format nil "\"~a\":\"" field))
         (at (search key text)))
    (when at
      (let* ((start (+ at (length key)))
             (end (position #\" text :start start)))
        (and end (subseq text start end))))))

(defparameter *key-names*
  '((32 . "space") (4194305 . "escape") (4194306 . "tab") (4194308 . "backspace")
    (4194309 . "enter") (4194311 . "insert") (4194312 . "delete") (4194317 . "home")
    (4194318 . "end") (4194319 . "left") (4194320 . "up") (4194321 . "right")
    (4194322 . "down") (4194323 . "pageup") (4194324 . "pagedown") (4194325 . "shift")
    (4194326 . "ctrl") (4194327 . "meta") (4194328 . "alt") (4194329 . "capslock")
    (4194332 . "F1") (4194333 . "F2") (4194334 . "F3") (4194335 . "F4") (4194336 . "F5")
    (4194337 . "F6") (4194338 . "F7") (4194339 . "F8") (4194340 . "F9") (4194341 . "F10")
    (4194342 . "F11") (4194343 . "F12"))
  "The Godot 4 key codes a person searches for by name; a letter or a digit
is its own character, and anything else answers as its number.")

(defun key-name (code)
  "CODE as a key name: A, 3, space, left, F2, or keycode N."
  (concatenate 'string
               (cond ((null code) "?")
                     ((<= 65 code 90) (string (code-char code)))
                     ((<= 48 code 57) (string (code-char code)))
                     (t (or (cdr (assoc code *key-names*))
                            (format nil "keycode ~d" code))))))

(defun input-events (value)
  "VALUE's Object(...) event literals, each as its own text."
  (let ((text (or value "")) (events '()) (start 0))
    (loop for open = (search "Object(" text :start2 start)
          while open
          do (let ((close (position #\) text :start open)))
               (push (subseq text open (if close (1+ close) (length text))) events)
               (setf start (if close (1+ close) (length text)))))
    (nreverse events)))

(defun event-label (event)
  "EVENT as a person names it: the key, the mouse button, the joypad axis."
  (cond ((search "InputEventKey" event)
         (key-name (or (json-integer "physical_keycode" event)
                       (json-integer "keycode" event))))
        ((search "InputEventMouseButton" event)
         (format nil "mouse button ~a" (or (json-integer "button_index" event) "?")))
        ((search "InputEventJoypadButton" event)
         (format nil "joypad button ~a" (or (json-integer "button_index" event) "?")))
        ((search "InputEventJoypadMotion" event)
         (format nil "joypad axis ~a" (or (json-integer "axis" event) "?")))
        ((search "InputEventAction" event)
         (format nil "action ~a" (or (json-string "action" event) "?")))
        ((search "InputEventMouseMotion" event) "mouse motion")
        (t (or (nlk:first-line event 60) "an event"))))

(defun autoloads-text ()
  "project.godot's autoloads, the names a scene and a script can use."
  (let ((autoloads (ini-section (project-ini) "autoload")))
    (if (null autoloads)
        "no autoload: no scene can name a node by a bare singleton name"
        (format nil "~d autoload~:p, in the order project.godot lists them:~%~{~a~^~%~}"
                (length autoloads)
                (loop for (name . value) in autoloads
                      for target = (plain-value value)
                      for singleton = (and (plusp (length target)) (char= #\* (char target 0)))
                      collect (format nil "  ~a -> ~a~@[ (singleton)~]"
                                      name (if singleton (subseq target 1) target)
                                      (and singleton t)))))))

(defun input-map-text (&key action)
  "project.godot's input map: each action and the keys bound to it, or one
ACTION's bindings."
  (let* ((map (ini-section (project-ini) "input"))
         (kept (if (and action (plusp (length action)))
                   (remove-if-not (lambda (pair) (string-equal action (car pair))) map)
                   map)))
    (cond ((null map) "no input action is declared in project.godot")
          ((null kept) (format nil "project.godot declares no input action ~s" action))
          (t (format nil "~d input action~:p:~%~{~a~^~%~}"
                     (length kept)
                     (loop for (name . value) in kept
                           for events = (mapcar #'event-label (input-events value))
                           collect (format nil "  ~a: ~{~a~^, ~}" name
                                           (or events (list "no event")))))))))

(defun count-project-files (extensions)
  "How many files of these types the project folder holds."
  (length (project-files :extensions extensions)))

(defun project-summary-text ()
  "The project in a few lines: name, main scene, autoloads, actions, and how
many scenes and scripts are on disk."
  (let ((ini (project-ini)))
    (format nil "Godot project ~a~%main scene ~a~%autoloads ~d, input actions ~d~%on disk: ~d scene~:p, ~d script~:p, ~d resource file~:p"
            (plain-value (or (ini-get ini "application" "config/name") "\"\""))
            (plain-value (or (ini-get ini "application" "run/main_scene") "(none)"))
            (length (ini-section ini "autoload"))
            (length (ini-section ini "input"))
            (count-project-files '("tscn"))
            (count-project-files '("gd" "cs"))
            (count-project-files '("tres" "res")))))
