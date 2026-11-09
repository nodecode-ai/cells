;;;; overlay.lisp --- the marks on the screen: one Tcl process, and the scene it paints.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The overlay is a Tk window the cell owns: a borderless, always-on-top,
;;;; per-pixel transparent canvas over the captured window, painted from a
;;;; scene script the cell writes. A small daemon (wish, the Tcl/Tk interpreter
;;;; every Mac that has Tk ships) polls the scene's revision and repaints when
;;;; it changes, so a draw is one file write and needs no socket; the daemon
;;;; writes a heartbeat the cell reads back for status.
;;;;
;;;; Nothing here types or clicks in the user's app: the marks are a picture
;;;; of where a control is. A click on the marks dismisses them, since a plain
;;;; Tk window cannot ignore the mouse (see NOTICE).

(in-package #:nodecode-hud-teach)

;;; --- the files the daemon and the cell share ---------------------------------------

(defun overlay-file (&optional (dir (state-dir)))
  (merge-pathnames "overlay.tcl" dir))

(defun scene-file (&optional (dir (state-dir)))
  (merge-pathnames "scene.tcl" dir))

(defun revision-file (&optional (dir (state-dir)))
  (merge-pathnames "revision" dir))

(defun heartbeat-file (&optional (dir (state-dir)))
  (merge-pathnames "heartbeat" dir))

(defun pid-file (&optional (dir (state-dir)))
  (merge-pathnames "overlay.pid" dir))

(defun overlay-state-file (&optional (dir (state-dir)))
  (merge-pathnames "state" dir))

(defun overlay-log-file (&optional (dir (state-dir)))
  (merge-pathnames "overlay.log" dir))

(defun write-text (path text)
  "TEXT to PATH, readably, 0600."
  (nlk:write-file-atomically path text :mode #o600 :directory-mode #o700)
  path)

(defun read-text (path)
  "PATH's text, or \"\" when it is not there."
  (if (probe-file path)
      (or (ignore-errors (uiop:read-file-string path)) "")
      ""))

(defun file-integer (path)
  "The integer PATH holds, or NIL."
  (ignore-errors (parse-integer (string-trim '(#\Space #\Newline #\Return)
                                             (read-text path)))))

;;; --- a Tcl string ---------------------------------------------------------------

(defun tcl-string (text)
  "TEXT as a Tcl double-quoted word: a caption the model wrote cannot end the
string, interpolate, or run a command."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for character across (or text "")
          do (case character
               (#\\ (write-string "\\\\" out))
               (#\" (write-string "\\\"" out))
               (#\[ (write-string "\\[" out))
               (#\] (write-string "\\]" out))
               (#\$ (write-string "\\$" out))
               (#\Newline (write-string "\\n" out))
               (t (write-char character out))))
    (write-char #\" out)))

;;; --- the daemon ------------------------------------------------------------------

(defun overlay-script (dir)
  "The Tcl daemon's source: a transparent always-on-top window that repaints
from the scene file whenever its revision changes, beats its heartbeat, and
quits after `idle_exit_seconds' without a new scene."
  (format nil "# Nodecode Teach HUD: the overlay process. Written by the cell; it ~
repaints when the scene's revision changes.~%~
package require Tk~%~
set hud ~a~%~
proc hud_read {path} {~%~
    if {[catch {open $path r} f]} { return {} }~%~
    set text [read $f]~%~
    close $f~%~
    return $text~%~
}~%~
proc hud_state {name value} {~%~
    catch {~%~
        set f [open [file join $::hud $name] w]~%~
        puts -nonewline $f $value~%~
        close $f~%~
    }~%~
}~%~
proc hud_quit {} {~%~
    hud_state state {visible 0 idle}~%~
    catch { destroy .hud }~%~
    exit~%~
}~%~
wm withdraw .~%~
toplevel .hud~%~
wm title .hud {Nodecode Teach HUD}~%~
wm overrideredirect .hud 1~%~
wm attributes .hud -topmost 1~%~
catch { wm attributes .hud -transparent 1 }~%~
canvas .hud.c -highlightthickness 0 -bd 0~%~
catch { .hud.c configure -background systemTransparent }~%~
pack .hud.c -fill both -expand 1~%~
bind .hud <Button-1> { catch { wm withdraw .hud }; hud_state state {visible 0 click} }~%~
set ::hud_rev {}~%~
proc hud_poll {} {~%~
    set rev [hud_read [file join $::hud revision]]~%~
    if {[string length $rev] && $rev ne $::hud_rev} {~%~
        set ::hud_rev $rev~%~
        set err [catch { uplevel #0 [hud_read [file join $::hud scene.tcl]] } why]~%~
        if {$err} { hud_state error $why }~%~
    }~%~
    after 150 hud_poll~%~
}~%~
proc hud_beat {} {~%~
    hud_state heartbeat [clock seconds]~%~
    after 2000 hud_beat~%~
}~%~
hud_state pid [pid]~%~
hud_beat~%~
hud_poll~%~
after [expr {~d * 1000}] hud_quit~%"
          (tcl-string (namestring dir))
          (or (setting :idle-exit-seconds) 1800)))

(defun write-overlay-script ()
  "The daemon's file, rewritten when its settings changed."
  (let ((dir (state-dir)))
    (write-text (overlay-file dir) (overlay-script dir))))

(defun launch-overlay ()
  "Start the daemon detached, its output in the state folder. A launch never
signals: the wait for its heartbeat is what reports a Mac without Tk."
  (let* ((dir (state-dir))
         (script (overlay-file dir))
         (log (overlay-log-file dir)))
    (ignore-errors (delete-file (heartbeat-file dir)))
    (handler-case
        (uiop:launch-program (list (tcl-interpreter) (namestring script))
                              :output log :error-output log
                              :if-output-exists :append :if-error-output-exists :append)
      (error () nil))))

(defvar *launch* #'launch-overlay
  "How the daemon is started: a function of no arguments. A test binds this.")

;;; --- which Tcl ---------------------------------------------------------------

(defvar *tcl-interpreter* nil
  "The interpreter the last probe found, so a draw does not probe twice.

The section's `tcl' names one; without it, the first of wish, wish8.6 and
wish8.5 that runs a script.")

(defun probe-interpreter (program dir)
  "PROGRAM's Tcl version when it runs a script, else NIL."
  (let ((probe (merge-pathnames "probe.tcl" dir)))
    (write-text probe "puts [info patchlevel]")
    (multiple-value-bind (out err status) (run (list program (namestring probe)))
      (declare (ignore err))
      (and (zerop status) (plusp (length (string-trim '(#\Space #\Newline) out)))
           (string-trim '(#\Space #\Newline) out)))))

(defun tcl-interpreter ()
  "The Tcl/Tk interpreter a draw paints with, refusing with the fix when the
Mac has none."
  (or *tcl-interpreter*
      (setf *tcl-interpreter*
            (let ((dir (state-dir)))
              (or (loop for program in (remove-duplicates
                                         (remove nil (list (setting :tcl) "wish" "wish8.6" "wish8.5"))
                                         :test #'string=)
                        for version = (probe-interpreter program dir)
                        when version return program)
                  (fail "no Tcl/Tk interpreter answered (tried ~{~a~^, ~}); install Tk or name one in the cell's tcl setting"
                        (remove nil (list (setting :tcl) "wish" "wish8.6" "wish8.5"))))))))

;;; --- is it up ------------------------------------------------------------------

(defun heartbeat-age ()
  "Seconds since the daemon last beat, or NIL when it never did."
  (let ((beat (file-integer (heartbeat-file))))
    (and beat (max 0 (- (funcall *now*) beat)))))

(defun overlay-running-p ()
  "Whether a daemon is alive: its heartbeat is younger than +HEARTBEAT-MAX+."
  (let ((age (heartbeat-age)))
    (and age (<= age +heartbeat-max+))))

(defun ensure-overlay ()
  "The daemon, started and beating, or a refusal naming what is missing."
  (when (overlay-running-p)
    (return-from ensure-overlay t))
  (write-overlay-script)
  (ignore-errors (delete-file (revision-file)))
  (funcall *launch*)
  (loop repeat 50
        do (when (overlay-running-p) (return-from ensure-overlay t))
           (funcall *sleep* 0.1))
  (fail "the overlay did not start: ~a"
        (let ((log (string-trim '(#\Space #\Newline) (read-text (overlay-log-file)))))
          (if (plusp (length log)) log "no output from the interpreter"))))

;;; --- a scene ------------------------------------------------------------------

(defun shape-line (out shape)
  "Emit SHAPE's marks: the outline in its colour, its badge when it names a
step, and its caption."
  (let* ((kind (shape-string shape "kind"))
         (color (color-hex (or (shape-string shape "color") "red")))
         (dashed (nlk:json-value shape :boolean "dashed"))
         (text (shape-string shape "text"))
         (label (shape-string shape "label"))
         (step (nlk:json-value shape :integer "step"))
         (dash (if dashed " -dash {6 4}" "")))
    (flet ((px (key) (format nil "[expr {~a * $SX}]" (shape-number shape key))))
      (cond
        ((string= kind "circle")
         (format out "~%# circle~%.hud.c create oval [expr {(~a - ~a) * $SX}] [expr {(~a - ~a) * $SY}] \
[expr {(~a + ~a) * $SX}] [expr {(~a + ~a) * $SY}] -outline ~a -width 3~a"
                 (shape-number shape "x") (shape-number shape "radius")
                 (shape-number shape "y") (shape-number shape "radius")
                 (shape-number shape "x") (shape-number shape "radius")
                 (shape-number shape "y") (shape-number shape "radius")
                 color dash))
        ((string= kind "rect")
         (format out "~%# rect~%.hud.c create rectangle ~a ~a [expr {(~a + ~a) * $SX}] [expr {(~a + ~a) * $SY}] \
-outline ~a -width 3~a~@[ -fill ~a~]"
                 (px "x") (px "y")
                 (shape-number shape "x") (shape-number shape "width")
                 (shape-number shape "y") (shape-number shape "height")
                 color dash
                 (and (nlk:json-value shape :boolean "fill") (halo-hex (or (shape-string shape "color") "red")))))
        ((member kind '("arrow" "line") :test #'string=)
         (format out "~%# ~a~%.hud.c create line ~a ~a ~a ~a -fill ~a -width 3~a~a"
                 kind (px "fromX") (px "fromY") (px "toX") (px "toY") color dash
                 (if (string= kind "arrow") " -arrow last -arrowshape {14 18 6}" "")))
        ((string= kind "polyline")
         (format out "~%# polyline~%.hud.c create line ~{~a~^ ~} -fill ~a -width 3~a"
                 (loop for point in (shape-list (nlk:json-value shape :any "points"))
                       append (list (format nil "[expr {~a * $SX}]" (shape-number point "x"))
                                    (format nil "[expr {~a * $SY}]" (shape-number point "y"))))
                 color dash))
        (t
         (format out "~%# label~%.hud.c create text [expr {~a * $SX}] [expr {~a * $SY}] -text ~a \
-anchor sw -fill ~a -font {Helvetica 13 bold}"
                 (shape-number shape "x") (shape-number shape "y") (tcl-string text) color)))
      (when (and step (not (string= kind "label")))
        (let* ((anchor (first (shape-points shape)))
               (hx (+ (car anchor) 10))
               (hy (- (cdr anchor) 10))
               (ink (or (shape-string shape "color") "red")))
          (format out "~%# badge ~d~%.hud.c create oval [expr {(~a - 13) * $SX}] [expr {(~a - 13) * $SY}] \
[expr {(~a + 13) * $SX}] [expr {(~a + 13) * $SY}] -fill ~a -outline ~a -width 2~%\
.hud.c create text [expr {~a * $SX}] [expr {~a * $SY}] -text ~d -fill ~a -font {Helvetica 13 bold}"
                  step hx hy hx hy (halo-hex ink) (color-hex ink) hx hy step (color-hex ink))))
      (when (and label (plusp (length label)))
        (let ((anchor (first (shape-points shape))))
          (format out "~%.hud.c create text [expr {~a * $SX}] [expr {~a * $SY}] -text ~a \
-anchor nw -fill ~a -font {Helvetica 12}"
                  (+ (car anchor) 16) (- (cdr anchor) 4) (tcl-string label)
                  (color-hex (or (shape-string shape "color") "red"))))))))

(defun card-lines (title steps current)
  "The step card's text, one line each, the current step marked."
  (append (when (and title (plusp (length title)))
            (list (cons (format nil "~a" title) :title)))
          (loop for step in steps
                for n = (getf step :n)
                collect (cons (format nil "~:[~;-~] ~d. ~a"
                                      (getf step :done) n (getf step :text))
                              (cond ((getf step :done) :done)
                                    ((eql n current) :current)
                                    (t :rest))))))

(defun card-script (corner title steps current)
  "Emit the step card in CORNER: the lesson, what is done, what to do now."
  (let* ((lines (card-lines title steps current))
         (height (+ 24 (* 20 (length lines)))))
    (with-output-to-string (out)
      (format out "~%# step card~%set CARDW 380~%set CARDH ~d~%" height)
      (ecase corner
        (:top-left (format out "set CARDX 20~%set CARDY 24~%"))
        (:top-right (format out "set CARDX [expr {$HUD_W - $CARDW - 20}]~%set CARDY 24~%"))
        (:bottom-left (format out "set CARDX 20~%set CARDY [expr {$HUD_H - $CARDH - 24}]~%"))
        (:bottom-right (format out "set CARDX [expr {$HUD_W - $CARDW - 20}]~%set CARDY [expr {$HUD_H - $CARDH - 24}]~%")))
      (format out ".hud.c create rectangle $CARDX $CARDY [expr {$CARDX + $CARDW}] [expr {$CARDY + $CARDH}] \
-fill {#1C1C1E} -outline {#3A3A3C}~%")
      (loop for (text . kind) in lines
            for i from 0
            do (format out ".hud.c create text [expr {$CARDX + 14}] [expr {$CARDY + ~d}] -text ~a -anchor nw \
-fill ~a -width [expr {$CARDW - 28}] -justify left -font {Helvetica ~d~:[~; bold~]}~%"
                       (+ 12 (* 20 i)) (tcl-string text)
                       (case kind (:title "#F2F2F7") (:done "#8E8E93") (:current "#FFD60A") (t "#F2F2F7"))
                       (if (eq kind :title) 13 12) (member kind '(:title :current)))))))

(defun corner-key (corner)
  (intern (string-upcase corner) :keyword))

(defun scene-script (bounds frame-width frame-height shapes title steps current legend ttl)
  "The scene the daemon paints: the window's geometry over the captured
window, every shape in it, and the card."
  (with-output-to-string (out)
    (format out "# Nodecode Teach HUD scene. Coordinates are screenshot pixels scaled to the window.~%")
    (format out "# geometry ~dx~d+~d+~d~%set HUD_X ~d~%set HUD_Y ~d~%set HUD_W ~d~%set HUD_H ~d~%"
            (or (third bounds) 0) (or (fourth bounds) 0) (or (first bounds) 0) (or (second bounds) 0)
            (or (first bounds) 0) (or (second bounds) 0)
            (or (third bounds) 0) (or (fourth bounds) 0))
    (format out "wm deiconify .hud~%")
    (format out "if {$HUD_W <= 0 || $HUD_H <= 0} { set HUD_W [winfo screenwidth .hud]; \
set HUD_H [winfo screenheight .hud]; set HUD_X 0; set HUD_Y 0 }~%")
    (format out "wm geometry .hud ${HUD_W}x${HUD_H}+${HUD_X}+${HUD_Y}~%")
    (format out ".hud.c configure -width $HUD_W -height $HUD_H~%")
    (format out ".hud.c delete all~%")
    (format out "set SX [expr {double($HUD_W) / ~,1f}]~%set SY [expr {double($HUD_H) / ~,1f}]~%"
            (max 1 frame-width) (max 1 frame-height))
    (dolist (shape shapes)
      (shape-line out shape))
    (when (or (and title (plusp (length title))) steps)
      (write-string (card-script (corner-key legend) title steps current) out))
    (format out "hud_state drawn ~d~%hud_state state {visible 1}~%"
            (length shapes))
    (format out "catch { after cancel $::hud_ttl }~%")
    (format out "set ::hud_ttl [after ~d { catch { wm withdraw .hud }; hud_state state {visible 0 ttl} }]~%"
            (round (* 1000 ttl)))))

;;; --- drawing, clearing, asking ------------------------------------------------------

(defun bump-revision ()
  "A new revision number, so the daemon repaints exactly once."
  (let ((next (1+ (or (file-integer (revision-file)) 0))))
    (write-text (revision-file) (princ-to-string next))
    next))

(defun draw (bounds frame-width frame-height shapes title steps current legend ttl)
  "Paint SHAPES over BOUNDS, with the lesson card when there is one.
=> the overlay's own report."
  (ensure-overlay)
  (write-text (scene-file)
              (scene-script bounds frame-width frame-height shapes title steps current legend ttl))
  (bump-revision)
  (loop repeat 30
        until (search "visible 1" (read-text (overlay-state-file)))
        do (funcall *sleep* 0.05))
  (list :drawn (length (shape-list shapes)) :ttl ttl :title title
        :steps (length steps) :current-step current :legend legend))

(defun clear-overlay (&optional (reason "clear"))
  "Take the marks off the screen. => whether a daemon was up to take them."
  (let ((running (overlay-running-p)))
    (when running
      (write-text (scene-file)
                  (format nil ".hud.c delete all~%catch { wm withdraw .hud }~%hud_state state {visible 0 ~a}~%"
                          reason))
      (bump-revision))
    running))

(defun stop-overlay ()
  "Clear, then end the daemon: /teach stop, and the cell's own stop."
  (let ((running (clear-overlay "stop"))
        (pid (file-integer (pid-file))))
    (when (and running pid)
      (run (list "kill" (princ-to-string pid))))
    (ignore-errors (delete-file (heartbeat-file)))
    (setf *tcl-interpreter* nil)
    running))

(defun overlay-status ()
  "What is on screen: whether the daemon runs, whether marks are visible, and
which drawing they are."
  (let* ((running (overlay-running-p))
         (state (string-trim '(#\Space #\Newline) (read-text (overlay-state-file)))))
    (list :running running
          :age (heartbeat-age)
          :state (if (plusp (length state)) state "unknown")
          :visible (and running (search "visible 1" state) t)
          :drawn (and running (file-integer (merge-pathnames "drawn" (state-dir)))))))
