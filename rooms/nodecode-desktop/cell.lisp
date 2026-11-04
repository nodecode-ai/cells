;;;; cell.lisp --- /desktop: this home's page in a window of the organism's own.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The desktop app without Electron. /desktop opens the page `nodecode web'
;;;; opens, served by this organism's own gateway on 127.0.0.1, in a window
;;;; this process draws through lisptron: the OS's own web engine (WebKitGTK
;;;; on Linux), driven from Lisp in-process. No Node, no bundled Chromium,
;;;; no second program: the window lives in the organism, and its page is the
;;;; web cell's page, unchanged, so whatever a browser tab shows, the window
;;;; shows.
;;;;
;;;;   /desktop              open the window, or bring the open one forward
;;;;   /desktop status       the window, and the page's own word on its gateway
;;;;   /desktop look [PATH]  what the window shows, as a PNG: an agent's eyes
;;;;   /desktop close        close it
;;;;
;;;; The page's address carries the operator token after its #, as `nodecode
;;;; web' prints it: it goes to the web engine and nowhere else, never into
;;;; an answer or a notice.
;;;;
;;;; Config, a sibling top-level key next to `cron' and `memory':
;;;;   "desktop": {"open": true}    open the window when the organism starts

(defpackage #:nodecode-desktop
  (:use #:cl))

(in-package #:nodecode-desktop)

(nlk:define-peripheral desktop :not-running t)

(defparameter *title* "Nodecode")
(defparameter *size* '(1280 820)
  "The window's first width and height, in pixels; the operator resizes it.")

(defvar *window* nil
  "The window /desktop opened, while it is open.")

(defvar *page-state* nil
  "The page's own last word on its connection: #app's data-state, which reads
open once its /gateway socket has the gateway's acknowledgement, else
connecting or closed.")

;;; The page is not changed: this reads what it already says about itself,
;;; and says it to the window's one verb when it moves, and every few
;;; seconds besides, so a page that reloads is heard again.
(defparameter *preload*
  "(()=>{let last='',n=0;setInterval(()=>{if(!window.lisptron)return;
const a=document.getElementById('app');const s=a?(a.dataset.state||'starting'):'loading';
if(s!==last||++n%10===0){last=s;n=0;lisptron.ask('desktop-report',s)}},500)})();")

(lt:defverb desktop-report (state)
  (setf *page-state* state)
  "")

(defun open-p ()
  (let ((window *window*))
    (and window (member window lt:*windows*) t)))

(defun page-url ()
  "This organism's page, the link `nodecode web' prints, or NIL while its
gateway does not listen."
  (multiple-value-bind (url token) (nle:gateway-endpoint)
    (and url token (format nil "~a/web/#t=~a" url token))))

(defun display-p ()
  (or (uiop:getenv "WAYLAND_DISPLAY") (uiop:getenv "DISPLAY")))

(defun open-desktop ()
  "Open the window, or bring the open one forward. => the answer's text."
  (cond
    ((open-p)
     (lt:present-window *window*)
     "desktop: the window is open; brought it forward")
    ((not (uiop:os-unix-p))
     "desktop: windows open on Linux for now; macOS and Windows come next")
    ((not (display-p))
     "desktop: this organism has no display to open a window on (neither WAYLAND_DISPLAY nor DISPLAY is set where it runs)")
    (t
     (let ((url (page-url)))
       (if (null url)
           "desktop: this organism's gateway is not listening, so there is no page to open yet"
           (handler-case
               (progn
                 (setf *page-state* nil
                       *window* (lt:open-window :title *title*
                                                :width (first *size*) :height (second *size*)
                                                :url url :bridge t :preload *preload*))
                 "desktop: opened this home's page in a window of its own")
             (error (e) (format nil "desktop: no window: ~a" e))))))))

(defun status ()
  (if (open-p)
      (format nil "desktop: window open · page ~a~:[~; · gateway connected~]"
              (cond ((not (lt:window-ready *window*)) "loading")
                    (*page-state*)
                    (t "starting"))
              (equal *page-state* "open"))
      "desktop: no window open · /desktop opens one"))

(defun look (path)
  (cond ((not (open-p)) "desktop: no window open to look at")
        (t (let ((path (if (plusp (length path)) path (namestring (nlk:home "desktop/look.png")))))
             (ensure-directories-exist path)
             (if (lt:snapshot *window* path)
                 (format nil "desktop: wrote what the window shows to ~a" path)
                 "desktop: the web engine gave no picture in time")))))

(defun close-desktop ()
  (cond ((open-p)
         (lt:close-window *window*)
         (setf *window* nil *page-state* nil)
         "desktop: closed the window")
        (t "desktop: no window open")))

(defun run-command (args session-id)
  "One /desktop invocation => the answer's text."
  (declare (ignore session-id))
  (let* ((words (string-trim " " (or args "")))
         (space (position #\Space words))
         (verb (string-downcase (subseq words 0 space)))
         (rest (if space (string-trim " " (subseq words space)) "")))
    (cond ((member verb '("" "open") :test #'string=) (open-desktop))
          ((string= verb "status") (status))
          ((string= verb "look") (look rest))
          ((string= verb "close") (close-desktop))
          (t "desktop: /desktop opens the window; /desktop status, /desktop look [PATH], /desktop close"))))

(nle:define-cell desktop
  (:section ("desktop")
    (:guide "nothing here is required; /desktop opens the window")
    ("open" :boolean :default nil :doc "open the window when the organism starts"))
  (:command "desktop" 'run-command
            :description "Open nodecode in a desktop window of its own"
            :argument-hint "[status | look [PATH] | close]")
  (:start (lambda ()
            (when (setting :open)
              ;; the gateway may still be coming up: the window waits for it
              ;; on a thread of its own, never holding up the start
              (sb-thread:make-thread
               (lambda ()
                 (loop repeat 60 until (page-url) do (sleep 0.5))
                 (let ((answer (open-desktop)))
                   (unless (open-p) (nle:notice answer :key "nodecode-desktop"))))
               :name "nodecode-desktop open"))
            (nle:on-stop (lambda ()
                           (when (open-p) (lt:close-window *window*))
                           (setf *window* nil *page-state* nil)
                           (nle:notice nil :key "nodecode-desktop"))))))
