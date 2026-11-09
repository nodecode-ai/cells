;;;; computer.lisp --- which app this cell drives, and what it shows.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original keeps the target app, its window and the element list the
;;;; model's numbers refer to in the backend; so does this cell, in
;;;; *TARGET*, and every gesture names a control by the number a capture
;;;; printed. A host switch voids the lot: app ids and numbers from the
;;;; other machine mean nothing here, as the original's host_changed says.

(in-package #:nodecode-alans-way)

(defvar *target* nil
  "The app this cell is driving, as a plist: :PID :APP :WINDOW-ID
:GENERATION :HOST :OS and :ELEMENTS, the last a vector read 1-based.")

(defun current (key) (getf *target* key))
(defun current-pid () (current :pid))
(defun current-app () (current :app))
(defun current-window () (current :window-id))
(defun current-elements () (or (current :elements) #()))

(defun forget-elements ()
  (setf (getf *target* :elements) nil
        (getf *target* :generation) nil)
  *target*)

(defun seen-host (host)
  "Record HOST. A host other than the one the element list was read on voids
that list and the app id with it, as the original's host_changed does."
  (let ((before (current :host)))
    (when (and host before (not (equal host before)))
      (setf *target* (list :host host)))
    (when host (setf (getf *target* :host) host)))
  host)

(defun target (pid app window)
  "Bind the cell to PID and APP and WINDOW, forgetting the element list when
any of the three changed."
  (when (or (not (eql pid (current-pid)))
            (not (equal app (current-app)))
            (not (eql window (current-window))))
    (forget-elements))
  (setf (getf *target* :pid) pid
        (getf *target* :app) app
        (getf *target* :window-id) window)
  *target*)

(defun element-at (index)
  "The control numbered INDEX in the last capture, or NIL."
  (let ((elements (current-elements)))
    (and (integerp index) (<= 1 index (length elements)) (aref elements (1- index)))))

(defun element-label (element)
  "ELEMENT's own words: its title, else its name, else its description."
  (or (nlk:json-value element :string "title")
      (nlk:json-value element :string "name")
      (nlk:json-value element :string "description")
      ""))

(defun element-line (index element)
  "One control as the model reads it."
  (format nil " ~d. ~a~@[ ~s~]~@[ at (~a, ~a) ~ax~a~]~@[ value ~s~]~@[ disabled~]"
          index
          (or (nlk:json-value element :string "role") "?")
          (let ((label (element-label element)))
            (and (plusp (length label)) label))
          (nlk:json-value element :any "x") (nlk:json-value element :any "y")
          (nlk:json-value element :any "width") (nlk:json-value element :any "height")
          (nlk:json-value element :string "value")
          (nlk:json-value element :boolean "disabled")))

(defun app-name (app)
  (or (nlk:json-value app :string "name") "?"))

(defun app-lines (body)
  "BODY's apps, one line each, and the host they run on."
  (let ((apps (nlk:json-value body :array "apps")))
    (with-output-to-string (out)
      (format out "On ~a~@[ (~a)~]:"
              (or (current :host) "the workspace")
              (nlk:json-value body :string "host"))
      (if (or (null apps) (zerop (length apps)))
          (format out "~% no app is running")
          (loop for app across apps
                for index from 1
                do (format out "~% ~d. ~a (pid ~a)~a~a"
                           index (app-name app)
                           (nlk:json-value app :any "pid")
                           (if (nlk:json-value app :boolean "frontmost") " [frontmost]" "")
                           (let ((bundle (nlk:json-value app :string "bundleId")))
                             (if bundle (format nil " ~a" bundle) ""))))))))

(defun list-apps ()
  "Every app the workspace can drive, as its own reply describes it."
  (multiple-value-bind (body host) (ask "workspace_computer_apps" (nlk:json-object))
    (seen-host host)
    (setf (getf *target* :os) (nlk:json-value body :string "host"))
    body))

(defun matching (app apps)
  "APPS whose name or bundle id is APP, or else contains it."
  (let* ((wanted (string-downcase (string-trim '(#\Space) app)))
         (field (lambda (entry key) (string-downcase (or (nlk:json-value entry :string key) ""))))
         (exact (remove-if-not (lambda (entry)
                                  (or (equal wanted (funcall field entry "name"))
                                      (equal wanted (funcall field entry "bundleId"))))
                                apps)))
    (or exact
        (remove-if-not (lambda (entry)
                         (or (search wanted (funcall field entry "name"))
                             (search wanted (funcall field entry "bundleId"))))
                       apps))))

(defun runnning-names (apps)
  (format nil "~{~a~^, ~}" (mapcar #'app-name apps)))

(defun launch-arguments (pairs host)
  "PAIRS as an object, with the host this cell believes it is on pinned."
  (apply #'nlk:make-json-object
         (append pairs
                 (when host
                   (list "expect_host"
                         (if (member host '("vps" "vm") :test #'equal) "vm" "computer"))))))

(defun launch-app (app)
  "Ask the workspace to launch APP, when it advertises that it can."
  (multiple-value-bind (status host) (ask "cua_alans_way_status" (nlk:json-object))
    (let ((capabilities (nlk:json-value status :array "capabilities")))
      (unless (and capabilities (find "computer-launch-v1" (coerce capabilities 'list)
                                      :test #'equal))
        (fail "No running app matches ~s, and this workspace cannot launch one by name." app))
      (multiple-value-bind (launched answer-host)
          (ask "workspace_computer_launch" (launch-arguments (list "app" app) host))
        (let ((pid (nlk:json-value launched :any "pid")))
          (unless (and (integerp pid) (plusp pid))
            (fail "The workspace could not launch ~s." app))
          (seen-host (or answer-host host))
          (values pid (or (nlk:json-value launched :string "name") app)))))))

(defun resolve-app (app &key launch)
  "The pid and name APP names. With LAUNCH a missing app the host can launch
is launched; otherwise the running apps are the refusal's next step."
  (let* ((body (list-apps))
         (apps (coerce (or (nlk:json-value body :array "apps") #()) 'list))
         (hits (matching app apps)))
    (cond ((and (= 1 (length hits)) (integerp (nlk:json-value (first hits) :any "pid")))
           (values (nlk:json-value (first hits) :any "pid") (app-name (first hits))))
          ((rest hits)
           (fail "Several apps match ~s: ~a; name one exactly." app (runnning-names hits)))
          (launch (launch-app app))
          (t (fail "No running app matches ~s: ~a." app (runnning-names apps))))))

;;; --- a capture --------------------------------------------------------------

(defun screenshot (pid window)
  "The window's screenshot item, or NIL when the router sends none."
  (let ((result (funcall *router-call* "workspace_computer_screenshot"
                          (apply #'nlk:make-json-object
                                 (list* "pid" pid (when window (list "windowId" window)))))))
    (image-of result)))

(defun save-image (image)
  "IMAGE written under the home; answers its path."
  (let ((data (nlk:json-value image :string "data")))
    (unless (and data (plusp (length data)))
      (fail "the router sent an image with no data"))
    (let* ((directory (ensure-directories-exist
                       (if (setting :screenshot-dir)
                           (uiop:ensure-directory-pathname (setting :screenshot-dir))
                           (nlk:home "computer/"))))
           (path (merge-pathnames (format nil "capture-~d.png" (get-universal-time)) directory)))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
        (write-sequence (cl-base64:base64-string-to-usb8-array data) out))
      (uiop:native-namestring path))))

(defun snapshot (pid window)
  "The target's element tree: a whole list, never a delta this cell cannot
fold (a capture always answers what the numbers mean now)."
  (multiple-value-bind (body host notes)
      (ask "workspace_computer_snapshot"
           (apply #'nlk:make-json-object
                  (list* "pid" pid "delta" nil (when window (list "windowId" window)))))
    (seen-host host)
    (values body notes)))

(defun named-controls (elements)
  (some (lambda (element) (plusp (length (element-label element)))) elements))

(defun capture (app pid window mode)
  "Read APP's (or PID's) window and answer the text the model reads. MODE is
som (the numbers alone when they name enough of the window), vision (the
screenshot too) or ax (the numbers, never a screenshot)."
  (let ((name nil))
    (cond (pid (setf name (or app (and (eql pid (current-pid)) (current-app)) (format nil "pid ~a" pid))))
          (app (multiple-value-setq (pid name) (resolve-app app)))
          ((current-pid) (setf pid (current-pid) name (current-app)))
          (t (fail "capture needs app (a name from computer_apps) or pid; the focused app is off limits")))
    (when (and window (not (and (integerp window) (<= 0 window #xffffffff))))
      (fail "window_id is a window number, not ~s" window))
    (let* ((bound (and (eql pid (current-pid)) (current-window)))
           (wanted (or window bound)))
      (multiple-value-bind (body notes) (snapshot pid wanted)
        (let* ((shown (nlk:json-value body :any "windowId"))
               (elements (coerce (or (nlk:json-value body :array "elements") #()) 'list)))
          (when (and shown bound (not (eql shown bound)))
            (fail "stale_window: the reply does not identify window ~a; capture again" bound))
          (target pid name (or shown wanted))
          (setf (getf *target* :generation) (nlk:json-value body :any "generation"))
          (setf (getf *target* :elements) (coerce elements 'vector))
          (let ((text (with-output-to-string (out)
                        (format out "~a (pid ~a)~@[, window ~a~] on ~a: ~d controls."
                                name pid (or shown wanted) (or (current :host) "the workspace")
                                (length elements))
                        (loop for element in elements for index from 1
                              do (format out "~%~a" (element-line index element)))
                        (dolist (note notes)
                          (when (plusp (length (string-trim '(#\Space) note)))
                            (format out "~%~a" note))))))
            (cond ((equal mode "ax") text)
                  ((and (equal mode "som") (named-controls elements))
                   (concatenate 'string text
                                "~%No screenshot is attached: act on these numbers; mode \"vision\" shows the window."))
                  (t (let ((image (screenshot pid (or shown wanted))))
                       (if image
                           (format nil "~a~%Screenshot: ~a (~a)" text (save-image image)
                                   (or (nlk:json-value image :string "mimeType") "image"))
                           (concatenate 'string text "~%The router sent no screenshot.")))))))))))
