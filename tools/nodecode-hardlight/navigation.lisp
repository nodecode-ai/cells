;;;; navigation.lisp --- a declared page graph, its task paths and its defects.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's hardlight/navigation.py, in Lisp: the caller declares the
;;;; pages, their links and the tasks that must be finishable, and gets back
;;;; the shortest path per task in the caller's own link order, plus unknown
;;;; targets, one label reaching several pages, unreachable pages and dead
;;;; ends. Structural only: no browser, no usability certificate.

(in-package #:nodecode-hardlight)

(defparameter +navigation-limits+
  '((:pages . 300) (:tasks . 100) (:links-per-page . 1000) (:links-total . 10000)
    (:id-length . 200) (:label-length . 500) (:max-steps . 1000000))
  "The bounds one call is held to, as the original plugin holds it.")

(defparameter +navigation-limitations+
  '("Structural shortest paths are not usability certification."
    "Only declared links are analyzed; browser history, permissions, keyboard behavior and real user tasks are not verified.")
  "What the answer does not establish, said in the answer.")

(defun limit (key) (cdr (assoc key +navigation-limits+)))

(defun nav-text-ok-p (text maximum)
  "TEXT as an ID or a label: nonblank, short enough, no control characters."
  (and (stringp text)
       (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
       (<= (length text) maximum)
       (notany (lambda (character) (< (char-code character) 32)) text)))

;;; --- the boundary -----------------------------------------------------------

(nlk:define-contract nav-link
  (:doc "One link a page offers: where it goes and the literal label it carries.")
  ("to" :text :required t :doc "the destination page's ID, as the page declares it")
  ("label" :text :required t :doc "the literal label a person reads on the link"))

(nlk:define-contract nav-page
  (:doc "One page: its ID, the label navigation shows for it, and its outgoing links in the order they appear.")
  ("id" :text :required t :doc "the page's ID, unique in the graph")
  ("label" :text :required t :doc "the page's own label, as navigation shows it")
  ("links" :array :of nav-link :doc "0..1000 outgoing links, in reading order"))

(nlk:define-contract nav-task
  (:doc "One path that must be finishable: a name, the page it starts on, the page it must reach, and optionally the steps allowed.")
  ("name" :text :required t :doc "what the task is, as a person would say it")
  ("start" :text :required t :doc "the ID of the page the task starts on")
  ("goal" :text :required t :doc "the ID of the page the task must reach")
  ("max_steps" :integer :default -1
               :doc "the steps the path may take, when one is set; absent (-1) applies no click rule"))

(nlk:define-contract navigation-call
  (:doc "Analyze a declared page graph and its task paths: shortest paths in link order, unknown targets, labels that reach several pages, unreachable pages and dead ends. Structural evidence only, not usability certification.")
  ("pages" :array :of nav-page :required t :doc "1..300 pages of the site or app")
  ("tasks" :array :of nav-task :doc "0..100 paths that must be finishable")
  ("home" :string :default "" :doc "the ID of the entry page; empty takes the first page declared"))

;;; --- the analysis -----------------------------------------------------------

(defun nav-validate (pages tasks home)
  "NIL when the graph is well formed, otherwise the one reason it is not."
  (let ((page-count (length pages)))
    (cond
      ((or (zerop page-count) (> page-count (limit :pages)))
       (format nil "pages must declare 1..~a pages" (limit :pages)))
      ((> (length tasks) (limit :tasks))
       (format nil "tasks must be 0..~a task paths" (limit :tasks)))
      (t
       (let ((ids (make-hash-table :test #'equal)) (links 0) (why nil))
         (loop for page across pages
               for index from 0
               until why
               do (let ((id (nav-page-id page)) (label (nav-page-label page))
                        (page-links (nav-page-links page)))
                    (cond
                      ((not (nav-text-ok-p id (limit :id-length)))
                       (setf why (format nil "pages[~a].id must be at most ~a characters without control characters"
                                         index (limit :id-length))))
                      ((gethash id ids) (setf why (format nil "duplicate page ID at pages[~a].id" index)))
                      ((not (nav-text-ok-p label (limit :label-length)))
                       (setf why (format nil "pages[~a].label must be at most ~a characters without control characters"
                                         index (limit :label-length))))
                      (t
                       (setf (gethash id ids) page)
                       (when (> (length page-links) (limit :links-per-page))
                         (setf why (format nil "pages[~a].links must be at most ~a links"
                                           index (limit :links-per-page))))
                       (incf links (length page-links))
                       (when (> links (limit :links-total))
                         (setf why "the graph declares more than 10000 total links"))
                       (loop for link across page-links
                             until why
                             do (unless (and (nav-text-ok-p (nav-link-to link) (limit :id-length))
                                             (nav-text-ok-p (nav-link-label link) (limit :label-length)))
                                  (setf why (format nil "pages[~a].links needs a destination and a label within the limits"
                                                    index))))))))
         (cond
           (why why)
           ((and (plusp (length home)) (not (gethash home ids)))
            "home must name a declared page ID")
           (t
            (loop for task across tasks
                  for index from 0
                  until why
                  finally (return why)
                  do (let ((start (nav-task-start task)) (goal (nav-task-goal task))
                           (steps (nav-task-max-steps task)))
                       (unless (and (nav-text-ok-p (nav-task-name task) (limit :label-length))
                                    (nav-text-ok-p start (limit :id-length))
                                    (nav-text-ok-p goal (limit :id-length)))
                         (setf why (format nil "tasks[~a] needs a name, a start and a goal within the limits" index)))
                       (when (and (null why) (or (not (gethash start ids)) (not (gethash goal ids))))
                         (setf why (format nil "tasks[~a] must reference declared page IDs" index)))
                       (when (and (null why) (or (< steps -1) (> steps (limit :max-steps))))
                         (setf why (format nil "tasks[~a].max_steps must be 0..~a" index (limit :max-steps)))))))))))))

(defun nav-shortest-path (adjacency start goal)
  "The shortest path START to GOAL as a list of IDs, ties kept in link order, or NIL."
  (let ((parents (make-hash-table :test #'equal))
        (queue (list start))
        (found nil))
    (setf (gethash start parents) nil)
    (loop while (and queue (not found))
          do (let ((node (pop queue)))
               (if (equal node goal)
                   (setf found t)
                   (dolist (target (gethash node adjacency))
                     (unless (nth-value 1 (gethash target parents))
                       (setf (gethash target parents) node)
                       (setf queue (append queue (list target))))))))
    (when found
      (let ((path '()) (node goal))
        (loop while node
              do (push node path)
                 (setf node (gethash node parents)))
        path))))

(defun nav-reachable (adjacency home)
  "Every ID reachable from HOME."
  (let ((seen (make-hash-table :test #'equal)) (queue (list home)))
    (setf (gethash home seen) t)
    (loop while queue
          do (dolist (target (gethash (pop queue) adjacency))
               (unless (gethash target seen)
                 (setf (gethash target seen) t)
                 (setf queue (append queue (list target))))))
    seen))

(defun analyze-navigation (call)
  "hardlight_navigation: the declared graph of CALL analyzed, as JSON."
  (let* ((pages (navigation-call-pages call))
         (tasks (navigation-call-tasks call))
         (home (or (navigation-call-home call) "")))
    (let ((why (nav-validate pages tasks home)))
      (if why
          (nlk:encode-json-object (nlk:json-object "ok" nil "error" why))
          (let* ((ids (make-hash-table :test #'equal))
                 (adjacency (make-hash-table :test #'equal))
                 (entry (if (plusp (length home)) home (nav-page-id (aref pages 0))))
                 (unknown '()) (ambiguous '()) (goals '()))
            (loop for page across pages
                  do (setf (gethash (nav-page-id page) ids) page))
            (loop for page across pages
                  do (let ((source (nav-page-id page)) (labels (make-hash-table :test #'equal))
                           (targets '()))
                       (loop for link across (nav-page-links page)
                             do (let ((to (nav-link-to link)) (label (nav-link-label link)))
                                  (if (gethash to ids)
                                      (progn (push to targets)
                                             (pushnew to (gethash label labels) :test #'equal))
                                      (push (nlk:json-object "source" source "to" to "label" label)
                                            unknown))))
                       (setf (gethash source adjacency) (nreverse targets))
                       (maphash (lambda (label destinations)
                                  (when (> (length destinations) 1)
                                    (push (nlk:json-object "source" source "label" label
                                                            "targets" (coerce (nreverse destinations) 'vector))
                                          ambiguous)))
                                labels)))
            (loop for task across tasks do (push (nav-task-goal task) goals))
            (let ((reachable (nav-reachable adjacency entry))
                  (results '()) (dead-ends '()) (unreachable '()))
              (loop for task across tasks
                    do (let* ((path (nav-shortest-path adjacency (nav-task-start task) (nav-task-goal task)))
                              (steps (and path (1- (length path))))
                              (budget (nav-task-max-steps task)))
                         (push (nlk:json-object
                                "name" (nav-task-name task)
                                "start" (nav-task-start task)
                                "goal" (nav-task-goal task)
                                "path" (if path (coerce path 'vector) :null)
                                "steps" (if steps steps :null)
                                "reachable" (and path t)
                                "over_budget" (if (and steps (>= budget 0) (> steps budget)) t nil))
                               results)))
              (loop for page across pages
                    do (let ((id (nav-page-id page)))
                         (unless (gethash id reachable) (push id unreachable))
                         (when (and (null (gethash id adjacency)) (not (member id goals :test #'equal)))
                           (push id dead-ends))))
              (nlk:encode-json-object
               (nlk:json-object
                "ok" t
                "home" entry
                "tasks" (coerce (nreverse results) 'vector)
                "unknown_targets" (coerce (nreverse unknown) 'vector)
                "ambiguous_labels" (coerce (nreverse ambiguous) 'vector)
                "unreachable_pages" (coerce (coerce (nreverse unreachable) 'list) 'vector)
                "dead_ends" (coerce (coerce (nreverse dead-ends) 'list) 'vector)
                "coverage" (nlk:json-object
                            "page_count" (length pages)
                            "edge_count" (loop for page across pages
                                               sum (length (nav-page-links page)))
                            "known_edge_count" (loop for page across pages
                                                     sum (length (gethash (nav-page-id page) adjacency))))
                "limitations" (coerce (copy-list +navigation-limitations+) 'vector)))))))))
