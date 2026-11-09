;;;; model.lisp --- the openspec/ folder as data: changes, tasks, specs, ideas.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; OpenSpec's artifact model, read and written with the file tools alone:
;;;;
;;;;   openspec/specs/<spec>/spec.md              the baseline spec
;;;;   openspec/changes/<change>/proposal.md      why, what changes, impact
;;;;   openspec/changes/<change>/design.md        trade-offs, when they matter
;;;;   openspec/changes/<change>/tasks.md         `- [ ] 1.1 ...' checkboxes
;;;;   openspec/changes/<change>/specs/<spec>/spec.md   the change's delta
;;;;   openspec/changes/archive/<change>/         a finished change, moved here
;;;;   openspec/ideas/<slug>.md                   a raw idea, before a change
;;;;
;;;; Everything here answers Lisp values; tools.lisp renders them as JSON.
;;;; The status of a change is derived from its checkboxes, never stored.

(in-package #:nodecode-openspec)

(nlk:define-peripheral openspec :not-running t)

(defparameter +primer+
  "Spec-driven development, on the project's own openspec/ folder. A change is a folder under openspec/changes/ holding proposal.md, tasks.md, optional design.md and specs/<spec>/spec.md; its status is read off tasks.md's `- [ ]'/`- [x]' lines. openspec_list, openspec_show, openspec_status, openspec_task_list and openspec_validate read; openspec_change_create, openspec_task_set_status, openspec_change_archive and the openspec_spec_* tools write. Call openspec_instructions for the authoring guide before writing a proposal, a design, a spec or a task list."
  "One paragraph: what the tools reach and in what order to call them.")

(defparameter +default-tasks+
  '("Refine scope" "Implement behavior" "Validate OpenSpec artifacts and tests")
  "The checklist a promoted change gets when the caller names none.")

(defparameter +enrichment-start+ (concatenate 'string "<!-- " "OPENSPEC_IDEA_ENRICHMENT_START" " -->"))
(defparameter +enrichment-end+ (concatenate 'string "<!-- " "OPENSPEC_IDEA_ENRICHMENT_END" " -->"))

(defparameter +feasibilities+ '("low" "medium" "high"))
(defparameter +tshirt-sizes+ '("xs" "s" "m" "l" "xl"))

;;; --- paths ---------------------------------------------------------------------------

(defun directory-pathname (raw)
  "RAW (a string, `~' allowed, a relative one read under this process's folder)
as a directory pathname; NIL for an empty one."
  (let ((text (nlk:trimmed (or raw ""))))
    (when (plusp (length text))
      (let* ((home (uiop:ensure-directory-pathname (user-homedir-pathname)))
             (path (cond ((string= text "~") home)
                         ((uiop:string-prefix-p "~/" text)
                          (merge-pathnames (subseq text 2) home))
                         (t (pathname text))))
             (absolute (if (char= (char (namestring path) 0) #\/)
                           path
                           (merge-pathnames path (uiop:getcwd)))))
        (uiop:ensure-directory-pathname absolute)))))

(defun project-root (&optional workdir)
  "The project folder a call works in: WORKDIR, else the section's workdir."
  (let* ((raw (if (and (stringp workdir) (plusp (length (nlk:trimmed workdir)))) workdir (setting :workdir)))
         (root (directory-pathname raw)))
    (unless root
      (fail "no project: set workdir in the openspec section, or pass workdir with the call"))
    (unless (uiop:directory-exists-p root)
      (fail "project folder does not exist: ~a" (namestring root)))
    root))

(defun openspec-root (root) (merge-pathnames "openspec/" root))
(defun changes-root (root) (merge-pathnames "changes/" (openspec-root root)))
(defun archive-root (root) (merge-pathnames "archive/" (changes-root root)))
(defun specs-root (root) (merge-pathnames "specs/" (openspec-root root)))
(defun ideas-root (root) (merge-pathnames "ideas/" (openspec-root root)))
(defun change-dir (root change) (merge-pathnames (concatenate 'string change "/") (changes-root root)))
(defun archived-dir (root change) (merge-pathnames (concatenate 'string change "/") (archive-root root)))

(defun ensure-layout (root)
  "ROOT's openspec/ tree, created where it is missing."
  (dolist (dir (list (changes-root root) (archive-root root) (specs-root root) (ideas-root root)))
    (ensure-directories-exist (merge-pathnames "seen" dir)))
  (openspec-root root))

;;; --- text files ----------------------------------------------------------------------

(defun read-text (path)
  "PATH's text, or an empty string when there is none."
  (if (uiop:file-exists-p path)
      (handler-case (uiop:read-file-string path :external-format :utf-8)
        (error () ""))
      ""))

(defun write-text (path text)
  "TEXT into PATH, its folders made first."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
    (write-string text out))
  path)

(defun append-text (path text)
  (write-text path (concatenate 'string (read-text path) text)))

(defun relative (path root)
  "PATH as it reads under ROOT, with forward slashes."
  (let ((namestring (namestring (pathname path)))
        (prefix (namestring (uiop:ensure-directory-pathname root))))
    (if (uiop:string-prefix-p prefix namestring)
        (subseq namestring (length prefix))
        namestring)))

(defun subdirectories (dir)
  "DIR's immediate subfolders, sorted by name; NIL when DIR is missing."
  (when (uiop:directory-exists-p dir)
    (sort (remove-if-not (lambda (path) (uiop:directory-pathname-p path))
                          (uiop:subdirectories dir))
          #'string< :key #'namestring)))

(defun folder-name (dir)
  "DIR's last component, without the trailing slash."
  (car (last (remove "" (uiop:split-string (string-right-trim "/" (namestring dir))
                                            :separator '(#\/)) :test #'string=))))

(defun markdown-files (dir)
  "Every .md file under DIR, recursively, sorted by path."
  (when (uiop:directory-exists-p dir)
    (sort (remove-if-not (lambda (path) (equal "md" (pathname-type path)))
                          (uiop:directory-files dir))
          #'string< :key #'namestring)))

(defun recursive-markdown-files (dir)
  "Every .md file under DIR and its subfolders, sorted by path."
  (when (uiop:directory-exists-p dir)
    (let ((out '()))
      (labels ((walk (folder)
                 (dolist (path (uiop:directory-files folder))
                   (when (equal "md" (pathname-type path)) (push path out)))
                 (dolist (sub (subdirectories folder)) (walk sub))))
        (walk dir))
      (sort out #'string< :key #'namestring))))

(defun delete-tree (dir)
  "DIR and everything under it, refused outside an openspec/ tree."
  (when (uiop:directory-exists-p dir)
    (uiop:delete-directory-tree dir :validate t))
  dir)

(defun move-directory (from to)
  "FROM's folder to TO, replacing one already there."
  (let ((source (uiop:ensure-directory-pathname from))
        (target (uiop:ensure-directory-pathname to)))
    (ensure-directories-exist (merge-pathnames "seen" target))
    (delete-tree target)
    (rename-file source target)
    target))

;;; --- words ----------------------------------------------------------------------------

(defun now-iso ()
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" year month day hour minute second)))

(defun slugify (text &optional (fallback "idea"))
  "TEXT lowercased to [a-z0-9-], one dash where a run of anything else was,
or FALLBACK when nothing is left."
  (let ((out (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (pending nil))
    (loop for character across (string-downcase (string-trim " " (or text "")))
          do (cond ((or (char<= #\a character #\z) (char<= #\0 character #\9))
                    (when (and pending (plusp (fill-pointer out)))
                      (vector-push-extend #\- out))
                    (setf pending nil)
                    (vector-push-extend character out))
                   (t (setf pending t))))
    (let ((slug (coerce out 'string)))
      (if (plusp (length slug)) slug fallback))))

(defun kebab-case-p (text)
  "TEXT is a kebab-case id: [a-z0-9], single dashes, no leading or trailing one."
  (and (stringp text)
       (plusp (length text))
       (let ((previous #\-))
         (loop for character across text
               always (cond ((or (char<= #\a character #\z) (char<= #\0 character #\9))
                             (setf previous character) t)
                            ((and (char= character #\-) (not (char= previous #\-)))
                             (setf previous character) t)
                            (t nil)))
         (not (char= previous #\-)))))

(defun kebab (raw)
  "RAW as a kebab-case id, refusing when nothing usable is left."
  (let ((text (slugify raw "")))
    (unless (plusp (length text))
      (fail "~s is not a kebab-case id (letters, numbers, hyphens)" raw))
    text))

(defun lines (text)
  (uiop:split-string (or text "") :separator '(#\Newline)))

(defun between (text start end)
  "TEXT with START..END removed, when both markers are there; NIL otherwise."
  (let ((from (search start text)))
    (when from
      (let ((to (search end text :start2 from)))
        (when to
          (values (concatenate 'string (subseq text 0 from) (subseq text (+ to (length end))))
                  t))))))

;;; --- changes --------------------------------------------------------------------------

(defun change-folders (root)
  "Every change folder under ROOT: (NAME DIR ARCHIVED-P), active ones first."
  (let ((out '()))
    (dolist (dir (subdirectories (changes-root root)))
      (unless (string= "archive" (folder-name dir))
        (when (uiop:file-exists-p (merge-pathnames "proposal.md" dir))
          (push (list (folder-name dir) dir nil) out))))
    (dolist (dir (subdirectories (archive-root root)))
      (when (uiop:file-exists-p (merge-pathnames "proposal.md" dir))
        (push (list (folder-name dir) dir t) out)))
    (nreverse out)))

(defun find-change (root name &key (archived :any))
  "The change folder NAME in ROOT as (VALUES DIR ARCHIVED-P), or NIL."
  (loop for (folder dir archived-p) in (change-folders root)
        when (and (string= folder (or name ""))
                  (or (eq archived :any) (eql archived archived-p)))
          return (values dir archived-p)))

(defun parse-task-line (line)
  "LINE as an OpenSpec task: (:indent N :mark #\\x or #\\Space :id ID :text TEXT)."
  (let ((indent (or (position-if-not (lambda (c) (or (char= c #\Space) (char= c #\Tab))) line)
                    (length line))))
    (when (<= (+ indent 5) (length line))
      (when (and (char= (char line indent) #\-)
                 (char= (char line (1+ indent)) #\Space)
                 (char= (char line (+ indent 2)) #\[)
                 (member (char line (+ indent 3)) '(#\Space #\x #\X))
                 (char= (char line (+ indent 4)) #\]))
        (let* ((rest (string-trim " " (subseq line (+ indent 5))))
               (first (subseq rest 0 (or (position #\Space rest) (length rest))))
               (numeric (and (plusp (length first)) (numeric-id-p first)))
               (text (if numeric (string-trim " " (subseq rest (length first))) rest)))
          (list :indent indent
                :mark (if (member (char line (+ indent 3)) '(#\x #\X)) #\x #\Space)
                :id (and numeric first)
                :text text))))))

(defun numeric-id-p (token)
  "TOKEN is a dotted numeric task id: 1, 2.3, 4.5.6."
  (and (plusp (length token))
       (loop for character across token
             always (or (char<= #\0 character #\9) (char= character #\.)))
       (not (member (char token 0) '(#\.)))
       (not (member (char token (1- (length token))) '(#\.)))))

(defun render-task-line (parts mark)
  (concatenate 'string
               (make-string (getf parts :indent) :initial-element #\Space)
               "- [" (string mark) "] "
               (if (getf parts :id) (format nil "~a " (getf parts :id)) "")
               (getf parts :text)))

(defun change-tasks (change-folder)
  "Every checkbox in CHANGE-FOLDER's tasks.md, in file order."
  (let ((text (read-text (merge-pathnames "tasks.md" change-folder)))
        (out '())
        (number 0))
    (loop for line in (lines text)
          for line-number from 1
          for parts = (parse-task-line line)
          when parts
            do (incf number)
               (push (list :id (or (getf parts :id) (princ-to-string number))
                           :text (getf parts :text)
                           :status (if (char= (getf parts :mark) #\x) "done" "todo")
                           :line line-number)
                     out))
    (nreverse out)))

(defun task-counts (tasks)
  (let ((done (count "done" tasks :key (lambda (task) (getf task :status)) :test #'string=)))
    (list :total (length tasks) :done done :todo (- (length tasks) done))))

(defun change-status (change-folder &key archived)
  "A change's lifecycle column, derived from its checkboxes."
  (cond (archived "archived")
        (t (let* ((tasks (change-tasks change-folder))
                  (counts (task-counts tasks)))
             (cond ((null tasks) "draft")
                   ((zerop (getf counts :done)) "todo")
                   ((= (getf counts :done) (getf counts :total)) "done")
                   (t "in_progress"))))))

(defun change-title (change-folder change)
  "The first `# ' heading of the proposal, else the folder name in words."
  (let ((proposal (read-text (merge-pathnames "proposal.md" change-folder))))
    (or (loop for line in (lines proposal)
              when (uiop:string-prefix-p "# " line)
                return (string-trim " " (subseq line 2)))
        (string-capitalize (substitute #\Space #\- change)))))

(defun change-artifacts (change-folder)
  "Which artifacts a change folder holds, as an alist (NAME . PATH-OR-NIL)."
  (list (cons "proposal" (merge-pathnames "proposal.md" change-folder))
        (cons "tasks" (merge-pathnames "tasks.md" change-folder))
        (cons "design" (merge-pathnames "design.md" change-folder))
        (cons "specs" (merge-pathnames "specs/" change-folder))))

;;; --- specs -----------------------------------------------------------------------------

(defun spec-path (root spec &optional change)
  "The spec file SPEC names, in CHANGE or in the baseline."
  (let ((base (if change
                  (merge-pathnames (concatenate 'string change "/specs/") (changes-root root))
                  (specs-root root))))
    (merge-pathnames (concatenate 'string spec "/spec.md") base)))

(defun spec-names (root &optional change)
  "The spec names under CHANGE's deltas, or the baseline's."
  (let ((dir (if change
                 (merge-pathnames (concatenate 'string change "/specs/") (changes-root root))
                 (specs-root root))))
    (loop for folder in (subdirectories dir)
          when (uiop:file-exists-p (merge-pathnames "spec.md" folder))
            collect (folder-name folder))))

(defun scenario-step (line)
  "LINE as a GIVEN/WHEN/THEN step: (:type :text), or NIL."
  (let ((text (nlk:trimmed line)))
    (when (and (>= (length text) 2) (member (char text 0) '(#\- #\*)) (char= (char text 1) #\Space))
      (let* ((rest (string-trim " " (subseq text 2)))
             (open (search "**" rest)))
        (when (= open 0)
          (let ((close (search "**" rest :start2 2)))
            (when close
              (list :type (string-upcase (subseq rest 2 close))
                    :text (string-trim " " (subseq rest (+ close 2)))))))))))

(defun parse-spec (text)
  "SPEC.md as (:title :purpose :requirements), each requirement
(:name :description :scenarios), each scenario (:name :steps)."
  (let ((title "") (purpose "") (requirements '())
        (requirement nil) (scenario nil) (state :top))
    (flet ((finish-scenario ()
             (when scenario (push (list :name (getf scenario :name)
                                        :steps (nreverse (getf scenario :steps)))
                                  (getf requirement :scenarios)))
             (setf scenario nil)))
      (dolist (line (lines (or text "")))
        (let ((trim (nlk:trimmed line)))
          (cond
            ((and (uiop:string-prefix-p "# " line) (not (uiop:string-prefix-p "## " line)))
             (finish-scenario)
             (let ((heading (string-trim " " (subseq trim 2))))
               (setf title (if (uiop:string-prefix-p "Specification" heading)
                               (string-trim " " (subseq heading (length "Specification")))
                               heading))
               (setf state :top)))
            ((uiop:string-prefix-p "## " (string-downcase trim))
             (cond ((uiop:string-prefix-p "## purpose" (string-downcase trim))
                    (finish-scenario) (setf state :purpose))
                   ((uiop:string-prefix-p "## requirements" (string-downcase trim))
                    (finish-scenario) (setf state :top))
                   (t (finish-scenario) (setf state :top))))
            ((uiop:string-prefix-p "### requirement:" (string-downcase trim))
             (finish-scenario)
             (let ((name (string-trim " " (subseq trim (length "### Requirement:")))))
               (setf requirement (list :name name :description "" :scenarios '()))
               (push requirement requirements)
               (setf state :description)))
            ((uiop:string-prefix-p "#### scenario:" (string-downcase trim))
             (finish-scenario)
             (setf scenario (list :name (string-trim " " (subseq trim (length "#### Scenario:")))
                                  :steps '()))
             (setf state :scenario))
            ((zerop (length trim)) nil)
            (t
             (ecase state
               (:purpose (setf purpose (if (plusp (length purpose))
                                           (concatenate 'string purpose " " trim)
                                           trim)))
               (:description
                (when requirement
                  (setf (getf requirement :description)
                        (let ((was (getf requirement :description)))
                          (if (plusp (length was)) (concatenate 'string was " " trim) trim)))))
               (:scenario
                (when scenario
                  (let ((step (scenario-step line)))
                    (cond (step (push step (getf scenario :steps)))
                          ((getf scenario :steps)
                           (let ((last-step (first (getf scenario :steps))))
                             (setf (getf last-step :text)
                                   (concatenate 'string (getf last-step :text) " " trim))))))))
               (:top nil))))))
      (finish-scenario))
    (list :title title :purpose purpose
          :requirements (mapcar (lambda (requirement)
                                  (list :name (getf requirement :name)
                                        :description (getf requirement :description)
                                        :scenarios (nreverse (getf requirement :scenarios))))
                                (nreverse requirements)))))
