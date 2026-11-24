;;;; cell-test.lisp --- lifecycle, the six tools, /canvas, and the rendered page.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, over a scratch folder in a
;;;; temporary directory: a healthy start and a stop that takes everything
;;;; back, a section with no dir refusing, the six board tools answering
;;;; through the files they read and write (a drawing that lands in the JSON,
;;;; an unknown op and an edge to a node that is not there refused, the
;;;; rendered SVG carrying the drawing), and /canvas's answers. Nothing here
;;;; dials anything: the only I/O is the folder the fixture makes.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *canvas-root* nil "The scratch folder the current test draws in.")

(defun canvas-scratch ()
  "A fresh, empty folder name, made, as a namestring."
  (let ((path (merge-pathnames (format nil "nodecode-canvas-test-~d-~d/"
                                       (get-universal-time) (random 100000))
                                (uiop:temporary-directory))))
    (ensure-directories-exist path)
    (namestring path)))

(defmacro with-canvas-dir (&body body)
  `(let ((*canvas-root* (canvas-scratch)))
     ,@body))

(defun canvas-tool (name arguments)
  "NAME's registered handler, called with ARGUMENTS as a tool call hands them over."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun canvas-file (relative)
  (merge-pathnames relative *canvas-root*))

(defun canvas-text (relative)
  (nlk:read-text (namestring (canvas-file relative))))

(defun canvas-op (&rest pairs)
  (apply #'nlk:make-json-object pairs))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "canvas" "CANVAS-CELL-" :start nodecode-canvas:start-cell)

(define-cell-lifecycle-tests "canvas"
  (:fixture with-canvas-dir)
  (:config "dir" *canvas-root*)
  (:help :canvas)
  (:command "canvas")
  (:running (is (nle::find-tool "board_list") "board_list is registered")
            (is (nle::find-tool "board_get"))
            (is (nle::find-tool "board_new"))
            (is (nle::find-tool "board_delete"))
            (is (nle::find-tool "board_apply"))
            (is (nle::find-tool "board_render"))
            (is (probe-file (canvas-file "boards/")) "the boards folder is made where the section points")
            (is (equal *canvas-root* (namestring (nodecode-canvas::canvas-dir))) "the section's dir is the one the tools write to"))
  (:stopped (is (null (nle::find-tool "board_apply")) "stop unregisters the tools"))
  (:refused ("max_nodes" 0))
  (:idle nodecode-canvas:canvas-error (nodecode-canvas::run-command "" nil)))

(deftest canvas-cell-a-board-is-drawn-saved-and-read-back ()
  (with-canvas-dir
    (with-cell-stop ((canvas-start "dir" *canvas-root*))
      (is-carrying (made (canvas-tool "board_new" (canvas-op "id" "plan" "title" "Rollout")))
                   "\"ok\":true" "\"id\":\"plan\"")
      (is (probe-file (canvas-file "boards/plan.json")) "the board is one JSON document on disk")
      (is-carrying (drawn (canvas-tool "board_apply"
                                        (canvas-op
                                         "id" "plan"
                                         "ops" (vector (canvas-op "op" "add_node" "id" "api"
                                                                  "text" "API gateway")
                                                       (canvas-op "op" "add_node" "id" "db"
                                                                  "text" "Orders DB" "x" 400 "y" 120)
                                                       (canvas-op "op" "add_edge" "from" "api"
                                                                  "to" "db" "label" "writes")
                                                       (canvas-op "op" "add_note" "text" "next: shard"
                                                                  "x" 60 "y" 320)
                                                       (canvas-op "op" "set_title" "title" "Rollout")))))
                   "add_node api" "add_edge api -> db" "add_note" "set_title" "\"summary\"")
      (is-carrying (read-back (canvas-tool "board_get" (canvas-op "id" "plan")))
                   "\"api\"" "\"Orders DB\"" "\"writes\"" "\"next: shard\"" "2 nodes, 1 edge"
                   (is (search "\"x\":400" read-back) "a node the op placed keeps its coordinates"))
      (is-carrying (listed (canvas-tool "board_list" (canvas-op)))
                   "plan" "2 nodes, 1 edge")
      (is-carrying (changed (canvas-tool "board_apply"
                                         (canvas-op
                                          "id" "plan"
                                          "ops" (vector (canvas-op "op" "move_node" "id" "api"
                                                                   "x" 40 "y" 60)
                                                        (canvas-op "op" "delete_node" "id" "db")))))
                   "move_node api" "delete_node db")
      (is-carrying (after (canvas-tool "board_get" (canvas-op "id" "plan"))) "1 node, 0 edges")
      (is-carrying (deleted (canvas-tool "board_delete" (canvas-op "id" "plan"))) "\"deleted\":true")
      (is (null (probe-file (canvas-file "boards/plan.json"))) "and its file is gone"))))

(deftest canvas-cell-the-render-is-one-page-you-can-open ()
  (with-canvas-dir
    (with-cell-stop ((canvas-start "dir" *canvas-root*))
      (canvas-tool "board_new" (canvas-op "id" "flow" "title" "Flow"))
      (canvas-tool "board_apply"
                   (canvas-op
                    "id" "flow"
                    "ops" (vector (canvas-op "op" "add_node" "id" "a" "text" "Ingest")
                                  (canvas-op "op" "add_node" "id" "b" "text" "Store" "x" 360)
                                  (canvas-op "op" "add_edge" "from" "a" "to" "b")
                                  (canvas-op "op" "add_line"
                                             "points" (vector (vector 20 20) (vector 120 20))))))
      (is-carrying (html (canvas-tool "board_render" (canvas-op "id" "flow")))
                   "\"format\":\"html\"" "flow.html")
      (is (probe-file (canvas-file "render/flow.html")) "the page is written")
      (is-carrying (page (canvas-text "render/flow.html"))
                   "<svg" "Ingest" "<line" "polyline" (:absent "fonts.googleapis")
                   (:absent "src=\"http"))
      (is-carrying (svg (canvas-tool "board_render" (canvas-op "id" "flow" "format" "svg")))
                   "flow.svg")
      (is (search "Store" (canvas-text "render/flow.svg")) "the SVG alone is the same drawing"))))

(deftest canvas-cell-bad-ops-and-missing-boards-refuse ()
  (with-canvas-dir
    (with-cell-stop ((canvas-start "dir" *canvas-root*))
      (is-carrying (missing (canvas-tool "board_get" (canvas-op "id" "nope")))
                   "ERROR: canvas:" "no board 'nope'" "board_new first")
      (canvas-tool "board_new" (canvas-op "id" "one"))
      (is-carrying (twice (canvas-tool "board_new" (canvas-op "id" "one")))
                   "ERROR: canvas:" "exists")
      (is-carrying (ghost (canvas-tool "board_apply"
                                        (canvas-op
                                         "id" "one"
                                         "ops" (vector (canvas-op "op" "add_node" "text" "A")
                                                       (canvas-op "op" "add_edge" "from" "ghost"
                                                                  "to" "n2")))))
                   "ERROR: canvas:" "op 1 (add_edge)" "names no node 'ghost'")
      (is-carrying (unknown (canvas-tool "board_apply"
                                          (canvas-op "id" "one"
                                                     "ops" (vector (canvas-op "op" "explode")))))
                   "ERROR: canvas:" "unknown op 'explode'" "add_node")
      (is-carrying (purple (canvas-tool "board_apply"
                                         (canvas-op
                                          "id" "one"
                                          "ops" (vector (canvas-op "op" "add_node" "text" "B"
                                                                   "tone" "purple")))))
                   "ERROR: canvas:" "unknown tone 'purple'")
      (is (search "0 nodes" (canvas-tool "board_get" (canvas-op "id" "one"))) "an op list that refused halfway saved nothing, the good op included"))))

(deftest canvas-cell-the-command-lists-shows-and-renders ()
  (with-canvas-dir
    (with-cell-stop ((canvas-start "dir" *canvas-root*))
      (is-carrying (empty (cell-entry "nodecode-canvas" "canvas" "")) "no boards yet" "new NAME")
      (is-carrying (made (cell-entry "nodecode-canvas" "canvas" "new demo Demo board"))
                   "board demo made")
      (is-carrying (listed (cell-entry "nodecode-canvas" "canvas" "list")) "demo" "0 nodes")
      (is-carrying (shown (cell-entry "nodecode-canvas" "canvas" "show demo"))
                   "0 nodes" "\"title\": \"Demo board\"")
      (is-carrying (plain (cell-entry "nodecode-canvas" "canvas" "show demo"))
                   "Demo board" "0 nodes")
      (is-carrying (rendered (cell-entry "nodecode-canvas" "canvas" "render demo svg")) "demo.svg")
      (is (probe-file (canvas-file "render/demo.svg")) "the command's render is on disk")
      (is-carrying (gone (cell-entry "nodecode-canvas" "canvas" "delete demo")) "board demo deleted")
      (is-carrying (nonsense (cell-entry "nodecode-canvas" "canvas" "nonsense"))
                   "unknown /canvas verb"))))
