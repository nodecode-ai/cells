;;;; cell.lisp --- a board the agent draws on, saved as one SVG page.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of skk2010/hermes-board-plugin (see NOTICE). A board is one
;;;; JSON document under the configured folder; the agent draws on it with the
;;;; original's six board_* tools and the same op vocabulary, and board_render
;;;; writes the picture as a standalone SVG inside one HTML page. The desktop
;;;; pane, the edit-watch loop, the headless-Chrome PNG and the bus/graph/
;;;; table ops are left out — the NOTICE says why.

(in-package #:nodecode-canvas)

(nlk:define-peripheral canvas :not-running t :lock "canvas")

;;; --- what the board is made of --------------------------------------------

(defparameter +canvas-width+ 960 "A new board's canvas, as the original's default.")
(defparameter +canvas-height+ 640 "A new board's canvas height.")
(defparameter +node-w+ 168 "A node's width when the op gives none.")
(defparameter +node-h+ 64 "A node's height when the op gives none.")
(defparameter +text-size+ 14 "A node's text size, in px.")

(defparameter +tones+
  '(("default" "#ffffff" "#c3cddf" "#1f2937")
    ("accent" "#eaf1ff" "#5b8cff" "#173a7a")
    ("ok" "#e8f7ee" "#3aa76d" "#14532d")
    ("warn" "#fff7e6" "#d39412" "#5c3d05")
    ("danger" "#fdecec" "#d64545" "#6b1111")
    ("muted" "#f3f4f6" "#d1d5db" "#4b5563"))
  "Fill, stroke and ink for one tone name, as the original's TONES.")

(defparameter +kinds+ '("box" "pill" "diamond" "note" "caption" "cloud" "cylinder"
                        "document" "hexagon" "area")
  "The node shapes a board may carry, as the original's KINDS without its table.")

(defparameter +ops+
  '("add_node" "update_node" "move_node" "delete_node"
    "add_edge" "update_edge" "delete_edge"
    "add_line" "update_line" "delete_line"
    "add_group" "update_group" "delete_group"
    "add_caption" "add_note" "update_note" "delete_note"
    "set_title" "set_grid" "layout")
  "Every op board_apply applies, as the original's OPS less bus, graph and add_table.")

(defparameter +kinds-note+ "box pill diamond note caption cloud cylinder document hexagon area")

;;; --- reading a payload -----------------------------------------------------

(defun text-at (object &rest path)
  "OBJECT's string at PATH, trimmed, or \"\"."
  (let ((value (apply #'nlk:json-value object :string path)))
    (if value (string-trim " " value) "")))

(defun optional-text-at (object &rest path)
  "OBJECT's string at PATH, or NIL when absent or empty."
  (let ((value (apply #'text-at object path)))
    (when (plusp (length value)) value)))

(defun number-at (object path &optional default)
  "OBJECT's number at PATH, or DEFAULT."
  (let ((value (apply #'nlk:json-value object :number path)))
    (if (numberp value) (round value) default)))

(defun arguments-object (args)
  "ARGS as the tool call's object: a JSON object, or a refusal."
  (if (hash-table-p args) args (nlk:json-object)))

(defun argument (args name &key required)
  "ARGS' NAME as a trimmed string; REQUIRED refuses an empty one."
  (let ((value (gethash name args)))
    (cond ((and (stringp value) (plusp (length (string-trim " " value))))
           (string-trim " " value))
          (required (fail "~a is required" (substitute #\_ #\- name)))
          ((stringp value) "")
          (t ""))))

(defun argument-integer (args name default)
  "ARGS' NAME as an integer, or DEFAULT."
  (let ((value (gethash name args)))
    (cond ((integerp value) value) ((floatp value) (round value)) (t default))))

;;; --- where boards live -----------------------------------------------------

(defun safe-id (id)
  "ID as the ONE form a file name is built from, as the original's _safe_id.
A model- or user-supplied id never reaches a path raw."
  (let ((text (string-downcase (string-trim " " (if (stringp id) id (princ-to-string id))))))
    (let ((clean (remove-if-not (lambda (char) (or (alphanumericp char)
                                                    (member char '(#\- #\_))))
                                text)))
      (if (plusp (length clean)) (subseq clean 0 (min 64 (length clean))) "default"))))

(defun canvas-dir ()
  "The folder the section names, made."
  (let ((dir (setting :dir)))
    (unless (stringp dir) (fail "the `canvas' section names no dir"))
    (uiop:ensure-directory-pathname (nlk:expand-home dir))))

(defun boards-dir () (ensure-directories-exist (merge-pathnames "boards/" (canvas-dir))))
(defun renders-dir () (ensure-directories-exist (merge-pathnames "render/" (canvas-dir))))
(defun board-path (id) (merge-pathnames (format nil "~a.json" (safe-id id)) (boards-dir)))

(defun board-exists-p (id) (not (null (probe-file (board-path id)))))

(defun board-ids ()
  "Every board's id, sorted, as the folder holds it."
  (sort (loop for file in (directory (merge-pathnames "*.json" (boards-dir)))
               collect (pathname-name file))
        #'string<))

(defun read-board (id)
  "The board ID as decoded JSON, or a refusal naming it."
  (let ((path (board-path id)))
    (unless (probe-file path) (fail "no board '~a' in ~a — call board_new first" (safe-id id) (boards-dir)))
    (let ((board (ignore-errors (nlk:decode-json (nlk:read-text (namestring path))))))
      (if (hash-table-p board) board (fail "board '~a' is not a JSON object: ~a" (safe-id id) path)))))

(defun write-board (id board)
  "BOARD written over ID's file, atomically; answers its version."
  (let* ((path (board-path id))
         (text (nlk:pretty-json board))
         (hash (nlk:sha256-text text))
         (version (subseq hash (max 0 (- (length hash) 8)))))
    (setf (gethash "version" board) version)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
      (write-string (nlk:pretty-json board) out))
    version))

(defun new-board (id title)
  "An empty board, as the original's board_new writes one."
  (nlk:json-object "id" (safe-id id)
                    "title" (if (plusp (length title)) title (safe-id id))
                    "width" +canvas-width+
                    "height" +canvas-height+
                    "nodes" #()
                    "edges" #()
                    "lines" #()
                    "groups" #()
                    "notes" #()
                    "captions" #()))

;;; --- inside a board -------------------------------------------------------

(defun board-items (board key)
  "BOARD's array at KEY, as a vector."
  (let ((value (nlk:json-value board :array key)))
    (if value (coerce value 'vector) #())))

(defun (setf board-items) (items board key)
  (setf (gethash key board) (coerce items 'vector)))

(defun item-with-id (board key id)
  "The item of BOARD's KEY array whose id is ID, or NIL."
  (find id (board-items board key) :test #'equal
                                 :key (lambda (item) (text-at item "id"))))

(defun remove-at (vector index)
  "VECTOR without the element at INDEX."
  (concatenate 'vector (subseq vector 0 index) (subseq vector (1+ index))))

(defun item-index (board key id)
  (position id (board-items board key) :test #'equal
                                    :key (lambda (item) (text-at item "id"))))

(defun node-ids (board)
  (loop for node across (board-items board "nodes") collect (text-at node "id")))

(defun next-id (board prefix)
  "PREFIX plus one past the highest number already used, so an id is never reused."
  (let ((taken (append (node-ids board)
                        (loop for item in (append (coerce (board-items board "lines") 'list)
                                                      (coerce (board-items board "groups") 'list)
                                                      (coerce (board-items board "notes") 'list))
                              collect (text-at item "id")))))
    (loop for index from 1
          for id = (format nil "~a~d" prefix index)
          unless (member id taken :test #'equal) return id)))

(defun ensure-node (board id what)
  "ID as an existing node's id, or a refusal naming WHAT."
  (unless (item-with-id board "nodes" id)
    (fail "~a names no node '~a'; the board has ~{~a~^, ~}" what id (node-ids board)))
  id)

;;; --- the ops ---------------------------------------------------------------

(defun tone-of (name &optional (default "default"))
  (if (assoc name +tones+ :test #'equal) name default))

(defun checked-tone (op)
  "OP's tone, refused when it names one the palette does not carry."
  (let ((name (text-at op "tone")))
    (cond ((zerop (length name)) "default")
          ((assoc name +tones+ :test #'equal) name)
          (t (fail "unknown tone '~a'; tones are ~{~a~^, ~}" name (mapcar #'first +tones+))))))

(defun put (object key value)
  (when (and value (not (and (stringp value) (zerop (length value)))))
    (setf (gethash key object) value))
  object)

(defun fit-width (text)
  (max 96 (min 360 (+ 48 (* 7 (length text))))))

(defun place-next (board)
  "Where a node with no x/y lands: a left-to-right row that wraps, as the
original's default placement."
  (let* ((count (length (board-items board "nodes")))
         (column (mod count 4))
         (row (floor count 4)))
    (values (+ 40 (* column 200)) (+ 96 (* row 120)))))

(defun add-node (board op)
  (let* ((text (text-at op "text"))
         (id (or (optional-text-at op "id") (next-id board "n"))))
    (when (item-with-id board "nodes" id) (fail "node '~a' already exists" id))
    (when (>= (length (board-items board "nodes")) (or (setting :max-nodes) 400))
      (fail "the board is full: ~d nodes (raise the canvas section's max_nodes)"
            (length (board-items board "nodes"))))
    (multiple-value-bind (x y) (place-next board)
      (let ((node (nlk:json-object "id" id "text" text)))
        (put node "sub" (optional-text-at op "sub"))
        (put node "x" (number-at op '("x") x))
        (put node "y" (number-at op '("y") y))
        (put node "w" (number-at op '("w") (fit-width text)))
        (put node "h" (number-at op '("h") (if (optional-text-at op "sub") (+ +node-h+ 14) +node-h+)))
        (put node "tone" (and (optional-text-at op "tone") (checked-tone op)))
        (put node "kind" (let ((kind (text-at op "kind")))
                           (when (plusp (length kind))
                             (if (member kind +kinds+ :test #'equal)
                                 kind (fail "unknown kind '~a' (~a)" kind +kinds-note+)))))
        (put node "text_size" (optional-text-at op "text_size"))
        (put node "bold" (nlk:json-value op :boolean "bold"))
        (put node "align" (optional-text-at op "align"))
        (setf (gethash "nodes" board)
              (concatenate 'vector (board-items board "nodes") (vector node)))
        (format nil "add_node ~a \"~a\"" id (shorten text 40))))))

(defun shorten (text limit)
  (if (> (length text) limit) (concatenate 'string (subseq text 0 (max 0 (- limit 1))) "…") text))

(defun update-node (board op)
  (let* ((id (argument-required op "id" "update_node"))
         (node (or (item-with-id board "nodes" id) (fail "no node '~a'" id)))
         (changed '()))
    (dolist (key '("text" "sub" "kind" "tone" "x" "y" "w" "h" "text_size" "bold" "align"))
      (let ((value (gethash key op)))
        (when (and value (not (eq value :null)))
          (cond ((stringp value)
                 (setf (gethash key node) value)
                 (push (format nil "~a=" key) changed))
                ((numberp value)
                 (setf (gethash key node) value)
                 (push (format nil "~a=~d" key value) changed))
                ((eq value t)
                 (setf (gethash key node) t)
                 (push key changed))))))
    (when (and (stringp (gethash "tone" op)) (not (assoc (gethash "tone" op) +tones+ :test #'equal)))
      (fail "unknown tone '~a'" (gethash "tone" op)))
    (if changed
        (format nil "update_node ~a (~{~a~^, ~})" id (reverse changed))
        (format nil "update_node ~a (nothing changed)" id))))

(defun argument-required (object name what)
  (let ((value (optional-text-at object name)))
    (or value (fail "~a needs ~s" what (substitute #\_ #\- name)))))

(defun move-node (board op)
  (let* ((id (argument-required op "id" "move_node"))
         (node (or (item-with-id board "nodes" id) (fail "no node '~a'" id)))
         (x (number-at op '("x")))
         (y (number-at op '("y"))))
    (unless (and x y) (fail "move_node needs x and y"))
    (setf (gethash "x" node) x (gethash "y" node) y)
    (format nil "move_node ~a to (~d, ~d)" id x y)))

(defun delete-node (board op)
  (let* ((id (argument-required op "id" "delete_node"))
         (index (or (item-index board "nodes" id) (fail "no node '~a'" id))))
    (setf (board-items board "nodes")
          (remove-at (board-items board "nodes") index))
    (setf (board-items board "edges")
          (remove-if (lambda (edge) (member id (list (text-at edge "from") (text-at edge "to"))
                                           :test #'equal))
                     (board-items board "edges")))
    (format nil "delete_node ~a (and its edges)" id)))

(defun add-edge (board op)
  (let* ((from (ensure-node board (argument-required op "from" "add_edge") "add_edge from"))
         (to (ensure-node board (argument-required op "to" "add_edge") "add_edge to"))
         (existing (find-if (lambda (edge) (and (equal from (text-at edge "from"))
                                                (equal to (text-at edge "to"))))
                            (board-items board "edges"))))
    (when existing (fail "an edge ~a -> ~a already exists" from to))
    (let ((edge (nlk:json-object "from" from "to" to)))
      (put edge "label" (optional-text-at op "label"))
      (put edge "tone" (and (optional-text-at op "tone") (tone-of (text-at op "tone"))))
      (put edge "style" (let ((style (text-at op "style")))
                          (when (plusp (length style))
                            (if (member style '("solid" "dashed" "dotted") :test #'equal)
                                style (fail "unknown style '~a' (solid dashed dotted)" style)))))
      (put edge "bidir" (nlk:json-value op :boolean "bidir"))
      (setf (gethash "edges" board)
            (concatenate 'vector (board-items board "edges") (vector edge)))
      (format nil "add_edge ~a -> ~a~@[ \"~a\"~]" from to (optional-text-at op "label")))))

(defun find-edge (board from to)
  (find-if (lambda (edge) (and (equal from (text-at edge "from"))
                                (equal to (text-at edge "to"))))
           (board-items board "edges")))

(defun update-edge (board op)
  (let* ((from (argument-required op "from" "update_edge"))
         (to (argument-required op "to" "update_edge"))
         (edge (or (find-edge board from to) (fail "no edge ~a -> ~a" from to))))
    (dolist (key '("label" "tone" "style" "bidir"))
      (let ((value (gethash key op)))
        (when value (setf (gethash key edge) value))))
    (format nil "update_edge ~a -> ~a" from to)))

(defun delete-edge (board op)
  (let* ((from (argument-required op "from" "delete_edge"))
         (to (argument-required op "to" "delete_edge"))
         (before (length (board-items board "edges"))))
    (setf (board-items board "edges")
          (remove-if (lambda (edge) (and (equal from (text-at edge "from"))
                                         (equal to (text-at edge "to"))))
                     (board-items board "edges")))
    (when (= before (length (board-items board "edges")))
      (fail "no edge ~a -> ~a" from to))
    (format nil "delete_edge ~a -> ~a" from to)))

(defun points-of (op)
  "OP's points as a vector of (x y) pairs, or a refusal."
  (let ((raw (nlk:json-value op :array "points")))
    (unless raw (fail "points is required: an array of [x, y] pairs"))
    (let ((points (coerce raw 'vector)))
      (when (< (length points) 2) (fail "a line needs at least two points, got ~d" (length points)))
      (map 'vector (lambda (pair)
                     (unless (and (vectorp pair) (>= (length pair) 2))
                       (fail "a point is [x, y], got ~s" pair))
                     (list (round (or (nlk:json-value pair :number 0) 0))
                           (round (or (nlk:json-value pair :number 1) 0))))
           points))))

(defun json-points (points)
  (map 'vector (lambda (point) (vector (first point) (second point))) points))

(defun add-line (board op)
  (let* ((points (points-of op))
         (id (or (optional-text-at op "id") (next-id board "l")))
         (line (nlk:json-object "id" id)))
    (setf (gethash "points" line) (json-points points))
    (put line "tone" (and (optional-text-at op "tone") (tone-of (text-at op "tone"))))
    (put line "style" (optional-text-at op "style"))
    (setf (gethash "lines" board)
          (concatenate 'vector (board-items board "lines") (vector line)))
    (format nil "add_line ~a (~d points)" id (length points))))

(defun update-line (board op)
  (let* ((id (argument-required op "id" "update_line"))
         (line (or (item-with-id board "lines" id) (fail "no line '~a'" id))))
    (when (nlk:json-value op :array "points")
      (setf (gethash "points" line) (json-points (points-of op))))
    (dolist (key '("tone" "style"))
      (let ((value (gethash key op)))
        (when (stringp value) (setf (gethash key line) value))))
    (format nil "update_line ~a" id)))

(defun delete-line (board op)
  (let* ((id (argument-required op "id" "delete_line"))
         (index (or (item-index board "lines" id) (fail "no line '~a'" id))))
    (setf (board-items board "lines") (remove-at (board-items board "lines") index))
    (format nil "delete_line ~a" id)))

(defun add-rectangle (board op key prefix label)
  "A group or a note: a box with a text and a position, as the original's
add_group and add_note."
  (let* ((id (or (optional-text-at op "id") (next-id board prefix)))
         (item (nlk:json-object "id" id "text" (text-at op "text"))))
    (put item "x" (number-at op '("x") 40))
    (put item "y" (number-at op '("y") 40))
    (put item "w" (number-at op '("w") 240))
    (put item "h" (number-at op '("h") 140))
    (put item "tone" (and (optional-text-at op "tone") (tone-of (text-at op "tone"))))
    (setf (gethash key board) (concatenate 'vector (board-items board key) (vector item)))
    (format nil "add_~a ~a~@[ \"~a\"~]" label id (optional-text-at op "text"))))

(defun update-rectangle (board op key what)
  (let* ((id (argument-required op "id" what))
         (item (or (item-with-id board key id) (fail "no ~a '~a'" what id))))
    (dolist (name '("text" "tone" "x" "y" "w" "h"))
      (let ((value (gethash name op)))
        (when (or (stringp value) (numberp value)) (setf (gethash name item) value))))
    (format nil "update_~a ~a" what id)))

(defun delete-rectangle (board op key what)
  (let* ((id (argument-required op "id" what))
         (index (or (item-index board key id) (fail "no ~a '~a'" what id))))
    (setf (board-items board key) (remove-at (board-items board key) index))
    (format nil "delete_~a ~a" what id)))

(defun add-caption (board op)
  (let* ((id (or (optional-text-at op "id") (next-id board "c")))
         (item (nlk:json-object "id" id "text" (text-at op "text"))))
    (put item "x" (number-at op '("x") 40))
    (put item "y" (number-at op '("y") 28))
    (put item "w" (number-at op '("w") 400))
    (setf (gethash "captions" board)
          (concatenate 'vector (board-items board "captions") (vector item)))
    (format nil "add_caption ~a \"~a\"" id (shorten (text-at op "text") 40))))

(defun set-title (board op)
  (let ((title (argument-required op "title" "set_title")))
    (setf (gethash "title" board) title)
    (format nil "set_title \"~a\"" (shorten title 60))))

(defun set-grid (board op)
  (let ((grid (or (nlk:json-value board :object "grid") (nlk:json-object))))
    (dolist (name '("size" "on" "snap"))
      (multiple-value-bind (value present) (gethash name op)
        (when present
          (setf (gethash name grid) (cond ((numberp value) (round value))
                                           (t (and value t)))))))
    (let ((style (optional-text-at op "style")))
      (when style
        (unless (member style '("lines" "dots") :test #'equal)
          (fail "unknown grid style '~a' (lines dots)" style))
        (setf (gethash "style" grid) style)))
    (setf (gethash "grid" board) grid)
    "set_grid"))

(defun layout-op (board op)
  "The original's `layout`: rows of node ids placed in bands, downward or to
the right."
  (let* ((rows (nlk:json-value op :array "rows"))
         (direction (or (optional-text-at op "direction") "down"))
         (gap-node (or (number-at op '("gap_node")) 40))
         (gap-layer (or (number-at op '("gap_layer")) 90))
         (width (or (number-at op '("w")) +node-w+))
         (height (or (number-at op '("h")) +node-h+))
         (placed 0))
    (unless rows (fail "layout needs rows: an array of arrays of node ids"))
    (unless (member direction '("down" "right") :test #'equal)
      (fail "layout direction is down or right, got '~a'" direction))
    (loop for band across rows
          for index from 0
          do (let ((step 0))
               (loop for id across (coerce band 'vector)
                     do (let* ((node (or (item-with-id board "nodes" id)
                                         (fail "layout names no node '~a'" id)))
                               (x (if (equal direction "right")
                                      (+ 40 (* index (+ width gap-layer)))
                                      (+ 40 step)))
                               (y (if (equal direction "right")
                                      (+ 60 step)
                                      (+ 96 (* index (+ height gap-layer))))))
                          (setf (gethash "x" node) x (gethash "y" node) y)
                          (incf placed)
                          (incf step (+ (if (equal direction "right") height width) gap-node))))))
    (format nil "layout (~d nodes in ~d bands, ~a)" placed (length rows) direction)))

(defun apply-op (board op)
  "One op applied to BOARD; answers the line the tool reports for it."
  (let ((name (or (optional-text-at op "op") (fail "every op needs an \"op\" key"))))
    (cond ((equal name "add_node") (add-node board op))
          ((equal name "update_node") (update-node board op))
          ((equal name "move_node") (move-node board op))
          ((equal name "delete_node") (delete-node board op))
          ((equal name "add_edge") (add-edge board op))
          ((equal name "update_edge") (update-edge board op))
          ((equal name "delete_edge") (delete-edge board op))
          ((equal name "add_line") (add-line board op))
          ((equal name "update_line") (update-line board op))
          ((equal name "delete_line") (delete-line board op))
          ((equal name "add_group") (add-rectangle board op "groups" "g" "group"))
          ((equal name "update_group") (update-rectangle board op "groups" "group"))
          ((equal name "delete_group") (delete-rectangle board op "groups" "group"))
          ((equal name "add_note") (add-rectangle board op "notes" "note" "note"))
          ((equal name "update_note") (update-rectangle board op "notes" "note"))
          ((equal name "delete_note") (delete-rectangle board op "notes" "note"))
          ((equal name "add_caption") (add-caption board op))
          ((equal name "set_title") (set-title board op))
          ((equal name "set_grid") (set-grid board op))
          ((equal name "layout") (layout-op board op))
          (t (fail "unknown op '~a'; available: ~{~a~^, ~}" name +ops+)))))

(defun apply-ops (board ops)
  "OPS applied in order to BOARD, answering what was done, one line each."
  (unless (and (vectorp ops) (plusp (length ops)))
    (fail "ops is required: a non-empty array of op objects"))
  (loop for op across ops
        for index from 0
        collect (handler-case (apply-op board op)
                  (canvas-error (condition)
                    (fail "op ~d (~a): ~a" index (or (optional-text-at op "op") "?")
                          (canvas-error-detail condition))))))

;;; --- the summary -----------------------------------------------------------

(defun board-summary (board)
  "The one-line shape of a board, as the original's summary."
  (format nil "~d node~:p, ~d edge~:p, ~d line~:p, ~d group~:p, ~d note~:p, ~d caption~:p"
          (length (board-items board "nodes")) (length (board-items board "edges"))
          (length (board-items board "lines")) (length (board-items board "groups"))
          (length (board-items board "notes")) (length (board-items board "captions"))))

;;; --- drawing ---------------------------------------------------------------

(defun xml (value)
  "VALUE as XML text: the five characters a browser would read as markup."
  (with-output-to-string (out)
    (loop for char across (princ-to-string value)
          do (case char
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&#39;" out))
               (t (write-char char out))))))

(defun tone-colours (name)
  (or (cdr (assoc (or name "default") +tones+ :test #'equal)) (cdr (assoc "default" +tones+ :test #'equal))))

(defun wrap-text (text width)
  "TEXT as lines that fit WIDTH px at +TEXT-SIZE+, breaking on spaces."
  (let ((limit (max 6 (floor width (* 0.56 +text-size+)))))
    (loop with words = (remove-if (lambda (word) (zerop (length word)))
                                  (loop for start = 0 then (1+ end)
                                        for end = (position #\Space text :start start)
                                        collect (subseq text start end)
                                        while end))
          with lines = '() with line = ""
          for word in words
          do (cond ((zerop (length line)) (setf line word))
                   ((<= (+ (length line) 1 (length word)) limit)
                    (setf line (concatenate 'string line " " word)))
                   (t (push line lines) (setf line word)))
          finally (when (plusp (length line)) (push line lines))
                  (return (or (nreverse lines) (list ""))))))

(defun node-box (node)
  (values (or (number-at node '("x")) 40) (or (number-at node '("y")) 40)
          (or (number-at node '("w")) +node-w+) (or (number-at node '("h")) +node-h+)))

(defun anchor-point (node toward-x toward-y)
  "Where the line to (TOWARD-X, TOWARD-Y) leaves NODE's box."
  (multiple-value-bind (x y w h) (node-box node)
    (let* ((cx (+ x (/ w 2))) (cy (+ y (/ h 2)))
           (dx (- toward-x cx)) (dy (- toward-y cy))
           (sx (if (zerop dx) 1 (/ (/ w 2) (abs dx))))
           (sy (if (zerop dy) 1 (/ (/ h 2) (abs dy))))
           (scale (min 1 sx sy)))
      (values (+ cx (* dx scale)) (+ cy (* dy scale))))))

(defun arrow-head (from-x from-y to-x to-y tone)
  "The head drawn at (TO-X, TO-Y) of a line that arrives from (FROM-X, FROM-Y)."
  (let* ((angle (atan (- to-y from-y) (- to-x from-x)))
         (length 11) (spread 0.42)
         (left-x (- to-x (* length (cos (- angle spread)))))
         (left-y (- to-y (* length (sin (- angle spread)))))
         (right-x (- to-x (* length (cos (+ angle spread)))))
         (right-y (- to-y (* length (sin (+ angle spread)))))
         (ink (third (tone-colours tone))))
    (format nil "<polygon points=\"~,1f,~,1f ~,1f,~,1f ~,1f,~,1f\" fill=\"~a\"/>"
            to-x to-y left-x left-y right-x right-y ink)))

(defun draw-node (out node)
  (multiple-value-bind (x y w h) (node-box node)
    (destructuring-bind (fill stroke ink) (tone-colours (text-at node "tone"))
      (let* ((kind (text-at node "kind"))
             (radius (cond ((equal kind "pill") (/ h 2)) ((equal (text-at node "tone") "muted") 4) (t 8)))
             (font (if (nlk:json-value node :boolean "bold") (+ +text-size+ 1) +text-size+))
             (lines (wrap-text (text-at node "text") (- w 20)))
             (sub (text-at node "sub"))
             (lh (+ font 4))
             (block (+ (* lh (length lines)) (if (plusp (length sub)) (+ lh 2) 0)))
             (start-y (+ y (/ (- h block) 2) font)))
        (format out "<rect x=\"~d\" y=\"~d\" width=\"~d\" height=\"~d\" rx=\"~,1f\" ry=\"~,1f\" fill=\"~a\" stroke=\"~a\" stroke-width=\"1.5\"/>~%"
                x y w h radius radius fill stroke)
        (when (equal kind "diamond")
          (format out "<polygon points=\"~d,~d ~d,~d ~d,~d ~d,~d\" fill=\"none\" stroke=\"~a\" stroke-width=\"1.5\"/>~%"
                  (+ x (/ w 2)) y (+ x w) (+ y (/ h 2)) (+ x (/ w 2)) (+ y h) x (+ y (/ h 2)) stroke))
        (loop for line in lines
              for index from 0
              do (format out "<text x=\"~d\" y=\"~d\" text-anchor=\"~a\" font-size=\"~d\" fill=\"~a\">~a</text>~%"
                         (if (equal (text-at node "align") "left") (+ x 10) (+ x (/ w 2)))
                         (round (+ start-y (* index lh)))
                         (if (equal (text-at node "align") "left") "start" "middle")
                         font ink (xml line)))
        (when (plusp (length sub))
          (format out "<text x=\"~d\" y=\"~d\" text-anchor=\"middle\" font-size=\"~d\" fill=\"~a\" opacity=\"0.75\">~a</text>~%"
                  (+ x (/ w 2)) (round (+ start-y (* lh (length lines)) 4))
                  (max 9 (- +text-size+ 3)) ink (xml (car (wrap-text sub (- w 16))))))))))

(defun draw-edge (out board edge)
  (let* ((from (item-with-id board "nodes" (text-at edge "from")))
         (to (item-with-id board "nodes" (text-at edge "to")))
         (style (or (optional-text-at edge "style") "solid"))
         (tone (text-at edge "tone"))
         (ink (third (tone-colours tone))))
    (when (and from to)
      (multiple-value-bind (fx fy fw fh) (node-box from)
        (multiple-value-bind (tx ty tw th) (node-box to)
          (multiple-value-bind (start-x start-y) (anchor-point from (+ tx (/ tw 2)) (+ ty (/ th 2)))
            (multiple-value-bind (end-x end-y) (anchor-point to (+ fx (/ fw 2)) (+ fy (/ fh 2)))
              (format out "<line x1=\"~,1f\" y1=\"~,1f\" x2=\"~,1f\" y2=\"~,1f\" stroke=\"~a\" stroke-width=\"1.6\"~@[ stroke-dasharray=\"6 4\"~]/>~%"
                      start-x start-y end-x end-y ink
                      (and (member style '("dashed" "dotted") :test #'equal) t))
              (format out "~a~%" (arrow-head start-x start-y end-x end-y tone))
              (when (nlk:json-value edge :boolean "bidir")
                (format out "~a~%" (arrow-head end-x end-y start-x start-y tone)))
              (let ((label (text-at edge "label")))
                (when (plusp (length label))
                  (let ((mid-x (/ (+ start-x end-x) 2)) (mid-y (/ (+ start-y end-y) 2))
                        (width (+ 12 (* 7 (length label)))))
                    (format out "<rect x=\"~,1f\" y=\"~,1f\" width=\"~d\" height=\"18\" rx=\"3\" fill=\"#ffffff\" opacity=\"0.92\"/>~%"
                            (- mid-x (/ width 2)) (- mid-y 9) width)
                    (format out "<text x=\"~,1f\" y=\"~,1f\" text-anchor=\"middle\" font-size=\"12\" fill=\"~a\">~a</text>~%"
                            mid-x (+ mid-y 4) ink (xml label))))))))))))

(defun draw-line (out line)
  (let* ((points (coerce (or (nlk:json-value line :array "points") #()) 'list))
         (style (or (optional-text-at line "style") "solid"))
         (tone (text-at line "tone"))
         (ink (third (tone-colours tone))))
    (when (>= (length points) 2)
      (format out "<polyline points=\"~{~a~^ ~}\" fill=\"none\" stroke=\"~a\" stroke-width=\"1.6\"~@[ stroke-dasharray=\"6 4\"~]/>~%"
              (loop for point in points
                    collect (format nil "~d,~d" (point-x point) (point-y point)))
              ink (and (member style '("dashed" "dotted") :test #'equal) t))
      (destructuring-bind (last-but-one last) (last points 2)
        (format out "~a~%" (arrow-head (point-x last-but-one) (point-y last-but-one)
                                       (point-x last) (point-y last) tone))))))

(defun draw-rectangle (out item dashed)
  (let ((x (or (number-at item '("x")) 40)) (y (or (number-at item '("y")) 40))
        (w (or (number-at item '("w")) 240)) (h (or (number-at item '("h")) 120)))
    (destructuring-bind (fill stroke ink) (tone-colours (text-at item "tone"))
      (format out "<rect x=\"~d\" y=\"~d\" width=\"~d\" height=\"~d\" rx=\"10\" fill=\"~a\" fill-opacity=\"~a\" stroke=\"~a\" stroke-width=\"1.4\"~@[ stroke-dasharray=\"8 5\"~]/>~%"
              x y w h fill (if dashed "0.35" "1") stroke (and dashed t))
      (loop for line in (wrap-text (text-at item "text") (- w 20))
            for index from 0
            do (format out "<text x=\"~d\" y=\"~d\" text-anchor=\"middle\" font-size=\"~d\" fill=\"~a\">~a</text>~%"
                       (+ x (/ w 2)) (+ y 24 (* index 18)) +text-size+ ink (xml line))))))

(defun draw-caption (out item)
  (loop for line in (wrap-text (text-at item "text") (or (number-at item '("w")) 400))
        for index from 0
        do (format out "<text x=\"~d\" y=\"~d\" font-size=\"16\" font-style=\"italic\" fill=\"#4b5563\">~a</text>~%"
                   (or (number-at item '("x")) 40) (+ (or (number-at item '("y")) 28) (* index 20))
                   (xml line))))

(defun board-extent (board)
  "The canvas the picture needs: the board's own, or the content's."
  (let ((right 0) (bottom 0))
    (flet ((see (x y) (setf right (max right x) bottom (max bottom y))))
      (loop for node across (board-items board "nodes")
            do (multiple-value-bind (x y w h) (node-box node) (see (+ x w) (+ y h))))
      (loop for key in '("groups" "notes")
            do (loop for item across (board-items board key)
                     do (see (+ (or (number-at item '("x")) 40) (or (number-at item '("w")) 240))
                             (+ (or (number-at item '("y")) 40) (or (number-at item '("h")) 120)))))
      (loop for item across (board-items board "captions")
            do (see (+ (or (number-at item '("x")) 40) (or (number-at item '("w")) 400))
                     (+ (or (number-at item '("y")) 28) 40))))
    (values (max +canvas-width+ (+ right 60)) (max +canvas-height+ (+ bottom 60)))))

(defun render-svg (board)
  "BOARD as one standalone SVG document."
  (multiple-value-bind (width height) (board-extent board)
    (let ((grid (nlk:json-value board :object "grid"))
          (title (text-at board "title")))
      (with-output-to-string (out)
        (format out "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~d\" height=\"~d\" viewBox=\"0 0 ~d ~d\" font-family=\"-apple-system, Segoe UI, Helvetica, Arial, sans-serif\">~%"
                width height width height)
        (format out "<rect width=\"~d\" height=\"~d\" fill=\"#fbfcfe\"/>~%" width height)
        (when (and grid (not (eq (gethash "on" grid) :false)) (gethash "on" grid))
          (let ((size (or (number-at grid '("size")) 10)))
            (format out "<defs><pattern id=\"grid\" width=\"~d\" height=\"~d\" patternUnits=\"userSpaceOnUse\"><path d=\"M ~d 0 L 0 0 0 ~d\" fill=\"none\" stroke=\"#e5e9f2\" stroke-width=\"1\"/></pattern></defs>~%"
                    size size size size)
            (format out "<rect width=\"~d\" height=\"~d\" fill=\"url(#grid)\"/>~%" width height)))
        (when (plusp (length title))
          (format out "<text x=\"~d\" y=\"42\" text-anchor=\"middle\" font-size=\"22\" font-weight=\"600\" fill=\"#111827\">~a</text>~%"
                  (/ width 2) (xml title)))
        (loop for group across (board-items board "groups") do (draw-rectangle out group t))
        (loop for edge across (board-items board "edges") do (draw-edge out board edge))
        (loop for line across (board-items board "lines") do (draw-line out line))
        (loop for note across (board-items board "notes") do (draw-rectangle out note nil))
        (loop for node across (board-items board "nodes") do (draw-node out node))
        (loop for caption across (board-items board "captions") do (draw-caption out caption))
        (format out "</svg>~%")))))

(defun render-html (board id)
  "The SVG inside one page that opens in a browser, with the board's name."
  (format nil "<!doctype html>~%<html lang=\"en\">~%<head><meta charset=\"utf-8\"><title>~a</title>~%<style>body{margin:0;background:#eef1f7;font-family:-apple-system,Segoe UI,Helvetica,Arial,sans-serif}main{padding:18px}svg{background:#fff;border-radius:12px;box-shadow:0 2px 14px rgba(15,23,42,.12);max-width:100%;height:auto}</style>~%</head>~%<body><main>~%<p style=\"color:#64748b;font-size:13px;margin:0 0 10px\">board ~a — ~a</p>~%~a</main></body>~%</html>~%"
          (xml (text-at board "title")) (xml (safe-id id)) (xml (board-summary board))
          (render-svg board)))

(defun render-to-file (board id kind)
  "BOARD written under the render folder as KIND (html or svg); answers the path."
  (let* ((safe (safe-id id))
         (path (merge-pathnames (format nil "~a.~a" safe kind) (renders-dir))))
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
      (write-string (if (equal kind "svg") (render-svg board) (render-html board id)) out))
    (namestring path)))

;;; --- the tools --------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     (canvas-error (condition)
       (nle:failure (format nil "ERROR: canvas: ~a" (canvas-error-detail condition))))))

(defun board-id-argument (args)
  "ARGS' board id, sanitised, defaulting to `default'."
  (let ((given (argument args "id")))
    (safe-id (if (plusp (length given)) given "default"))))

(defun point-x (point) (if (vectorp point) (aref point 0) (first point)))
(defun point-y (point) (if (vectorp point) (aref point 1) (second point)))

(defparameter *tools-primer*
  (format nil "board_list, board_get, board_new, board_delete, board_apply, board_render: a board is one JSON document under the canvas folder; board_apply draws with ops (~{~a~^, ~}), board_render writes it as an SVG/HTML page you open; /canvas shows, renders or deletes one." +ops+))

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES — (NAME TYPE DOC REQUIRED-P) ... — as a tool schema."
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

(defparameter *list-schema*
  (tool-schema "List the boards in this cell's folder: id, title, node/edge counts and version. Call it before board_get when you do not know an id."
                '()))

(defparameter *get-schema*
  (tool-schema "Read one board whole: the JSON document the drawing is (nodes, edges, lines, groups, notes, captions, title, grid) plus a one-line summary. Read before editing, so node ids are the ones on the board."
                '(("id" "string" "Board id (default: default)." nil))))

(defparameter *new-schema*
  (tool-schema "Create an empty board under the configured folder. Fails when the id exists — board_delete first if you mean to start over."
                '(("id" "string" "Board id, lower-case, dashes allowed (default: default)." nil)
                  ("title" "string" "The title drawn at the top of the picture." nil))))

(defparameter *delete-schema*
  (tool-schema "Delete one board's file. The drawing is gone; board_list shows what is left."
                '(("id" "string" "Board id to delete." t))))

(defparameter *apply-schema*
  (tool-schema
   (format nil "Draw on a board: apply an ordered list of ops, then save it. One call builds a whole diagram. Ops, each an object with an \"op\" key: add_node {id?,text,sub?,x?,y?,w?,h?,tone?,kind?(~a),text_size?(small|default|large),bold?,align?(center|left)}; update_node/move_node/delete_node {id,...same fields}; add_edge {from,to,label?,tone?,style?(solid|dashed|dotted),bidir?} and update_edge/delete_edge {from,to,...}; add_line {points:[[x,y],...],tone?,style?} plus update_line/delete_line {id,...}; add_group {id?,text,x,y,w,h,tone?} and update_group/delete_group {id,...}; add_note/text; add_caption {text,x,y,w?}; set_title {title}; set_grid {size?,on?,style?(lines|dots),snap?}; layout {rows:[[id,...],...],direction?(down|right),gap_node?,gap_layer?,w?,h?} places nodes in bands. A node with no x/y lands in the next free slot. The answer says what each op did."
           +kinds-note+)
   '(("id" "string" "Board id (default: default)." nil)
     ("ops" "array" "Ordered op objects, each with an \"op\" key." t))))

(defparameter *render-schema*
  (tool-schema "Render a board to a file and answer its path: format=svg is the drawing alone, format=html the same drawing inside one page you open in any browser (default html). Nothing has to be installed to see it."
                '(("id" "string" "Board id (default: default)." nil)
                  ("format" "string" "svg or html (default html)." nil))))

(defun board-answer (board id)
  (nlk:encode-json-object
   (nlk:json-object "ok" t "id" (safe-id id) "version" (or (text-at board "version") "")
                     "summary" (board-summary board)
                     "board" board)))

(defun tool-list (args)
  (declare (ignore args))
  (answering
   (nlk:encode-json-object
   (nlk:json-object "ok" t "dir" (namestring (boards-dir))
                     "boards" (loop for id in (board-ids)
                                     collect (nlk:json-object
                                              "id" id
                                              "title" (ignore-errors (text-at (read-board id) "title"))
                                              "summary" (ignore-errors (board-summary (read-board id)))))))))

(defun tool-get (args)
  (answering
    (let ((id (board-id-argument args)))
      (board-answer (read-board id) id))))

(defun tool-new (args)
  (answering
    (let ((safe (board-id-argument args)))
      (when (board-exists-p safe) (fail "board '~a' exists — board_delete it first, or draw on it" safe))
      (let* ((board (new-board safe (argument args "title")))
             (version (write-board safe board)))
        (nlk:encode-json-object (nlk:json-object "ok" t "id" safe "version" version
                                                  "path" (namestring (board-path safe))
                                                  "summary" (board-summary board)))))))

(defun tool-delete (args)
  (answering
    (let* ((id (safe-id (argument args "id" :required t)))
           (path (board-path id)))
      (unless (probe-file path) (fail "no board '~a'" id))
      (delete-file path)
      (nlk:encode-json-object (nlk:json-object "ok" t "id" id "deleted" t
                                                "remaining" (coerce (board-ids) 'vector))))))

(defun tool-apply (args)
  (answering
    (let* ((safe (board-id-argument args))
           (board (read-board safe))
           (did (apply-ops board (nlk:json-value args :array "ops")))
           (version (write-board safe board)))
      (nlk:encode-json-object (nlk:json-object "ok" t "id" safe "version" version
                                                "did" (coerce did 'vector)
                                                "summary" (board-summary board))))))

(defun tool-render (args)
  (answering
    (let* ((safe (board-id-argument args))
           (format (let ((given (string-downcase (argument args "format"))))
                     (cond ((plusp (length given))
                            (unless (member given '("svg" "html") :test #'equal)
                              (fail "format is svg or html, got '~a'" given))
                            given)
                           (t "html"))))
           (board (read-board safe))
           (path (render-to-file board safe format)))
      (nlk:encode-json-object (nlk:json-object "ok" t "id" safe "format" format "path" path
                                                "hint" (if (equal format "svg")
                                                           "open it with any browser or an image viewer, or embed it"
                                                           "open it with your browser: one page, nothing to install"))))))

;;; --- /canvas -------------------------------------------------------------------

(defun run-command (args session-id)
  "The operator's side of the board: /canvas list | show ID | new ID [TITLE] |
delete ID | render ID [svg|html]."
  (declare (ignore session-id))
  (running-settings)
  (let* ((words (remove-if (lambda (word) (zerop (length word)))
                           (loop for start = 0 then (1+ end)
                                         for end = (position #\Space args :start start)
                                         collect (string-trim " " (subseq args start end))
                                         while end)))
         (verb (string-downcase (or (first words) "list")))
         (id (or (second words) "default"))
         (rest-words (cddr words)))
    (cond ((member verb '("list" "boards") :test #'equal)
           (let ((ids (board-ids)))
             (if ids
                 (format nil "~d board~:p in ~a:~%~{  ~a~^~%~}"
                         (length ids) (namestring (boards-dir))
                         (loop for one in ids
                               collect (format nil "~a — ~a" one
                                               (ignore-errors (board-summary (read-board one))))))
                 (format nil "no boards yet in ~a: \"new NAME\" makes one" (namestring (boards-dir))))))
          ((member verb '("show" "get") :test #'equal)
           (let ((board (read-board id)))
             (format nil "~a — ~a~%~a"
                     (safe-id id) (board-summary board) (nlk:pretty-json board))))
          ((equal verb "new")
           (let ((safe (safe-id id)))
             (when (board-exists-p safe) (fail "board '~a' exists" safe))
             (write-board safe (new-board safe (format nil "~{~a~^ ~}" rest-words)))
             (format nil "board ~a made in ~a" safe (namestring (boards-dir)))))
          ((member verb '("delete" "rm") :test #'equal)
           (let ((safe (safe-id id)))
             (unless (probe-file (board-path safe)) (fail "no board '~a'" safe))
             (delete-file (board-path safe))
             (format nil "board ~a deleted" safe)))
          ((equal verb "render")
           (let* ((safe (safe-id id))
                  (format (if (member (first rest-words) '("svg" "html") :test #'equal)
                              (first rest-words) "html")))
             (format nil "~a" (render-to-file (read-board safe) safe format))))
          (t (fail "unknown /canvas verb '~a': list, show ID, new ID [TITLE], delete ID, render ID [svg|html]"
                   verb)))))

;;; --- the declaration -------------------------------------------------------

(nle:define-cell canvas
  (:section ("canvas")
    (:guide "dir is the one required member: a folder you can write, where boards and their renders are kept; then board_list, or /canvas list, shows what is there")
    ("dir" :path :required t
     :doc "the folder boards live in: boards/<id>.json under it, renders in render/")
    ("max_nodes" :integer :default 400 :min 1
     :doc "how many nodes one board may hold"))
  (:help :canvas
   "canvas: a board the agent draws with ops and board_render turns into one SVG/HTML page"
   (format nil "~a~%~%A board is one JSON document: boards/<id>.json under the `canvas` section's dir. The agent draws with board_apply's ops (add_node, add_edge, add_line, add_group, add_note, add_caption, set_title, set_grid, layout, and their update_/delete_ forms) and board_render writes the picture as a standalone SVG or as one HTML page you open in a browser. /canvas lists, shows, makes, renders and deletes boards. The ops are validated: an unknown op, a bad tone, an edge naming a node that is not there, or a full board all come back as an ERROR line naming the op by index." *tools-primer*))
  (:command "canvas" 'run-command
            :catches 'canvas-error
            :description "Boards: list, show, make, render or delete one"
            :argument-hint "list | show ID | new ID [TITLE] | delete ID | render ID [svg|html]")
  (:tool "board_list" *list-schema* 'tool-list)
  (:tool "board_get" *get-schema* 'tool-get)
  (:tool "board_new" *new-schema* 'tool-new)
  (:tool "board_delete" *delete-schema* 'tool-delete)
  (:tool "board_apply" *apply-schema* 'tool-apply)
  (:tool "board_render" *render-schema* 'tool-render)
  (:start (lambda ()
            (let ((dir (setting :dir)))
              (when (stringp dir)
                (ensure-directories-exist (merge-pathnames "boards/" (uiop:ensure-directory-pathname dir))))))))
