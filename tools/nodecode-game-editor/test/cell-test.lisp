;;;; cell-test.lisp --- the .tscn, project.godot and script readers over a real project folder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing is stubbed and nothing is mocked: the cell reads a Godot project
;;;; this file writes into a scratch folder, and every assertion is about what
;;;; a reader of that project would see -- the tree, the search, one node's
;;;; stored values, the groups, the connections, an outline, a numbered file,
;;;; the autoloads, the input map, and each refusal. No Godot binary, no
;;;; network, no editor: the four tools the original plugin answered from
;;;; disk are the four this cell answers from a folder.

(in-package #:nodecode.test)

(define-test-slice "game-editor" "GAME-EDITOR-CELL-" :start nodecode-game-editor:start-cell)


(defmacro carrying (binding &body checks)
  "IS-CARRYING with the checks as one list, the shape a test reads best in
when each check is a pair or an :absent on its own line."
  `(is-carrying ,binding ,@(first checks)))

;;; --- the project the tests read ------------------------------------------------

(defparameter *project-godot* "
; Engine configuration file.
config_version=5

[application]

config/name=\"Lantern Run\"
run/main_scene=\"res://scenes/main.tscn\"

[autoload]

Game=\"*res://autoload/game.gd\"

[input]

move_left={
\"deadzone\": 0.5,
\"events\": [Object(InputEventKey,\"resource_local_to_scene\":false,\"resource_name\":\"\",\"device\":-1,\"pressed\":false,\"keycode\":0,\"physical_keycode\":65,\"unicode\":97,\"echo\":false,\"script\":null)
]
}
jump={
\"deadzone\": 0.5,
\"events\": [Object(InputEventKey,\"resource_local_to_scene\":false,\"physical_keycode\":32,\"script\":null)
]
}
"
  "A project.godot as Godot writes one: the input map's event literals run over
several lines, which is what the reader has to survive.")

(defparameter *main-tscn* "
[gd_scene load_steps=3 format=3 uid=\"uid://c8lantern\"]

[ext_resource type=\"Script\" path=\"res://player.gd\" id=\"1_player\"]
[ext_resource type=\"PackedScene\" path=\"res://scenes/coin.tscn\" id=\"2_coin\"]

[node name=\"Main\" type=\"Node2D\"]

[node name=\"Player\" type=\"CharacterBody2D\" parent=\".\" groups=[\"player\", \"damageable\"]]
position = Vector2(120, 64)
speed = 300.0
script = ExtResource(\"1_player\")

[node name=\"Sprite\" type=\"Sprite2D\" parent=\"Player\"]
position = Vector2(0, -16)

[node name=\"Coin\" parent=\".\" instance=ExtResource(\"2_coin\")]
position = Vector2(300, 100)

[connection signal=\"died\" from=\"Player\" to=\".\" method=\"_on_player_died\"]
[connection signal=\"body_entered\" from=\"Coin\" to=\".\" method=\"_on_coin_body_entered\"]
"
  "A scene with a script, a group, an instanced child and two connections.")

(defparameter *player-gd* "extends CharacterBody2D
class_name Player

signal died(reason: String)

@export var speed: float = 300.0
const MAX_HEALTH := 3

var health: int = MAX_HEALTH

func _ready() -> void:
\tprint(\"ready\")

func take_damage(amount: int) -> void:
\thealth -= amount
\tif health <= 0:
\t\tdied.emit(\"hit\")
"
  "A GDScript with the declarations an outline is expected to find.")

(defparameter *coin-tscn* "
[gd_scene format=3]

[node name=\"Coin\" type=\"Area2D\"]
"
  "A second scene, the one main.tscn instances.")

(defparameter *game-gd* "
extends Node

var score := 0
"
  "An autoload, so file_look has a second folder's script to list.")

(defun fixture-files ()
  "The scratch project: its res:// name and its text."
  (list (cons "project.godot" *project-godot*)
        (cons "scenes/main.tscn" *main-tscn*)
        (cons "scenes/coin.tscn" *coin-tscn*)
        (cons "player.gd" *player-gd*)
        (cons "autoload/game.gd" *game-gd*)
        (cons ".godot/hidden.tscn" *coin-tscn*)))

(defun fixture-root ()
  "A scratch folder of this test run's own, gone when the test ends."
  (merge-pathnames (format nil "game-editor-~(~a~)/" (gensym "T"))
                   (uiop:temporary-directory)))

(defun write-fixture-project (root)
  "Write the scratch project under ROOT, parents and all."
  (dolist (pair (fixture-files) root)
    (let ((file (merge-pathnames (pathname (car pair)) root)))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :supersede
                                :external-format :utf-8)
        (write-string (cdr pair) out)))))

(defmacro with-project (&body body)
  "BODY with a scratch Godot project written and the cell started on it."
  (let ((root (gensym "ROOT")))
    `(let ((,root (fixture-root)))
       (unwind-protect
            (progn (write-fixture-project ,root)
                   (with-cell-stop ((game-editor-start "project" (namestring ,root)))
                     ,@body))
         (uiop:delete-directory-tree ,root :validate t :if-does-not-exist :ignore)))))

;;; --- the four tools, called the way a call reaches them ---------------------------

(defun request (contract &rest pairs)
  "The contract's record from PAIRS, as a tool call materializes it."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    (nlk:contract-decode (nlk:contract contract) object)))

(defun scene-read (op &rest pairs)
  (nodecode-game-editor:scene-look
   (apply #'request 'nodecode-game-editor::scene-look-request "op" op pairs)))

(defun script-read (op &rest pairs)
  (nodecode-game-editor:script-look
   (apply #'request 'nodecode-game-editor::script-look-request "op" op pairs)))

(defun file-read (op &rest pairs)
  (nodecode-game-editor:file-look
   (apply #'request 'nodecode-game-editor::file-look-request "op" op pairs)))

(defun project-read (op &rest pairs)
  (nodecode-game-editor:project-look
   (apply #'request 'nodecode-game-editor::project-look-request "op" op pairs)))

(defun refusal (thunk)
  "THUNK's refusal as its text, or the empty string when it did not refuse."
  (handler-case (progn (funcall thunk) "")
    (nodecode-game-editor:game-editor-error (condition) (princ-to-string condition))))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "game-editor"
  (:command "game-editor")
  (:help :game-editor)
  (:running (is (nle::find-tool "scene_look") "scene_look is registered")
            (is (nle::find-tool "script_look") "script_look is registered")
            (is (nle::find-tool "file_look") "file_look is registered")
            (is (nle::find-tool "project_look") "project_look is registered"))
  (:refused ("read_lines" 0) ("read_chars" 20))
  (:idle nodecode-game-editor:game-editor-error
         (nodecode-game-editor:scene-look nil)
         (nodecode-game-editor:project-look nil)))

(deftest game-editor-cell-a-missing-project-setting-refuses-with-its-name ()
  (with-cell-stop ((game-editor-start))
    (carrying (result (handler-case (progn (project-read "summary") "")
                        (nodecode-game-editor:game-editor-error (condition)
                          (princ-to-string condition))))
      (("game-editor.project is not set" "the refusal names the setting to answer")))))

;;; --- scene_look -----------------------------------------------------------------

(deftest game-editor-cell-the-scene-tree-is-the-file ()
  (with-project
    (let ((answer (scene-read "scene_tree")))
      (carrying answer
        (("scene res://scenes/main.tscn") ("4 nodes") ("2 connections")
         ("/Main (Node2D)") ("/Main/Player (CharacterBody2D)")
         ("groups: player, damageable") ("script res://player.gd")
         ("instance of res://scenes/coin.tscn"))))
    (carrying (result (scene-read "scene_tree" "scene" "res://scenes/coin.tscn"))
      (("res://scenes/coin.tscn") ("1 node")))))

(deftest game-editor-cell-find-node-and-children-narrow-it ()
  (with-project
    (carrying (result (scene-read "find_nodes" "type" "Sprite2D"))
      (("1 node of res://scenes/main.tscn match") ("/Main/Player/Sprite")
       (:absent "/Main/Coin")))
    (carrying (result (scene-read "find_nodes" "group" "damageable"))
      (("/Main/Player") (:absent "/Main/Player/Sprite")))
    (carrying (result (scene-read "children" "path" "/Main/Player"))
      (("/Main/Player, 1 child") ("/Main/Player/Sprite (Sprite2D)")))
    (carrying (result (scene-read "children"))
      (("/Main, 2 child") ("/Main/Player") ("/Main/Coin") (:absent "/Main/Player/Sprite")))
    (carrying (result (refusal (lambda () (scene-read "children" "path" "/Main/Nope"))))
      (("no node at /Main/Nope" "the refusal names the nodes there are")))))

(deftest game-editor-cell-properties-groups-and-signals-come-from-the-file ()
  (with-project
    (carrying (result (scene-read "properties" "path" "/Main/Player"))
      (("speed = 300.0") ("position = Vector2(120, 64)") ("script = ExtResource")))
    (carrying (result (scene-read "properties" "path" "/Main/Player" "fields" #("speed")))
      (("speed = 300.0") (:absent "position") ("1 value")))
    (carrying (result (scene-read "groups"))
      (("2 groups in res://scenes/main.tscn") ("player: /Main/Player")
       ("damageable: /Main/Player")))
    (carrying (result (scene-read "groups" "path" "/Main/Player/Sprite"))
      (("/Main/Player/Sprite groups: none")))
    (carrying (result (scene-read "signals"))
      (("2 connections in res://scenes/main.tscn")
       ("/Main/Player --died--> /Main._on_player_died")
       ("/Main/Coin --body_entered--> /Main._on_coin_body_entered")))
    (carrying (result (scene-read "signals" "path" "/Main/Coin"))
      (("1 connection") ("body_entered") (:absent "died")))))

;;; --- script_look ----------------------------------------------------------------

(deftest game-editor-cell-a-script-answers-its-source-and-its-outline ()
  (with-project
    (carrying (result (script-read "outline" "path" "res://player.gd"))
      (("8 declaration")
       ("    1  extends CharacterBody2D")
       ("    2  class_name Player")
       ("    4  signal died(reason: String)")
       ("    6  @export var speed: float = 300.0")
       ("   11  func _ready() -> void:")
       (:absent "print(" "a body line is not a declaration")))
    (carrying (result (script-read "source" "path" "res://player.gd" "offset" 1 "limit" 2))
      (("player.gd, 17 lines") ("2  class_name Player")
       ("more lines; pass offset 3") (:absent "extends CharacterBody2D")))
    (carrying (result (refusal (lambda () (script-read "source" "path" "res://scenes/main.tscn"))))
      (("not a GDScript")))))

;;; --- file_look ------------------------------------------------------------------

(deftest game-editor-cell-files-resources-and-text-stay-in-the-project ()
  (with-project
    (carrying (result (file-read "files" "type" "gd"))
      (("2 files") ("res://player.gd") ("res://autoload/game.gd") (:absent "main.tscn")))
    (carrying (result (file-read "files" "type" "tscn"))
      (("res://scenes/main.tscn") ("res://scenes/coin.tscn")
       (:absent ".godot" "a hidden folder is not listed")))
    (carrying (result (file-read "resources" "type" "scene"))
      (("res://scenes/main.tscn  (scene)") ("res://scenes/coin.tscn  (scene)")))
    (carrying (result (file-read "text" "path" "res://player.gd" "limit" 3))
      (("player.gd, 17 lines") ("1  extends CharacterBody2D") ("more lines; pass offset 3")))
    (carrying (result (refusal (lambda () (file-read "text" "path" "res://../../etc/passwd"))))
      (("outside the project folder" "a path that climbs out is refused")))
    (carrying (result (refusal (lambda () (file-read "text" "path" "res://.godot/hidden.tscn"))))
      (("hidden file or folder")))))

;;; --- project_look ---------------------------------------------------------------

(deftest game-editor-cell-project-settings-autoloads-and-input-map ()
  (with-project
    (carrying (result (project-read "get" "key" "application/run/main_scene"))
      ((:= "application/run/main_scene = res://scenes/main.tscn")))
    (is (handler-case (progn (project-read "get" "key" "application/run/missing_scene") nil)
          (nodecode-game-editor:game-editor-error () t)) "a setting project.godot does not carry refuses")
    (carrying (result (project-read "get" "key" "application/config/name"))
      (("= Lantern Run" "a stored string reads without its quotes")))
    (carrying (result (project-read "autoloads"))
      (("1 autoload") ("Game -> res://autoload/game.gd (singleton)")))
    (carrying (result (project-read "input_map"))
      (("2 input actions") ("move_left: A") ("jump: space")
       (:absent "keycode 65" "a letter is named, not numbered")))
    (carrying (result (project-read "summary"))
      (("Godot project Lantern Run") ("main scene res://scenes/main.tscn")
       ("autoloads 1, input actions 2") ("2 scenes")))))

;;; --- /game-editor ---------------------------------------------------------------

(deftest game-editor-cell-the-command-answers-the-project-and-its-panel ()
  (with-project
    (multiple-value-bind (result dialog)
        (nodecode-game-editor::run-slash "" nil)
      (carrying result (("Godot project Lantern Run") ("main scene")))
      (is (plusp (length (getf dialog :rows))) "bare /game-editor answers a panel to pick from"))
    (carrying (result (nodecode-game-editor::run-slash "scenes" nil))
      (("scenes: 2") ("res://scenes/main.tscn")))
    (carrying (result (nodecode-game-editor::run-slash "scripts" nil))
      (("scripts: 2") ("res://player.gd") (:absent "main.tscn")))
    (carrying (result (nodecode-game-editor::run-slash "settings" nil))
      (("[application] 2 settings") ("[autoload] 1 setting")))
    (carrying (result (nodecode-game-editor::run-slash "lantern" nil))
      (("hold" "no file name holds it")))))

(deftest game-editor-cell-the-primer-names-the-four-tools ()
  (with-project
    (let ((primer (nle:help :game-editor)))
      (carrying (result primer)
        (("scene_look") ("script_look") ("file_look") ("project_look")
         ("game-editor.project"))))))


