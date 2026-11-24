;;;; cell.lisp --- the tools, the command and the section: what a user of this
;;;; cell reaches.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Five of the plugin's nine tools, the ones that need nothing installed and
;;;; no key: forge_sfx, forge_music, forge_sprite, forge_tileset and
;;;; forge_engine. Every file lands inside the game project, every entry in
;;;; its gameforge.json ledger carries the recipe that made it, and the same
;;;; seed and arguments write the same bytes.

(in-package #:nodecode-gameforge)

;;; --- rendering hardware

(defun object-json (&rest pairs)
  "A hash table of string keys, which is what the JSON encoder writes."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key table) value))
    table))

(defun object-text (table)
  (nlk:encode-json-object table))

(defun string-vector (items)
  (map 'vector #'identity items))

(defun answer (root &rest pairs)
  "The tool answer: the pairs, plus the project this call wrote into."
  (object-text (apply #'object-json (append (list "project" (namestring root)) pairs))))

(defun seconds-of (samples)
  (/ (coerce (length samples) 'double-float) (coerce +rate+ 'double-float)))

;;; --- the tools --------------------------------------------------------------

(nlk:define-contract sfx-call
  (:doc "Synthesize a game sound effect into the project folder as a WAV, offline: a preset from the sfxr generator set, one to eight seeded variations, and a post-processing style. The answer names the files and their length in seconds. The same preset and seed write the same bytes, so a variation is a new seed, not a new tuning.")
  ("project" :text :doc "the game project folder, created if missing; the section's default_project under its directory when omitted")
  ("name" :text :doc "the asset's name: its file stem and its ledger key")
  ("preset" :text :required t :options +sfx-preset-names+
            :doc "which sfxr generator: coin, pickup, laser, shoot, explosion, powerup, hit, hurt, jump, blip, select")
  ("seed" :integer :doc "the same preset and seed write the same bytes; change it for another take")
  ("variations" :integer :doc "1 to 8 files, each the seed plus its index: a pool a game picks from at random")
  ("style" :text :options '("retro" "modern" "crunchy" "distant")
           :doc "retro as generated; modern airy; crunchy 5-bit and lo-fi; distant muffled with an echo"))

(nlk:define-contract music-call
  (:doc "Compose a seamless music loop into the project folder as a WAV: bass, chord arpeggio, a seeded melody and drums, in the mood's mode, chord progression and tempo. The answer names the file, its length and the tempo and bars it actually used.")
  ("project" :text :doc "the game project folder, created if missing")
  ("name" :text :doc "the asset's name: its file stem and its ledger key")
  ("mood" :text :options +mood-name-list+ :doc "the mood, which fixes the mode, tempo and progression")
  ("seed" :integer :doc "the melody, the progression choice and the drum fills follow the seed")
  ("bpm" :integer :doc "beats a minute; the mood's own tempo unless set")
  ("bars" :integer :doc "4 to 64, a multiple of 4; four beats a bar")
  ("key" :text :doc "the root note, such as C, F#, Bb or A")
  ("mode" :text :options +mode-name-list+ :doc "override the mood's scale")
  ("style" :text :options +music-styles+ :doc "chiptune unless set; synthwave, ambient and orchestral change the voices and the density"))

(nlk:define-contract sprite-call
  (:doc "Draw a 2D pixel-art asset into the project folder: two frames of a character, a monster or an item, as one PNG sheet (frame 1 beside frame 2) plus a JSON naming the frames. Colours come from a hue-shifted ramp or from a named palette; the seed varies the shading.")
  ("project" :text :doc "the game project folder, created if missing")
  ("name" :text :doc "the asset's name: a character archetype, a monster or an item; its file stem and its ledger key")
  ("kind" :text :options +sprite-kinds+ :doc "character, monster or item")
  ("seed" :integer :doc "the shading and the hair colour follow the seed")
  ("size" :integer :doc "the sprite's own size in pixels, 8 to 64 (16 unless set); the saved sheet is that scaled")
  ("scale" :integer :doc "screen pixels a drawn pixel becomes in the saved sheet")
  ("hue" :text :doc "a colour name such as teal, or #rrggbb; the kind's own hue unless set")
  ("palette" :text :options +palette-name-list+ :doc "snap every colour to a named palette"))

(nlk:define-contract tileset-call
  (:doc "Draw a seamless autotiling tileset between two terrains into the project folder as one PNG: sixteen cells, a Wang corner set, each an engine assigns by which neighbouring corners are the upper terrain. The terrain texture comes from one wrapping noise field, so cells meet without a seam.")
  ("project" :text :doc "the game project folder, created if missing")
  ("lower" :text :options +terrain-name-list+ :doc "the terrain the ground is")
  ("upper" :text :options +terrain-name-list+ :doc "the terrain that sits on it")
  ("name" :text :doc "the asset's name: its file stem and its ledger key")
  ("scale" :integer :doc "1 to 8 screen pixels a drawn pixel, 2 unless set")
  ("seed" :integer :doc "the noise field, so a seed is a version of the same terrain"))

(nlk:define-contract engine-call
  (:doc "Scaffold a playable project for a game engine, import the project's assets into it, run it, or report what this machine has. engine=web writes a complete index.html that plays in a browser with no build step; engine=godot writes a Godot 4 project.godot, a scene and a player script; action=import copies every asset the ledger knows into the engine folder beside a manifest; action=run starts the engine on it, or says which program it looked for; action=status reports which engines and programs are installed.")
  ("project" :text :doc "the game project folder, created if missing")
  ("engine" :text :required t :options '("web" "godot" "unity" "blender" "unreal")
            :doc "web and godot scaffold, import and run; the others are reported by status")
  ("action" :text :options '("scaffold" "import" "run" "status")
           :doc "scaffold (default) writes the engine's project files; import copies the project's assets in beside a manifest; run starts the engine; status reports which engines and programs are installed")
  ("title" :text :doc "the game's title, as the page and the manifest write it")
  ("seed" :integer :doc "the web page's coin layout follows it"))

(nlk:define-contract export-call
  (:doc "Ship the web build: target=web writes a PWA bundle (manifest, two icons, a service worker, the page patched to install it) and the zip itch.io takes; target=single writes one self-contained .html with every local image, sound and script inlined; target=capacitor writes a Capacitor project wrapping the bundle. Files only: nothing is uploaded and no key is needed.")
  ("project" :text :doc "the game project folder")
  ("target" :text :required t :options '("web" "single" "capacitor")
            :doc "web (bundle and zip), single (one page) or capacitor (a native wrapper project)")
  ("source" :text :doc "the web build to ship, inside the project; <project>/web unless set")
  ("title" :text :doc "the title the manifest and the file names carry; the project's own title unless set")
  ("app_id" :text :doc "capacitor's application id, such as com.example.game; derived from the title unless set"))

(defun forge-sfx (call)
  "One preset, VARIANTS times, as WAV files in the project: a coin, a laser, a
jump -- the sfxr generators, each seed its own variation."
  (let* ((root (project-root (sfx-call-project call)))
         (preset (sfx-call-preset call))
         (seed (or (sfx-call-seed call) 1))
         (style (or (sfx-call-style call) "retro"))
         (variations (or (sfx-call-variations call) 1))
         (name (safe-name (sfx-call-name call) preset)))
    (unless (<= 1 variations 8)
      (fail "variations is ~a; name 1 to 8 of them (a game wants a few takes, not eighty)" variations))
    (let ((files '()) (seconds 0.0d0))
      (dotimes (index variations)
        (multiple-value-bind (samples parameters) (render-sfx preset (+ seed index) :style style)
          (declare (ignore parameters))
          (let* ((stem (if (= variations 1) name (format nil "~a_~d" name (1+ index))))
                 (path (merge-pathnames (format nil "~a.wav" stem) (asset-directory root "sfx"))))
            (write-wav path samples)
            (setf seconds (seconds-of samples))
            (push path files))))
      (record-asset root "sfx" name (nreverse files)
                    (object-json "tool" "forge_sfx" "engine" "synth" "preset" preset
                                 "seed" seed "style" style "variations" variations))
      (answer root "ok" t "preset" preset "seed" seed "style" style
              "files" (string-vector (mapcar (lambda (path) (asset-relative root path)) files))
              "seconds" seconds))))

(defun forge-music (call)
  "A seamless loop: the mood's progression, the key's scale, a seeded melody
and drums, as one WAV whose length is exactly the loop."
  (let* ((root (project-root (music-call-project call)))
         (mood (or (music-call-mood call) "adventure"))
         (seed (or (music-call-seed call) 1))
         (bars (or (music-call-bars call) 8))
         (key (or (music-call-key call) "C"))
         (style (or (music-call-style call) "chiptune"))
         (name (safe-name (music-call-name call) (format nil "~a-theme" mood))))
    (unless (and (<= 4 bars +max-bars+) (zerop (mod bars 4)))
      (fail "bars is ~a; a loop is 4 to ~a bars, and a multiple of 4" bars +max-bars+))
    (multiple-value-bind (samples mood-used bpm-used bars-used)
        (compose-music :mood mood :seed seed :bpm (music-call-bpm call) :bars bars
                       :key key :mode (music-call-mode call) :style style)
      (let ((path (merge-pathnames (format nil "~a.wav" name) (asset-directory root "music"))))
        (write-wav path samples)
        (record-asset root "music" name (list path)
                      (object-json "tool" "forge_music" "mood" mood-used "style" style
                                   "seed" seed "bpm" bpm-used "bars" bars-used "key" key))
        (answer root "ok" t "mood" mood-used "bpm" bpm-used "bars" bars-used
                "seconds" (seconds-of samples)
                "files" (string-vector (list (asset-relative root path))))))))

(defun forge-sprite (call)
  "Two frames of a character, a monster or an item, as one PNG sheet, plus the
frames file an engine reads."
  (let* ((root (project-root (sprite-call-project call)))
         (kind (or (sprite-call-kind call) "character"))
         (seed (or (sprite-call-seed call) 1))
         (size (or (sprite-call-size call) 16))
         (scale (or (sprite-call-scale call) (setting :sprite-scale) 4))
         (palette (sprite-call-palette call))
         (hue (sprite-call-hue call))
         (subject (or (sprite-call-name call)
                      (first (cond ((string= kind "character") +archetype-names+)
                                   ((string= kind "monster") +monster-names+)
                                   (t +item-names+)))))
         (name (safe-name subject "sprite")))
    (unless (member kind +sprite-kinds+ :test #'string=)
      (fail "~a is not a sprite kind this cell draws; choose from ~{~a~^, ~}" kind +sprite-kinds+))
    (unless (<= 8 size 64)
      (fail "size is ~a: a pixel sprite is 8 to 64 pixels a side" size))
    (let* ((sheet (sprite-sheet kind subject seed :size size :palette palette :hue hue))
           (scaled (canvas-scaled sheet (max 1 scale)))
           (directory (asset-directory root "sprite"))
           (png (merge-pathnames (format nil "~a.png" name) directory))
           (meta (merge-pathnames (format nil "~a.json" name) directory)))
      (write-png png scaled)
      (let ((frames (object-json "tool" "forge_sprite" "kind" kind "name" name
                                 "seed" seed "pixel_size" size "scale" scale
                                 "hue" (and hue (string hue)) "palette" palette
                                 "frame_width" (* size scale) "frame_height" (* size scale)
                                 "frames" (string-vector '("idle" "bob"))
                                 "license" +generated-licence+)))
        (with-open-file (out meta :direction :output :if-exists :supersede :if-does-not-exist :create)
          (write-string (object-text frames) out)))
      (record-asset root "sprite" name (list png meta)
                    (object-json "tool" "forge_sprite" "kind" kind "seed" seed
                                 "pixel_size" size "scale" scale))
      (answer root "ok" t "kind" kind "name" name "width" (* 2 size scale) "height" (* size scale)
              "files" (string-vector (list (asset-relative root png) (asset-relative root meta)))))))

(defun forge-tileset (call)
  "A Wang corner set between two terrains, as one PNG: sixteen cells an
engine's autotiler assigns by which neighbours are the upper terrain."
  (let* ((root (project-root (tileset-call-project call)))
         (lower (or (tileset-call-lower call) "grass"))
         (upper (or (tileset-call-upper call) "dirt"))
         (seed (or (tileset-call-seed call) 1))
         (scale (or (tileset-call-scale call) 2))
         (name (safe-name (tileset-call-name call) (format nil "~a-~a" lower upper))))
    (unless (<= 1 scale 8)
      (fail "scale is ~a; a tileset is scaled 1 to 8 times" scale))
    (let* ((sheet (tileset-sheet lower upper seed :scale scale))
           (png (merge-pathnames (format nil "~a.png" name) (asset-directory root "tileset"))))
      (write-png png sheet)
      (record-asset root "tileset" name (list png)
                    (object-json "tool" "forge_tileset" "lower" lower "upper" upper
                                 "seed" seed "scale" scale))
      (answer root "ok" t "lower" lower "upper" upper
              "tile" 16 "scale" scale "columns" 4 "rows" 4
              "files" (string-vector (list (asset-relative root png)))))))

;;; --- the engines -------------------------------------------------------------

(defparameter +web-html+
  "<!doctype html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">
<title>~a</title>
<style>
  html,body{margin:0;background:#1a1c2c;color:#f4f4f4;font:14px system-ui,sans-serif}
  canvas{display:block;margin:0 auto;image-rendering:pixelated;max-width:100vw}
  p{text-align:center}
</style>
</head>
<body>
<canvas id=\"c\" width=\"480\" height=\"270\"></canvas>
<p>arrows or WASD to move, collect the coins in ten seconds</p>
<script>
const c = document.getElementById('c'), g = c.getContext('2d');
let seed = ~d;
function rnd(){ seed = (seed * 1103515245 + 12345) % 2147483648; return seed / 2147483648; }
const player = {x: 240, y: 135, size: 12, speed: 2.2};
const coins = [];
for (let i = 0; i < 8; i++) coins.push({x: 40 + rnd() * 400, y: 40 + rnd() * 190, got: false});
const keys = {};
addEventListener('keydown', e => { keys[e.key.toLowerCase()] = true; });
addEventListener('keyup', e => { keys[e.key.toLowerCase()] = false; });
function beep(freq){
  try {
    const a = new (window.AudioContext || window.webkitAudioContext)();
    const o = a.createOscillator(), v = a.createGain();
    o.type = 'square'; o.frequency.value = freq; v.gain.value = 0.05;
    o.connect(v); v.connect(a.destination); o.start(); o.stop(a.currentTime + 0.08);
  } catch (e) {}
}
let collected = 0, over = false, started = performance.now();
function step(t){
  const dt = Math.min(2.2, (t - (step.last || t)) / 16);
  step.last = t;
  if (!over) {
    if (keys.arrowleft || keys.a) player.x -= player.speed * dt;
    if (keys.arrowright || keys.d) player.x += player.speed * dt;
    if (keys.arrowup || keys.w) player.y -= player.speed * dt;
    if (keys.arrowdown || keys.s) player.y += player.speed * dt;
    player.x = Math.max(6, Math.min(474, player.x));
    player.y = Math.max(6, Math.min(264, player.y));
    for (const coin of coins) {
      if (!coin.got && Math.abs(coin.x - player.x) < 12 && Math.abs(coin.y - player.y) < 12) {
        coin.got = true; collected++; beep(880);
      }
    }
    if (performance.now() - started > 10000) over = true;
  }
  g.fillStyle = '#1a1c2c'; g.fillRect(0, 0, 480, 270);
  g.fillStyle = '#ffcd75';
  for (const coin of coins) if (!coin.got) g.fillRect(coin.x - 5, coin.y - 5, 10, 10);
  g.fillStyle = '#41a6f6'; g.fillRect(player.x - 6, player.y - 6, 12, 12);
  g.fillStyle = '#f4f4f4'; g.font = '16px system-ui';
  g.fillText('coins ' + collected + ' / ' + coins.length, 12, 22);
  if (over) g.fillText('time! reload to play again', 150, 140);
  requestAnimationFrame(step);
}
requestAnimationFrame(step);
</script>
</body>
</html>
"
  "The web engine's scaffold: one page, no build step, no dependency. It
plays in a browser from the file itself, which is what makes it a test of the
assets rather than of a toolchain. ~a is the title, ~d the seed.")

(defparameter +godot-project+
  "; Engine configuration file.
; GameForge scaffold: open this folder in Godot 4 and press play.

config_version=5

[application]

config/name=\"~a\"
run/main_scene=\"res://main.tscn\"
config/features=PackedStringArray(\"4.2\", \"GL Compatibility\")

[display]

window/size/viewport_width=480
window/size/viewport_height=270
window/stretch/mode=\"canvas_items\"
"
  "The Godot engine's project file, with the title a call gave.")

(defparameter +godot-scene+
  "[gd_scene load_steps=3 format=3]

[ext_resource type=\"Script\" path=\"res://player.gd\" id=\"1_player\"]

[sub_resource type=\"RectangleShape2D\" id=\"RectangleShape2D_1\"]
size = Vector2(16, 16)

[node name=\"Main\" type=\"Node2D\"]

[node name=\"Player\" type=\"CharacterBody2D\" parent=\".\"]
position = Vector2(240, 135)
script = ExtResource(\"1_player\")

[node name=\"Body\" type=\"ColorRect\" parent=\"Player\"]
offset_left = -8.0
offset_top = -8.0
offset_right = 8.0
offset_bottom = 8.0
color = Color(0.25, 0.65, 0.96, 1)

[node name=\"Shape\" type=\"CollisionShape2D\" parent=\"Player\"]
shape = SubResource(\"RectangleShape2D_1\")
"
  "The scene: a player the script moves, drawn as a rectangle until the
project's sprite is dropped in.")

(defparameter +godot-script+
  "extends CharacterBody2D

# GameForge scaffold: arrows or WASD, eight-direction movement, a speed you
# can tune. Replace Body's ColorRect with a Sprite2D once forge_sprite has
# written a sheet into assets/sprites/.
const SPEED = 140.0

func _physics_process(_delta: float) -> void:
	var direction := Input.get_vector(\"ui_left\", \"ui_right\", \"ui_up\", \"ui_down\")
	velocity = direction * SPEED
	move_and_slide()
"
  "The player's script: Godot's default ui_* actions, so it runs before any
input map is set up.")

(defun scaffold-web (root title seed)
  (let* ((directory (merge-pathnames "web/" root))
         (page (merge-pathnames "index.html" directory)))
    (ensure-directories-exist page)
    (with-open-file (out page :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string (format nil +web-html+ title seed) out))
    (list page)))

(defun scaffold-godot (root title)
  (let* ((directory (merge-pathnames "godot/" root))
         (project (merge-pathnames "project.godot" directory))
         (scene (merge-pathnames "main.tscn" directory))
         (script (merge-pathnames "player.gd" directory)))
    (ensure-directories-exist scene)
    (with-open-file (out project :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string (format nil +godot-project+ title) out))
    (with-open-file (out scene :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string +godot-scene+ out))
    (with-open-file (out script :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string +godot-script+ out))
    (list project scene script)))

(defparameter +engines+
  '(("web" . ("google-chrome" "chromium" "chromium-browser" "chrome" "firefox" "xdg-open" "open"))
    ("godot" . ("godot" "godot4" "godot3")))
  "The engines this cell scaffolds, and the programs run opens them with,
most wanted first.")

(defun engine-directory (root engine)
  "Where ENGINE's project lives inside ROOT."
  (merge-pathnames (format nil "~a/" engine) root))

(defun engine-scaffold (root engine title seed)
  "ENGINE's project files, written; the namestrings."
  (cond ((string= engine "web") (scaffold-web root title seed))
        ((string= engine "godot") (scaffold-godot root title))
        (t (fail "~a is not an engine this cell scaffolds; choose web or godot (status reports ~
whether unity, blender and unreal are installed)" engine))))

(defun manifest-text-for (root title)
  "The manifest an engine reads to find the project's assets: the ledger,
written out, so an engine needs no idea where the project came from."
  (let ((entries '()))
    (dolist (asset (ledger-assets (read-ledger root)))
      (let ((files (gethash "files" asset)))
        (push (format nil "    {\"kind\": ~a, \"name\": ~a, \"files\": [~{~a~^, ~}]}"
                      (json-string (gethash "kind" asset))
                      (json-string (gethash "name" asset))
                      (map 'list #'json-string (if (vectorp files) files '())))
              entries)))
    (format nil "{~%  \"title\": ~a,~%  \"assets\": [~{~%~a~^,~}~%  ]~%}~%"
            (json-string title)
            (nreverse entries))))

(defun engine-import (root engine title)
  "Every asset the ledger knows, copied in beside the engine's project, with
the manifest naming them; the namestrings written."
  (let* ((directory (engine-directory root engine))
         (written (list (write-text (merge-pathnames
                                     (if (string= engine "web") "game.json" "assets/manifest.json")
                                     directory)
                                    (manifest-text-for root title)))))
    (dolist (asset (ledger-assets (read-ledger root)))
      (let ((files (gethash "files" asset)))
        (loop for file across (if (vectorp files) files (vector))
              for from = (merge-pathnames file root)
              when (probe-file from)
                do (let ((to (merge-pathnames file directory)))
                     (copy-file from to)
                     (push (namestring to) written)))))
    (nreverse written)))

(defun engine-run (root engine)
  "Start ENGINE on the project here, or refuse naming what it looked for."
  (let ((directory (engine-directory root engine)))
    (unless (probe-file directory)
      (fail "there is no ~a project in ~a yet; action=scaffold writes one"
            engine (namestring root)))
    (let ((names (cdr (assoc engine +engines+ :test #'string=)))
          (wanted (if (string= engine "web")
                      (merge-pathnames "index.html" directory)
                      directory)))
      (unless (probe-file wanted)
        (fail "~a has nothing to run; action=scaffold writes it" (namestring wanted)))
      (let ((program (and names (funcall *program-finder* names))))
        (unless program
          (fail "nothing here opens ~a: looked for ~{~a~^, ~} on PATH. Open ~a yourself, ~
or install one of them" engine names (namestring wanted)))
        (let ((command (if (string= engine "web")
                           (list program (namestring wanted))
                           (list program "--path" (namestring directory)))))
          (funcall *launcher* command)
          (values program command))))))

(defun forge-engine (call)
  "Scaffold, import into, or run an engine project; or report what this
machine has."
  (let* ((engine (string-downcase (or (engine-call-engine call) "web")))
         (action (string-downcase (or (engine-call-action call) "scaffold"))))
    (unless (member engine '("web" "godot" "unity" "blender" "unreal") :test #'string=)
      (fail "~a is not an engine this cell knows; choose web, godot, unity, blender or unreal" engine))
    (unless (member action '("scaffold" "import" "run" "status") :test #'string=)
      (fail "~a is not an action this cell takes; choose scaffold, import, run or status" action))
    (if (string= action "status")
        (let ((status (make-hash-table :test #'equal)))
          (dolist (pair (engine-status)) (setf (gethash (car pair) status) (cdr pair)))
          (object-text (object-json "ok" t "engines" status)))
        (progn
          (unless (assoc engine +engines+ :test #'string=)
            (fail "~a is reported by status but not scaffolded here; choose web or godot" engine))
          (let ((root (project-root (engine-call-project call)))
                (title (or (engine-call-title call) "Untitled Quest"))
                (seed (or (engine-call-seed call) 1)))
            (if (string= action "run")
                (multiple-value-bind (program command) (engine-run root engine)
                  (answer root "ok" t "engine" engine "action" "run" "program" program
                          "command" (string-vector (mapcar #'namestring command))))
                (let ((files (if (string= action "import")
                                 (progn (engine-scaffold root engine title seed)
                                        (engine-import root engine title))
                                 (engine-scaffold root engine title seed))))
                  (record-asset root "engine" (format nil "~a-~a" engine action) files
                                (object-json "tool" "forge_engine" "engine" engine "action" action
                                             "title" title "seed" seed))
                  (answer root "ok" t "engine" engine "action" action
                          "files" (string-vector (mapcar (lambda (path) (asset-relative root path))
                                                          files))))))))))

(defun project-title (root)
  "The game's title as the project already knows it: the newest engine
scaffold's title, else the web page's, else a plain word."
  (or (loop for asset in (reverse (ledger-assets (read-ledger root)))
            for recipe = (gethash "recipe" asset)
            when (and (hash-table-p recipe) (stringp (gethash "title" recipe)))
              return (gethash "title" recipe))
      (let ((page (merge-pathnames "web/index.html" root)))
        (when (probe-file page)
          (let* ((html (read-text page))
                 (from (search "<title>" html))
                 (to (and from (search "</title>" html :start2 (+ from 7)))))
            (when (and to (plusp (- to (+ from 7))))
              (subseq html (+ from 7) to)))))))

(defun forge-export (call)
  "PWA bundle and zip, one self-contained page, or a Capacitor project:
the web build, shipped without a network."
  (let* ((root (project-root (export-call-project call)))
         (target (string-downcase (or (export-call-target call) "web")))
         (source (export-call-source call))
         (title (or (export-call-title call) (project-title root) "game")))
    (cond
      ((string= target "web")
       (multiple-value-bind (files zip) (export-web root title source)
         (record-asset root "export" (format nil "~a-web" (slug title)) (cons zip files)
                       (object-json "tool" "forge_export" "target" "web" "title" title))
         (answer root "ok" t "target" "web" "zip" (asset-relative root zip)
                 "files" (string-vector (mapcar (lambda (path) (asset-relative root path)) files))
                 "upload" "itch.io: new project, kind HTML, upload the zip")))
      ((string= target "single")
       (let ((page (export-single root title source)))
         (record-asset root "export" (slug title) (list page)
                       (object-json "tool" "forge_export" "target" "single" "title" title))
         (answer root "ok" t "target" "single" "file" (asset-relative root page))))
      ((string= target "capacitor")
       (multiple-value-bind (files mobile)
           (export-capacitor root title source
                             (or (export-call-app-id call) (format nil "com.gameforge.~a" (slug title))))
         (record-asset root "export" (format nil "~a-mobile" (slug title)) files
                       (object-json "tool" "forge_export" "target" "capacitor" "title" title))
         (answer root "ok" t "target" "capacitor" "directory" (asset-relative root mobile)
                 "files" (string-vector (mapcar (lambda (path) (asset-relative root path)) files))
                 "next" (string-vector (list "npm install" "npm run android")))))
      (t (fail "~a is not an export target here; choose web (a PWA bundle and the zip), single (one ~
.html) or capacitor (a native wrapper project): itch and the godot and unity builds need butler or ~
an engine on this machine" target)))))

;;; --- the command ---------------------------------------------------------------

(defun project-line (root)
  "One line about a project's ledger, or NIL when it has none."
  (when (probe-file (ledger-path root))
    (multiple-value-bind (total counts) (project-summary root)
      (let ((kinds '()))
        (maphash (lambda (kind count) (push (format nil "~d ~a" count kind) kinds)) counts)
        (format nil "~a: ~d assets (~{~a~^, ~})" (namestring root) total (sort kinds #'string<))))))

(defun joined-lines (lines)
  "LINES as one text, one a line."
  (with-output-to-string (out)
    (loop for line in lines
          for first = t then nil
          do (unless first (terpri out))
             (write-string line out))))

(defun run-slash (args session-id)
  "(values TEXT DIALOG) for /gameforge: the section's folder, the engines
found, and the project's ledger."
  (declare (ignore session-id))
  (let* ((lines (list "GameForge"
                      (format nil "directory: ~a" (namestring (setting :directory)))
                      (format nil "default project: ~a" (setting :default-project))))
         (found (remove-if-not #'cdr (engine-status)))
         (missing (remove-if #'cdr (engine-status))))
    (push (format nil "engines: ~{~a~^, ~}"
                  (if found (mapcar (lambda (pair) (format nil "~a (~a)" (car pair) (cdr pair))) found)
                      '("none installed")))
          lines)
    (when missing
      (push (format nil "not found: ~{~a~^, ~}" (mapcar #'car missing)) lines))
    (let* ((raw (nlk:trimmed (or args "")))
           (root (and (plusp (length raw)) (ignore-errors (project-root raw)))))
      (when root
        (let ((line (project-line root))) (when line (push line lines))))
      (values (joined-lines (nreverse lines)) nil))))

;;; --- what the model is told ------------------------------------------------------

(defparameter +primer+
  "GameForge is available through the nodecode-gameforge cell: five tools that
write real game assets into a project folder, offline, plus /gameforge.
  forge_sfx(preset, name?, seed?, variations?, style?, project?)
      a WAV sound effect from the sfxr synthesizer: coin, pickup, laser, shoot,
      explosion, powerup, hit, hurt, jump, blip, select. One file per variant, up
      to 8; style retro, modern, crunchy or distant. The same seed and preset
      write the same bytes, so a variant is a new seed, not a new tuning.
  forge_music(mood, bars?, bpm?, key?, mode?, style?, name?, seed?, project?)
      a seamless WAV loop: bass, arpeggio, melody and drums in the mood's mode
      and chord progression. moods: ~{~a~^, ~}. 4 to 64 bars, a multiple of 4.
  forge_sprite(kind, name?, size?, scale?, hue?, palette?, seed?, project?)
      a 2-frame PNG sheet of a character (~{~a~^, ~}), a monster (~{~a~^, ~}) or an
      item (~{~a~^, ~}), plus a JSON of its frames.
  forge_tileset(lower, upper, scale?, seed?, name?, project?)
      a 16-cell Wang corner set between two terrains (~{~a~^, ~}) as PNG.
  forge_engine(engine, action?, title?, seed?, project?)
      engine web: a complete playable index.html with no build step; godot: a
      project.godot, a scene and a player script. action import copies every
      asset the project has into the engine folder beside a manifest (game.json
      for web, assets/manifest.json for godot); action run opens the engine on
      it, or names the program it looked for; action status: which engines and
      programs this machine actually has.
  forge_export(target, title?, source?, app_id?, project?)
      ship the web build: web writes a PWA bundle (manifest, two icons, a
      service worker) and the zip itch.io takes; single writes one .html with
      every local image, sound and script inlined; capacitor writes a native
      wrapper project. Files only: nothing is uploaded.
Every file lands under the project's assets/ folder and every generation is
recorded in its gameforge.json ledger with its recipe and seed, so the asset
can be rebuilt later. Nothing is uploaded: the synthesis and the drawing are
local arithmetic. Name the project folder the first time , and pass
the same name again to add to it; tell the operator where the files are."
  "What (help :gameforge) answers while the cell runs.")

;;; --- the declaration -------------------------------------------------------------

(nle:define-cell gameforge
  (:section ("gameforge")
    (:guide "nothing here is required: a call may name a project folder, and the default project sits under the directory; sprite_scale is how many screen pixels one drawn pixel becomes")
    ("directory" :directory :default "gameforge/"
                 :doc "where game projects are kept, under the home")
    ("default_project" :string :default "game"
                       :doc "the project a call that names none writes to")
    ("sprite_scale" :integer :default 4 :min 1
                    :doc "screen pixels a drawn pixel is scaled to in a saved sprite"))
  (:tool "forge_sfx" (nlk:contract 'sfx-call) 'forge-sfx)
  (:tool "forge_music" (nlk:contract 'music-call) 'forge-music)
  (:tool "forge_sprite" (nlk:contract 'sprite-call) 'forge-sprite)
  (:tool "forge_tileset" (nlk:contract 'tileset-call) 'forge-tileset)
  (:tool "forge_engine" (nlk:contract 'engine-call) 'forge-engine)
  (:tool "forge_export" (nlk:contract 'export-call) 'forge-export)
  (:help :gameforge "gameforge: synthesized game assets and engine scaffolds - /gameforge"
         +primer+)
  (:command "gameforge" 'run-slash
            :argument-hint "[project]"
            :description "GameForge: the project's assets, and the engines installed"))
