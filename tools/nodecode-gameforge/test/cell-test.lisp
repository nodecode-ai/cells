;;;; cell-test.lisp --- each tool's file, its ledger line, and every refusal,
;;;; with nothing dialled and nothing installed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is pinned is what a user gets: a WAV whose own header says 44.1 kHz
;;;; mono, a PNG whose own IHDR says the size that was asked for, a ledger
;;;; entry naming the file and the recipe, a seed that reproduces an asset byte
;;;; for byte, a web page and a Godot project that exist where they are said
;;;; to, and the refusals that cost nothing because they come before a write.

(in-package #:nodecode.test)

(define-test-slice "gameforge" "GAMEFORGE-" :start nodecode-gameforge:start-cell)

;;; --- the cell's own lifecycle ---------------------------------------------------

(define-cell-lifecycle-tests "gameforge"
  (:config "directory" (namestring (nlk:home "gameforge-cell-test/")))
  (:help :gameforge)
  (:command "gameforge")
  (:running (is (nle::find-tool "forge_sfx") "the sound tool is registered")
            (is (nle::find-tool "forge_music") "the music tool is registered")
            (is (nle::find-tool "forge_sprite") "the sprite tool is registered")
            (is (nle::find-tool "forge_tileset") "the tileset tool is registered")
            (is (nle::find-tool "forge_engine") "the engine tool is registered")
            (is (nle::find-tool "forge_export") "the export tool is registered")
            (is (equal "game" (nodecode-gameforge::setting :default-project)))
            (is (= 4 (nodecode-gameforge::setting :sprite-scale))))
  (:stopped (is (null (nle::find-tool "forge_sfx")) "stop takes every tool back out")
            (is (null (nle::find-tool "forge_engine"))))
  (:refused ("sprite_scale" 0) ("directory" 7))
  (:idle nodecode-gameforge:gameforge-error (nodecode-gameforge::run-slash "nothing" "s1")))

;;; --- fixtures ---------------------------------------------------------------------

(defun gameforge-directory ()
  "A folder of this test's own under the scratch home, so no two tests share one."
  (merge-pathnames (format nil "gameforge-~12,'0x/" (random (expt 16 12))) (nlk:home)))

(defvar *gameforge-test-directory* nil
  "The directory the running test's cell was started on.")

(defun test-directory ()
  "The folder this test's cell writes into."
  *gameforge-test-directory*)

(defmacro with-gameforge ((&rest pairs) &body body)
  "The cell started on PAIRS, in a fresh directory of this test's own."
  `(let* ((*gameforge-test-directory* (gameforge-directory))
          (stop (gameforge-start "directory" (namestring *gameforge-test-directory*) ,@pairs)))
     (unwind-protect (progn ,@body)
       (funcall stop))))

(defun call-tool (name &rest arguments)
  "Call a registered tool the way the provider does: through its contract."
  (let* ((entry (nle::find-tool name))
         (schema (second entry)))
    (unless entry (error "no tool named ~a is registered" name))
    (funcall (third entry) (nlk:contract-decode schema (apply #'nlk:make-json-object arguments)))))

(defun answer-field (text key)
  (gethash key (nlk:decode-json text)))

(defun octets-of (path)
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(defun project-of (directory &optional (name "demo"))
  (merge-pathnames (format nil "~a/" name) directory))

;;; --- the sound effects ---------------------------------------------------------------

(deftest gameforge-cell-a-sound-effect-is-a-wav-the-header-agrees-with ()
  (with-gameforge ()
    (let* ((answer (call-tool "forge_sfx" "project" "demo" "preset" "coin" "seed" 7))
           (file (merge-pathnames "assets/sfx/coin.wav" (project-of (gameforge-directory)))))
      (declare (ignore file))
      (is (equal "coin" (answer-field answer "preset")) "the answer names the preset")
      (is (equal t (answer-field answer "ok")) "and says it worked")
      (let ((named (aref (answer-field answer "files") 0)))
        (is (search "coin.wav" named) "the answer names the file it wrote")))))

(deftest gameforge-cell-the-wav-is-44100-mono-16-bit-and-one-seed-is-one-sound ()
  (with-gameforge ()
    (let* ((directory (nodecode-gameforge::setting :directory))
           (answer (call-tool "forge_sfx" "project" "demo" "preset" "laser" "seed" 3))
           (path (merge-pathnames (aref (answer-field answer "files") 0) (project-of directory)))
           (first-bytes (octets-of path)))
      (is (probe-file path) "the file the answer names is on disk")
      (multiple-value-bind (channels rate bits bytes) (nodecode-gameforge::read-wav-header path)
        (is (= 1 channels) "mono")
        (is (= 44100 rate) "44.1 kHz")
        (is (= 16 bits) "16-bit samples")
        (is (plusp bytes) "and a body")
        (is (> (/ bytes 2.0 44100.0) 0.05) "long enough to hear"))
      ;; the same seed, the same bytes: a variation is a new seed
      (let* ((again (call-tool "forge_sfx" "project" "demo" "preset" "laser" "seed" 3
                                "name" "laser2"))
             (second-path (merge-pathnames (aref (answer-field again "files") 0)
                                            (project-of directory))))
        (is (equalp first-bytes (octets-of second-path)) "one seed, one sound"))
      ;; the ledger holds the recipe that made it
      (let* ((ledger (nlk:decode-json
                       (uiop:read-file-string
                        (merge-pathnames "gameforge.json" (project-of directory)))))
             (assets (gethash "assets" ledger)))
        (is (>= (length assets) 2) "every generation is a ledger entry")
        (let ((entry (aref assets 0)))
          (is (equal "sfx" (gethash "kind" entry)))
          (is (equal "laser" (gethash "preset" (gethash "recipe" entry))) "the preset is written down"))))))

(deftest gameforge-cell-a-refused-call-writes-nothing ()
  (with-gameforge ()
    (let ((directory (nodecode-gameforge::setting :directory)))
      (progn
               (is (signals-error nlk:contract-refusal
                                  (call-tool "forge_sfx" "project" "demo")) "a call with no preset refuses: the preset is required")
               (is (signals-error nodecode-gameforge:gameforge-error
                                  (call-tool "forge_sfx" "preset" "coin" "project" "../escape")) "a project path that walks out of the folder refuses")
               (is (signals-error nlk:contract-refusal
                                  (call-tool "forge_sfx" "preset" "nonsense" "project" "demo")) "a preset this cell does not carry refuses")
               (is (signals-error nodecode-gameforge:gameforge-error
                                  (call-tool "forge_sfx" "preset" "coin" "project" "demo" "variations" 99)) "ninety-nine variations refuse")
               (is (null (probe-file (merge-pathnames "gameforge.json" (project-of directory)))) "and a refused call never made a project")))))

;;; --- pixels -------------------------------------------------------------------------

(deftest gameforge-cell-a-sprite-is-a-png-of-two-frames-at-the-scale-asked-for ()
  (with-gameforge ()
    (let* ((directory (nodecode-gameforge::setting :directory)))
      (unwind-protect
           (progn
             (let* ((answer (call-tool "forge_sprite" "project" "demo" "kind" "character" "name" "hero"
                                       "size" 16 "scale" 4))
                    (named (aref (answer-field answer "files") 0))
                    (path (merge-pathnames named (project-of directory))))
               (is (probe-file path) "the sheet is on disk where the answer says")
               (multiple-value-bind (width height) (nodecode-gameforge::read-png-size path)
                 (is (= 128 width) "two 16-pixel frames at four pixels each")
                 (is (= 64 height) "one frame tall"))
               (is (equalp #(137 80 78 71) (subseq (octets-of path) 0 4)) "a real PNG signature"))
             (let* ((coin (call-tool "forge_sprite" "project" "demo" "kind" "item" "name" "coin"
                                     "palette" "pico8"))
                    (frames (aref (answer-field coin "files") 1)))
               (is (search ".json" frames) "the frames file is named in the answer")
               (is (probe-file (merge-pathnames frames (project-of directory))) "the frames file is written beside the sheet"))
             (is (signals-error nodecode-gameforge:gameforge-error
                                (call-tool "forge_sprite" "project" "demo" "kind" "character"
                                           "name" "a dragon")) "a name this cell cannot draw refuses"))
        (funcall stop)))))

(deftest gameforge-cell-a-tileset-is-sixteen-cells-between-two-terrains ()
  (with-gameforge ()
    (let* ((directory (nodecode-gameforge::setting :directory)))
      (unwind-protect
           (progn
             (let* ((answer (call-tool "forge_tileset" "project" "demo" "lower" "grass" "upper" "dirt" "scale" 1))
                    (named (aref (answer-field answer "files") 0)))
              (is (probe-file (merge-pathnames named (project-of directory))) "the sheet is on disk")
              (multiple-value-bind (width height)
                 (nodecode-gameforge::read-png-size
                  (merge-pathnames named (project-of directory)))
               (is (= 64 width) "four cells of sixteen")
               (is (= 64 height))))
             (is (signals-error nlk:contract-refusal
                                (call-tool "forge_tileset" "project" "demo" "lower" "nonsense")) "a terrain this cell does not tile refuses"))
        (funcall stop)))))

;;; --- music ---------------------------------------------------------------------------

(deftest gameforge-cell-a-loop-is-as-long-as-the-bars-and-the-tempo-say ()
  (with-gameforge ()
    (let* ((directory (nodecode-gameforge::setting :directory)))
      (unwind-protect
           (progn
             (let ((answer (call-tool "forge_music" "project" "demo" "mood" "tense"
                                       "bars" 4 "key" "D")))   ; tense is 140 bpm
               (is (= 140 (answer-field answer "bpm")) "the mood's tempo, in the answer")
               (is (= 4 (answer-field answer "bars")))
               (is (< 6.5 (answer-field answer "seconds") 7.0) "four bars at 140 is 6.86 seconds"))
             (is (signals-error nodecode-gameforge:gameforge-error
                                (call-tool "forge_music" "project" "demo" "mood" "tense" "bars" 5)) "five bars is not a loop of fours")
             (is (signals-error nodecode-gameforge:gameforge-error
                                (call-tool "forge_music" "project" "demo" "mood" "tense" "key" "H")) "a key that is not a note refuses"))
        (funcall stop)))))

;;; --- engines --------------------------------------------------------------------------

(deftest gameforge-cell-the-web-and-godot-scaffolds-write-what-they-say ()
  (with-gameforge ()
    (let* ((directory (nodecode-gameforge::setting :directory)))
      (unwind-protect
           (progn
             (let* ((answer (call-tool "forge_engine" "project" "demo" "engine" "web" "title" "Night Quest"))
                    (named (aref (answer-field answer "files") 0))
                    (page (uiop:read-file-string (merge-pathnames named (project-of directory)))))
               (is (search "<canvas" page) "a page that draws")
               (is (search "Night Quest" page) "with the title it was given"))
             (call-tool "forge_engine" "project" "demo" "engine" "godot" "title" "Night Quest")
             (let ((project (uiop:read-file-string
                             (merge-pathnames "godot/project.godot" (project-of directory)))))
               (is (search "config/name=\"Night Quest\"" project) "Godot reads a real project file")
               (is (search "main.tscn" project) "pointing at the scene"))
             (let ((answer (call-tool "forge_engine" "engine" "web" "action" "status")))
               (is (hash-table-p (answer-field answer "engines")) "status answers what is installed"))
             (is (signals-error nlk:contract-refusal
                                (call-tool "forge_engine" "project" "demo" "engine" "cryengine")) "an engine this cell does not scaffold refuses"))
        (funcall stop)))))

;;; --- shipping it -------------------------------------------------------------------

(deftest gameforge-cell-export-writes-a-pwa-bundle-and-the-zip-itch-takes ()
  (with-gameforge ()
    (let* ((directory (nodecode-gameforge::setting :directory))
           (root (project-of directory)))
      (call-tool "forge_engine" "project" "demo" "engine" "web" "title" "Night Quest")
      (let ((answer (call-tool "forge_export" "project" "demo" "target" "web")))
        (is (search "dist/night-quest-web.zip" (format nil "~a" (answer-field answer "zip"))) "the answer names the zip")
        (is (string= "Night Quest"
                     (gethash "name" (nlk:decode-json
                                       (uiop:read-file-string
                                        (merge-pathnames "dist/web/manifest.webmanifest" root))))) "the manifest carries the title")
        (multiple-value-bind (width height)
            (nodecode-gameforge::read-png-size (merge-pathnames "dist/web/icons/icon-192.png" root))
          (is (and (= width 192) (= height 192)) "an icon of the size the manifest promises"))
        (is (search "CACHE" (uiop:read-file-string (merge-pathnames "dist/web/sw.js" root))) "a service worker that caches the build")
        (is (search "rel=\"manifest\""
                    (uiop:read-file-string (merge-pathnames "dist/web/index.html" root))) "the page knows it is installable")
        (let* ((zip (merge-pathnames "dist/night-quest-web.zip" root))
               (names (nodecode-gameforge::zip-names zip)))
          (is (member "index.html" names :test #'string=) "the zip holds the page at its root")
          (is (member "manifest.webmanifest" names :test #'string=) "and the manifest")
          (is (with-open-file (stream zip :direction :input :element-type '(unsigned-byte 8))
                (plusp (file-length stream))) "a file on disk"))))))

(deftest gameforge-cell-export-single-inlines-the-build-into-one-page ()
  (with-gameforge ()
    (let* ((directory (nodecode-gameforge::setting :directory))
           (root (project-of directory)))
      (call-tool "forge_sfx" "project" "demo" "preset" "coin")
      (nodecode-gameforge::copy-file (merge-pathnames "assets/sfx/coin.wav" root)
                                      (merge-pathnames "web/coin.wav" root))
      (nodecode-gameforge::write-text
       (merge-pathnames "web/index.html" root)
       "<!doctype html><html><head></head><body><audio src=\"coin.wav\"></audio></body></html>")
      (let* ((answer (call-tool "forge_export" "project" "demo" "target" "single"))
             (page (uiop:read-file-string
                    (merge-pathnames (gethash "file" (nlk:decode-json answer)) root))))
        (is (search "data:audio/wav;base64," page) "the sound is inside the page")
        (is (not (search "src=\"coin.wav\"" page)) "and nothing points at a file beside it")))))

(deftest gameforge-cell-export-refuses-what-it-cannot-ship ()
  (with-gameforge ()
    (progn
      (is (signals-error nodecode-gameforge:gameforge-error
                         (call-tool "forge_export" "project" "demo" "target" "web")) "a project with no web build refuses")
      (call-tool "forge_engine" "project" "demo" "engine" "web")
      (is (signals-error nodecode-gameforge:gameforge-error
                         (call-tool "forge_export" "project" "demo" "target" "web"
                                    "source" "../../elsewhere")) "a source outside the project refuses")
      (is (signals-error nlk:contract-refusal
                         (call-tool "forge_export" "project" "demo" "target" "itch")) "a target only butler can do refuses"))))

;;; --- the engine's other two actions ---------------------------------------------------

(deftest gameforge-cell-engine-import-brings-the-assets-in-with-a-manifest ()
  (with-gameforge ()
    (let* ((directory (nodecode-gameforge::setting :directory))
           (root (project-of directory)))
      (call-tool "forge_sfx" "project" "demo" "preset" "coin")
      (call-tool "forge_sprite" "project" "demo" "kind" "character" "name" "hero")
      (call-tool "forge_engine" "project" "demo" "engine" "web" "action" "import" "title" "Night Quest")
      (let ((manifest (uiop:read-file-string (merge-pathnames "web/game.json" root))))
        (is (search "Night Quest" manifest) "the manifest names the game")
        (is (search "coin" manifest) "and the sound the project made"))
      (is (probe-file (merge-pathnames "web/assets/sfx/coin.wav" root)) "the sound itself is beside the page")
      (is (probe-file (merge-pathnames "web/assets/sprites/hero.png" root)) "and so is the sprite"))))

(deftest gameforge-cell-engine-run-refuses-without-a-program-and-starts-with-one ()
  (with-gameforge ()
    (let* ((directory (nodecode-gameforge::setting :directory))
           (root (project-of directory)))
      (call-tool "forge_engine" "project" "demo" "engine" "web")
      (let ((nodecode-gameforge::*program-finder* (lambda (names) (declare (ignore names)) nil)))
        (is (signals-error nodecode-gameforge:gameforge-error
                           (call-tool "forge_engine" "project" "demo" "engine" "web" "action" "run")) "a machine with nothing to open a page refuses, and says what it looked for"))
      (let* ((launched '())
             (nodecode-gameforge::*program-finder*
              (lambda (names) (declare (ignore names)) "/usr/bin/fake-browser"))
             (nodecode-gameforge::*launcher* (lambda (command) (push command launched)))
             (answer (call-tool "forge_engine" "project" "demo" "engine" "web" "action" "run")))
        (is (equal "/usr/bin/fake-browser" (answer-field answer "program")) "the answer names the program it started")
        (is (search "index.html" (format nil "~{~a~}" (coerce (answer-field answer "command") 'list))) "on the page it scaffolded")
        (is (= 1 (length launched)) "one process asked for, recorded here and not started"))
      (is (probe-file (merge-pathnames "web/index.html" root)) "nothing above disturbed the build"))))
