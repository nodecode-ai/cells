;;;; contracts.lisp --- the eighteen tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.
;;;;
;;;; The argument names and their meanings are the original plugin's; the
;;;; ranges the original clamped are said in a member's :DOC and clamped in the
;;;; body, because a contract states a shape and a clamp is a rule of the verb.

(in-package #:nodecode-android-emulator)

(nlk:define-contract shell-command
  (:doc "Run an adb shell command inside the emulator (the intentional raw device shell: it runs on Android, never on this machine). Returns what it wrote, what it complained about, and its exit code.")
  ("command" :text :required t
   :doc "the device shell command, e.g. 'ls /sdcard', 'pm list packages', 'dumpsys battery'")
  ("timeout_seconds" :integer :default 30
   :doc "how long the command may run, in seconds: 1-600, clamped, default 30"))

(nlk:define-contract install-apk
  (:doc "Install an APK on the emulator from a path on this machine.")
  ("apk_path" :text :required t
   :doc "absolute path to the .apk file on this machine")
  ("replace" :boolean :default t :slot replace-p
   :doc "replace an existing install of the same package (default true)")
  ("grant_permissions" :boolean :default t
   :doc "grant every runtime permission the app declares (default true)"))

(nlk:define-contract uninstall-package
  (:doc "Uninstall an app from the emulator by package name.")
  ("package" :text :required t :slot app-package
   :doc "the package name, e.g. com.example.app"))

(nlk:define-contract screenshot-take
  (:doc "Take a screenshot of the emulator display and save it as a PNG on this machine. Answers the file's path.")
  ("output_path" :string :default ""
   :doc "where to save the PNG on this machine, under the home directory, the cwd, the temp dir or output_root; omit for emu_screenshot.png in the cwd")
  ("overwrite" :boolean :default nil
   :doc "allow replacing an existing file (default false)"))

(nlk:define-contract tap-at
  (:doc "Tap the emulator screen at (x, y) in pixels.")
  ("x" :integer :required t :doc "X coordinate, 0-100000")
  ("y" :integer :required t :doc "Y coordinate, 0-100000"))

(nlk:define-contract swipe-from
  (:doc "Swipe the emulator screen from (x1, y1) to (x2, y2).")
  ("x1" :integer :required t :doc "start X, 0-100000")
  ("y1" :integer :required t :doc "start Y, 0-100000")
  ("x2" :integer :required t :doc "end X, 0-100000")
  ("y2" :integer :required t :doc "end Y, 0-100000")
  ("duration_ms" :integer :default 300
   :doc "how long the swipe takes, in milliseconds: 1-10000, clamped, default 300"))

(nlk:define-contract typed-text
  (:doc "Type text into the focused field on the emulator. Spaces and shell metacharacters are escaped for you; a literal '%s' renders as a space, as adb's own input text does.")
  ("text" :text :required t :doc "the text to type, at most 2000 characters"))

(nlk:define-contract key-event
  (:doc "Press one key on the emulator. Common codes: HOME, BACK, ENTER, DEL, POWER, VOLUME_UP, VOLUME_DOWN, TAB, ESCAPE, DPAD_UP, DPAD_DOWN, DPAD_LEFT, DPAD_RIGHT, MENU, APP_SWITCH.")
  ("keycode" :text :required t
   :doc "an Android keycode name, with or without the KEYCODE_ prefix (HOME, KEYCODE_BACK)"))

(nlk:define-contract launch-intent
  (:doc "Launch an app by package name (optionally one activity), or open a URL in the emulator's browser when no package is given.")
  ("package" :string :default "" :slot app-package
   :doc "the package to launch, e.g. com.android.chrome; omit it and give url instead")
  ("activity" :string :default ""
   :doc "a specific activity, e.g. com.android.chrome.Main; omit for the launcher's own")
  ("url" :string :default ""
   :doc "a URL to open in the browser, when package is omitted"))

(nlk:define-contract package-list
  (:doc "List the packages installed in the emulator, optionally filtered by a keyword.")
  ("filter" :string :default ""
   :doc "a package-name filter, e.g. 'google' or 'com.example'; letters, digits, dot, dash and underscore"))

(nlk:define-contract push-file
  (:doc "Copy a file from this machine into the emulator.")
  ("local_path" :text :required t :doc "the file on this machine")
  ("remote_path" :text :required t :doc "where it goes in the emulator, e.g. /sdcard/file.txt"))

(nlk:define-contract pull-file
  (:doc "Copy a file out of the emulator onto this machine. The destination must be under the home directory, the cwd, the temp dir or output_root.")
  ("remote_path" :text :required t :doc "the file in the emulator, e.g. /sdcard/file.txt")
  ("local_path" :text :required t :doc "where it goes on this machine")
  ("overwrite" :boolean :default nil
   :doc "allow replacing an existing file (default false)"))

(nlk:define-contract logcat-read
  (:doc "Read recent logcat output from the emulator, optionally filtered by tag and priority.")
  ("filter" :string :default ""
   :doc "the logcat filter expression, e.g. 'ActivityManager:I *:S' or '*:E'; each spec is tag:priority with priority one of V D I W E F S")
  ("lines" :integer :default 50
   :doc "how many lines to read, 1-5000, clamped, default 50"))

(nlk:define-contract gps-fix
  (:doc "Set a simulated GPS location on the emulator, or clear the override. Both lat and lng are needed to set one; clear=true removes it.")
  ("lat" :number :default nil :doc "latitude, -90 to 90 (omit with clear)")
  ("lng" :number :default nil :doc "longitude, -180 to 180 (omit with clear)")
  ("clear" :boolean :default nil :doc "clear the location override instead of setting one"))

(nlk:define-contract battery-state
  (:doc "Simulate the emulator's battery: a level, unplugged (draining), or back to the real values.")
  ("level" :integer :default nil :doc "battery level 0-100, clamped")
  ("unplug" :boolean :default nil :doc "report the device unplugged and draining")
  ("reset" :boolean :default nil :doc "go back to the emulator's real battery values"))

(nlk:define-contract network-state
  (:doc "Simulate the emulator's network conditions.")
  ("condition" :text :required t :slot network-condition :options '("offline" "slow" "fast")
   :doc "offline (wifi and data off), slow (500 ms delay and 10% loss on wlan0), or fast (back to normal)"))

(nlk:define-contract deep-link
  (:doc "Open a deep link or URL scheme in the emulator, e.g. myapp://path or https://example.com.")
  ("url" :text :required t :doc "the URL or deep link, scheme included"))
