;;;; contracts.lisp --- the six tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-athena)

(nlk:define-contract store-search
  (:doc "Search the app catalogue for installable AI apps: image generators, upscalers, voice cloners, LLM front ends, agent projects. This searches the whole store, not just what is installed. Answers each app's name, its git repository, its version, a short description, its tags and an install_with handle; pass that handle straight to athena_install.")
  ("query" :text :required t
   :doc "search terms, e.g. 'image upscaler', 'voice clone', 'whisper', 'stable diffusion'")
  ("limit" :integer :default 20
   :doc "how many apps to answer, at most 60"))

(nlk:define-contract app-list
  (:doc "List every app installed in the local app store: its id, the folder it lives in, whether it is running and, when it is, the process id and the URL its web UI answers on. This is the inventory to read before launching, reading a log of or stopping anything. An empty answer means nothing is installed yet: run athena_search and then athena_install first.")
  ("app" :string :default ""
   :doc "answer only this one app's row; omit for every installed app"))

(nlk:define-contract app-install
  (:doc "Install an app into the local app store by cloning its git repository into the apps folder. Accepts a full git URL, an owner/repo shorthand, or the name of a catalog app, which is resolved to its repository first. The app's own code is then run by athena_launch, in this folder, with your files and your privileges: nothing here sandboxes it.")
  ("url" :text :required t
   :doc "the app to install: a git URL such as https://github.com/owner/repo, an owner/repo shorthand, or a catalog app name answered by athena_search"))

(nlk:define-contract app-launch
  (:doc "Start an installed app so its local web UI becomes reachable: it runs the app's own start script in the app's folder, in the background, with its output going to the app's log. A cold start often needs ten or more seconds to bind a port, so a missing url means 'still booting', not 'failed': call athena_logs to see what it is doing.")
  ("app" :text :required t
   :doc "the app id, as athena_apps answers it")
  ("script" :string :default ""
   :doc "the command to run instead of the one the folder suggests, e.g. 'python3 app.py'; omit to run start.sh, start.js, start.py or run.sh, whichever the folder has"))

(nlk:define-contract app-stop
  (:doc "Stop an app this cell started, ending the process it launched and freeing the port the app held.")
  ("app" :text :required t
   :doc "the app id, as athena_apps answers it"))

(nlk:define-contract app-logs
  (:doc "Read an installed app's output, and what its log says went wrong. The reading names the cause (the GPU's memory, the machine's memory, a port already taken, a missing package, the install script never run, a credential the app did not get, the network) with the action that answers it; why=1 answers only the reading, which is the fastest way to answer 'why did this fail'. A stopped app's log is kept, so a failure can be read after the fact.")
  ("app" :text :required t
   :doc "the app id, as athena_apps answers it")
  ("lines" :integer :default 80
   :doc "how many trailing lines of the log to answer")
  ("why" :boolean :default nil
   :doc "answer only the reading, the named cause and its fix, without the log lines"))


(nlk:define-contract store-status
  (:doc "What the app store is pointed at and what it holds: the apps folder, the catalogue it reads, the git it clones with, and how many apps are installed and running, and with verbose each app's own line. The cheapest way to check the cell is set up before any other verb.")
  ("verbose" :boolean :default nil
   :doc "also answer one line per installed app, as athena_apps does"))
