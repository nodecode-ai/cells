;;;; nodecode-channel-matrix.asd --- Matrix channel adapter add-on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-CELL when channels.matrix is
;;;; present in the shared config. The .asd sits inside its own folder so the
;;;; kit finds it through the folder registry. The kit is shipped with every
;;;; release; this folder is installed from the add-on hub (/setup, Choose) or
;;;; cloned into ~/.nodecode/addons/.
;;;;
;;;; Matrix is reached over the Client-Server API's /sync long poll, so the
;;;; organism needs no public address and holds no socket. The command menu
;;;; is MSC4391 state in each room (commands.lisp); cl-base64 and ironclad
;;;; ride in with the kit and are named for its state keys. Rooms with
;;;; end-to-end encryption are refused in words: this version carries no
;;;; crypto.

(defsystem "nodecode-channel-matrix"
  :description "Talk to nodecode through a Matrix account"
  :license "MIT"
  :version "0.1.0"
  ;; The kit brings what a request is made of (dexador, quri) and the
  ;; organism the rest; naming only the kit keeps the hub's depends_on the one
  ;; folder a person installs with it.
  :depends-on ("nodecode-channel-kit" "cl-base64" "ironclad/digest/sha256")
  :serial t
  :components ((:file "package")
               (:file "markdown")
               (:file "events")
               (:file "rest")
               (:file "commands")
               (:file "probe")
               (:file "adapter"))
  :in-order-to ((test-op (test-op "nodecode-channel-matrix/test"))))

(defsystem "nodecode-channel-matrix/test"
  :description "Matrix adapter tests, registered under the CHANNEL-MATRIX- prefix."
  :license "MIT"
  :depends-on ("nodecode-channel-matrix" "nodecode-channel-kit/test")
  :pathname "test/"
  :serial t
  :components ((:file "fake-matrix")
               (:file "markdown-test")
               (:file "events-test")
               (:file "rest-test")
               (:file "adapter-test")
               (:file "commands-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-matrix-tests)))
