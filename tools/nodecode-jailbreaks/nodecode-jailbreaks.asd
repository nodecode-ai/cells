;;;; nodecode-jailbreaks.asd --- a working collection of jailbreak prompts, indexed and quotable.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One source: togg53192-cmd/jailbreaks, 51 prompt files in 19 folders, 716,315
;;;; bytes between them at head ee999ee6 (11 November 2026). Every size, line count and digest
;;;; in DATA.LISP is measured from that collection; every sentence in :SAYS is our
;;;; reading of the file. The collection carries no licence file, so this cell
;;;; indexes and quotes it and fetches each prompt from the author's own repo --
;;;; it redistributes none of them. See NOTICE.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking -- this
;;;; directory under ~/.nodecode/cells/.

(defsystem "nodecode-jailbreaks"
  :description "A working collection of jailbreak prompts, indexed: which model each one targets, what kind it is, and any of them quoted by line"
  :license "MIT"
  :author "togg53192-cmd (the collection), Nodecode (this cell)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "query")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-jailbreaks/test"))))

(defsystem "nodecode-jailbreaks/test"
  :description "jailbreaks tests. Registered into the shared nodecode.test registry; RUN-JAILBREAKS-TESTS filters by the JAILBREAKS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-jailbreaks" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-jailbreaks-tests)))
