;;;; nodecode-forum-index.asd --- a directory of the hacking, fraud and security
;;;; forums, for the model.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The list is a Dread /d/hacking post of 10 November 2026, relayed on X; the
;;;; code is ours and holds no forum data beyond the post's own names, links,
;;;; categories and notes. See NOTICE.
;;;;
;;;; Three files beside this one: DATA.LISP, the list itself and the legend,
;;;; QUERY.LISP, the filters and the blocks a person reads, and CELL.LISP, the
;;;; one declaration. The .asd sits INSIDE its folder: install by putting -- or
;;;; symlinking -- this directory under ~/.nodecode/cells/.

(defsystem "nodecode-forum-index"
  :description "A directory of hacking, fraud and security forums: who trades in what, and where"
  :license "MIT"
  :author "the Dread /d/hacking poster (the list), Nodecode (this cell)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "query")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-forum-index/test"))))

(defsystem "nodecode-forum-index/test"
  :description "forum-index tests. Registered into the shared nodecode.test registry; RUN-FORUM-INDEX-TESTS filters by the FORUM-INDEX-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-forum-index" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-forum-index-tests)))
