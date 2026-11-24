;;;; cell-test.lisp --- the tool, the command, the refusals, the answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here stubs I/O because there is none: a value is a string and
;;;; the answer is a string, worked out in MODES.LISP with no network, no
;;;; subprocess and no file read. What is proved is the wiring: that the
;;;; start registers the tool the model calls and /chmod, that a call through
;;;; the registry's own preflight and body answers the explanation, that each
;;;; kind of value reads the way it should, that a value that is not a mode
;;;; answers a failed call naming the fault, and that stop takes it all back.

(in-package #:nodecode.test)

(define-test-slice "chmod-ref" "CHMOD-REF-CELL-" :start nodecode-chmod-ref:start-cell)

(define-cell-lifecycle-tests "chmod-ref"
  (:help :chmod-ref)
  (:command "chmod")
  (:running (is (nle::find-tool "chmod_explain") "the tool the model calls")
            (is (= 200 (nodecode-chmod-ref::setting :max-target-chars)) "200 characters by default"))
  (:refused ("max_target_chars" 10))
  (:idle nodecode-chmod-ref:chmod-ref-error
         (nodecode-chmod-ref:explain "755")
         (nodecode-chmod-ref:summarise "755")))

;;; --- fixtures -------------------------------------------------------------------

(defun chmod-call (target)
  "The answer TARGET gets through the registry, the way a model's call reaches
it: the tool's own preflight, then its body with what the declaration made."
  (let* ((entry (nle::find-tool "chmod_explain"))
         (raw (nlk:json-object "target" target)))
    (multiple-value-bind (arguments refusal)
        (nle::tool-call-preflight "chmod_explain" raw entry)
      (if refusal
          refusal
          (funcall (third entry) (nle::tool-body-arguments entry arguments))))))

;;; --- the tool -------------------------------------------------------------------

(deftest chmod-ref-cell-the-tool-explains-a-mode-in-both-forms ()
  (with-cell-stop ((chmod-ref-start))
    (is-carrying (text (chmod-call "755"))
      ("Mode 755 (rwxr-xr-x)" "the head names the mode and its bits")
      ("octal     755")
      ("ls -l     -rwxr-xr-x for a file, drwxr-xr-x for a directory")
      ("owner     read, write, execute") ("others    read, execute")
      ("special   none"))
    (is-carrying (text (chmod-call "-rw-r--r--"))
      ("Mode 644 (rw-r--r--), a regular file" "ls -l's own text, read back")
      ("group     read"))
    (is-carrying (text (chmod-call "100644"))
      ("st_mode   100644 (file type and permission bits, as stat and git write them)"))
    (is-carrying (text (chmod-call "4755"))
      ("rwsr-xr-x" "the setuid s is read, not assumed")
      ("special   setuid")
      ("setuid: the program runs with its owner's rights"))
    (is-carrying (text (chmod-call "drwxrwxrwt"))
      ("Mode 1777 (rwxrwxrwt), a directory")
      ("owner     list names, add, rename and delete entries, enter" "a directory's x is enter")
      ("sticky on a directory"))))

(deftest chmod-ref-cell-a-change-is-applied-the-way-chmod-applies-it ()
  (with-cell-stop ((chmod-ref-start))
    (is-carrying (text (chmod-call "644 u+x,go-w"))
      ("Mode 644 with u+x,go-w")
      ("before    rw-r--r--  (644)")
      ("u+x       rwxr--r--  (744)  add execute for the owner")
      ("after     rwxr--r--  (744)"))
    (is-carrying (text (chmod-call "u+x,go-w"))
      ("chmod u+x,go-w") ("on 644    rwxr--r--  (744)") ("on 755    rwxr-xr-x  (755)"))
    (is-carrying (text (chmod-call "2775 g-s"))
      ("turn off setgid") ("(775)" "GNU chmod takes the setgid bit off"))
    (is-carrying (text (chmod-call "100644 100755"))
      ("Mode 100644 -> 100755")
      ("adds      execute for owner, group and others" "the change git shows"))))

(deftest chmod-ref-cell-a-umask-says-what-new-files-get ()
  (with-cell-stop ((chmod-ref-start))
    (is-carrying (text (chmod-call "umask 027"))
      ("umask 027")
      ("removes   write for group; read, write and execute for others")
      ("files     rw-r-----  (640)  new files (programs ask for 666)")
      ("dirs      rwxr-x---  (750)  new directories (mkdir asks for 777)"))))

(deftest chmod-ref-cell-a-value-that-is-not-a-mode-says-why ()
  (with-cell-stop ((chmod-ref-start))
    (is-carrying (text (chmod-call "788"))
      ("ERROR:" "a failed call, so the transcript draws it failed")
      ("8 is not an octal digit"))
    (is-carrying (text (chmod-call "-rwwr-xr-x")) ("looks like ls -l text but a letter is in the wrong place"))
    (is-carrying (text (chmod-call "u+zs")) ("is not a mode"))
    ;; a required member the call does not carry is refused before the body runs
    (is (signals-error nlk:contract-refusal
          (nlk:contract-decode (nlk:contract 'nodecode-chmod-ref::chmod-value)
                                (nlk:json-object))))))

(deftest chmod-ref-cell-the-command-and-the-one-line-answer ()
  (with-cell-stop ((chmod-ref-start))
    (is-carrying (text (nodecode-chmod-ref::run-chmod "644 u+x,go-w" "s1"))
      ("Mode 644 with u+x,go-w"))
    (is-carrying (text (nodecode-chmod-ref::run-chmod "" "s1"))
      ("chmod-ref: explain a file mode" "typed alone, the examples")
      ("755 = rwxr-xr-x"))
    (is-carrying (text (nodecode-chmod-ref::run-chmod "9x" "s1"))
      ("ERROR:" "a wrong value answers why and the examples behind it"))
    (let ((mode (nodecode-chmod-ref:summarise "4755"))
          (change (nodecode-chmod-ref:summarise "644 u+x"))
          (mask (nodecode-chmod-ref:summarise "umask 027")))
      (is (equal "4755 = rwsr-xr-x" mode) "one line for a mode")
      (is (equal "644 u+x -> 744 (rwxr--r--)" change) "one line for a change")
      (is (equal "umask 027: new files 640, new directories 750" mask) "one line for a umask"))))
