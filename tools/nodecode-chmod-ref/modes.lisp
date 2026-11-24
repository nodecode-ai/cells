;;;; modes.lisp --- the mode bits, the chmod change and the umask, in words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here imports Nodecode: the bits are worked out by hand, the
;;;; words are plain strings, and the one condition is MODE-ERROR. A cell
;;;; that wants them as a tool or a command (CELL.LISP) is the only caller
;;;; that knows about the organism.
;;;;
;;;; Symbolic changes (u+x,go-w) follow GNU chmod with a umask of 0, the
;;;; way the original does: u, g, o and a, the +, - and = operators, r, w,
;;;; x, X, s and t, and copies like u=g.

(in-package #:nodecode-chmod-ref)

;;; --- the bits ---------------------------------------------------------------

(defconstant +setuid+ #o4000)
(defconstant +setgid+ #o2000)
(defconstant +sticky+ #o1000)
(defconstant +all-bits+ #o7777)
(defconstant +x-all+ #o111)
(defconstant +gitlink+ #o16)

(defparameter +classes+
  #(#("owner" 6 #o4000 #\s) #("group" 3 #o2000 #\s) #("others" 0 #o1000 #\t))
  "The three classes, in ls -l's order: name, the shift of its rwx bits, its
special bit and the letter ls -l shows for it.")

(defparameter +types+
  '((#o14 #\s "socket")
    (#o12 #\l "symbolic link")
    (#o10 #\- "regular file")
    (#o06 #\b "block device")
    (#o04 #\d "directory")
    (#o02 #\c "character device")
    (#o01 #\p "named pipe (FIFO)"))
  "File types as the top bits of st_mode: the st_mode code, ls -l's letter
and the name in words.")

(defparameter +who-bits+
  '((#\u . #o4700) (#\g . #o2070) (#\o . #o1007) (#\a . #o7777))
  "What a who letter of a chmod clause reaches: its rwx bits and its special bit.")

(defparameter +perm-bits+
  '((#\r . #o444) (#\w . #o222) (#\x . #o111) (#\X . 0)
    (#\s . #o6000) (#\t . #o1000))
  "What a permission letter of a chmod clause gives.")

(defparameter +copy-bits+ '((#\u . #o700) (#\g . #o070) (#\o . #o007)))

(defparameter +suffix-notes+
  '((#\+ . "the + after the bits means an access control list (ACL) grants more than these bits show; getfacl lists it")
    (#\. . "the . after the bits means the file has an SELinux security context; ls -Z shows it")
    (#\@ . "the @ after the bits means the file has extended attributes (macOS); xattr -l lists them"))
  "What the +, . and @ after ls -l's bits mean, in words.")

(defparameter *max-target-chars* 200
  "The longest value read: a mode, or a mode and a change.")

;;; --- the condition ----------------------------------------------------------

(define-condition mode-error (error)
  ((message :initarg :message :reader mode-error-message))
  (:report (lambda (condition stream)
             (write-string (mode-error-message condition) stream)))
  (:documentation "The value read was not a mode, a chmod change or a umask."))

(defun mode-fail (control &rest arguments)
  (error 'mode-error :message (apply #'format nil control arguments)))

;;; --- a mode ----------------------------------------------------------------

(defstruct (mode (:constructor make-mode (bits &key (ftype nil) (suffix "") (written "") (gitlink nil))))
  (bits 0 :type fixnum)
  ftype
  (suffix "")
  (written "")
  (gitlink nil))

(defun mode-is-dir (mode)
  (eql (mode-ftype mode) #\d))

(defun octal-string (bits)
  "BITS as chmod writes them: three digits, or four when a special bit is set."
  (if (> bits #o777)
      (format nil "~4,'0o" bits)
      (format nil "~3,'0o" bits)))

(defun symbolic (bits)
  "The nine letters ls -l shows for BITS: rwxr-xr-x, with s, S, t and T."
  (with-output-to-string (out)
    (loop for class across +classes+
          for shift = (aref class 1)
          for special = (aref class 2)
          for letter = (aref class 3)
          do (write-char (if (logbitp (+ shift 2) bits) #\r #\-) out)
             (write-char (if (logbitp (+ shift 1) bits) #\w #\-) out)
             (write-char (if (logtest special bits)
                             (if (logbitp shift bits) letter (char-upcase letter))
                             (if (logbitp shift bits) #\x #\-))
                         out))))

(defun state-string (bits)
  "rwxr-xr-x  (755)"
  (format nil "~a  (~a)" (symbolic bits) (octal-string bits)))

(defun digits-only-p (text)
  (and (plusp (length text))
       (every (lambda (c) (and (<= (char-code #\0) (char-code c)) (<= (char-code c) (char-code #\9)))) text)))

;;; --- reading a mode ---------------------------------------------------------

(defun parse-octal (text)
  "TEXT as octal digits with an optional 0o prefix, or NIL: 755, 0755, 0o644."
  (let ((start 0))
    (when (and (>= (length text) 3)
               (char= (char text 0) #\0)
               (char-equal (char text 1) #\o))
      (setf start 2))
    (let ((count (- (length text) start)))
      (when (and (<= 1 count 6)
                 (loop for i from start below (length text)
                       always (digit-char-p (char text i) 8)))
        (parse-integer text :start start :radix 8)))))

(defun read-ls-mode (text)
  "TEXT as ls -l's bits, with or without the type letter, or NIL."
  (let* ((n (length text))
         (start (if (and (plusp n) (find (char text 0) "-dlcbps")) 1 0))
         (ftype (and (= start 1) (char text 0))))
    (when (< (- n start) 9)
      (return-from read-ls-mode nil))
    (let ((letters (subseq text start (+ start 9))))
      (unless (loop for k from 0 below 9
                    for c = (char letters k)
                    always (case (mod k 3)
                             (0 (find c "r-"))
                             (1 (find c "w-"))
                             (2 (if (= k 8) (find c "xsStT-") (find c "xsS-")))))
        (return-from read-ls-mode nil))
      (let ((at (+ start 9))
            (suffix ""))
        (when (and (< at n) (find (char text at) "+.@"))
          (setf suffix (string (char text at)))
          (incf at))
        (unless (= at n)
          (return-from read-ls-mode nil))
        (make-mode (ls-bits letters) :ftype ftype :suffix suffix :written text)))))

(defun ls-bits (letters)
  "The bits ls -l's nine LETTERS stand for."
  (let ((bits 0))
    (loop for class across +classes+
          for k from 0
          for r = (char letters (* 3 k))
          for w = (char letters (1+ (* 3 k)))
          for x = (char letters (+ 2 (* 3 k)))
          for shift = (aref class 1)
          do (setf bits (logior bits (ash (logior (if (char= r #\r) 4 0)
                                                  (if (char= w #\w) 2 0)
                                                  (if (find x "xst") 1 0))
                                         shift)))
             (when (find x "sStT")
               (setf bits (logior bits (aref class 2)))))
    bits))

(defun read-mode (text)
  "TEXT as a mode: octal (755, 4755, 0o644, 100644) or ls -l bits
(rwxr-xr-x, -rw-r--r--@). => a MODE, or NIL when TEXT is neither; a
five-digit value whose type is not a file type signals MODE-ERROR."
  (let ((value (parse-octal text)))
    (if (null value)
        (read-ls-mode text)
        (let ((kind (ash value -12)))
          (cond ((zerop kind) (make-mode value :written text))
                ((= kind +gitlink+) (make-mode value :written text :gitlink t))
                (t (let ((entry (assoc kind +types+)))
                     (unless entry
                       (mode-fail "~s: ~o is not a file type (st_mode types are 01, 02, 04, 06, 10, 12 and 14, as in 100644 for a file or 040755 for a directory)"
                                  text kind))
                     (make-mode value :ftype (second entry) :written text))))))))

;;; --- reading a chmod change -------------------------------------------------

(defstruct (change (:constructor %make-change))
  clause who op perms index affected copy if-any-x value mentioned)

(defun make-change-entry (clause who op perms index)
  (let ((entry (%make-change :clause clause :who who :op op :perms perms :index index))
        (affected 0)
        (copy (member perms '("u" "g" "o") :test #'string=))
        (value 0))
    (loop for letter across who
          do (setf affected (logior affected (cdr (assoc letter +who-bits+)))))
    (if copy
        (setf value (cdr (assoc (char perms 0) +copy-bits+)))
        (loop for letter across perms
              do (setf value (logior value (cdr (assoc letter +perm-bits+))))))
    (setf (change-affected entry) affected
          (change-copy entry) (and copy t)
          (change-if-any-x entry) (and (find #\X perms) t)
          (change-value entry) value
          (change-mentioned entry) value)
    entry))

(defun split-commas (text)
  (let ((out '())
        (start 0))
    (loop for i = (position #\, text :start start)
          do (push (subseq text start i) out)
             (if i (setf start (1+ i)) (return)))
    (nreverse out)))

(defun parse-actions (text i n)
  "(VALUES ACTIONS OK): TEXT[i..N) as (OP . PERMS) runs, backtracking the
way the original's regular expression does between a copy's single u, g or o
and a run of rwxXst."
  (if (= i n)
      (values '() t)
      (if (not (find (char text i) "-+="))
          (values '() nil)
          (let ((op (char text i)))
            (multiple-value-bind (copied copies-p)
                (if (and (< (1+ i) n) (find (char text (1+ i)) "ugo"))
                    (multiple-value-bind (rest ok) (parse-actions text (+ i 2) n)
                      (if ok
                          (values (cons (cons op (string (char text (1+ i)))) rest) t)
                          (values '() nil)))
                    (values '() nil))
              (if copies-p
                  (values copied t)
                  (let* ((j (1+ i))
                         (k (loop while (and (< j n) (find (char text j) "rwxXst"))
                                  do (incf j)
                                  finally (return j))))
                    (multiple-value-bind (rest ok) (parse-actions text k n)
                      (if ok
                          (values (cons (cons op (subseq text (1+ i) k)) rest) t)
                          (values '() nil))))))))))

(defun parse-clause (clause)
  "CLAUSE as (WHO . ACTIONS), or NIL when it is not one comma-separated
chmod clause: an optional ugoa, then one or more OP PERMS."
  (let ((n (length clause))
        (i 0))
    (loop while (and (< i n) (find (char clause i) "ugoa"))
          do (incf i))
    (when (< i n)
      (multiple-value-bind (actions ok) (parse-actions clause i n)
        (when ok (cons (subseq clause 0 i) actions))))))

(defun read-changes (text)
  "TEXT as u+x,go-w's changes, or NIL when it is not a symbolic chmod mode."
  (let ((entries '())
        (ok t))
    (loop for clause in (split-commas text)
          for index from 0
          do (let ((parsed (parse-clause clause)))
               (if (null parsed)
                   (setf ok nil)
                   (destructuring-bind (who . actions) parsed
                     (dolist (action actions)
                       (push (make-change-entry clause who (car action) (cdr action) index)
                             entries))))))
    (when ok
      (nreverse entries))))

;;; --- applying a change ------------------------------------------------------

(defun apply-changes (bits changes &key (is-dir nil))
  "BITS after CHANGES, the way GNU chmod applies them with a umask of 0."
  (let ((new bits))
    (dolist (change changes new)
      (let* ((omit (if is-dir
                       (logand (lognot (change-mentioned change)) (logior +setuid+ +setgid+))
                       0))
             (value (change-value change)))
        (if (change-copy change)
            (progn
              (setf value (logand value new))
              (setf value (logior value
                                  (if (logtest #o444 value) #o444 0)
                                  (if (logtest #o222 value) #o222 0)
                                  (if (logtest #o111 value) #o111 0))))
            (when (and (change-if-any-x change)
                       (or (plusp (logand new +x-all+)) is-dir))
              (setf value (logior value +x-all+))))
        (setf value (logand value
                             (logand (if (zerop (change-affected change))
                                         +all-bits+
                                         (change-affected change))
                                     (lognot omit))))
        (case (change-op change)
          (#\= (let ((preserved (logior (if (zerop (change-affected change))
                                            0
                                            (logand (lognot (change-affected change)) +all-bits+))
                                        omit)))
                 (setf new (logior (logand new preserved) value))))
          (#\+ (setf new (logior new value)))
          (#\- (setf new (logand new (lognot value)))))))
    (logand new +all-bits+)))

;;; --- words ------------------------------------------------------------------

(defun collapse-spaces (text)
  "TEXT with every run of whitespace one space and its ends trimmed."
  (with-output-to-string (out)
    (let ((space nil))
      (loop for c across text
            do (if (member c '(#\space #\tab #\newline #\return) :test #'char=)
                   (setf space t)
                   (progn (when space (write-char #\space out))
                          (setf space nil)
                          (write-char c out)))))))

(defun join-words (words)
  (cond ((null words) "")
        ((null (rest words)) (first words))
        (t (format nil "~{~a~^, ~} and ~a" (butlast words) (car (last words))))))

(defun who-words (who)
  (if (or (zerop (length who))
          (find #\a who)
          (and (find #\u who) (find #\g who) (find #\o who)))
      "everyone"
      (join-words (loop for letter across "ugo"
                         when (find letter who)
                           collect (if (char= letter #\u) "the owner"
                                       (if (char= letter #\g) "the group" "others"))))))

(defun who-set (who)
  (if (zerop (length who)) "a" who))

(defun describe-change (change)
  "One change in words: GO-W -> remove write from the group and others."
  (let ((target (who-words (change-who change))))
    (if (change-copy change)
        (let* ((source (cond ((string= (change-perms change) "u") "the owner")
                             ((string= (change-perms change) "g") "the group")
                             (t "others")))
               (has (if (string= (change-perms change) "o") "have" "has")))
          (case (change-op change)
            (#\= (format nil "set ~a to the rights ~a ~a now" target source has))
            (#\+ (format nil "give ~a the rights ~a ~a now" target source has))
            (t (format nil "take the rights ~a ~a now away from ~a" source has target))))
        (let* ((perms (change-perms change))
               (words (append (if (find #\r perms) '("read"))
                              (if (find #\w perms) '("write"))
                              (if (find #\x perms) '("execute"))
                              (if (find #\X perms)
                                  '("execute where some class can already execute (or on a directory)"))))
               (who (who-set (change-who change)))
               (specials (append (if (find #\s perms)
                                     (append (if (find #\u who) '("setuid"))
                                             (if (find #\g who) '("setgid"))
                                             (if (find #\a who) '("setuid" "setgid"))))
                                 (if (and (find #\t perms) (or (find #\o who) (find #\a who)))
                                     '("the sticky bit"))))
               (ignored (or (and (find #\s perms) (not (or (find #\u who) (find #\g who) (find #\a who))))
                            (and (find #\t perms) (not (or (find #\o who) (find #\a who))))))
               (out
                 (if (char= (change-op change) #\=)
                     (format nil "set ~a to exactly ~a~@[, with ~a~]"
                             target (if words (join-words words) "nothing") (and specials (join-words specials)))
                     (let ((parts '()))
                       (when words
                         (push (format nil "~a ~a ~a ~a"
                                       (if (char= (change-op change) #\+) "add" "remove")
                                       (join-words words)
                                       (if (char= (change-op change) #\+) "for" "from")
                                       target)
                               parts))
                       (when specials
                         (push (format nil "turn ~a ~a"
                                       (if (char= (change-op change) #\+) "on" "off")
                                       (join-words specials))
                               parts))
                       (if parts (format nil "~{~a~^; ~}" (nreverse parts)) "no change")))))
          (cond ((not ignored) out)
                ((string= out "no change")
                 (format nil "no change: ~a" (if (find #\s perms) "s needs u or g" "t needs o")))
                (t (format nil "~a (~a, so that letter is ignored)" out
                           (if (find #\s perms) "s needs u or g" "t needs o"))))))))

(defun rights (bits shift is-dir)
  "One class's rights in words: what its rwx bits let it do here."
  (let* ((part (logand (ash bits (- shift)) 7))
         (names (if is-dir
                    (list (cons 4 "list names") (cons 2 "add, rename and delete entries") (cons 1 "enter"))
                    (list (cons 4 "read") (cons 2 "write") (cons 1 "execute"))))
         (words (loop for (bit . word) in names when (logtest part bit) collect word)))
    (if words (format nil "~{~a~^, ~}" words) "nothing")))

;;; --- notes --------------------------------------------------------------------

(defun special-notes (mode)
  (let ((bits (mode-bits mode))
        (is-dir (mode-is-dir mode))
        (kind (mode-ftype mode))
        (notes '()))
    (when (logtest +setuid+ bits)
      (if is-dir
          (push "setuid on a directory is ignored by Linux and macOS" notes)
          (progn
            (push "setuid: the program runs with its owner's rights, not those of the user who starts it (root-owned setuid programs, like passwd, run as root)"
                  notes)
            (unless (logtest #o100 bits)
              (push "setuid is set but the owner has no execute (ls shows S), so it does nothing" notes)))))
    (when (logtest +setgid+ bits)
      (push (cond (is-dir
                   "setgid on a directory: new files and directories inside take its group (new directories also get setgid)")
                  ((null kind)
                   "setgid: a program runs with the file's group; on a directory, new entries inside take its group")
                  (t "setgid: the program runs with the file's group"))
            notes)
      (when (and (not (logtest #o010 bits)) (not is-dir))
        (push "setgid is set but the group has no execute (ls shows S), so it does not apply when the program runs"
              notes)))
    (when (logtest +sticky+ bits)
      (push (cond ((or is-dir (null kind))
                   (concatenate 'string
                                "sticky on a directory: only a file's owner, the directory's owner or root can delete or rename files in it, as on /tmp (1777)"
                                (if is-dir "" "; on files Linux and macOS ignore it")))
                  (t "sticky on a file is ignored by Linux and macOS"))
            notes)
      (unless (logtest #o001 bits)
        (push "ls shows T because others have no execute; the sticky bit still applies to a directory"
              notes)))
    (nreverse notes)))

(defun later-rights (bits index)
  "The rights a later class has that an earlier one lacks: the kernel uses
only the first class of the user it matches."
  (let* ((low (aref (aref +classes+ index) 1))
         (own (logand (ash bits (- low)) 7))
         (extra 0))
    (loop for k from (1+ index) below 3
          do (setf extra (logior extra (logand (ash bits (- (aref (aref +classes+ k) 1))) 7))))
    (logand extra (logand (lognot own) 7))))

(defun notes (mode)
  "What is worth saying about MODE, beyond its rows."
  (let ((bits (mode-bits mode))
        (is-dir (mode-is-dir mode))
        (perm (logand (mode-bits mode) #o777))
        (out '()))
    (when (mode-gitlink mode)
      (return-from notes
        (list "160000 is how git records a submodule (a commit of another repository), not a file mode")))
    (when (eql (mode-ftype mode) #\l)
      (push "a symbolic link's own mode is not checked on Linux (it is always rwxrwxrwx there); access depends on its target"
            out))
    (cond ((zerop perm)
           (push "no one but root can read, change or run it" out))
          ((= perm #o777)
           (push (concatenate 'string
                              "everyone can read, change and "
                              (if is-dir "enter it" "run it"))
                 out))
          ((logtest #o002 bits)
           (if is-dir
               (push (concatenate 'string
                                  "others can write: anyone on the machine can create, rename and delete entries in it"
                                  (if (logtest +sticky+ bits)
                                      ""
                                      " (the sticky bit, as on /tmp, would stop them deleting others' files)"))
                     out)
               (push "others can write: anyone on the machine can change it" out))))
    (when (and (zerop (logand perm #o077)) (logtest #o400 perm))
      (push (if is-dir
                "only the owner (and root) can use it"
                "only the owner (and root) can use it; ssh wants this (600 or 400) for private keys")
            out))
    (dotimes (index 2)
      (let* ((extra (later-rights bits index))
             (low (aref (aref +classes+ index) 0))
             (who (format nil "the ~a" low)))
        (when (plusp extra)
          (let ((words (loop for (bit . word) in (list (cons 4 "read") (cons 2 "write") (cons 1 "execute"))
                             when (logtest extra bit) collect word))
                (holders (loop for k from (1+ index) below 3
                               for name = (aref (aref +classes+ k) 0)
                               when (logtest (logand (ash bits (- (aref (aref +classes+ k) 1))) 7) extra)
                                 collect (if (string= name "group") "the group" name))))
            (push (format nil "~a ~a ~a, which ~a lacks: only the first class that matches a user counts, so ~a cannot use those rights"
                          (join-words holders)
                          (if (equal holders '("the group")) "has" "have")
                          (join-words words) who who)
                  out)))))
    (when (and (not is-dir) (member (mode-ftype mode) '(nil #\-) :test #'eql))
      (loop for class across +classes+
            for shift = (aref class 1)
            for part = (logand (ash bits (- shift)) 7)
            when (and (logtest 1 part) (not (logtest 4 part)))
              do (push (format nil "~a can execute but not read: a compiled program runs, a script does not (its interpreter has to read it)"
                               (aref class 0))
                       out)
                 (return))
      (when (and (logtest #o400 bits) (not (logtest #o200 bits)))
        (push "read-only for the owner too: editors warn before saving; whether it can be deleted depends on the directory, not on the file"
              out)))
    (when is-dir
      (loop for class across +classes+
            for shift = (aref class 1)
            for part = (logand (ash bits (- shift)) 7)
            unless (logtest 1 part)
              do (cond ((logtest 2 part)
                        (push (format nil "~a: write without execute on a directory does nothing; creating, renaming and deleting entries needs execute too"
                                      (aref class 0))
                              out)
                        (return))
                       ((logtest 4 part)
                        (push (format nil "~a can list the names but not enter the directory or open anything in it"
                                      (aref class 0))
                              out)
                        (return)))))
    (when (and (eql (mode-ftype mode) #\-)
               (digits-only-p (mode-written mode))
               (>= (length (mode-written mode)) 5)
               (not (member perm '(#o644 #o755))))
      (push "git records only 100644 or 100755 for a file; other bits are not kept" out))
    (setf out (append (nreverse out) (special-notes mode)))
    (when (plusp (length (mode-suffix mode)))
      (push (cdr (assoc (char (mode-suffix mode) 0) +suffix-notes+ :test #'char=))
            out))
    out))

;;; --- rendering ---------------------------------------------------------------

(defun type-name (mode)
  (cond ((mode-gitlink mode) "git submodule")
        ((null (mode-ftype mode)) nil)
        (t (third (find (mode-ftype mode) +types+ :key #'second :test #'eql)))))

(defun ls-string (mode)
  (format nil "~a~a~a" (or (mode-ftype mode) #\-) (symbolic (mode-bits mode)) (mode-suffix mode)))

(defun mode-rows (mode)
  (let ((bits (mode-bits mode))
        (rows (list (format nil "  ~9a ~a" "octal" (octal-string (mode-bits mode))))))
    (when (and (digits-only-p (mode-written mode)) (>= (length (mode-written mode)) 5))
      (setf rows (append rows
                         (list (format nil "  ~9a ~a (file type and permission bits, as stat and git write them)"
                                       "st_mode" (mode-written mode))))))
    (if (and (null (mode-ftype mode)) (not (mode-gitlink mode)))
        (setf rows (append rows
                           (list (format nil "  ~9a -~a for a file, d~a for a directory"
                                         "ls -l" (symbolic bits) (symbolic bits)))))
        (setf rows (append rows (list (format nil "  ~9a ~a" "ls -l" (ls-string mode))))))
    (when (mode-gitlink mode)
      (return-from mode-rows rows))
    (loop for class across +classes+
          do (setf rows (append rows
                                (list (format nil "  ~9a ~a" (aref class 0)
                                              (rights bits (aref class 1) (mode-is-dir mode)))))))
    (setf rows (append rows
                       (list (format nil "  ~9a ~a" "special"
                                     (let ((specials (remove nil
                                                             (list (if (logtest +setuid+ bits) "setuid")
                                                                   (if (logtest +setgid+ bits) "setgid")
                                                                   (if (logtest +sticky+ bits) "sticky")))))
                                       (if specials (format nil "~{~a~^, ~}" specials) "none"))))))
    rows))

(defun with-notes (lines extra)
  (if (null extra)
      lines
      (append lines
              (list "Notes")
              (loop for note in extra collect (format nil "  - ~a" note)))))

(defun describe-mode (mode)
  "MODE's rows and notes, one string a line."
  (let ((kind (type-name mode)))
    (with-notes (cons (format nil "Mode ~a (~a)~@[, a ~a~]"
                              (octal-string (mode-bits mode)) (symbolic (mode-bits mode)) kind)
                      (mode-rows mode))
                (notes mode))))

(defun diff-rows (before after)
  "What changes between two modes, one row a verb."
  (let ((rows '()))
    (loop for (verb . bits) in (list (cons "adds" (logand after (lognot before)))
                                      (cons "removes" (logand before (lognot after))))
          when (plusp (logand bits +all-bits+))
            do (let ((groups '())
                     (phrases '()))
                 (loop for class across +classes+
                       for shift = (aref class 1)
                       for words = (loop for (bit . word) in (list (cons 4 "read") (cons 2 "write") (cons 1 "execute"))
                                         when (logtest (logand (ash bits (- shift)) 7) bit) collect word)
                       when words
                         do (let ((key (join-words words))
                                  (place (find (join-words words) groups :key #'first :test #'string=)))
                              (if place
                                  (setf (rest place) (append (rest place) (list (aref class 0))))
                                  (setf groups (append groups (list (list key (aref class 0))))))))
                 (setf phrases (loop for (words . names) in groups
                                     collect (format nil "~a for ~a" words (join-words names))))
                 (loop for (word . bit) in (list (cons "setuid" +setuid+)
                                                 (cons "setgid" +setgid+)
                                                 (cons "sticky" +sticky+))
                       when (logtest bit bits)
                         do (setf phrases (append phrases (list word))))
                 (push (format nil "  ~9a ~{~a~^; ~}" verb phrases) rows)))
    (or (nreverse rows) (list "  no change"))))

(defun compare-modes (before after)
  (let ((lines (list (format nil "Mode ~a -> ~a" (octal-string (mode-bits before)) (octal-string (mode-bits after)))
                     (format nil "  ~9a ~a" "before" (state-string (mode-bits before)))
                     (format nil "  ~9a ~a" "after" (state-string (mode-bits after)))))
        (extra '()))
    (setf lines (append lines (diff-rows (mode-bits before) (mode-bits after))))
    (when (and (not (eql (mode-ftype before) (mode-ftype after)))
               (mode-ftype before) (mode-ftype after))
      (push (format nil "the file type changes too: ~a -> ~a" (type-name before) (type-name after))
            extra))
    (when (and (eql (mode-ftype before) #\-) (eql (mode-ftype after) #\-)
               (let ((a (logand (mode-bits before) #o777)) (b (logand (mode-bits after) #o777)))
                 (or (and (= a #o644) (= b #o755))
                     (and (= a #o755) (= b #o644)))))
      (push "this is the change git shows as old mode / new mode when a file's execute bit flips"
            extra))
    (append (with-notes lines (nreverse extra)) (list "") (describe-mode after))))

(defun clause-groups (changes)
  "CHANGES grouped back into the comma-separated clauses they came from."
  (let ((out '()))
    (dolist (change changes)
      (let ((group (first out)))
        (if (and group (= (change-index (first (second group))) (change-index change)))
            (setf (rest group) (append (rest group) (list change)))
            (push (list (change-clause change) (list change)) out))))
    (nreverse out)))

(defun change-notes (changes)
  (let ((out '()))
    (when (find-if (lambda (change) (zerop (length (change-who change)))) changes)
      (push (concatenate 'string
                         "with no u, g, o or a, chmod applies the change to everyone but leaves out bits your umask blocks; "
                         "this assumes a umask of 0 (umask 022 keeps +w to the owner)")
            out))
    (when (find-if #'change-if-any-x changes)
      (push "X adds execute only to directories and to files some class can already execute" out))
    (nreverse out)))

(defun clause-width (changes)
  (let ((width 9))
    (dolist (change changes)
      (setf width (max width (length (change-clause change)))))
    width))

(defun apply-text (base changes text)
  (let* ((width (clause-width changes))
         (lines (list (format nil "Mode ~a with ~a" (octal-string (mode-bits base)) text)
                      (format nil "  ~9a ~a" "before" (state-string (mode-bits base)))))
         (bits (mode-bits base)))
    (loop for (clause group) in (clause-groups changes)
          do (setf bits (apply-changes bits group :is-dir (mode-is-dir base)))
             (setf lines (append lines
                                 (list (format nil "  ~va ~a  ~{~a~^; ~}" width clause (state-string bits)
                                               (loop for change in group collect (describe-change change)))))))
    (setf lines (append lines
                        (list (format nil "  ~va ~a" width "after" (state-string bits)))))
    (setf lines (append lines (diff-rows (mode-bits base) bits)))
    (setf lines (with-notes lines (change-notes changes)))
    (append lines (list "") (describe-mode (make-mode bits :ftype (mode-ftype base))))))

(defun changes-text (changes text)
  (let* ((width (clause-width changes))
         (lines (list (format nil "chmod ~a" text))))
    (loop for (clause group) in (clause-groups changes)
          do (setf lines (append lines
                                 (list (format nil "  ~va ~{~a~^; ~}" width clause
                                               (loop for change in group collect (describe-change change)))))))
    (dolist (base (list #o644 #o755))
      (setf lines (append lines
                           (list (format nil "  ~va ~a" width (format nil "on ~o" base)
                                         (state-string (apply-changes base changes)))))))
    (let ((extra (append (change-notes changes)
                         (list (format nil "give a starting mode to see one result: 644 ~a" text)))))
      (with-notes lines extra))))

(defun umask-text (mask)
  (let* ((files (logand #o666 (lognot mask)))
         (dirs (logand #o777 (lognot mask)))
         (lines (list (format nil "umask ~3,'0o" mask)))
         (blocked '()))
    (loop for class across +classes+
          for shift = (aref class 1)
          for words = (loop for (bit . word) in (list (cons 4 "read") (cons 2 "write") (cons 1 "execute"))
                            when (logtest (logand (ash mask (- shift)) 7) bit) collect word)
          when words
            do (let ((key (join-words words))
                     (place (find (join-words words) blocked :key #'first :test #'string=)))
                 (if place
                     (setf (rest place) (append (rest place) (list (aref class 0))))
                     (setf blocked (append blocked (list (list key (aref class 0))))))))
    (let ((phrase (if blocked
                      (format nil "~{~a~^; ~}"
                              (loop for (words . names) in blocked
                                    collect (format nil "~a for ~a" words (join-words names))))
                      "nothing")))
      (setf lines (append lines
                          (list (format nil "  ~9a ~a" "removes" phrase)
                                (format nil "  ~9a ~a  new files (programs ask for 666)" "files" (state-string files))
                                (format nil "  ~9a ~a  new directories (mkdir asks for 777)" "dirs" (state-string dirs))))))
    (let ((extra '()))
      (when (zerop (logand mask #o002))
        (push "new files and directories are writable by everyone on the machine" extra))
      (when (= (logand mask #o077) #o077)
        (push "new files and directories are private to their owner" extra))
      (when (and (plusp (logand mask +x-all+)) (zerop (logand mask #o666)))
        (push (concatenate 'string
                           "the execute bits only matter for directories and for files made with execute asked for "
                           "(a compiler's output, for example)")
              extra))
      (when (plusp (logand mask #o700))
        (push "the umask also blocks the owner's own rights, which is unusual" extra))
      (push (concatenate 'string
                         "a umask only takes rights away: programs that ask for less (600 for a key, for example) "
                         "get less")
            extra)
      (with-notes lines (nreverse extra)))))

(defun unquote (text)
  (let ((text (string-trim '(#\space #\tab #\newline #\return) text)))
    (when (and (>= (length text) 2)
               (char= (char text 0) (char text (1- (length text))))
               (find (char text 0) "\"'`"))
      (setf text (string-trim '(#\space #\tab #\newline #\return) (subseq text 1 (1- (length text))))))
    text))

(defun loose-ls-p (text)
  "TEXT that looks like ls -l's bits with a letter in the wrong place."
  (let* ((n (length text))
         (start (if (and (plusp n) (find (char text 0) "-dlcbps")) 1 0)))
    (and (<= (+ start 9) n)
         (loop for i from start below (+ start 9) always (find (char text i) "rwxsStT-"))
         (<= (- n (+ start 9)) 3)
         (loop for i from (+ start 9) below n always (find (char text i) "-rwxsStT+.@")))))

(defun parse-mode (text)
  (let ((mode (read-mode text)))
    (unless mode (mode-fail "~a" (mode-error-text text)))
    mode))

(defun mode-error-text (text)
  (cond ((digits-only-p text)
         (let ((bad (remove-duplicates
                     (loop for c across text when (member c '(#\8 #\9)) collect (string c))
                     :test #'string=)))
           (if bad
               (format nil "~s is not octal: ~{~a~^, ~} is not an octal digit (0-7)" text bad)
               (format nil "~s has ~d digits; a mode has 3 or 4 (755, 4755), or 6 with the file type (100644)"
                       text (length text)))))
        ((loose-ls-p text)
         (format nil "~s looks like ls -l text but a letter is in the wrong place: each class is r or -, then w or -, then x, s or S (t or T for others) or -"
                 text))
        (t (format nil "~s is not a mode: try 755, 0644, rwxr-xr-x, -rw-r--r--, u+x,go-w, \"644 u+x\" or \"umask 022\""
                   text))))

(defun umask-match (text)
  "MASK as TEXT's `umask 022' says it, or NIL: the digits come back."
  (when (and (>= (length text) 5) (string-equal "umask" (subseq text 0 5)))
    (let ((start 5))
      (let ((i (loop while (and (< start (length text)) (find (char text start) "=: "))
                      do (incf start)
                      finally (return start))))
        (let ((digits (subseq text i)))
          (when (and (<= 1 (length digits) 4)
                     (every (lambda (c) (digit-char-p c 8)) digits))
            digits))))))

(defun expand (target)
  "TARGET, the value a user writes, as the explanation text."
  (let* ((text (unquote target)))
    (when (zerop (length text))
      (mode-fail "no mode given; try 755, -rw-r--r-- or \"644 u+x\""))
    (when (> (length text) *max-target-chars*)
      (mode-fail "the value is ~d characters; up to ~d are read" (length text) *max-target-chars*))
    (let ((mask (umask-match text)))
      (when mask
        (when (and (= (length mask) 4) (char/= (char mask 0) #\0))
          (mode-fail "umask ~a: a umask covers only the 9 permission bits, so a 4-digit one starts with 0" mask))
        (return-from expand (format nil "~{~a~^~%~}" (umask-text (parse-integer mask :radix 8))))))
    (let ((tokens (split-spaces text)))
      (when (= (length tokens) 1)
        (let ((mode (read-mode text)))
          (when mode
            (return-from expand (format nil "~{~a~^~%~}" (describe-mode mode)))))
        (let ((changes (unless (loose-ls-p text) (read-changes text))))
          (when changes
            (return-from expand (format nil "~{~a~^~%~}" (changes-text changes text))))
          (mode-fail "~a" (mode-error-text text))))
      (let ((base (parse-mode (first tokens))))
        (when (= (length tokens) 2)
          (let ((other (read-mode (second tokens))))
            (when other
              (return-from expand (format nil "~{~a~^~%~}" (compare-modes base other))))))
        (let* ((rest (format nil "~{~a~^,~}" (rest tokens)))
               (changes (read-changes rest)))
          (unless changes
            (mode-fail "~s is not a chmod change: write it like u+x, go-w, a=r, g+s or u=g (several joined with commas)"
                       (format nil "~{~a~^ ~}" (rest tokens))))
          (when (mode-gitlink base)
            (mode-fail "160000 is a git submodule entry; it has no permission bits to change"))
          (format nil "~{~a~^~%~}" (apply-text base changes rest)))))))

(defun split-spaces (text)
  (let ((out '())
        (start 0)
        (n (length text)))
    (loop
      (loop while (and (< start n) (member (char text start) '(#\space #\tab #\newline #\return)))
            do (incf start))
      (when (= start n) (return (nreverse out)))
      (let ((end (or (position-if (lambda (c) (member c '(#\space #\tab #\newline #\return))) text
                                  :start start)
                     n)))
        (push (subseq text start end) out)
        (setf start end)))))

(defun one-line-text (target)
  "TARGET as one line: 755 = rwxr-xr-x, 644 u+x -> 744 (rwxr--r--)."
  (let ((text (unquote target)))
    (let ((mask (umask-match text)))
      (when mask
        (let ((value (parse-integer mask :radix 8)))
          (return-from one-line-text
            (format nil "umask ~3,'0o: new files ~3,'0o, new directories ~3,'0o"
                    value (logand #o666 (lognot value)) (logand #o777 (lognot value)))))))
    (let ((tokens (split-spaces text)))
      (when (= (length tokens) 1)
        (let ((mode (read-mode text)))
          (when mode
            (let ((kind (type-name mode)))
              (return-from one-line-text
                (concatenate 'string
                             (format nil "~a = ~a" (octal-string (mode-bits mode))
                                     (if (mode-ftype mode) (ls-string mode) (symbolic (mode-bits mode))))
                             (if kind (format nil ", ~a" kind) ""))))))
        (let ((changes (unless (loose-ls-p text) (read-changes text))))
          (if changes
              (return-from one-line-text
                (format nil "~{~a~^; ~}" (loop for change in changes collect (describe-change change))))
              (mode-fail "~a" (mode-error-text text)))))
      (let ((base (parse-mode (first tokens))))
        (when (= (length tokens) 2)
          (let ((other (read-mode (second tokens))))
            (when other
              (return-from one-line-text
                (format nil "~a -> ~a: ~{~a~^; ~}"
                        (octal-string (mode-bits base)) (octal-string (mode-bits other))
                        (loop for row in (diff-rows (mode-bits base) (mode-bits other))
                              collect (collapse-spaces row)))))))
        (let ((changes (read-changes (format nil "~{~a~^,~}" (rest tokens)))))
          (when (or (null changes) (mode-gitlink base))
            (mode-fail "not a chmod change"))
          (let ((after (apply-changes (mode-bits base) changes :is-dir (mode-is-dir base))))
            (format nil "~a ~a -> ~a (~a)"
                    (octal-string (mode-bits base))
                    (format nil "~{~a~^,~}" (rest tokens))
                    (octal-string after) (symbolic after))))))))

(defparameter +examples+ (list "755" "644 u+x" "umask 022")
  "What the original offered before anything was typed.")

(defun preview-rows (query)
  "Suggestions for QUERY: a list of (INSERTED SHOWN META), as the original's
autocomplete answered them."
  (let ((text (unquote (or query ""))))
    (if (zerop (length text))
        (loop for example in +examples+
              collect (list (if (find #\space example) (format nil "\"~a\"" example) example)
                            (one-line-text example)
                            "example"))
        (handler-case
            (let ((shown (one-line-text text)))
              (list (list (if (find #\space text) (format nil "\"~a\"" text) text) shown "file mode")))
          (mode-error () '())))))
