;;;; bracket.lisp --- the tournament state machine behind the arena tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A run is one JSON file, arena.json, in the run folder, so the
;;;; orchestrator never loses track of who is alive, who fought whom and what
;;;; happens next, even if its own context is compacted halfway through a
;;;; hundred-agent run. Every verb reads that file, changes it and writes it
;;;; back; nothing of the tournament lives in the conversation.
;;;;
;;;; The pieces, in the order they are used: a seeded generator (a run is
;;;; reproducible from its seed), the strategy cards, the bracket (pair,
;;;; open, record, advance), the judges' arithmetic, the sub-agent briefs
;;;; rendered from the templates in skill/SKILL.md, and the verbs of run in
;;;; cell.lisp.

(in-package #:nodecode-arena)

;;; --- reading the state -----------------------------------------------------

(defun jget (object name)
  "OBJECT's member NAME, JSON null read as NIL."
  (let ((value (and (hash-table-p object) (gethash name object))))
    (unless (eq value :null) value)))

(defun jtrue (object name)
  "Whether OBJECT's NAME reads true; JSON false and null both read NIL."
  (eq (jget object name) t))

(defun agents-of (state) (jget state "agents"))
(defun agent-of (state id) (gethash id (agents-of state)))
(defun rounds-of (state) (nlk:json-array state "rounds"))
(defun matches-of (round) (nlk:json-array round "matches"))
(defun agent-ids-of (state) (loop for id being the hash-keys of (agents-of state) collect id))
(defun card-part (agent part) (jget (jget agent "card") part))

;;; --- paths and files -------------------------------------------------------

(defun sub (dir name)
  "NAME inside DIR, both as namestrings."
  (namestring (merge-pathnames name (uiop:ensure-directory-pathname dir))))

(defun ensure-dir (dir)
  (ensure-directories-exist (uiop:ensure-directory-pathname dir)))

(defun read-text (path)
  "PATH's text, or NIL when it is not there."
  (handler-case (uiop:read-file-string path :external-format :utf-8)
    (error () nil)))

(defun write-text (path text)
  "TEXT into PATH, its folders made first."
  (ensure-dir (uiop:pathname-directory-pathname path))
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (write-string text out))
  path)

(defun has-output-p (path)
  "Whether PATH holds a file with something in it."
  (let ((probe (probe-file path)))
    (and probe
         (with-open-file (in probe :external-format :utf-8)
           (plusp (file-length in))))))

(defun now-stamp ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

;;; --- a seeded generator ----------------------------------------------------
;;; A run reproduces from its seed alone: the deal, the pairings, which side a
;;; judge reads first and which of the two blind copies is the champion's are
;;; all drawn from one string-seeded generator, on every platform alike, so no
;;; caller depends on an implementation's own RANDOM-STATE.

(defstruct (rng (:constructor %make-rng (state)))
  (state 1 :type (unsigned-byte 32)))

(defun fnv (text)
  (loop with hash = 2166136261
        for char across text
        do (setf hash (ldb (byte 32 0) (* (logxor hash (char-code char)) 16777619)))
        finally (return hash)))

(defun rng (seed)
  (let ((state (fnv (format nil "~a" seed))))
    (when (zerop state) (setf state 2463534242))
    (%make-rng state)))

(defun rng-next (generator)
  (let ((x (rng-state generator)))
    (setf x (ldb (byte 32 0) (logxor x (ash x 13))))
    (setf x (ldb (byte 32 0) (logxor x (ash x -17))))
    (setf x (ldb (byte 32 0) (logxor x (ash x 5))))
    (when (zerop x) (setf x 2463534242))
    (setf (rng-state generator) x)
    x))

(defun rng-below (generator n)
  (mod (rng-next generator) n))

(defun shuffle-list (generator items)
  (let ((vector (coerce items 'vector)))
    (loop for i from (1- (length vector)) downto 1
          for j = (rng-below generator (1+ i))
          do (rotatef (aref vector i) (aref vector j)))
    (coerce vector 'list)))

;;; --- the data beside this cell ---------------------------------------------

(defparameter *data-directory* nil
  "The skill/ folder beside this system: the prompt templates in SKILL.md,
the rubric and the strategy cards. Found once, from the system's own source
directory.")

(defun data-directory ()
  (or *data-directory*
      (setf *data-directory*
            (merge-pathnames
             "skill/"
             (or (ignore-errors (asdf:system-source-directory "nodecode-arena"))
                 (uiop:pathname-directory-pathname *load-truename*))))))

(defun data-file (name)
  (namestring (merge-pathnames name (data-directory))))

(defun strategies ()
  "skill/strategies.json decoded, checked for the three lists it must carry."
  (let* ((text (read-text (data-file "strategies.json")))
         (data (and text (handler-case (nlk:decode-json text :whole t) (error () nil)))))
    (unless (hash-table-p data)
      (fail "cannot read the strategy cards from skill/strategies.json"))
    (dolist (group '("reasoning" "workflows" "strategies"))
      (when (zerop (length (nlk:json-array data group)))
        (fail "strategies.json has no '~a' entries" group)))
    data))

(defun group-items (data group) (nlk:json-array data group))

(defun combo-count (data)
  (* (length (group-items data "reasoning"))
     (length (group-items data "workflows"))
     (length (group-items data "strategies"))))

(defun deal (n seed data)
  "N distinct (reasoning workflow strategy) cards, from a shuffle that the
seed fixes. Every card is different; the three lists come out as evenly used
as one shuffle over them makes them."
  (let ((total (combo-count data)))
    (unless (<= 1 n total)
      (fail "can deal between 1 and ~d cards, not ~d" total n))
    (let ((cards (loop for r across (group-items data "reasoning")
                       nconc (loop for w across (group-items data "workflows")
                                   nconc (loop for s across (group-items data "strategies")
                                               collect (list r w s))))))
      (subseq (shuffle-list (rng (format nil "arena-deal:~a" seed)) cards) 0 n))))

(defun card-json (card)
  (destructuring-bind (reasoning workflow strategy) card
    (nlk:json-object "reasoning" reasoning "workflow" workflow "strategy" strategy)))

;;; --- agent ids, sizes, the run's paths -------------------------------------

(defun agent-ids (n)
  (let ((width (max 3 (length (format nil "~d" n)))))
    (loop for i from 1 to n collect (format nil "a~v,'0d" width i))))

(defun bracket-sizes (n)
  "Alive at the start of each round, ending at 1: 16 -> 8 -> 4 -> 2 -> 1."
  (loop with sizes = (list n)
        while (> (car (last sizes)) 1)
        do (setf sizes (append sizes (list (ceiling (car (last sizes)) 2))))
        finally (return sizes)))

(defun spawn-out (dir aid) (sub dir (format nil "r0/~a.md" aid)))
(defun attack-out (dir round match who) (sub dir (format nil "r~d/~a.~a.attack.md" round match who)))
(defun defense-out (dir round match who) (sub dir (format nil "r~d/~a.~a.defense.md" round match who)))
(defun revised-out (dir round match who) (sub dir (format nil "r~d/~a.~a.solution.md" round match who)))
(defun verdict-out (dir round match) (sub dir (format nil "r~d/~a.verdict.json" round match)))
(defun final-out (dir) (sub dir "final.verdict.json"))

(defun prompt-path (dir round job-id)
  (sub dir (format nil "prompts/~a/~a.md" (if round (format nil "r~d" round) "final") job-id)))

;;; --- the bracket -----------------------------------------------------------

(defun new-state (n seed data dir &key (wave 10) has-baseline)
  (let ((agents (nlk:json-object)))
    (loop for aid in (agent-ids n)
          for card in (deal n seed data)
          do (setf (gethash aid agents)
                   (nlk:json-object "card" (card-json card)
                                    "solution" (spawn-out dir aid)
                                    "alive" t
                                    "eliminated_in" 0
                                    "eliminated_by" ""
                                    "byes" 0)))
    (let ((state (nlk:json-object "version" 1
                                  "created" (now-stamp)
                                  "seed" seed
                                  "agents_n" n
                                  "cards_available" (combo-count data)
                                  "wave" wave
                                  "dir" dir
                                  "has_baseline" (and has-baseline t)
                                  "agents" agents
                                  "rounds" (vector)
                                  "champion" ""
                                  "final" (nlk:json-object))))
      (open-round state (agent-ids n))
      state)))

(defun pair-round (state round alive)
  "The round's matches and its bye. An odd pool gives the bye to whoever has
had the fewest, and, where it can, a competitor is paired with one whose
reasoning mode differs, so every attack comes from another angle. The
first-listed side is drawn, which is the order a judge reads them in."
  (let ((generator (rng (format nil "arena-pair:~a:~d" (jget state "seed") round)))
        (agents (agents-of state))
        (bye nil))
    (let ((pool (shuffle-list generator (sort (copy-list alive) #'string<))))
      (when (oddp (length pool))
        (let ((fewest (loop for id in pool
                             minimize (or (jget (gethash id agents) "byes") 0))))
          (setf bye (find-if (lambda (id) (= fewest (or (jget (gethash id agents) "byes") 0)))
                             pool)
                pool (remove bye pool :count 1))))
      (let ((width (max 2 (length (format nil "~d" (floor (length pool) 2)))))
            (matches '()))
        (loop while pool
              for a = (pop pool)
              for mode = (jget (card-part (gethash a agents) "reasoning") "id")
              for at = (or (position-if
                            (lambda (b)
                              (not (equal mode (jget (card-part (gethash b agents) "reasoning") "id"))))
                            pool)
                           0)
              for b = (nth at pool)
              do (setf pool (append (subseq pool 0 at) (nthcdr (1+ at) pool)))
                 (when (zerop (rng-below generator 2)) (rotatef a b))
                 (push (nlk:json-object "id" (format nil "r~d-m~v,'0d" round width (1+ (length matches)))
                                         "a" a "b" b
                                         "winner" "" "loser" "" "reason" ""
                                         "survived" (vector) "scores" (nlk:json-object)
                                         "source" "")
                       matches))
        (values (coerce (nreverse matches) 'vector) bye)))))

(defun open-round (state alive)
  "Open the next round, or crown the champion when one competitor is left."
  (let ((alive (sort (copy-list alive) #'string<)))
    (if (= (length alive) 1)
        (progn
          (setf (gethash "champion" state) (first alive))
          (when (and (jget state "has_baseline") (null (final-of state)))
            (let ((first-is-champion (zerop (rng-below (rng (format nil "arena-final:~a"
                                                                     (jget state "seed")))
                                                       2))))
              (setf (gethash "final" state)
                    (nlk:json-object "X" (if first-is-champion "champion" "baseline")
                                     "Y" (if first-is-champion "baseline" "champion")
                                     "result" nil))))
          nil)
        (let ((round (1+ (length (rounds-of state)))))
          (multiple-value-bind (matches bye) (pair-round state round alive)
            (round-add state
                       (nlk:json-object "n" round
                                        "alive" (coerce alive 'vector)
                                        "matches" matches
                                        "bye" (or bye "")
                                        "closed" nil)))))))

(defun round-add (state round)
  (setf (gethash "rounds" state)
        (concatenate 'vector (rounds-of state) (vector round))))

(defun champion-of (state)
  (let ((champion (jget state "champion")))
    (and (stringp champion) (plusp (length champion)) champion)))

(defun final-of (state)
  "The blind final check's record, or NIL when the run has none."
  (let ((final (jget state "final")))
    (and (hash-table-p final) (jget final "X") final)))

(defun current-round (state)
  "The round taking work, or NIL when the arena has a champion or a closed
last round."
  (unless (champion-of state)
    (let* ((rounds (rounds-of state))
           (last (and (plusp (length rounds)) (aref rounds (1- (length rounds))))))
      (unless (and last (jtrue last "closed")) last))))

(defun rounds-played (state)
  (count-if (lambda (round) (jtrue round "closed")) (rounds-of state)))

(defun alive-ids (state)
  (sort (loop for id being the hash-keys of (agents-of state)
               when (jtrue (agent-of state id) "alive") collect id)
        #'string<))

(defun norm-agent (state raw)
  "RAW as one of the arena's agent ids, or a refusal."
  (let ((id (string-downcase (nlk:trimmed (princ-to-string (or raw ""))))))
    (or (and (gethash id (agents-of state)) id)
        (let ((digits (string-left-trim "a" id)))
          (and (plusp (length digits))
               (every #'digit-char-p digits)
               (loop for known being the hash-keys of (agents-of state)
                     when (= (parse-integer digits)
                             (parse-integer (subseq known 1)))
                       return known)))
        (fail "no agent called '~a'" raw))))

(defun find-match (state match)
  (let ((round (current-round state)))
    (unless round
      (fail "no round is open~a" (if (champion-of state) " (the arena has a champion)" "")))
    (let* ((raw (string-downcase (nlk:trimmed (princ-to-string (or match "")))))
           (exact (find-if (lambda (one) (equal raw (string-downcase (jget one "id"))))
                           (matches-of round)))
           (digits (string-left-trim "m" (car (last (uiop:split-string raw :separator "-m"))))))
      (or exact
          (and (plusp (length digits))
               (every #'digit-char-p digits)
               (find-if (lambda (one)
                          (= (parse-integer digits)
                             (parse-integer (car (last (uiop:split-string (jget one "id")
                                                                           :separator "-m"))))))
                        (matches-of round)))
          (fail "no match '~a' in round ~d (run pairings)" match (jget round "n"))))))

(defun record-match (state match winner &key reason (survived '()) scores (source "manual"))
  "Record WINNER in MATCH; answers the match and the winner it replaced."
  (let ((one (find-match state match)))
    (let ((winner (norm-agent state winner)))
      (unless (member winner (list (jget one "a") (jget one "b")) :test #'equal)
        (fail "~a is not in ~a (~a vs ~a)"
              winner (jget one "id") (jget one "a") (jget one "b")))
      (let ((previous (jget one "winner")))
        (setf (gethash "winner" one) winner
              (gethash "loser" one) (if (equal winner (jget one "a"))
                                         (jget one "b")
                                         (jget one "a"))
              (gethash "reason" one) (or reason "")
              (gethash "survived" one) (coerce survived 'vector)
              (gethash "scores" one) (or scores (nlk:json-object))
              (gethash "source" one) source)
        (values one previous)))))

(defun advance (state)
  "Close the round: every loser out, the winners' revised solutions carried
forward, the bye through, and the next round opened."
  (let ((round (current-round state)))
    (unless round
      (fail "no round is open~a" (if (champion-of state) " (the arena has a champion)" "")))
    (let ((unrecorded (loop for one across (matches-of round)
                            unless (plusp (length (text-of (jget one "winner"))))
                              collect (jget one "id"))))
      (when unrecorded
        (fail "round ~d has ~d unrecorded match(es): ~{~a~^, ~}"
              (jget round "n") (length unrecorded) unrecorded)))
    (let ((survivors '())
          (dir (jget state "dir"))
          (number (jget round "n")))
      (loop for one across (matches-of round)
            do (let* ((winner (jget one "winner"))
                      (loser (agent-of state (jget one "loser"))))
                 (setf (gethash "alive" loser) nil
                       (gethash "eliminated_in" loser) number
                       (gethash "eliminated_by" loser) winner)
                 (dolist (aid (list (jget one "a") (jget one "b")))
                   (let ((revised (revised-out dir number (jget one "id") aid)))
                     (when (has-output-p revised)
                       (setf (gethash "solution" (agent-of state aid)) revised))))
                 (push winner survivors)))
      (let ((bye (jget round "bye")))
        (when (and (stringp bye) (plusp (length bye)))
          (let ((agent (agent-of state bye)))
            (setf (gethash "byes" agent) (1+ (or (jget agent "byes") 0)))
            (push bye survivors))))
      (setf (gethash "closed" round) t)
      (open-round state survivors)
      survivors)))

;;; --- the judges' arithmetic ------------------------------------------------

(defparameter *weights*
  '(("correctness" . 30) ("completeness" . 25) ("specificity" . 15)
    ("robustness" . 20) ("clarity" . 10))
  "The five criteria and their weight, as skill/rubric.md has them.")

(defun member-value (object key)
  "OBJECT's member KEY, whatever case or spacing the writer gave it."
  (let ((wanted (string-downcase (nlk:trimmed key))))
    (when (hash-table-p object)
      (block found
        (maphash (lambda (name value)
                   (when (equal wanted (string-downcase (nlk:trimmed (princ-to-string name))))
                     (return-from found (unless (eq value :null) value))))
                 object)
        nil))))

(defun truthy (value)
  (if (stringp value)
      (member (string-downcase (nlk:trimmed value)) '("true" "yes" "1") :test #'equal)
      (and value t)))

(defun round2 (number)
  (/ (fround (* number 100)) 100))

(defun weighted-total (scores)
  "0 to 100 from five 0 to 10 scores, weighed as rubric.md weighs them; NIL
when one is missing."
  (when (hash-table-p scores)
    (let ((total 0) (complete t))
      (dolist (pair *weights*)
        (let ((value (member-value scores (car pair))))
          (cond ((or (null value) (eq value t) (eq value :false)) (setf complete nil))
                ((numberp value)
                 (incf total (* (max 0 (min 10 value)) (cdr pair))))
                ((and (stringp value)
                      (ignore-errors (read-from-string value))
                      (numberp (read-from-string value)))
                 (incf total (* (max 0 (min 10 (read-from-string value))) (cdr pair))))
                (t (setf complete nil)))))
      (when complete (round2 (/ total 10))))))

(defun extract-json (text)
  "The first JSON object TEXT holds, the rest of the file ignored."
  (when text
    (let ((direct (handler-case (nlk:decode-json text :whole t) (error () nil))))
      (or (and (hash-table-p direct) direct)
          (let ((from (search "{" text))
                (to (position #\} text :from-end t)))
            (when (and from to (> to from))
              (let ((inside (handler-case (nlk:decode-json (subseq text from (1+ to)) :whole t)
                             (error () nil))))
                (and (hash-table-p inside) inside))))))))

(defun standing-count (verdict id)
  (let ((standing (member-value (jget verdict "standing") id)))
    (and (vectorp standing) (length standing))))

(defun tie-break (verdict sa sb a b pick)
  (let ((na (standing-count verdict a)) (nb (standing-count verdict b)))
    (cond ((and na nb (/= na nb)) (if (< na nb) a b))
          (t (let ((ca (member-value sa "correctness"))
                   (cb (member-value sb "correctness")))
               (cond ((and (numberp ca) (numberp cb) (/= ca cb)) (if (> ca cb) a b))
                     (pick pick)
                     (t (fail "an exact tie and the judge did not pick a winner"))))))))

(defun decide-verdict (verdict a b)
  "The rubric's arithmetic over one judge's verdict: the winner, the two
totals, and a note when the judge's own pick lost."
  (unless (hash-table-p verdict) (fail "a verdict is not a JSON object"))
  (let* ((scores (jget verdict "scores"))
         (sa (member-value scores a))
         (sb (member-value scores b))
         (pick-text (string-downcase (princ-to-string (or (jget verdict "winner") ""))))
         (in-a (and (search a pick-text) t))
         (in-b (and (search b pick-text) t))
         (pick (cond ((and in-a (not in-b)) a) ((and in-b (not in-a)) b)))
         (ta (weighted-total sa))
         (tb (weighted-total sb))
         (totals (nlk:make-json-object a ta b tb)))
    (cond ((or (null ta) (null tb))
           (if pick
               (values pick totals "scores incomplete, used the judge's pick")
               (fail "a verdict has neither complete scores nor a clear winner")))
          (t (let ((fa (truthy (member-value sa "fatal")))
                   (fb (truthy (member-value sb "fatal"))))
               (let ((winner (cond ((not (eq (and fa t) (and fb t))) (if fa b a))
                                   ((/= ta tb) (if (> ta tb) a b))
                                   (t (tie-break verdict sa sb a b pick)))))
                 (values winner totals
                         (if (and pick (not (equal pick winner)))
                             (format nil "the judge picked ~a, but its own scores favour ~a, so the scores win"
                                     pick winner)
                             ""))))))))

(defun clip (text limit)
  (subseq text 0 (min limit (length text))))

(defun strings-of (value limit)
  (let ((array (if (vectorp value) value #())))
    (loop for i from 0 below (min limit (length array))
          for item = (aref array i)
          when (stringp item) collect item)))

(defun collect-final (state)
  "The blind final check's verdict, recorded on the run."
  (let* ((dir (jget state "dir"))
         (final (final-of state))
         (path (final-out dir)))
    (cond ((not (has-output-p path)) (list (list "final" "missing" "")))
          (t (let ((verdict (extract-json (read-text path))))
               (handler-case
                   (multiple-value-bind (winner totals note) (decide-verdict verdict "x" "y")
                     (let* ((better (jget final (string-upcase winner)))
                            (champion-side (if (equal (jget final "X") "champion") "x" "y"))
                            (baseline-side (if (equal (jget final "X") "baseline") "x" "y")))
                       (setf (gethash "result" final)
                             (nlk:json-object
                              "better" better
                              "champion_total" (jget totals champion-side)
                              "baseline_total" (jget totals baseline-side)
                              "reason" (clip (princ-to-string (or (jget verdict "reason") "")) 400)
                              "fixed" (coerce (strings-of (jget verdict "fixed") 10) 'vector)))
                       (list (list "final" "recorded"
                                   (string-trim " " (format nil "~a is better. ~a" better note))))))
                 (error (condition)
                   (ignore-errors
                    (rename-file path (concatenate 'string path ".unreadable")))
                   (list (list "final" "unreadable" (princ-to-string condition)))))))))
)
(defun collect-match (state round one)
  "One match's verdict, read and recorded."
  (let* ((dir (jget state "dir"))
         (number (jget round "n"))
         (match (jget one "id"))
         (a (jget one "a"))
         (b (jget one "b"))
         (path (verdict-out dir number match)))
    (if (not (has-output-p path))
        (list match "missing" "")
        (let ((verdict (extract-json (read-text path))))
          (handler-case
              (multiple-value-bind (winner totals note) (decide-verdict verdict a b)
                (let ((recorded (record-match state match winner
                                              :reason (clip (princ-to-string
                                                             (or (jget verdict "reason") ""))
                                                            400)
                                              :survived (strings-of (jget verdict "survived") 10)
                                              :scores totals
                                              :source "judge")))
                  (list match "recorded"
                        (string-trim " " (format nil "~a beat ~a, ~a to ~a. ~a"
                                                   winner (jget recorded "loser")
                                                   (fmt-total (jget totals winner))
                                                   (fmt-total (jget totals (jget recorded "loser")))
                                                   note)))))
            (error (condition)
              (ignore-errors
               (rename-file path (concatenate 'string path ".unreadable")))
              (list match "unreadable" (princ-to-string condition))))))))

(defun collect (state)
  "Record every verdict on disk for the open round, or the final check.
Answers a list of (WHAT STATE NOTE)."
  (cond
    ((champion-of state)
     (let ((final (final-of state)))
       (if (or (null final) (jget final "result")) '() (collect-final state))))
    (t (let ((round (current-round state)))
         (when round
           (loop for one across (matches-of round)
                 unless (plusp (length (text-of (jget one "winner"))))
                   collect (collect-match state round one)))))))
(defun fmt-total (number)
  (if (numberp number) (format nil "~,2f" (float number 1.0)) "?"))

(defun text-of (value)
  (if (stringp value) value (if value (princ-to-string value) "")))

;;; --- the jobs, and the briefs they are told --------------------------------

(defparameter *phases* '("spawn" "attack" "defend" "judge" "final"))

(defun phase-jobs (state phase)
  "Every sub-agent job one phase of the arena still holds, each with the
prompt file it reads and the outputs it must write. A match already decided
holds none."
  (unless (member phase *phases* :test #'equal)
    (fail "unknown phase '~a' (one of: spawn, attack, defend, judge, final)" phase))
  (let ((dir (jget state "dir"))
        (jobs '()))
    (cond
      ((equal phase "spawn")
       (dolist (aid (sort (agent-ids-of state) #'string<))
         (push (list :id (format nil "~a.spawn" aid) :kind "competitor" :round 0
                     :agent aid :outputs (list (spawn-out dir aid)))
               jobs)))
      ((equal phase "final")
       (let ((final (final-of state)))
         (when (and (champion-of state) final (not (jget final "result")))
           (push (list :id "final.judge" :kind "final" :round nil
                       :outputs (list (final-out dir)))
                 jobs))))
      (t (let ((round (current-round state)))
           (when round
             (dolist (match (coerce (matches-of round) 'list))
               (unless (plusp (length (text-of (jget match "winner"))))
                 (dolist (job (match-jobs state round match phase dir))
                   (push job jobs))))))))
    (nreverse (mapcar (lambda (job)
                        (list* :prompt (prompt-path dir (getf job :round) (getf job :id))
                               job))
                      jobs))))

(defun match-jobs (state round match phase dir)
  "The jobs one undecided MATCH holds in PHASE: one judge, or a pair of
attacks, or a pair of defenses."
  (let ((number (jget round "n"))
        (id (jget match "id"))
        (a (jget match "a"))
        (b (jget match "b")))
    (cond
      ((equal phase "judge")
       (list (list :id (format nil "~a.judge" id) :kind "judge" :round number
                   :match id :a a :b b
                   :outputs (list (verdict-out dir number id)))))
      ((equal phase "attack")
       (list (list :id (format nil "~a.~a.attack" id a) :kind "attacker" :round number
                   :match id :agent a :opponent b
                   :outputs (list (attack-out dir number id a)))
             (list :id (format nil "~a.~a.attack" id b) :kind "attacker" :round number
                   :match id :agent b :opponent a
                   :outputs (list (attack-out dir number id b)))))
      (t (list (list :id (format nil "~a.~a.defend" id a) :kind "defender" :round number
                     :match id :agent a :opponent b
                     :outputs (list (defense-out dir number id a)
                                    (revised-out dir number id a)))
               (list :id (format nil "~a.~a.defend" id b) :kind "defender" :round number
                     :match id :agent b :opponent a
                     :outputs (list (defense-out dir number id b)
                                    (revised-out dir number id b))))))))

(defun missing-jobs (jobs)
  (remove-if (lambda (job) (every #'has-output-p (getf job :outputs))) jobs))

(defun template-in (text name)
  "The ```text block between NAME's template marks in TEXT."
  (let ((open (search (format nil "<!-- template:~a -->" name) text)))
    (when open
      (let* ((fence (search "```text" text :start2 open))
             (from (and fence (+ fence (length "```text"))))
             (to (and from (search "```" text :start2 from))))
        (when (and from to)
          (string-trim '(#\Newline #\Return #\Space) (subseq text from to)))))))

(defun load-templates ()
  (let ((text (read-text (data-file "SKILL.md"))))
    (unless text (fail "cannot read the prompt templates from skill/SKILL.md"))
    (let ((table (make-hash-table :test #'equal)))
      (dolist (name '("competitor" "attacker" "defender" "judge" "final"))
        (let ((body (template-in text name)))
          (unless body (fail "skill/SKILL.md is missing the ~a prompt template" name))
          (setf (gethash name table) body)))
      table)))

(defun placeholders (template)
  (loop with found = '() with start = 0
        for open = (search "{{" template :start2 start)
        while open
        for close = (search "}}" template :start2 (+ open 2))
        while close
        do (push (subseq template (+ open 2) close) found)
           (setf start (+ close 2))
        finally (return (nreverse found))))

(defun fill-template (template values)
  "TEMPLATE with every {{name}} replaced from VALUES, an alist of strings, in
one pass, so a task that itself carries braces is left as it was written."
  (let ((wanted (remove-duplicates (placeholders template) :test #'equal)))
    (let ((missing (remove-if (lambda (name) (assoc name values :test #'equal)) wanted)))
      (when missing
        (fail "no value for placeholder(s): ~{~a~^, ~}" missing)))
    (with-output-to-string (out)
      (loop with start = 0
            for open = (search "{{" template :start2 start)
            while open
            for close = (search "}}" template :start2 (+ open 2))
            while close
            do (write-string template out :start start :end open)
               (write-string (cdr (assoc (subseq template (+ open 2) close) values :test #'equal)) out)
               (setf start (+ close 2))
            finally (write-string template out :start start)))))

(defun read-task (state)
  (let ((text (read-text (sub (jget state "dir") "task.md"))))
    (unless text (fail "the run has no task.md"))
    (string-right-trim '(#\Newline #\Return) text)))

(defun card-values (agent)
  (loop for part in '("reasoning" "workflow" "strategy")
        append (list (cons (format nil "~a_name" part) (text-of (jget (card-part agent part) "name")))
                     (cons (format nil "~a_how" part) (text-of (jget (card-part agent part) "how"))))))

(defun render-job (state job templates task)
  (let* ((dir (jget state "dir"))
         (agents (agents-of state))
         (kind (getf job :kind))
         (base (list (cons "task" task)
                     (cons "arena_dir" dir)
                     (cons "rubric" (sub dir "rubric.md"))
                     (cons "n" (format nil "~d" (jget state "agents_n"))))))
    (ecase (intern (string-upcase kind) :keyword)
      (:competitor
       (let ((agent (getf job :agent)))
         (fill-template (gethash "competitor" templates)
               (append base (card-values (agent-of state agent))
                       (list (cons "agent" agent)
                             (cons "out" (first (getf job :outputs)))
                             (cons "baseline_note"
                                   (if (jget state "has_baseline")
                                       (format nil "The user already got an answer to this task and was NOT satisfied with it. It is at ~a. Read it first and work out exactly why it fell short. Then beat it. Do not just polish it: the winner of this arena is compared with that answer at the end."
                                               (sub dir "baseline.md"))
                                       "There is no earlier answer to beat. Start from the task.")))))))
      (:attacker
       (let ((me (getf job :agent)) (opponent (getf job :opponent)))
         (fill-template (gethash "attacker" templates)
               (append base (card-values (agent-of state me))
                       (list (cons "agent" me)
                             (cons "target" opponent)
                             (cons "match" (getf job :match))
                             (cons "round" (format nil "~d" (getf job :round)))
                             (cons "own_solution" (text-of (jget (agent-of state me) "solution")))
                             (cons "target_solution" (text-of (jget (agent-of state opponent) "solution")))
                             (cons "out" (first (getf job :outputs))))))))
      (:defender
       (let ((me (getf job :agent)) (opponent (getf job :opponent)))
         (fill-template (gethash "defender" templates)
               (append base (card-values (agent-of state me))
                       (list (cons "agent" me)
                             (cons "attacker" opponent)
                             (cons "match" (getf job :match))
                             (cons "round" (format nil "~d" (getf job :round)))
                             (cons "own_solution" (text-of (jget (agent-of state me) "solution")))
                             (cons "attacks" (attack-out dir (getf job :round) (getf job :match) opponent))
                             (cons "defense_out" (first (getf job :outputs)))
                             (cons "solution_out" (second (getf job :outputs))))))))
      (:judge
       (let ((a (getf job :a)) (b (getf job :b))
             (number (getf job :round)) (match (getf job :match)))
         (fill-template (gethash "judge" templates)
               (append base
                       (list (cons "match" match)
                             (cons "round" (format nil "~d" number))
                             (cons "first" a) (cons "second" b)
                             (cons "first_solution" (revised-out dir number match a))
                             (cons "second_solution" (revised-out dir number match b))
                             (cons "first_attacks" (attack-out dir number match b))
                             (cons "second_attacks" (attack-out dir number match a))
                             (cons "first_defense" (defense-out dir number match a))
                             (cons "second_defense" (defense-out dir number match b))
                             (cons "out" (first (getf job :outputs))))))))
      (:final
       (fill-template (gethash "final" templates)
             (append base
                     (list (cons "rounds" (format nil "~d" (rounds-played state)))
                           (cons "x_solution" (sub dir "final/X.md"))
                           (cons "y_solution" (sub dir "final/Y.md"))
                           (cons "out" (first (getf job :outputs))))))))))

(defun write-prompts (state phase)
  "Write every brief the phase holds; answers the jobs, or NIL when there are
none. The outputs' folders are made too, so a sub-agent writing its first
file needs no approval for a folder that is not there."
  (let ((jobs (phase-jobs state phase)))
    (when jobs
      (let ((templates (load-templates))
            (task (read-task state))
            (dir (jget state "dir")))
        (dolist (job jobs)
          (dolist (path (getf job :outputs))
            (ensure-dir (uiop:pathname-directory-pathname path)))
          (write-text (getf job :prompt) (format nil "~a~%" (render-job state job templates task))))
        (when (equal phase "spawn")
          (dolist (aid (agent-ids-of state))
            (ensure-dir (sub dir (format nil "scratch/~a/" aid)))))
        (when (equal phase "final")
          (let ((final (final-of state))
                (sources (list (cons "champion"
                                     (text-of (jget (agent-of state (champion-of state)) "solution")))
                               (cons "baseline" (sub dir "baseline.md")))))
            (dolist (label '("X" "Y"))
              (let ((text (read-text (cdr (assoc (text-of (jget final label)) sources :test #'equal)))))
                (unless text
                  (fail "the ~a solution is missing, so the final check cannot be blind"
                        (text-of (jget final label))))
                (write-text (sub dir (format nil "final/~a.md" label)) text)))))))
    jobs))

(defun next-action (state)
  "What the orchestrator does now: (values PHASE JOBS-LEFT JOBS-ALL), PHASE one
of spawn, attack, defend, judge, final, collect, advance, done."
  (let* ((spawn (phase-jobs state "spawn"))
         (left (missing-jobs spawn)))
    (if left
        (values "spawn" left spawn)
        (if (champion-of state)
            (let ((final (final-of state)))
              (if (and final (not (jget final "result")))
                  (let* ((jobs (phase-jobs state "final"))
                         (left (missing-jobs jobs)))
                    (if left (values "final" left jobs) (values "collect" '() jobs)))
                  (values "done" '() '())))
            (let ((open nil))
              (dolist (phase '("attack" "defend" "judge"))
                (unless open
                  (let* ((jobs (phase-jobs state phase))
                         (left (missing-jobs jobs)))
                    (when left (setf open (list phase left jobs))))))
              (if open
                  (values (first open) (second open) (third open))
                  (let ((round (current-round state)))
                    (if (and round
                             (find-if (lambda (one)
                                        (not (plusp (length (text-of (jget one "winner"))))))
                                      (matches-of round)))
                        (values "collect" '() '())
                        (values "advance" '() '())))))))))

;;; --- the plan --------------------------------------------------------------

(defun plan-rows (n wave)
  (loop for alive in (butlast (bracket-sizes n))
        for round from 1
        for matches = (floor alive 2)
        collect (list :round round :alive alive :matches matches :bye (oddp alive)
                      :calls (* 5 matches)
                      :waves (+ (* 2 (ceiling (* 2 matches) wave)) (ceiling matches wave)))))

(defun plan-totals (n &key (wave 10))
  (let ((rows (plan-rows n wave)))
    (list :agents n :rounds (length rows)
          :calls (+ n (loop for row in rows sum (getf row :calls)))
          :waves (+ (ceiling n wave) (loop for row in rows sum (getf row :waves)))
          :rows rows)))

;;; --- where a run lives -----------------------------------------------------

(defun arena-root ()
  "The .arena folder of the working directory."
  (sub (namestring (uiop:getcwd)) ".arena/"))

(defun latest-path () (sub (arena-root) "LATEST"))

(defun absolute-dir (dir)
  (namestring (uiop:ensure-directory-pathname
               (merge-pathnames (uiop:ensure-directory-pathname dir) (uiop:getcwd)))))

(defun resolved-dir (dir)
  (let ((given (nlk:trimmed (princ-to-string (or dir "")))))
    (if (plusp (length given))
        (absolute-dir given)
        (let ((latest (read-text (latest-path))))
          (if (and latest (plusp (length (nlk:trimmed latest))))
              (nlk:trimmed latest)
              (fail "no arena here: run init first, or pass dir"))))))

(defun save-state (state)
  (nlk:write-file-atomically (sub (jget state "dir") "arena.json")
                             (format nil "~a~%" (nlk:pretty-json state))))

(defun load-state (&key dir)
  (let* ((where (resolved-dir dir))
         (path (sub where "arena.json")))
    (unless (probe-file path) (fail "no arena.json in ~a" where))
    (let ((state (handler-case (nlk:decode-json (uiop:read-file-string path :external-format :utf-8)
                                               :whole t)
                   (error () (fail "arena.json in ~a is not JSON" where)))))
      (unless (hash-table-p state) (fail "arena.json in ~a is not an arena" where))
      state)))

;;; --- how a verb is named back to the orchestrator ---------------------------

(defun cmd (&rest parts)
  (format nil "arena ~{~a~^ ~}" parts))

(defun arena-size (agents quick default)
  (cond ((and quick (plusp agents)) (fail "pass quick or agents, not both"))
        (quick 16)
        ((plusp agents) agents)
        (t default)))

(defun compact-stamp ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" year month date hour minute second)))

;;; --- the verbs -------------------------------------------------------------

(defun verb-plan (agents quick wave)
  (let* ((n (arena-size agents quick (setting :agents)))
         (width (if (and wave (plusp wave)) wave (setting :wave)))
         (totals (plan-totals n :wave width)))
    (with-output-to-string (out)
      (format out "~d agents, ~d rounds, waves of ~d~%~%" n (getf totals :rounds) width)
      (format out "  round  alive  matches  bye  sub-agent calls  waves~%")
      (format out "  spawn  ~5d  ~7@a  ~3@a  ~15d  ~5d~%"
              n "-" "-" n (ceiling n width))
      (dolist (row (getf totals :rows))
        (format out "  ~5d  ~5d  ~7d  ~3@a  ~15d  ~5d~%"
                (getf row :round) (getf row :alive) (getf row :matches)
                (if (getf row :bye) "yes" "-") (getf row :calls) (getf row :waves)))
      (format out "  total                     ~15d  ~5d~%~%" (getf totals :calls) (getf totals :waves))
      (format out "  alive per round: ~{~d~^ -> ~}~%" (bracket-sizes n))
      (format out "  plus one call for the final check when there is a rejected answer to beat~%"))))

(defun verb-init (task agents quick wave seed baseline dir)
  (let* ((data (strategies))
         (n (arena-size agents quick (setting :agents)))
         (width (if (and wave (plusp wave)) wave (setting :wave)))
         (text (nlk:trimmed (or task "")))
         (seed (if (and seed (plusp seed)) seed (1+ (random 999999))))
         (default-run (not (plusp (length (nlk:trimmed (princ-to-string (or dir "")))))))
         (where (if default-run
                    (sub (arena-root) (format nil "run-~a-s~d/" (compact-stamp) seed))
                    (absolute-dir (nlk:trimmed dir))))
         (baseline-text (nlk:trimmed (or baseline ""))))
    (when (zerop (length text))
      (fail "the task is empty: pass task, if no task is given use the user's own last request"))
    (when (probe-file (sub where "arena.json"))
      (fail "~a already holds an arena: pick another dir" where))
    (ensure-dir where)
    (write-text (sub where "task.md") (format nil "~a~%" text))
    (when (plusp (length baseline-text))
      (write-text (sub where "baseline.md") (format nil "~a~%" baseline-text)))
    (let ((rubric (read-text (data-file "rubric.md"))))
      (unless rubric (fail "cannot read skill/rubric.md"))
      (write-text (sub where "rubric.md") rubric))
    (let ((state (new-state n seed data where :wave width
                                              :has-baseline (plusp (length baseline-text)))))
      (save-state state)
      (when default-run
        (ensure-dir (arena-root))
        (write-text (latest-path) (format nil "~a~%" where)))
      (let ((totals (plan-totals n :wave width)))
        (with-output-to-string (out)
          (format out "arena ready: ~a~%" where)
          (format out "seed ~d. ~d agents, ~d distinct cards dealt from ~d, no repeats.~%"
                  seed n n (combo-count data))
          (format out "~d rounds (~{~d~^ -> ~}). ~d sub-agent calls in ~d waves of ~d~a.~%"
                  (getf totals :rounds) (bracket-sizes n)
                  (+ (getf totals :calls) (if (plusp (length baseline-text)) 1 0))
                  (+ (getf totals :waves) (if (plusp (length baseline-text)) 1 0))
                  width (if (plusp (length baseline-text)) ", including the final check" ""))
          (format out "next: ~a~%" (cmd "command=next")))))))

(defun verb-next (dir)
  (let ((state (load-state :dir dir)))
    (multiple-value-bind (phase left jobs) (next-action state)
      (declare (ignore left))
      (let* ((round (current-round state))
             (total (1- (length (bracket-sizes (jget state "agents_n")))))
             (job-count (length jobs))
             (left-count (length (missing-jobs jobs))))
        (with-output-to-string (out)
          (cond ((equal phase "spawn")
                 (format out "spawn: ~d competitors, then ~d round(s)~%"
                         (jget state "agents_n") total))
                (round (format out "round ~d of ~d, ~d alive~%"
                               (jget round "n") total (length (jget round "alive"))))
                ((champion-of state)
                 (format out "champion: ~a after ~d round(s)~%"
                         (champion-of state) (rounds-played state))))
          (cond ((member phase *phases* :test #'equal)
                 (format out "NEXT: ~a. ~d of ~d job(s) to run.~%" phase left-count job-count)
                 (format out "  1. ~a~%" (cmd "command=prompts" (format nil "phase=~a" phase)))
                 (format out "  2. launch the jobs it lists, one wave of ~d at a time~%"
                         (jget state "wave"))
                 (format out "  3. ~a~%" (cmd "command=next")))
                ((equal phase "collect")
                 (format out "NEXT: collect. Every verdict for this step is on disk.~%")
                 (format out "  ~a~%" (cmd "command=collect")))
                ((equal phase "advance")
                 (format out "NEXT: advance. Every match in round ~d has a winner.~%"
                         (jget round "n"))
                 (format out "  ~a~%" (cmd "command=advance")))
                (t (format out "DONE. ~a~%" (cmd "command=winner")))))))))

(defun verb-prompts (phase dir)
  (let* ((state (load-state :dir dir))
         (phase (string-downcase (nlk:trimmed (princ-to-string (or phase "")))))
         (jobs (write-prompts state phase)))
    (if (null jobs)
        (format nil "nothing to run for ~a right now. ~a~%" phase (cmd "command=next"))
        (let* ((left (missing-jobs jobs))
               (width (jget state "wave"))
               (round (current-round state))
               (where (cond ((equal phase "spawn") "round 0")
                            ((equal phase "final") "final")
                            (t (format nil "round ~d" (jget round "n")))))
               (waves (loop for from from 0 below (length left) by width
                            collect (subseq left from (min (length left) (+ from width))))))
          (with-output-to-string (out)
            (format out "~a, ~a: ~d job(s), ~d still to run, ~d wave(s) of up to ~d~%"
                    where phase (length jobs) (length left) (length waves) width)
            (loop for wave in waves for k from 1
                  do (format out "wave ~d~%" k)
                     (dolist (job wave)
                       (format out "  ~a  ~a~%" (getf job :id) (getf job :prompt))))
            (when left
              (format out "~%Each job is one sub-agent call: its own session, told~%")
              (format out "Read <prompt path> and follow it exactly. It is your whole brief.~%")
              (format out "A wave is one round of calls, all of them at once. Let it finish before the next wave.~%"))
            (format out "then: ~a~%" (cmd "command=next")))))))

(defun verb-check (phase dir)
  (let* ((state (load-state :dir dir))
         (phase (string-downcase (nlk:trimmed (princ-to-string (or phase "")))))
         (jobs (phase-jobs state phase))
         (left (missing-jobs jobs)))
    (with-output-to-string (out)
      (format out "~a: ~d of ~d job(s) done~%" phase (- (length jobs) (length left)) (length jobs))
      (dolist (job left)
        (format out "  missing ~a  ~{~a~^, ~}~%"
                (getf job :id)
                (remove-if #'has-output-p (getf job :outputs)))))))

(defun verb-pairings (dir)
  (let* ((state (load-state :dir dir))
         (round (current-round state)))
    (cond ((null round)
           (if (champion-of state)
               (format nil "No matches left. ~a is the last one standing.~%" (champion-of state))
               (fail "no round is open")))
          (t (let ((bye (jget round "bye")))
               (with-output-to-string (out)
                 (format out "round ~d: ~d alive, ~d match(es), bye: ~a~%"
                         (jget round "n") (length (nlk:json-array round "alive"))
                         (length (matches-of round))
                         (if (plusp (length (text-of bye))) bye "none"))
                 (loop for one across (matches-of round)
                       do (format out "  ~a  ~a vs ~a  (~a vs ~a)  ~a~%"
                                  (jget one "id") (jget one "a") (jget one "b")
                                  (text-of (jget (card-part (agent-of state (jget one "a")) "reasoning") "name"))
                                  (text-of (jget (card-part (agent-of state (jget one "b")) "reasoning") "name"))
                                  (if (plusp (length (text-of (jget one "winner"))))
                                      (format nil "winner ~a" (jget one "winner"))
                                      "open")))
                 (when (plusp (length (text-of bye)))
                   (format out "  bye     ~a goes through without a match~%" bye)))))))
)
(defun verb-record (match winner reason dir)
  (let ((state (load-state :dir dir)))
    (multiple-value-bind (one previous) (record-match state match winner :reason (or reason ""))
      (save-state state)
      (format nil "~a: ~a beat ~a~a~%"
              (jget one "id") (jget one "winner") (jget one "loser")
              (if (and (plusp (length (text-of previous)))
                       (not (equal previous (jget one "winner"))))
                  (format nil " (was ~a)" previous)
                  "")))))

(defun verb-collect (dir)
  (let ((state (load-state :dir dir)))
    (let ((results (collect state)))
      (save-state state)
      (if (null results)
          (format nil "nothing to collect. ~a~%" (cmd "command=next"))
          (with-output-to-string (out)
            (let ((bad 0))
              (dolist (result results)
                (unless (equal (second result) "recorded") (incf bad))
                (format out "  ~a  ~a  ~a~%" (first result) (second result) (third result)))
              (let ((round (current-round state)))
                (when round
                  (format out "round ~d: ~d of ~d match(es) recorded~%"
                          (jget round "n")
                          (count-if (lambda (one) (plusp (length (text-of (jget one "winner")))))
                                    (matches-of round))
                          (length (matches-of round)))))
              (if (plusp bad)
                  (format out "~d verdict(s) missing or unreadable. Re-run those judges: ~a~%"
                          bad (cmd "command=next"))
                  (format out "then: ~a~%" (cmd "command=next")))))))))

(defun verb-advance (dir)
  (let ((state (load-state :dir dir)))
    (let* ((round (current-round state))
           (closing (and round (jget round "n")))
           (survivors (advance state)))
      (save-state state)
      (with-output-to-string (out)
        (format out "round ~a closed: ~d through, ~d eliminated~%"
                closing (length survivors) (length (matches-of round)))
        (if (champion-of state)
            (format out "CHAMPION: ~a. ~a~%" (champion-of state) (cmd "command=next"))
            (let ((next (aref (rounds-of state) (1- (length (rounds-of state))))))
              (format out "round ~d open: ~d match(es)~a. ~a~%"
                      (jget next "n") (length (matches-of next))
                      (if (plusp (length (text-of (jget next "bye"))))
                          (format nil ", bye ~a" (jget next "bye"))
                          "")
                      (cmd "command=next"))))))))

(defun verb-status (dir)
  (let* ((state (load-state :dir dir))
         (alive (alive-ids state)))
    (with-output-to-string (out)
      (format out "arena ~a~%" (jget state "dir"))
      (format out "seed ~a, ~d agents, waves of ~a, ~d distinct cards available~%"
              (jget state "seed") (jget state "agents_n") (jget state "wave")
              (jget state "cards_available"))
      (format out "  round  alive  matches  bye     eliminated  state~%")
      (dolist (round (coerce (rounds-of state) 'list))
        (let* ((match-list (coerce (matches-of round) 'list))
               (recorded (count-if (lambda (one) (plusp (length (text-of (jget one "winner"))))) match-list))
               (bye (jget round "bye")))
          (format out "  ~5d  ~5d  ~7d  ~6@a  ~10d  ~a~%"
                  (jget round "n") (length (jget round "alive")) (length match-list)
                  (if (plusp (length (text-of bye))) bye "-")
                  recorded
                  (if (jtrue round "closed")
                      "closed"
                      (format nil "open, ~d of ~d recorded" recorded (length match-list))))))
      (format out "now: ~d alive, ~d eliminated~a~%"
              (length alive) (- (jget state "agents_n") (length alive))
              (if (champion-of state)
                  (format nil ", champion ~a" (champion-of state))
                  "")))))

(defun winner-report (state)
  (let ((champion (champion-of state)))
    (when champion
      (let ((agent (agent-of state champion)) (path '()) (survived '()))
        (dolist (round (coerce (rounds-of state) 'list))
          (let ((bye (text-of (jget round "bye"))))
            (when (equal champion bye)
              (push (list :round (jget round "n") :bye t) path))
            (loop for one across (matches-of round)
                  when (member champion (list (jget one "a") (jget one "b")) :test #'equal)
                    do (let* ((opponent (if (equal (jget one "a") champion) (jget one "b") (jget one "a")))
                              (scores (jget one "scores")))
                         (push (list :round (jget round "n") :bye nil
                                     :beat opponent :match (jget one "id")
                                     :score (jget scores champion)
                                     :opponent-score (jget scores opponent)
                                     :reason (text-of (jget one "reason"))
                                     :survived (coerce (nlk:json-array one "survived") 'list))
                               path)
                         (setf survived (append survived
                                                (coerce (nlk:json-array one "survived") 'list))))))) 
        (list :champion champion
              :card-line (format nil "~a + ~a + ~a"
                                 (text-of (jget (card-part agent "reasoning") "name"))
                                 (text-of (jget (card-part agent "workflow") "name"))
                                 (text-of (jget (card-part agent "strategy") "name")))
              :solution (text-of (jget agent "solution"))
              :rounds (rounds-played state)
              :agents (jget state "agents_n")
              :matches-won (count-if (lambda (entry) (not (getf entry :bye))) path)
              :path (nreverse path)
              :survived survived
              :final (jget (jget state "final") "result"))))))

(defun verb-winner (dir)
  (let* ((state (load-state :dir dir))
         (report (winner-report state)))
    (if (null report)
        (let ((round (current-round state)))
          (format nil "no winner yet: round ~a, ~d alive. ~a~%"
                  (if round (jget round "n") "?")
                  (length (alive-ids state)) (cmd "command=next")))
        (with-output-to-string (out)
          (format out "CHAMPION  ~a~%" (getf report :champion))
          (format out "card      ~a~%" (getf report :card-line))
          (format out "solution  ~a~%" (getf report :solution))
          (format out "rounds    ~d played, ~d agents in, 1 left, ~d match(es) won~%"
                  (getf report :rounds) (getf report :agents) (getf report :matches-won))
          (dolist (entry (getf report :path))
            (if (getf entry :bye)
                (format out "  round ~d  bye~%" (getf entry :round))
                (progn
                  (format out "  round ~d  beat ~a, ~a to ~a. ~a~%"
                          (getf entry :round) (getf entry :beat)
                          (fmt-total (getf entry :score)) (fmt-total (getf entry :opponent-score))
                          (getf entry :reason))
                  (dolist (line (getf entry :survived))
                    (format out "           survived: ~a~%" line)))))
          (let ((final (getf report :final)))
            (when final
              (format out "vs the answer you rejected: winner ~a, rejected ~a, ~a. ~a~%"
                      (fmt-total (jget final "champion_total"))
                      (fmt-total (jget final "baseline_total"))
                      (if (equal (text-of (jget final "better")) "champion")
                          "the contested solution beats it"
                          "the answer you rejected scored higher, so say so")
                      (text-of (jget final "reason")))))))))

(defun verb-card (agent dir)
  (let* ((state (load-state :dir dir))
         (id (norm-agent state agent))
         (one (agent-of state id)))
    (with-output-to-string (out)
      (format out "~a  ~a~%" id
              (if (jtrue one "alive")
                  "alive"
                  (format nil "eliminated in round ~a by ~a"
                          (jget one "eliminated_in") (jget one "eliminated_by"))))
      (dolist (part '("reasoning" "workflow" "strategy"))
        (format out "  ~9a ~a. ~a~%" part
                (text-of (jget (card-part one part) "name"))
                (text-of (jget (card-part one part) "how"))))
      (format out "  solution  ~a~%" (text-of (jget one "solution"))))))

;;; --- one door onto them ----------------------------------------------------

(defun run (command &key (task "") (agents 0) (quick nil) (wave 0) (seed 0)
                      (baseline "") (phase "") (match "") (winner "")
                      (agent "") (reason "") (dir ""))
  "One arena verb, its text back. COMMAND is one of plan, init, next, prompts,
check, pairings, collect, advance, status, record, winner, card."
  (let ((command (string-downcase (nlk:trimmed (princ-to-string (or command ""))))))
    (cond ((equal command "plan") (verb-plan agents quick wave))
          ((equal command "init") (verb-init task agents quick wave seed baseline dir))
          ((equal command "next") (verb-next dir))
          ((equal command "prompts") (verb-prompts phase dir))
          ((equal command "check") (verb-check phase dir))
          ((equal command "pairings") (verb-pairings dir))
          ((equal command "collect") (verb-collect dir))
          ((equal command "advance") (verb-advance dir))
          ((equal command "status") (verb-status dir))
          ((equal command "record") (verb-record match winner reason dir))
          ((equal command "winner") (verb-winner dir))
          ((equal command "card") (verb-card agent dir))
          (t (fail "~a is not one of the arena's commands (plan, init, next, prompts, check, pairings, collect, advance, status, record, winner, card)"
                   command)))))
