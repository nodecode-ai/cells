;;;; observe-test.lisp --- the wake's observe gate and its spend reader.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun wake-call-verdict (form &key (name "eval") strings json)
  "WAKE-CALL-REFUSAL of one eval call carrying FORM (and STRINGS, an alist),
its arguments a hash table, or the JSON text when JSON."
  (let ((arguments (make-hash-table :test 'equal))
        (table (make-hash-table :test 'equal)))
    (loop for (k . v) in strings do (setf (gethash k table) v))
    (setf (gethash "form" arguments) form (gethash "strings" arguments) table)
    (nodecode-cron::wake-call-refusal
     (list :name name :call-id "c1"
           :arguments (if json (nlk:encode-json-object arguments) arguments)))))

(deftest cron-cell-wake-gate-refuses-every-act ()
  ;; Each line acts, or turns a value into a call the walk cannot see: the
  ;; kernel's replay check admits the first five.
  (dolist (form '("(mapcar 'delete-file (list \"x\"))"
                  "(find 1 (list 1) :key 'sh)"
                  "(coerce '(lambda () (delete-file 1)) 'function)"
                  "(read-from-string \"#.(delete-file 1)\")"
                  "(format nil \"~/sh/\" 1)"
                  "(funcall (intern \"SH\") \"x\")"
                  "(write-file \"x\" \"y\")" "(setf *print-base* 2)" "(defun f () 1)"
                  "(sh \"rm -rf x\")" "(sh \"ls; rm x\")" "(sh \"cat a > b\")"
                  "(sh \"echo $(rm x)\")" "(sh \"/bin/rm x\")" "(sh \"X=1 ls\")"
                  "(sh \"git push\")" "(sh \"git -c core.pager=sh log\")" "(sh \"git -C /r commit -m x\")"
                  "(sh \"git branch -D main\")" "(sh \"git diff --output=f\")"
                  "(sh \"find . -delete\")" "(sh \"find . -exec rm {} +\")" "(sh \"sort -o f g\")"
                  "(sh \"tail -f log\")" "(sh \"nvidia-smi -pl 100\")" "(sh \"systemctl restart x\")"
                  "(sh \"journalctl --vacuum-time=1s\")" "(sh \"docker rm x\")" "(sh \"curl -X POST u\")"
                  "(sh \"sed -i s/a/b/ f\")" "(sh \"sed -n w,out f\")" "(sh \"gh pr merge 1\")" "(sh \"gh api -X DELETE x\")"
                  "(nodecode-cron:wake :on)" "(nodecode-cron:remove \"wake\")"
                  "(sh \"ls 'unclosed\")" "(sh (concatenate 'string \"r\" \"m x\"))"))
    (is (stringp (wake-call-verdict form)) (format nil "refused: ~a" form)))
  (is (search "touch" (wake-call-verdict "(sh ($ \"s\"))" :strings '(("s" . "touch x")))) "a ($ key) command is judged by the text it names"))

(deftest cron-cell-wake-gate-passes-reads ()
  (dolist (form '("(recall-log :match \"keep an eye\")"
                  "(let ((rows (recall-log :match \"x\"))) (length rows))"
                  "(mapcar #'first (list (list 1 2)))"
                  "(remove-if (lambda (r) (null r)) (list 1 nil))"
                  "(sort (list 3 1) #'<)" "(format nil \"~a of ~a\" 1 2)"
                  "(help 'nodecode-cron:add)" "(nodecode-cron:runs \"wake\")"
                  "(sh \"git log --oneline -5 | head -3\")" "(sh \"git -C /r status --short\")"
                  "(sh \"git branch -a\")" "(sh \"grep -rn 'two words' src | wc -l\")"
                  "(sh \"nvidia-smi --query-gpu=memory.used --format=csv\")"
                  "(sh \"systemctl --user status nodecode\")" "(sh \"journalctl -n 50 -u x\")"
                  "(sh \"docker logs --tail 50 web\")" "(sh \"df -h\" :timeout 10)" "(sh \"sed -n 1,40p file.lisp\")"
                  "(sh \"gh pr checks 178\")" "(nodecode-cron:wake-next \"2h\" \"why\")"
                  "(nodecode-cron:wake :status)" "(let (seen (n 0)) (push 1 seen) (incf n) seen)"
                  "(sh \"cd ~/Lab && rg -n wake tools 2>/dev/null | wc -l\")"))
    (is (null (wake-call-verdict form)) (format nil "passes: ~a" form)))
  (is (null (wake-call-verdict "(sh ($ \"s\"))" :strings '(("s" . "git status")) :json t)) "a ($ key) read command passes from JSON arguments"))

(deftest cron-cell-wake-gate-refuses-other-tools-and-noise ()
  (is (search "only eval" (wake-call-verdict "(+ 1 2)" :name "report_issue")) "a tool other than eval is refused")
  (is (stringp (wake-call-verdict "(+ 1")) "an unreadable snippet is refused")
  (is (stringp (wake-call-verdict "(recall-log) (sh \"rm x\")")) "one acting form refuses the whole snippet"))

(defun wake-round (session cost &rest usage)
  "One turn of SESSION and one usage round on it: priced at COST, or
unpriced when COST is NIL; USAGE rides to RECORD-TURN-USAGE."
  (let ((turn (nlk:admit-turn session (nlk::make-durable-id "command") "x")))
    (apply #'nlk:record-turn-usage turn :provider "vendor" :model "m1" :input-tokens 10 :output-tokens 2
           (append (when cost (list :cost-known t :cost-usd cost)) usage))
    (nlk:complete-turn turn nil)))

(deftest cron-cell-wake-spent-24h-sums-the-wake-and-its-forks (with-temp-store ())
  (let ((now (+ (get-universal-time) 60)))
    (nlk:create-session :id "wake")
    (nlk:create-session :id "fork" :parent "wake")
    (nlk:create-session :id "other")
    (wake-round "wake" 1.0d0)
    (wake-round "wake" 0.5d0)
    (wake-round "fork" 0.25d0 :voided t)
    (wake-round "other" 7d0)
    (is (= 0 (nodecode-cron::wake-spent-24h "nobody" now)) "a session that ran nothing spent nothing")
    (multiple-value-bind (usd known unpriced rounds) (nodecode-cron::wake-spent-24h "wake" now)
      (is (and usd (< (abs (- usd 1.75d0)) 1d-6)) "the wake and its fork, the voided attempt paid for, the other session not")
      (is (and (eql known usd) (= unpriced 0) (= rounds 3)) "known cost and rounds ride as further values"))
    (is (= 0 (nodecode-cron::wake-spent-24h "wake" (+ now (* 2 86400)))) "rounds older than a day are out of the window")
    (is (= 0 (nodecode-cron::wake-spent-24h "wake" (- now 86400 3600))) "rounds after NOW are not yet spent")
    (wake-round "fork" nil)
    (multiple-value-bind (usd known unpriced) (nodecode-cron::wake-spent-24h "wake" now)
      (is (null usd) "a round with no price makes the spend unknown, never a smaller number")
      (is (and (< (abs (- known 1.75d0)) 1d-6) (= unpriced 1)) "the known part and the unpriced rounds still say how much is known"))))

(deftest cron-cell-wake-spent-24h-without-a-store-is-unknown ()
  (is (null (nodecode-cron::wake-spent-24h "wake" (get-universal-time))) "no store to read: unknown, not zero"))
