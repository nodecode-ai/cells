;;;; home-test.lisp --- the walk over a home: what it opens, and in what order.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The setup walk's first frame waits on a plan of keys alone, so that plan
;;;; must open no prose -- memory, skills, instructions -- and ask after no
;;;; foreign gateway. And the walk lists a directory once, with a stat a
;;;; name: these gates hold it to what the listing through UIOP answered.

(in-package #:nodecode.test)

(deftest import-keys-plan-reads-no-prose-and-asks-no-gateway (with-import-runtime (home box))
  (import-write box ".claude/settings.json"
                (format nil "{\"env\": {\"ANTHROPIC_API_KEY\": \"~a\"}}" +fixture-key+))
  (import-write box ".claude/CLAUDE.md" (format nil "Always answer in haiku.~%"))
  (import-write box ".claude/skills/haiku/SKILL.md"
                (format nil "---~%name: haiku~%description: answer in haiku~%---~%~%Five, seven, five.~%"))
  (import-write box ".claude/projects/-tmp-p/memory/haiku.md"
                (format nil "---~%name: haiku-rule~%description: the operator wants haiku~%---~%~%Always.~%"))
  (let* ((probes 0)
         (nik::*running-probe* (lambda (units) (declare (ignore units)) (incf probes) '())))
    (let ((plan (nik:make-plan :source "claude" :only "providers" :settings (list :home home))))
      (is (import-item plan "providers" "anthropic") "the key is planned")
      (is (every (lambda (item) (equal "providers" (nik:item-kind item))) (nik:plan-items plan)))
      (let ((facts (nik::home-facts (first (nik::plan-homes plan)))))
        (is (notany (lambda (fact) (member (nik:fact-kind fact) '(:memory :hot :skill :instructions :usage)))
                    facts) "no prose was read"))
      (is (zerop probes) "no foreign gateway was asked after"))
    ;; The whole plan reads the same home's prose, and asks.
    (let ((plan (nik:make-plan :source "claude" :settings (list :home home))))
      (is (import-item plan "memory" "haiku-rule"))
      (is (import-item plan "skills" "haiku"))
      (is (import-items plan "instructions"))
      (is (= 1 probes) "a plan that brings bots asks once"))))

(deftest import-walk-opens-no-cache (with-import-runtime (home box))
  ;; What a harness fetched and keeps is no setting of the operator's, even
  ;; when it names a key: it is neither read nor listed as unmapped.
  (import-write box ".claude/settings.json"
                (format nil "{\"env\": {\"ANTHROPIC_API_KEY\": \"~a\"}}" +fixture-key+))
  (import-write box ".claude/plugins/plugin-catalog-cache.json"
                (format nil "{\"env\": {\"OPENROUTER_API_KEY\": \"~a\"}}" +fixture-key+))
  (import-write box ".claude/Models_Cache.json"
                (format nil "{\"env\": {\"OPENAI_API_KEY\": \"~a\"}}" +fixture-key+))
  (let* ((plan (nik:make-plan :source "claude" :only "providers" :settings (list :home home)))
         (walked (first (nik::plan-homes plan))))
    (is (import-item plan "providers" "anthropic") "the settings file's key is planned")
    (is (not (import-item plan "providers" "openrouter")) "a cache's key is not")
    (is (not (import-item plan "providers" "openai")) "whatever case the name is in")
    (is (notany (lambda (line) (search "cache" line :test #'char-equal)) (nik::home-unmapped walked))
        "and a cache is not the backlog")))

#-win32
(deftest import-walk-lists-a-directory-as-uiop-did (with-temp-directory (box "walk-box"))
  ;; A link counts as what it reaches, its own path kept; a link to nothing
  ;; is passed over; names sort as SBCL's DIRECTORY sorts namestrings, where
  ;; a subdirectory's ends in `/'.
  (let ((root (uiop:ensure-directory-pathname box)))
    (import-write root "elsewhere/x.json" "{}")
    (import-write root "d/b.json" "{}")
    (import-write root "d/a.json" "{}")
    (import-write root "d/.env" "A=1")
    (import-write root "d/abc/k.json" "{}")
    (import-write root "d/abc-d/k.json" "{}")
    (sb-posix:symlink (namestring (merge-pathnames "elsewhere/" root))
                      (namestring (merge-pathnames "d/linked" root)))
    (sb-posix:symlink (namestring (merge-pathnames "elsewhere/x.json" root))
                      (namestring (merge-pathnames "d/linked.json" root)))
    (sb-posix:symlink (namestring (merge-pathnames "nowhere.json" root))
                      (namestring (merge-pathnames "d/gone.json" root)))
    (let ((directory (merge-pathnames "d/" root)))
      (multiple-value-bind (files subdirectories)
          (nik::directory-entries (uiop:native-namestring directory))
        (is (equal (mapcar #'file-namestring (uiop:directory-files directory)) files))
        (is (equal (mapcar #'nlk:folder-name (uiop:subdirectories directory)) subdirectories))
        (is (equal '(".env" "a.json" "b.json" "linked.json") files))
        (is (equal '("abc-d" "abc" "linked") subdirectories))))))
