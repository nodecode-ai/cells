;;;; cell-test.lisp --- the tool slimmer against a stubbed ingress and prompt build.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test dials anything: the two seams this cell advises are funcalled by
;;;; hand, with a NEXT that answers what the test chose, so what is under test
;;;; is the part the cell owns -- the ranking of a request's words against a
;;;; tool's name and description, and the vector it hands back to the build.

(in-package #:nodecode.test)

(define-test-slice "tool-slimmer" "TOOL-SLIMMER-CELL-" :start nodecode-tool-slimmer:start-cell)

(define-cell-lifecycle-tests "tool-slimmer"
  (:hooks 'nle:submit 'nle::turn-prompt)
  (:help :tool-slimmer)
  (:command "tool-slimmer")
  (:running
   (is (nle::find-tool "tool_slimmer_select") "the ranking tool is registered")
   (is (nle::find-tool "tool_slimmer_status") "and the report")
   (is (nle::find-tool "tool_slimmer_request_full_tools") "and the way back to every schema")
   (is (= 8 (nodecode-tool-slimmer::setting :top-k)) "the section's default is eight")
   (is (nodecode-tool-slimmer::setting :fail-open) "and a request it cannot judge fails open"))
  (:stopped
   (is (null (nle::find-tool "tool_slimmer_select")) "and stop takes every one back")
   (is (null (nle::find-tool "tool_slimmer_status"))))
  (:refused ("top_k" 1)))

;;; --- a request's tool vector, as the provider carries it ---------------------

(defun tool-slimmer-cell-tools ()
  "Four tool entries shaped as a request's vector carries them: what the
ranking reads is the name and the description, nothing else."
  (vector (list :name "read_file" :description "read a file from disk")
          (list :name "web_search" :description "search the web for pages")
          (list :name "cron_add" :description "schedule a prompt to run on a schedule")
          (list :name "tool_slimmer_status" :description "what the last request carried")))

(defun tool-slimmer-cell-build (build session tools)
  "The prompt build for SESSION, answered by a NEXT that offers TOOLS: the
tool vector the build hands the provider back, as a list of names."
  (multiple-value-bind (base sections slimmed)
      (funcall build (lambda (id bindings)
                        (declare (ignore id bindings))
                        (values "BASE" '() tools))
               session nil)
    (declare (ignore base sections))
    (map 'list #'nodecode-tool-slimmer::tool-name slimmed)))

;;; --- the trim ---------------------------------------------------------------

(deftest tool-slimmer-cell-trims-a-request-to-its-own-words ()
  (with-cell-stop ((tool-slimmer-start))
    (let ((ingress (cell-hook "nodecode-tool-slimmer" 'nle:submit))
          (build (cell-hook "nodecode-tool-slimmer" 'nle::turn-prompt)))
      (is (functionp ingress) "the ingress is advised")
      (is (functionp build) "and the prompt build")
      (funcall ingress (lambda (session prompt &rest keys)
                          (declare (ignore keys))
                          (list session prompt))
               "slimmer-1" "please add a cron job for the nightly release")
      (let ((names (tool-slimmer-cell-build build "slimmer-1" (tool-slimmer-cell-tools))))
        (is (member "cron_add" names :test #'equal) "the tool the words ask for is carried")
        (is (member "tool_slimmer_status" names :test #'equal) "and the pinned one rides every request")
        (is (not (member "web_search" names :test #'equal)) "a tool the request does not need is left out")
        (is (= 2 (length names)) "two schemas where the catalog had four")))))

(deftest tool-slimmer-cell-carries-everything-it-cannot-judge ()
  (with-cell-stop ((tool-slimmer-start))
    (let* ((build (cell-hook "nodecode-tool-slimmer" 'nle::turn-prompt))
           (tools (tool-slimmer-cell-tools))
           (unknown (tool-slimmer-cell-build build "never-submitted" tools)))
      (is (= 4 (length unknown)) "a session whose words the cell never saw carries every schema")
      (is (equal '("tool_slimmer_status") (tool-slimmer-cell-build build "never-submitted" (vector (elt tools 3)))) "and a lone tool is never trimmed away"))))

(deftest tool-slimmer-cell-hands-every-tool-back-once-when-asked ()
  (with-cell-stop ((tool-slimmer-start))
    (let* ((build (cell-hook "nodecode-tool-slimmer" 'nle::turn-prompt))
           (tools (tool-slimmer-cell-tools)))
      (setf (gethash "slimmer-2" nodecode-tool-slimmer::*prompts*) "add a cron job")
      (setf (gethash "slimmer-2" nodecode-tool-slimmer::*full-sessions*) t)
      (is (= 4 (length (tool-slimmer-cell-build build "slimmer-2" tools))) "the asked-for request carries every schema")
      (is (= 2 (length (tool-slimmer-cell-build build "slimmer-2" tools))) "and the next one is trimmed again"))))

;;; --- the tools and the command ---------------------------------------------

(deftest tool-slimmer-cell-tools-answer ()
  (with-cell-stop ((tool-slimmer-start))
    (let ((answer (nle::message-content
                   (execute-wire-call "slimmer-call-1" "tool_slimmer_select"
                                      "{\"query\": \"schedule a prompt every morning\", \"tools\": [\"read_file: read a file from disk\", \"cron_add: run a prompt on a schedule\"]}"))))
      (is (search "cron_add" answer) "the scheduling tool ranks first")
      (is (search "left_out\": [\"read_file\"]" answer) "and the other is named as left out"))
    (let ((answer (nle::message-content (execute-wire-call "slimmer-call-2" "tool_slimmer_status" "{}"))))
      (is (search "\"top_k\": 8" answer) "status reads the section back")
      (is (search "\"running\": true" answer) "and says it is running"))
    (let ((answer (nle::message-content
                   (execute-wire-call "slimmer-call-3" "tool_slimmer_request_full_tools"
                                      "{\"reason\": \"the cron tool was not offered\"}"))))
      (is (search "every tool schema" answer) "the model can ask for every schema")
      (is nodecode-tool-slimmer::*full-next* "and the next request carries them"))))

(deftest tool-slimmer-cell-command-answers ()
  (with-cell-stop ((tool-slimmer-start))
    (is (nle::find-registered-command "tool-slimmer") "/tool-slimmer is in the one catalog")
    (multiple-value-bind (text dialog)
        (nodecode-tool-slimmer::run-command "" "slimmer-3")
      (declare (ignore dialog))
      (is (search "tool_slimmer_status" text) "the report names what every request keeps"))
    (multiple-value-bind (text dialog)
        (nodecode-tool-slimmer::run-command "full" "slimmer-3")
      (declare (ignore dialog))
      (is (search "every tool schema" text) "/tool-slimmer full asks for all of them")
      (is (gethash "slimmer-3" nodecode-tool-slimmer::*full-sessions*) "and it marks this session's next request"))))
