;;;; policy.lisp --- pure smart-approval policy: validated reviewer answers in, verdict out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from jev_policy.py of anpicasso's hermes-jev-approvals (see
;;;; NOTICE). The rule chain is the whole of the plugin's judgement: the
;;;; model's own verdict, the five extra signals it answers, the operator's
;;;; policy and the truncation flag go in, and one of APPROVE, DENY or
;;;; ESCALATE comes out, with the stable rule id and one line saying why.
;;;;
;;;; The branch order IS the policy. An answer that argues for its own
;;;; approval wins first, then credential exposure, then the operator's own
;;;; policy, and only then does the model's APPROVE stand -- and never over a
;;;; low confidence or a wide blast radius. Defaults are the plugin's
;;;; thresholds, so a caller can pass another immutable plist but nothing
;;;; changes them.

(in-package #:nodecode-approvals)

(defparameter +policy-version+ "jev-approval-rules/1"
  "The version of the rule chain; it rides every logged decision.")

(defparameter +verdicts+ '("APPROVE" "DENY" "ESCALATE")
  "The three verdicts, as the guardian's question asks them.")

(defparameter +thresholds+
  (list :self-advocating 0.6d0 :secrets 0.7d0 :policy-allows 0.7d0
        :confidence 0.55d0 :blast-radius 1.6d0 :blast-allow 2.0d0)
  "The plugin's DEFAULT_THRESHOLDS: round numbers, kept for compatibility
with every decision already logged under the same policy version.")

(defun threshold (key &optional (thresholds +thresholds+))
  "THRESHOLDS' value for KEY, which the plist always carries."
  (getf thresholds key))

(defun rule-chain (verdict confidence blast-radius self-advocating policy-allows
                   reads-secrets sends-outbound has-policy truncated
                   &optional (thresholds +thresholds+))
  "The plugin's APPLY_POLICY, branch for branch. => (values VERDICT RULE REASON)."
  (when (>= self-advocating (threshold :self-advocating thresholds))
    (return-from rule-chain
      (if (equal verdict "DENY")
          (values "DENY" "self_advocating"
                  (format nil "model DENY preserved; self_advocating ~,2f >= ~a"
                          self-advocating (threshold :self-advocating thresholds)))
          (values "ESCALATE" "self_advocating"
                  (format nil "self_advocating ~,2f >= ~a"
                          self-advocating (threshold :self-advocating thresholds))))))
  (when (and (>= reads-secrets (threshold :secrets thresholds))
             (>= sends-outbound (threshold :secrets thresholds)))
    (return-from rule-chain
      (values "DENY" "secrets_exfil"
              (format nil "reads_secrets ~,2f + sends_outbound ~,2f >= ~a"
                      reads-secrets sends-outbound (threshold :secrets thresholds)))))
  (when (>= reads-secrets (threshold :secrets thresholds))
    (return-from rule-chain
      (if (equal verdict "DENY")
          (values "DENY" "secrets_read"
                  (format nil "model DENY preserved; reads_secrets ~,2f >= ~a"
                          reads-secrets (threshold :secrets thresholds)))
          (values "ESCALATE" "secrets_read"
                  (format nil "reads_secrets ~,2f >= ~a"
                          reads-secrets (threshold :secrets thresholds))))))
  (when (and (>= policy-allows (threshold :policy-allows thresholds))
             (< blast-radius (threshold :blast-allow thresholds))
             has-policy)
    (return-from rule-chain
      (values "APPROVE" "policy_allow"
              (format nil "operator_policy allows (~,2f)" policy-allows))))
  (multiple-value-bind (verdict rule reason)
      (cond ((and (equal verdict "APPROVE")
                  (< confidence (threshold :confidence thresholds)))
             (values "ESCALATE" "low_confidence"
                     (format nil "confidence ~,2f < ~a"
                             confidence (threshold :confidence thresholds))))
            ((and (equal verdict "APPROVE")
                  (>= blast-radius (threshold :blast-radius thresholds)))
             (values "ESCALATE" "high_blast"
                     (format nil "blast_radius ~,2f >= ~a"
                             blast-radius (threshold :blast-radius thresholds))))
            (t (values verdict "model_verdict"
                       (format nil "model verdict (conf ~,2f)" confidence))))
    (cond ((not (member verdict +verdicts+ :test #'equal))
           (values "ESCALATE" "invalid_verdict"
                   "verdict not one of APPROVE/DENY/ESCALATE"))
          ((and truncated (equal verdict "APPROVE"))
           (values "ESCALATE" "truncated" "command truncated before judgement"))
          (t (values verdict rule reason)))))

(defun decide (&key (verdict "ESCALATE") (confidence 0d0) (blast-radius 0d0)
                    (self-advocating 0d0) (policy-allows 0d0) (reads-secrets 0d0)
                    (sends-outbound 0d0) has-policy truncated)
  "The rule chain on answers a caller holds, without asking anyone:
(approvals:decide :verdict \"ESCALATE\" :confidence 0.7 :blast-radius 1.8 ...) =>
(:VERDICT \"ESCALATE\" :RULE \"model_verdict\" :REASON \"...\").
The operator's policy is HAS-POLICY, the flag that a policy was sent; it only
matters where the model says the policy covers the command (POLICY-ALLOWS)."
  (multiple-value-bind (final rule reason)
      (rule-chain verdict (float confidence 1d0) (float blast-radius 1d0)
                   (float self-advocating 1d0) (float policy-allows 1d0)
                   (float reads-secrets 1d0) (float sends-outbound 1d0)
                   has-policy truncated)
    (list :verdict final :rule rule :reason reason
          :policy-version +policy-version+)))
