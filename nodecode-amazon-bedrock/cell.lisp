;;;; cell.lisp --- the cell: Amazon Bedrock among the organism's providers, on a lane of its own.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A lane and three hooks, each declining for every provider but this one:
;;;;
;;;;   the amazon-bedrock lane  Converse Stream: the request, SigV4 or a
;;;;                            bearer, the binary event stream (wire.lisp)
;;;;   MODELS-CATALOG-TABLE     the catalog carries Bedrock's row: this lane's
;;;;                            package, the region's runtime host, omp's
;;;;                            bundled models over whatever models.dev
;;;;                            published (whose own package no lane speaks)
;;;;   LIST-PROVIDER-MODELS     the listing is the roster: Bedrock's own is a
;;;;                            signed control-plane call
;;;;   :CREDENTIAL              a Bedrock API key from AWS_BEARER_TOKEN_BEDROCK,
;;;;                            else the marker a round turns into a SigV4
;;;;                            signature over the AWS credential chain; a key
;;;;                            /connect saved answers before this point does
;;;;
;;;; omp's login for Bedrock is no login: an API key, or the AWS chain
;;;; (credentials.lisp). The region and profile are the section's settings,
;;;; else AWS's own variables and files.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "amazon-bedrock": {"region": "us-west-2", "profile": "work"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-amazon-bedrock)

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Bedrock's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Bedrock's row, made once per
catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun listing (next provider &rest keys &key key &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: Bedrock's listing is its roster. Asked with
KEY, which only /connect's key check does, it says why the key was not
checked."
  ;; The roster asks Bedrock nothing, and the check reads a NIL second value
  ;; as a key Bedrock took: any key read `works'.
  (if (equal provider +provider+)
      (values (listing-rows)
              (and key "Bedrock's model list is a signed AWS call this cell does not make, so the first turn tries the key"))
      (apply next provider keys)))

(defun credential (op next)
  "The :CREDENTIAL answer for amazon-bedrock: the bearer
AWS_BEARER_TOKEN_BEDROCK holds, else the marker a round signs with SigV4."
  ;; Never NEXT for this provider: the lane's family is its own, and the
  ;; core's environment ladder knows only its four. A probe (no endpoint)
  ;; answers the marker only when a source is configured on this machine,
  ;; asking no network; a round answers it always, since an instance role
  ;; or a container may answer.
  (if (equal (getf op :provider) +provider+)
      (cond ((env +bearer-env+) (nle:make-credential (env +bearer-env+) :env))
            ((or (getf op :endpoint) (aws-source-p)) (nle:make-credential "aws-sigv4" :aws))
            (t (nle:make-credential "public" :public)))
      (funcall next op)))

(defun start ()
  "Register the lane; on stop take it out, drop the merged catalog and every
cached credential."
  (setf *catalog* (cons nil nil))
  (forget-credentials)
  (let ((lane (make-lane)))
    (nle::register-provider-lane lane)
    (nle:on-stop (lambda ()
                   (setf nle::*provider-lanes* (remove lane nle::*provider-lanes*))
                   (setf *catalog* (cons nil nil))
                   (forget-credentials)))))

(nle:define-cell amazon-bedrock
  (:section ("amazon-bedrock")
    (:guide "use the AWS credential chain (environment keys, a profile in ~/.aws, SSO, a role, a container or an instance role), or a Bedrock API key saved with /connect or AWS_BEARER_TOKEN_BEDROCK; region and profile pick where and as whom")
    ("region" :string
     :doc "the Bedrock region (else AWS_REGION, AWS_DEFAULT_REGION, the profile's, or the model's geo)")
    ("profile" :string :doc "the AWS profile in ~/.aws/config and ~/.aws/credentials (else AWS_PROFILE, else default)")
    ("base_url" :string
     :doc "a runtime origin of its own (a VPC endpoint, a gateway), used verbatim; AWS's own regional host follows the region")
    ("guardrail_identifier" :string :doc "a Bedrock guardrail id or ARN every request carries")
    ("guardrail_version" :string :doc "the guardrail's version (DRAFT when left out)")
    ("guardrail_trace" :choice :options '("enabled" "disabled" "enabled_full")
     :doc "the guardrail trace (Bedrock's default when left out)"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::list-provider-models #'listing)
  (:hook :credential #'credential))
