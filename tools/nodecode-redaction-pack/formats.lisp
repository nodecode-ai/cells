;;; formats.lisp --- the vendor credential formats this cell masks, one row each.
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Every row is a TokenFormat the original pack carried: VENDOR,
;;; the TOKEN's name, the PATTERN that finds it, and the SOURCE the
;;; shape was taken from -- a gitleaks rule id (MIT), a TruffleHog
;;; detector (pkg/detectors/<name>) or the vendor's own documentation.
;;;
;;; The patterns are the original's, unchanged: each begins with a
;;; literal vendor prefix, carries no top-level |, no capturing group,
;;; no inline flag and no nested unbounded repeat, and spells its case
;;; out in character classes. The matcher wraps each in a boundary of
;;; its own (MATCH-TEXT), so a token inside a longer word is not one.

(in-package #:nodecode-redaction-pack)

(defstruct (token-format (:constructor make-token-format (vendor token pattern source)))
  "One row of the table: the vendor, the token's name, the pattern that finds it, and where the shape came from."
  (vendor "" :type string :read-only t)
  (token "" :type string :read-only t)
  (pattern "" :type string :read-only t)
  (source "" :type string :read-only t))

(defparameter *formats*
  (list
   (make-token-format
    "1Password" "account Secret Key"
    "A3-[A-Z0-9]{6}-(?:[A-Z0-9]{11}|[A-Z0-9]{6}-[A-Z0-9]{5})-[A-Z0-9]{5}-[A-Z0-9]{5}-[A-Z0-9]{5}"
    "gitleaks:1password-secret-key")
   (make-token-format
    "HashiCorp Vault" "service token"
    "hvs\\.[A-Za-z0-9_-]{90,120}"
    "gitleaks:vault-service-token")
   (make-token-format
    "HashiCorp Vault" "batch token"
    "hvb\\.[A-Za-z0-9_-]{138,300}"
    "gitleaks:vault-batch-token")
   (make-token-format
    "Doppler" "CLI, personal, service account, SCIM and audit tokens"
    "dp\\.(?:ct|pt|sa|said|scim|audit)\\.[A-Za-z0-9]{40,44}"
    "https://docs.doppler.com/reference/auth-token-formats")
   (make-token-format
    "Doppler" "service token"
    "dp\\.st\\.(?:[a-z0-9_-]{2,35}\\.)?[A-Za-z0-9]{40,44}"
    "https://docs.doppler.com/reference/auth-token-formats")
   (make-token-format
    "Amazon Bedrock" "long-term API key"
    "ABSK[A-Za-z0-9+/]{109,269}={0,2}"
    "gitleaks:aws-amazon-bedrock-api-key-long-lived")
   (make-token-format
    "Amazon Bedrock" "short-term API key"
    "bedrock-api-key-YmVkcm9jay5hbWF6b25hd3MuY29t[A-Za-z0-9+/]{20,}={0,2}"
    "gitleaks:aws-amazon-bedrock-api-key-short-lived")
   (make-token-format
    "Alibaba Cloud" "AccessKey ID"
    "LTAI[A-Za-z0-9]{20}"
    "gitleaks:alibaba-access-key-id")
   (make-token-format
    "DigitalOcean" "OAuth refresh token"
    "dor_v1_[a-f0-9]{64}"
    "gitleaks:digitalocean-refresh-token")
   (make-token-format
    "Fly.io" "access token"
    "fo1_[A-Za-z0-9_-]{43}"
    "gitleaks:flyio-access-token")
   (make-token-format
    "Fly.io" "macaroon token"
    "fm1[ar]_[A-Za-z0-9+/]{100,}={0,3}"
    "gitleaks:flyio-access-token")
   (make-token-format
    "Fly.io" "macaroon token (v2)"
    "fm2_[A-Za-z0-9+/]{100,}={0,3}"
    "gitleaks:flyio-access-token")
   (make-token-format
    "Heroku" "API key"
    "HRKU-AA[A-Za-z0-9_-]{58}"
    "gitleaks:heroku-api-key-v2")
   (make-token-format
    "OpenShift" "user token"
    "sha256~[A-Za-z0-9_-]{43}"
    "gitleaks:openshift-user-token")
   (make-token-format
    "Scalingo" "API token"
    "tk-us-[A-Za-z0-9_-]{48}"
    "gitleaks:scalingo-api-token")
   (make-token-format
    "Cloudflare" "global API key (cfk_ format)"
    "cfk_[A-Za-z0-9]{40}[a-f0-9]{8}"
    "trufflehog:cloudflareglobalapikey/v2")
   (make-token-format
    "Pulumi" "access token"
    "pul-[a-f0-9]{40}"
    "gitleaks:pulumi-api-token")
   (make-token-format
    "Infracost" "API key"
    "ico-[A-Za-z0-9]{32}"
    "gitleaks:infracost-api-token")
   (make-token-format
    "Databricks" "personal access token"
    "dapi[a-f0-9]{32}(?:-[0-9])?"
    "gitleaks:databricks-api-token")
   (make-token-format
    "PlanetScale" "service token, OAuth token and password"
    "pscale_(?:tkn|oauth|pw)_[A-Za-z0-9_.=-]{32,64}"
    "gitleaks:planetscale-api-token, planetscale-oauth-token, planetscale-password")
   (make-token-format
    "Supabase" "personal and OAuth access token"
    "sbp_(?:oauth_|v0_)?[a-f0-9]{40}"
    "https://github.com/supabase/cli (AccessTokenPattern, apps/cli-go/internal/utils/access_token.go)")
   (make-token-format
    "Tailscale" "API, auth, OAuth client, SCIM and webhook keys"
    "tskey-(?:api|auth|client|scim|webhook)-[A-Za-z0-9_]+-[A-Za-z0-9_]{16,}"
    "https://tailscale.com/kb/1277/key-prefixes; trufflehog:tailscale")
   (make-token-format
    "Hugging Face" "organization API token"
    "api_org_[A-Za-z]{34}"
    "gitleaks:huggingface-organization-api-token")
   (make-token-format
    "LangSmith" "personal access token and service key"
    "lsv2_(?:pt|sk)_[a-f0-9]{32}_[a-f0-9]{10}"
    "trufflehog:langsmith; prefixes per https://docs.langchain.com/langsmith/create-account-api-key")
   (make-token-format
    "Pinecone" "API key"
    "pcsk_[A-Za-z0-9]{5,6}_[A-Za-z0-9]{63}"
    "trufflehog:pinecone")
   (make-token-format
    "Weights & Biases" "API key (v1 format)"
    "wandb_v1_[A-Za-z0-9]{27}_[A-Za-z0-9]{49}"
    "trufflehog:weightsandbiases/v2")
   (make-token-format
    "Grafana" "Cloud API token"
    "glc_[A-Za-z0-9+/]{32,400}={0,3}"
    "gitleaks:grafana-cloud-api-token")
   (make-token-format
    "Grafana" "service account token"
    "glsa_[A-Za-z0-9]{32}_[A-Fa-f0-9]{8}"
    "gitleaks:grafana-service-account-token")
   (make-token-format
    "Sentry" "user auth token"
    "sntryu_[a-f0-9]{64}"
    "gitleaks:sentry-user-token")
   (make-token-format
    "Sentry" "organization auth token"
    "sntrys_eyJ[A-Za-z0-9+/]{30,400}={0,2}_[A-Za-z0-9+/]{43}"
    "gitleaks:sentry-org-token")
   (make-token-format
    "Dynatrace" "API token"
    "dt0c01\\.[A-Za-z0-9]{24}\\.[A-Za-z0-9]{64}"
    "gitleaks:dynatrace-api-token")
   (make-token-format
    "New Relic" "user API key"
    "NRAK-[A-Za-z0-9]{27}"
    "gitleaks:new-relic-user-api-key")
   (make-token-format
    "New Relic" "insert key"
    "NRII-[A-Za-z0-9_-]{32}"
    "gitleaks:new-relic-insert-key")
   (make-token-format
    "New Relic" "Insights query key"
    "NRIQ-[A-Za-z0-9_-]{25}"
    "trufflehog:newrelicinsightsquerykey")
   (make-token-format
    "PostHog" "personal API key"
    "phx_[A-Za-z0-9_]{43,48}"
    "trufflehog:posthog")
   (make-token-format
    "Rootly" "API key"
    "rootly_[a-f0-9]{64}"
    "trufflehog:rootly")
   (make-token-format
    "Postman" "API key"
    "PMAK-[a-fA-F0-9]{24}-[a-fA-F0-9]{34}"
    "gitleaks:postman-api-token")
   (make-token-format
    "Sourcegraph" "access token"
    "sgp_(?:[a-fA-F0-9]{16}_|local_)?[a-fA-F0-9]{40}"
    "gitleaks:sourcegraph-access-token")
   (make-token-format
    "Prefect" "API key"
    "pnu_[A-Za-z0-9]{36}"
    "gitleaks:prefect-api-token")
   (make-token-format
    "ReadMe" "API key"
    "rdme_[a-z0-9]{70}"
    "gitleaks:readme-api-token")
   (make-token-format
    "RubyGems" "API key"
    "rubygems_[a-f0-9]{48}"
    "gitleaks:rubygems-api-token")
   (make-token-format
    "Clojars" "deploy token"
    "CLOJARS_[A-Za-z0-9]{60}"
    "gitleaks:clojars-api-token")
   (make-token-format
    "JFrog Artifactory" "API key"
    "AKCp[A-Za-z0-9]{69}"
    "gitleaks:artifactory-api-key; trufflehog:artifactory")
   (make-token-format
    "JFrog Artifactory" "reference token"
    "cmVmdGtu[A-Za-z0-9]{56}"
    "trufflehog:artifactoryreferencetoken; gitleaks:artifactory-reference-token")
   (make-token-format
    "Typeform" "personal access token"
    "tfp_[A-Za-z0-9_.=-]{59}"
    "gitleaks:typeform-api-token")
   (make-token-format
    "Frame.io" "developer token"
    "fio-u-[A-Za-z0-9_=-]{64}"
    "gitleaks:frameio-api-token")
   (make-token-format
    "Docker Hub" "personal access token"
    "dckr_pat_[A-Za-z0-9_-]{27}"
    "trufflehog:dockerhub/v2")
   (make-token-format
    "Docker Hub" "organization access token"
    "dckr_oat_[A-Za-z0-9_-]{32}"
    "trufflehog:dockerhub/v2")
   (make-token-format
    "Figma" "personal access token"
    "figd_[A-Za-z0-9_-]{40}"
    "trufflehog:figmapersonalaccesstoken/v2")
   (make-token-format
    "Figma" "personal access token (figp_ format)"
    "figp_[A-Za-z0-9_=-]{40,54}"
    "trufflehog:figmapersonalaccesstoken/v3")
   (make-token-format
    "Slack" "app configuration refresh token"
    "xoxe-[0-9]-[A-Za-z0-9]{146}"
    "gitleaks:slack-config-refresh-token")
   (make-token-format
    "Atlassian" "API token (Jira, Confluence)"
    "ATATT[A-Za-z0-9+/=_-]+=[A-Za-z0-9]{8}"
    "trufflehog:jiratoken/v2")
   (make-token-format
    "Atlassian" "organization admin API key"
    "ATCTT3xFfG[A-Za-z0-9+/=_-]+=[A-Za-z0-9]{8}"
    "trufflehog:atlassian/v2")
   (make-token-format
    "Bitbucket Data Center" "HTTP access token"
    "BBDC-[A-Za-z0-9+/@_-]{40,50}"
    "trufflehog:atlassiandatacenter/bitbucketdatacenter")
   (make-token-format
    "CircleCI" "personal API token"
    "CCIPAT_[A-Za-z0-9]{22}_[a-fA-F0-9]{40}"
    "trufflehog:circleci/v2")
   (make-token-format
    "Buildkite" "user API access token"
    "bkua_[a-z0-9]{40}"
    "trufflehog:buildkite/v2")
   (make-token-format
    "SonarCloud" "token"
    "sqco_[A-Za-z0-9]{59}"
    "trufflehog:sonarcloud/v2")
   (make-token-format
    "Sourcegraph" "Cody gateway access token"
    "slk_[a-f0-9]{64}"
    "trufflehog:sourcegraphcody")
   (make-token-format
    "Contentful" "personal access token"
    "CFPAT-[A-Za-z0-9_-]{43}"
    "trufflehog:contentfulpersonalaccesstoken")
   (make-token-format
    "Apify" "API token"
    "apify_api_[A-Za-z0-9]{36}"
    "trufflehog:apify")
   (make-token-format
    "Adobe" "client secret"
    "p8e-[A-Za-z0-9]{32}"
    "gitleaks:adobe-client-secret")
   (make-token-format
    "Shopify" "access tokens and shared secret"
    "shp(?:at|ca|pa|ss)_[a-fA-F0-9]{32}"
    "gitleaks:shopify-access-token, -custom-access-token, -private-app-access-token, -shared-secret")
   (make-token-format
    "Square" "access token"
    "sq0atp-[A-Za-z0-9_-]{22,60}"
    "gitleaks:square-access-token")
   (make-token-format
    "Shippo" "API token"
    "shippo_(?:live|test)_[a-fA-F0-9]{40}"
    "gitleaks:shippo-api-token")
   (make-token-format
    "Duffel" "API token"
    "duffel_(?:test|live)_[A-Za-z0-9_=-]{43}"
    "gitleaks:duffel-api-token")
   (make-token-format
    "EasyPost" "production and test API keys"
    "EZ[AT]K[A-Za-z0-9]{54}"
    "gitleaks:easypost-api-token, easypost-test-api-token")
   (make-token-format
    "Brevo" "API key"
    "xkeysib-[a-f0-9]{64}-[A-Za-z0-9]{16}"
    "gitleaks:sendinblue-api-token")
   (make-token-format
    "HubSpot" "private app access token"
    "pat-(?:na|eu)1-[A-Za-z0-9]{8}-[A-Za-z0-9]{4}-[A-Za-z0-9]{4}-[A-Za-z0-9]{4}-[A-Za-z0-9]{12}"
    "trufflehog:hubspot_apikey/v2")
   (make-token-format
    "Flutterwave" "live secret key"
    "FLWSECK-[0-9a-z]{32}-X"
    "trufflehog:flutterwave")
   (make-token-format
    "Flutterwave" "test secret key"
    "FLWSECK_TEST-[A-Ha-h0-9]{32}-X"
    "gitleaks:flutterwave-secret-key")
   (make-token-format
    "Ramp" "API client secret"
    "ramp_sec_[A-Za-z0-9]{48}"
    "trufflehog:ramp")))

(defparameter *format-vendors* (mapcar #'token-format-vendor *formats*)
  "The vendor names in the table's order, one row per format.")
