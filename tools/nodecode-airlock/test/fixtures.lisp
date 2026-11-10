;;;; fixtures.lisp --- one allow and one deny, signed the way an Airlock receipt is:
;;;; RFC 8785 canonical JSON, SHA-256, Ed25519.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Generated once from fixed test data and kept as bytes, so the tests need no
;;;; network, no key generation and no second implementation of the canonical
;;;; form: what they check is that this cell arrives at the same bytes. The
;;;; private seed is public test data, not a secret, and nothing trusts it.

(in-package #:nodecode.test)

(defparameter +allow-public-key+ "03a107bff3ce10be1d70dd18e74bc09967e4d6309ba50d5f1ddc8664125531b8"
  "The Ed25519 key this fixture's receipts verify under: public test data.")


(defparameter +allow-step+ "{\"action\":{\"code\":\"echo hello\",\"id\":\"act-0001\"},\"execution\":{\"exit_code\":0,\"stderr\":\"\",\"stdout\":\"hello\\n\"},\"reason\":\"matched rule allow-echo\",\"receipt\":{\"action_id\":\"act-0001\",\"decision\":{\"action_id\":\"act-0001\",\"reason\":\"matched rule allow-echo\",\"rules_fired\":[\"allow-echo\"],\"verdict\":\"allow\"},\"execution\":{\"exit_code\":0,\"report_invalid\":false,\"stderr_sha256\":\"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855\",\"stderr_truncated\":false,\"stdout_sha256\":\"5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03\",\"stdout_truncated\":false},\"hash\":\"6dab692d55ccc6cd5fe79124b9925ac1d34c65891a5f340dc39a486a95833e59\",\"prev_hash\":null,\"receipt_v\":1,\"schema_v\":1,\"sig_ed25519\":\"50be1d25a487b81d55fd786c605082a2bf5029fc0c7cce23c6efebb37210d25d2571736277ab89fbcec5ae1b2d8f7ac3103f11da3c65ce3066b3275b1ec59406\",\"task_id\":\"nc-airlock-fixture\"},\"receipt_id\":\"6dab692d55ccc6cd5fe79124b9925ac1d34c65891a5f340dc39a486a95833e59\",\"rules_fired\":[\"allow-echo\"],\"status\":\"executed\",\"verdict\":\"allow\"}"
  "One allow arena answer, its receipt signed over its canonical bytes.")

(defparameter +deny-public-key+ "03a107bff3ce10be1d70dd18e74bc09967e4d6309ba50d5f1ddc8664125531b8"
  "The Ed25519 key this fixture's receipts verify under: public test data.")


(defparameter +deny-step+ "{\"action\":{\"code\":\"curl https://example.test\",\"id\":\"act-0001\"},\"reason\":\"rule block-curl matched\",\"receipt\":{\"action_id\":\"act-0001\",\"decision\":{\"action_id\":\"act-0001\",\"reason\":\"rule block-curl matched\",\"rules_fired\":[\"block-curl\"],\"verdict\":\"deny\"},\"hash\":\"70c8751379450e40fc1a478f95edc0356b11a3878cdb1ee4e05a23900a444600\",\"prev_hash\":null,\"receipt_v\":1,\"schema_v\":1,\"sig_ed25519\":\"ebcd28ba41b172426aaa61d625d7c6e568498e95438455b5c4f5656982c192c30a20f92f1146b1634137b333c05326a37162b64d0f597b85f0a34c50b3065c0c\",\"task_id\":\"nc-airlock-fixture\"},\"receipt_id\":\"70c8751379450e40fc1a478f95edc0356b11a3878cdb1ee4e05a23900a444600\",\"rules_fired\":[\"block-curl\"],\"status\":\"blocked\",\"verdict\":\"deny\"}"
  "One deny arena answer, its receipt signed over its canonical bytes.")
