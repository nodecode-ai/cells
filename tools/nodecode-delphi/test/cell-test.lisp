;;;; cell-test.lisp --- lifecycle, the blocks, the arithmetic, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Gensyn: the config
;;;; surface, every tool through the real dispatch seam against a stubbed
;;;; API, subgraph and node, the exact-decimal edge that needs no network at
;;;; all, and the refusals a missing key and a missing row get.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "delphi"
  (:config "network" "testnet")
  (:help :delphi)
  (:command "delphi")
  (:running
   (is (nle::find-tool "delphi_list_markets") "delphi_list_markets is registered")
   (is (nle::find-tool "delphi_compute_edge") "delphi_compute_edge is registered")
   (is (nle::find-tool "delphi_query_subgraph") "delphi_query_subgraph is registered")
   (is (nle::find-tool "delphi_quote_buy") "delphi_quote_buy is registered")
   (is (equal "testnet" (nodecode-delphi::setting :network)) "the section's network is the one in force"))
  (:stopped
   (is (null (nle::find-tool "delphi_list_markets")) "stop unregisters the tools"))
  (:refused
   ("network" "devnet")
   ("timeout_seconds" 0))
  (:idle nodecode-delphi:delphi-error
   (nodecode-delphi:markets)))

;;; --- a page of markets ---------------------------------------------------------------

(deftest delphi-cell-lists-markets-with-the-key (with-delphi ()
  (let ((text (delphi-call-tool "delphi_list_markets" "{\"limit\":5}")))
    (is (cl:search "Will the network ship in May?" text) "the question the API sent")
    (is (cl:search "open" text) "and its status")
    (is (cl:search "Yes / No" text) "its outcomes")
    (is (cl:search "fee 1%" text) "the trading fee, 10^16 raw, as a percent")
    (is (cl:search "https://testnet.delphi.fyi/market/11111111" text) "and its own page")
    (is (cl:search "not an instruction" text) "with the line saying whose text it is"))
  (let ((call (first *calls*)))
    (is (equal "key-1" (call-header call "x-api-key")) "the key rides as x-api-key")
    (is (cl:search "/markets?limit=5&skip=0" (getf call :url)) "on the SDK's own route, paged")
    (is (null (call-header call "x-delphi-mode")) "and without the competition's header"))
  (is (= 1 (length *calls*)) "one request, nothing else")))

(deftest delphi-cell-refuses-without-a-key (with-delphi ()
  (let ((*key* nil))
    (let ((condition (signals-error nodecode-delphi:delphi-error
                                    (nodecode-delphi:markets))))
      (is (cl:search "needs a Delphi read API key" (princ-to-string condition)) "the refusal says what is missing")
      (is (cl:search +key-env+ (princ-to-string condition)) "and names the variable that carries it"))
    (is (null *calls*) "and nothing was dialled"))))

(deftest delphi-cell-refuses-an-argument-it-cannot-read (with-delphi ()
  (let ((condition (signals-error nodecode-delphi:delphi-error
                                  (nodecode-delphi:markets :limit 500))))
    (is (cl:search "limit must be an integer from 1 to 50" (princ-to-string condition)) "a limit outside its range refuses"))
  (is (null *calls*) "before any request")))

;;; --- one market -------------------------------------------------------------------

(deftest delphi-cell-reads-one-market (with-delphi ()
  (let ((text (nodecode-delphi:market "0x1111111111111111111111111111111111111111")))
    (is (cl:search "Market 0x1111111111111111111111111111111111111111 on testnet" text) "the market it read")
    (is (cl:search "0. Yes" text) "outcome 0 by index")
    (is (cl:search "1. No" text) "and outcome 1")
    (is (cl:search "settles 2026-05-01T00:00:00Z" text) "when it settles"))
  (is (cl:search "/markets/0x1111111111111111111111111111111111111111" (first (call-urls))) "its own route")))

(deftest delphi-cell-says-a-market-the-api-does-not-list (with-delphi ()
  (let ((condition (signals-error nodecode-delphi:delphi-error
                                  (nodecode-delphi:market "0x2222222222222222222222222222222222222222"))))
    (is (cl:search "no such row (404)" (princ-to-string condition)) "the 404 is a refusal a model can read"))))

(deftest delphi-cell-refuses-an-address-that-is-not-one (with-delphi ()
  (let ((condition (signals-error nodecode-delphi:delphi-error (nodecode-delphi:market "0x1234"))))
    (is (cl:search "0x and 40 hex characters" (princ-to-string condition)) "the refusal shows the shape"))
  (is (null *calls*) "and nothing was dialled")))

;;; --- positions --------------------------------------------------------------------

(deftest delphi-cell-reads-the-sections-wallet (with-delphi ("wallet_address" "0x3333333333333333333333333333333333333333")
  (let ((text (nodecode-delphi:positions)))
    (is (cl:search "Positions of 0x3333333333333333333333333333333333333333 on testnet: 1" text) "the wallet the section holds")
    (is (cl:search "shares 3.5 (3500000000000000000 raw)" text) "and its shares, raw and as a decimal")
    (is (cl:search "redeemed no" text) "with the redeemed flag"))
  (is (cl:search "wallet=0x3333333333333333333333333333333333333333" (first (call-urls))) "asked for by address")))

(deftest delphi-cell-refuses-positions-with-no-wallet (with-delphi ()
  (let ((condition (signals-error nodecode-delphi:delphi-error (nodecode-delphi:positions))))
    (is (cl:search "wallet must be an address" (princ-to-string condition)) "no wallet, no read"))))

;;; --- the edge, offline -----------------------------------------------------------

(deftest delphi-cell-computes-an-exact-edge (with-delphi ()
  (let ((text (nodecode-delphi:edge "0.62" :implied "0.45")))
    (is (cl:search "0.17 above" text) "0.62 against 0.45 is 0.17 above, exactly")
    (is (cl:search "not investment advice" text) "and says what it is"))
  (is (cl:search "0.25 below" (nodecode-delphi:edge "0.5" :implied "0.75")) "a lower probability reads below")
  (is (cl:search "= 0 equal" (nodecode-delphi:edge "0.5" :implied "0.5")) "the same reads equal")
  (is (null *calls*) "no request: the argument form is arithmetic alone")))

(deftest delphi-cell-refuses-a-probability-that-is-not-one (with-delphi ()
  (dolist (bad '("1.5" "1e-7" "-0.2" "0.1234567890123456789" "half"))
    (is (signals-error nodecode-delphi:delphi-error (nodecode-delphi:edge bad :implied "0.5"))
        (format nil "~s is not a probability" bad)))))

(deftest delphi-cell-needs-one-way-to-know-the-implied-probability (with-delphi ()
  (let ((condition (signals-error nodecode-delphi:delphi-error (nodecode-delphi:edge "0.5"))))
    (is (cl:search "needs implied_probability" (princ-to-string condition)) "neither given: a refusal says which"))))

(deftest delphi-cell-reads-the-markets-implied-probability (with-delphi ()
  (let ((text (nodecode-delphi:edge "0.7" :market-id "0x1111111111111111111111111111111111111111" :outcome-idx 1)))
    (is (cl:search "= 0.1 above" text) "0.7 against the gateway's 0.6 for outcome 1")
    (is (cl:search "read from the market itself" text) "and says the number came from the market"))
  (let ((posts (remove-if-not (lambda (call) (cl:search "alchemy" (getf call :url))) *calls*)))
    (is (cl:search (nodecode-delphi::selector "spotImpliedProbabilities(address,uint256[])") (getf (first posts) :content)) "asked of the gateway that owns the market"))))

(deftest delphi-cell-refuses-an-outcome-the-market-does-not-have (with-delphi ()
  (let ((condition (signals-error nodecode-delphi:delphi-error
                                  (nodecode-delphi:edge "0.7" :market-id "0x1111111111111111111111111111111111111111" :outcome-idx 5))))
    (is (cl:search "outside this market's 2 outcome" (princ-to-string condition)) "an index past the outcomes refuses"))))


;;; --- history, from the subgraph ----------------------------------------------------

(deftest delphi-cell-reads-a-markets-trades (with-delphi ()
  (let ((text (nodecode-delphi:history "trades" :market-address "0x1111111111111111111111111111111111111111")))
    (is (cl:search "1 buy, 0 sells" text) "the page's two lists, counted")
    (is (cl:search "buy outcome 0" text) "the buy by outcome")
    (is (cl:search "shares 2.5 (2500000000000000000 raw)" text) "its shares, raw and as a decimal")
    (is (cl:search "tokens 1.5 (1500000000000000000 raw)" text) "and what it paid"))
  (let ((post (first (call-posts))))
    (is (cl:search "goldsky" (getf post :url)) "the subgraph's own endpoint")
    (is (cl:search "gatewayBuys" (getf post :content)) "asked in GraphQL for the buys"))))

(deftest delphi-cell-reads-the-subgraphs-meta (with-delphi ()
  (let ((text (nodecode-delphi:history "meta")))
    (is (cl:search "block 18934221" text) "how far the indexer has read the chain")
    (is (cl:search "indexing errors: no" text) "and whether it is healthy"))
  (is (cl:search "_meta" (getf (first (call-posts)) :content)) "asked for the meta field")))

(deftest delphi-cell-reads-a-settlement (with-delphi ()
  (let ((text (nodecode-delphi:history "settlement"
                                       :market-address "0x1111111111111111111111111111111111111111")))
    (is (cl:search "1 settlement, 0 failures, 0 resolution requests" text) "the three lists, counted")
    (is (cl:search "winning outcome 1" text) "how the market resolved"))
  (let ((condition (signals-error nodecode-delphi:delphi-error
                                  (nodecode-delphi:history "settlement"
                                                           :market-address "0x1111111111111111111111111111111111111111"
                                                           :skip 5))))
    (is (cl:search "no offset" (princ-to-string condition)) "settlement has no offset, as the original said"))))

(deftest delphi-cell-refuses-history-that-names-no-market (with-delphi ()
  (let ((condition (signals-error nodecode-delphi:delphi-error (nodecode-delphi:history "trades"))))
    (is (cl:search "needs market_address" (princ-to-string condition)) "trades without a market refuse"))
  (is (null *calls*) "before anything is dialled")))

;;; --- quotes, from the node -------------------------------------------------------

(deftest delphi-cell-quotes-a-buy (with-delphi ()
  (let ((text (nodecode-delphi:quote-trade "buy" "0x1111111111111111111111111111111111111111" 0 "3.5")))
    (is (cl:search "Quote to buy 3.5 shares of outcome 0" text) "the trade quoted")
    (is (cl:search "cost 0.42 (420000000000000000 raw)" text) "what it costs, in collateral")
    (is (cl:search "per share 0.12" text) "and per share")
    (is (cl:search "not a probability" text) "with the caveat the original gave"))
  (let ((posts (call-posts)))
    (is (= 3 (length posts)) "the factory's claim, the quote, and the collateral's decimals")
    (is (cl:search (nodecode-delphi::selector "quoteBuyExactOut(address,uint256,uint256)") (getf (second posts) :content)) "the gateway's own quote function"))))

(deftest delphi-cell-quotes-a-sell (with-delphi ()
  (let ((text (nodecode-delphi:quote-trade "sell" "0x1111111111111111111111111111111111111111" 1 "2")))
    (is (cl:search "proceeds 0.3 (300000000000000000 raw)" text) "what selling pays")
    (is (cl:search "per share 0.15" text) "per share"))
  (is (cl:search (nodecode-delphi::selector "quoteSellExactIn(address,uint256,uint256)") (getf (second (call-posts)) :content)) "the gateway's own sell function")))

(deftest delphi-cell-refuses-a-quote-it-cannot-make (with-delphi ()
  (let ((condition (signals-error nodecode-delphi:delphi-error
                                  (nodecode-delphi:quote-trade "buy" "0x1111111111111111111111111111111111111111" 0 "0"))))
    (is (cl:search "shares must be decimal text greater than 0" (princ-to-string condition)) "no shares, no quote"))
  (let ((condition (signals-error nodecode-delphi:delphi-error
                                  (nodecode-delphi:quote-trade "hold" "0x1111111111111111111111111111111111111111" 0 "1"))))
    (is (cl:search "buy or sell" (princ-to-string condition)) "and only a buy or a sell is a side"))
  (is (null *calls*) "nothing was dialled")))

;;; --- the competition's own data ----------------------------------------------------

(deftest delphi-cell-asks-for-the-competition-by-header (with-delphi ("network" "competition-testnet")
  (nodecode-delphi:markets :limit 5)
  (is (equal "competition" (call-header (first *calls*) "x-delphi-mode")) "competition-testnet is the same API, told apart by the header")))

;;; --- /delphi ---------------------------------------------------------------------

(deftest delphi-cell-the-command-answers (with-delphi ()
  (let ((text (cell-entry "nodecode-delphi" "delphi" "edge 0.6 0.4")))
    (is (cl:search "0.2 above" text) "/delphi edge answers the same edge"))
  (let ((bare (cell-entry "nodecode-delphi" "delphi" "")))
    (is (cl:search "delphi: testnet network" bare) "with no argument it says where it reads")
    (is (cl:search "key set" bare) "and whether it has a key"))))


