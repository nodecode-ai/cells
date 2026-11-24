;;;; catalog.lisp --- the 66 Laso tools, as the MCP server at /mcp names them.

;;;; SPDX-License-Identifier: MIT

;;;; Generated from the original plugin's tools.json: the name each tool is called by on
;;;; the server (the plugin's laso_ prefix dropped, exactly as its handler did), whether the
;;;; plugin asked the human to approve the call before it ran, and the tool's own one-line
;;;; title. Descriptions of the arguments live on the server and are read with
;;;; laso_tools "<name>", which forwards a docs search.

(in-package #:nodecode-laso)

(defparameter +catalog+
  '(
  ("get_version" NIL "Get the current docs version")
  ("refresh_token" NIL "Refresh an ID token")
  ("order_card" T "Order a USA prepaid card")
  ("get_card_data" NIL "Get card details")
  ("list_card_transactions" NIL "List reloadable card transactions")
  ("get_card_deposit_address" NIL "Get the reloadable card deposit address")
  ("get_saved_cards" NIL "List the holder's saved cards")
  ("create_saved_card_link" NIL "Get a link for the holder to save their own card")
  ("buy_with_saved_card" NIL "Shop and pay with the holder's saved card")
  ("get_purchase_turn" NIL "Read a purchase turn that was still running")
  ("get_purchase_conversation" NIL "Read a purchase conversation's orders")
  ("create_reloadable_card" T "Create a reloadable card")
  ("get_account_balance" NIL "Get account balance")
  ("get_kyc_status" NIL "Get KYC verification status (optional)")
  ("get_kyc_link" NIL "Get a KYC verification link (optional)")
  ("register_webhook" T "Register a notification webhook (free)")
  ("get_webhook" NIL "Get webhook registration & delivery health (free)")
  ("delete_webhook" T "Delete the notification webhook (free)")
  ("withdraw" T "Withdraw from account balance")
  ("get_withdrawal_status" NIL "Get withdrawal statuses")
  ("refresh_card_data" NIL "Trigger a card data refresh")
  ("order_intl_card" T "Order an international prepaid card")
  ("cancel_intl_order" T "Cancel a queued international card order")
  ("search_merchants" NIL "Search merchant spend data")
  ("start_signup" NIL "Start a signup for a human with no Laso account")
  ("get_signup_status" NIL "Wait for your human to finish the signup")
  ("get_auth_link" NIL "Get a login link for the web dashboard")
  ("send_feedback" NIL "Send feedback about the API")
  ("list_address_book" NIL "List saved recipients")
  ("save_address_book_entry" NIL "Save or rename a recipient")
  ("delete_address_book_entry" T "Remove a saved recipient")
  ("push_to_card" T "Send money to a debit card (USD, EUR, GBP)")
  ("send_payment" T "Send a payment via Venmo or PayPal")
  ("fund_card_balance" T "Load a reloadable card balance")
  ("withdraw_card_balance" T "Withdraw from a reloadable card balance")
  ("list_payment_recipients" NIL "List saved Venmo and PayPal recipients (free)")
  ("update_payment_recipient" NIL "Rename or archive a saved recipient (free)")
  ("delete_payment_recipient" T "Remove a saved recipient (free)")
  ("get_payment_status" NIL "Check Venmo and PayPal payout status (free)")
  ("send_bank_payment" T "Send dollars to a bank account")
  ("search_gift_cards" NIL "Search the gift card catalog")
  ("order_gift_card" T "Order a gift card")
  ("create_banking_profile" NIL "Create the banking profile")
  ("get_banking_profile_status" NIL "Check the banking profile status")
  ("get_banking_application" NIL "Read the banking application")
  ("update_banking_application_details" NIL "Answer application questions")
  ("upload_banking_document" NIL "Upload a proof-of-address document")
  ("submit_banking_application" NIL "Submit the application")
  ("create_banking_account" NIL "Open an on-ramp or off-ramp account")
  ("list_banking_accounts" NIL "List banking accounts")
  ("create_banking_recipient" NIL "Create a payout recipient")
  ("add_banking_destination" NIL "Attach bank details to a recipient")
  ("list_banking_recipients" NIL "List recipients and destinations")
  ("update_banking_destination" NIL "Rename a destination")
  ("delete_banking_recipient" T "Delete a recipient")
  ("get_banking_transaction" NIL "Read one transaction")
  ("list_banking_transactions" NIL "List transactions")
  ("exchange_api_key" NIL "Exchange an API key for a session")
  ("get_agent_spend_limit" NIL "Read the per-payment spend limit")
  ("create_agent_api_key" T "Issue an API key")
  ("announce_connection" NIL "Call this FIRST, before any other Laso tool. Your human is wat")
  ("get_agent_wallet" NIL "Get your managed agent wallet: whether one exists, its Solana ")
  ("create_agent_wallet" NIL "Create your managed agent wallet (one per account; idempotent)")
  ("agent_wallet_transfer" T "Transfer USDC out of your managed agent wallet to any Solana a")
  ("pay_x402_endpoint" T "Pay ANY external x402-paywalled endpoint from your managed age")
  ("search_docs" NIL "Search the Laso Finance agent docs (SKILL.md and the full API "))

  "Every tool the Laso MCP server answers, in the original's order: (NAME APPROVAL-P TITLE).
The 19 a human approved before are the ones that move money or mint a credential.")
(defparameter +approval-count+
  (count-if #'second +catalog+)
  "How many of the tools move money or change the account: the original's 19.")

(defparameter +tool-names+ (mapcar #'first +catalog+)
  "The names LASO_CALL accepts.")
