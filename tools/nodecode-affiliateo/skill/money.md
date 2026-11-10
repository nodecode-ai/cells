# Money on Affiliateo

All amounts are integer cents (USD unless a field says otherwise). Show them as normal currency.

## Paying affiliates

1. `get_payouts` is the dashboard's Pay tab: every affiliate (and Live Chat closer) with what they are owed,
   most owed first, plus the business wallet balance. `payable_cents` is exactly what a pay run sends.
2. Tell the owner who gets paid and how much in total, and wait for a yes.
3. `pay_affiliates` with `affiliate_ids` from step 1, or `all: true`. Money leaves the owner's wallet and each
   person is told.

This needs an API key with its **Payouts** switch on (a sign-in never shows `pay_affiliates`), and each run
counts against the key's daily limit. Past the limit
nobody is paid (`PAYOUT_LIMIT_EXCEEDED`): tell the owner to pay the rest from the dashboard or raise the limit.
`stopped_early: true` means call it again for the rest.

The three buckets, in the words to use:

- **Pending**: earned, not released yet.
- **Payable**: released and ready to pay.
- **Paid**: already in the affiliate's Affiliateo balance, not yet in their bank. Avoid the words "paid out".

## Revenue and customers

- `get_revenue_overview`: active trials, active subscriptions, MRR, 28-day revenue, new and active customers,
  each with a 28-day line. Narrow with `app` or `source=affiliate|ad|organic`.
- `list_revenue_transactions`: every sale, renewal, trial, refund and subscription change.
- `get_revenue_chart`: ARR, subscriptions, churn, trial conversion and more, period by period.
- A customer is a person, not a device. `find_customers` looks one up by any id (Affiliateo's, the app's user
  id, a store or Stripe id) and `get_customer` shows them whole.
- Revenue figures are gross, before store and payment fees.

## Invoices

`create_invoice` emails a bill to one person **at once** (there is no draft). Confirm who, how much and what
for before calling it. The person pays through the link with no account needed; paying makes them a member and
credits any affiliate on the bill. A subscription invoice charges again every interval until the owner
(`stop_invoice_subscription`) or the payer stops it. `void_invoice` kills an unpaid link and emails nobody.

## Products and plans

- `get_product` before `update_product`, always.
- A product has up to four plans and a sales page made of elements; the tool descriptions list every element.
- Re-pricing or removing a plan that has subscribers cancels them, so `update_product` refuses until
  `confirm_subscriber_cancellation: true`. Tell the owner how many people it affects first.
- Files and images go up with `create_upload` first; pass the returned upload key.
