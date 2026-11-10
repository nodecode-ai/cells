# Building an affiliate screen inside your own product

Use this when the user is a developer adding an affiliate program to their own app or website, so their users
can share a link and see what they earned without leaving it.

## Which tools to use

- `enroll_affiliate` signs a user up by email (it finds or creates their Affiliateo account) and returns their
  referral links.
- `get_affiliate` (by email or ref_code) returns their links, lifetime stats and the pending, payable and paid
  money for that app.
- `get_app` returns the per-product commission rates.
- `list_affiliate_conversions` is the sales list: cursor pages (`limit`, `starting_after`), filtered on the
  server by date (`from`, `to`) and type (`type=refund,chargeback` and so on). Always filter on the server;
  filtering one fetched page misses older rows.

Do **not** use the `my_*` tools for this. They return whoever signed in or owns the API key, never one of the developer's
users.

## The three embedded Elements

Three screens are mounted from Affiliateo instead of built, because they handle bank details, the account-wide
wallet or an ID check:

- `withdraw`: cash out
- `payouts`: withdrawal history
- `identity`: the ID check

Mint a session with `call_api` on `POST .../affiliates/embed-session` and name only the components you need.
Everything else (link, QR, stats, MRR, balance, sales list) is built from the tools above, in the developer's
own design. SDKs: `@affiliateo/elements-react`, `@affiliateo/elements-react-native`, Swift and Kotlin. Docs:
https://affiliateo.com/docs/elements

## A layout that works

For an affiliate section inside an existing product, one scrolling page: referral link and QR at the top,
then lifetime stats, then earnings with the sales list, then a Cash out button that opens `withdraw` in a
dialog. Keep the element mounted and hide it when the dialog closes, so the person does not have to confirm
again each time.

## Words to use

The API returns data, not labels. Affiliateo's own strings for these screens, in 16 languages, are at
`https://affiliateo.com/locales/{lang}.json` (keys under `embed.*`). Copy the values into the app at build
time rather than fetching them live. Keep the money words exact: Pending, Payable, Paid.

- `churn_rate` is null until five people have subscribed: show a dash.
- `revenue_cents` is gross: label it Gross revenue.
