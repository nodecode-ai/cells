# Ads on Affiliateo

Affiliateo reads and runs ads on six networks: Meta, TikTok, Google Ads, Apple Search Ads, Snapchat and X.
`list_businesses` says which are connected for each business.

## How did my ads do?

1. If today's numbers matter, call `sync_ad_stats` first (it pulls fresh numbers; it is heavy, so at most a
   few times a minute).
2. `get_ad_stats` gives spend, clicks, installs, revenue and ROAS per campaign, ad group and ad for one
   network over a date range (default: the last 30 days).
3. Spend and revenue are **micros**: divide by 1,000,000 for normal currency.
4. When you report results, show the whole funnel the numbers allow: clicks, installs or visits, trials,
   paying customers, with the rate between each step. Customers are people, counted once across devices.

## Where is my budget wasted?

`get_ad_stats` only has daily totals. To split spend by placement, device, age, gender, country, hour or
publisher, use `get_ad_breakdown`:

- Call it with no `dimension` first: it lists what each network can split by today.
- Pass `campaignId`, `adGroupId` or `adId` to look inside one object. Read `scopeVerified` in the answer
  before trusting a deep split.
- Each bucket's `key` is the network's own value, which is what you would exclude in targeting.

For Apple Search Ads keywords, `get_apple_keyword_economics` gives spend, installs, estimated cost per
customer and lifetime value per keyword and country.

## Changing what runs

- `update_ad_entity` pauses, resumes, re-budgets or re-bids a campaign, ad group, keyword or ad. Resuming
  starts real spend: only on the user's clear yes.
- `get_ad_basic_targeting` / `set_ad_basic_targeting` read and change devices, ages and gender in one shape on
  every network, leaving the rest of the targeting alone.
- `add_apple_keywords` adds search terms as keywords, paused by default.

## Building a new campaign

1. Upload the creative first: `call_api` with `POST .../ads/media` and `{"kind":"image","url":"https://..."}`
   (or a video).
2. `launch_ad_campaign` builds the whole stack (campaign, ad set or ad group, creative, ad) in one call and
   rolls back if any step fails. It is always created **paused**. Its description lists each network's
   objectives: read it before choosing one, because every network names them differently.
3. Show the user what was built. Resume with `update_ad_entity` when they say so.
4. For anything the launch has no field for (a second ad set, detailed targeting, a schedule):
   `describe_ad_fields` for the field names, `list_ad_lookups` or `list_ad_audiences` for the ids those
   fields take, then `write_ad_object` to create or edit one object.

On Meta, Advantage+ creative enhancements are on by default and can crop, add music or reword an ad. When the
user is testing a creative, tell them to turn those off so the test is fair.

## Network quirks worth knowing

- **X**: no lead ads, no product sales ads, no embeddable preview and no billing balance exist in X's API, so
  those calls are refused on purpose. A 401 from X means the advertiser revoked access: they reconnect, you
  do not retry. X ids are case sensitive.
- **TikTok**: a missing-permission error usually means an old token. The owner reconnects TikTok once.
- **Google**: a video is uploaded to YouTube as unlisted for you and takes a while to process before it can be
  used.
- Disconnecting a network (`disconnect_ad_network`) stops syncing only. Campaigns keep running on the network.
