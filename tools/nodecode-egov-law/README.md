# nodecode-egov-law

Japanese laws from e-Gov: find a statute by the name a person uses, read one article as in force today or on a date, and check when an amendment takes effect. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-egov-law](https://github.com/TakeshiTGAL/hermes-plugin-egov-law) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

The data is the Law API v2 of [e-Gov 法令検索](https://laws.e-gov.go.jp/), the Digital Agency of Japan's official law database: public, read-only, no key. Answers carry the credit e-Gov's terms ask for (`source.attribution`), the law's own e-Gov page, and — for an article — the date the version in force took effect.

## Get it

Tell the agent "add the egov-law cell", or run `nodecode add nc://egov-law`.

## Set it up

Nothing is required. The section `egov-law` in `~/.nodecode/config.jsonc` has three members, all optional:

| Member | Default | What it does |
| --- | --- | --- |
| `endpoint` | `https://laws.e-gov.go.jp/api/2` | the API base |
| `timeout_seconds` | `20` | one request's ceiling |
| `max_chars` | `4000` | how much of one article `egov_law_article` returns by default |

## Use it

Ask in the conversation — 「労基法32条の条文を見せて」, 「個人情報保護法の次の改正はいつ施行？」, 「民法第四百十五条」 — and the model calls one of the three tools:

| Tool | When | Arguments |
| --- | --- | --- |
| `egov_law_search` | you do not know the exact law, or want laws whose text mentions a phrase | `query`, `mode` (`title` or `text`), `limit`, `include_repealed` |
| `egov_law_article` | you need one article's wording, or whether an amendment changes it | `law`, `article`, `paragraph`, `as_of`, `compare_with`, `max_chars` |
| `egov_law_revisions` | the question is about when something took or takes effect | `law`, `limit` |

A law is named any way a person names it: official title (労働基準法), e-Gov abbreviation (労基法), a common short name (`下請法`, `憲法`, 18 of them), law number (昭和二十二年法律第四十九号) or law ID (322AC0000000049). An article is written `32`, `第32条`, `三十二条`, `32の2`, or `附則第143条` for the law's own supplementary provisions. A name that fits several laws answers the candidates with their law IDs.

The operator can also read from the shell:

```
/egov-law 労基法 32      one article, as text, with its e-Gov page
/egov-law 個人情報保護法  the laws whose title matches
```

and from a Lisp call: `(egov-law:article "労基法" "32")` answers the same object the tool returns. `(help :egov-law)` carries the primer a model reads, including the rule to quote `source.attribution` with any law text.

## What it does not do

- Bills still in the Diet are not in e-Gov, so no scheduled amendment is not proof there is none.
- It reads the main provisions (本則) and the law's own 附則, not 別表, forms, or the 附則 of an amending law.
- The original's two convenience notes are not carried: it does not hunt an article that an amendment moved to another number, and it does not point at an 附則 article that restates the one you read. Ask for `附則` articles yourself when the answer depends on a transitional rule.
- No key, no account, nothing written to disk: every request goes to `laws.e-gov.go.jp`.

## Test it

```sh
.github/test-cell.sh tools/nodecode-egov-law
```

The run stubs every `nlk:http` call, so it needs no network: the three tools, the element name an article is read by, the operator's command and the section's refusal are all checked against documents shaped like the API's.
