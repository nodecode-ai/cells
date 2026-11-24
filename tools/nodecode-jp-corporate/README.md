# nodecode-jp-corporate

JP Corporate: look a Japanese corporation up in gBizINFO, the registry of Japan's Ministry of
Economy, Trade and Industry, by name or 法人番号, and read its record. A Nodecode rewrite of
[TakeshiTGAL/hermes-plugin-jp-corporate](https://github.com/TakeshiTGAL/hermes-plugin-jp-corporate)
by [TakeshiTGAL](https://github.com/TakeshiTGAL).

## Get it

Tell the agent "add the jp-corporate cell", or run `nodecode add nc://jp-corporate`.

## Set it up

gBizINFO's REST API is free but answers only with a token of your own. Apply once at
<https://info.gbiz.go.jp/hojin/various_registration/form> (申請区分「トークン利用(WebAPI・データダウンロード)」),
open the link in the 「Web API 利用申請完了」 email, and keep that one token: the terms forbid extra
tokens made to get round usage limits.

Then either export it, or write it in the cell's section of `~/.nodecode/config.jsonc`:

```jsonc
{
  "jp-corporate": {
    "api_key": "...",          // the token from the email
    "timeout_seconds": 20
  }
}
```

`GBIZINFO_API_TOKEN` in the environment is read either way; `api_key_env` names another variable
when the token is kept somewhere else. `/jp-corporate` alone says whether a token is set.

## Use it

The model calls two tools:

- `jp_corp_search` — find a corporation by `name` (partial match; 株式会社, (株), ㈱, half-width
  kana and full-width letters are folded first), narrow by `address`, or look one up by
  `corporate_number` (13 digits; an invoice number such as `T1180301018771` also works). Given a
  number plus a name or an address, the answer says whether they match the official record.
- `jp_corp_profile` — one corporation's record by number, section by section (each section is one
  request): `basic`, `subsidy`, `procurement`, `commendation`, `certification`, `finance`,
  `patent`, `workplace`, `offices`.

From the shell, `/jp-corporate トヨタ自動車` runs one search, `/jp-corporate 1180301018771` looks one
number up, and `/jp-corporate` alone reports the endpoint and whether the token is set.

Every answer carries the credit line gBizINFO's terms ask for (出典：「Gビズインフォ」（経済産業省）).
`registered` means the registration is not closed, not that the business is operating; the registry
holds corporations, not sole proprietors; and 0 results is not proof that a company does not exist.
It does not check qualified-invoice-issuer (適格請求書発行事業者) registration — that is
<https://www.invoice-kohyo.nta.go.jp/>.
