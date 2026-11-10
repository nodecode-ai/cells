# nodecode-zatca

ZATCA: Saudi Arabia's 15% VAT, split exactly, and the ZATCA Phase 2 (FATOORA) invoice fields a seller can check before clearance. A Nodecode rewrite of [qeemah-cloud/hermes-plugin-zatca](https://github.com/qeemah-cloud/hermes-plugin-zatca) by [qeemah-cloud](https://github.com/qeemah-cloud).

## Get it

Tell the agent "add the zatca cell", or run `nodecode add nc://zatca`.

## Set it up

Set your seller VAT number once and every invoice check fills it in: `seller_vat` is your 15-digit Saudi VAT registration number. Two more members are optional: `vat_rate_percent` (15 by default; a 5% invoice dated before July 2020 is why it is a setting) and `tolerance_halalas` (5 by default, the 0.05 SAR a VAT amount or a total may sit off the rule for line-level rounding). `config.example.jsonc` is the section written out. Nothing else is needed: no key, no account, no network.

## Use it

Two tools the model calls, and `/zatca` for you:

- `ksa_vat_calculate` takes `amount` in Saudi Riyals and `is_inclusive`, and answers the taxable amount, the VAT and the total. The split is done in halalas - SAR cents - with a half halala rounded up, so the three figures always add up.
- `zatca_validate_invoice` takes `uuid`, `seller_vat` (or the section's), `icv`, `total_without_vat`, `vat_amount` and `total_with_vat`, plus `issue_date` and `issue_time` when they are known, and answers which rules failed and how many ran.
- `/zatca 2500` answers what 15% adds to SAR 2,500; `/zatca 2300 inclusive` answers what SAR 2,300 holds of VAT; `/zatca check <uuid> <seller-vat> <icv> <net> <vat> <total> [date] [time]` checks a whole invoice.
- `(help :zatca)` is the manual the model reads for the full rules.

```
zatca: 15% VAT on SAR 2,500.00
  taxable amount  SAR 2,500.00
  VAT             SAR 375.00
  total           SAR 2,875.00
```

```
zatca: 1 of 7 ZATCA Phase 2 checks failed on invoice 8f2c1a44-3c7e-4f3f-9a2b-6d1e0f7c4b21:
  BR-KSA-06: seller VAT number 300000000000004 is not 15 digits starting and ending with 3
```

The rules it runs, each named by the code ZATCA gives it: BR-KSA-03 the UUID is a version 4 UUID; BR-KSA-06 the seller's VAT number is 15 digits starting and ending with 3; BR-KSA-14 the invoice counter value (ICV) is an integer of at least 1; BR-KSA-05 the issue date is a real calendar date written YYYY-MM-DD; BR-KSA-25 the issue time is written HH:MM:SS in Saudi Arabia (AST, UTC+3); BR-KSA-52 the VAT is the standard rate of the taxable amount; BR-KSA-53 the total is the taxable amount plus the VAT. The date and the time are checked only when the call carries them.

What it is not: it reads no invoice XML, no QR code and no cryptographic stamp, and it submits nothing to ZATCA - it is the field-level pre-check a seller can run before clearance, not clearance itself.

Security: no network, no subprocess, no file read or write, no key, no clock. The invoice values you pass are the invoice values it answers about, and they become part of the conversation like any tool result - which is the point.

## What is in it

| File | What it is |
| --- | --- |
| `vat.lisp` | The money in halalas, the VAT split, and the ZATCA field rules. |
| `cell.lisp` | The declaration: the contracts, the two tools, `/zatca`, `(help :zatca)`, the `zatca` section. |
| `nodecode-zatca.asd` | The system and its test system. |
| `test/` | The offline tests: the money and the rules, the tools through the wire, the command, the section. |
| `config.example.jsonc` | The section, written out as documentation. |

## License

MIT. The original is Copyright (c) 2026 Qeemah Cloud (قيمة): see `LICENSE` and `NOTICE`.
