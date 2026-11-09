# nodecode-protonmail

Proton Mail: folders, inbox, a decrypted message, attachments, the trash, and sending — straight from Proton's own API without Proton Bridge. A Nodecode rewrite of [Maytrella007/hermes-proton](https://github.com/Maytrella007/hermes-proton) by [Maytrella007](https://github.com/Maytrella007).

The cell signs in the way Proton's own web client does (an SRP handshake built
here, over `ironclad`), decrypts each message with the `gpg` already on the
machine, and sends the way Proton's client sends: the body signed, encrypted
once under a session key, that key wrapped for every recipient inside Proton
and handed over in the clear for everyone else. gpg does the cryptography, on a
throwaway keyring the cell makes and deletes for itself.

## Get it

Tell the agent "add the protonmail cell", or run `nodecode add nc://protonmail`.

## Set it up

```jsonc
"protonmail": {
  "username": "someone@proton.me",
  "password_env": "PROTON_PASSWORD"   // or "password": "…"
}
```

The password is used once per session: to sign in, and to unlock the account's
private keys. It is kept in memory only; no session file is written. Add
`"totp_env": "PROTON_TOTP"` when the account answers a login with a
two-factor challenge. `"host"` points the cell at another Proton region;
`"gpg_program"` names a gpg that is not on the PATH.

## Use it

| Tool | What it does |
| --- | --- |
| `proton_mail_folders` | Every folder and label, with its unread count. |
| `proton_mail_inbox` | A folder's messages, newest first: subject, sender, time, unread, attachments. `unread_only`, `keyword`, `from`, `subject` and `limit` narrow it. |
| `proton_mail_read` | One message, its body decrypted and read as prose. Reading does not mark it read. |
| `proton_mail_mark` | Set or clear the read flag. |
| `proton_mail_trash` | Move messages to the trash, or remove them for good with `permanent`. |
| `proton_mail_attachment` | One attachment decrypted and written to the path you name: the bytes never enter the conversation. |
| `proton_mail_send` | Send as the account's own address: end-to-end to Proton addresses, in the clear to the rest. `reply_to` threads a reply. |

A message is what a stranger wrote: the cell's manual tells the model to treat
its text as data, never as instructions.
