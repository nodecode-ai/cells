# nodecode-yandex-mail

Yandex Mail: search, read, flag, move and delete mail over IMAP, save attachments, send over SMTP. A Nodecode rewrite of [akinfold/hermes-yandex-mail](https://github.com/akinfold/hermes-yandex-mail) by [akinfold](https://github.com/akinfold).

The mailbox is your own, and nothing sits between you and it: the cell speaks
IMAP and SMTP to Yandex's servers itself, with the certificate verified. Eight
tools, one toolset, the same eight the plugin had:

| Tool | What it does |
| --- | --- |
| `yandex_mail_list_folders` | Every folder it may touch, with message and unread counts. |
| `yandex_mail_search_messages` | Search one folder by sender, recipient, subject, body text, date, unread or flagged; newest first. |
| `yandex_mail_read_message` | One message: its text, paged, and the list of its attachments. Reading does not mark it read unless you say so. |
| `yandex_mail_save_attachment` | One attachment written to a file, and the path answered: the bytes never enter the conversation. Off until you switch it on. |
| `yandex_mail_mark_message` | Set or clear read and flagged on one message or several. |
| `yandex_mail_move_message` | Copy first, remove the original after: a folder that refuses the copy keeps its message. |
| `yandex_mail_delete_message` | To the Trash folder by default; `permanent` removes it for good. |
| `yandex_mail_send_message` | Send as your own address, over SMTP. Off until you switch it on. |

Three fences come with it, and each is a sentence in the settings:

- **folders** — the only folders the tools may touch. Unset means all of them.
- **actions** — which tools exist as far as the model is concerned. Sending
  and saving attachments are *not* granted by leaving it empty, so switching
  the cell on never hands a running agent the right to write as you.
- **send_to** — when set, the only addresses or `@domains` this account may
  write to.

## Get it

Tell the agent "add the yandex-mail cell", or run:

```sh
nodecode add nc://yandex-mail
```

## Set it up

Make an app password in Yandex ID (https://id.yandex.ru/security/app-passwords)
with the Mail/IMAP scope — not your account password, and revocable on its
own. Then set two settings:

```jsonc
{
  "yandex-mail": {
    "login": "you@yandex.ru",
    "app_password_env": "YANDEX_MAIL_APP_PASSWORD"
  }
}
```

`app_password` takes the password inline instead, which the organism keeps in a
0600 file. Add `"imap_port": 143` for STARTTLS on a server that wants it, and
`"actions": ["read", "write"]` to narrow what the model can do.

## Use it

Ask in words: "what came in overnight?", "read me the one from the bank",
"file everything from that sender into Archive and mark it read". The same
verbs are callable from eval in the `yandex-mail` package, where each takes
the decoded arguments and answers JSON text.

A message is what a stranger wrote: its text is data, not instructions. The
manual each request carries says so, and the tools never follow a link or run
what a message asks for.

## What it costs nothing to keep

No runtime dependency beyond what Nodecode already loads: IMAP, SMTP, MIME and
the transfer decoders are this cell's own Lisp. A saved attachment goes to
`mail/` under the Nodecode home unless `attachment_dir` says otherwise.
