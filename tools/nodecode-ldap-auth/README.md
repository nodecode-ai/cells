# nodecode-ldap-auth

LDAP Auth: signs a user in against an LDAP or Active Directory directory by an LDAP bind - the directory verifies the password, and the cell mints a signed session. A Nodecode rewrite of [angel12/hermes-ldap-auth](https://github.com/angel12/hermes-ldap-auth) by [angel12](https://github.com/angel12).

## Get it

Tell the agent "add the ldap-auth cell", or run `nodecode add nc://ldap-auth`.

## Set it up

The `ldap-auth` section of the config needs `server_url` and exactly one bind mode. Direct bind, when the DN shape is known (no service account):

```jsonc
{
  "ldap-auth": {
    "server_url": "ldaps://ldap.example.com",
    "user_dn_template": "uid={username},ou=people,dc=example,dc=com",
    "secret": "<32 random bytes, base64: openssl rand -base64 32>"
  }
}
```

Search-then-bind, needed for Active Directory `sAMAccountName` logins and whenever the session should carry the entry's mail and display name:

```jsonc
{
  "ldap-auth": {
    "server_url": "ldaps://dc01.corp.example.com",
    "bind_dn": "cn=svc-nodecode,ou=services,dc=corp,dc=example,dc=com",
    "bind_password": "the service account's password",
    "user_search_base": "ou=staff,dc=corp,dc=example,dc=com",
    "user_search_filter": "(sAMAccountName={username})",
    "require_group": "cn=nodecode-users,ou=groups,dc=corp,dc=example,dc=com",
    "secret": "<32 random bytes, base64: openssl rand -base64 32>"
  }
}
```

`config.example.jsonc` is the whole section, every key commented. The original's rules are the cell's: the two bind modes are mutually exclusive, `user_dn_template` and `user_search_filter` must each carry `{username}`, plain `ldap://` is refused unless `start_tls` or `allow_insecure` says otherwise, and `secret` must be at least 16 bytes when it is set. Without `secret` a key is generated per process: sessions then work, but not across a restart.

## Use it

- `ldap_login` with a `username` and a `password` answers the verified identity (DN, mail, display name, and the group when one is required) and a signed access and refresh token, or one line saying the directory rejected the pair - the same line for an unknown user and a wrong password, so a login endpoint tells nothing apart.
- `ldap_verify` with a `token` gives the identity it stands for, checking the signature and the expiry with the directory untouched; with `verify_user_on_refresh` on and search-then-bind configured, a session whose account has since been deleted or moved is refused.
- `ldap_lookup` with a `username`, and `/ldap-auth <username>`, show what the directory holds for a name (DN, mail, display name, group) with no password involved. `/ldap-auth` alone says how the cell is set up.
- `(help :ldap-auth)` is the manual the model reads when it wants the full rules.

```
ldap_login {"username": "jdoe", "password": "..."}
ldap-auth: verified by bind as uid=jdoe,ou=people,dc=example,dc=com
  name: Jane Doe
  mail: jdoe@example.com
session: access 12 h (until 2026-11-10T20:14:03Z), refresh 720 h
access token: eyJzdWIi...
```

What it holds to, from the original: an empty password is refused before any bind (LDAP reads one as an anonymous bind, which always succeeds); usernames are escaped before they reach a DN template or a filter; a filter that matches more than one entry rejects the login; unknown user and wrong password cost about the same and read the same; referrals are never chased, so credentials are never replayed to a host the directory names; TLS is required unless `allow_insecure` is set, and certificates are always verified, with `ca_certs_file` for a private bundle; `timeout_seconds` bounds a connect and a read.

The session is a stateless signed token, as in the original: nothing server-side to revoke, and a merely disabled account keeps refreshing until its token expires - shorten `refresh_ttl_seconds`, or delete or move the entry, for a hard cutoff. `ldap_login` sends the password to the configured directory in a bind and nowhere else; it is not stored, hashed or written to a log. The token is part of the answer the model reads, so it lives in the conversation history.

## What is in it

| File | What it is |
| --- | --- |
| `ber.lisp` | LDAPv3 over BER: the requests, the replies read back, the two escapes. |
| `wire.lisp` | The connection (usocket, cl+ssl, StartTLS), bind and search, the group probe, and the three verbs' directory half. |
| `token.lisp` | The stateless session: base64url, HMAC-SHA256, sign and check. |
| `cell.lisp` | The declaration: the `ldap-auth` section and its rules, `ldap_login`, `ldap_lookup`, `ldap_verify`, `/ldap-auth`, `(help :ldap-auth)`. |
| `nodecode-ldap-auth.asd` | The system and its test system. |
| `test/` | The offline tests: a scripted directory answers every bind and search. |
| `config.example.jsonc` | The section, written out as documentation. |

## License

MIT. The original is Copyright (c) 2026 angel12: see `LICENSE` and `NOTICE`.
