# nodecode-airlock

Airlock Sandbox: send a command through your own Airlock policy gate, which
decides allow or deny, runs what it allows in a disposable sandbox instead of
this machine, and signs a receipt this cell checks under a key you pinned
locally. A Nodecode rewrite of
[justrunme/hermes-airlock-plugin](https://github.com/justrunme/hermes-airlock-plugin)
by [justrunme](https://github.com/justrunme).

In Hermes the plugin swaps the terminal the agent's tools run in, so every
`sh`, file and `execute_code` call goes through Airlock. Here it is one tool,
`airlock_run`, and one set of verbs: this machine's shell is untouched, and
what goes through the gate is the command you meant to isolate.

Every answer opens with the one word it earned:

- **VERIFIED** — the receipt's hash and Ed25519 signature checked out under
  the key this section pins, and the stdout, stderr and exit status the gate
  returned matched the digests and the integer the receipt signed.
- **ALLOWED** — the gate allowed it and it ran, but nothing signed it as far
  as this cell knows (`verify_receipts` is off, so read it as a report, not
  as proof).
- **BLOCKED** — the policy denied it and nothing ran, or the receipt did not
  bind and this cell refused to trust what the gate sent back.

## Get it

Tell the agent "add the airlock cell", or run `nodecode add nc://airlock`.

## Set it up

The cell is the client half. The other half is an Airlock deployment of your
own — the policy engine, the disposable sandbox and the signing key live in
[justrunme/airlock](https://github.com/justrunme/airlock). There is **no
built-in host**: nothing is dialled until you name one.

- `base_url` — the https origin of your Airlock, with no path: what the
  plugin's `base_url` was. A redirect is refused, so the command body is
  never replayed onto another origin.
- `public_key_hex` — the Ed25519 public key receipts are checked under, 64
  lowercase hex characters, obtained from the operator of that Airlock
  out-of-band. Never fetched from the server at verify time: a host that
  could forge a receipt could serve a matching key.
- `verify_receipts` — default on. On, a receipt that does not hash or verify
  fails closed, and `public_key_hex` is required. Off, answers read ALLOWED.
- `language` — `bash` (default) or `python`, the arena's language field.
- `timeout_s` — how long one arena call may take, default 60.
- `token_env` — the name of an environment variable holding the API token of
  a private Airlock; or `token`, pasted into `/settings`, which keeps it in a
  0600 file. The token rides as a bearer header and nowhere else.

Set the two required ones from the settings panel, `/settings`, or by
editing the `airlock` section of `~/.nodecode/config.jsonc`;
[`config.example.jsonc`](config.example.jsonc) is that section, commented.

## Use it

Ask the model to run something through the gate, or call the verbs from eval:

```lisp
(airlock:run "printf hello")        ; => (:status "VERIFIED" :exit-code 0 :stdout "hello" ...)
(airlock:run "cat input.txt" :input "hi")  ; input is a sandbox file, not stdin
(airlock:health)                    ; => the gate's own health object
(airlock:receipt "<64 hex hash>")   ; => one receipt, fetched and checked
(airlock:last-receipt)             ; => the last receipt this cell verified
(airlock:verify-text "{...}")       ; => a receipt an operator pasted, checked
```

`/airlock` answers the gate's health and the key this machine pins;
`/airlock last` the last receipt; `/airlock <hash>` one receipt by id.

What the rewrite is honest about, as the plugin was:

- **The signed receipt does not carry the command text.** VERIFIED binds the
  decision and the execution, not which command string was evaluated. The
  gate's outer `action.code` is compared to what was sent when present, as a
  consistency check only.
- **Each arena call is a fresh sandbox.** Airlock's arena has no cwd field,
  so a command starts where the sandbox starts; say where to go in the
  command. Files do not survive between calls.
- **`input` is a file, not standard input.** Airlock writes it to the
  sandbox as `input.txt`; nothing is piped to the process.
- **Hash-chain linkage is not verified.** `prev_hash` is shown as
  information: a shared Airlock interleaves other clients, so a chain audit
  is Airlock's own offline `verify-chain` job.

Run `.github/test-cell.sh tools/nodecode-airlock` from the cells repository.
The tests are offline: every exchange is stubbed, and the two receipts they
verify are real SHA-256 and Ed25519 over real canonical JSON, so the
verification path runs end to end without a network.
