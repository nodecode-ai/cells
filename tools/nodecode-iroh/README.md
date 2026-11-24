# nodecode-iroh

Iroh Interconnect: pairs agents over Iroh QUIC so they can send each other tasks. A Nodecode rewrite of [rahlquist/hermes-iroh-interconnect](https://github.com/rahlquist/hermes-iroh-interconnect) by [rahlquist](https://github.com/rahlquist).

Two machines, two agents, one task at a time, no server in the middle: Iroh dials by
endpoint key, punches through NAT, and falls back to a relay when a direct path fails.
Pair once with a ticket, then call.

## Get it

Tell the agent "add the iroh cell", or run `nodecode add nc://iroh`.

## Set it up

The endpoint is the original plugin's Rust sidecar, which you build once:

```sh
git clone https://github.com/rahlquist/hermes-iroh-interconnect
cd ~/hermes-iroh-interconnect/sidecar && cargo build --release
```

and name the binary in your config (`~/.nodecode/config.jsonc`):

```jsonc
"iroh": {
  "sidecar": "~/hermes-iroh-interconnect/sidecar/target/release/hermes-iroh-sidecar",
  "relay": "default",
  "timeout_seconds": 120
}
```

`relay` is `default` (the public Iroh relays, which see connection metadata and not the
task's text), `off` (LAN only: peers then need a direct address each time), or a relay URL
of your own. Everything but calling works without the sidecar: pairing, tickets, the peer
list.

## Use it

Pair once, out of band (a chat, a QR, one paste):

```
(iroh:ticket)                       ; this agent's hermes-iroh://pair? ticket, 15 minutes, one use
(iroh:pair "hermes-iroh://pair?...")            ; refuses, naming the peer it would trust
(iroh:pair "hermes-iroh://pair?..." :confirm t) ; trusts it
```

Then:

```
(iroh:call "peer-id" "run the tests in src/ and tell me what broke")
(iroh:peers)   (iroh:status)
```

`/iroh` is the panel: pair with a ticket, issue this agent's ticket, call a peer. The model
has the same five things as the tools `iroh_peer_status`, `iroh_peer_list`,
`iroh_peer_make_ticket`, `iroh_peer_pair` and `iroh_peer_call`.

What a call carries is scrubbed of credential-shaped tokens (`token=...`, `Bearer ...`, a
private key block) before it leaves, and the answer is another agent's words: data, not the
operator's instruction. A ticket is a bearer capability for fifteen minutes: send it only to
the peer you mean to pair with. Every refusal is `ERROR: IROH-ERROR` with the reason.

Pairing state lives under `~/.nodecode/iroh/`, files `0600`: `peers.json`, `nonces.json`,
`pairing.secret`, and `endpoint.key` (the identity the sidecar keeps).

## Test it

```sh
.github/test-cell.sh tools/nodecode-iroh
```
