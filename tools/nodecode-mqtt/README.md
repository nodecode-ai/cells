# nodecode-mqtt

MQTT: publish an event to a broker, read a topic. A Nodecode rewrite of [JasonSwindle/hermes-mqtt](https://github.com/JasonSwindle/hermes-mqtt) by [JasonSwindle](https://github.com/JasonSwindle).

The agent publishes one thin event to a topic on your MQTT broker and reads what arrives on a topic filter, through the same gates the original kept: nothing leaves unless the topic is on an allowlist.

## Get it

Tell the agent "add the mqtt cell", or run `nodecode add nc://nodecode-mqtt`.

## Set it up

It needs an MQTT broker you already run, and a user on it — the cell uses one, it never creates one. In the `mqtt` section of `~/.nodecode/config.jsonc`:

```jsonc
{
  "mqtt": {
    "url": "tcp://127.0.0.1:1883",       // or mqtts://host:8883 for TLS
    "username": "nodecode",
    "password": "",                       // empty means anonymous
    "publish_allowed": ["home/sensor"],  // empty denies every publish
    "subscribe_allowed": ["home/#"],      // empty denies every read
    "timeout_seconds": 10,
    "max_payload_bytes": 32768
  }
}
```

An `mqtts://` url is a TLS listener with the certificate and the hostname verified, against the system trust store. Then `/mqtt ping` says whether the broker answers.

## Use it

From a session, in eval or through the tools:

```lisp
(mqtt:publish "home/sensor" '(:temp 21) :kind "reading")  ; => "Published 132 bytes to home/sensor at QoS 1, acked by the broker"
(mqtt:read-topics "home/#" :count 5)                       ; => the messages, or "No messages ..."
```

or the tools `mqtt_publish` and `mqtt_read`, which take the same arguments as a JSON object. `/mqtt` reports the section; `/mqtt ping` probes the broker.

What the gates are:

- **A publish topic is literal.** No `+` and no `#`, and it must be matched by `publish_allowed`; an entry there may itself be a filter, so `home/#` allows `home/sensor`. An empty allowlist denies.
- **A read cannot widen its filter.** `subscribe_allowed` must cover every topic the filter you ask for could match: with `home/+` allowed, `home/#` is refused.
- **The payload is scanned.** A string shaped like an API token, a private key or an AWS key is refused before it goes on the wire — publish a reference to the data, not the secret.
- **The envelope is thin:** schema, kind, time, a per-process nonce, and your data. The nonce is what tells two processes apart.
- **QoS 1, so the broker acks.** A publish answers only once the PUBACK arrives; a read collects at most the count you asked for, waits at most the timeout, and then leaves the connection.

The wire is the MQTT 3.1.1 client protocol itself, over the socket support already in the image — no `mosquitto_pub` and no client library to install.

## What it does not do

The original was a platform: one subscription held open by a gateway, routing each topic to a delivered chat line, an agent turn or a cron job, with burst coalescing and a reconnect supervisor, and it spoke MQTT 5.0 as well. A cell is a verb in a session rather than a resident process, so here a session publishes and reads one bounded window at a time; watching a topic is a loop the operator runs against the same broker, and the protocol is MQTT 3.1.1 with TLS over `mqtts://`.
