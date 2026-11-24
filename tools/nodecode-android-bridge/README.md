# nodecode-android-bridge

Android Bridge: drives a paired Android phone from the chat — read its screen, tap and type, take a photo, browse and fetch its files, read notifications, send an SMS or start a call. A Nodecode rewrite of [TechFlipsi/FlipsiBridge](https://github.com/TechFlipsi/FlipsiBridge) by [TechFlipsi](https://github.com/TechFlipsi).

Fifty-eight `android_*` tools, the original plugin's, with the same names and arguments. Every one is one HTTP call to the relay the Hermes Bridge companion app dials, so the cell talks to the same relay the plugin did.

| Group | Tools |
| --- | --- |
| Reach | `android_ping`, `android_setup` |
| Read the screen | `android_read_screen`, `android_find_nodes`, `android_describe_node`, `android_screenshot`, `android_screen_hash`, `android_diff_screen`, `android_read_widgets`, `android_events`, `android_event_stream` |
| Act on it | `android_tap`, `android_tap_text`, `android_type`, `android_press_key`, `android_swipe`, `android_scroll`, `android_drag`, `android_pinch`, `android_long_press`, `android_wait`, `android_macro` |
| Apps | `android_get_apps`, `android_current_app`, `android_open_app`, `android_send_intent`, `android_broadcast` |
| People | `android_notifications`, `android_notify_reply`, `android_send_sms`, `android_call`, `android_search_contacts` |
| Files | `android_files_list`, `android_files_search`, `android_files_count`, `android_file_get`, `android_file_put`, `android_file_delete`, `android_files_permission` |
| Device | `android_clipboard_read`, `android_clipboard_write`, `android_location`, `android_health`, `android_network_status`, `android_photo`, `android_torch`, `android_media`, `android_volume`, `android_alarm`, `android_timer`, `android_speak`, `android_speak_stop`, `android_screen_record`, `android_mic_record`, `android_mic_stop`, `android_mic_status`, `android_mic_fetch` |

Ask "what is on my phone's screen?", "tap the Send button", "read my last three notifications", "send a file from my phone to this machine", "text Ana that I am late" — the model picks the tool.

## Get it

Tell the agent "add the android-bridge cell", or run `nodecode add nc://android-bridge`.

## Set it up

The other half is the Hermes Bridge app on the phone, from the original project, and the relay it dials. The cell speaks to that relay and bundles nothing.

Two settings, in the `android-bridge` section of `~/.nodecode/config.jsonc` (or in `/settings`):

```jsonc
{
  "android-bridge": {
    "url": "http://127.0.0.1:8766",
    "token": "<the pairing code the Hermes Bridge app shows>"
  }
}
```

- `url` is the relay's base address; `http://127.0.0.1:8766` is the relay's own default. On the same network you can point it at the phone's own HTTP server instead, and skip the relay.
- `token` is the pairing code from the app. It rides as `Authorization: Bearer` to that one address and nowhere else.

`url` is the one required setting: the cell refuses to start without it, rather than guessing an address. A relay the cell does not start is the original project's; run it there, or put your own TLS relay in front and point `url` at it.

## Use it

Call `android_ping` first: it says whether the relay answers and whether a phone is connected, and `android_setup` says what to do about a phone that is not. Then read the screen before touching it:

```
android_read_screen            the accessibility tree: node ids, text, classes
android_tap {"node_id": "..."}   or {"x": 540, "y": 1200}
android_tap_text {"text": "Send"}
```

Files that come to this machine — `android_file_get`, `android_mic_fetch` — answer with a `MEDIA:` line naming a file in the temp directory, which the chat shows.

`android_macro` runs a list of `{"tool": ..., "args": {...}}` steps in order and stops at the first failure. Every answer is the relay's own JSON, or `error: ...` when the call could not be made.

Calling one from eval, in the `android-bridge` package, is the same function the model calls:

```lisp
(android-bridge:handle-tool "android_read_screen" (make-hash-table))
```

`(help :android-bridge)` lists every tool in the running image.
