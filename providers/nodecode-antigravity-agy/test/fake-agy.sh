#!/bin/sh
# fake-agy.sh --- a stand-in for the agy CLI, speaking its stream-json.
#
# It reads the one user event on stdin, prints an init, one agent response
# step and a result, and exits. FAKE_AGY_ANSWER picks the answer:
#   text (default)  a plain answer
#   tool            one <call> line, the round's tool protocol
#   fallback        an init naming another agent, which a round must refuse
set -eu
cat >/dev/null
answer=${FAKE_AGY_ANSWER:-text}
agent=nodecode-relay
if [ "$answer" = "fallback" ]; then agent=default; fi
printf '%s\n' "{\"event\":\"init\",\"init\":{\"agent\":\"$agent\"}}"
case "$answer" in
  tool)
    printf '%s\n' '{"event":"step_update","step_update":{"step_type":"agent_response","text_delta":"<call name=\"read\">{\"path\":\"notes\"}</call>"}}'
    printf '%s\n' '{"event":"result","result":{"status":"SUCCESS","response":"<call name=\"read\">{\"path\":\"notes\"}</call>","usage":{"input_tokens":10,"output_tokens":4,"cache_read_tokens":2,"thinking_tokens":1}}}'
    ;;
  *)
    printf '%s\n' '{"event":"step_update","step_update":{"step_type":"agent_response","text_delta":"Hello"}}'
    printf '%s\n' '{"event":"result","result":{"status":"SUCCESS","response":"Hello from agy","usage":{"input_tokens":10,"output_tokens":4}}}'
    ;;
esac
exit 0
