#!/bin/sh
# fake-acp.sh --- a stand-in OpenCode ACP server for the offline tests.
#
# SPDX-License-Identifier: MIT
#
# It answers the five requests one round sends, in order: initialize,
# session/new, the two config selections, and the prompt. It never reads a
# model, never reaches the network and knows nothing of a real CLI; the
# second config selection is answered with the model the round asked for,
# which is the one the fake advertises.

reply() {
  printf '%s\n' "$1"
}

n=0
while IFS= read -r line; do
  n=$((n + 1))
  case $n in
    1)
      reply '{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":1}}'
      ;;
    2)
      reply '{"jsonrpc":"2.0","id":2,"result":{"sessionId":"fake-session","configOptions":[{"id":"mode","category":"mode","currentValue":"plan","options":[{"value":"plan"},{"value":"build"}]},{"id":"model","category":"model","currentValue":"opencode/unset","options":[{"value":"opencode/glm-5.3-flash-free"},{"value":"opencode/big-pickle"}]}]}}'
      ;;
    3)
      reply '{"jsonrpc":"2.0","id":3,"result":{"configOptions":[{"id":"mode","currentValue":"build"}]}}'
      ;;
    4)
      reply '{"jsonrpc":"2.0","id":4,"result":{"configOptions":[{"id":"model","currentValue":"opencode/glm-5.3-flash-free"}]}}'
      ;;
    5)
      reply '{"jsonrpc":"2.0","method":"session/update","params":{"update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"Let me read it.\n"}}}}'
      reply '{"jsonrpc":"2.0","method":"session/update","params":{"update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"<hermes_tool_call>{\"id\":\"call_1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"notes.txt\\\"}\"}}</hermes_tool_call>"}}}}'
      reply '{"jsonrpc":"2.0","id":5,"result":{"stopReason":"end_turn"}}'
      ;;
  esac
done
