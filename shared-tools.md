# Shared Tools Reference

Tools available via `emacsclient --eval '(...)'`.

## Communication

| Function | Description |
|----------|-------------|
| `(meta-agent-shell-send-to-session "buffer" "msg")` | Send message (fire and forget) |
| `(meta-agent-shell-ask-session "buffer" "question")` | Ask and get reply back |

**Use `ask-*` when you need a response.** The reply arrives automatically as a new message. Don't poll or sleep - just wait.

**Use `send-*` only for notifications** where you don't need a reply.

## Viewing

| Function | Description |
|----------|-------------|
| `(meta-agent-shell-view-session "buffer" 100)` | View last N lines from buffer |
| `(meta-agent-shell-list-sessions)` | List all sessions with status |

## Agent Lifecycle

| Function | Description |
|----------|-------------|
| `(meta-agent-shell-start-named-agent "~/path" "Name" "msg")` | Start named agent |
| `(meta-agent-shell-close-session "buffer")` | Close/kill session |
| `(meta-agent-shell-interrupt-session "buffer")` | Stop a runaway agent |

## Buffer Naming

- **Agents**: `(ProjectName)-AgentName` e.g. `(myproject)-Refactor`
- **Dispatchers**: `(ProjectName)-Dispatcher` e.g. `(myproject)-Dispatcher`
