# Shared Tools Reference

## Communication

| Command | Description |
|---------|-------------|
| `agent-shell-send "buffer" "message"` | Send message (fire and forget) |
| `agent-shell-ask "buffer" "question"` | Ask and get reply routed back |
| `agent-shell-whoami` | Get your own buffer name |

**Use `agent-shell-ask` when you need a response.** The reply arrives automatically as a new message. Don't poll or sleep - just wait.

**Use `agent-shell-send` only for notifications** where you don't need a reply.

## Discovery

| Command | Description |
|---------|-------------|
| `agent-shell-list` | List all active sessions |
| `agent-shell-list --dispatchers` | List dispatchers only |
| `agent-shell-search "pattern"` | Search all agent sessions for pattern |
| `agent-shell-search "pattern" projectname` | Search specific project's sessions |
| `agent-shell-view "buffer" [lines]` | View last N lines from a session |

## Agent Lifecycle

| Command | Description |
|---------|-------------|
| `agent-shell-spawn "Name" "task"` | Start named agent with initial task |
| `agent-shell-close "buffer"` | Close/kill a session |
| `agent-shell-interrupt "buffer"` | Stop a runaway agent |

**Always include the initial task** when spawning - saves a separate send call.

## Notes

| Command | Description |
|---------|-------------|
| `agent-shell-note desc "note"` | Append timestamped note to `.tasks/agent_<desc>.org` |

Use your role as the desc (e.g., `refactor`, `tests`). Notes persist across sessions.

## Buffer Naming

Buffer names follow the pattern `AgentName Agent @ projectname`:
- **Agents**: `Refactor Agent @ myproject`
- **Dispatchers**: `Dispatcher Agent @ myproject`

Use `agent-shell-whoami` to get your own buffer name.
