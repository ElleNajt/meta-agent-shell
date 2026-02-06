# Shared Tools Reference

## Communication

| Command | Description |
|---------|-------------|
| `agent-send "buffer" "message"` | Send message (fire and forget) |
| `agent-ask "buffer" "question"` | Ask and get reply routed back |
| `agent-whoami` | Get your own buffer name |

**Use `agent-ask` when you need a response.** The reply arrives automatically as a new message. Don't poll or sleep - just wait.

**Use `agent-send` only for notifications** where you don't need a reply.

## Discovery

| Command | Description |
|---------|-------------|
| `agent-list` | List all active sessions |
| `agent-list --dispatchers` | List dispatchers only |
| `agent-search "pattern"` | Search all agent sessions for pattern |
| `agent-search "pattern" projectname` | Search specific project's sessions |
| `agent-view "buffer" [lines]` | View last N lines from a session |

## Agent Lifecycle

| Command | Description |
|---------|-------------|
| `agent-spawn "path" "Name" "task"` | Start named agent with initial task |
| `agent-close "buffer"` | Close/kill a session |
| `agent-interrupt "buffer"` | Stop a runaway agent |

**Always include the initial task** when spawning - saves a separate send call.

## Buffer Naming

Buffer names follow the pattern `AgentName Agent @ projectname`:
- **Agents**: `Refactor Agent @ myproject`
- **Dispatchers**: `Dispatcher Agent @ myproject`

Use `agent-whoami` to get your own buffer name.
