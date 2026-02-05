# meta-agent-shell

Emacs package providing a supervisory meta-agent for agent-shell sessions.

## Testing Changes

Reload after editing:
```bash
emacsclient -e '(load-file "/Users/elle/code/meta-agent-shell/meta-agent-shell.el")'
```

## Key State

Global state:
- `meta-agent-shell--buffer` - The dedicated meta-agent buffer
- `meta-agent-shell--heartbeat-timer` - Timer for periodic heartbeats
- `meta-agent-shell--last-user-interaction` - Timestamp for cooldown logic

## Dependencies

Requires:
- `agent-shell-to-go` - For Slack integration and `--active-buffers` list
- Uses `agent-shell-to-go--inject-message` for sending to other sessions
- Uses `agent-shell-to-go--get-project-path` for project detection

## Architecture

- Heartbeat: Formats session status + user instructions, sends to meta session
- Tools: Elisp functions meta-agent calls via `emacsclient --eval`
- All tools operate on `agent-shell-to-go--active-buffers`
