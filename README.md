# meta-agent-shell

A supervisory agent for [agent-shell](https://github.com/xenodium/agent-shell) sessions. Monitor all your active Claude sessions, search across them, send messages between agents, and manage your fleet of AI agents from Slack.

Requires [agent-shell-to-go](https://github.com/ElleNajt/agent-shell-to-go) for Slack integration.

## Features

- **Heartbeat monitoring** - Periodic status updates with all active sessions
- **Session inspection** - View recent output from any session
- **Cross-session search** - Find errors, patterns, or content across all agents
- **Inter-agent messaging** - Send messages from one agent to another
- **Session management** - Start, stop, or interrupt agent sessions
- **Standing instructions** - Configure what the meta-agent should do on each heartbeat

## Installation

```elisp
(use-package meta-agent-shell
  :after agent-shell-to-go
  :config
  (setq meta-agent-shell-heartbeat-file "~/heartbeat.org")
  ;; Optional: auto-start heartbeat
  ;; (meta-agent-shell-heartbeat-start)
  )
```

## Quick Start

1. Start the meta-agent session:
   ```
   M-x meta-agent-shell-start
   ```

2. Start the heartbeat timer:
   ```
   M-x meta-agent-shell-heartbeat-start
   ```

3. The meta-agent runs in `~/.claude-meta/` and receives periodic heartbeats with session status and your standing instructions from `~/heartbeat.org`.

## Configuration

```elisp
;; Path to your instructions file (default: ~/heartbeat.org)
(setq meta-agent-shell-heartbeat-file "~/heartbeat.org")

;; Interval in seconds (default: 900 = 15 minutes)
(setq meta-agent-shell-heartbeat-interval 900)

;; Cooldown after user interaction (default: 300 = 5 minutes)
(setq meta-agent-shell-heartbeat-cooldown 300)

;; Directory for meta session (default: ~/.claude-meta/)
(setq meta-agent-shell-directory "~/.claude-meta/")

;; How many lines of recent output to include in heartbeat
(setq meta-agent-shell-heartbeat-recent-lines 50)
```

## Standing Instructions

Create `~/heartbeat.org` (or your configured path) with instructions for the meta-agent:

```org
* Instructions for Meta-Agent

You receive periodic heartbeats with status on active agent sessions.

** On Each Heartbeat

Use your tools to investigate and give a status report:

1. Run (meta-agent-shell-list-sessions) to see all active sessions
2. For interesting sessions, use (meta-agent-shell-view-project "name" 50)
3. Search for problems with (meta-agent-shell-search-sessions "error\\|failed" 2)

Then summarize what's happening. Keep it concise.

** Available Tools

| Function | Description |
|----------|-------------|
| (meta-agent-shell-list-sessions) | List all sessions |
| (meta-agent-shell-view-project "name" 100) | View last N lines |
| (meta-agent-shell-search-sessions "pattern" 2) | Search all sessions |
| (meta-agent-shell-send-to-project "name" "msg") | Send message to agent |
| (meta-agent-shell-interrupt-project "name") | Stop runaway agent |
```

## Tools

The meta-agent can call these via `emacsclient --eval`:

| Function | Description |
|----------|-------------|
| `(meta-agent-shell-list-sessions)` | List all sessions with status |
| `(meta-agent-shell-view-project "name" 100)` | View last N lines from session |
| `(meta-agent-shell-view-session "buffer" 100)` | View by buffer name |
| `(meta-agent-shell-search-sessions "pattern" 2)` | Search all sessions (regexp) |
| `(meta-agent-shell-search-project "name" "pattern")` | Search one session |
| `(meta-agent-shell-send-to-project "name" "msg")` | Send message to agent |
| `(meta-agent-shell-send-to-session "buffer" "msg")` | Send by buffer name |
| `(meta-agent-shell-close-project "name")` | Close/kill session |
| `(meta-agent-shell-interrupt-project "name")` | Interrupt running agent |
| `(meta-agent-shell-start-agent "~/path" "msg")` | Start new agent |

## Commands

| Function | Description |
|----------|-------------|
| `meta-agent-shell-start` | Start or switch to meta session |
| `meta-agent-shell-heartbeat-start` | Start periodic heartbeat timer |
| `meta-agent-shell-heartbeat-stop` | Stop heartbeat timer |
| `meta-agent-shell-heartbeat-send-now` | Send heartbeat immediately |

## Security

The meta-agent needs permissions to use its tools (emacsclient --eval). Consider using directory-based mode settings:

```elisp
(setq agent-shell-anthropic-claude-code-default-session-mode-id
      (lambda ()
        (cond
         ;; Meta-agent: needs permissions for its tools
         ((string-prefix-p (expand-file-name "~/.claude-meta")
                           (expand-file-name default-directory))
          "default")
         ;; Untrusted directories: restricted mode
         ((string-prefix-p (expand-file-name "~/Downloads")
                           (expand-file-name default-directory))
          "dontAsk")
         ;; Default for other directories
         (t "default"))))
```

## Example Use Cases

- **From your phone**: "What's the agent-shell-to-go session working on?"
- **Check for errors**: "Search all sessions for any errors or failures"
- **Coordinate work**: "Tell the secretary agent to prioritize the email task"
- **Clean up**: "Close all the idle sessions"
- **Status report**: "Give me a summary of what all agents accomplished today"

## License

GPL-3.0
