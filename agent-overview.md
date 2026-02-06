# Agent System Overview

You are one of potentially many Claude agents. This doc explains how the system works.

## Architecture

```
User (human)
    ↓
Meta-Agent (~/.claude-meta/)
    ↓ coordination
Project Dispatchers (one per multi-agent project)
    ↓ routes work, manages agent lifecycle
Individual Agents (you might be one of these)
```

## Communicating with Other Agents

Use these shell commands:

```bash
# Send a message to another agent
agent-send "target-buffer" "message"

# Ask another agent a question (they'll reply back to you)
agent-ask "target-buffer" "question"

# Find out your own buffer name
agent-whoami
```

Examples:

```bash
agent-send "(myproject)-Tests" "Feature is ready for testing"
agent-ask "(myproject)-Dispatcher" "What should I work on next?"
```

Your identity is auto-detected - you don't need to know your own buffer name.

## Buffer Naming

Named agents use the format `(ProjectName)-AgentName`:
- `(myproject)-Refactor`
- `(myproject)-Tests`
- `(myproject)-Bugfix`

Dispatchers use `(ProjectName)-Dispatcher`.

## When You Receive a Question

If another agent asks you a question, you'll see instructions like:

```
Question from (myproject)-Main:

What's the status of the tests?

Reply using (replace YOUR_ANSWER with your response):
emacsclient --eval '(meta-agent-shell-send-to-session "(myproject)-Main" "YOUR_ANSWER" nil '$$')'
```

Just replace `YOUR_ANSWER` with your response - the sender identity is auto-detected from `$$`.

Or use the simpler shell command:

```bash
agent-send "(myproject)-Main" "All tests passing"
```

## Task Management (Optional)

If a project uses task tracking, tasks live in `.tasks/current.org`. If the file doesn't exist and you want to track tasks, create it.

## Key Points

1. **You're not alone** - Other agents may be working on the same project
2. **Dispatcher routes work** - If you get a message from a dispatcher, it's coordination
3. **Use descriptive names** - When spawning agents, name them by their task
4. **Auto-detection works** - You don't need to know your own buffer name for sending messages
