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

# Search all agent sessions for a pattern
agent-search "pattern"

# Search a specific project's sessions
agent-search "pattern" projectname
```

Examples:

```bash
agent-send "(myproject)-Tests" "Feature is ready for testing"
agent-ask "(myproject)-Dispatcher" "What should I work on next?"
```

Your identity is auto-detected - you don't need to know your own buffer name.

## Buffer Naming

Full buffer names include the agent-shell suffix:
- **Agents**: `AgentName Agent @ projectname` (e.g., `Refactor Agent @ myproject`)
- **Dispatchers**: `Dispatcher Agent @ projectname`

The examples in this doc use short forms for readability, but use exact buffer names from `agent-whoami` or list commands when messaging.

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

## When to Use a Worktree

Ask yourself: "Will my uncommitted changes get in anyone's way, or could I lose work if someone else commits?"

If yes, create a worktree:
```bash
git worktree add ../projectname-taskname -b task-branch
cd ../projectname-taskname
# ... do work, commit ...
# when done, merge or PR back
```

**Use a worktree when:**
- Editing multiple files as part of one change
- Work that needs intermediate commits before it's ready
- Changes you might revert entirely

**Stay in main tree when:**
- Single file edits
- Adding a new file
- Running tests, reading code, exploration

## Key Points

1. **You're not alone** - Other agents may be working on the same project
2. **Dispatcher routes work** - If you get a message from a dispatcher, it's coordination
3. **Use descriptive names** - When spawning agents, name them by their task
4. **Auto-detection works** - You don't need to know your own buffer name for sending messages
