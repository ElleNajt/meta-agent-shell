# Agent System Overview

You are one of potentially many Claude agents working with Elle. This doc explains how the system works.

## Architecture

```
Elle (human)
    ↓
Meta-Agent (~/.claude-meta/)
    ↓ heartbeat + coordination
Project Dispatchers (one per multi-agent project)
    ↓ routes work
Individual Agents (you might be one of these)
```

**Meta-agent**: Supervisory Claude that monitors all sessions, receives periodic heartbeats with status of all agents.

**Dispatcher**: Per-project coordinator that routes work to appropriate agents. Stateless - asks agents what they're working on rather than maintaining state.

**Agent**: You. Works on a specific project or task.

## Task Management

Tasks live in `.tasks/` folders as org files:

```
project/.tasks/current.org
```

```org
** TODO Implement feature X :@claude:
   :PROPERTIES:
   :ASSIGNED: Claude Code Agent @ myproject
   :END:
   
   Details here.
```

**When you start a task**: Mark `TODO` → `DOING`
**When you finish**: Mark `DOING` → `DONE`, add `CLOSED: [timestamp]`

These show up in Elle's Emacs agenda.

## Inter-Agent Communication

Agents can message each other via elisp:

```bash
# Send message (fire and forget)
emacsclient --eval '(meta-agent-shell-send-to-session "buffer-name" "message")'

# Ask and expect reply
emacsclient --eval '(meta-agent-shell-ask-session "buffer-name" "question")'
```

When asked a question, you'll be told how to reply.

## Key Points

1. **You're not alone** - Other agents may be working on the same project
2. **Check .tasks/** - Your assigned work is there
3. **Update task status** - So Elle and dispatchers know what's happening
4. **Dispatcher routes work** - If you get a message from dispatcher, it's coordination
5. **Meta-agent observes** - Periodic heartbeats monitor all sessions

## If You're Confused

- Check `.tasks/current.org` for your assignments
- Ask the dispatcher: "What should I be working on?"
- Look at your `:ASSIGNED:` property in tasks
