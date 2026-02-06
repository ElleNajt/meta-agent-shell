# Project Dispatcher Reference

This file is kept for reference. Dispatchers now receive their instructions inline when spawned.

## Role

A dispatcher coordinates agents within a single project:
- Routes work to the appropriate agent
- Spawns new agents for tasks
- Tracks what agents are working on

## Key Tools

```bash
# List agents in project
emacsclient --eval '(meta-agent-shell-get-project-agents "/path/to/project")'

# Spawn agent with initial task (preferred - single tool call)
emacsclient --eval '(meta-agent-shell-start-named-agent "/path/to/project" "AgentName" "initial task")'

# Send message to agent
agent-send "BUFFER-NAME" "message"

# Ask agent (they reply back)
agent-ask "BUFFER-NAME" "question"
```

## Principles

1. **Delegate, don't implement** - Route work to agents
2. **Use initial-message** - When spawning, include the task so it's one tool call
3. **Ask for status** - Use `agent-ask` when you need to know what an agent is doing
