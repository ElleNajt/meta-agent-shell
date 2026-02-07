# Testing the Meta-Agent System

Instructions for testing the multi-agent coordination system.

## Setup

Create a test project directory:

```bash
rm -rf /tmp/test-agent-project && mkdir -p /tmp/test-agent-project
```

Clean up any existing test agents:

```elisp
(dolist (buf (copy-sequence agent-shell-to-go--active-buffers))
  (when (string-match-p "test-agent-project" (buffer-name buf))
    (kill-buffer buf)))
(setq meta-agent-shell--dispatchers nil)
```

Reload the package:

```elisp
(load-file "/Users/elle/code/meta-agent-shell/meta-agent-shell.el")
```

## Test 1: Dispatcher Spawning

**Goal:** Verify dispatcher starts in project directory and receives instructions.

```elisp
(meta-agent-shell-start-dispatcher "/tmp/test-agent-project")
```

**Expected:**
- Returns buffer name like `Dispatcher Agent @ test-agent-project`
- Dispatcher receives inline instructions about its role
- Dispatcher's working directory is `/tmp/test-agent-project`

**Verify:** Wait ~10 seconds, then check the dispatcher understood its role:

```elisp
(meta-agent-shell-view-session "Dispatcher Agent @ test-agent-project" 30)
```

Should see output indicating it knows it's a dispatcher and checked for existing agents.

## Test 2: Dispatcher Spawns Agent with Initial Task

**Goal:** Verify dispatcher can spawn a named agent and the agent works in the correct directory.

```elisp
(meta-agent-shell-send-to-session 
  "Dispatcher Agent @ test-agent-project"
  "Spawn a Worker agent to create hello.py that prints hello world"
  nil nil)
```

**Expected:**
- Dispatcher spawns `Worker Agent @ test-agent-project`
- Worker creates `/tmp/test-agent-project/hello.py`
- File is in the project directory, NOT in `~/.claude-meta/`

**Verify:** After ~20 seconds:

```bash
cat /tmp/test-agent-project/hello.py
```

Should contain a hello world print statement.

## Test 3: Named Agent Spawning Directly

**Goal:** Verify `meta-agent-shell-start-named-agent` works with initial message.

```elisp
(meta-agent-shell-start-named-agent 
  "/tmp/test-agent-project" 
  "Calculator" 
  "Create calculator.py with add and multiply functions")
```

**Expected:**
- Returns buffer name like `Calculator Agent @ test-agent-project`
- Agent receives the initial task immediately
- Creates `/tmp/test-agent-project/calculator.py`

**Verify:** After ~15 seconds:

```bash
cat /tmp/test-agent-project/calculator.py
```

## Test 4: Agent Communication

**Goal:** Verify agents can send messages to each other.

First, spawn two agents:

```elisp
(meta-agent-shell-start-named-agent "/tmp/test-agent-project" "AgentA" "You are AgentA. Wait for messages.")
(meta-agent-shell-start-named-agent "/tmp/test-agent-project" "AgentB" "You are AgentB. Wait for messages.")
```

Then have AgentA message AgentB:

```elisp
(meta-agent-shell-send-to-session
  "AgentA Agent @ test-agent-project"
  "Send a message to AgentB asking what 2+2 is. Use: agent-shell-send \"AgentB Agent @ test-agent-project\" \"What is 2+2?\""
  nil nil)
```

**Expected:** AgentB receives the question from AgentA.

**Verify:**

```elisp
(meta-agent-shell-view-session "AgentB Agent @ test-agent-project" 20)
```

Should show message received from AgentA.

## Test 5: Agent Ask (with reply)

**Goal:** Verify `agent-ask` delivers questions and the reply mechanism works.

Using the agents from Test 4, have the dispatcher ask AgentA a question:

```elisp
(meta-agent-shell-send-to-session
  "Dispatcher Agent @ test-agent-project"
  "Use agent-shell-ask to ask AgentA what its name is. The command is: agent-shell-ask \"AgentA Agent @ test-agent-project\" \"What is your name?\""
  nil nil)
```

**Expected:** 
- AgentA receives the question with reply instructions
- AgentA sends a reply back to the dispatcher

**Verify:** After ~15 seconds, check the dispatcher received a reply:

```elisp
(meta-agent-shell-view-session "Dispatcher Agent @ test-agent-project" 30)
```

Should show a message from AgentA with its response.

**Note:** `agent-shell-spawn` returns the buffer name, so dispatchers should capture and use that value rather than guessing buffer names.

## Test 6: List Project Agents

**Goal:** Verify `meta-agent-shell-get-project-agents` returns correct list.

```elisp
(meta-agent-shell-get-project-agents "/tmp/test-agent-project")
```

**Expected:** Returns list of all agent buffer names in the project (excluding dispatcher).

## Test 7: Interrupt Agent

**Goal:** Verify agents can be interrupted.

```elisp
(meta-agent-shell-interrupt-session "Worker Agent @ test-agent-project")
```

**Expected:** Returns `t` and the agent stops its current operation.

## Test 8: Dispatcher Interrupts Agent

**Goal:** Verify dispatcher can interrupt a runaway agent.

First spawn an agent that will take a while:

```elisp
(meta-agent-shell-start-named-agent 
  "/tmp/test-agent-project" 
  "SlowWorker" 
  "Count from 1 to 1000, printing each number with a 1 second delay")
```

Then have the dispatcher interrupt it:

```elisp
(meta-agent-shell-send-to-session
  "Dispatcher Agent @ test-agent-project"
  "Interrupt the SlowWorker agent - it's taking too long. Use: agent-shell-interrupt \"SlowWorker Agent @ test-agent-project\""
  nil nil)
```

**Expected:** Dispatcher successfully interrupts SlowWorker.

## Cleanup

```elisp
(dolist (buf (copy-sequence agent-shell-to-go--active-buffers))
  (when (string-match-p "test-agent-project" (buffer-name buf))
    (kill-buffer buf)))
(setq meta-agent-shell--dispatchers nil)
```

```bash
rm -rf /tmp/test-agent-project
```

## Common Issues

**Agent creates files in wrong directory:**
- Check that `default-directory` is set correctly before spawning
- Dispatcher should run in project directory, not `~/.claude-meta/`

**Dispatcher doesn't know its role:**
- Verify `meta-agent-shell--dispatcher-instructions` is being sent
- Check `meta-agent-shell-view-session` output for the instructions

**Buffer names don't match:**
- Full buffer name includes ` Agent @ projectname` suffix
- Use `(mapcar #'buffer-name agent-shell-to-go--active-buffers)` to see exact names

**Messages not delivered:**
- Check buffer name is exact (case-sensitive)
- Verify buffer exists with `(get-buffer "buffer-name")`
