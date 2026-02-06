# Task Management with Org Files

Dispatchers and agents use `.tasks/` folders with org files for persistent task tracking. Tasks show up in Elle's Emacs agenda.

## Structure

```
project/
└── .tasks/
    └── current.org   # main task file (or split by thread)
```

## Task File Format

```org
#+TITLE: Project Tasks
#+CATEGORY: myproject
#+FILETAGS: :myproject:

* Active Work

** TODO Refactor auth middleware :@claude:
   SCHEDULED: <2026-02-05>
   :PROPERTIES:
   :ASSIGNED: Claude Code Agent @ myproject
   :CREATED: [2026-02-05 Wed 14:00]
   :END:
   
   Context and details about the task.

** DOING Implement rate limiting :@claude:
   :PROPERTIES:
   :ASSIGNED: Claude Code Agent @ myproject<2>
   :END:

* Done
** DONE Set up test fixtures
   CLOSED: [2026-02-03]
```

## Workflow

1. **Planning**: Elle and dispatcher discuss work to be done
2. **Dispatcher writes tasks**: Creates/updates TODOs in `.tasks/current.org`
3. **Assignment**: Sets `:ASSIGNED:` property to agent buffer name
4. **Agent works**: Marks `TODO` → `DOING` when starting
5. **Completion**: Agent marks `DOING` → `DONE`, adds `CLOSED` timestamp
6. **Visibility**: Elle sees all tasks in org-agenda

## Tags

| Tag | Meaning |
|-----|---------|
| `:@claude:` | Assigned to an agent |
| `:@elle:` | Needs Elle's input/review |
| `:blocked:` | Waiting on something external |

## TODO States

- `TODO` - Not started
- `DOING` - Agent actively working
- `WAITING` - Blocked, needs input
- `DONE` - Completed

## Properties

| Property | Purpose |
|----------|---------|
| `:ASSIGNED:` | Agent buffer name (for routing) |
| `:CREATED:` | When task was created |
| `:BLOCKED_BY:` | What it's waiting on (if blocked) |

## For Dispatchers

When creating tasks:
```elisp
;; Add a task to the project's task file
;; Use standard org-mode format, include :ASSIGNED: property
```

When assigning work:
1. Check `.tasks/current.org` for unassigned `TODO` items
2. Set `:ASSIGNED:` to the agent buffer name
3. Notify the agent of the task

When agent completes:
1. Agent marks task `DONE` with timestamp
2. Dispatcher can move to "Done" section or archive

## For Agents

When starting work:
1. Read your assigned tasks from `.tasks/current.org`
2. Mark `TODO` → `DOING` when you begin
3. Add notes under the task as you work

When completing:
1. Mark `DOING` → `DONE`
2. Add `CLOSED: [timestamp]`
3. Brief summary of what was done

## Agenda Integration

Elle adds project task files to org-agenda:
```elisp
(add-to-list 'org-agenda-files "~/code/myproject/.tasks/")
```

Then tasks appear in daily/weekly agenda views with their scheduled dates.
