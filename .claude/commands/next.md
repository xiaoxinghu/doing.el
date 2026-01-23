---
description: Select and work on the next TODO item from project.org
---

## Instructions

- Read the following files (in order) for context:
  - @docs/research.org
  - @docs/commands.org
  - @docs/format.org
- Read @progress.org to get more context on the current progress and leanrings.
- Parse the @project.org file, locate the first TODO item. Read the full task details.
- Begin working on it, ONLY WORK ON THE SELECTED TASK!
- Update the TODO status to DONE in @project.org file.
- Append learnings to @progress.org file.
- Commit with title format `feat: [Title]`, where Title is the headline of the task you found in @project.org file. Generate appropriate message that describe the changes underneath.

## Progress Format

APPEND to the end of @progress.org

```org-mode
* [Date] - [Title]
- what was implemented
- files changed
- learnings
  - patterns discoverd
  - gotchas encountered
```
