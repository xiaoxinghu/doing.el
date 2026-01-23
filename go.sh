#!/usr/bin/env bash
set -euo pipefail

task_id=$(emacs --batch tasks.org \
  -l ./batch.el \
  --eval "(my-org-next-task)")

output_dir=".$task_id"
mkdir -p "$output_dir"

emacs --batch tasks.org \
  -l ./batch.el \
  --eval "(my-org-task-content \"$task_id\")" \
  > "$output_dir/task.org"

echo "Working on task: $task_id"

result_file="$output_dir/result.json"

claude -p --output-format json --model sonnet \
  --dangerously-skip-permissions \
	< "$output_dir/task.org" \
	<<'EOF' > "$result_file"
You are given an task (in org-mode format) ABOVE.

## Instructions

- Read the following files (in order) for context:
  - @docs/research.org
  - @docs/commands.org
  - @docs/format.org
  - @project.org
- Read @progress.org to get more context on the current progress and leanrings.
- Begin working on the gaven task, ONLY WORK ON THIS TASK!
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
EOF

jq -e '.is_error == false' "$result_file" > /dev/null \
  || { echo "Claude error"; exit 1; }

cost=$(jq -r '.total_cost_usd // 0' "$result_file")
time=$(jq -r '.duration_ms // 0' "$result_file")
input_tokens=$(jq -r '.usage.input_tokens // 0' "$result_file")
output_tokens=$(jq -r '.usage.output_tokens // 0' "$result_file")
session_id=$(jq -r '.session_id // ""' "$result_file")

props="(
  (time_ms . $time)
  (cost . $cost)
  (input_tokens . $input_tokens)
  (output_tokens . $output_tokens)
  (session_id . \"$session_id\")
)"

emacs --batch tasks.org \
  -l ./batch.el \
  --eval "(my-org-mark-done \"$task_id\" '$props)"
