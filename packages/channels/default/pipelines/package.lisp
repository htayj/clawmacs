(in-package :clawmacs)

(register-package-prompt-section
 "pipelines"
 "## Deterministic pipelines

- Clawmacs pipelines are user-defined, deterministic routing graphs that run
  one or more agent stages for a user request.
- A pipeline stage can use the original user input, the previous stage output,
  or a named prior stage output. Clawmacs handles that routing outside the
  model; do not simulate the routing yourself unless the user explicitly asks.
- When a buffer has an active pipeline, normal message sending runs the
  configured pipeline instead of a single agent response.
- Pipeline stages are configured in Lisp with `define-pipeline` or
  `defpipeline`. Stages support static `:next` targets and function-valued
  `:next` routers for deterministic retry or repair loops."
 :title "Deterministic pipelines"
 :package "pipelines")

(defcommand set-buffer-pipeline
  :prompts ((pipeline-name :prompt "Pipeline name"))
  :docstring "Set the current buffer's deterministic pipeline by name.")

(defcommand clear-buffer-pipeline
  :docstring "Clear the current buffer's deterministic pipeline.")

(defdoc define-pipeline
  :category "pipelines"
  :usage "(define-pipeline NAME :stages '((:name \"plan\" :agent \"planner\" :prompt \"Plan {{input}}\" :next \"implement\") ...))"
  :returns "pipeline-definition - The registered deterministic pipeline."
  :side-effects "Updates the process-local pipeline registry. Buffers run a pipeline when BUFFER-PIPELINE-NAME names one, and prompt.sh can run one with --pipeline NAME."
  :see-also (defpipeline register-pipeline-definition set-buffer-pipeline run-pipeline-prompt))

(defdoc defpipeline
  :category "pipelines"
  :usage "(defpipeline plan-implement-test :stages '((:name \"plan\" ...)))"
  :returns "pipeline-definition - The registered deterministic pipeline."
  :see-also (define-pipeline register-pipeline-definition))

(defdoc set-buffer-pipeline
  :category "pipelines"
  :usage "(set-buffer-pipeline BUFFER PIPELINE-NAME) - (clear-buffer-pipeline BUFFER)"
  :returns "buffer - The mutated buffer."
  :side-effects "Causes SEND-MESSAGE to run the named deterministic pipeline instead of a single streaming agent response."
  :see-also (define-pipeline run-pipeline-for-buffer buffer-pipeline-name))

(defdoc run-pipeline-prompt
  :category "pipelines"
  :usage "(run-pipeline-prompt PROMPT PIPELINE-NAME &key :session-name :agent-name :provider :model :think-level :max-tool-iterations :auto-approve-tools-p :package-names)"
  :returns "prompt-run-result - The final pipeline stage summarized as prompt-mode output."
  :side-effects "Loads or creates a prompt buffer, records each pipeline stage as context, runs provider requests, executes allowed tools, and persists session snapshots when SESSION-NAME is supplied."
  :see-also (define-pipeline run-pipeline-on-buffer pipeline-stage-result-final-text))
