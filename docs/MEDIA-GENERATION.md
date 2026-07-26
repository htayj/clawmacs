# Media generation

Clawmacs generates media through small provider packages built on the `media`
contract. The initial bundled provider is `codex-image`, which mirrors the
current Codex CLI image tool's one-image behavior: it selects `gpt-image-2`,
uses a text prompt, optionally edits from up to five local reference images,
and stores the resulting PNG in the active buffer's Artifactum session.

## Enabling and using Codex images

Enable the bundled `media` and `codex-image` packages. `codex-image` depends
on `media` and `artifactum`, so enabling the former loads the needed contract
and durable artifact store. The agent-facing tool is intentionally narrow:

```lisp
(media_generate_image
  :prompt "A small Common Lisp mascot at a REPL"
  :referenced_image_paths #("/absolute/path/reference.png"))
```

`prompt` is required. `referenced_image_paths` is optional and must contain at
most five existing absolute regular files. Without references, the adapter
posts to Codex's image-generation route; with references it posts to the edit
route. The tool never accepts a provider, model, output directory, or billing
selector. Those decisions remain trusted user configuration and package code.

The returned artifact records are durable session files. Their metadata records
the model and subscription transport; provenance records the media provider,
request, and operation. There is deliberately no live McCLIM bitmap preview
yet: Artifactum owns generated files, while a future CLIM presentation-based
viewer can render those durable records without taking over the repaint loop.

## Authentication and billing boundary

This adapter only reads the shared Codex login store at `~/.codex/auth.json`.
It requires ChatGPT/Codex OAuth `access_token` and `account_id`, refreshes stale
credentials through the existing Codex OAuth refresh logic, and sends Codex's
`Authorization`, `originator`, and `ChatGPT-Account-ID` headers. It does not
read Clawmacs's static OpenAI token file, does not use an OpenAI API key in
`auth.json`, and has no automatic public API fallback. That keeps the route
subscription-bound instead of silently creating API-billed image usage.

Image generation consumes the general Codex usage allowance and can consume it
more quickly than ordinary work. Do not use a live smoke test merely to verify
installation: deterministic tests exercise the auth selection, refresh,
request construction, retries, response validation, and Artifactum persistence
without spending subscription quota.

The background tool path registers cooperative cancellation before the adapter
opens its connection. Cancellation is checked before connect and between
transient retry/backoff attempts; a late remote result is never persisted after
the local media operation has been cancelled.

The ChatGPT Codex backend route is not a documented third-party public API.
This package mirrors the current Codex source and is intentionally
version-sensitive. A server-side route, entitlement, response-shape, or
authentication change can make it unavailable; failures are reported as safe
public categories rather than exposing credential or raw response data.

## Provider contract and future media

`media` owns provider registration, a user-selected default provider, request
and operation lifecycle data, and durable artifact persistence. A provider
declares its media kinds and start callback, and may later add poll/cancel
callbacks for asynchronous jobs. The current Codex package implements one
synchronous image result. Future packages may implement xAI image generation
or asynchronous image-to-video/video generation through the same contract;
those packages should preserve user-selected provider and billing boundaries.

## Sources

- [Codex image-generation documentation](https://developers.openai.com/codex/images/)
- [Codex source: image-generation extension (revision 61a44880a85d2fd0d8770908dea5733495e571c8)](https://github.com/openai/codex/tree/61a44880a85d2fd0d8770908dea5733495e571c8/codex-rs/ext/image-generation)
- [OpenAI Image API guide](https://platform.openai.com/docs/guides/image-generation)
- [xAI image generation](https://docs.x.ai/docs/guides/image-generation) and [video generation](https://docs.x.ai/docs/guides/video-generation)
