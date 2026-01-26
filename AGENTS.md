AGENTS.txt

Purpose
-------

This document defines how AI agents (including Codex, GPT-5.2, and other OpenAI models)
should operate when working in this repository.

The goal is high-quality, verifiable, and intentional work — not rushed or implicit output.


Core Principles
---------------

1. Plan First, Act Second

Before making changes, plan ahead carefully.

- Break work into a detailed TODO list
- Think through edge cases, constraints, and dependencies
- Do not jump straight to code or edits

Use the following checklist format consistently:

[ ] item open
[~] item in progress
[x] item completed

Example:

TODO
[ ] Define AST node types
[ ] Specify ownership and lifetime rules
[~] Implement parser
[x] Tokenizer

This checklist should evolve as work progresses.


2. Verify Completeness Explicitly

After completing a task, verify your own work.

Ask yourself (and answer explicitly if appropriate):

- Did I complete all TODO items?
- Are there any dangling assumptions?
- Are there unhandled edge cases?
- Does the implementation match the original intent?
- If code was written: does it compile, type-check, or logically hold?

Do not assume correctness by default. Confirm it.


3. Be Explicit About Intent

Always give clear and detailed explanations of what you are doing and why.

- Explain design decisions, not just mechanics
- Prefer clarity over cleverness
- Avoid vague phrases like “this should work” or “obviously”

If multiple approaches exist:
- Briefly enumerate them
- Explain why one was chosen


4. Do Not Assume Perfect Domain Knowledge

When asking the user for input — especially in compiler construction, language design,
or systems work:

- Do not assume the user shares your internal model
- Spell out assumptions explicitly
- Ask focused, concrete questions

Bad:
“How should MIR lowering work here?”

Good:
“Should MIR lowering preserve high-level control flow (loops, matches),
or should it normalize everything into basic blocks with explicit jumps?”


5. Precision Over Verbosity (But Not Silence)

- Be concise where possible
- Be detailed where necessary
- Avoid filler explanations
- Avoid repeating the same information in different words

Never omit a critical explanation just to be short.


6. Safe Iteration Style

When modifying existing work:

- Do not rewrite unrelated sections
- Show only the relevant changes
- Respect existing structure, naming, and conventions
- Preserve intentional design constraints


7. Tone and Interaction

- Professional, collaborative, and constructive
- It is okay to show some personality 🙂
- Light emoji use is welcome, but sparing 😀

8. File Reading Expectations

Do not assume partial file reads are sufficient.

When working with a file:

- Read the entire file from start to end
- Do not rely on offsets, excerpts, or summaries unless explicitly instructed
- If the file is long, read it in multiple passes if necessary
- Re-read earlier sections if later sections depend on them

Token efficiency is not a priority in this repository.
Completeness and correctness take precedence over brevity.

If you are unsure whether you have full context:
- Say so explicitly
- Ask to read more rather than guessing

8. Using Agentic resources

Use agentic resources at your disposal, specifically subagents and skills.

Useful subagents:
- wiggum : will tell you if the task is complete
- MIR Expert: will help you thoroughly examine MIR-related code
- language designer: reviews and critiques language design decisions


Summary Checklist for Agents
----------------------------

Before responding, mentally confirm:

[ ] I planned the work with a TODO list
[ ] I explained why, not just what
[ ] I did not assume hidden knowledge
[ ] I verified completeness
[ ] I respected existing structure
[ ] I kept things clear and intentional
[ ] Read Files completely if possible for better context

If any box is unchecked — fix it first.


Good agents think before they type.
Great agents verify before they ship. 🚀

-- Other stuff:
Git commit command: GIT_SSH_COMMAND="ssh -i ~/.ssh/id_glyph -o IdentitiesOnly=yes" git push origin master

Utilities
----------
(1) Update glyph binary: use justfile:  `just build && just install`