WHITEPAPER_PLAN.txt — Token-Efficiency Evaluation of Glyph vs Mainstream Languages
=================================================================================

Purpose
-------
This white paper defines a rigorous, reproducible methodology to compare
**LLM token cost and repair behavior** across programming languages, with
Glyph as the focal point.

The goal is NOT to compare raw runtime performance or human ergonomics alone,
but to measure **total tokens-to-green (TTG)** in AI-assisted development
workflows.

Core thesis:
> Languages with lower semantic ambiguity, fewer hidden transformations,
> and compiler-visible structure require fewer LLM tokens to reach
> correct, working software.

Glyph is designed explicitly to optimize for this regime.

-------------------------------------------------------------------------------
1) Key metric definitions
-------------------------------------------------------------------------------

1.1 Primary metric: Tokens-to-Green (TTG)
-----------------------------------------
TTG = total number of LLM tokens consumed until the program:
- compiles successfully
- passes all tests
- produces correct observable behavior

TTG includes:
- initial prompt tokens
- completion tokens
- repair / iteration tokens
- compiler error feedback tokens (if included in context)

TTG explicitly excludes:
- tokens after the program is already correct
- human commentary not related to code generation

1.2 Secondary metrics
---------------------
- Repair iterations: number of compile/test failures before success
- Tokens per iteration
- Failure density: failures per 1k tokens
- Edit TTG: tokens required to implement a small change request
- Cold-start penalty: TTG difference between zero-shot and few-shot setups

-------------------------------------------------------------------------------
2) Experimental tasks
-------------------------------------------------------------------------------

A single coherent project is used across all languages, implemented in stages.

Project name:
  Mini Service + CLI

Description:
A small HTTP JSON service with persistence and a CLI client.

-------------------------------------------------------------------------------
2.1 Milestone breakdown
-------------------------------------------------------------------------------

M1: HTTP health endpoint
- Start HTTP server
- GET /health → {"status":"ok"}
- Single-threaded, blocking IO allowed

M2: CRUD endpoint
- GET /items/:id
- POST /items with JSON body
- In-memory store

M3: Persistence
- File-backed storage (append or overwrite)
- Reload state on startup

M4: Logging + configuration
- Config file or env-based port selection
- Logging of requests

M5: Refactor task
- Rename a field in Item
- Update all affected code and tests

Each milestone must pass the same black-box test suite.

-------------------------------------------------------------------------------
3) Languages evaluated
-------------------------------------------------------------------------------

Baseline languages:
- Glyph (target language)
- Rust
- C
- C++
- Python

Optional extensions:
- Go
- Zig
- Swift

-------------------------------------------------------------------------------
4) Library usage policy
-------------------------------------------------------------------------------

Two tracks are evaluated independently.

4.1 Track A: Stdlib-only
------------------------
- Only language standard library permitted
- No third-party frameworks
- Measures language + stdlib ergonomics

4.2 Track B: Minimal ecosystem
-------------------------------
- Small, widely-accepted libraries allowed
- Must be declared explicitly per language
- Goal: reflect realistic usage without large frameworks

Glyph policy:
- Uses its standard library only
- No macro or framework shortcuts

-------------------------------------------------------------------------------
5) Models and tokenization
-------------------------------------------------------------------------------

5.1 Models
----------
Use at least two representative LLMs:
- One frontier-capability model
- One cost-efficient / fast model

Models may be anonymized in publication.

5.2 Token accounting
--------------------
- Token counts measured using each model’s native tokenizer
- Additionally record:
  - character count
  - line count
for sanity comparison

All reported TTG values include tokenizer-specific counts.

-------------------------------------------------------------------------------
6) Prompting protocol
-------------------------------------------------------------------------------

6.1 Initial prompt template
---------------------------
Each run begins with:
- problem statement
- constraints
- language-specific instructions
- starter files (if any)

Prompt structure is identical across languages, except:
- language name
- LLMS.txt content for Glyph

6.2 Glyph-specific instruction material
---------------------------------------
Glyph runs include:
- LLMS.txt (language rules, patterns, constraints)
- Optional: small canonical example pack (few-shot condition)

Two conditions are tested:
- Zero-shot Glyph: LLMS.txt only
- Few-shot Glyph: LLMS.txt + ~10 idiomatic examples

-------------------------------------------------------------------------------
7) Execution protocol
-------------------------------------------------------------------------------

For each milestone:
1. Provide prompt + context
2. Allow model to generate code
3. Compile and/or run tests
4. Feed compiler/runtime errors back to model verbatim
5. Repeat until success or token budget exhausted

Stopping criteria:
- Success (green)
- Token budget exceeded
- Maximum iteration count exceeded

All iterations are logged.

-------------------------------------------------------------------------------
8) Data collected per run
-------------------------------------------------------------------------------

Per iteration:
- Prompt tokens
- Completion tokens
- Total tokens
- Compiler errors (raw text)
- Runtime errors (raw text)
- Success/failure flag

Per milestone:
- Total TTG
- Iteration count
- Failure categories (syntax, typing, logic, IO)

-------------------------------------------------------------------------------
9) Expected Glyph advantages (hypotheses)
-------------------------------------------------------------------------------

Hypothesis H1:
Glyph requires fewer TTG than Rust/C++ due to:
- no macros
- no overloading
- no generics (initially)
- deterministic interpolation

Hypothesis H2:
Glyph shows fewer repair iterations due to:
- compiler-visible formatting
- simpler ownership surface
- explicit interfaces

Hypothesis H3:
Glyph cold-start penalty is low:
- LLMS.txt enables rapid competence
- few-shot examples significantly reduce TTG

-------------------------------------------------------------------------------
10) Threats to validity
-------------------------------------------------------------------------------

- Different stdlib maturity levels
- Library quality differences
- Model familiarity bias (Rust/Python favored)
- Tokenizers differ across models
- Human bias in prompt phrasing

Mitigations:
- explicit reporting
- dual-track evaluation
- reproducible logs
- open benchmark scripts

-------------------------------------------------------------------------------
11) Deliverables
-------------------------------------------------------------------------------

- This white paper (PDF + markdown)
- Open-source benchmark harness
- Reproducible logs (JSON/CSV)
- Glyph LLMS.txt and example pack
- Charts:
  - TTG per language per milestone
  - Repair iteration histograms
  - Cold-start vs few-shot comparison

-------------------------------------------------------------------------------
12) Intended audience
-------------------------------------------------------------------------------

- Programming language designers
- AI tooling researchers
- Infrastructure engineers
- Organizations evaluating AI-assisted development cost

-------------------------------------------------------------------------------
13) Publication strategy
-------------------------------------------------------------------------------

- GitHub repository (benchmark + paper)
- Hacker News / Reddit PL communities
- Academic-style arXiv preprint (optional)
- Blog post summarizing results
- Follow-up: “Designing languages for LLMs” essay

-------------------------------------------------------------------------------
End of file.
-------------------------------------------------------------------------------
