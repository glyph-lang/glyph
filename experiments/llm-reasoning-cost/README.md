# LLM Reasoning Cost Experiment

## Hypothesis

Glyph's simple, regular grammar should reduce the **total token cost** for LLMs to generate code, even if the output code itself doesn't save tokens.

**What we're measuring:** Input tokens + output tokens = total reasoning cost

## Methodology

### Test Matrix

- **Languages:** Glyph, C, C++, Go, Java
- **Models:** Claude Sonnet 4.6, Claude Haiku 4.5
- **Tasks:** 5 programming challenges (hello world, fibonacci, vectors, error handling, file I/O)

**Total:** 5 languages × 2 models × 5 tasks = **50 experiments**

### Fair Comparison

For Glyph, we provide a compressed language description (690 tokens) in the prompt. To make the comparison fair, we calculate:

```
adjusted_glyph_tokens = total_tokens - description_tokens
```

This isolates the reasoning cost of generating the code itself, not learning the language.

### Validation

All generated code is validated by compilation:
- Glyph: `glyph-cli check`
- C: `gcc -c`
- C++: `g++ -c`
- Go: `go build`
- Java: `javac`

## Setup

### Prerequisites

1. **Anthropic API Key:**
   ```bash
   export ANTHROPIC_API_KEY="your-key-here"
   ```

2. **Install dependencies:**
   ```bash
   pip install -r requirements.txt
   ```

3. **Build Glyph compiler** (for validation):
   ```bash
   cd ../..
   cargo build --release
   ```

4. **Have compilers installed** (for validation):
   ```bash
   gcc --version      # For C
   g++ --version      # For C++
   go version         # For Go
   javac -version     # For Java
   ```

## Running the Experiment

### Pilot Test (Recommended First)

Run just the fibonacci task to validate the setup:

```bash
python3 runner.py --pilot
```

This runs 5 experiments (glyph, c, cpp, go, java) with Sonnet.

### Full Experiment

Run the complete test matrix (50 experiments):

```bash
python3 runner.py
```

This will take several minutes and cost ~$1.00-$2.00 in API credits.

## Results

Results are saved to:

- `results/raw/results_{timestamp}.json` - Full experiment data
- `results/processed/token_counts.csv` - Summary table
- `generated_code/{language}/` - All generated code samples

The runner prints a summary showing average token costs by task and language.

## Results Summary

### Experiment Complete (490 runs, March 4 2026 rerun)

**Date:** March 4, 2026 (rerun of January 17, 2026 experiment)
**Models:** gpt-5.2-codex, gpt-4o-mini
**Languages:** 7 (Glyph, Rust, C, C++, Go, Java, Python)
**Repetitions:** 7 per condition

### Changes from January Run

1. **Fixed critical validation bug:** Glyph compiler was picking up stale `.glyph` temp files, causing 100% false failures. Fixed by using isolated temp directories.
2. **Updated glyph_description.md:** Now 890 tokens (was 690) reflecting current language: methods-in-structs, range loops, enums, match, accurate stdlib.
3. **Rebuilt compiler:** Includes all improvements since January (JSON parser V2, std/term, drop glue, etc.)

### Key Findings

**Glyph achieves 80% compilation success with gpt-5.2-codex** (28/35 runs pass `glyph-cli check`), with **100% on 4 of 5 tasks** (hello world, fibonacci, vector ops, error handling).

**Glyph ranks 3rd overall** in token cost (tied with Go), behind Rust and Python.

**Glyph wins error handling** outright at 281.6 tokens avg -- cheaper than all 6 other languages.

**Overall Average Tokens (Mean +/- SD):**
1. Rust: 249.5 +/- 89.2 tokens
2. Python: 266.6 +/- 139.3 tokens
3. **Glyph: 297.3 +/- 182.8 tokens** (80% compilation with codex)
4. Go: 297.3 +/- 111.5 tokens
5. C++: 354.1 +/- 180.7 tokens
6. C: 360.2 +/- 195.3 tokens
7. Java: 382.8 +/- 193.7 tokens

### Compilation Success Rates

| Language | codex | mini | Overall |
|----------|-------|------|---------|
| **Glyph** | **80%** | 0% | **40%** |
| Java | ~50% | ~50% | 50% |
| Python | ~50% | ~50% | 50% |
| C++ | ~33% | ~33% | 33% |
| C | ~30% | ~30% | 30% |

### Where Glyph Excels

- **Error handling:** Cheapest language (281.6 tokens). `Result<T,E>` is dramatically more efficient for LLMs than exceptions.
- **Simple algorithms:** 2nd cheapest for fibonacci (213.1 tokens). Beats all compiled languages.
- **Compilation success:** 100% on 4/5 tasks with codex -- competitive with Python and Java.

### Remaining Gap

- **File I/O:** 0% success. The stdlib lacks a high-level File API (`File::create`/`write`). LLMs generate idiomatic Rust-like code that doesn't compile. Adding this API would likely bring Glyph to 100% across all tasks.

### Full Analysis

See `results/processed/analysis.md` for complete task-by-task breakdowns and comparison with the January run.

## File Structure

```
llm-reasoning-cost/
  README.md                      # This file
  glyph_description.md           # 690-token Glyph language guide
  runner.py                      # Experiment automation script
  requirements.txt               # Python dependencies

  prompts/                       # Task descriptions
    01_hello_world.md
    02_fibonacci.md
    03_vector_ops.md
    04_error_handling.md
    05_file_io.md

  results/
    raw/                         # Full JSON responses
    processed/
      token_counts.csv           # Summary table

  generated_code/                # All generated code
    glyph/
    c/
    cpp/
    go/
    java/
```

## Analysis

After running the experiments, analyze the results:

1. Open `results/processed/token_counts.csv` in a spreadsheet
2. Calculate average token costs per language
3. Compute percentage differences
4. Look for patterns by task type
5. Compare Sonnet vs Haiku results

Key questions:
- Which language has lowest total reasoning cost?
- Does the pattern differ by task complexity?
- Are there specific task types where Glyph shines?
- How do the two models compare?

---

**Note:** This is a research experiment. Results will inform Glyph's design and the README's claims about LLM efficiency.
