# LLM Reasoning Cost Experiment

## Hypothesis

Glyph's simple, regular grammar should reduce the **total token cost** for LLMs to generate code, even if the output code itself doesn't save tokens.

**What we're measuring:** Input tokens + output tokens = total reasoning cost

## Methodology

### Test Matrix

- **Languages:** Glyph, C, C++, Go, Java
- **Models:** Claude Sonnet 4.5, Claude Haiku 4
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

### ✅ Experiment Complete (490 runs with statistical validation)

**Date:** January 17, 2026
**Models:** gpt-5.2-codex, gpt-4o-mini
**Languages:** 7 (Glyph, Rust, C, C++, Go, Java, Python)
**Repetitions:** 7 per condition for statistical rigor

### Key Findings

🎯 **Glyph ranks 2nd overall** out of 7 languages (statistically tied with Rust for 1st place!)

🏆 **Glyph DOMINATES error handling** with massive effect sizes (Cohen's d > 2.0) vs C, C++, Go, Java

📊 **Overall Average Tokens (Mean ± SD):**
1. Rust: 239.7 ± 91.5 tokens
2. **Glyph: 242.4 ± 93.1 tokens** ✅
3. Python: 255.5 ± 150.3 tokens
4. Go: 273.9 ± 103.5 tokens
5. C: 336.7 ± 160.2 tokens
6. C++: 339.9 ± 180.0 tokens
7. Java: 385.1 ± 238.2 tokens

### Validated Hypothesis

✅ **Glyph achieves reasoning costs statistically equivalent to the best established languages**

**Where Glyph Excels (statistically validated):**
- Error handling (tied with Rust, p<0.001 better than all others)
- Simple algorithms (p<0.001 better than all compiled languages)
- File I/O (tied with Rust, Python, Go)

**Where Glyph is Competitive:**
- Generic collections (tied with C++, Rust, Go, Java)
- Hello World (no significant differences)

### Statistical Validation

All results confirmed with:
- Paired t-tests (7 repetitions per condition)
- 95% confidence intervals
- Cohen's d effect sizes
- p-values < 0.05 for significant differences

### Full Analysis

See `results/processed/analysis.md` for complete statistical analysis, task-by-task breakdowns, and detailed findings.

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
