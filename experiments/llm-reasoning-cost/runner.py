#!/usr/bin/env python3
"""
LLM Reasoning Cost Experiment Runner

Measures total token cost (input + output) for generating code in Glyph vs Rust vs Python
using Claude models via the Anthropic API.
"""

import openai
import os
import json
import csv
import time
import subprocess
import tempfile
from pathlib import Path
from dataclasses import dataclass, asdict
from typing import Literal

@dataclass
class ExperimentResult:
    """Result from a single experiment run"""
    language: str
    model: str
    task: str
    input_tokens: int
    output_tokens: int
    total_tokens: int
    adjusted_tokens: int  # For Glyph: total - description_cost
    generated_code: str
    compilation_success: bool
    compilation_error: str
    timestamp: str

class ExperimentRunner:
    def __init__(self):
        # Handle both OPENAI_API_KEY and OPEN_API_KEY
        api_key = os.environ.get("OPENAI_API_KEY") or os.environ.get("OPEN_API_KEY")
        if not api_key:
            raise ValueError("Please set OPENAI_API_KEY or OPEN_API_KEY environment variable")

        self.client = openai.OpenAI(api_key=api_key)
        self.base_dir = Path(__file__).parent
        self.results = []

        # Load Glyph description
        with open(self.base_dir / "glyph_description.md") as f:
            self.glyph_description = f.read()

        # Count description tokens
        import tiktoken
        enc = tiktoken.get_encoding("cl100k_base")
        self.glyph_description_tokens = len(enc.encode(self.glyph_description))
        print(f"Glyph description: {self.glyph_description_tokens} tokens\n")

    def load_prompt(self, task_name: str) -> str:
        """Load a task prompt from the prompts/ directory"""
        prompt_file = self.base_dir / "prompts" / f"{task_name}.md"
        with open(prompt_file) as f:
            return f.read()

    def build_prompt(self, language: str, task_prompt: str) -> str:
        """Build the full prompt for a given language"""
        if language == "glyph":
            return f"""{self.glyph_description}

---

{task_prompt}

Please write the solution in Glyph. Only output the code, no explanations."""

        elif language == "rust":
            return f"""{task_prompt}

Please write the solution in Rust. Only output the code, no explanations."""

        elif language == "c":
            return f"""{task_prompt}

Please write the solution in C. Only output the code, no explanations."""

        elif language == "cpp":
            return f"""{task_prompt}

Please write the solution in C++. Only output the code, no explanations."""

        elif language == "go":
            return f"""{task_prompt}

Please write the solution in Go. Only output the code, no explanations."""

        elif language == "java":
            return f"""{task_prompt}

Please write the solution in Java. Only output the code, no explanations."""

        elif language == "python":
            return f"""{task_prompt}

Please write the solution in Python. Only output the code, no explanations."""

        else:
            raise ValueError(f"Unknown language: {language}")

    def call_api(self, prompt: str, model: str) -> tuple[str, int, int]:
        """
        Call the OpenAI API and return (code, input_tokens, output_tokens)
        """
        # gpt-5.1-codex models use the responses API endpoint
        if "codex" in model.lower() or model.startswith("gpt-5"):
            response = self.client.responses.create(
                model=model,
                input=prompt
            )

            # Extract code from response
            # response.output is a list of items, including ResponseOutputMessage
            code_parts = []
            for item in response.output:
                # Look for message items with content
                if hasattr(item, 'type') and item.type == 'message' and hasattr(item, 'content'):
                    # content is a list of ResponseOutputText objects
                    for content_item in item.content:
                        if hasattr(content_item, 'text'):
                            code_parts.append(content_item.text)

            code = "".join(code_parts)

            return (
                code,
                response.usage.input_tokens,
                response.usage.output_tokens
            )
        else:
            # Other models use chat completions API
            response = self.client.chat.completions.create(
                model=model,
                max_completion_tokens=2048,
                temperature=0,
                messages=[{"role": "user", "content": prompt}]
            )

            # Extract code from response
            code = response.choices[0].message.content

            return (
                code,
                response.usage.prompt_tokens,
                response.usage.completion_tokens
            )

    def validate_code(self, language: str, code: str) -> tuple[bool, str]:
        """
        Validate generated code by compiling/running it
        Returns (success, error_message)
        """
        try:
            if language == "glyph":
                return self._validate_glyph(code)
            elif language == "rust":
                return self._validate_rust(code)
            elif language == "c":
                return self._validate_c(code)
            elif language == "cpp":
                return self._validate_cpp(code)
            elif language == "go":
                return self._validate_go(code)
            elif language == "java":
                return self._validate_java(code)
            elif language == "python":
                return self._validate_python(code)
            else:
                return False, f"Unknown language: {language}"
        except Exception as e:
            return False, str(e)

    def _validate_glyph(self, code: str) -> tuple[bool, str]:
        """Validate Glyph code by running glyph-cli check"""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.glyph', delete=False) as f:
            f.write(code)
            temp_file = f.name

        try:
            # Try to find glyph-cli
            glyph_cli = self.base_dir.parent.parent / "target" / "release" / "glyph-cli"
            if not glyph_cli.exists():
                glyph_cli = self.base_dir.parent.parent / "target" / "debug" / "glyph-cli"

            if not glyph_cli.exists():
                return False, "glyph-cli not found (need to build first)"

            result = subprocess.run(
                [str(glyph_cli), "check", temp_file],
                capture_output=True,
                text=True,
                timeout=10
            )

            if result.returncode == 0:
                return True, ""
            else:
                return False, result.stderr or result.stdout

        finally:
            os.unlink(temp_file)

    def _validate_rust(self, code: str) -> tuple[bool, str]:
        """Validate Rust code by compiling it"""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.rs', delete=False) as f:
            f.write(code)
            temp_file = f.name

        try:
            result = subprocess.run(
                ["rustc", "--crate-type=lib", temp_file, "-o", "/dev/null"],
                capture_output=True,
                text=True,
                timeout=30
            )

            if result.returncode == 0:
                return True, ""
            else:
                return False, result.stderr

        finally:
            os.unlink(temp_file)

    def _validate_c(self, code: str) -> tuple[bool, str]:
        """Validate C code by compiling it"""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.c', delete=False) as f:
            f.write(code)
            temp_file = f.name

        try:
            result = subprocess.run(
                ["gcc", "-c", temp_file, "-o", "/dev/null"],
                capture_output=True,
                text=True,
                timeout=30
            )

            if result.returncode == 0:
                return True, ""
            else:
                return False, result.stderr

        finally:
            os.unlink(temp_file)

    def _validate_cpp(self, code: str) -> tuple[bool, str]:
        """Validate C++ code by compiling it"""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.cpp', delete=False) as f:
            f.write(code)
            temp_file = f.name

        try:
            result = subprocess.run(
                ["g++", "-c", temp_file, "-o", "/dev/null"],
                capture_output=True,
                text=True,
                timeout=30
            )

            if result.returncode == 0:
                return True, ""
            else:
                return False, result.stderr

        finally:
            os.unlink(temp_file)

    def _validate_go(self, code: str) -> tuple[bool, str]:
        """Validate Go code by compiling it"""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.go', delete=False) as f:
            f.write(code)
            temp_file = f.name

        try:
            result = subprocess.run(
                ["go", "build", "-o", "/dev/null", temp_file],
                capture_output=True,
                text=True,
                timeout=30
            )

            if result.returncode == 0:
                return True, ""
            else:
                return False, result.stderr

        finally:
            os.unlink(temp_file)

    def _validate_java(self, code: str) -> tuple[bool, str]:
        """Validate Java code by compiling it"""
        # Extract class name from code
        import re
        match = re.search(r'public\s+class\s+(\w+)', code)
        if not match:
            return False, "Could not find public class declaration"

        class_name = match.group(1)

        with tempfile.TemporaryDirectory() as tmpdir:
            java_file = os.path.join(tmpdir, f"{class_name}.java")
            with open(java_file, 'w') as f:
                f.write(code)

            result = subprocess.run(
                ["javac", java_file],
                capture_output=True,
                text=True,
                timeout=30,
                cwd=tmpdir
            )

            if result.returncode == 0:
                return True, ""
            else:
                return False, result.stderr

    def _validate_python(self, code: str) -> tuple[bool, str]:
        """Validate Python code by syntax checking"""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            f.write(code)
            temp_file = f.name

        try:
            result = subprocess.run(
                ["python3", "-m", "py_compile", temp_file],
                capture_output=True,
                text=True,
                timeout=10
            )

            if result.returncode == 0:
                return True, ""
            else:
                return False, result.stderr

        finally:
            os.unlink(temp_file)

    def run_experiment(self, language: str, model: str, task: str) -> ExperimentResult:
        """Run a single experiment and return the result"""
        print(f"Running: {language:6s} | {model:25s} | {task:20s} ... ", end="", flush=True)

        # Load task prompt
        task_prompt = self.load_prompt(task)

        # Build full prompt
        full_prompt = self.build_prompt(language, task_prompt)

        # Call API
        code, input_tokens, output_tokens = self.call_api(full_prompt, model)
        total_tokens = input_tokens + output_tokens

        # Calculate adjusted tokens (subtract description for Glyph)
        if language == "glyph":
            adjusted_tokens = total_tokens - self.glyph_description_tokens
        else:
            adjusted_tokens = total_tokens

        # Validate code
        success, error = self.validate_code(language, code)

        # Save generated code
        code_dir = self.base_dir / "generated_code" / language
        code_file = code_dir / f"{task}_{model.split('-')[-1]}.{self._get_extension(language)}"
        code_dir.mkdir(parents=True, exist_ok=True)
        with open(code_file, 'w') as f:
            f.write(code)

        result = ExperimentResult(
            language=language,
            model=model,
            task=task,
            input_tokens=input_tokens,
            output_tokens=output_tokens,
            total_tokens=total_tokens,
            adjusted_tokens=adjusted_tokens,
            generated_code=code,
            compilation_success=success,
            compilation_error=error,
            timestamp=time.strftime("%Y-%m-%d %H:%M:%S")
        )

        status = "✓" if success else "✗"
        print(f"{status} | {total_tokens:4d} tokens (adj: {adjusted_tokens:4d})")

        return result

    def _get_extension(self, language: str) -> str:
        """Get file extension for a language"""
        extensions = {
            "glyph": "glyph",
            "rust": "rs",
            "c": "c",
            "cpp": "cpp",
            "go": "go",
            "java": "java",
            "python": "py"
        }
        return extensions[language]

    def save_results(self):
        """Save results to JSON and CSV"""
        # Save raw JSON
        json_file = self.base_dir / "results" / "raw" / f"results_{int(time.time())}.json"
        json_file.parent.mkdir(parents=True, exist_ok=True)
        with open(json_file, 'w') as f:
            json.dump([asdict(r) for r in self.results], f, indent=2)

        # Save CSV summary
        csv_file = self.base_dir / "results" / "processed" / "token_counts.csv"
        csv_file.parent.mkdir(parents=True, exist_ok=True)
        with open(csv_file, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow([
                "Language", "Model", "Task",
                "Input Tokens", "Output Tokens", "Total Tokens", "Adjusted Tokens",
                "Compilation Success", "Timestamp"
            ])
            for r in self.results:
                writer.writerow([
                    r.language, r.model, r.task,
                    r.input_tokens, r.output_tokens, r.total_tokens, r.adjusted_tokens,
                    r.compilation_success, r.timestamp
                ])

        print(f"\nResults saved to:")
        print(f"  - {json_file}")
        print(f"  - {csv_file}")

    def run_all(self, languages=None, models=None, tasks=None, pilot=False, repeats=1):
        """Run the full experiment matrix"""
        if languages is None:
            languages = ["glyph", "rust", "c", "cpp", "go", "java", "python"]
        if models is None:
            models = [
                "gpt-5.2-codex",
                "gpt-4o-mini"
            ]
        if tasks is None:
            tasks = [
                "01_hello_world",
                "02_fibonacci",
                "03_vector_ops",
                "04_error_handling",
                "05_file_io"
            ]

        if pilot:
            # Just run fibonacci with one model for testing
            languages = ["glyph", "rust", "c", "cpp", "go", "java", "python"]
            models = ["gpt-5.2-codex"]
            tasks = ["02_fibonacci"]
            repeats = 1
            print("=== PILOT MODE: Running fibonacci only ===\n")

        total = len(languages) * len(models) * len(tasks) * repeats
        print(f"Running {total} experiments ({repeats} repeats per condition):\n")

        for repeat in range(repeats):
            if repeats > 1:
                print(f"\n{'='*80}")
                print(f"REPEAT {repeat + 1} of {repeats}")
                print(f"{'='*80}\n")

            for language in languages:
                for model in models:
                    for task in tasks:
                        result = self.run_experiment(language, model, task)
                        self.results.append(result)
                        time.sleep(0.5)  # Small delay between API calls

        self.save_results()
        self.print_summary()

    def print_summary(self):
        """Print a summary of the results"""
        import statistics

        print("\n" + "="*80)
        print("SUMMARY")
        print("="*80 + "\n")

        # Group by task and language
        by_task = {}
        for r in self.results:
            key = (r.task, r.language)
            if key not in by_task:
                by_task[key] = []
            by_task[key].append(r.adjusted_tokens)

        # Determine if we have multiple runs (for statistics)
        has_repeats = any(len(values) > 2 for values in by_task.values())

        # Print averages (and std dev if repeats)
        tasks = sorted(set(r.task for r in self.results))
        languages = ["glyph", "rust", "c", "cpp", "go", "java", "python"]

        for task in tasks:
            print(f"\n{task}:")
            for lang in languages:
                key = (task, lang)
                if key in by_task:
                    values = by_task[key]
                    avg_tokens = statistics.mean(values)

                    if has_repeats and len(values) > 1:
                        std_tokens = statistics.stdev(values)
                        print(f"  {lang:10s}: {avg_tokens:6.1f} ± {std_tokens:5.1f} tokens (n={len(values)})")
                    else:
                        print(f"  {lang:10s}: {avg_tokens:6.1f} tokens (avg)")

            # Calculate savings vs C++
            if (task, "glyph") in by_task and (task, "cpp") in by_task:
                glyph_avg = statistics.mean(by_task[(task, "glyph")])
                cpp_avg = statistics.mean(by_task[(task, "cpp")])
                savings = ((cpp_avg - glyph_avg) / cpp_avg) * 100
                print(f"  → Glyph vs C++: {savings:+.1f}%")

def main():
    import argparse
    parser = argparse.ArgumentParser(description="Run LLM reasoning cost experiment")
    parser.add_argument("--pilot", action="store_true", help="Run pilot test only (fibonacci)")
    parser.add_argument("--repeats", type=int, default=1, help="Number of repetitions per condition (default: 1)")
    args = parser.parse_args()

    runner = ExperimentRunner()
    runner.run_all(pilot=args.pilot, repeats=args.repeats)

if __name__ == "__main__":
    main()
