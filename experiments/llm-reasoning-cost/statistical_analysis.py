#!/usr/bin/env python3
"""
Statistical Analysis for LLM Reasoning Cost Experiment

Performs statistical significance testing on repeated-measures data.
"""

import json
import statistics
from pathlib import Path
from collections import defaultdict
from scipy import stats
import numpy as np

def load_latest_results():
    """Load the most recent results file"""
    results_dir = Path(__file__).parent / "results" / "raw"
    json_files = sorted(results_dir.glob("results_*.json"))
    if not json_files:
        raise FileNotFoundError("No results files found")

    latest_file = json_files[-1]
    print(f"Loading: {latest_file}\n")

    with open(latest_file) as f:
        return json.load(f)

def group_by_condition(results):
    """Group results by (language, model, task)"""
    grouped = defaultdict(list)

    for r in results:
        key = (r['language'], r['model'], r['task'])
        grouped[key].append(r['adjusted_tokens'])

    return grouped

def calculate_statistics(data):
    """Calculate mean, std dev, and confidence interval"""
    if len(data) < 2:
        return {
            'mean': data[0] if data else 0,
            'std': 0,
            'n': len(data),
            'ci_lower': data[0] if data else 0,
            'ci_upper': data[0] if data else 0
        }

    mean = statistics.mean(data)
    std = statistics.stdev(data)
    n = len(data)

    # 95% confidence interval
    ci = stats.t.interval(0.95, n-1, loc=mean, scale=std/np.sqrt(n))

    return {
        'mean': mean,
        'std': std,
        'n': n,
        'ci_lower': ci[0],
        'ci_upper': ci[1]
    }

def paired_t_test(data1, data2):
    """Perform paired t-test between two conditions"""
    if len(data1) != len(data2):
        return None, None, None

    if len(data1) < 2:
        return None, None, None

    t_stat, p_value = stats.ttest_rel(data1, data2)

    # Calculate effect size (Cohen's d)
    diff = np.array(data1) - np.array(data2)
    d = np.mean(diff) / np.std(diff, ddof=1)

    return t_stat, p_value, d

def analyze_by_task(grouped, models, languages):
    """Analyze results by task across languages"""
    tasks = sorted(set(key[2] for key in grouped.keys()))

    print("="*80)
    print("STATISTICAL ANALYSIS BY TASK")
    print("="*80)

    for task in tasks:
        print(f"\n{'='*80}")
        print(f"Task: {task}")
        print(f"{'='*80}\n")

        # Collect data for this task (averaged across models)
        task_data = {}
        for lang in languages:
            values = []
            for model in models:
                key = (lang, model, task)
                if key in grouped:
                    values.extend(grouped[key])

            if values:
                task_data[lang] = {
                    'values': values,
                    'stats': calculate_statistics(values)
                }

        # Print descriptive statistics
        print("Descriptive Statistics:")
        print(f"{'Language':<12} {'Mean':>8} {'SD':>8} {'95% CI':>20} {'n':>4}")
        print("-"*60)

        for lang in languages:
            if lang in task_data:
                s = task_data[lang]['stats']
                ci = f"[{s['ci_lower']:6.1f}, {s['ci_upper']:6.1f}]"
                print(f"{lang:<12} {s['mean']:8.1f} {s['std']:8.1f} {ci:>20} {s['n']:4d}")

        # Perform pairwise comparisons with Glyph
        if 'glyph' in task_data:
            print(f"\n\nPairwise Comparisons (Glyph vs Others):")
            print(f"{'Comparison':<20} {'Mean Diff':>10} {'t-stat':>8} {'p-value':>10} {'Cohen\'s d':>10} {'Sig':>5}")
            print("-"*75)

            glyph_values = task_data['glyph']['values']

            for lang in languages:
                if lang == 'glyph' or lang not in task_data:
                    continue

                other_values = task_data[lang]['values']

                # Only compare if we have paired data
                if len(glyph_values) == len(other_values):
                    t_stat, p_value, cohens_d = paired_t_test(glyph_values, other_values)

                    if t_stat is not None:
                        mean_diff = task_data['glyph']['stats']['mean'] - task_data[lang]['stats']['mean']
                        sig = "***" if p_value < 0.001 else "**" if p_value < 0.01 else "*" if p_value < 0.05 else "ns"

                        print(f"Glyph vs {lang:<12} {mean_diff:10.1f} {t_stat:8.3f} {p_value:10.4f} {cohens_d:10.3f} {sig:>5}")

def overall_analysis(grouped, models, languages):
    """Overall analysis across all tasks"""
    print(f"\n\n{'='*80}")
    print("OVERALL ANALYSIS (Across All Tasks)")
    print(f"{'='*80}\n")

    # Collect all data for each language
    overall_data = {}
    for lang in languages:
        values = []
        for key, vals in grouped.items():
            if key[0] == lang:  # language matches
                values.extend(vals)

        if values:
            overall_data[lang] = {
                'values': values,
                'stats': calculate_statistics(values)
            }

    # Print descriptive statistics
    print("Descriptive Statistics:")
    print(f"{'Language':<12} {'Mean':>8} {'SD':>8} {'95% CI':>20} {'n':>4}")
    print("-"*60)

    for lang in languages:
        if lang in overall_data:
            s = overall_data[lang]['stats']
            ci = f"[{s['ci_lower']:6.1f}, {s['ci_upper']:6.1f}]"
            print(f"{lang:<12} {s['mean']:8.1f} {s['std']:8.1f} {ci:>20} {s['n']:4d}")

    # Overall ranking
    print(f"\n\nOverall Ranking:")
    ranked = sorted(overall_data.items(), key=lambda x: x[1]['stats']['mean'])
    for i, (lang, data) in enumerate(ranked, 1):
        medal = "🥇" if i == 1 else "🥈" if i == 2 else "🥉" if i == 3 else "  "
        print(f"{medal} {i}. {lang:<12} {data['stats']['mean']:6.1f} ± {data['stats']['std']:5.1f} tokens")

def main():
    # Load results
    results = load_latest_results()

    # Check if we have repeats
    grouped = group_by_condition(results)
    max_repeats = max(len(vals) for vals in grouped.values())

    print(f"Total experiments: {len(results)}")
    print(f"Max repeats per condition: {max_repeats}")

    if max_repeats < 2:
        print("\n⚠️  WARNING: Need at least 2 repeats per condition for statistical analysis")
        print("Run with: python3 runner.py --repeats 7\n")
        return

    print(f"✓ Sufficient data for statistical analysis\n")

    # Extract unique values
    languages = sorted(set(r['language'] for r in results))
    models = sorted(set(r['model'] for r in results))

    print(f"Languages: {', '.join(languages)}")
    print(f"Models: {', '.join(models)}\n")

    # Perform analyses
    analyze_by_task(grouped, models, languages)
    overall_analysis(grouped, models, languages)

    print(f"\n\n{'='*80}")
    print("LEGEND")
    print(f"{'='*80}")
    print("Significance levels: *** p<0.001, ** p<0.01, * p<0.05, ns = not significant")
    print("Cohen's d effect sizes: small (0.2), medium (0.5), large (0.8)")
    print(f"{'='*80}\n")

if __name__ == "__main__":
    main()
