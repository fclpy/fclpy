#!/usr/bin/env python3
"""Coverage reporting tool for ANSI Common Lisp symbol implementation.

This script reads the canonical ANSI target list from docs/ansi_targets.txt
and compares it with the current registry to generate a coverage report.
"""

import os
import sys
from pathlib import Path


def load_ansi_targets():
    """Load the canonical ANSI target list from docs/ansi_targets.txt.
    
    Returns a set of uppercase symbol names.
    """
    targets_file = Path(__file__).parent.parent / 'docs' / 'ansi_targets.txt'
    
    symbols = set()
    with open(targets_file, 'r') as f:
        for line in f:
            line = line.strip()
            # Skip comments and empty lines
            if not line or line.startswith('#'):
                continue
            # Skip lines that are section headers
            if line.startswith('##'):
                continue
            # Skip footer lines
            if line.startswith('Total'):
                continue
            # Parse symbols (space-separated on each line)
            for symbol in line.split():
                # Clean up symbol (remove punctuation)
                symbol = symbol.strip('()[]{}')
                if symbol and not symbol.startswith('('):
                    symbols.add(symbol.upper())
    
    return symbols


def get_implemented_symbols():
    """Get the set of currently implemented symbols from the registry.
    
    Returns a set of uppercase symbol names.
    """
    try:
        # Set up the Lisp environment to ensure all symbols are registered
        import fclpy.lispenv as lispenv
        lispenv.setup_standard_environment()
        
        # Import the function registry
        from fclpy.lispfunc.registry import function_registry, special_registry
        
        symbols = set()
        
        # Add all registered functions
        for name in function_registry.keys():
            symbols.add(name.upper())
        
        # Add all registered special forms
        for name in special_registry.keys():
            symbols.add(name.upper())
        
        return symbols
    except Exception as e:
        print(f"Error: Could not import function registry: {e}")
        return set()


def generate_coverage_report(targets, implemented):
    """Generate a detailed coverage report.
    
    Returns a dict with coverage statistics.
    """
    implemented_symbols = targets & implemented
    missing_symbols = targets - implemented
    extra_symbols = implemented - targets
    
    total_targets = len(targets)
    total_implemented = len(implemented_symbols)
    coverage_percent = (total_implemented / total_targets * 100) if total_targets > 0 else 0
    
    return {
        'targets': targets,
        'implemented': implemented_symbols,
        'missing': missing_symbols,
        'extra': extra_symbols,
        'total_targets': total_targets,
        'total_implemented': total_implemented,
        'coverage_percent': coverage_percent
    }


def print_report(report, detail_level=1):
    """Print the coverage report in a readable format.
    
    detail_level: 0 = summary only, 1 = summary + lists, 2 = detailed
    """
    print("\n" + "=" * 70)
    print("ANSI Common Lisp Symbol Coverage Report")
    print("=" * 70)
    
    print(f"\nTarget Symbols: {report['total_targets']}")
    print(f"Implemented: {report['total_implemented']}")
    print(f"Missing: {len(report['missing'])}")
    print(f"Extra (not in target): {len(report['extra'])}")
    print(f"\nCoverage: {report['coverage_percent']:.1f}%")
    
    if detail_level >= 1:
        if report['missing']:
            print(f"\n--- Missing Symbols ({len(report['missing'])}) ---")
            missing_sorted = sorted(report['missing'])
            for i, sym in enumerate(missing_sorted):
                if (i + 1) % 5 == 0:
                    print(sym)
                else:
                    print(sym, end='  ')
            if len(missing_sorted) % 5 != 0:
                print()
        
        if report['extra']:
            print(f"\n--- Extra Symbols ({len(report['extra'])}) ---")
            extra_sorted = sorted(report['extra'])
            for i, sym in enumerate(extra_sorted):
                if (i + 1) % 5 == 0:
                    print(sym)
                else:
                    print(sym, end='  ')
            if len(extra_sorted) % 5 != 0:
                print()
    
    print("\n" + "=" * 70)


def generate_markdown_table(report):
    """Generate a markdown table of coverage statistics.
    
    Returns a string with markdown table.
    """
    lines = []
    lines.append("| Category | Count | Percentage |")
    lines.append("|----------|-------|-----------|")
    
    total = report['total_targets']
    lines.append(f"| Implemented | {report['total_implemented']} | {report['coverage_percent']:.1f}% |")
    lines.append(f"| Missing | {len(report['missing'])} | {100 - report['coverage_percent']:.1f}% |")
    lines.append(f"| Extra (not in target) | {len(report['extra'])} | - |")
    
    return "\n".join(lines)


def main():
    """Main entry point for coverage reporting."""
    import argparse
    
    parser = argparse.ArgumentParser(
        description='Generate ANSI Common Lisp symbol coverage report'
    )
    parser.add_argument(
        '--detail', '-d',
        type=int,
        default=1,
        choices=[0, 1, 2],
        help='Detail level: 0=summary, 1=with lists, 2=detailed'
    )
    parser.add_argument(
        '--markdown', '-m',
        action='store_true',
        help='Output markdown table instead of text report'
    )
    parser.add_argument(
        '--min-coverage',
        type=float,
        default=0,
        help='Minimum coverage percentage (fail if below this)'
    )
    parser.add_argument(
        '--json',
        action='store_true',
        help='Output JSON format'
    )
    
    args = parser.parse_args()
    
    # Load targets and implemented symbols
    targets = load_ansi_targets()
    implemented = get_implemented_symbols()
    
    # Generate report
    report = generate_coverage_report(targets, implemented)
    
    # Output report
    if args.json:
        import json
        output = {
            'targets': len(targets),
            'implemented': len(report['implemented']),
            'missing': len(report['missing']),
            'extra': len(report['extra']),
            'coverage_percent': report['coverage_percent'],
            'missing_symbols': sorted(report['missing']),
            'extra_symbols': sorted(report['extra'])
        }
        print(json.dumps(output, indent=2))
    elif args.markdown:
        print(generate_markdown_table(report))
    else:
        print_report(report, detail_level=args.detail)
    
    # Check minimum coverage
    if report['coverage_percent'] < args.min_coverage:
        print(f"\nERROR: Coverage {report['coverage_percent']:.1f}% is below minimum {args.min_coverage}%")
        return 1
    
    return 0


if __name__ == '__main__':
    sys.exit(main())
