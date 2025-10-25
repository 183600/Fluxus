#!/usr/bin/env python3
"""
Script to fix Haskell warnings systematically
"""

import re
import os
from pathlib import Path

# Warning patterns and fixes
FIXES = {
    # Unused imports - comment them out
    'unused_import': {
        'pattern': r"The import of '([^']+)' is redundant",
        'action': 'comment_import'
    },
    'qualified_unused_import': {
        'pattern': r"The qualified import of '([^']+)' is redundant",
        'action': 'comment_import'
    },
    # Unused top-level bindings - prefix with underscore
    'unused_top_bind': {
        'pattern': r"Defined but not used: '([^']+)'",
        'action': 'prefix_underscore'
    },
    # Name shadowing - rename variable
    'name_shadow': {
        'pattern': r"This binding for '([^']+)' shadows",
        'action': 'rename_shadow'
    }
}

def parse_warning_file(filename):
    """Parse warnings_full.txt and extract structured warnings"""
    warnings = []
    with open(filename, 'r') as f:
        content = f.read()
    
    # Split by file compilation markers
    sections = re.split(r'\[\s*\d+\s+of\s+\d+\]\s+Compiling\s+', content)
    
    for section in sections[1:]:  # Skip first empty section
        lines = section.split('\n')
        if not lines:
            continue
            
        # Extract module name from first line
        module_name = lines[0].strip()
        
        # Find all warnings in this section
        warning_pattern = r'(/[^\s]+\.hs):(\d+):(\d+):\s+warning:\s+\[([^\]]+)\]\s+\[([^\]]+)\]\s*\n\s*(.+?)(?=\n/|\n\[|\Z)'
        
        for match in re.finditer(warning_pattern, section, re.DOTALL):
            filepath, line, col, code1, code2, message = match.groups()
            warnings.append({
                'file': filepath,
                'line': int(line),
                'col': int(col),
                'code': code2,
                'message': message.strip(),
                'module': module_name
            })
    
    return warnings

def fix_unused_import(filepath, line_num, import_name):
    """Comment out an unused import"""
    with open(filepath, 'r') as f:
        lines = f.readlines()
    
    # Find the import line
    for i in range(max(0, line_num - 5), min(len(lines), line_num + 5)):
        if import_name in lines[i] and 'import' in lines[i]:
            # Comment it out
            if not lines[i].strip().startswith('--'):
                lines[i] = '-- ' + lines[i]
                print(f"Commented out import in {filepath}:{i+1}")
                break
    
    with open(filepath, 'w') as f:
        f.writelines(lines)

def fix_unused_binding(filepath, line_num, binding_name):
    """Prefix unused binding with underscore"""
    with open(filepath, 'r') as f:
        lines = f.readlines()
    
    if line_num > len(lines):
        return
    
    line = lines[line_num - 1]
    # Replace the binding name with _binding_name
    if binding_name in line and not line.strip().startswith('--'):
        # Be careful to only replace the definition, not uses
        pattern = r'\b' + re.escape(binding_name) + r'\b'
        # Only replace if it's at the start of a definition
        if re.match(r'^\s*' + pattern, line):
            new_line = re.sub(pattern, '_' + binding_name, line, count=1)
            lines[line_num - 1] = new_line
            print(f"Prefixed {binding_name} with underscore in {filepath}:{line_num}")
    
    with open(filepath, 'w') as f:
        f.writelines(lines)

def main():
    warnings = parse_warning_file('warnings_full.txt')
    print(f"Found {len(warnings)} warnings")
    
    # Group warnings by type
    by_type = {}
    for w in warnings:
        code = w['code']
        if code not in by_type:
            by_type[code] = []
        by_type[code].append(w)
    
    print("\nWarnings by type:")
    for code, warns in sorted(by_type.items()):
        print(f"  {code}: {len(warns)}")
    
    # Fix unused imports
    if '-Wunused-imports' in by_type:
        print("\nFixing unused imports...")
        for w in by_type['-Wunused-imports']:
            match = re.search(r"The (?:qualified )?import of '([^']+)'", w['message'])
            if match:
                import_name = match.group(1)
                fix_unused_import(w['file'], w['line'], import_name)
    
    # Fix unused top-level bindings
    if '-Wunused-top-binds' in by_type:
        print("\nFixing unused top-level bindings...")
        for w in by_type['-Wunused-top-binds']:
            match = re.search(r"Defined but not used: '([^']+)'", w['message'])
            if match:
                binding_name = match.group(1)
                fix_unused_binding(w['file'], w['line'], binding_name)

if __name__ == '__main__':
    main()
