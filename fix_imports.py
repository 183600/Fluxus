#!/usr/bin/env python3
import os
import re
import subprocess
from pathlib import Path

def find_used_symbols(file_path, module_name):
    """Find symbols used from a specific module in a Haskell file."""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
    except:
        return []
    
    # Get the module's exported symbols
    try:
        module_file = f"src{module_name.replace('.', '/')}.hs"
        result = subprocess.run(
            f"grep -A 200 '^module {module_name}' {module_file} | grep -B 200 '^where' | head -n -1",
            shell=True, capture_output=True, text=True
        )
        if result.returncode != 0:
            return []
        
        module_content = result.stdout
        
        # Extract exported symbols
        exports = []
        in_export_list = False
        for line in module_content.split('\n'):
            if '-- *' in line or '-- |' in line:
                continue
            if '(' in line and not in_export_list:
                in_export_list = True
            if in_export_list:
                # Extract symbols from the line
                symbols = re.findall(r'(\w+)(?:\(\.\.\.\))?', line)
                exports.extend([s for s in symbols if s and not s.startswith('--')])
            if ')' in line and in_export_list:
                break
        
        # Find which of these symbols are used in the target file
        used_symbols = []
        for symbol in exports:
            if re.search(r'\b' + re.escape(symbol) + r'\b', content):
                used_symbols.append(symbol)
        
        return used_symbols
    except:
        return []

def fix_imports():
    """Fix import statements in Haskell files."""
    src_dir = Path("src")
    
    # Find all Haskell files
    for hs_file in src_dir.rglob("*.hs"):
        try:
            with open(hs_file, 'r') as f:
                content = f.read()
        except:
            continue
        
        # Find import statements without explicit lists
        imports = re.findall(r'^import\s+([A-Z][A-Za-z0-9.]*)\s*$', content, re.MULTILINE)
        
        for module in imports:
            if module.startswith('Fluxus'):
                print(f"Processing {hs_file} - module {module}")
                used_symbols = find_used_symbols(hs_file, module)
                
                if used_symbols:
                    # Create explicit import list
                    import_list = ', '.join([f"{s}(..)" for s in sorted(set(used_symbols))])
                    new_import = f"import {module} ({import_list})"
                    
                    # Replace the old import
                    old_pattern = f"^import {module}\\s*$"
                    content = re.sub(old_pattern, new_import, content, flags=re.MULTILINE)
                    
                    # Write back the file
                    with open(hs_file, 'w') as f:
                        f.write(content)
                    
                    print(f"  Fixed: {module}")
        
    print("Done!")

if __name__ == "__main__":
    fix_imports()