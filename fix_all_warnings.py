#!/usr/bin/env python3
"""
Script to fix all GHC warnings in the Fluxus project.
"""

import re
import sys

# List of files and their fixes
fixes = [
    # Analysis/OwnershipInference.hs - unused import
    {
        'file': 'src/Fluxus/Analysis/OwnershipInference.hs',
        'old': 'import qualified Data.Text as T',
        'new': '-- import qualified Data.Text as T  -- unused'
    },
    # CodeGen/Go.hs - unused imports
    {
        'file': 'src/Fluxus/CodeGen/Go.hs',
        'old': 'import Control.Monad.State',
        'new': '-- import Control.Monad.State  -- unused'
    },
    {
        'file': 'src/Fluxus/CodeGen/Go.hs',
        'old': 'import Control.Monad.Writer',
        'new': '-- import Control.Monad.Writer  -- unused'
    },
    # Internal/Monad.hs - unused import
    {
        'file': 'src/Fluxus/Internal/Monad.hs',
        'old': 'import Control.Monad.IO.Class',
        'new': '-- import Control.Monad.IO.Class  -- unused'
    },
    # Runtime/Go.hs - unused imports
    {
        'file': 'src/Fluxus/Runtime/Go.hs',
        'old': 'import qualified Data.Vector as V',
        'new': '-- import qualified Data.Vector as V  -- unused'
    },
    {
        'file': 'src/Fluxus/Runtime/Go.hs',
        'old': 'import Foreign.C.Types',
        'new': '-- import Foreign.C.Types  -- unused'
    },
    {
        'file': 'src/Fluxus/Runtime/Go.hs',
        'old': 'import Foreign.C.String',
        'new': '-- import Foreign.C.String  -- unused'
    },
    # Runtime/Python.hs - unused import
    {
        'file': 'src/Fluxus/Runtime/Python.hs',
        'old': 'import Control.Monad.IO.Class',
        'new': '-- import Control.Monad.IO.Class  -- unused'
    },
]

def apply_fix(fix):
    """Apply a single fix to a file."""
    try:
        with open(fix['file'], 'r') as f:
            content = f.read()
        
        if fix['old'] in content:
            new_content = content.replace(fix['old'], fix['new'])
            with open(fix['file'], 'w') as f:
                f.write(new_content)
            print(f"✓ Fixed: {fix['file']}")
            return True
        else:
            print(f"✗ Not found in {fix['file']}: {fix['old'][:50]}...")
            return False
    except Exception as e:
        print(f"✗ Error fixing {fix['file']}: {e}")
        return False

def main():
    print("Fixing GHC warnings...")
    success_count = 0
    for fix in fixes:
        if apply_fix(fix):
            success_count += 1
    
    print(f"\nFixed {success_count}/{len(fixes)} issues")

if __name__ == '__main__':
    main()
