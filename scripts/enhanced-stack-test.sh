#!/bin/bash

set -e

echo "=========================================="
echo "Running Enhanced Stack Test for Production"
echo "=========================================="

# Ensure we're in the project root
cd "$(dirname "$0")/.."

# Step 1: Clean previous builds
echo "Step 1: Cleaning previous builds..."
stack clean

# Step 2: Build with tests and benchmarks
echo "Step 2: Building project with tests and benchmarks..."
stack build --test --bench --no-run-tests --no-run-benchmarks

# Step 3: Run unit tests with coverage
echo "Step 3: Running unit tests with coverage..."
stack test --coverage

# Step 4: Run benchmarks
echo "Step 4: Running benchmarks..."
stack bench --benchmark-options=--output=bench-results.html

# Step 5: Run fluxus conversion tests
echo "Step 5: Running fluxus conversion tests..."
run_fluxus_tests() {
    echo "Testing fluxus conversion..."

    # Clean up previous output directory if it exists
    if [ -d "test/python-testsoutput" ]; then
        rm -rf "test/python-testsoutput"
    fi

    # Run fluxus conversion
    echo "Running: fluxus --python -O2 convert test/python-tests -o test/python-testsoutput"
    if fluxus --python -O2 convert test/python-tests -o test/python-testsoutput; then
        echo "✓ Fluxus conversion completed successfully"
        return 0
    else
        echo "✗ Fluxus conversion failed"
        return 1
    fi
}

# Step 6: Run Python file execution tests
echo "Step 6: Running Python file execution tests..."
run_python_execution_tests() {
    echo "Testing Python file execution..."

    local input_dir="test/python-tests"
    local output_dir="test/python-testsoutput"

    # Check if input directory exists
    if [ ! -d "$input_dir" ]; then
        echo "✗ Input directory $input_dir does not exist"
        return 1
    fi

    # Test each Python file in input directory
    for py_file in "$input_dir"/*.py; do
        if [ -f "$py_file" ]; then
            local filename=$(basename "$py_file")
            echo "Running: python $filename"

            # Capture output and check for errors
            if python "$py_file" > "/tmp/${filename}.input.out" 2> "/tmp/${filename}.input.err"; then
                echo "✓ $filename executed successfully"
            else
                echo "✗ $filename execution failed with error:"
                cat "/tmp/${filename}.input.err"
                return 1
            fi
        fi
    done
}

# Step 7: Run executable file execution tests
echo "Step 7: Running executable file execution tests..."
run_executable_execution_tests() {
    echo "Testing executable file execution..."

    local input_dir="test/python-tests"
    local output_dir="test/python-testsoutput"

    # Check if output directory exists
    if [ ! -d "$output_dir" ]; then
        echo "✗ Output directory $output_dir does not exist"
        return 1
    fi

    # Test each executable file in output directory
    for exec_file in "$output_dir"/*; do
        if [ -x "$exec_file" ] && [ -f "$exec_file" ]; then
            local filename=$(basename "$exec_file")
            echo "Running: $filename"

            # Capture output and check for errors
            if "$exec_file" > "/tmp/${filename}.exec.out" 2> "/tmp/${filename}.exec.err"; then
                echo "✓ $filename executed successfully"
            else
                echo "✗ $filename execution failed with error:"
                cat "/tmp/${filename}.exec.err"
                return 1
            fi
        fi
    done
}

# Step 8: Compare outputs
echo "Step 8: Comparing outputs..."
compare_outputs() {
    echo "Comparing execution outputs..."

    local input_dir="test/python-tests"
    local output_dir="test/python-testsoutput"
    local output_mismatch=false

    # Compare outputs for corresponding files
    for py_file in "$input_dir"/*.py; do
        if [ -f "$py_file" ]; then
            local filename=$(basename "$py_file" .py)
            local exec_file="$output_dir/$filename"

            if [ -x "$exec_file" ] && [ -f "$exec_file" ]; then
                echo "Comparing outputs for $filename..."

                # Compare outputs, ignoring whitespace differences
                if diff -u -w "/tmp/${filename}.input.out" "/tmp/${filename}.exec.out" > "/tmp/${filename}.diff"; then
                    echo "✓ Output matches for $filename"
                else
                    echo "✗ Output mismatch for $filename:"
                    cat "/tmp/${filename}.diff"
                    output_mismatch=true
                fi
            fi
        fi
    done

    if [ "$output_mismatch" = true ]; then
        echo "✗ Some outputs do not match"
        return 1
    else
        echo "✓ All outputs match"
        return 0
    fi
}

# Execute the fluxus and Python tests
echo "Executing fluxus conversion tests..."
if run_fluxus_tests; then
    echo "✓ Fluxus tests passed"
else
    echo "✗ Fluxus tests failed"
    exit 1
fi

echo ""
echo "Executing Python file execution tests..."
if run_python_execution_tests; then
    echo "✓ Python execution tests passed"
else
    echo "✗ Python execution tests failed"
    exit 1
fi

echo ""
echo "Executing executable file execution tests..."
if run_executable_execution_tests; then
    echo "✓ Executable execution tests passed"
else
    echo "✗ Executable execution tests failed"
    exit 1
fi

echo ""
echo "Comparing outputs..."
if compare_outputs; then
    echo "✓ Output comparison tests passed"
else
    echo "✗ Output comparison tests failed"
    exit 1
fi

echo ""
echo "Step 9: Running quality gate checks..."
if [ -f "scripts/quality-gate.sh" ]; then
    chmod +x scripts/quality-gate.sh
    ./scripts/quality-gate.sh "$@"
else
    echo "Warning: quality-gate.sh not found, skipping quality checks"
fi

echo ""
echo "=========================================="
echo "Enhanced Stack Test Completed Successfully"
echo "=========================================="
echo "The project has passed all production readiness checks"
echo ""
echo "Generated reports:"
echo "- Coverage: .stack-work/install/.../hpc/"
echo "- Benchmarks: bench-results.html"
echo "- Quality reports: See individual tool outputs above"