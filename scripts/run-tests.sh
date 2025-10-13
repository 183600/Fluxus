#!/bin/bash

# Fluxus Test Runner Script
# This script runs the test suite with various options

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default values
COVERAGE=false
VERBOSE=false
FAST=false
SPECIFIC_TEST=""
PARALLEL=true

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --coverage)
      COVERAGE=true
      shift
      ;;
    --verbose|-v)
      VERBOSE=true
      shift
      ;;
    --fast)
      FAST=true
      shift
      ;;
    --test|-t)
      SPECIFIC_TEST="$2"
      shift 2
      ;;
    --no-parallel)
      PARALLEL=false
      shift
      ;;
    --help|-h)
      echo "Usage: $0 [OPTIONS]"
      echo ""
      echo "Options:"
      echo "  --coverage        Run tests with coverage reporting"
      echo "  --verbose, -v     Run tests with verbose output"
      echo "  --fast            Skip performance tests for faster execution"
      echo "  --test, -t NAME   Run specific test suite"
      echo "  --no-parallel     Disable parallel test execution"
      echo "  --help, -h        Show this help message"
      echo ""
      echo "Examples:"
      echo "  $0                          # Run all tests"
      echo "  $0 --coverage               # Run with coverage"
      echo "  $0 --fast                   # Skip performance tests"
      echo "  $0 --test \"Parser\"          # Run only parser tests"
      exit 0
      ;;
    *)
      echo -e "${RED}Unknown option: $1${NC}"
      exit 1
      ;;
  esac
done

# Build the project first
echo -e "${BLUE}Building project...${NC}"
stack build

# Prepare test command
TEST_CMD="stack test"
TEST_ARGS=""

if [ "$COVERAGE" = true ]; then
  echo -e "${YELLOW}Running tests with coverage...${NC}"
  TEST_CMD="$TEST_CMD --coverage"
fi

if [ "$VERBOSE" = true ]; then
  TEST_ARGS="$TEST_ARGS -v"
fi

if [ "$FAST" = true ]; then
  echo -e "${YELLOW}Skipping performance tests...${NC}"
  TEST_ARGS="$TEST_ARGS --skip Performance"
fi

if [ -n "$SPECIFIC_TEST" ]; then
  echo -e "${YELLOW}Running specific test: $SPECIFIC_TEST${NC}"
  TEST_ARGS="$TEST_ARGS -m $SPECIFIC_TEST"
fi

if [ "$PARALLEL" = false ]; then
  TEST_ARGS="$TEST_ARGS -j1"
fi

# Run tests
echo -e "${BLUE}Running tests...${NC}"
if [ -n "$TEST_ARGS" ]; then
  $TEST_CMD --test-arguments="$TEST_ARGS"
else
  $TEST_CMD
fi

# Generate coverage report if requested
if [ "$COVERAGE" = true ]; then
  echo -e "${BLUE}Generating coverage report...${NC}"
  stack hpc report --all
  
  # Check if coverage meets requirements
  COVERAGE_DIR=".stack-work/install"
  if [ -d "$COVERAGE_DIR" ]; then
    echo -e "${GREEN}Coverage report generated successfully${NC}"
    echo -e "${YELLOW}View HTML report: find .stack-work -name 'hpc_index.html'${NC}"
  fi
fi

echo -e "${GREEN}Tests completed successfully!${NC}"
