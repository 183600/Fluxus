#!/usr/bin/env node

const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

// Test configuration
const FLUXUS_BINARY = './bin/fluxus';
const TEST_DIR = './test/python-tests';
const TEMP_DIR = './test/temp';

// Test results
let totalTests = 0;
let passedTests = 0;
let failedTests = 0;
const testResults = [];

// Parse command line arguments
const args = process.argv.slice(2);
const verbose = args.includes('--verbose') || args.includes('-v');
const debug = args.includes('--debug') || args.includes('-d');

// Colors for output
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m'
};

function log(message, color = colors.reset) {
  console.log(color + message + colors.reset);
}

function debugLog(message) {
  if (debug) {
    log('[DEBUG] ' + message, colors.cyan);
  }
}

function createTempDirectory() {
  if (!fs.existsSync(TEMP_DIR)) {
    fs.mkdirSync(TEMP_DIR, { recursive: true });
  }
}

function cleanupTempDirectory() {
  if (fs.existsSync(TEMP_DIR)) {
    fs.rmSync(TEMP_DIR, { recursive: true, force: true });
  }
}

function runPythonTest(pythonFile, expectedOutput) {
  debugLog(`Testing ${pythonFile}`);
  
  try {
    // Step 1: Run Python code to get expected output
    const pythonOutput = execSync(`python3 ${pythonFile}`, { encoding: 'utf8' }).trim();
    debugLog(`Python output: "${pythonOutput}"`);
    
    // Step 2: Compile Python to C++
    const baseName = path.basename(pythonFile, '.py');
    const cppFile = path.join(TEMP_DIR, `${baseName}.cpp`);
    const compiledExe = path.join(TEMP_DIR, `${baseName}`);
    
    debugLog(`Compiling ${pythonFile} to C++`);
    const compileResult = execSync(`${FLUXUS_BINARY} ${pythonFile}`, { encoding: 'utf8' });
    debugLog(`Compile result: ${compileResult}`);
    
    // Step 3: Check if C++ file was generated
    if (!fs.existsSync(`${baseName}.cpp`)) {
      throw new Error('C++ file not generated');
    }
    
    // Move C++ file to temp directory
    fs.renameSync(`${baseName}.cpp`, cppFile);
    
    // Step 4: Compile C++ to executable
    debugLog(`Compiling C++ to executable`);
    execSync(`g++ -std=c++17 -o ${compiledExe} ${cppFile}`, { encoding: 'utf8' });
    
    // Step 5: Run compiled executable
    debugLog(`Running compiled executable`);
    const cppOutput = execSync(`${compiledExe}`, { encoding: 'utf8' }).trim();
    debugLog(`C++ output: "${cppOutput}"`);
    
    // Step 6: Compare outputs
    const outputsMatch = pythonOutput === cppOutput;
    
    return {
      success: outputsMatch,
      pythonOutput,
      cppOutput,
      error: outputsMatch ? null : 'Output mismatch'
    };
    
  } catch (error) {
    debugLog(`Test failed: ${error.message}`);
    return {
      success: false,
      pythonOutput: null,
      cppOutput: null,
      error: error.message
    };
  }
}

function runTestGroup(groupName, testFiles) {
  log(`\n🧪 Running ${groupName} tests...`, colors.blue);
  
  for (const testFile of testFiles) {
    totalTests++;
    const testPath = path.join(TEST_DIR, testFile);
    const testName = testFile.replace('.py', '');
    
    log(`  📄 ${testName}`, colors.cyan);
    
    // Read expected output from .expected file if it exists
    const expectedFile = testPath.replace('.py', '.expected');
    let expectedOutput = null;
    
    if (fs.existsSync(expectedFile)) {
      expectedOutput = fs.readFileSync(expectedFile, 'utf8').trim();
    }
    
    const result = runPythonTest(testPath, expectedOutput);
    
    if (result.success) {
      passedTests++;
      log(`    ✅ PASSED`, colors.green);
      if (verbose) {
        log(`       Python: "${result.pythonOutput}"`, colors.yellow);
        log(`       C++:    "${result.cppOutput}"`, colors.yellow);
      }
    } else {
      failedTests++;
      log(`    ❌ FAILED: ${result.error}`, colors.red);
      if (verbose && result.pythonOutput && result.cppOutput) {
        log(`       Python: "${result.pythonOutput}"`, colors.yellow);
        log(`       C++:    "${result.cppOutput}"`, colors.yellow);
      }
    }
    
    testResults.push({
      name: testName,
      group: groupName,
      success: result.success,
      error: result.error,
      pythonOutput: result.pythonOutput,
      cppOutput: result.cppOutput
    });
  }
}

function generateTestReport() {
  const report = {
    timestamp: new Date().toISOString(),
    summary: {
      total: totalTests,
      passed: passedTests,
      failed: failedTests,
      successRate: totalTests > 0 ? (passedTests / totalTests * 100).toFixed(1) : 0
    },
    results: testResults
  };
  
  const reportPath = path.join(TEMP_DIR, 'test-report.json');
  fs.writeFileSync(reportPath, JSON.stringify(report, null, 2));
  
  return report;
}

function main() {
  log('🚀 Fluxus Compiler Test Suite', colors.bright + colors.blue);
  log('================================', colors.bright + colors.blue);
  
  // Create temp directory
  createTempDirectory();
  
  try {
    // Check if Fluxus binary exists
    if (!fs.existsSync(FLUXUS_BINARY)) {
      throw new Error(`Fluxus binary not found at ${FLUXUS_BINARY}`);
    }
    
    // Check if test directory exists
    if (!fs.existsSync(TEST_DIR)) {
      log(`Test directory ${TEST_DIR} not found, creating sample tests...`, colors.yellow);
      fs.mkdirSync(TEST_DIR, { recursive: true });
      createSampleTests();
    }
    
    // Get test files
    const testFiles = fs.readdirSync(TEST_DIR)
      .filter(file => file.endsWith('.py'))
      .sort();
    
    if (testFiles.length === 0) {
      log('No test files found, creating sample tests...', colors.yellow);
      createSampleTests();
    }
    
    // Group tests by category
    const basicTests = testFiles.filter(f => f.startsWith('basic_'));
    const advancedTests = testFiles.filter(f => f.startsWith('advanced_'));
    const featureTests = testFiles.filter(f => f.startsWith('feature_'));
    const otherTests = testFiles.filter(f => 
      !f.startsWith('basic_') && !f.startsWith('advanced_') && !f.startsWith('feature_')
    );
    
    // Run tests by group
    if (basicTests.length > 0) runTestGroup('Basic', basicTests);
    if (advancedTests.length > 0) runTestGroup('Advanced', advancedTests);
    if (featureTests.length > 0) runTestGroup('Feature', featureTests);
    if (otherTests.length > 0) runTestGroup('Other', otherTests);
    
    // Generate and display report
    const report = generateTestReport();
    
    log('\n📊 Test Summary', colors.bright + colors.blue);
    log('================', colors.bright + colors.blue);
    log(`Total Tests: ${report.summary.total}`, colors.cyan);
    log(`Passed: ${report.summary.passed}`, colors.green);
    log(`Failed: ${report.summary.failed}`, colors.red);
    log(`Success Rate: ${report.summary.successRate}%`, 
      report.summary.successRate >= 90 ? colors.green : 
      report.summary.successRate >= 70 ? colors.yellow : colors.red);
    
    log(`\n📄 Detailed report saved to: ${path.join(TEMP_DIR, 'test-report.json')}`, colors.cyan);
    
    // Exit with appropriate code
    process.exit(failedTests > 0 ? 1 : 0);
    
  } catch (error) {
    log(`\n❌ Test runner error: ${error.message}`, colors.red);
    process.exit(1);
  } finally {
    // Cleanup
    if (!debug) {
      cleanupTempDirectory();
    }
  }
}

function createSampleTests() {
  log('Creating sample test files...', colors.yellow);
  
  // Basic arithmetic test
  const basicArithmetic = `# Basic arithmetic test
x = 10
y = 20
result = x + y
print(result)`;
  
  fs.writeFileSync(path.join(TEST_DIR, 'basic_arithmetic.py'), basicArithmetic);
  fs.writeFileSync(path.join(TEST_DIR, 'basic_arithmetic.expected'), '30');
  
  // Function test
  const functionTest = `# Function test
def add(a, b):
    return a + b

result = add(5, 3)
print(result)`;
  
  fs.writeFileSync(path.join(TEST_DIR, 'basic_function.py'), functionTest);
  fs.writeFileSync(path.join(TEST_DIR, 'basic_function.expected'), '8');
  
  // List comprehension test
  const listCompTest = `# List comprehension test
numbers = [1, 2, 3, 4, 5]
squares = [x * x for x in numbers]
print(squares)`;
  
  fs.writeFileSync(path.join(TEST_DIR, 'feature_list_comprehension.py'), listCompTest);
  fs.writeFileSync(path.join(TEST_DIR, 'feature_list_comprehension.expected'), '[1, 4, 9, 16, 25]');
  
  // F-string test
  const fstringTest = `# F-string test
name = "World"
age = 25
message = f"Hello {name}! You are {age} years old."
print(message)`;
  
  fs.writeFileSync(path.join(TEST_DIR, 'feature_fstring.py'), fstringTest);
  fs.writeFileSync(path.join(TEST_DIR, 'feature_fstring.expected'), 'Hello World! You are 25 years old.');
  
  log('Sample tests created successfully!', colors.green);
}

// Run the test suite
if (require.main === module) {
  main();
}

module.exports = { runPythonTest, createSampleTests };