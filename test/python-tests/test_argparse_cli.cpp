#include <iostream>
#include <string>
#include <vector>
#include <optional>
#include <unordered_map>
#include <tuple>
#include <utility>
#include <type_traits>
#include <functional>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <algorithm>
#include <typeinfo>
#include <cstring>

using namespace std;

// helpers
template<typename T> static inline std::ostream& operator<<(std::ostream& os, const std::vector<T>& v);
template<typename T> static inline std::string to_str(const T& v){ std::ostringstream os; os<<v; return os.str(); }
static inline std::string to_str(const std::string& s){ return s; }
static inline std::string to_str(const char* s){ return std::string(s); }
static inline std::string to_str(double v){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1)<<v; return os.str(); }
static inline std::string to_str(float v){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1)<<v; return os.str(); }
template<typename T> static inline int to_int(const T& v){ if constexpr (std::is_convertible_v<T,std::string>) return std::stoi(v); else return static_cast<int>(v); }
template<typename T> static inline float to_float(const T& v){ if constexpr (std::is_convertible_v<T,std::string>) return std::stof(v); else return static_cast<float>(v); }
template<typename C> static inline int sum(const C& c){ int s=0; for(const auto& e: c) s += e; return s; }
template<typename T> static inline int sum(std::initializer_list<T> c){ int s=0; for(const auto& e: c) s += e; return s; }
template<typename T> static inline size_t len(const T& c){ return c.size(); }
static inline size_t len(const char* s){ return std::char_traits<char>::length(s); }
template<typename K, typename V> static inline std::string to_str(const std::unordered_map<K,V>&){ return std::string("{...}"); }
// minimal ABC base to support 'class X : public ABC'
struct ABC {};
// minimal math namespace replacement for 'math.pi'
struct __fluxus_math { static constexpr double pi = 3.141592653589793; static inline double sqrt(double x){ return std::sqrt(x); } } math;
// minimal sys module stub
namespace sys { static std::vector<std::string> argv; }
namespace asyncio { template<typename F> static inline void run(F f){ f(); } }

// vector to string conversion for printing
template<typename T>
static inline std::string vec_to_str(const std::vector<T>& v) {
    std::ostringstream os;
    os << "[";
    for (size_t i = 0; i < v.size(); ++i) {
        if (i > 0) os << ", ";
        if constexpr (std::is_same_v<T, std::string>) {
            os << "'" << v[i] << "'";
        } else {
            os << v[i];
        }
    }
    os << "]";
    return os.str();
}

// nested vector to string
template<typename T>
static inline std::string vec_to_str(const std::vector<std::vector<T>>& v) {
    std::ostringstream os;
    os << "[";
    for (size_t i = 0; i < v.size(); ++i) {
        if (i > 0) os << ", ";
        os << vec_to_str(v[i]);
    }
    os << "]";
    return os.str();
}

// stream operator for vectors
template<typename T>
static inline std::ostream& operator<<(std::ostream& os, const std::vector<T>& v) {
    os << vec_to_str(v);
    return os;
}
template<typename K, typename V> static inline std::ostream& operator<<(std::ostream& os, const std::unordered_map<K,V>&){ os << "{...}"; return os; }
template<typename K, typename V> static inline auto py_items(const std::unordered_map<K,V>& m){ std::vector<std::pair<K,V>> v; v.reserve(m.size()); for(const auto& kv: m){ v.emplace_back(kv.first, kv.second);} return v; }


void test_basic_argparse() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "=== Testing Basic argparse Functionality ===" << std::endl;
    auto parser = argparse.ArgumentParser(;
        auto description = 0;
        auto epilog = 'For more information, visit https://example.com';
    }
    );
    parser.add_argument('filename', help('Input file to process'));
    parser.add_argument('output', nargs('?', help='Output file (optional)'));
    parser.add_argument('-v', '--verbose', action('store_true', help='Enable verbose output'));
    parser.add_argument('-n', '--number', type(int, default=10, help='Number of iterations (default: 10)'));
    parser.add_argument('--mode', choices(std::vector<std::string>{'fast', 'slow', 'normal'}, default='normal', help='Processing mode'));
    auto test_args = std::vector<std::string>{'input.txt', 'output.txt', '--verbose', '--number', '20', '--mode', 'fast'};
    {
        auto args = parser.parse_args(test_args);
        std::cout << to_str("Parsed arguments:") << std::endl;
        std::cout << to_str("  filename: ") << to_str(args.filename) << std::endl;
        std::cout << to_str("  output: ") << to_str(args.output) << std::endl;
        std::cout << to_str("  verbose: ") << to_str(args.verbose) << std::endl;
        std::cout << to_str("  number: ") << to_str(args.number) << std::endl;
        std::cout << to_str("  mode: ") << to_str(args.mode) << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << "Argument parsing failed" << std::endl;
    }
}


std::string test_advanced_argument_types() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Advanced Argument Types ===" << std::endl;
    auto parser = argparse.ArgumentParser(description='Advanced argument types demo');
    parser.add_argument('-f', '--file', type(argparse.FileType('r'), help='Input file'));
    parser.add_argument('-o', '--output', type(argparse.FileType('w'), help='Output file'));
    std::function<int()> positive_int = [&]() {
        /* docstring */
        auto ivalue = to_int(value);
        if (ivalue <= 0) {
            /* raise */
        }
        return ivalue;
    };
    std::function<int()> valid_date = [&]() {
        /* docstring */
        {
            return datetime.strptime(value, '%Y-%m-%d');
        }
        if (__fluxus_exc) { auto e = 0;
            /* raise */
        }
    };
    parser.add_argument('--threads', type(positive_int, default=4, help='Number of threads (positive integer)'));
    parser.add_argument('--date', type(valid_date, help='Target date (YYYY-MM-DD)'));
    parser.add_argument('--ratio', type(0);
    parser.add_argument('--tags', nargs('+', help='List of tags'));
    parser.add_argument('--numbers', nargs('*', type=int, default=std::vector<decltype(1)>{1, 2, 3}, help='List of numbers'));
    parser.add_argument('--values', nargs(3, type=float, help='Exactly 3 float values'));
    auto test_args = [;
        '--threads', '8',;
        '--date', '2024-01-15',;
        '--ratio', '0.7',;
        '--tags', 'python', 'argparse', 'cli',;
        '--numbers', '10', '20', '30', '40',;
        '--values', '1.1', '2.2', '3.3';
    }
    ];
    {
        auto args = parser.parse_args(test_args);
        std::cout << to_str("Parsed advanced arguments:") << std::endl;
        std::cout << to_str("  threads: ") << to_str(args.threads) << std::endl;
        std::cout << to_str("  date: ") << to_str(args.date) << std::endl;
        std::cout << to_str("  date type: ") << to_str(type(args.date)) << std::endl;
        std::cout << to_str("  ratio: ") << to_str(args.ratio) << std::endl;
        std::cout << to_str("  tags: ") << to_str(args.tags) << std::endl;
        std::cout << to_str("  numbers: ") << to_str(args.numbers) << std::endl;
        std::cout << to_str("  values: ") << to_str(args.values) << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("Argument parsing failed: ") << to_str(e) << std::endl;
    }
}


void test_subcommands() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Subcommands ===" << std::endl;
    auto parser = argparse.ArgumentParser(description='Git-like CLI with subcommands');
    auto subparsers = parser.add_subparsers(dest='command', help='Available commands');
    auto init_parser = subparsers.add_parser('init', help='Initialize a new project');
    init_parser.add_argument('project_name', help('Name of the project'));
    init_parser.add_argument('--template', choices(std::vector<std::string>{'basic', 'advanced'}, default='basic', help='Project template'));
    init_parser.add_argument('--force', action('store_true', help='Force initialization'));
    auto add_parser = subparsers.add_parser('add', help='Add files to project');
    add_parser.add_argument('files', nargs('+', help='Files to add'));
    add_parser.add_argument('--recursive', '-r', action('store_true', help='Add files recursively'));
    add_parser.add_argument('--exclude', action('append', default=[], help='Patterns to exclude'));
    auto status_parser = subparsers.add_parser('status', help='Show project status');
    status_parser.add_argument('--verbose', '-v', action('store_true', help='Verbose output'));
    status_parser.add_argument('--short', '-s', action('store_true', help='Short format'));
    auto test_commands = [;
        std::vector<std::string>{'init', 'my_project', '--template', 'advanced'},;
        std::vector<std::string>{'add', 'file1.py', 'file2.py', '--recursive', '--exclude', '*.pyc'},;
        std::vector<std::string>{'status', '--verbose'};
    }
    ];
    for (auto cmd_args : test_commands) {
        std::cout << to_str("\\nTesting command: ") << to_str(' '.join(cmd_args)) << std::endl;
        {
            auto args = parser.parse_args(cmd_args);
            std::cout << to_str("  Command: ") << to_str(args.command) << std::endl;
            if (args.command == 'init') {
                std::cout << to_str("  Project name: ") << to_str(args.project_name) << std::endl;
                std::cout << to_str("  Template: ") << to_str(args.template) << std::endl;
                std::cout << to_str("  Force: ") << to_str(args.force) << std::endl;
            }
            else if (args.command == 'add') {
                std::cout << to_str("  Files: ") << to_str(args.files) << std::endl;
                std::cout << to_str("  Recursive: ") << to_str(args.recursive) << std::endl;
                std::cout << to_str("  Exclude: ") << to_str(args.exclude) << std::endl;
            }
            else if (args.command == 'status') {
                std::cout << to_str("  Verbose: ") << to_str(args.verbose) << std::endl;
                std::cout << to_str("  Short: ") << to_str(args.short) << std::endl;
            }
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << "  Command parsing failed" << std::endl;
        }
    }
}


void test_mutually_exclusive_groups() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Mutually Exclusive Groups ===" << std::endl;
    auto parser = argparse.ArgumentParser(description='Deployment tool');
    parser.add_argument('--verbose', '-v', action('store_true', help='Enable verbose output'));
    auto group = parser.add_mutually_exclusive_group(required=true);
    group.add_argument('--dev', action('store_true', help='Deploy to development'));
    group.add_argument('--staging', action('store_true', help='Deploy to staging'));
    group.add_argument('--prod', action('store_true', help='Deploy to production'));
    auto format_group = parser.add_mutually_exclusive_group();
    format_group.add_argument('--json', action('store_true', help='JSON output format'));
    format_group.add_argument('--yaml', action('store_true', help='YAML output format'));
    format_group.add_argument('--xml', action('store_true', help='XML output format'));
    auto valid_combinations = [;
        std::vector<std::string>{'--dev'},;
        std::vector<std::string>{'--staging', '--json'},;
        std::vector<std::string>{'--prod', '--yaml', '--verbose'};
    }
    ];
    for (auto args : valid_combinations) {
        std::cout << to_str("\\nTesting valid args: ") << to_str(args) << std::endl;
        {
            if ('--verbose' not in args) {
                args.push_back('--verbose');
            }
            auto parsed = parser.parse_args(args);
            std::cout << to_str("  Environment: dev=") << to_str(parsed.dev) << to_str(", staging=") << to_str(parsed.staging) << to_str(", prod=") << to_str(parsed.prod) << std::endl;
            std::cout << to_str("  Format: json=") << to_str(parsed.json) << to_str(", yaml=") << to_str(parsed.yaml) << to_str(", xml=") << to_str(parsed.xml) << std::endl;
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << "  Unexpected parsing failure" << std::endl;
        }
    }
}


void test_custom_actions() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Custom Actions ===" << std::endl;
    class StoreNameValue { public: StoreNameValue() = default; };
    auto parser = argparse.ArgumentParser(description='Custom actions demo');
    parser.add_argument('--define', '-D', action(StoreNameValue, dest='definitions',);
                    auto help = 'Define a name-value pair (format: name=value)');
                }
            }
        }
    }
    parser.add_argument('--version', action('version', version='%(prog)s 1.0.0'));
    parser.add_argument('--append', action('append', dest='items', default=[],);
                    auto help = 'Append to list (can be used multiple times)');
                }
            }
        }
    }
    parser.add_argument('--append-const', action('append_const', dest='flags', const='FLAG1',);
                    auto help = 'Append constant to list');
                }
            }
        }
    }
    parser.add_argument('--count', '-c', action('count', default=0,);
                    auto help = 'Count occurrences (can be used multiple times)');
                }
            }
        }
    }
    parser.add_argument('--store-const', action('store_const', dest='constant_value', const=42,);
                    auto help = 'Store a constant value');
                }
            }
        }
    }
    auto test_args = [;
        auto '--define', 'key1 = value1',;
        auto '--define', 'key2 = value2',;
        '--append', 'item1',;
        '--append', 'item2',;
        '--count', '--count', '--count',;
        '--store-const';
    }
    ];
    {
        auto args = parser.parse_args(test_args);
        std::cout << to_str("Custom definitions: ") << to_str(args.definitions) << std::endl;
        std::cout << to_str("Appended items: ") << to_str(args.items) << std::endl;
        std::cout << to_str("Count value: ") << to_str(args.count) << std::endl;
        std::cout << to_str("Constant value: ") << to_str(args.constant_value) << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << "Custom action parsing failed" << std::endl;
    }
}


void test_argument_validation() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Argument Validation ===" << std::endl;
    auto parser = argparse.ArgumentParser(description='Validation demo');
    parser.add_argument('--required', required(true, help='This argument is required'));
    parser.add_argument('--optional', help('This argument is optional'));
    parser.add_argument('--config', help('Configuration file'));
    parser.add_argument('--validate-config', action('store_true', help='Validate configuration'));
    std::cout << "Testing missing required argument:" << std::endl;
    {
        auto args = parser.parse_args(std::vector<std::string>{'--optional', 'value'});
        std::cout << "ERROR: Should have failed due to missing required argument" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << "✓ Correctly failed due to missing required argument" << std::endl;
    }
    std::cout << "\nTesting valid arguments:" << std::endl;
    {
        auto args = parser.parse_args(std::vector<std::string>{'--required', 'value', '--config', 'config.json'});
        std::cout << to_str("✓ Successfully parsed: required=") << to_str(args.required) << to_str(", config=") << to_str(args.config) << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << "ERROR: Should have succeeded with valid arguments" << std::endl;
    }
}


void test_help_generation() {
    /* docstring */
    std::cout << "\n=== Testing Help Generation ===" << std::endl;
    auto parser = argparse.ArgumentParser(;
        auto prog = 'my-tool',;
        auto description = 0;
        auto epilog = 'For more details, see the documentation at https://example.com/docs';
    }
    );
    parser.add_argument('input', help('Input file to process'));
    parser.add_argument('-v', '--verbose', action('count', default=0,);
                    auto help = 0;
                }
            }
        }
    }
    parser.add_argument('--config', metavar('FILE', help='Configuration file path'));
    parser.add_argument('--timeout', type(float, default=30.0,);
                    auto help = 'Timeout in seconds (default: 30.0)');
                }
            }
        }
    }
    std::cout << "Generated help message:" << std::endl;
    std::cout << std::string(60, '-') << std::endl;
    parser.print_help();
    std::cout << std::string(60, '-') << std::endl;
}


void demonstrate_real_world_cli() {
    /* docstring */
    std::cout << "\n=== Real-World CLI Example ===" << std::endl;
    class FileProcessor { public: FileProcessor() = default; };
}


int main() {
    /* docstring */
    std::cout << "Python argparse Module Demonstration" << std::endl;
    std::cout << std::string(50, '=') << std::endl;
    test_basic_argparse();
    test_advanced_argument_types();
    test_subcommands();
    test_mutually_exclusive_groups();
    test_custom_actions();
    test_argument_validation();
    test_help_generation();
    demonstrate_real_world_cli();
    std::cout << "\n=== Summary ===" << std::endl;
    std::cout << "argparse features demonstrated:" << std::endl;
    std::cout << "- Basic argument parsing (positional and optional)" << std::endl;
    std::cout << "- Advanced argument types and validation" << std::endl;
    std::cout << 0 << std::endl;
    std::cout << "- Mutually exclusive argument groups" << std::endl;
    std::cout << 0 << std::endl;
    std::cout << "- Argument validation and error handling" << std::endl;
    std::cout << "- Help message generation and formatting" << std::endl;
    std::cout << "- Real-world CLI application patterns" << std::endl;
    std::cout << "\nKey best practices:" << std::endl;
    std::cout << "- Use meaningful argument names and help text" << std::endl;
    std::cout << "- Provide sensible defaults" << std::endl;
    std::cout << "- Validate input data types and ranges" << std::endl;
    std::cout << 0 << std::endl;
    std::cout << "- Handle errors gracefully" << std::endl;
    return 0;
}


