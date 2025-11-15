import os
import shutil
import subprocess
import sys


def detect_cpp_compiler() -> str | None:
    """Locate an available C++ compiler suitable for the sample test."""
    candidates = []

    env_compiler = os.environ.get("CXX")
    if env_compiler:
        candidates.append(env_compiler)

    # Prefer clang++ to mirror the project default, but fall back to common alternatives.
    candidates.extend(["clang++", "g++", "c++"])

    seen = set()
    for candidate in candidates:
        if candidate in seen:
            continue
        seen.add(candidate)

        path = shutil.which(candidate)
        if path:
            return path

    return None


def main() -> None:
    compiler = detect_cpp_compiler()
    if compiler is None:
        print(
            "Unable to locate a C++ compiler. Install clang++, g++, or set the CXX environment variable.",
            file=sys.stderr,
        )
        sys.exit(1)

    print(f"Using C++ compiler: {compiler}")

    compile_cmd = [
        compiler,
        "-std=c++20",
        "test_simple_output.cpp",
        "-o",
        "test_simple_output_manual",
    ]
    result = subprocess.run(compile_cmd, capture_output=True, text=True)
    print(f"Compilation exit code: {result.returncode}")
    if result.returncode != 0:
        if result.stderr:
            print(f"Compilation stderr: {result.stderr}")
        sys.exit(1)

    run_cmd = ["./test_simple_output_manual"]
    result = subprocess.run(run_cmd, capture_output=True, text=True)
    print(f"Program output: '{result.stdout}'")
    print(f"Program stderr: '{result.stderr}'")
    print(f"Program exit code: {result.returncode}")


if __name__ == "__main__":
    main()
