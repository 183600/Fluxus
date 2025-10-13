#!/bin/bash

# 批量编译测试所有.py文件
# 输出格式：文件名 | 编译状态 | 错误信息

OUTPUT_FILE="batch_compile_results.txt"
ERRORS_FILE="batch_compile_errors_detailed.txt"
SUCCESS_COUNT=0
FAIL_COUNT=0

> "$OUTPUT_FILE"
> "$ERRORS_FILE"

echo "开始批量编译测试..." | tee -a "$OUTPUT_FILE"
echo "======================================" | tee -a "$OUTPUT_FILE"

# 所有Python文件列表
files=(
./test_nested_comp.py
./test_empty.py
./test_gen_only.py
./test_call_simple.py
./test_assign_simple.py
./test_gen_simple.py
./test_async_simple.py
./test_with_tuple.py
./test_vars_only.py
./test_tuple_input.py
./test_tuple.py
./test_strings.py
./test_string_concat.py
./test_str_only.py
./test_simple_print.py
./test_simple_only.py
./test_simple_list.py
./test_simple_func.py
./test_simple_comprehension.py
./test_simple_add.py
./test_simple.py
./test_python_modified.py
./test_python_features.py
./test_python.py
./test_print_var.py
./test_print_only.py
./test_print_multiple.py
./test_print_fixed.py
./test_print_final.py
./test_print2.py
./test_print.py
./test_only_vars.py
./test_no_comments.py
./test_minimal.py
./test_math.py
./test_list_assignment.py
./test_just_x.py
./test_just_number.py
./test_just_list.py
./test_functions.py
./test_fstring.py
./test_division.py
./test_div_vars.py
./test_div_simple.py
./test_div_same.py
./test_div_print.py
./test_div_literals.py
./test_div_explicit_bits.py
./test_div_explicit.py
./test_div.py
./test_dir_scanning/test2.py
./test_dir_scanning/test1.py
./test_debug.py
./test_bool_lexer.py
./test_basic.py
./test_assignment_partial.py
./test_add_vars.py
./test_actual_memory.py
./simple_test.py
./simple_python_syntax.py
./simple_python_demo.py
./simple_comprehension_test.py
./minimal_test.py
./minimal_python.py
./fix_struct_parser.py
./fix_cpp_code.py
./debug_to_string.py
./debug_test2.py
./debug_test.py
./debug_simple.py
./debug_print2.py
./debug_print.py
./debug_memory_test.py
./debug_division.py
./comprehensive_test.py
./comprehensive_python_syntax_english.py
./comprehensive_python_syntax.py
./all_syntax.py
./all_python_syntax_en.py
./all_python_syntax.py
./250922_simple.py
./250922_minimal.py
./250922.py
./test/python-tests/feature_decorator.py
./test/python-tests/feature_exception.py
./test/python-tests/feature_async.py
./test/python-tests/basic_arithmetic.py
./test/python-tests/feature_list_comprehension.py
./test/python-tests/feature_with.py
./test/python-tests/test_classes.py
./test/python-tests/test_iterators.py
./test/python-tests/test_loops.py
./test/python-tests/test_json_operations.py
./test/python-tests/test_memory_management.py
./test/python-tests/test_generators.py
./test/python-tests/test_modules_imports.py
./test/python-tests/test_functions.py
./test/python-tests/test_functional_programming.py
./test/python-tests/test_file_operations.py
./test/python-tests/test_exceptions.py
./test/python-tests/test_dictionaries.py
./test/python-tests/test_error_handling.py
./test/python-tests/test_multiprocessing_futures.py
./test/python-tests/test_decorators.py
./test/python-tests/test_datetime.py
./test/python-tests/test_networking.py
./test/python-tests/test_data_structures.py
./test/python-tests/test_csv_xml_yaml.py
./test/python-tests/test_copy_pickle_shelve.py
./test/python-tests/test_context_managers.py
./test/python-tests/test_comprehensions.py
./test/python-tests/test_collections.py
./test/python-tests/test_argparse_cli.py
./test/python-tests/test_object_oriented.py
./test/python-tests/test_advanced_oop.py
./test/python-tests/test_advanced_metaclasses.py
./test/python-tests/test_advanced_functional.py
./test/python-tests/test_advanced_decorators.py
./test/python-tests/test_advanced_asyncio.py
./test/python-tests/test_testing_frameworks.py
./test/python-tests/test_strings.py
./test/python-tests/test_sqlite_integration.py
./test/python-tests/test_regular_expressions.py
./test/python-tests/test_recursion.py
./test/python-tests/test_standard_library_basic.py
./test/python-tests/test_scope_closures.py
./test/python-tests/test_builtin_functions.py
./test/python-tests/test_magic_methods.py
./test/python-tests/test_function_arguments.py
./test/python-tests/test_unpacking.py
./test/python-tests/test_control_flow_advanced.py
./test/python-tests/test_tuples.py
./test/python-tests/test_operators.py
./test/python-tests/test_web_development.py
./test/python-tests/test_type_hints_data.py
./test/python-tests/test_python_basics.py
./test/python-tests/test_properties_classmethods.py
./test/python-tests/test_advanced_async.py
./test/python-tests/test_abstract_base_classes.py
./test/python-tests/test_assertions.py
./test/python-tests/test_dict_set_comprehensions.py
./test/python-tests/feature_fstring.py
)

for file in "${files[@]}"; do
    if [ ! -f "$file" ]; then
        echo "SKIP | $file | 文件不存在" | tee -a "$OUTPUT_FILE"
        continue
    fi
    
    echo -n "测试: $file ... "
    
    # 创建输出文件名
    output_name="test_output_$(basename "$file" .py)"
    
    # 尝试编译
    compile_output=$(stack exec fluxus -- --python -O2 "$file" -o "$output_name" 2>&1)
    compile_status=$?
    
    if [ $compile_status -eq 0 ]; then
        echo "✓ SUCCESS" | tee -a "$OUTPUT_FILE"
        ((SUCCESS_COUNT++))
        # 清理生成的文件
        rm -f "$output_name" "${output_name}.cpp"
    else
        echo "✗ FAIL" | tee -a "$OUTPUT_FILE"
        echo "" >> "$ERRORS_FILE"
        echo "========================================" >> "$ERRORS_FILE"
        echo "文件: $file" >> "$ERRORS_FILE"
        echo "========================================" >> "$ERRORS_FILE"
        echo "$compile_output" >> "$ERRORS_FILE"
        ((FAIL_COUNT++))
    fi
done

echo "" | tee -a "$OUTPUT_FILE"
echo "======================================" | tee -a "$OUTPUT_FILE"
echo "编译完成！" | tee -a "$OUTPUT_FILE"
echo "成功: $SUCCESS_COUNT" | tee -a "$OUTPUT_FILE"
echo "失败: $FAIL_COUNT" | tee -a "$OUTPUT_FILE"
echo "详细错误信息见: $ERRORS_FILE" | tee -a "$OUTPUT_FILE"
