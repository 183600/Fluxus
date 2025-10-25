def fibonacci_recursive(n):
    if n <= 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)

def fibonacci_iterative(n):
    if n <= 0:
        return 0
    elif n == 1:
        return 1
    
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return b

def fibonacci_memoized(n, memo=None):
    if memo is None:
        memo = {}
    
    if n in memo:
        return memo[n]
    
    if n <= 0:
        return 0
    elif n == 1:
        return 1
    else:
        memo[n] = fibonacci_memoized(n - 1, memo) + fibonacci_memoized(n - 2, memo)
        return memo[n]

def fibonacci_generator(limit):
    a, b = 0, 1
    count = 0
    while count < limit:
        yield a
        a, b = b, a + b
        count += 1

def factorial_recursive(n):
    if n <= 1:
        return 1
    return n * factorial_recursive(n - 1)

def factorial_iterative(n):
    result = 1
    for i in range(2, n + 1):
        result *= i
    return result

def gcd_euclidean(a, b):
    while b:
        a, b = b, a % b
    return a

def lcm(a, b):
    return abs(a * b) // gcd_euclidean(a, b)

def is_prime(n):
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False
    
    for i in range(3, int(n ** 0.5) + 1, 2):
        if n % i == 0:
            return False
    return True

def sieve_of_eratosthenes(limit):
    sieve = [True] * (limit + 1)
    sieve[0] = sieve[1] = False
    
    for i in range(2, int(limit ** 0.5) + 1):
        if sieve[i]:
            for j in range(i * i, limit + 1, i):
                sieve[j] = False
    
    return [i for i in range(2, limit + 1) if sieve[i]]

def power_iterative(base, exponent):
    result = 1
    for _ in range(exponent):
        result *= base
    return result

def power_recursive(base, exponent):
    if exponent == 0:
        return 1
    elif exponent == 1:
        return base
    else:
        return base * power_recursive(base, exponent - 1)

def power_optimized(base, exponent):
    if exponent == 0:
        return 1
    if exponent % 2 == 0:
        half_power = power_optimized(base, exponent // 2)
        return half_power * half_power
    else:
        return base * power_optimized(base, exponent - 1)

def binary_search(arr, target):
    left, right = 0, len(arr) - 1
    
    while left <= right:
        mid = (left + right) // 2
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    
    return -1

def linear_search(arr, target):
    for i, value in enumerate(arr):
        if value == target:
            return i
    return -1

def tower_of_hanoi(n, source, destination, auxiliary):
    moves = []
    
    def hanoi_recursive(n, source, destination, auxiliary):
        if n == 1:
            moves.append(f"Move disk 1 from {source} to {destination}")
        else:
            hanoi_recursive(n - 1, source, auxiliary, destination)
            moves.append(f"Move disk {n} from {source} to {destination}")
            hanoi_recursive(n - 1, auxiliary, destination, source)
    
    hanoi_recursive(n, source, destination, auxiliary)
    return moves

def generate_pascals_triangle(rows):
    triangle = []
    for i in range(rows):
        row = [1] * (i + 1)
        for j in range(1, i):
            row[j] = triangle[i - 1][j - 1] + triangle[i - 1][j]
        triangle.append(row)
    return triangle

def matrix_multiply(A, B):
    if len(A[0]) != len(B):
        raise ValueError("Cannot multiply matrices: incompatible dimensions")
    
    rows_A, cols_A = len(A), len(A[0])
    rows_B, cols_B = len(B), len(B[0])
    
    result = [[0 for _ in range(cols_B)] for _ in range(rows_A)]
    
    for i in range(rows_A):
        for j in range(cols_B):
            for k in range(cols_A):
                result[i][j] += A[i][k] * B[k][j]
    
    return result

def matrix_transpose(matrix):
    return [[matrix[j][i] for j in range(len(matrix))] for i in range(len(matrix[0]))]

def determinant_2x2(matrix):
    if len(matrix) != 2 or len(matrix[0]) != 2:
        raise ValueError("Matrix must be 2x2")
    return matrix[0][0] * matrix[1][1] - matrix[0][1] * matrix[1][0]

def knapsack_01(weights, values, capacity):
    n = len(weights)
    dp = [[0 for _ in range(capacity + 1)] for _ in range(n + 1)]
    
    for i in range(1, n + 1):
        for w in range(1, capacity + 1):
            if weights[i - 1] <= w:
                dp[i][w] = max(
                    values[i - 1] + dp[i - 1][w - weights[i - 1]],
                    dp[i - 1][w]
                )
            else:
                dp[i][w] = dp[i - 1][w]
    
    return dp[n][capacity]

def longest_common_subsequence(str1, str2):
    m, n = len(str1), len(str2)
    dp = [[0] * (n + 1) for _ in range(m + 1)]
    
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if str1[i - 1] == str2[j - 1]:
                dp[i][j] = dp[i - 1][j - 1] + 1
            else:
                dp[i][j] = max(dp[i - 1][j], dp[i][j - 1])
    
    return dp[m][n]

def edit_distance(str1, str2):
    m, n = len(str1), len(str2)
    dp = [[0] * (n + 1) for _ in range(m + 1)]
    
    for i in range(m + 1):
        dp[i][0] = i
    for j in range(n + 1):
        dp[0][j] = j
    
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if str1[i - 1] == str2[j - 1]:
                dp[i][j] = dp[i - 1][j - 1]
            else:
                dp[i][j] = 1 + min(
                    dp[i - 1][j],     # deletion
                    dp[i][j - 1],     # insertion
                    dp[i - 1][j - 1]  # substitution
                )
    
    return dp[m][n]

def coin_change(coins, amount):
    dp = [float('inf')] * (amount + 1)
    dp[0] = 0
    
    for coin in coins:
        for i in range(coin, amount + 1):
            dp[i] = min(dp[i], dp[i - coin] + 1)
    
    return dp[amount] if dp[amount] != float('inf') else -1

def palindrome_check(s):
    s = ''.join(c.lower() for c in s if c.isalnum())
    return s == s[::-1]

def anagram_check(str1, str2):
    return sorted(str1.lower()) == sorted(str2.lower())

def run_algorithm_demonstrations():
    print("=== Algorithm Demonstrations ===\n")
    
    print("1. Fibonacci Sequence:")
    n = 10
    print(f"   Recursive fibonacci({n}): {fibonacci_recursive(n)}")
    print(f"   Iterative fibonacci({n}): {fibonacci_iterative(n)}")
    print(f"   Memoized fibonacci({n}): {fibonacci_memoized(n)}")
    print(f"   Generator (first 10): {list(fibonacci_generator(10))}")
    
    print("\n2. Factorial:")
    n = 6
    print(f"   Recursive factorial({n}): {factorial_recursive(n)}")
    print(f"   Iterative factorial({n}): {factorial_iterative(n)}")
    
    print("\n3. Number Theory:")
    a, b = 48, 18
    print(f"   GCD({a}, {b}): {gcd_euclidean(a, b)}")
    print(f"   LCM({a}, {b}): {lcm(a, b)}")
    print(f"   Is 17 prime: {is_prime(17)}")
    print(f"   Primes up to 30: {sieve_of_eratosthenes(30)}")
    
    print("\n4. Power Functions:")
    base, exp = 3, 4
    print(f"   {base}^{exp} iterative: {power_iterative(base, exp)}")
    print(f"   {base}^{exp} recursive: {power_recursive(base, exp)}")
    print(f"   {base}^{exp} optimized: {power_optimized(base, exp)}")
    
    print("\n5. Search Algorithms:")
    arr = [1, 3, 5, 7, 9, 11, 13, 15]
    target = 7
    print(f"   Array: {arr}")
    print(f"   Binary search for {target}: {binary_search(arr, target)}")
    print(f"   Linear search for {target}: {linear_search(arr, target)}")
    
    print("\n6. Tower of Hanoi:")
    moves = tower_of_hanoi(3, 'A', 'C', 'B')
    print(f"   Moves for 3 disks: {len(moves)} moves")
    for move in moves:
        print(f"   {move}")
    
    print("\n7. Pascal's Triangle:")
    triangle = generate_pascals_triangle(5)
    for i, row in enumerate(triangle):
        print(f"   Row {i}: {row}")
    
    print("\n8. Matrix Operations:")
    A = [[1, 2], [3, 4]]
    B = [[5, 6], [7, 8]]
    print(f"   Matrix A: {A}")
    print(f"   Matrix B: {B}")
    print(f"   A × B: {matrix_multiply(A, B)}")
    print(f"   A transposed: {matrix_transpose(A)}")
    print(f"   Determinant of A: {determinant_2x2(A)}")
    
    print("\n9. Dynamic Programming:")
    weights = [10, 20, 30]
    values = [60, 100, 120]
    capacity = 50
    print(f"   0/1 Knapsack (weights={weights}, values={values}, capacity={capacity}): {knapsack_01(weights, values, capacity)}")
    
    str1, str2 = "AGGTAB", "GXTXAYB"
    print(f"   LCS of '{str1}' and '{str2}': {longest_common_subsequence(str1, str2)}")
    
    print(f"   Edit distance between 'kitten' and 'sitting': {edit_distance('kitten', 'sitting')}")
    
    coins = [1, 3, 4]
    amount = 6
    print(f"   Coin change for amount {amount} with coins {coins}: {coin_change(coins, amount)}")
    
    print("\n10. String Algorithms:")
    test_str = "A man a plan a canal Panama"
    print(f"   Is '{test_str}' a palindrome: {palindrome_check(test_str)}")
    print(f"   Are 'listen' and 'silent' anagrams: {anagram_check('listen', 'silent')}")

if __name__ == "__main__":
    run_algorithm_demonstrations()