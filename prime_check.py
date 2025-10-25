def check_prime(n):
    if n < 2:
        return 0
    i = 2
    while i * i <= n:
        if n % i == 0:
            return 0
        i = i + 1
    return 1

num = 17
result = check_prime(num)
if result == 1:
    print("prime")
else:
    print("not prime")