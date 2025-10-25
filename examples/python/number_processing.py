def is_prime(n):
    if n <= 1:
        return False
    if n <= 3:
        return True
    if n % 2 == 0 or n % 3 == 0:
        return False
    
    i = 5
    while i * i <= n:
        if n % i == 0 or n % (i + 2) == 0:
            return False
        i = i + 6
    return True

def gcd(a, b):
    while b != 0:
        temp = b
        b = a % b
        a = temp
    return a

def main():
    print("Prime Numbers and GCD")
    
    print("Prime numbers from 1 to 20:")
    for i in range(1, 21):
        if is_prime(i):
            print(i, end=" ")
    print()
    
    a = 48
    b = 18
    result = gcd(a, b)
    print("GCD of", a, "and", b, "is", result)
    
    numbers = [10, 20, 30, 40, 50]
    total = 0
    
    print("Array:", end=" ")
    for num in numbers:
        print(num, end=" ")
        total = total + num
    print()
    print("Sum:", total)

if __name__ == "__main__":
    main()