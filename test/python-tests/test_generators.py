def simple_counter(n):
    for i in range(n):
        yield i

def fibonacci_generator():
    a, b = 0, 1
    while True:
        yield a
        a, b = b, a + b

def even_numbers(start, end):
    for num in range(start, end + 1):
        if num % 2 == 0:
            yield num

def generator_with_send():
    value = yield "Ready"
    while True:
        value = yield f"Received: {value}"

def count_up_to(n):
    count = 0
    while count <= n:
        yield count
        count += 1

def prime_numbers(limit):
    def is_prime(num):
        if num < 2:
            return False
        for i in range(2, int(num ** 0.5) + 1):
            if num % i == 0:
                return False
        return True

    for num in range(2, limit + 1):
        if is_prime(num):
            yield num

def test_simple_generator():
    counter = simple_counter(5)
    assert list(counter) == [0, 1, 2, 3, 4]

def test_fibonacci_generator():
    fib = fibonacci_generator()
    result = []
    for _ in range(10):
        result.append(next(fib))
    assert result == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

def test_even_numbers_generator():
    evens = even_numbers(1, 10)
    assert list(evens) == [2, 4, 6, 8, 10]

def test_generator_send():
    gen = generator_with_send()
    initial = next(gen)
    assert initial == "Ready"

    response1 = gen.send("Hello")
    assert response1 == "Received: Hello"

    response2 = gen.send("World")
    assert response2 == "Received: World"

def test_count_up_to_generator():
    counter = count_up_to(3)
    assert list(counter) == [0, 1, 2, 3]

def test_prime_numbers_generator():
    primes = prime_numbers(20)
    assert list(primes) == [2, 3, 5, 7, 11, 13, 17, 19]

def test_generator_expressions():
    squares = (x ** 2 for x in range(5))
    assert list(squares) == [0, 1, 4, 9, 16]

def test_generator_chain():
    def chain(*iterables):
        for iterable in iterables:
            yield from iterable

    result = list(chain([1, 2, 3], ['a', 'b'], [4, 5]))
    assert result == [1, 2, 3, 'a', 'b', 4, 5]

if __name__ == "__main__":
    test_simple_generator()
    test_fibonacci_generator()
    test_even_numbers_generator()
    test_generator_send()
    test_count_up_to_generator()
    test_prime_numbers_generator()
    test_generator_expressions()
    test_generator_chain()
    print("All generator tests passed!")