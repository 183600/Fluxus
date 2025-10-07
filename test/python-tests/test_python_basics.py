def test_basic_operations():
    x = 10
    y = 20
    assert x + y == 30
    assert x * y == 200
    assert x - y == -10
    assert y / x == 2.0

def test_string_operations():
    text = "Hello, World!"
    assert text.startswith("Hello")
    assert text.endswith("!")
    assert "World" in text
    assert text.lower() == "hello, world!"
    assert text.upper() == "HELLO, WORLD!"

def test_list_operations():
    numbers = [1, 2, 3, 4, 5]
    assert len(numbers) == 5
    assert numbers[0] == 1
    assert numbers[-1] == 5
    assert sum(numbers) == 15
    assert sorted(numbers) == numbers

def test_dictionary_operations():
    data = {"name": "Alice", "age": 25, "city": "New York"}
    assert data["name"] == "Alice"
    assert data.get("age") == 25
    assert "city" in data
    assert len(data) == 3

def test_boolean_logic():
    a = True
    b = False
    assert a and not b
    assert a or b
    assert not b
    assert a == True
    assert b == False

if __name__ == "__main__":
    test_basic_operations()
    test_string_operations()
    test_list_operations()
    test_dictionary_operations()
    test_boolean_logic()
    print("All Python basics tests passed!")