# Test advanced collections and data structures
from collections import defaultdict, Counter, OrderedDict, deque, namedtuple
import heapq

# Counter
def test_counter():
    words = ['apple', 'banana', 'apple', 'cherry', 'banana', 'apple', 'date']
    word_counts = Counter(words)

    print("Words:", words)
    print("Word counts:", word_counts)

    # Most common elements
    most_common = word_counts.most_common(2)
    print("Most common:", most_common)

    # Counter operations
    counter1 = Counter(['a', 'a', 'b', 'c'])
    counter2 = Counter(['a', 'b', 'b', 'd'])

    print("\nCounter1:", counter1)
    print("Counter2:", counter2)
    print("Addition:", counter1 + counter2)
    print("Subtraction:", counter1 - counter2)
    print("Intersection:", counter1 & counter2)
    print("Union:", counter1 | counter2)

# DefaultDict
def test_defaultdict():
    # Using defaultdict with list
    grouped_by_length = defaultdict(list)
    words = ['cat', 'dog', 'elephant', 'rat', 'mouse']

    for word in words:
        grouped_by_length[len(word)].append(word)

    print("Words grouped by length:", dict(grouped_by_length))

    # Using defaultdict with int
    char_count = defaultdict(int)
    text = "hello world"

    for char in text:
        char_count[char] += 1

    print("\nCharacter counts:", dict(char_count))

    # Using defaultdict with set
    first_letters = defaultdict(set)
    words = ['apple', 'ant', 'banana', 'ball', 'cherry']

    for word in words:
        first_letters[word[0]].add(word)

    print("\nWords by first letter:", dict(first_letters))

# OrderedDict
def test_ordereddict():
    # Create ordered dict
    ordered_dict = OrderedDict()
    ordered_dict['first'] = 1
    ordered_dict['second'] = 2
    ordered_dict['third'] = 3

    print("Ordered dict:", ordered_dict)
    print("Keys in order:", list(ordered_dict.keys()))

    # Move to end
    ordered_dict.move_to_end('first')
    print("After moving 'first' to end:", list(ordered_dict.keys()))

    # Move to front
    ordered_dict.move_to_end('second', last=False)
    print("After moving 'second' to front:", list(ordered_dict.keys()))

    # Pop from end
    last_item = ordered_dict.popitem()
    print("Popped last item:", last_item)
    print("Remaining:", list(ordered_dict.keys()))

# Deque
def test_deque():
    # Create deque
    d = deque([1, 2, 3, 4, 5])

    print("Original deque:", d)

    # Append operations
    d.append(6)
    d.appendleft(0)
    print("After append(6) and appendleft(0):", d)

    # Pop operations
    right_pop = d.pop()
    left_pop = d.popleft()
    print(f"Popped right: {right_pop}, left: {left_pop}")
    print("After pops:", d)

    # Extend operations
    d.extend([7, 8, 9])
    d.extendleft([-1, -2])
    print("After extend([7,8,9]) and extendleft([-1,-2]):", d)

    # Rotate
    d.rotate(2)
    print("After rotate(2):", d)
    d.rotate(-1)
    print("After rotate(-1):", d)

    # Clear
    d.clear()
    print("After clear:", d)

# NamedTuple
def test_namedtuple():
    # Create named tuple type
    Person = namedtuple('Person', ['name', 'age', 'city'])
    Student = namedtuple('Student', ['name', 'student_id', 'grades'], defaults=[None])

    # Create instances
    person1 = Person('Alice', 30, 'New York')
    person2 = Person('Bob', 25, 'Los Angeles')
    student = Student('Charlie', 'S123', [90, 85, 95])

    print("Person1:", person1)
    print("Person2:", person2)
    print("Student:", student)

    # Access by name
    print(f"\nPerson1 name: {person1.name}")
    print(f"Person1 age: {person1.age}")

    # Access by index
    print(f"Person1[0]: {person1[0]}")
    print(f"Person1[1]: {person1[1]}")

    # Convert to dict
    person_dict = person1._asdict()
    print("Person1 as dict:", person_dict)

    # Replace values
    updated_person = person1._replace(age=31)
    print("Updated person:", updated_person)

# Heap operations (using heapq)
def test_heap():
    numbers = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]

    # Create heap
    heapq.heapify(numbers)
    print("Heapified list:", numbers)

    # Push items
    heapq.heappush(numbers, 0)
    heapq.heappush(numbers, 7)
    print("After pushing 0 and 7:", numbers)

    # Pop items
    smallest = heapq.heappop(numbers)
    second_smallest = heapq.heappop(numbers)
    print(f"Smallest: {smallest}, Second smallest: {second_smallest}")
    print("After pops:", numbers)

    # Get n smallest items
    smallest_3 = heapq.nsmallest(3, numbers)
    print("3 smallest items:", smallest_3)

    # Get n largest items
    largest_3 = heapq.nlargest(3, numbers)
    print("3 largest items:", largest_3)

# ChainMap
def test_chainmap():
    from collections import ChainMap

    dict1 = {'a': 1, 'b': 2}
    dict2 = {'b': 3, 'c': 4}
    dict3 = {'c': 5, 'd': 6}

    chain = ChainMap(dict1, dict2, dict3)

    print("ChainMap:", chain)
    print("Keys:", list(chain.keys()))
    print("Values:", list(chain.values()))

    # Lookup order
    print("Value of 'a':", chain['a'])  # From dict1
    print("Value of 'b':", chain['b'])  # From dict1
    print("Value of 'c':", chain['c'])  # From dict2
    print("Value of 'd':", chain['d'])  # From dict3

    # Add new mapping
    new_chain = chain.new_child({'e': 7, 'a': 10})
    print("\nNew chain with new mapping:", new_chain)
    print("Value of 'a' in new chain:", new_chain['a'])  # From new mapping

# Run all tests
print("=== Counter ===")
test_counter()

print("\n=== DefaultDict ===")
test_defaultdict()

print("\n=== OrderedDict ===")
test_ordereddict()

print("\n=== Deque ===")
test_deque()

print("\n=== NamedTuple ===")
test_namedtuple()

print("\n=== Heap Operations ===")
test_heap()

print("\n=== ChainMap ===")
test_chainmap()