# Test string operations
text = "Hello, World!"
name = "Alice"
number = 42

# String concatenation
greeting = text + " How are you?"
full_name = "First " + name

print("Concatenated:", greeting)
print("Full name:", full_name)

# String methods
print("\nString methods:")
print("Uppercase:", text.upper())
print("Lowercase:", text.lower())
print("Strip spaces:", '  spaces  '.strip())
print("Replace:", text.replace("World", "Python"))
print("Split:", text.split(","))
print("Join:", "-".join(["a", "b", "c"]))

# String formatting
print("\nString formatting:")
print(f"Name: {name}, Number: {number}")
print("Name: {}, Number: {}".format(name, number))
print("Name: %s, Number: %d" % (name, number))

# String slicing
print("\nString slicing:")
sample = "Python Programming"
print("First 6 chars:", sample[:6])
print("Last 11 chars:", sample[-11:])
print("Middle:", sample[7:18])

# String operations
print("\nString operations:")
print("Length:", len(text))
print("Contains 'World':", "World" in text)
print("Starts with 'Hello':", text.startswith("Hello"))
print("Ends with '!':", text.endswith("!"))

# Multiline strings
multiline = """This is a
multiline string
with multiple lines"""
print("\nMultiline string:")
print(multiline)