# F-string tests
name = "Alice"
age = 30
temperature = 23.5

# Basic f-string
message = f"Hello, {name}!"
print(message)

# F-string with expressions
info = f"{name} is {age} years old."
print(info)

# F-string with calculations
birth_year = 2024 - age
year_message = f"{name} was born around {birth_year}."
print(year_message)

# F-string with format specification
temp_message = f"The temperature is {temperature:.1f}°C"
print(temp_message)