# Test basic control flow
age = 25

if age < 13:
    print("Child")
elif age < 18:
    print("Teenager")
elif age < 65:
    print("Adult")
else:
    print("Senior")

count = 0
while count < 5:
    print(count)
    count += 1

for i in range(5):
    print(i)

for i in range(10):
    if i == 3:
        continue
    if i == 7:
        break
    print(i)