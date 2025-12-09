class Counter:
    def __init__(self):
        self.count = 0
    def increment(self):
        self.count += 1
c = Counter()
c.increment()
print(c.count)
