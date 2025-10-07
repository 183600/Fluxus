def greet(name):
    message = "Hello, " + name + "!"
    return message

def main():
    result = greet("Fluxus")
    print(result)
    print("String with escape: \\n\\t\"quote\"")
    print("Another test")
    
if __name__ == "__main__":
    main()
