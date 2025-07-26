def reverse_string(s):
    return s[::-1]

def count_vowels(s):
    count = 0
    vowels = "aeiouAEIOU"
    for char in s:
        if char in vowels:
            count = count + 1
    return count

def is_palindrome(s):
    reversed_s = reverse_string(s)
    return s == reversed_s

def main():
    print("String Processing Demo")
    
    text = "hello"
    print("Original:", text)
    print("Reversed:", reverse_string(text))
    print("Vowel count:", count_vowels(text))
    print("Is palindrome:", is_palindrome(text))
    
    palindrome = "racecar"
    print()
    print("Palindrome test:", palindrome)
    print("Is palindrome:", is_palindrome(palindrome))
    
    words = ["go", "programming", "language"]
    print()
    print("Words and their vowel counts:")
    for word in words:
        vowels = count_vowels(word)
        print(word + ":", vowels, "vowels")

if __name__ == "__main__":
    main()