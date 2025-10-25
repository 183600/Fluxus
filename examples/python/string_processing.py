def string_length(text):
    count = 0
    for char in text:
        count = count + 1
    return count

def count_vowels(text):
    vowels = "aeiouAEIOU"
    count = 0
    for char in text:
        if char in vowels:
            count = count + 1
    return count

def count_consonants(text):
    vowels = "aeiouAEIOU"
    count = 0
    for char in text:
        if char.isalpha() and char not in vowels:
            count = count + 1
    return count

def reverse_text(text):
    result = ""
    for i in range(len(text) - 1, -1, -1):
        result = result + text[i]
    return result

def to_uppercase(text):
    result = ""
    for char in text:
        if char >= 'a' and char <= 'z':
            new_char = chr(ord(char) - 32)
            result = result + new_char
        else:
            result = result + char
    return result

def to_lowercase(text):
    result = ""
    for char in text:
        if char >= 'A' and char <= 'Z':
            new_char = chr(ord(char) + 32)
            result = result + new_char
        else:
            result = result + char
    return result

def remove_spaces(text):
    result = ""
    for char in text:
        if char != ' ':
            result = result + char
    return result

def count_words(text):
    if len(text) == 0:
        return 0
    
    word_count = 0
    in_word = False
    
    for char in text:
        if char != ' ':
            if not in_word:
                word_count = word_count + 1
                in_word = True
        else:
            in_word = False
    
    return word_count

def contains_substring(text, substring):
    text_len = len(text)
    sub_len = len(substring)
    
    if sub_len > text_len:
        return False
    
    for i in range(text_len - sub_len + 1):
        match = True
        for j in range(sub_len):
            if text[i + j] != substring[j]:
                match = False
                break
        if match:
            return True
    
    return False

def main():
    print("String processing test")
    
    test_text = "Hello World Programming"
    print("Test text:")
    print(test_text)
    
    print("Length:")
    print(string_length(test_text))
    
    print("Vowel count:")
    print(count_vowels(test_text))
    
    print("Consonant count:")
    print(count_consonants(test_text))
    
    print("Reversed:")
    print(reverse_text(test_text))
    
    print("Uppercase:")
    print(to_uppercase(test_text))
    
    print("Lowercase:")
    print(to_lowercase(test_text))
    
    print("Without spaces:")
    print(remove_spaces(test_text))
    
    print("Word count:")
    print(count_words(test_text))
    
    print("Contains 'World':")
    print(contains_substring(test_text, "World"))
    
    print("Contains 'Python':")
    print(contains_substring(test_text, "Python"))

if __name__ == "__main__":
    main()