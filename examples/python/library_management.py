import datetime
import random

class Book:
    def __init__(self, isbn, title, author, publication_year, total_copies):
        self.isbn = isbn
        self.title = title
        self.author = author
        self.publication_year = publication_year
        self.total_copies = total_copies
        self.available_copies = total_copies
        self.borrowed_copies = 0

    def __str__(self):
        return f"Book(ISBN: {self.isbn}, Title: '{self.title}', Author: {self.author}, Available: {self.available_copies}/{self.total_copies})"

class Member:
    def __init__(self, member_id, name, email, phone):
        self.member_id = member_id
        self.name = name
        self.email = email
        self.phone = phone
        self.borrowed_books = []
        self.membership_date = datetime.date.today()
        self.fine_amount = 0.0

    def __str__(self):
        return f"Member(ID: {self.member_id}, Name: {self.name}, Books Borrowed: {len(self.borrowed_books)}, Fine: ${self.fine_amount:.2f})"

class Transaction:
    def __init__(self, transaction_id, member_id, isbn, transaction_type, date):
        self.transaction_id = transaction_id
        self.member_id = member_id
        self.isbn = isbn
        self.transaction_type = transaction_type  # 'borrow' or 'return'
        self.date = date
        self.due_date = None
        if transaction_type == 'borrow':
            self.due_date = date + datetime.timedelta(days=14)  # 2 weeks loan period

    def __str__(self):
        due_str = f", Due: {self.due_date}" if self.due_date else ""
        return f"Transaction({self.transaction_id}, {self.transaction_type.upper()}, {self.date}{due_str})"

class Library:
    def __init__(self, name):
        self.name = name
        self.books = {}  # ISBN -> Book
        self.members = {}  # member_id -> Member
        self.transactions = []
        self.next_transaction_id = 1
        self.fine_per_day = 0.50  # $0.50 per day

    def add_book(self, book):
        if book.isbn in self.books:
            # If book already exists, increase the total and available copies
            existing_book = self.books[book.isbn]
            existing_book.total_copies += book.total_copies
            existing_book.available_copies += book.available_copies
            print(f"Added {book.total_copies} more copies of '{book.title}'")
        else:
            self.books[book.isbn] = book
            print(f"New book added: {book.title}")

    def register_member(self, member):
        self.members[member.member_id] = member
        print(f"Member registered: {member.name} (ID: {member.member_id})")

    def search_books(self, query):
        results = []
        query_lower = query.lower()
        for book in self.books.values():
            if (query_lower in book.title.lower() or 
                query_lower in book.author.lower() or 
                query in book.isbn):
                results.append(book)
        return results

    def borrow_book(self, member_id, isbn):
        if member_id not in self.members:
            print(f"Member {member_id} not found")
            return False
        
        if isbn not in self.books:
            print(f"Book with ISBN {isbn} not found")
            return False
        
        member = self.members[member_id]
        book = self.books[isbn]
        
        if book.available_copies <= 0:
            print(f"'{book.title}' is not available")
            return False
        
        if len(member.borrowed_books) >= 5:  # Max 5 books per member
            print(f"Member {member.name} has reached the borrowing limit")
            return False
        
        # Create transaction
        transaction = Transaction(
            self.next_transaction_id,
            member_id,
            isbn,
            'borrow',
            datetime.date.today()
        )
        
        self.transactions.append(transaction)
        self.next_transaction_id += 1
        
        # Update book and member records
        book.available_copies -= 1
        book.borrowed_copies += 1
        member.borrowed_books.append({
            'isbn': isbn,
            'transaction_id': transaction.transaction_id,
            'borrow_date': transaction.date,
            'due_date': transaction.due_date
        })
        
        print(f"'{book.title}' borrowed by {member.name}. Due date: {transaction.due_date}")
        return True

    def return_book(self, member_id, isbn):
        if member_id not in self.members:
            print(f"Member {member_id} not found")
            return False
        
        member = self.members[member_id]
        book = self.books.get(isbn)
        
        if not book:
            print(f"Book with ISBN {isbn} not found")
            return False
        
        # Find the borrowed book record
        borrowed_book = None
        for borrowed in member.borrowed_books:
            if borrowed['isbn'] == isbn:
                borrowed_book = borrowed
                break
        
        if not borrowed_book:
            print(f"Member {member.name} has not borrowed this book")
            return False
        
        # Calculate fine if overdue
        return_date = datetime.date.today()
        fine = 0.0
        if return_date > borrowed_book['due_date']:
            days_overdue = (return_date - borrowed_book['due_date']).days
            fine = days_overdue * self.fine_per_day
            member.fine_amount += fine
            print(f"Book returned late! Fine: ${fine:.2f} ({days_overdue} days overdue)")
        
        # Create return transaction
        transaction = Transaction(
            self.next_transaction_id,
            member_id,
            isbn,
            'return',
            return_date
        )
        
        self.transactions.append(transaction)
        self.next_transaction_id += 1
        
        # Update records
        book.available_copies += 1
        book.borrowed_copies -= 1
        member.borrowed_books.remove(borrowed_book)
        
        print(f"'{book.title}' returned by {member.name}")
        return True

    def pay_fine(self, member_id, amount):
        if member_id in self.members:
            member = self.members[member_id]
            if amount >= member.fine_amount:
                paid = member.fine_amount
                member.fine_amount = 0
                print(f"Fine paid: ${paid:.2f}. Remaining balance: $0.00")
            else:
                member.fine_amount -= amount
                print(f"Partial payment: ${amount:.2f}. Remaining balance: ${member.fine_amount:.2f}")
        else:
            print(f"Member {member_id} not found")

    def list_available_books(self):
        print(f"\n=== Available Books at {self.name} ===")
        available_books = [book for book in self.books.values() if book.available_copies > 0]
        if not available_books:
            print("No books available")
        else:
            for book in sorted(available_books, key=lambda x: x.title):
                print(f"  {book}")
        print("=" * 50)

    def list_members(self):
        print(f"\n=== Library Members ===")
        for member in sorted(self.members.values(), key=lambda x: x.name):
            print(f"  {member}")
        print("=" * 40)

    def get_overdue_books(self):
        overdue = []
        today = datetime.date.today()
        
        for member in self.members.values():
            for borrowed in member.borrowed_books:
                if today > borrowed['due_date']:
                    days_overdue = (today - borrowed['due_date']).days
                    book = self.books[borrowed['isbn']]
                    overdue.append({
                        'member': member,
                        'book': book,
                        'days_overdue': days_overdue,
                        'due_date': borrowed['due_date']
                    })
        
        return overdue

    def print_overdue_report(self):
        overdue_books = self.get_overdue_books()
        if not overdue_books:
            print("\nNo overdue books")
            return
        
        print(f"\n=== Overdue Books Report ===")
        for item in overdue_books:
            fine = item['days_overdue'] * self.fine_per_day
            print(f"Member: {item['member'].name}")
            print(f"Book: '{item['book'].title}' by {item['book'].author}")
            print(f"Due Date: {item['due_date']}")
            print(f"Days Overdue: {item['days_overdue']}")
            print(f"Fine: ${fine:.2f}")
            print("-" * 30)

def main():
    print("Library Management System Demo")
    
    # Create library
    library = Library("Central City Library")
    
    # Add books
    books = [
        Book("978-0-14-143951-8", "Pride and Prejudice", "Jane Austen", 1813, 3),
        Book("978-0-7432-7356-5", "To Kill a Mockingbird", "Harper Lee", 1960, 2),
        Book("978-0-452-28423-4", "1984", "George Orwell", 1949, 4),
        Book("978-0-14-017739-8", "The Catcher in the Rye", "J.D. Salinger", 1951, 2),
        Book("978-0-06-112008-4", "The Great Gatsby", "F. Scott Fitzgerald", 1925, 3),
        Book("978-0-544-00341-5", "The Lord of the Rings", "J.R.R. Tolkien", 1954, 2)
    ]
    
    for book in books:
        library.add_book(book)
    
    # Register members
    members = [
        Member("M001", "Alice Johnson", "alice@email.com", "555-0101"),
        Member("M002", "Bob Smith", "bob@email.com", "555-0102"),
        Member("M003", "Carol Brown", "carol@email.com", "555-0103"),
        Member("M004", "David Wilson", "david@email.com", "555-0104"),
        Member("M005", "Emily Davis", "emily@email.com", "555-0105")
    ]
    
    for member in members:
        library.register_member(member)
    
    library.list_available_books()
    library.list_members()
    
    # Simulate borrowing
    library.borrow_book("M001", "978-0-14-143951-8")  # Alice borrows Pride and Prejudice
    library.borrow_book("M002", "978-0-452-28423-4")  # Bob borrows 1984
    library.borrow_book("M003", "978-0-06-112008-4")  # Carol borrows The Great Gatsby
    library.borrow_book("M001", "978-0-7432-7356-5")  # Alice borrows To Kill a Mockingbird
    
    print("\nAfter borrowing:")
    library.list_available_books()
    
    # Search for books
    print("\nSearching for 'pride':")
    results = library.search_books("pride")
    for book in results:
        print(f"  {book}")
    
    # Simulate some returns
    library.return_book("M002", "978-0-452-28423-4")  # Bob returns 1984
    
    # Simulate overdue by manually adjusting due dates (for demo purposes)
    alice = library.members["M001"]
    if alice.borrowed_books:
        alice.borrowed_books[0]['due_date'] = datetime.date.today() - datetime.timedelta(days=5)
    
    library.print_overdue_report()
    
    # Return overdue book
    library.return_book("M001", "978-0-14-143951-8")
    
    # Pay fine
    library.pay_fine("M001", 2.50)
    
    print("\nFinal status:")
    library.list_members()

if __name__ == "__main__":
    main()