package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"sync"
	"time"
)

type User struct {
	ID       int    `json:"id"`
	Name     string `json:"name"`
	Email    string `json:"email"`
	Balance  float64 `json:"balance"`
	Created  time.Time `json:"created"`
}

type Transaction struct {
	ID        int     `json:"id"`
	UserID    int     `json:"user_id"`
	Amount    float64 `json:"amount"`
	Type      string  `json:"type"` // "deposit" or "withdraw"
	Timestamp time.Time `json:"timestamp"`
}

type Bank struct {
	users        map[int]*User
	transactions []Transaction
	nextUserID   int
	nextTxID     int
	mutex        sync.RWMutex
}

func NewBank() *Bank {
	return &Bank{
		users:        make(map[int]*User),
		transactions: make([]Transaction, 0),
		nextUserID:   1,
		nextTxID:     1,
	}
}

func (b *Bank) CreateUser(name, email string) *User {
	b.mutex.Lock()
	defer b.mutex.Unlock()
	
	user := &User{
		ID:      b.nextUserID,
		Name:    name,
		Email:   email,
		Balance: 0.0,
		Created: time.Now(),
	}
	
	b.users[b.nextUserID] = user
	b.nextUserID++
	
	return user
}

func (b *Bank) GetUser(id int) (*User, bool) {
	b.mutex.RLock()
	defer b.mutex.RUnlock()
	
	user, exists := b.users[id]
	return user, exists
}

func (b *Bank) Deposit(userID int, amount float64) error {
	b.mutex.Lock()
	defer b.mutex.Unlock()
	
	user, exists := b.users[userID]
	if !exists {
		return fmt.Errorf("user not found")
	}
	
	if amount <= 0 {
		return fmt.Errorf("amount must be positive")
	}
	
	user.Balance += amount
	
	transaction := Transaction{
		ID:        b.nextTxID,
		UserID:    userID,
		Amount:    amount,
		Type:      "deposit",
		Timestamp: time.Now(),
	}
	
	b.transactions = append(b.transactions, transaction)
	b.nextTxID++
	
	return nil
}

func (b *Bank) Withdraw(userID int, amount float64) error {
	b.mutex.Lock()
	defer b.mutex.Unlock()
	
	user, exists := b.users[userID]
	if !exists {
		return fmt.Errorf("user not found")
	}
	
	if amount <= 0 {
		return fmt.Errorf("amount must be positive")
	}
	
	if user.Balance < amount {
		return fmt.Errorf("insufficient funds")
	}
	
	user.Balance -= amount
	
	transaction := Transaction{
		ID:        b.nextTxID,
		UserID:    userID,
		Amount:    amount,
		Type:      "withdraw",
		Timestamp: time.Now(),
	}
	
	b.transactions = append(b.transactions, transaction)
	b.nextTxID++
	
	return nil
}

func (b *Bank) GetTransactions(userID int) []Transaction {
	b.mutex.RLock()
	defer b.mutex.RUnlock()
	
	var userTransactions []Transaction
	for _, tx := range b.transactions {
		if tx.UserID == userID {
			userTransactions = append(userTransactions, tx)
		}
	}
	
	return userTransactions
}

func (b *Bank) handleCreateUser(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}
	
	var req struct {
		Name  string `json:"name"`
		Email string `json:"email"`
	}
	
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}
	
	user := b.CreateUser(req.Name, req.Email)
	
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(user)
}

func (b *Bank) handleDeposit(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}
	
	var req struct {
		UserID int     `json:"user_id"`
		Amount float64 `json:"amount"`
	}
	
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}
	
	if err := b.Deposit(req.UserID, req.Amount); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	
	user, _ := b.GetUser(req.UserID)
	
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"message": "Deposit successful",
		"balance": user.Balance,
	})
}

var bank *Bank

func main() {
	bank = NewBank()
	
	// Create some test users
	user1 := bank.CreateUser("Alice Johnson", "alice@example.com")
	user2 := bank.CreateUser("Bob Smith", "bob@example.com")
	
	// Test some operations
	bank.Deposit(user1.ID, 1000.0)
	bank.Deposit(user2.ID, 500.0)
	bank.Withdraw(user1.ID, 150.0)
	
	fmt.Printf("User 1 Balance: $%.2f\n", user1.Balance)
	fmt.Printf("User 2 Balance: $%.2f\n", user2.Balance)
	
	// Print transactions
	fmt.Println("\nUser 1 Transactions:")
	for _, tx := range bank.GetTransactions(user1.ID) {
		fmt.Printf("  %s: $%.2f at %s\n", tx.Type, tx.Amount, tx.Timestamp.Format("15:04:05"))
	}
	
	// Start HTTP server
	http.HandleFunc("/users", bank.handleCreateUser)
	http.HandleFunc("/deposit", bank.handleDeposit)
	
	fmt.Println("\nStarting bank server on :8080...")
	log.Fatal(http.ListenAndServe(":8080", nil))
}