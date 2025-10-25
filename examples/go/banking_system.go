package main

import (
	"fmt"
	"strings"
)

type BankAccount struct {
	accountNumber string
	balance       float64
	ownerName     string
}

func NewBankAccount(accountNumber, ownerName string, initialBalance float64) *BankAccount {
	return &BankAccount{
		accountNumber: accountNumber,
		balance:       initialBalance,
		ownerName:     ownerName,
	}
}

func (ba *BankAccount) Deposit(amount float64) {
	if amount > 0 {
		ba.balance += amount
		fmt.Printf("Deposited $%.2f. New balance: $%.2f\n", amount, ba.balance)
	} else {
		fmt.Println("Invalid deposit amount")
	}
}

func (ba *BankAccount) Withdraw(amount float64) bool {
	if amount > 0 && amount <= ba.balance {
		ba.balance -= amount
		fmt.Printf("Withdrew $%.2f. New balance: $%.2f\n", amount, ba.balance)
		return true
	}
	fmt.Println("Insufficient funds or invalid amount")
	return false
}

func (ba *BankAccount) GetBalance() float64 {
	return ba.balance
}

func (ba *BankAccount) GetAccountInfo() string {
	return fmt.Sprintf("Account: %s, Owner: %s, Balance: $%.2f", 
		ba.accountNumber, ba.ownerName, ba.balance)
}

type Bank struct {
	name     string
	accounts map[string]*BankAccount
}

func NewBank(name string) *Bank {
	return &Bank{
		name:     name,
		accounts: make(map[string]*BankAccount),
	}
}

func (b *Bank) CreateAccount(accountNumber, ownerName string, initialBalance float64) {
	account := NewBankAccount(accountNumber, ownerName, initialBalance)
	b.accounts[accountNumber] = account
	fmt.Printf("Account created: %s\n", account.GetAccountInfo())
}

func (b *Bank) GetAccount(accountNumber string) *BankAccount {
	return b.accounts[accountNumber]
}

func (b *Bank) Transfer(fromAccount, toAccount string, amount float64) {
	from := b.GetAccount(fromAccount)
	to := b.GetAccount(toAccount)
	
	if from == nil || to == nil {
		fmt.Println("One or both accounts not found")
		return
	}
	
	if from.Withdraw(amount) {
		to.Deposit(amount)
		fmt.Printf("Transfer completed: $%.2f from %s to %s\n", 
			amount, fromAccount, toAccount)
	}
}

func (b *Bank) ListAccounts() {
	fmt.Printf("\n=== %s Bank Accounts ===\n", b.name)
	for _, account := range b.accounts {
		fmt.Println(account.GetAccountInfo())
	}
	fmt.Println(strings.Repeat("=", 30))
}

func main() {
	fmt.Println("Banking System Simulation")
	
	bank := NewBank("Central")
	
	bank.CreateAccount("ACC001", "Alice Johnson", 1000.0)
	bank.CreateAccount("ACC002", "Bob Smith", 500.0)
	bank.CreateAccount("ACC003", "Carol Brown", 750.0)
	
	bank.ListAccounts()
	
	acc1 := bank.GetAccount("ACC001")
	acc2 := bank.GetAccount("ACC002")
	
	acc1.Deposit(200.0)
	acc2.Withdraw(100.0)
	
	bank.Transfer("ACC001", "ACC003", 300.0)
	
	bank.ListAccounts()
}