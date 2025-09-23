package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"time"

	_ "github.com/mattn/go-sqlite3"
)

type User struct {
	ID        int       `json:"id"`
	Name      string    `json:"name"`
	Email     string    `json:"email"`
	CreatedAt time.Time `json:"created_at"`
	UpdatedAt time.Time `json:"updated_at"`
}

type Product struct {
	ID          int       `json:"id"`
	Name        string    `json:"name"`
	Price       float64   `json:"price"`
	Category    string    `json:"category"`
	Stock       int       `json:"stock"`
	CreatedAt   time.Time `json:"created_at"`
}

type Order struct {
	ID        int       `json:"id"`
	UserID    int       `json:"user_id"`
	ProductID int       `json:"product_id"`
	Quantity  int       `json:"quantity"`
	Total     float64   `json:"total"`
	Status    string    `json:"status"`
	CreatedAt time.Time `json:"created_at"`
}

type DatabaseManager struct {
	db *sql.DB
}

func NewDatabaseManager(dataSourceName string) (*DatabaseManager, error) {
	db, err := sql.Open("sqlite3", dataSourceName)
	if err != nil {
		return nil, err
	}

	return &DatabaseManager{db: db}, nil
}

func (dm *DatabaseManager) Close() error {
	return dm.db.Close()
}

func (dm *DatabaseManager) Initialize() error {
	schemas := []string{
		`CREATE TABLE IF NOT EXISTS users (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			name TEXT NOT NULL,
			email TEXT UNIQUE NOT NULL,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
			updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
		)`,
		`CREATE TABLE IF NOT EXISTS products (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			name TEXT NOT NULL,
			price REAL NOT NULL,
			category TEXT NOT NULL,
			stock INTEGER DEFAULT 0,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP
		)`,
		`CREATE TABLE IF NOT EXISTS orders (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			user_id INTEGER NOT NULL,
			product_id INTEGER NOT NULL,
			quantity INTEGER NOT NULL,
			total REAL NOT NULL,
			status TEXT DEFAULT 'pending',
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
			FOREIGN KEY (user_id) REFERENCES users (id),
			FOREIGN KEY (product_id) REFERENCES products (id)
		)`,
	}

	for _, schema := range schemas {
		_, err := dm.db.Exec(schema)
		if err != nil {
			return err
		}
	}

	return nil
}

func (dm *DatabaseManager) CreateUser(user *User) (int64, error) {
	query := `INSERT INTO users (name, email) VALUES (?, ?)`
	result, err := dm.db.Exec(query, user.Name, user.Email)
	if err != nil {
		return 0, err
	}

	return result.LastInsertId()
}

func (dm *DatabaseManager) GetUserByID(id int) (*User, error) {
	query := `SELECT id, name, email, created_at, updated_at FROM users WHERE id = ?`
	row := dm.db.QueryRow(query, id)

	var user User
	err := row.Scan(&user.ID, &user.Name, &user.Email, &user.CreatedAt, &user.UpdatedAt)
	if err != nil {
		return nil, err
	}

	return &user, nil
}

func (dm *DatabaseManager) GetAllUsers() ([]User, error) {
	query := `SELECT id, name, email, created_at, updated_at FROM users`
	rows, err := dm.db.Query(query)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var users []User
	for rows.Next() {
		var user User
		err := rows.Scan(&user.ID, &user.Name, &user.Email, &user.CreatedAt, &user.UpdatedAt)
		if err != nil {
			return nil, err
		}
		users = append(users, user)
	}

	return users, nil
}

func (dm *DatabaseManager) CreateProduct(product *Product) (int64, error) {
	query := `INSERT INTO products (name, price, category, stock) VALUES (?, ?, ?, ?)`
	result, err := dm.db.Exec(query, product.Name, product.Price, product.Category, product.Stock)
	if err != nil {
		return 0, err
	}

	return result.LastInsertId()
}

func (dm *DatabaseManager) CreateOrder(order *Order) (int64, error) {
	tx, err := dm.db.Begin()
	if err != nil {
		return 0, err
	}

	defer tx.Rollback()

	var productPrice float64
	err = tx.QueryRow("SELECT price FROM products WHERE id = ?", order.ProductID).Scan(&productPrice)
	if err != nil {
		return 0, err
	}

	order.Total = float64(order.Quantity) * productPrice

	query := `INSERT INTO orders (user_id, product_id, quantity, total, status) VALUES (?, ?, ?, ?, ?)`
	result, err := tx.Exec(query, order.UserID, order.ProductID, order.Quantity, order.Total, order.Status)
	if err != nil {
		return 0, err
	}

	_, err = tx.Exec("UPDATE products SET stock = stock - ? WHERE id = ?", order.Quantity, order.ProductID)
	if err != nil {
		return 0, err
	}

	err = tx.Commit()
	if err != nil {
		return 0, err
	}

	return result.LastInsertId()
}

func (dm *DatabaseManager) GetOrdersByUserID(userID int) ([]Order, error) {
	query := `SELECT id, user_id, product_id, quantity, total, status, created_at FROM orders WHERE user_id = ?`
	rows, err := dm.db.Query(query, userID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var orders []Order
	for rows.Next() {
		var order Order
		err := rows.Scan(&order.ID, &order.UserID, &order.ProductID, &order.Quantity, &order.Total, &order.Status, &order.CreatedAt)
		if err != nil {
			return nil, err
		}
		orders = append(orders, order)
	}

	return orders, nil
}

func main() {
	fmt.Println("=== Database Patterns Example ===")

	dbManager, err := NewDatabaseManager("./test.db")
	if err != nil {
		log.Fatal("Error creating database manager:", err)
	}
	defer dbManager.Close()

	err = dbManager.Initialize()
	if err != nil {
		log.Fatal("Error initializing database:", err)
	}

	fmt.Println("Database initialized successfully")

	userID, err := dbManager.CreateUser(&User{
		Name:  "John Doe",
		Email: "john@example.com",
	})
	if err != nil {
		fmt.Printf("User already exists or error creating user: %v\n", err)
		userID = 1 // Assume user exists with ID 1
	} else {
		fmt.Printf("User created with ID: %d\n", userID)
	}

	productID, err := dbManager.CreateProduct(&Product{
		Name:     "Laptop",
		Price:    999.99,
		Category: "Electronics",
		Stock:    10,
	})
	if err != nil {
		fmt.Printf("Product already exists or error creating product: %v\n", err)
		productID = 1 // Assume product exists with ID 1
	} else {
		fmt.Printf("Product created with ID: %d\n", productID)
	}

	orderID, err := dbManager.CreateOrder(&Order{
		UserID:    int(userID),
		ProductID: int(productID),
		Quantity:  2,
		Status:    "completed",
	})
	if err != nil {
		log.Fatal("Error creating order:", err)
	}
	fmt.Printf("Order created with ID: %d\n", orderID)

	user, err := dbManager.GetUserByID(int(userID))
	if err != nil {
		log.Fatal("Error getting user:", err)
	}
	fmt.Printf("Retrieved user: %+v\n", user)

	users, err := dbManager.GetAllUsers()
	if err != nil {
		log.Fatal("Error getting users:", err)
	}
	fmt.Printf("Total users: %d\n", len(users))

	orders, err := dbManager.GetOrdersByUserID(int(userID))
	if err != nil {
		log.Fatal("Error getting orders:", err)
	}
	fmt.Printf("User orders: %d\n", len(orders))

	fmt.Println("\n=== JSON Serialization Example ===")
	userJSON, err := json.MarshalIndent(user, "", "  ")
	if err != nil {
		log.Fatal("Error marshaling user to JSON:", err)
	}
	fmt.Printf("User JSON:\n%s\n", userJSON)

	fmt.Println("\nDatabase operations completed successfully!")
}