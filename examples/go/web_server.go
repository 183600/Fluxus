package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"strconv"
	"time"
)

type User struct {
	ID       int    `json:"id"`
	Name     string `json:"name"`
	Email    string `json:"email"`
	Created  string `json:"created"`
}

var users []User
var nextID int = 1

func main() {
	// Initialize some sample data
	users = append(users, User{
		ID:      nextID,
		Name:    "John Doe",
		Email:   "john@example.com",
		Created: time.Now().Format("2006-01-02 15:04:05"),
	})
	nextID++

	// Setup routes
	http.HandleFunc("/", homeHandler)
	http.HandleFunc("/users", usersHandler)
	http.HandleFunc("/users/create", createUserHandler)
	http.HandleFunc("/health", healthHandler)

	fmt.Println("Server starting on port 8080...")
	fmt.Println("Available endpoints:")
	fmt.Println("  GET  /           - Home page")
	fmt.Println("  GET  /users      - List all users")
	fmt.Println("  POST /users/create - Create new user")
	fmt.Println("  GET  /health     - Health check")

	log.Fatal(http.ListenAndServe(":8080", nil))
}

func homeHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/html")
	html := `
	<html>
	<head><title>Go Web Server</title></head>
	<body>
		<h1>Welcome to Go Web Server</h1>
		<p>This is a simple web server built with Go.</p>
		<ul>
			<li><a href="/users">View Users</a></li>
			<li><a href="/health">Health Check</a></li>
		</ul>
	</body>
	</html>
	`
	fmt.Fprint(w, html)
}

func usersHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(users)
}

func createUserHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != "POST" {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	name := r.FormValue("name")
	email := r.FormValue("email")

	if name == "" || email == "" {
		http.Error(w, "Name and email are required", http.StatusBadRequest)
		return
	}

	user := User{
		ID:      nextID,
		Name:    name,
		Email:   email,
		Created: time.Now().Format("2006-01-02 15:04:05"),
	}

	users = append(users, user)
	nextID++

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(user)
}

func healthHandler(w http.ResponseWriter, r *http.Request) {
	response := map[string]interface{}{
		"status":    "healthy",
		"timestamp": time.Now().Format(time.RFC3339),
		"users":     len(users),
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(response)
}