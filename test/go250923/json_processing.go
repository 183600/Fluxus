package main

import (
    "fmt"
    "encoding/json"
)

type Config struct {
    Server   string `json:"server"`
    Port     int    `json:"port"`
    Debug    bool   `json:"debug"`
    Features []string `json:"features"`
}

func main() {
    jsonData := `{
        "server": "localhost",
        "port": 8080,
        "debug": true,
        "features": ["auth", "logging", "metrics"]
    }`

    var config Config
    err := json.Unmarshal([]byte(jsonData), &config)
    if err != nil {
        fmt.Printf("Error parsing JSON: %v\n", err)
        return
    }

    fmt.Printf("Server: %s\n", config.Server)
    fmt.Printf("Port: %d\n", config.Port)
    fmt.Printf("Debug: %t\n", config.Debug)
    fmt.Printf("Features: %v\n", config.Features)

    newConfig := Config{
        Server:   "example.com",
        Port:     3000,
        Debug:    false,
        Features: []string{"api", "cache"},
    }

    newJSON, err := json.MarshalIndent(newConfig, "", "  ")
    if err != nil {
        fmt.Printf("Error creating JSON: %v\n", err)
        return
    }

    fmt.Printf("\nNew config JSON:\n%s\n", newJSON)
}