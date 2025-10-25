package main

import (
	"fmt"
	"math/rand"
	"time"
)

type Weather struct {
	City        string
	Temperature float64
	Humidity    int
	Condition   string
	WindSpeed   float64
	Timestamp   time.Time
}

type WeatherStation struct {
	city     string
	readings []Weather
}

func NewWeatherStation(city string) *WeatherStation {
	return &WeatherStation{
		city:     city,
		readings: make([]Weather, 0),
	}
}

func (ws *WeatherStation) GenerateReading() Weather {
	conditions := []string{"Sunny", "Cloudy", "Rainy", "Snowy", "Foggy"}
	
	reading := Weather{
		City:        ws.city,
		Temperature: rand.Float64()*40 - 10, // -10 to 30 degrees
		Humidity:    rand.Intn(100),
		Condition:   conditions[rand.Intn(len(conditions))],
		WindSpeed:   rand.Float64() * 50, // 0 to 50 km/h
		Timestamp:   time.Now(),
	}
	
	ws.readings = append(ws.readings, reading)
	return reading
}

func (ws *WeatherStation) GetAverageTemperature() float64 {
	if len(ws.readings) == 0 {
		return 0
	}
	
	total := 0.0
	for _, reading := range ws.readings {
		total += reading.Temperature
	}
	return total / float64(len(ws.readings))
}

func (ws *WeatherStation) GetMaxTemperature() float64 {
	if len(ws.readings) == 0 {
		return 0
	}
	
	max := ws.readings[0].Temperature
	for _, reading := range ws.readings {
		if reading.Temperature > max {
			max = reading.Temperature
		}
	}
	return max
}

func (ws *WeatherStation) GetMinTemperature() float64 {
	if len(ws.readings) == 0 {
		return 0
	}
	
	min := ws.readings[0].Temperature
	for _, reading := range ws.readings {
		if reading.Temperature < min {
			min = reading.Temperature
		}
	}
	return min
}

func (ws *WeatherStation) PrintCurrentWeather() {
	if len(ws.readings) == 0 {
		fmt.Println("No weather data available")
		return
	}
	
	latest := ws.readings[len(ws.readings)-1]
	fmt.Printf("\n=== Current Weather in %s ===\n", latest.City)
	fmt.Printf("Temperature: %.1f°C\n", latest.Temperature)
	fmt.Printf("Humidity: %d%%\n", latest.Humidity)
	fmt.Printf("Condition: %s\n", latest.Condition)
	fmt.Printf("Wind Speed: %.1f km/h\n", latest.WindSpeed)
	fmt.Printf("Last Updated: %s\n", latest.Timestamp.Format("2006-01-02 15:04:05"))
	fmt.Println("================================")
}

func (ws *WeatherStation) PrintWeatherSummary() {
	if len(ws.readings) == 0 {
		fmt.Println("No weather data available for summary")
		return
	}
	
	fmt.Printf("\n=== Weather Summary for %s ===\n", ws.city)
	fmt.Printf("Total Readings: %d\n", len(ws.readings))
	fmt.Printf("Average Temperature: %.1f°C\n", ws.GetAverageTemperature())
	fmt.Printf("Max Temperature: %.1f°C\n", ws.GetMaxTemperature())
	fmt.Printf("Min Temperature: %.1f°C\n", ws.GetMinTemperature())
	fmt.Println("==================================")
}

func main() {
	fmt.Println("Weather Monitoring System")
	
	rand.Seed(time.Now().UnixNano())
	
	stations := []*WeatherStation{
		NewWeatherStation("New York"),
		NewWeatherStation("London"),
		NewWeatherStation("Tokyo"),
		NewWeatherStation("Sydney"),
	}
	
	// Generate weather readings
	for i := 0; i < 10; i++ {
		for _, station := range stations {
			reading := station.GenerateReading()
			fmt.Printf("%s: %.1f°C, %s, Humidity: %d%%\n", 
				reading.City, reading.Temperature, reading.Condition, reading.Humidity)
		}
		time.Sleep(100 * time.Millisecond) // Small delay for demonstration
	}
	
	// Print current weather for each city
	for _, station := range stations {
		station.PrintCurrentWeather()
	}
	
	// Print weather summaries
	for _, station := range stations {
		station.PrintWeatherSummary()
	}
}