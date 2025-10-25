package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"
	"unicode"
)

type RequestLog struct {
	Timestamp   time.Time `json:"timestamp"`
	Method      string    `json:"method"`
	Path        string    `json:"path"`
	StatusCode  int       `json:"status_code"`
	ResponseSize int      `json:"response_size"`
	UserAgent   string    `json:"user_agent"`
	IP          string    `json:"ip"`
	Duration    int64     `json:"duration_ms"`
	Referer     string    `json:"referer"`
}

type LogAnalyzer struct {
	logs           []RequestLog
	mutex          sync.RWMutex
	patterns       map[string]*regexp.Regexp
	topPaths       map[string]int
	topUserAgents  map[string]int
	topIPs         map[string]int
	statusCodes    map[int]int
	hourlyTraffic  map[int]int
	dailyTraffic   map[string]int
	pathLatency    map[string][]int64
	errorPatterns  []string
	suspiciousIPs  map[string]int
}

func NewLogAnalyzer() *LogAnalyzer {
	la := &LogAnalyzer{
		logs:          make([]RequestLog, 0),
		patterns:      make(map[string]*regexp.Regexp),
		topPaths:      make(map[string]int),
		topUserAgents: make(map[string]int),
		topIPs:        make(map[string]int),
		statusCodes:   make(map[int]int),
		hourlyTraffic: make(map[int]int),
		dailyTraffic:  make(map[string]int),
		pathLatency:   make(map[string][]int64),
		suspiciousIPs: make(map[string]int),
	}
	
	// Common log patterns
	la.patterns["apache_common"] = regexp.MustCompile(`^(\S+) \S+ \S+ \[([^\]]+)\] "(\S+) ([^"]*)" (\d+) (\d+)$`)
	la.patterns["apache_combined"] = regexp.MustCompile(`^(\S+) \S+ \S+ \[([^\]]+)\] "(\S+) ([^"]*)" (\d+) (\d+) "([^"]*)" "([^"]*)"$`)
	la.patterns["nginx"] = regexp.MustCompile(`^(\S+) - \S+ \[([^\]]+)\] "(\S+) ([^"]*)" (\d+) (\d+) "([^"]*)" "([^"]*)" "([^"]*)"$`)
	la.patterns["json"] = regexp.MustCompile(`^\{.*\}$`)
	
	// Suspicious patterns
	la.errorPatterns = []string{
		"sql injection", "script", "alert", "union select",
		"../", "passwd", "admin", "login", "etc/passwd",
		"cmd.exe", "powershell", "/bin/", "wget", "curl",
	}
	
	return la
}

func (la *LogAnalyzer) parseApacheLog(line string, pattern *regexp.Regexp, hasCombined bool) (*RequestLog, error) {
	matches := pattern.FindStringSubmatch(line)
	if matches == nil {
		return nil, fmt.Errorf("line does not match pattern")
	}
	
	// Parse timestamp
	timestamp, err := time.Parse("02/Jan/2006:15:04:05 -0700", matches[2])
	if err != nil {
		timestamp = time.Now()
	}
	
	// Parse status code
	statusCode, err := strconv.Atoi(matches[5])
	if err != nil {
		statusCode = 0
	}
	
	// Parse response size
	responseSize, err := strconv.Atoi(matches[6])
	if err != nil {
		responseSize = 0
	}
	
	log := &RequestLog{
		IP:           matches[1],
		Timestamp:    timestamp,
		Method:       matches[3],
		Path:         matches[4],
		StatusCode:   statusCode,
		ResponseSize: responseSize,
	}
	
	if hasCombined && len(matches) > 8 {
		log.Referer = matches[7]
		log.UserAgent = matches[8]
	}
	
	return log, nil
}

func (la *LogAnalyzer) parseJSONLog(line string) (*RequestLog, error) {
	var log RequestLog
	err := json.Unmarshal([]byte(line), &log)
	if err != nil {
		return nil, err
	}
	return &log, nil
}

func (la *LogAnalyzer) detectLogFormat(line string) string {
	line = strings.TrimSpace(line)
	
	if la.patterns["json"].MatchString(line) {
		return "json"
	} else if la.patterns["apache_combined"].MatchString(line) {
		return "apache_combined"
	} else if la.patterns["apache_common"].MatchString(line) {
		return "apache_common"
	} else if la.patterns["nginx"].MatchString(line) {
		return "nginx"
	}
	
	return "unknown"
}

func (la *LogAnalyzer) parseLine(line string) (*RequestLog, error) {
	line = strings.TrimSpace(line)
	if line == "" {
		return nil, fmt.Errorf("empty line")
	}
	
	format := la.detectLogFormat(line)
	
	switch format {
	case "json":
		return la.parseJSONLog(line)
	case "apache_combined":
		return la.parseApacheLog(line, la.patterns["apache_combined"], true)
	case "apache_common":
		return la.parseApacheLog(line, la.patterns["apache_common"], false)
	case "nginx":
		return la.parseApacheLog(line, la.patterns["nginx"], true)
	default:
		return nil, fmt.Errorf("unknown log format")
	}
}

func (la *LogAnalyzer) ParseLogFile(filename string) error {
	file, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer file.Close()
	
	scanner := bufio.NewScanner(file)
	lineCount := 0
	parseErrors := 0
	
	fmt.Printf("Parsing log file: %s\n", filename)
	
	for scanner.Scan() {
		lineCount++
		line := scanner.Text()
		
		log, err := la.parseLine(line)
		if err != nil {
			parseErrors++
			if parseErrors <= 10 {
				fmt.Printf("Parse error on line %d: %v\n", lineCount, err)
			}
			continue
		}
		
		la.mutex.Lock()
		la.logs = append(la.logs, *log)
		la.mutex.Unlock()
		
		if lineCount%10000 == 0 {
			fmt.Printf("Processed %d lines...\n", lineCount)
		}
	}
	
	if err := scanner.Err(); err != nil {
		return err
	}
	
	fmt.Printf("Finished parsing: %d lines processed, %d parse errors, %d valid logs\n", 
		lineCount, parseErrors, len(la.logs))
	
	return nil
}

func (la *LogAnalyzer) AnalyzeLogs() {
	la.mutex.Lock()
	defer la.mutex.Unlock()
	
	fmt.Println("Analyzing logs...")
	
	for _, log := range la.logs {
		// Count paths
		la.topPaths[log.Path]++
		
		// Count user agents
		if log.UserAgent != "" {
			la.topUserAgents[log.UserAgent]++
		}
		
		// Count IPs
		la.topIPs[log.IP]++
		
		// Count status codes
		la.statusCodes[log.StatusCode]++
		
		// Hourly traffic
		hour := log.Timestamp.Hour()
		la.hourlyTraffic[hour]++
		
		// Daily traffic
		day := log.Timestamp.Format("2006-01-02")
		la.dailyTraffic[day]++
		
		// Path latency
		if log.Duration > 0 {
			la.pathLatency[log.Path] = append(la.pathLatency[log.Path], log.Duration)
		}
		
		// Detect suspicious activity
		la.detectSuspiciousActivity(log)
	}
	
	fmt.Printf("Analysis complete: processed %d log entries\n", len(la.logs))
}

func (la *LogAnalyzer) detectSuspiciousActivity(log RequestLog) {
	suspicious := false
	
	// Check for error patterns in path
	lowerPath := strings.ToLower(log.Path)
	for _, pattern := range la.errorPatterns {
		if strings.Contains(lowerPath, pattern) {
			suspicious = true
			break
		}
	}
	
	// Check for too many requests from same IP
	if la.topIPs[log.IP] > 1000 {
		suspicious = true
	}
	
	// Check for suspicious status codes
	if log.StatusCode >= 400 && log.StatusCode < 500 {
		suspicious = true
	}
	
	// Check for unusually long paths
	if len(log.Path) > 200 {
		suspicious = true
	}
	
	// Check for non-printable characters
	for _, r := range log.Path {
		if !unicode.IsPrint(r) && !unicode.IsSpace(r) {
			suspicious = true
			break
		}
	}
	
	if suspicious {
		la.suspiciousIPs[log.IP]++
	}
}

func (la *LogAnalyzer) GenerateReport() {
	la.mutex.RLock()
	defer la.mutex.RUnlock()
	
	fmt.Println("\n" + strings.Repeat("=", 80))
	fmt.Println("WEB SERVER LOG ANALYSIS REPORT")
	fmt.Println(strings.Repeat("=", 80))
	
	// Basic statistics
	fmt.Printf("Total Requests: %d\n", len(la.logs))
	
	if len(la.logs) > 0 {
		firstLog := la.logs[0]
		lastLog := la.logs[len(la.logs)-1]
		duration := lastLog.Timestamp.Sub(firstLog.Timestamp)
		fmt.Printf("Time Range: %s to %s (%.1f hours)\n", 
			firstLog.Timestamp.Format("2006-01-02 15:04:05"),
			lastLog.Timestamp.Format("2006-01-02 15:04:05"),
			duration.Hours())
		
		if duration.Hours() > 0 {
			fmt.Printf("Average Requests/Hour: %.1f\n", float64(len(la.logs))/duration.Hours())
		}
	}
	
	// Top 10 paths
	fmt.Println("\nTop 10 Requested Paths:")
	fmt.Println(strings.Repeat("-", 50))
	la.printTopN(la.topPaths, 10)
	
	// Top 10 IPs
	fmt.Println("\nTop 10 IP Addresses:")
	fmt.Println(strings.Repeat("-", 50))
	la.printTopN(la.topIPs, 10)
	
	// Status code distribution
	fmt.Println("\nStatus Code Distribution:")
	fmt.Println(strings.Repeat("-", 30))
	for code, count := range la.statusCodes {
		percentage := float64(count) / float64(len(la.logs)) * 100
		fmt.Printf("%-10d %6d requests (%.1f%%)\n", code, count, percentage)
	}
	
	// Hourly traffic pattern
	fmt.Println("\nHourly Traffic Pattern:")
	fmt.Println(strings.Repeat("-", 30))
	for hour := 0; hour < 24; hour++ {
		count := la.hourlyTraffic[hour]
		percentage := float64(count) / float64(len(la.logs)) * 100
		bar := strings.Repeat("█", int(percentage/2))
		fmt.Printf("%02d:00 %6d requests %s (%.1f%%)\n", hour, count, bar, percentage)
	}
	
	// Daily traffic
	fmt.Println("\nDaily Traffic:")
	fmt.Println(strings.Repeat("-", 30))
	
	type DayCount struct {
		Day   string
		Count int
	}
	
	var days []DayCount
	for day, count := range la.dailyTraffic {
		days = append(days, DayCount{day, count})
	}
	
	sort.Slice(days, func(i, j int) bool {
		return days[i].Day < days[j].Day
	})
	
	for _, day := range days {
		fmt.Printf("%-12s %6d requests\n", day.Day, day.Count)
	}
	
	// Path latency analysis
	if len(la.pathLatency) > 0 {
		fmt.Println("\nSlowest Paths (Average Response Time):")
		fmt.Println(strings.Repeat("-", 50))
		
		type PathLatency struct {
			Path    string
			Average float64
			Count   int
		}
		
		var pathLatencies []PathLatency
		for path, latencies := range la.pathLatency {
			if len(latencies) == 0 {
				continue
			}
			
			total := int64(0)
			for _, latency := range latencies {
				total += latency
			}
			
			average := float64(total) / float64(len(latencies))
			pathLatencies = append(pathLatencies, PathLatency{
				Path:    path,
				Average: average,
				Count:   len(latencies),
			})
		}
		
		sort.Slice(pathLatencies, func(i, j int) bool {
			return pathLatencies[i].Average > pathLatencies[j].Average
		})
		
		for i, pl := range pathLatencies {
			if i >= 10 {
				break
			}
			fmt.Printf("%-40s %.1fms (%d requests)\n", 
				la.truncateString(pl.Path, 40), pl.Average, pl.Count)
		}
	}
	
	// Suspicious activity
	if len(la.suspiciousIPs) > 0 {
		fmt.Println("\nSuspicious IP Addresses:")
		fmt.Println(strings.Repeat("-", 40))
		
		type SuspiciousIP struct {
			IP    string
			Count int
		}
		
		var suspiciousIPs []SuspiciousIP
		for ip, count := range la.suspiciousIPs {
			suspiciousIPs = append(suspiciousIPs, SuspiciousIP{ip, count})
		}
		
		sort.Slice(suspiciousIPs, func(i, j int) bool {
			return suspiciousIPs[i].Count > suspiciousIPs[j].Count
		})
		
		for i, sip := range suspiciousIPs {
			if i >= 20 {
				break
			}
			fmt.Printf("%-16s %d suspicious requests\n", sip.IP, sip.Count)
		}
	}
	
	// Error analysis
	fmt.Println("\nError Analysis:")
	fmt.Println(strings.Repeat("-", 30))
	
	errorCodes := []int{400, 401, 403, 404, 500, 502, 503, 504}
	for _, code := range errorCodes {
		if count, exists := la.statusCodes[code]; exists {
			percentage := float64(count) / float64(len(la.logs)) * 100
			fmt.Printf("HTTP %d: %d requests (%.2f%%)\n", code, count, percentage)
		}
	}
}

func (la *LogAnalyzer) printTopN(data map[string]int, n int) {
	type Entry struct {
		Key   string
		Value int
	}
	
	var entries []Entry
	for k, v := range data {
		entries = append(entries, Entry{k, v})
	}
	
	sort.Slice(entries, func(i, j int) bool {
		return entries[i].Value > entries[j].Value
	})
	
	for i, entry := range entries {
		if i >= n {
			break
		}
		percentage := float64(entry.Value) / float64(len(la.logs)) * 100
		fmt.Printf("%-50s %6d requests (%.1f%%)\n", 
			la.truncateString(entry.Key, 50), entry.Value, percentage)
	}
}

func (la *LogAnalyzer) truncateString(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}

func (la *LogAnalyzer) ExportToJSON(filename string) error {
	la.mutex.RLock()
	defer la.mutex.RUnlock()
	
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()
	
	encoder := json.NewEncoder(file)
	encoder.SetIndent("", "  ")
	
	return encoder.Encode(la.logs)
}

func (la *LogAnalyzer) StartHTTPDashboard(port int) {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/html")
		
		html := `
<!DOCTYPE html>
<html>
<head>
    <title>Log Analysis Dashboard</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .metric { margin: 10px 0; padding: 10px; background: #f0f0f0; }
        .chart { width: 100%; height: 200px; background: #fff; border: 1px solid #ccc; }
    </style>
</head>
<body>
    <h1>Web Server Log Analysis Dashboard</h1>
    <div class="metric">
        <h3>Total Requests: %d</h3>
    </div>
    <div class="metric">
        <h3>Unique IPs: %d</h3>
    </div>
    <div class="metric">
        <h3>Unique Paths: %d</h3>
    </div>
    <h2>Recent Activity</h2>
    <div id="recent-logs"></div>
    <script>
        // Simple dashboard - in a real implementation, you'd use proper charting libraries
        setInterval(function() {
            fetch('/api/stats')
                .then(response => response.json())
                .then(data => {
                    // Update dashboard with real-time data
                });
        }, 5000);
    </script>
</body>
</html>`
		
		la.mutex.RLock()
		totalRequests := len(la.logs)
		uniqueIPs := len(la.topIPs)
		uniquePaths := len(la.topPaths)
		la.mutex.RUnlock()
		
		fmt.Fprintf(w, html, totalRequests, uniqueIPs, uniquePaths)
	})
	
	http.HandleFunc("/api/stats", func(w http.ResponseWriter, r *http.Request) {
		la.mutex.RLock()
		stats := map[string]interface{}{
			"total_requests": len(la.logs),
			"unique_ips":     len(la.topIPs),
			"unique_paths":   len(la.topPaths),
			"status_codes":   la.statusCodes,
			"hourly_traffic": la.hourlyTraffic,
		}
		la.mutex.RUnlock()
		
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(stats)
	})
	
	address := fmt.Sprintf(":%d", port)
	fmt.Printf("Starting web dashboard on http://localhost%s\n", address)
	http.ListenAndServe(address, nil)
}

func generateSampleLogFile(filename string, numEntries int) error {
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()
	
	ips := []string{"192.168.1.1", "10.0.0.1", "172.16.0.1", "203.0.113.1", "198.51.100.1"}
	paths := []string{"/", "/index.html", "/api/users", "/api/orders", "/static/css/style.css", "/login", "/admin"}
	userAgents := []string{
		"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
		"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
		"curl/7.68.0",
		"PostmanRuntime/7.26.8",
	}
	
	for i := 0; i < numEntries; i++ {
		timestamp := time.Now().Add(-time.Duration(i) * time.Minute)
		ip := ips[i%len(ips)]
		path := paths[i%len(paths)]
		userAgent := userAgents[i%len(userAgents)]
		statusCode := []int{200, 200, 200, 404, 500}[i%5]
		size := 1024 + (i * 100)
		
		logLine := fmt.Sprintf(`%s - - [%s] "GET %s HTTP/1.1" %d %d "-" "%s"`,
			ip, timestamp.Format("02/Jan/2006:15:04:05 -0700"),
			path, statusCode, size, userAgent)
		
		fmt.Fprintln(file, logLine)
	}
	
	return nil
}

func main() {
	fmt.Println("Advanced Web Server Log Analyzer")
	fmt.Println("================================")
	
	analyzer := NewLogAnalyzer()
	
	// Generate sample log file for demonstration
	sampleFile := "sample_access.log"
	fmt.Printf("Generating sample log file: %s\n", sampleFile)
	err := generateSampleLogFile(sampleFile, 1000)
	if err != nil {
		fmt.Printf("Error generating sample file: %v\n", err)
		return
	}
	
	// Parse log file
	err = analyzer.ParseLogFile(sampleFile)
	if err != nil {
		fmt.Printf("Error parsing log file: %v\n", err)
		return
	}
	
	// Analyze logs
	analyzer.AnalyzeLogs()
	
	// Generate report
	analyzer.GenerateReport()
	
	// Export to JSON
	jsonFile := "log_analysis.json"
	fmt.Printf("\nExporting analysis to %s...\n", jsonFile)
	err = analyzer.ExportToJSON(jsonFile)
	if err != nil {
		fmt.Printf("Error exporting to JSON: %v\n", err)
	} else {
		fmt.Printf("Analysis exported successfully to %s\n", jsonFile)
	}
	
	// Clean up sample file
	os.Remove(sampleFile)
	
	fmt.Println("\nLog analysis completed successfully!")
}