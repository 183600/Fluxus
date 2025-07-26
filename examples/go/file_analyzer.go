package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"
)

type FileInfo struct {
	Path     string
	Size     int64
	ModTime  time.Time
	IsDir    bool
	Lines    int
	Words    int
	Language string
}

type ProjectStats struct {
	TotalFiles  int
	TotalLines  int
	TotalWords  int
	TotalSize   int64
	Languages   map[string]int
	LargestFile FileInfo
}

func detectLanguage(filename string) string {
	ext := strings.ToLower(filepath.Ext(filename))
	switch ext {
	case ".go":
		return "Go"
	case ".py":
		return "Python"
	case ".js":
		return "JavaScript"
	case ".ts":
		return "TypeScript"
	case ".java":
		return "Java"
	case ".cpp", ".cc", ".cxx":
		return "C++"
	case ".c":
		return "C"
	case ".rs":
		return "Rust"
	case ".php":
		return "PHP"
	case ".rb":
		return "Ruby"
	case ".cs":
		return "C#"
	case ".html":
		return "HTML"
	case ".css":
		return "CSS"
	case ".json":
		return "JSON"
	case ".xml":
		return "XML"
	case ".md":
		return "Markdown"
	case ".txt":
		return "Text"
	default:
		return "Unknown"
	}
}

func countLinesAndWords(filepath string) (int, int, error) {
	file, err := os.Open(filepath)
	if err != nil {
		return 0, 0, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lines := 0
	words := 0

	for scanner.Scan() {
		lines++
		line := strings.TrimSpace(scanner.Text())
		if line != "" {
			wordCount := len(strings.Fields(line))
			words += wordCount
		}
	}

	return lines, words, scanner.Err()
}

func analyzeFile(path string, info os.FileInfo) FileInfo {
	fileInfo := FileInfo{
		Path:     path,
		Size:     info.Size(),
		ModTime:  info.ModTime(),
		IsDir:    info.IsDir(),
		Language: detectLanguage(path),
	}

	if !info.IsDir() && info.Size() > 0 {
		lines, words, err := countLinesAndWords(path)
		if err == nil {
			fileInfo.Lines = lines
			fileInfo.Words = words
		}
	}

	return fileInfo
}

func searchInFile(filepath string, pattern *regexp.Regexp) ([]string, error) {
	file, err := os.Open(filepath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var matches []string
	scanner := bufio.NewScanner(file)
	lineNum := 1

	for scanner.Scan() {
		line := scanner.Text()
		if pattern.MatchString(line) {
			matches = append(matches, fmt.Sprintf("%s:%d:%s", filepath, lineNum, strings.TrimSpace(line)))
		}
		lineNum++
	}

	return matches, scanner.Err()
}

func walkDirectory(root string, stats *ProjectStats, pattern *regexp.Regexp) error {
	return filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Skip hidden files and directories
		if strings.HasPrefix(info.Name(), ".") && info.Name() != "." {
			if info.IsDir() {
				return filepath.SkipDir
			}
			return nil
		}

		// Skip common build/dependency directories
		skipDirs := []string{"node_modules", "vendor", "target", "build", ".git"}
		for _, skipDir := range skipDirs {
			if info.IsDir() && info.Name() == skipDir {
				return filepath.SkipDir
			}
		}

		fileInfo := analyzeFile(path, info)

		if !info.IsDir() {
			stats.TotalFiles++
			stats.TotalLines += fileInfo.Lines
			stats.TotalWords += fileInfo.Words
			stats.TotalSize += fileInfo.Size

			if stats.Languages == nil {
				stats.Languages = make(map[string]int)
			}
			stats.Languages[fileInfo.Language]++

			if fileInfo.Size > stats.LargestFile.Size {
				stats.LargestFile = fileInfo
			}

			// Search for pattern if provided
			if pattern != nil && fileInfo.Language != "Unknown" {
				matches, err := searchInFile(path, pattern)
				if err == nil && len(matches) > 0 {
					fmt.Printf("\nMatches in %s:\n", path)
					for _, match := range matches {
						fmt.Println("  " + match)
					}
				}
			}
		}

		return nil
	})
}

func printStats(stats ProjectStats) {
	fmt.Println("\n=== Project Statistics ===")
	fmt.Printf("Total Files: %d\n", stats.TotalFiles)
	fmt.Printf("Total Lines: %d\n", stats.TotalLines)
	fmt.Printf("Total Words: %d\n", stats.TotalWords)
	fmt.Printf("Total Size: %.2f KB\n", float64(stats.TotalSize)/1024)

	fmt.Println("\nLanguage Distribution:")
	type langStat struct {
		Language string
		Count    int
	}

	var langStats []langStat
	for lang, count := range stats.Languages {
		langStats = append(langStats, langStat{lang, count})
	}

	sort.Slice(langStats, func(i, j int) bool {
		return langStats[i].Count > langStats[j].Count
	})

	for _, stat := range langStats {
		percentage := float64(stat.Count) / float64(stats.TotalFiles) * 100
		fmt.Printf("  %s: %d files (%.1f%%)\n", stat.Language, stat.Count, percentage)
	}

	fmt.Printf("\nLargest File: %s (%.2f KB, %d lines)\n",
		stats.LargestFile.Path,
		float64(stats.LargestFile.Size)/1024,
		stats.LargestFile.Lines)
}

func main() {
	fmt.Println("=== Advanced File System Analyzer ===")

	// Get command line arguments
	args := os.Args[1:]
	var rootPath string
	var searchPattern string

	if len(args) == 0 {
		rootPath = "."
	} else {
		rootPath = args[0]
		if len(args) > 1 {
			searchPattern = args[1]
		}
	}

	// Validate root path
	if _, err := os.Stat(rootPath); os.IsNotExist(err) {
		fmt.Printf("Error: Path '%s' does not exist\n", rootPath)
		return
	}

	fmt.Printf("Analyzing directory: %s\n", rootPath)
	if searchPattern != "" {
		fmt.Printf("Searching for pattern: %s\n", searchPattern)
	}

	var pattern *regexp.Regexp
	var err error
	if searchPattern != "" {
		pattern, err = regexp.Compile(searchPattern)
		if err != nil {
			fmt.Printf("Error compiling regex pattern: %v\n", err)
			return
		}
	}

	stats := &ProjectStats{}
	startTime := time.Now()

	err = walkDirectory(rootPath, stats, pattern)
	if err != nil {
		fmt.Printf("Error walking directory: %v\n", err)
		return
	}

	duration := time.Since(startTime)
	printStats(*stats)

	fmt.Printf("\nAnalysis completed in: %v\n", duration)

	// Interactive mode
	fmt.Println("\n=== Interactive Mode ===")
	fmt.Println("Enter commands (type 'help' for available commands, 'quit' to exit):")

	scanner := bufio.NewScanner(os.Stdin)
	for {
		fmt.Print("> ")
		if !scanner.Scan() {
			break
		}

		command := strings.TrimSpace(scanner.Text())
		parts := strings.Fields(command)

		if len(parts) == 0 {
			continue
		}

		switch parts[0] {
		case "help":
			fmt.Println("Available commands:")
			fmt.Println("  help              - Show this help")
			fmt.Println("  stats             - Show project statistics")
			fmt.Println("  search <pattern>  - Search for pattern in files")
			fmt.Println("  find <filename>   - Find files by name")
			fmt.Println("  large [n]         - Show n largest files (default: 10)")
			fmt.Println("  recent [n]        - Show n most recently modified files (default: 10)")
			fmt.Println("  quit              - Exit program")

		case "stats":
			printStats(*stats)

		case "search":
			if len(parts) < 2 {
				fmt.Println("Usage: search <pattern>")
				continue
			}
			searchPattern := strings.Join(parts[1:], " ")
			pattern, err := regexp.Compile(searchPattern)
			if err != nil {
				fmt.Printf("Error compiling regex pattern: %v\n", err)
				continue
			}
			fmt.Printf("Searching for pattern: %s\n", searchPattern)
			newStats := &ProjectStats{}
			walkDirectory(rootPath, newStats, pattern)

		case "find":
			if len(parts) < 2 {
				fmt.Println("Usage: find <filename>")
				continue
			}
			filename := parts[1]
			fmt.Printf("Searching for files containing '%s':\n", filename)
			filepath.Walk(rootPath, func(path string, info os.FileInfo, err error) error {
				if err != nil {
					return nil
				}
				if strings.Contains(strings.ToLower(info.Name()), strings.ToLower(filename)) {
					fmt.Printf("  %s\n", path)
				}
				return nil
			})

		case "quit", "exit":
			fmt.Println("Goodbye!")
			return

		default:
			fmt.Printf("Unknown command: %s (type 'help' for available commands)\n", parts[0])
		}
	}
}