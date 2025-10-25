package main

import (
	"crypto/md5"
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"
)

type FileInfo struct {
	Path         string
	Size         int64
	ModTime      time.Time
	IsDir        bool
	MD5Hash      string
	SHA256Hash   string
	Extension    string
	LineCount    int
	WordCount    int
	Permissions  os.FileMode
}

type AnalysisResult struct {
	TotalFiles      int
	TotalDirectories int
	TotalSize       int64
	LargestFile     string
	LargestFileSize int64
	FilesByExt      map[string]int
	SizeByExt       map[string]int64
	RecentFiles     []FileInfo
	DuplicateFiles  map[string][]string
	Files           []FileInfo
}

type FileAnalyzer struct {
	maxWorkers   int
	includeHash  bool
	includeCount bool
	mutex        sync.Mutex
}

func NewFileAnalyzer(maxWorkers int, includeHash, includeCount bool) *FileAnalyzer {
	return &FileAnalyzer{
		maxWorkers:   maxWorkers,
		includeHash:  includeHash,
		includeCount: includeCount,
	}
}

func (fa *FileAnalyzer) analyzeFile(path string, info os.FileInfo) FileInfo {
	fileInfo := FileInfo{
		Path:        path,
		Size:        info.Size(),
		ModTime:     info.ModTime(),
		IsDir:       info.IsDir(),
		Extension:   strings.ToLower(filepath.Ext(path)),
		Permissions: info.Mode(),
	}
	
	if !info.IsDir() && fa.includeHash {
		if file, err := os.Open(path); err == nil {
			defer file.Close()
			
			md5Hash := md5.New()
			sha256Hash := sha256.New()
			
			if _, err := io.Copy(io.MultiWriter(md5Hash, sha256Hash), file); err == nil {
				fileInfo.MD5Hash = fmt.Sprintf("%x", md5Hash.Sum(nil))
				fileInfo.SHA256Hash = fmt.Sprintf("%x", sha256Hash.Sum(nil))
			}
		}
	}
	
	if !info.IsDir() && fa.includeCount && isTextFile(path) {
		fileInfo.LineCount, fileInfo.WordCount = countLinesAndWords(path)
	}
	
	return fileInfo
}

func isTextFile(path string) bool {
	textExts := map[string]bool{
		".txt": true, ".go": true, ".py": true, ".js": true,
		".html": true, ".css": true, ".json": true, ".xml": true,
		".md": true, ".yml": true, ".yaml": true, ".toml": true,
		".ini": true, ".cfg": true, ".conf": true, ".log": true,
	}
	ext := strings.ToLower(filepath.Ext(path))
	return textExts[ext]
}

func countLinesAndWords(path string) (int, int) {
	file, err := os.Open(path)
	if err != nil {
		return 0, 0
	}
	defer file.Close()
	
	content, err := io.ReadAll(file)
	if err != nil {
		return 0, 0
	}
	
	text := string(content)
	lines := len(strings.Split(text, "\n"))
	words := len(strings.Fields(text))
	
	return lines, words
}

func (fa *FileAnalyzer) AnalyzeDirectory(rootPath string) (*AnalysisResult, error) {
	result := &AnalysisResult{
		FilesByExt:     make(map[string]int),
		SizeByExt:      make(map[string]int64),
		DuplicateFiles: make(map[string][]string),
		Files:          make([]FileInfo, 0),
	}
	
	fileChan := make(chan string, 100)
	resultChan := make(chan FileInfo, 100)
	
	var wg sync.WaitGroup
	
	// Start workers
	for i := 0; i < fa.maxWorkers; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for path := range fileChan {
				if info, err := os.Stat(path); err == nil {
					fileInfo := fa.analyzeFile(path, info)
					resultChan <- fileInfo
				}
			}
		}()
	}
	
	// Start result collector
	var collectorWG sync.WaitGroup
	collectorWG.Add(1)
	go func() {
		defer collectorWG.Done()
		hashToFiles := make(map[string][]string)
		
		for fileInfo := range resultChan {
			fa.mutex.Lock()
			result.Files = append(result.Files, fileInfo)
			
			if fileInfo.IsDir {
				result.TotalDirectories++
			} else {
				result.TotalFiles++
				result.TotalSize += fileInfo.Size
				
				if fileInfo.Size > result.LargestFileSize {
					result.LargestFileSize = fileInfo.Size
					result.LargestFile = fileInfo.Path
				}
				
				ext := fileInfo.Extension
				if ext == "" {
					ext = "no extension"
				}
				result.FilesByExt[ext]++
				result.SizeByExt[ext] += fileInfo.Size
				
				if fa.includeHash && fileInfo.MD5Hash != "" {
					hashToFiles[fileInfo.MD5Hash] = append(hashToFiles[fileInfo.MD5Hash], fileInfo.Path)
				}
			}
			fa.mutex.Unlock()
		}
		
		// Find duplicates
		for hash, files := range hashToFiles {
			if len(files) > 1 {
				result.DuplicateFiles[hash] = files
			}
		}
	}()
	
	// Walk directory tree
	err := filepath.Walk(rootPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil // Skip files that can't be accessed
		}
		fileChan <- path
		return nil
	})
	
	close(fileChan)
	wg.Wait()
	close(resultChan)
	collectorWG.Wait()
	
	// Find recent files (last 7 days)
	weekAgo := time.Now().AddDate(0, 0, -7)
	for _, file := range result.Files {
		if !file.IsDir && file.ModTime.After(weekAgo) {
			result.RecentFiles = append(result.RecentFiles, file)
		}
	}
	
	return result, err
}

func formatBytes(bytes int64) string {
	const unit = 1024
	if bytes < unit {
		return fmt.Sprintf("%d B", bytes)
	}
	div, exp := int64(unit), 0
	for n := bytes / unit; n >= unit; n /= unit {
		div *= unit
		exp++
	}
	return fmt.Sprintf("%.1f %cB", float64(bytes)/float64(div), "KMGTPE"[exp])
}

func printAnalysisResult(result *AnalysisResult) {
	fmt.Println("File System Analysis Report")
	fmt.Println("===========================")
	
	fmt.Printf("Total Files: %d\n", result.TotalFiles)
	fmt.Printf("Total Directories: %d\n", result.TotalDirectories)
	fmt.Printf("Total Size: %s\n", formatBytes(result.TotalSize))
	fmt.Printf("Largest File: %s (%s)\n", result.LargestFile, formatBytes(result.LargestFileSize))
	
	fmt.Println("\nFiles by Extension:")
	fmt.Println("------------------")
	for ext, count := range result.FilesByExt {
		size := result.SizeByExt[ext]
		fmt.Printf("%-15s: %4d files (%s)\n", ext, count, formatBytes(size))
	}
	
	if len(result.RecentFiles) > 0 {
		fmt.Printf("\nRecent Files (last 7 days): %d\n", len(result.RecentFiles))
		fmt.Println("---------------------------")
		for i, file := range result.RecentFiles {
			if i >= 10 { // Show only first 10
				fmt.Printf("... and %d more\n", len(result.RecentFiles)-10)
				break
			}
			fmt.Printf("%s (%s) - %s\n", file.Path, formatBytes(file.Size), file.ModTime.Format("2006-01-02 15:04"))
		}
	}
	
	if len(result.DuplicateFiles) > 0 {
		fmt.Printf("\nDuplicate Files Found: %d sets\n", len(result.DuplicateFiles))
		fmt.Println("-----------------------------")
		for hash, files := range result.DuplicateFiles {
			fmt.Printf("Hash %s:\n", hash[:12]+"...")
			for _, file := range files {
				fmt.Printf("  - %s\n", file)
			}
			fmt.Println()
		}
	}
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: file_analyzer <directory_path>")
		fmt.Println("Example: file_analyzer /home/user/documents")
		return
	}
	
	rootPath := os.Args[1]
	
	fmt.Printf("Analyzing directory: %s\n", rootPath)
	fmt.Println("This may take a while for large directories...")
	
	analyzer := NewFileAnalyzer(4, true, true)
	
	start := time.Now()
	result, err := analyzer.AnalyzeDirectory(rootPath)
	duration := time.Since(start)
	
	if err != nil {
		fmt.Printf("Error during analysis: %v\n", err)
	}
	
	fmt.Printf("\nAnalysis completed in %v\n\n", duration)
	printAnalysisResult(result)
}