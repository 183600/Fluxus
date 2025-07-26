package main

import (
	"bufio"
	"crypto/md5"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"time"
)

type FileInfo struct {
	Path         string
	Size         int64
	ModTime      time.Time
	MD5Hash      string
	SHA256Hash   string
	LineCount    int
	WordCount    int
	CharCount    int
	IsDirectory  bool
	Extension    string
}

type FileAnalyzer struct {
	results   []FileInfo
	mutex     sync.Mutex
	wg        sync.WaitGroup
	processed int
	total     int
}

func NewFileAnalyzer() *FileAnalyzer {
	return &FileAnalyzer{
		results: make([]FileInfo, 0),
	}
}

func (fa *FileAnalyzer) calculateHashes(filePath string) (string, string, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return "", "", err
	}
	defer file.Close()
	
	md5Hash := md5.New()
	sha256Hash := sha256.New()
	
	multiWriter := io.MultiWriter(md5Hash, sha256Hash)
	
	if _, err := io.Copy(multiWriter, file); err != nil {
		return "", "", err
	}
	
	md5Sum := hex.EncodeToString(md5Hash.Sum(nil))
	sha256Sum := hex.EncodeToString(sha256Hash.Sum(nil))
	
	return md5Sum, sha256Sum, nil
}

func (fa *FileAnalyzer) analyzeTextFile(filePath string) (int, int, int, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return 0, 0, 0, err
	}
	defer file.Close()
	
	var lineCount, wordCount, charCount int
	scanner := bufio.NewScanner(file)
	
	for scanner.Scan() {
		line := scanner.Text()
		lineCount++
		charCount += len(line) + 1 // +1 for newline
		
		words := strings.Fields(line)
		wordCount += len(words)
	}
	
	if err := scanner.Err(); err != nil {
		return lineCount, wordCount, charCount, err
	}
	
	return lineCount, wordCount, charCount, nil
}

func (fa *FileAnalyzer) isTextFile(filePath string) bool {
	ext := strings.ToLower(filepath.Ext(filePath))
	textExtensions := []string{
		".txt", ".md", ".go", ".py", ".js", ".html", ".css", ".json",
		".xml", ".yaml", ".yml", ".toml", ".ini", ".cfg", ".conf",
		".sh", ".bash", ".zsh", ".fish", ".ps1", ".bat", ".cmd",
		".c", ".cpp", ".h", ".hpp", ".java", ".cs", ".php", ".rb",
		".rs", ".swift", ".kt", ".scala", ".clj", ".hs", ".ml",
		".sql", ".r", ".m", ".pl", ".perl", ".lua", ".vim",
	}
	
	for _, textExt := range textExtensions {
		if ext == textExt {
			return true
		}
	}
	
	return false
}

func (fa *FileAnalyzer) analyzeFile(filePath string) {
	defer fa.wg.Done()
	
	fileInfo, err := os.Stat(filePath)
	if err != nil {
		fmt.Printf("Error getting file info for %s: %v\n", filePath, err)
		return
	}
	
	info := FileInfo{
		Path:        filePath,
		Size:        fileInfo.Size(),
		ModTime:     fileInfo.ModTime(),
		IsDirectory: fileInfo.IsDir(),
		Extension:   strings.ToLower(filepath.Ext(filePath)),
	}
	
	if !info.IsDirectory && info.Size > 0 {
		md5Hash, sha256Hash, err := fa.calculateHashes(filePath)
		if err != nil {
			fmt.Printf("Error calculating hashes for %s: %v\n", filePath, err)
		} else {
			info.MD5Hash = md5Hash
			info.SHA256Hash = sha256Hash
		}
		
		if fa.isTextFile(filePath) && info.Size < 10*1024*1024 { // Only analyze text files smaller than 10MB
			lineCount, wordCount, charCount, err := fa.analyzeTextFile(filePath)
			if err != nil {
				fmt.Printf("Error analyzing text file %s: %v\n", filePath, err)
			} else {
				info.LineCount = lineCount
				info.WordCount = wordCount
				info.CharCount = charCount
			}
		}
	}
	
	fa.mutex.Lock()
	fa.results = append(fa.results, info)
	fa.processed++
	if fa.processed%100 == 0 {
		fmt.Printf("Processed %d/%d files\n", fa.processed, fa.total)
	}
	fa.mutex.Unlock()
}

func (fa *FileAnalyzer) walkDirectory(rootPath string) error {
	var filePaths []string
	
	err := filepath.Walk(rootPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			fmt.Printf("Error accessing %s: %v\n", path, err)
			return nil
		}
		
		filePaths = append(filePaths, path)
		return nil
	})
	
	if err != nil {
		return err
	}
	
	fa.total = len(filePaths)
	fmt.Printf("Found %d files/directories to analyze\n", fa.total)
	
	maxWorkers := 50
	semaphore := make(chan struct{}, maxWorkers)
	
	for _, filePath := range filePaths {
		fa.wg.Add(1)
		go func(path string) {
			semaphore <- struct{}{}
			fa.analyzeFile(path)
			<-semaphore
		}(filePath)
	}
	
	fa.wg.Wait()
	return nil
}

func (fa *FileAnalyzer) generateReport() {
	fmt.Println("\n" + strings.Repeat("=", 80))
	fmt.Println("FILE ANALYSIS REPORT")
	fmt.Println(strings.Repeat("=", 80))
	
	totalFiles := 0
	totalDirs := 0
	totalSize := int64(0)
	extensionStats := make(map[string]int)
	sizeDistribution := make(map[string]int)
	
	for _, info := range fa.results {
		if info.IsDirectory {
			totalDirs++
		} else {
			totalFiles++
			totalSize += info.Size
			
			if info.Extension != "" {
				extensionStats[info.Extension]++
			} else {
				extensionStats["[no extension]"]++
			}
			
			sizeCategory := categorizeFileSize(info.Size)
			sizeDistribution[sizeCategory]++
		}
	}
	
	fmt.Printf("Total Files: %d\n", totalFiles)
	fmt.Printf("Total Directories: %d\n", totalDirs)
	fmt.Printf("Total Size: %s\n", formatBytes(totalSize))
	fmt.Printf("Average File Size: %s\n", formatBytes(totalSize/int64(max(totalFiles, 1))))
	
	fmt.Println("\nTop 10 File Extensions:")
	fmt.Println(strings.Repeat("-", 40))
	
	type ExtStat struct {
		Ext   string
		Count int
	}
	
	var extStats []ExtStat
	for ext, count := range extensionStats {
		extStats = append(extStats, ExtStat{ext, count})
	}
	
	sort.Slice(extStats, func(i, j int) bool {
		return extStats[i].Count > extStats[j].Count
	})
	
	for i, stat := range extStats {
		if i >= 10 {
			break
		}
		fmt.Printf("%-20s %d files\n", stat.Ext, stat.Count)
	}
	
	fmt.Println("\nFile Size Distribution:")
	fmt.Println(strings.Repeat("-", 40))
	for category, count := range sizeDistribution {
		fmt.Printf("%-20s %d files\n", category, count)
	}
	
	fmt.Println("\nLargest Files:")
	fmt.Println(strings.Repeat("-", 40))
	
	nonDirFiles := make([]FileInfo, 0)
	for _, info := range fa.results {
		if !info.IsDirectory {
			nonDirFiles = append(nonDirFiles, info)
		}
	}
	
	sort.Slice(nonDirFiles, func(i, j int) bool {
		return nonDirFiles[i].Size > nonDirFiles[j].Size
	})
	
	for i, info := range nonDirFiles {
		if i >= 10 {
			break
		}
		fmt.Printf("%-50s %s\n", truncatePath(info.Path, 50), formatBytes(info.Size))
	}
	
	duplicateHashes := findDuplicates(nonDirFiles)
	if len(duplicateHashes) > 0 {
		fmt.Println("\nDuplicate Files (by MD5 hash):")
		fmt.Println(strings.Repeat("-", 40))
		for hash, files := range duplicateHashes {
			if len(files) > 1 {
				fmt.Printf("Hash: %s\n", hash[:16]+"...")
				for _, file := range files {
					fmt.Printf("  %s (%s)\n", file.Path, formatBytes(file.Size))
				}
				fmt.Println()
			}
		}
	}
}

func findDuplicates(files []FileInfo) map[string][]FileInfo {
	hashMap := make(map[string][]FileInfo)
	
	for _, file := range files {
		if file.MD5Hash != "" {
			hashMap[file.MD5Hash] = append(hashMap[file.MD5Hash], file)
		}
	}
	
	duplicates := make(map[string][]FileInfo)
	for hash, fileList := range hashMap {
		if len(fileList) > 1 {
			duplicates[hash] = fileList
		}
	}
	
	return duplicates
}

func categorizeFileSize(size int64) string {
	if size < 1024 {
		return "< 1 KB"
	} else if size < 1024*1024 {
		return "1 KB - 1 MB"
	} else if size < 10*1024*1024 {
		return "1 MB - 10 MB"
	} else if size < 100*1024*1024 {
		return "10 MB - 100 MB"
	} else {
		return "> 100 MB"
	}
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

func truncatePath(path string, maxLen int) string {
	if len(path) <= maxLen {
		return path
	}
	return "..." + path[len(path)-maxLen+3:]
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func main() {
	fmt.Println("Advanced File System Analyzer")
	fmt.Println("=============================")
	
	rootPath := "."
	if len(os.Args) > 1 {
		rootPath = os.Args[1]
	}
	
	fmt.Printf("Analyzing directory: %s\n", rootPath)
	
	startTime := time.Now()
	analyzer := NewFileAnalyzer()
	
	if err := analyzer.walkDirectory(rootPath); err != nil {
		fmt.Printf("Error analyzing directory: %v\n", err)
		os.Exit(1)
	}
	
	duration := time.Since(startTime)
	fmt.Printf("\nAnalysis completed in %v\n", duration)
	
	analyzer.generateReport()
}