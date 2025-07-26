package main

import (
	"crypto/md5"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

type FileInfo struct {
	Name     string
	Size     int64
	Modified time.Time
	Hash     string
	Type     string
}

type FileAnalyzer struct {
	files []FileInfo
}

func NewFileAnalyzer() *FileAnalyzer {
	return &FileAnalyzer{
		files: make([]FileInfo, 0),
	}
}

func (fa *FileAnalyzer) AddFile(path string) error {
	info, err := os.Stat(path)
	if err != nil {
		return err
	}
	
	if info.IsDir() {
		return nil
	}
	
	hash, err := fa.calculateHash(path)
	if err != nil {
		hash = "error"
	}
	
	fileInfo := FileInfo{
		Name:     filepath.Base(path),
		Size:     info.Size(),
		Modified: info.ModTime(),
		Hash:     hash,
		Type:     filepath.Ext(path),
	}
	
	fa.files = append(fa.files, fileInfo)
	return nil
}

func (fa *FileAnalyzer) calculateHash(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	
	hash := sha256.Sum256(data)
	return hex.EncodeToString(hash[:]), nil
}

func (fa *FileAnalyzer) GetLargestFiles(count int) []FileInfo {
	sorted := make([]FileInfo, len(fa.files))
	copy(sorted, fa.files)
	
	sort.Slice(sorted, func(i, j int) bool {
		return sorted[i].Size > sorted[j].Size
	})
	
	if count > len(sorted) {
		count = len(sorted)
	}
	
	return sorted[:count]
}

func (fa *FileAnalyzer) GetFilesByType() map[string][]FileInfo {
	typeMap := make(map[string][]FileInfo)
	
	for _, file := range fa.files {
		ext := strings.ToLower(file.Type)
		if ext == "" {
			ext = "no extension"
		}
		typeMap[ext] = append(typeMap[ext], file)
	}
	
	return typeMap
}

func (fa *FileAnalyzer) GetTotalSize() int64 {
	var total int64
	for _, file := range fa.files {
		total += file.Size
	}
	return total
}

func (fa *FileAnalyzer) FindDuplicates() map[string][]FileInfo {
	hashMap := make(map[string][]FileInfo)
	
	for _, file := range fa.files {
		if file.Hash != "error" {
			hashMap[file.Hash] = append(hashMap[file.Hash], file)
		}
	}
	
	duplicates := make(map[string][]FileInfo)
	for hash, files := range hashMap {
		if len(files) > 1 {
			duplicates[hash] = files
		}
	}
	
	return duplicates
}

func formatSize(size int64) string {
	const unit = 1024
	if size < unit {
		return fmt.Sprintf("%d B", size)
	}
	div, exp := int64(unit), 0
	for n := size / unit; n >= unit; n /= unit {
		div *= unit
		exp++
	}
	return fmt.Sprintf("%.1f %cB", float64(size)/float64(div), "KMGTPE"[exp])
}

func main() {
	fmt.Println("File Analyzer")
	fmt.Println("=============")
	
	analyzer := NewFileAnalyzer()
	
	// Analyze current directory
	err := filepath.Walk(".", func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil
		}
		analyzer.AddFile(path)
		return nil
	})
	
	if err != nil {
		fmt.Printf("Error walking directory: %v\n", err)
		return
	}
	
	fmt.Printf("Total files analyzed: %d\n", len(analyzer.files))
	fmt.Printf("Total size: %s\n\n", formatSize(analyzer.GetTotalSize()))
	
	// Show largest files
	fmt.Println("Top 5 largest files:")
	largest := analyzer.GetLargestFiles(5)
	for i, file := range largest {
		fmt.Printf("%d. %s - %s\n", i+1, file.Name, formatSize(file.Size))
	}
	
	// Show files by type
	fmt.Println("\nFiles by type:")
	byType := analyzer.GetFilesByType()
	for ext, files := range byType {
		fmt.Printf("%s: %d files\n", ext, len(files))
	}
	
	// Show duplicates
	fmt.Println("\nDuplicate files:")
	duplicates := analyzer.FindDuplicates()
	if len(duplicates) == 0 {
		fmt.Println("No duplicates found")
	} else {
		for hash, files := range duplicates {
			fmt.Printf("Hash %s:\n", hash[:16]+"...")
			for _, file := range files {
				fmt.Printf("  - %s (%s)\n", file.Name, formatSize(file.Size))
			}
		}
	}
}