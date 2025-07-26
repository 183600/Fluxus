package main

import (
	"fmt"
	"hash/fnv"
	"log"
	"net/http"
	"strconv"
	"sync"
	"time"
)

type CacheNode struct {
	data   map[string]string
	mutex  sync.RWMutex
	nodeID int
}

type DistributedCache struct {
	nodes     []*CacheNode
	nodeCount int
	mutex     sync.RWMutex
}

func NewDistributedCache(nodeCount int) *DistributedCache {
	cache := &DistributedCache{
		nodes:     make([]*CacheNode, nodeCount),
		nodeCount: nodeCount,
	}
	
	for i := 0; i < nodeCount; i++ {
		cache.nodes[i] = &CacheNode{
			data:   make(map[string]string),
			nodeID: i,
		}
	}
	
	return cache
}

func (dc *DistributedCache) hash(key string) int {
	h := fnv.New32a()
	h.Write([]byte(key))
	return int(h.Sum32()) % dc.nodeCount
}

func (dc *DistributedCache) Set(key, value string) {
	nodeIndex := dc.hash(key)
	node := dc.nodes[nodeIndex]
	
	node.mutex.Lock()
	defer node.mutex.Unlock()
	
	node.data[key] = value
	fmt.Printf("Set %s=%s on node %d\n", key, value, nodeIndex)
}

func (dc *DistributedCache) Get(key string) (string, bool) {
	nodeIndex := dc.hash(key)
	node := dc.nodes[nodeIndex]
	
	node.mutex.RLock()
	defer node.mutex.RUnlock()
	
	value, exists := node.data[key]
	fmt.Printf("Get %s from node %d: %s (exists: %t)\n", key, nodeIndex, value, exists)
	return value, exists
}

func (dc *DistributedCache) Delete(key string) {
	nodeIndex := dc.hash(key)
	node := dc.nodes[nodeIndex]
	
	node.mutex.Lock()
	defer node.mutex.Unlock()
	
	delete(node.data, key)
	fmt.Printf("Deleted %s from node %d\n", key, nodeIndex)
}

func (dc *DistributedCache) GetStats() map[int]int {
	stats := make(map[int]int)
	
	for i, node := range dc.nodes {
		node.mutex.RLock()
		stats[i] = len(node.data)
		node.mutex.RUnlock()
	}
	
	return stats
}

func (dc *DistributedCache) StartHTTPServer(port int) {
	http.HandleFunc("/set", func(w http.ResponseWriter, r *http.Request) {
		key := r.URL.Query().Get("key")
		value := r.URL.Query().Get("value")
		
		if key == "" || value == "" {
			http.Error(w, "Missing key or value", http.StatusBadRequest)
			return
		}
		
		dc.Set(key, value)
		fmt.Fprintf(w, "Set %s=%s\n", key, value)
	})
	
	http.HandleFunc("/get", func(w http.ResponseWriter, r *http.Request) {
		key := r.URL.Query().Get("key")
		
		if key == "" {
			http.Error(w, "Missing key", http.StatusBadRequest)
			return
		}
		
		value, exists := dc.Get(key)
		if !exists {
			http.Error(w, "Key not found", http.StatusNotFound)
			return
		}
		
		fmt.Fprintf(w, "%s\n", value)
	})
	
	http.HandleFunc("/delete", func(w http.ResponseWriter, r *http.Request) {
		key := r.URL.Query().Get("key")
		
		if key == "" {
			http.Error(w, "Missing key", http.StatusBadRequest)
			return
		}
		
		dc.Delete(key)
		fmt.Fprintf(w, "Deleted %s\n", key)
	})
	
	http.HandleFunc("/stats", func(w http.ResponseWriter, r *http.Request) {
		stats := dc.GetStats()
		for nodeID, count := range stats {
			fmt.Fprintf(w, "Node %d: %d items\n", nodeID, count)
		}
	})
	
	addr := ":" + strconv.Itoa(port)
	fmt.Printf("Starting HTTP server on port %d\n", port)
	log.Fatal(http.ListenAndServe(addr, nil))
}

func simulateLoad(cache *DistributedCache) {
	fmt.Println("Starting load simulation...")
	
	for i := 0; i < 1000; i++ {
		key := fmt.Sprintf("key%d", i)
		value := fmt.Sprintf("value%d", i*i)
		cache.Set(key, value)
		
		if i%100 == 0 {
			fmt.Printf("Processed %d items\n", i)
		}
		
		time.Sleep(time.Millisecond)
	}
	
	fmt.Println("Load simulation completed")
	
	stats := cache.GetStats()
	fmt.Println("Final distribution:")
	for nodeID, count := range stats {
		fmt.Printf("Node %d: %d items\n", nodeID, count)
	}
}

func main() {
	fmt.Println("Distributed Cache System")
	fmt.Println("========================")
	
	cache := NewDistributedCache(5)
	
	cache.Set("user:1", "John Doe")
	cache.Set("user:2", "Jane Smith")
	cache.Set("user:3", "Bob Johnson")
	
	if value, exists := cache.Get("user:1"); exists {
		fmt.Printf("Found user:1 = %s\n", value)
	}
	
	if _, exists := cache.Get("user:999"); !exists {
		fmt.Println("user:999 not found (as expected)")
	}
	
	go simulateLoad(cache)
	
	time.Sleep(2 * time.Second)
	
	fmt.Println("Cache operations completed successfully!")
}