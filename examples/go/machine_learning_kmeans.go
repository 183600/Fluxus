package main

import (
	"fmt"
	"math"
	"math/rand"
	"sort"
	"sync"
	"time"
)

type Point struct {
	X, Y float64
}

type Cluster struct {
	Center Point
	Points []Point
	ID     int
}

type KMeans struct {
	K        int
	MaxIter  int
	Clusters []Cluster
	mutex    sync.RWMutex
}

func NewKMeans(k, maxIter int) *KMeans {
	return &KMeans{
		K:       k,
		MaxIter: maxIter,
	}
}

func (km *KMeans) distance(p1, p2 Point) float64 {
	dx := p1.X - p2.X
	dy := p1.Y - p2.Y
	return math.Sqrt(dx*dx + dy*dy)
}

func (km *KMeans) initializeClusters(points []Point) {
	km.Clusters = make([]Cluster, km.K)
	
	// Use K-means++ initialization
	rand.Seed(time.Now().UnixNano())
	
	// Choose first center randomly
	firstCenter := points[rand.Intn(len(points))]
	km.Clusters[0] = Cluster{
		Center: firstCenter,
		Points: make([]Point, 0),
		ID:     0,
	}
	
	// Choose remaining centers using K-means++
	for i := 1; i < km.K; i++ {
		distances := make([]float64, len(points))
		totalDistance := 0.0
		
		for j, point := range points {
			minDist := math.Inf(1)
			for k := 0; k < i; k++ {
				dist := km.distance(point, km.Clusters[k].Center)
				if dist < minDist {
					minDist = dist
				}
			}
			distances[j] = minDist * minDist
			totalDistance += distances[j]
		}
		
		// Select next center with probability proportional to squared distance
		r := rand.Float64() * totalDistance
		cumulative := 0.0
		
		for j, dist := range distances {
			cumulative += dist
			if cumulative >= r {
				km.Clusters[i] = Cluster{
					Center: points[j],
					Points: make([]Point, 0),
					ID:     i,
				}
				break
			}
		}
	}
}

func (km *KMeans) assignPointsToClusters(points []Point) bool {
	km.mutex.Lock()
	defer km.mutex.Unlock()
	
	// Clear previous assignments
	for i := range km.Clusters {
		km.Clusters[i].Points = make([]Point, 0)
	}
	
	changed := false
	
	for _, point := range points {
		minDistance := math.Inf(1)
		nearestCluster := 0
		
		for i, cluster := range km.Clusters {
			dist := km.distance(point, cluster.Center)
			if dist < minDistance {
				minDistance = dist
				nearestCluster = i
			}
		}
		
		km.Clusters[nearestCluster].Points = append(km.Clusters[nearestCluster].Points, point)
		changed = true
	}
	
	return changed
}

func (km *KMeans) updateClusterCenters() bool {
	km.mutex.Lock()
	defer km.mutex.Unlock()
	
	changed := false
	tolerance := 1e-4
	
	for i := range km.Clusters {
		if len(km.Clusters[i].Points) == 0 {
			continue
		}
		
		var sumX, sumY float64
		for _, point := range km.Clusters[i].Points {
			sumX += point.X
			sumY += point.Y
		}
		
		newCenter := Point{
			X: sumX / float64(len(km.Clusters[i].Points)),
			Y: sumY / float64(len(km.Clusters[i].Points)),
		}
		
		if km.distance(km.Clusters[i].Center, newCenter) > tolerance {
			km.Clusters[i].Center = newCenter
			changed = true
		}
	}
	
	return changed
}

func (km *KMeans) Fit(points []Point) error {
	if len(points) < km.K {
		return fmt.Errorf("number of points (%d) must be >= K (%d)", len(points), km.K)
	}
	
	km.initializeClusters(points)
	
	fmt.Printf("Starting K-means clustering with K=%d, MaxIter=%d\n", km.K, km.MaxIter)
	
	for iter := 0; iter < km.MaxIter; iter++ {
		fmt.Printf("Iteration %d/%d\n", iter+1, km.MaxIter)
		
		km.assignPointsToClusters(points)
		
		if !km.updateClusterCenters() {
			fmt.Printf("Converged after %d iterations\n", iter+1)
			break
		}
		
		if iter%10 == 9 {
			fmt.Printf("Inertia: %.4f\n", km.calculateInertia())
		}
	}
	
	return nil
}

func (km *KMeans) calculateInertia() float64 {
	km.mutex.RLock()
	defer km.mutex.RUnlock()
	
	inertia := 0.0
	
	for _, cluster := range km.Clusters {
		for _, point := range cluster.Points {
			dist := km.distance(point, cluster.Center)
			inertia += dist * dist
		}
	}
	
	return inertia
}

func (km *KMeans) calculateSilhouetteScore(points []Point) float64 {
	if km.K <= 1 {
		return 0.0
	}
	
	totalScore := 0.0
	validPoints := 0
	
	for _, cluster := range km.Clusters {
		for _, point := range cluster.Points {
			a := km.calculateIntraClusterDistance(point, cluster)
			b := km.calculateNearestClusterDistance(point, cluster.ID)
			
			if a == 0 && b == 0 {
				continue
			}
			
			silhouette := (b - a) / math.Max(a, b)
			totalScore += silhouette
			validPoints++
		}
	}
	
	if validPoints == 0 {
		return 0.0
	}
	
	return totalScore / float64(validPoints)
}

func (km *KMeans) calculateIntraClusterDistance(point Point, cluster Cluster) float64 {
	if len(cluster.Points) <= 1 {
		return 0.0
	}
	
	totalDistance := 0.0
	count := 0
	
	for _, clusterPoint := range cluster.Points {
		if clusterPoint.X != point.X || clusterPoint.Y != point.Y {
			totalDistance += km.distance(point, clusterPoint)
			count++
		}
	}
	
	if count == 0 {
		return 0.0
	}
	
	return totalDistance / float64(count)
}

func (km *KMeans) calculateNearestClusterDistance(point Point, currentClusterID int) float64 {
	minDistance := math.Inf(1)
	
	for _, cluster := range km.Clusters {
		if cluster.ID == currentClusterID || len(cluster.Points) == 0 {
			continue
		}
		
		totalDistance := 0.0
		for _, clusterPoint := range cluster.Points {
			totalDistance += km.distance(point, clusterPoint)
		}
		
		avgDistance := totalDistance / float64(len(cluster.Points))
		if avgDistance < minDistance {
			minDistance = avgDistance
		}
	}
	
	return minDistance
}

func (km *KMeans) PrintResults() {
	km.mutex.RLock()
	defer km.mutex.RUnlock()
	
	fmt.Println("\nClustering Results:")
	fmt.Println("==================")
	
	for i, cluster := range km.Clusters {
		fmt.Printf("Cluster %d (Center: %.2f, %.2f): %d points\n", 
			i, cluster.Center.X, cluster.Center.Y, len(cluster.Points))
		
		if len(cluster.Points) <= 10 {
			for j, point := range cluster.Points {
				fmt.Printf("  Point %d: (%.2f, %.2f)\n", j+1, point.X, point.Y)
			}
		} else {
			fmt.Printf("  [First 5 points]\n")
			for j := 0; j < 5; j++ {
				point := cluster.Points[j]
				fmt.Printf("  Point %d: (%.2f, %.2f)\n", j+1, point.X, point.Y)
			}
			fmt.Printf("  ... and %d more points\n", len(cluster.Points)-5)
		}
		fmt.Println()
	}
	
	inertia := km.calculateInertia()
	fmt.Printf("Final Inertia: %.4f\n", inertia)
}

func generateRandomPoints(n int, clusters int) []Point {
	rand.Seed(time.Now().UnixNano())
	points := make([]Point, n)
	
	// Generate points around random centers
	clusterCenters := make([]Point, clusters)
	for i := 0; i < clusters; i++ {
		clusterCenters[i] = Point{
			X: rand.Float64() * 100,
			Y: rand.Float64() * 100,
		}
	}
	
	pointsPerCluster := n / clusters
	
	for i := 0; i < n; i++ {
		centerIndex := i / pointsPerCluster
		if centerIndex >= clusters {
			centerIndex = clusters - 1
		}
		
		center := clusterCenters[centerIndex]
		
		// Add some noise around the center
		noise := 10.0
		points[i] = Point{
			X: center.X + (rand.Float64()-0.5)*2*noise,
			Y: center.Y + (rand.Float64()-0.5)*2*noise,
		}
	}
	
	// Shuffle the points
	for i := range points {
		j := rand.Intn(i + 1)
		points[i], points[j] = points[j], points[i]
	}
	
	return points
}

func generateCircularData(n int) []Point {
	rand.Seed(time.Now().UnixNano())
	points := make([]Point, n)
	
	for i := 0; i < n; i++ {
		angle := rand.Float64() * 2 * math.Pi
		radius := 20 + rand.Float64()*30
		
		points[i] = Point{
			X: 50 + radius*math.Cos(angle),
			Y: 50 + radius*math.Sin(angle),
		}
	}
	
	return points
}

func findOptimalK(points []Point, maxK int) int {
	inertias := make([]float64, maxK-1)
	
	fmt.Println("Finding optimal K using elbow method...")
	
	for k := 2; k <= maxK; k++ {
		kmeans := NewKMeans(k, 100)
		err := kmeans.Fit(points)
		if err != nil {
			fmt.Printf("Error fitting K=%d: %v\n", k, err)
			continue
		}
		
		inertias[k-2] = kmeans.calculateInertia()
		fmt.Printf("K=%d, Inertia=%.4f\n", k, inertias[k-2])
	}
	
	// Simple elbow detection (find maximum decrease in slope)
	optimalK := 2
	maxDecrease := 0.0
	
	for i := 1; i < len(inertias)-1; i++ {
		if inertias[i-1] == 0 || inertias[i+1] == 0 {
			continue
		}
		
		slope1 := inertias[i-1] - inertias[i]
		slope2 := inertias[i] - inertias[i+1]
		decrease := slope1 - slope2
		
		if decrease > maxDecrease {
			maxDecrease = decrease
			optimalK = i + 2
		}
	}
	
	fmt.Printf("Optimal K detected: %d\n", optimalK)
	return optimalK
}

func benchmarkKMeans(points []Point, k int, iterations int) {
	fmt.Printf("\nBenchmarking K-means (K=%d, %d runs)...\n", k, iterations)
	
	totalTime := time.Duration(0)
	var totalInertia float64
	
	for i := 0; i < iterations; i++ {
		start := time.Now()
		
		kmeans := NewKMeans(k, 100)
		err := kmeans.Fit(points)
		if err != nil {
			fmt.Printf("Error in run %d: %v\n", i+1, err)
			continue
		}
		
		duration := time.Since(start)
		totalTime += duration
		totalInertia += kmeans.calculateInertia()
		
		if i%10 == 9 {
			fmt.Printf("Completed %d/%d runs\n", i+1, iterations)
		}
	}
	
	avgTime := totalTime / time.Duration(iterations)
	avgInertia := totalInertia / float64(iterations)
	
	fmt.Printf("Average time per run: %v\n", avgTime)
	fmt.Printf("Average inertia: %.4f\n", avgInertia)
}

func main() {
	fmt.Println("Advanced K-Means Clustering Algorithm")
	fmt.Println("====================================")
	
	// Test 1: Random clustered data
	fmt.Println("\nTest 1: Random clustered data")
	points1 := generateRandomPoints(500, 4)
	
	kmeans1 := NewKMeans(4, 100)
	start := time.Now()
	err := kmeans1.Fit(points1)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}
	
	duration := time.Since(start)
	fmt.Printf("Clustering completed in %v\n", duration)
	
	silhouette1 := kmeans1.calculateSilhouetteScore(points1)
	fmt.Printf("Silhouette Score: %.4f\n", silhouette1)
	
	kmeans1.PrintResults()
	
	// Test 2: Circular data
	fmt.Println("\nTest 2: Circular data")
	points2 := generateCircularData(300)
	
	optimalK := findOptimalK(points2, 8)
	
	kmeans2 := NewKMeans(optimalK, 100)
	start = time.Now()
	err = kmeans2.Fit(points2)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}
	
	duration = time.Since(start)
	fmt.Printf("Circular data clustering completed in %v\n", duration)
	
	silhouette2 := kmeans2.calculateSilhouetteScore(points2)
	fmt.Printf("Silhouette Score: %.4f\n", silhouette2)
	
	// Test 3: Performance benchmark
	fmt.Println("\nTest 3: Performance benchmark")
	largeDataset := generateRandomPoints(2000, 6)
	benchmarkKMeans(largeDataset, 6, 50)
	
	fmt.Println("\nAll clustering tests completed successfully!")
}