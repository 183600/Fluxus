#!/usr/bin/env python3
"""
Advanced Machine Learning and Data Science Suite
Implements various ML algorithms and data analysis tools from scratch
"""

import math
import random
import statistics
from typing import List, Tuple, Dict, Any, Optional
from collections import defaultdict, Counter
import json


class Matrix:
    """A simple matrix implementation with basic operations"""
    
    def __init__(self, data: List[List[float]]):
        self.data = data
        self.rows = len(data)
        self.cols = len(data[0]) if data else 0
    
    def __str__(self) -> str:
        return '\n'.join([str(row) for row in self.data])
    
    def __mul__(self, other: 'Matrix') -> 'Matrix':
        if self.cols != other.rows:
            raise ValueError("Matrix dimensions don't match for multiplication")
        
        result = [[0 for _ in range(other.cols)] for _ in range(self.rows)]
        
        for i in range(self.rows):
            for j in range(other.cols):
                for k in range(self.cols):
                    result[i][j] += self.data[i][k] * other.data[k][j]
        
        return Matrix(result)
    
    def transpose(self) -> 'Matrix':
        result = [[self.data[i][j] for i in range(self.rows)] for j in range(self.cols)]
        return Matrix(result)
    
    @classmethod
    def identity(cls, size: int) -> 'Matrix':
        data = [[1 if i == j else 0 for j in range(size)] for i in range(size)]
        return cls(data)


class LinearRegression:
    """Simple linear regression implementation"""
    
    def __init__(self):
        self.slope = 0
        self.intercept = 0
        self.trained = False
    
    def fit(self, X: List[float], y: List[float]):
        if len(X) != len(y):
            raise ValueError("X and y must have the same length")
        
        n = len(X)
        sum_x = sum(X)
        sum_y = sum(y)
        sum_xy = sum(x * y for x, y in zip(X, y))
        sum_x2 = sum(x * x for x in X)
        
        # Calculate slope and intercept using least squares
        self.slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
        self.intercept = (sum_y - self.slope * sum_x) / n
        self.trained = True
    
    def predict(self, X: List[float]) -> List[float]:
        if not self.trained:
            raise ValueError("Model must be trained before prediction")
        return [self.slope * x + self.intercept for x in X]
    
    def score(self, X: List[float], y: List[float]) -> float:
        predictions = self.predict(X)
        y_mean = statistics.mean(y)
        
        ss_res = sum((y_true - y_pred) ** 2 for y_true, y_pred in zip(y, predictions))
        ss_tot = sum((y_true - y_mean) ** 2 for y_true in y)
        
        return 1 - (ss_res / ss_tot) if ss_tot != 0 else 0


class KMeans:
    """K-Means clustering implementation"""
    
    def __init__(self, k: int, max_iters: int = 100):
        self.k = k
        self.max_iters = max_iters
        self.centroids = []
        self.labels = []
    
    def _distance(self, p1: List[float], p2: List[float]) -> float:
        return math.sqrt(sum((a - b) ** 2 for a, b in zip(p1, p2)))
    
    def fit(self, X: List[List[float]]):
        n_samples, n_features = len(X), len(X[0])
        
        # Initialize centroids randomly
        self.centroids = []
        for _ in range(self.k):
            centroid = [random.uniform(
                min(point[i] for point in X),
                max(point[i] for point in X)
            ) for i in range(n_features)]
            self.centroids.append(centroid)
        
        for iteration in range(self.max_iters):
            # Assign points to closest centroid
            clusters = [[] for _ in range(self.k)]
            self.labels = []
            
            for point in X:
                distances = [self._distance(point, centroid) for centroid in self.centroids]
                closest_centroid = distances.index(min(distances))
                clusters[closest_centroid].append(point)
                self.labels.append(closest_centroid)
            
            # Update centroids
            new_centroids = []
            for cluster in clusters:
                if cluster:
                    centroid = [statistics.mean(point[i] for point in cluster) 
                              for i in range(n_features)]
                    new_centroids.append(centroid)
                else:
                    # Keep old centroid if cluster is empty
                    new_centroids.append(self.centroids[len(new_centroids)])
            
            # Check for convergence
            if self._centroids_converged(self.centroids, new_centroids):
                break
            
            self.centroids = new_centroids
    
    def _centroids_converged(self, old: List[List[float]], new: List[List[float]], tolerance: float = 1e-6) -> bool:
        for old_c, new_c in zip(old, new):
            if self._distance(old_c, new_c) > tolerance:
                return False
        return True
    
    def predict(self, X: List[List[float]]) -> List[int]:
        labels = []
        for point in X:
            distances = [self._distance(point, centroid) for centroid in self.centroids]
            labels.append(distances.index(min(distances)))
        return labels


class DecisionTree:
    """Simple decision tree for classification"""
    
    def __init__(self, max_depth: int = 10, min_samples_split: int = 2):
        self.max_depth = max_depth
        self.min_samples_split = min_samples_split
        self.tree = None
    
    def _gini_impurity(self, y: List[Any]) -> float:
        if not y:
            return 0
        
        counts = Counter(y)
        n = len(y)
        impurity = 1.0
        
        for count in counts.values():
            prob = count / n
            impurity -= prob ** 2
        
        return impurity
    
    def _split_data(self, X: List[List[float]], y: List[Any], feature_idx: int, threshold: float) -> Tuple:
        left_X, left_y, right_X, right_y = [], [], [], []
        
        for i, sample in enumerate(X):
            if sample[feature_idx] <= threshold:
                left_X.append(sample)
                left_y.append(y[i])
            else:
                right_X.append(sample)
                right_y.append(y[i])
        
        return left_X, left_y, right_X, right_y
    
    def _find_best_split(self, X: List[List[float]], y: List[Any]) -> Tuple[int, float]:
        best_feature, best_threshold = None, None
        best_gini = float('inf')
        
        n_features = len(X[0])
        
        for feature_idx in range(n_features):
            thresholds = sorted(set(sample[feature_idx] for sample in X))
            
            for i in range(len(thresholds) - 1):
                threshold = (thresholds[i] + thresholds[i + 1]) / 2
                
                left_X, left_y, right_X, right_y = self._split_data(X, y, feature_idx, threshold)
                
                if len(left_y) == 0 or len(right_y) == 0:
                    continue
                
                # Calculate weighted gini impurity
                n = len(y)
                left_gini = self._gini_impurity(left_y)
                right_gini = self._gini_impurity(right_y)
                weighted_gini = (len(left_y) / n) * left_gini + (len(right_y) / n) * right_gini
                
                if weighted_gini < best_gini:
                    best_gini = weighted_gini
                    best_feature = feature_idx
                    best_threshold = threshold
        
        return best_feature, best_threshold
    
    def _build_tree(self, X: List[List[float]], y: List[Any], depth: int = 0) -> Dict:
        # Base cases
        if depth >= self.max_depth or len(set(y)) == 1 or len(y) < self.min_samples_split:
            return {'class': Counter(y).most_common(1)[0][0]}
        
        feature_idx, threshold = self._find_best_split(X, y)
        
        if feature_idx is None:
            return {'class': Counter(y).most_common(1)[0][0]}
        
        left_X, left_y, right_X, right_y = self._split_data(X, y, feature_idx, threshold)
        
        return {
            'feature': feature_idx,
            'threshold': threshold,
            'left': self._build_tree(left_X, left_y, depth + 1),
            'right': self._build_tree(right_X, right_y, depth + 1)
        }
    
    def fit(self, X: List[List[float]], y: List[Any]):
        self.tree = self._build_tree(X, y)
    
    def _predict_sample(self, sample: List[float], tree: Dict) -> Any:
        if 'class' in tree:
            return tree['class']
        
        if sample[tree['feature']] <= tree['threshold']:
            return self._predict_sample(sample, tree['left'])
        else:
            return self._predict_sample(sample, tree['right'])
    
    def predict(self, X: List[List[float]]) -> List[Any]:
        return [self._predict_sample(sample, self.tree) for sample in X]


class DataAnalyzer:
    """Comprehensive data analysis toolkit"""
    
    @staticmethod
    def describe(data: List[float]) -> Dict[str, float]:
        """Generate descriptive statistics"""
        sorted_data = sorted(data)
        n = len(data)
        
        return {
            'count': n,
            'mean': statistics.mean(data),
            'median': statistics.median(data),
            'mode': statistics.mode(data) if len(set(data)) < len(data) else None,
            'std': statistics.stdev(data) if n > 1 else 0,
            'var': statistics.variance(data) if n > 1 else 0,
            'min': min(data),
            'max': max(data),
            'q1': sorted_data[n // 4],
            'q3': sorted_data[3 * n // 4],
            'range': max(data) - min(data),
            'skewness': DataAnalyzer._skewness(data),
            'kurtosis': DataAnalyzer._kurtosis(data)
        }
    
    @staticmethod
    def _skewness(data: List[float]) -> float:
        mean = statistics.mean(data)
        std = statistics.stdev(data)
        n = len(data)
        
        if std == 0:
            return 0
        
        skewness = sum(((x - mean) / std) ** 3 for x in data) / n
        return skewness
    
    @staticmethod
    def _kurtosis(data: List[float]) -> float:
        mean = statistics.mean(data)
        std = statistics.stdev(data)
        n = len(data)
        
        if std == 0:
            return 0
        
        kurtosis = sum(((x - mean) / std) ** 4 for x in data) / n - 3
        return kurtosis
    
    @staticmethod
    def correlation(x: List[float], y: List[float]) -> float:
        """Calculate Pearson correlation coefficient"""
        if len(x) != len(y):
            raise ValueError("Lists must have the same length")
        
        n = len(x)
        sum_x = sum(x)
        sum_y = sum(y)
        sum_xy = sum(a * b for a, b in zip(x, y))
        sum_x2 = sum(a * a for a in x)
        sum_y2 = sum(b * b for b in y)
        
        numerator = n * sum_xy - sum_x * sum_y
        denominator = math.sqrt((n * sum_x2 - sum_x ** 2) * (n * sum_y2 - sum_y ** 2))
        
        return numerator / denominator if denominator != 0 else 0
    
    @staticmethod
    def outliers_iqr(data: List[float]) -> List[float]:
        """Detect outliers using IQR method"""
        sorted_data = sorted(data)
        n = len(sorted_data)
        
        q1 = sorted_data[n // 4]
        q3 = sorted_data[3 * n // 4]
        iqr = q3 - q1
        
        lower_bound = q1 - 1.5 * iqr
        upper_bound = q3 + 1.5 * iqr
        
        return [x for x in data if x < lower_bound or x > upper_bound]


def generate_sample_data():
    """Generate sample datasets for testing"""
    random.seed(42)
    
    # Linear regression data
    X_reg = [i + random.gauss(0, 0.1) for i in range(100)]
    y_reg = [2 * x + 1 + random.gauss(0, 0.5) for x in X_reg]
    
    # Clustering data (2 clusters)
    cluster1 = [[random.gauss(2, 0.5), random.gauss(2, 0.5)] for _ in range(50)]
    cluster2 = [[random.gauss(6, 0.5), random.gauss(6, 0.5)] for _ in range(50)]
    X_cluster = cluster1 + cluster2
    
    # Classification data
    X_class = []
    y_class = []
    for _ in range(100):
        x1, x2 = random.uniform(0, 10), random.uniform(0, 10)
        X_class.append([x1, x2])
        # Simple decision boundary: x1 + x2 > 10
        y_class.append('A' if x1 + x2 > 10 else 'B')
    
    return X_reg, y_reg, X_cluster, X_class, y_class


def main():
    print("=== Advanced Machine Learning and Data Science Suite ===\n")
    
    # Generate sample data
    X_reg, y_reg, X_cluster, X_class, y_class = generate_sample_data()
    
    # 1. Linear Regression
    print("1. Linear Regression:")
    lr = LinearRegression()
    lr.fit(X_reg, y_reg)
    print(f"   Slope: {lr.slope:.4f}")
    print(f"   Intercept: {lr.intercept:.4f}")
    print(f"   R² Score: {lr.score(X_reg, y_reg):.4f}")
    
    # 2. K-Means Clustering
    print("\n2. K-Means Clustering:")
    kmeans = KMeans(k=2)
    kmeans.fit(X_cluster)
    print(f"   Centroids: {[[round(x, 2) for x in centroid] for centroid in kmeans.centroids]}")
    print(f"   Cluster distribution: {Counter(kmeans.labels)}")
    
    # 3. Decision Tree
    print("\n3. Decision Tree Classification:")
    dt = DecisionTree(max_depth=5)
    dt.fit(X_class, y_class)
    predictions = dt.predict(X_class)
    accuracy = sum(1 for true, pred in zip(y_class, predictions) if true == pred) / len(y_class)
    print(f"   Accuracy: {accuracy:.4f}")
    print(f"   Class distribution: {Counter(y_class)}")
    
    # 4. Data Analysis
    print("\n4. Data Analysis:")
    sample_data = [random.gauss(50, 15) for _ in range(1000)]
    stats = DataAnalyzer.describe(sample_data)
    
    print("   Descriptive Statistics:")
    for key, value in stats.items():
        if value is not None:
            if isinstance(value, float):
                print(f"     {key}: {value:.4f}")
            else:
                print(f"     {key}: {value}")
    
    # 5. Correlation Analysis
    print("\n5. Correlation Analysis:")
    data1 = [random.gauss(0, 1) for _ in range(100)]
    data2 = [x + random.gauss(0, 0.5) for x in data1]  # Correlated data
    data3 = [random.gauss(0, 1) for _ in range(100)]   # Uncorrelated data
    
    corr1 = DataAnalyzer.correlation(data1, data2)
    corr2 = DataAnalyzer.correlation(data1, data3)
    
    print(f"   Correlation (data1, data2): {corr1:.4f}")
    print(f"   Correlation (data1, data3): {corr2:.4f}")
    
    # 6. Outlier Detection
    print("\n6. Outlier Detection:")
    normal_data = [random.gauss(50, 10) for _ in range(95)]
    outliers_data = normal_data + [100, 120, 0, -10, 150]  # Add outliers
    
    detected_outliers = DataAnalyzer.outliers_iqr(outliers_data)
    print(f"   Total data points: {len(outliers_data)}")
    print(f"   Detected outliers: {len(detected_outliers)}")
    print(f"   Outlier values: {sorted(detected_outliers)}")
    
    # 7. Matrix Operations
    print("\n7. Matrix Operations:")
    matrix1 = Matrix([[1, 2], [3, 4]])
    matrix2 = Matrix([[5, 6], [7, 8]])
    
    print("   Matrix 1:")
    print(f"   {matrix1}")
    print("   Matrix 2:")
    print(f"   {matrix2}")
    print("   Matrix 1 × Matrix 2:")
    result = matrix1 * matrix2
    print(f"   {result}")
    
    print("\nAnalysis completed successfully!")


if __name__ == "__main__":
    main()