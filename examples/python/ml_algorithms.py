#!/usr/bin/env python3
"""
Machine Learning and Data Science Demo
Advanced algorithms, data processing, and statistical analysis
"""

import math
import random
import statistics
from collections import defaultdict, Counter
from dataclasses import dataclass
from typing import List, Tuple, Dict, Any, Optional
import json
import time


@dataclass
class DataPoint:
    features: List[float]
    label: Optional[str] = None
    target: Optional[float] = None


class LinearRegression:
    def __init__(self):
        self.weights: List[float] = []
        self.bias: float = 0.0
        self.learning_rate: float = 0.01
        self.trained: bool = False
    
    def fit(self, X: List[List[float]], y: List[float], epochs: int = 1000):
        """Train the linear regression model"""
        n_samples, n_features = len(X), len(X[0])
        
        # Initialize weights and bias
        self.weights = [random.uniform(-1, 1) for _ in range(n_features)]
        self.bias = random.uniform(-1, 1)
        
        # Gradient descent
        for epoch in range(epochs):
            # Forward pass
            predictions = []
            for features in X:
                pred = sum(w * x for w, x in zip(self.weights, features)) + self.bias
                predictions.append(pred)
            
            # Calculate gradients
            dw = [0.0] * n_features
            db = 0.0
            
            for i in range(n_samples):
                error = predictions[i] - y[i]
                db += error
                for j in range(n_features):
                    dw[j] += error * X[i][j]
            
            # Update parameters
            self.bias -= self.learning_rate * db / n_samples
            for j in range(n_features):
                self.weights[j] -= self.learning_rate * dw[j] / n_samples
        
        self.trained = True
    
    def predict(self, X: List[List[float]]) -> List[float]:
        """Make predictions"""
        if not self.trained:
            raise ValueError("Model must be trained before making predictions")
        
        predictions = []
        for features in X:
            pred = sum(w * x for w, x in zip(self.weights, features)) + self.bias
            predictions.append(pred)
        
        return predictions
    
    def score(self, X: List[List[float]], y: List[float]) -> float:
        """Calculate R-squared score"""
        predictions = self.predict(X)
        
        # Calculate R-squared
        y_mean = statistics.mean(y)
        ss_tot = sum((yi - y_mean) ** 2 for yi in y)
        ss_res = sum((yi - pi) ** 2 for yi, pi in zip(y, predictions))
        
        return 1 - (ss_res / ss_tot) if ss_tot != 0 else 0


class KMeans:
    def __init__(self, k: int = 3, max_iters: int = 100):
        self.k = k
        self.max_iters = max_iters
        self.centroids: List[List[float]] = []
        self.labels: List[int] = []
        self.trained: bool = False
    
    def fit(self, X: List[List[float]]):
        """Train the K-means clustering model"""
        n_samples, n_features = len(X), len(X[0])
        
        # Initialize centroids randomly
        self.centroids = []
        for _ in range(self.k):
            centroid = [random.uniform(
                min(X[i][j] for i in range(n_samples)),
                max(X[i][j] for i in range(n_samples))
            ) for j in range(n_features)]
            self.centroids.append(centroid)
        
        for iteration in range(self.max_iters):
            # Assign points to nearest centroid
            self.labels = []
            for point in X:
                distances = []
                for centroid in self.centroids:
                    dist = sum((p - c) ** 2 for p, c in zip(point, centroid)) ** 0.5
                    distances.append(dist)
                self.labels.append(distances.index(min(distances)))
            
            # Update centroids
            new_centroids = []
            for k in range(self.k):
                cluster_points = [X[i] for i in range(n_samples) if self.labels[i] == k]
                if cluster_points:
                    new_centroid = [
                        statistics.mean(point[j] for point in cluster_points)
                        for j in range(n_features)
                    ]
                    new_centroids.append(new_centroid)
                else:
                    new_centroids.append(self.centroids[k])
            
            # Check for convergence
            if self._centroids_equal(self.centroids, new_centroids):
                break
            
            self.centroids = new_centroids
        
        self.trained = True
    
    def _centroids_equal(self, old: List[List[float]], new: List[List[float]], tolerance: float = 1e-6) -> bool:
        """Check if centroids have converged"""
        for old_centroid, new_centroid in zip(old, new):
            for old_val, new_val in zip(old_centroid, new_centroid):
                if abs(old_val - new_val) > tolerance:
                    return False
        return True
    
    def predict(self, X: List[List[float]]) -> List[int]:
        """Predict cluster labels for new data"""
        if not self.trained:
            raise ValueError("Model must be trained before making predictions")
        
        labels = []
        for point in X:
            distances = []
            for centroid in self.centroids:
                dist = sum((p - c) ** 2 for p, c in zip(point, centroid)) ** 0.5
                distances.append(dist)
            labels.append(distances.index(min(distances)))
        
        return labels


class DecisionTree:
    def __init__(self, max_depth: int = 10, min_samples_split: int = 2):
        self.max_depth = max_depth
        self.min_samples_split = min_samples_split
        self.tree = None
        self.trained = False
    
    def fit(self, X: List[List[float]], y: List[str]):
        """Train the decision tree"""
        self.tree = self._build_tree(X, y, depth=0)
        self.trained = True
    
    def _build_tree(self, X: List[List[float]], y: List[str], depth: int):
        """Recursively build the decision tree"""
        n_samples, n_features = len(X), len(X[0]) if X else 0
        
        # Stopping criteria
        if (depth >= self.max_depth or 
            n_samples < self.min_samples_split or 
            len(set(y)) == 1):
            return self._most_common_label(y)
        
        # Find the best split
        best_gini = float('inf')
        best_feature = None
        best_threshold = None
        
        for feature_idx in range(n_features):
            feature_values = [X[i][feature_idx] for i in range(n_samples)]
            thresholds = sorted(set(feature_values))
            
            for threshold in thresholds:
                left_indices = [i for i in range(n_samples) if X[i][feature_idx] <= threshold]
                right_indices = [i for i in range(n_samples) if X[i][feature_idx] > threshold]
                
                if len(left_indices) == 0 or len(right_indices) == 0:
                    continue
                
                left_labels = [y[i] for i in left_indices]
                right_labels = [y[i] for i in right_indices]
                
                gini = self._weighted_gini(left_labels, right_labels)
                
                if gini < best_gini:
                    best_gini = gini
                    best_feature = feature_idx
                    best_threshold = threshold
        
        if best_feature is None:
            return self._most_common_label(y)
        
        # Create splits
        left_indices = [i for i in range(n_samples) if X[i][best_feature] <= best_threshold]
        right_indices = [i for i in range(n_samples) if X[i][best_feature] > best_threshold]
        
        left_X = [X[i] for i in left_indices]
        left_y = [y[i] for i in left_indices]
        right_X = [X[i] for i in right_indices]
        right_y = [y[i] for i in right_indices]
        
        return {
            'feature': best_feature,
            'threshold': best_threshold,
            'left': self._build_tree(left_X, left_y, depth + 1),
            'right': self._build_tree(right_X, right_y, depth + 1)
        }
    
    def _gini_impurity(self, labels: List[str]) -> float:
        """Calculate Gini impurity"""
        if not labels:
            return 0
        
        label_counts = Counter(labels)
        total = len(labels)
        gini = 1.0
        
        for count in label_counts.values():
            prob = count / total
            gini -= prob ** 2
        
        return gini
    
    def _weighted_gini(self, left_labels: List[str], right_labels: List[str]) -> float:
        """Calculate weighted Gini impurity for a split"""
        total_samples = len(left_labels) + len(right_labels)
        left_weight = len(left_labels) / total_samples
        right_weight = len(right_labels) / total_samples
        
        return (left_weight * self._gini_impurity(left_labels) + 
                right_weight * self._gini_impurity(right_labels))
    
    def _most_common_label(self, labels: List[str]) -> str:
        """Return the most common label"""
        return Counter(labels).most_common(1)[0][0] if labels else "unknown"
    
    def predict(self, X: List[List[float]]) -> List[str]:
        """Make predictions"""
        if not self.trained:
            raise ValueError("Model must be trained before making predictions")
        
        return [self._predict_single(sample, self.tree) for sample in X]
    
    def _predict_single(self, sample: List[float], node):
        """Predict a single sample"""
        if isinstance(node, str):  # Leaf node
            return node
        
        if sample[node['feature']] <= node['threshold']:
            return self._predict_single(sample, node['left'])
        else:
            return self._predict_single(sample, node['right'])


class NaiveBayes:
    def __init__(self):
        self.class_probs: Dict[str, float] = {}
        self.feature_probs: Dict[str, Dict[int, Dict[str, float]]] = {}
        self.trained = False
    
    def fit(self, X: List[List[str]], y: List[str]):
        """Train the Naive Bayes classifier"""
        n_samples = len(X)
        
        # Calculate class probabilities
        class_counts = Counter(y)
        self.class_probs = {cls: count / n_samples for cls, count in class_counts.items()}
        
        # Calculate feature probabilities for each class
        self.feature_probs = {}
        for cls in class_counts.keys():
            self.feature_probs[cls] = {}
            class_samples = [X[i] for i in range(n_samples) if y[i] == cls]
            
            for feature_idx in range(len(X[0])):
                feature_values = [sample[feature_idx] for sample in class_samples]
                value_counts = Counter(feature_values)
                total_values = len(feature_values)
                
                self.feature_probs[cls][feature_idx] = {
                    value: count / total_values for value, count in value_counts.items()
                }
        
        self.trained = True
    
    def predict(self, X: List[List[str]]) -> List[str]:
        """Make predictions"""
        if not self.trained:
            raise ValueError("Model must be trained before making predictions")
        
        predictions = []
        for sample in X:
            class_scores = {}
            
            for cls in self.class_probs.keys():
                score = math.log(self.class_probs[cls])
                
                for feature_idx, feature_value in enumerate(sample):
                    if (feature_idx in self.feature_probs[cls] and 
                        feature_value in self.feature_probs[cls][feature_idx]):
                        prob = self.feature_probs[cls][feature_idx][feature_value]
                    else:
                        prob = 1e-6  # Smoothing for unseen features
                    
                    score += math.log(prob)
                
                class_scores[cls] = score
            
            predicted_class = max(class_scores.keys(), key=class_scores.get)
            predictions.append(predicted_class)
        
        return predictions


class StatisticalAnalyzer:
    @staticmethod
    def correlation_coefficient(x: List[float], y: List[float]) -> float:
        """Calculate Pearson correlation coefficient"""
        if len(x) != len(y) or len(x) == 0:
            return 0.0
        
        n = len(x)
        sum_x = sum(x)
        sum_y = sum(y)
        sum_xy = sum(xi * yi for xi, yi in zip(x, y))
        sum_x2 = sum(xi ** 2 for xi in x)
        sum_y2 = sum(yi ** 2 for yi in y)
        
        numerator = n * sum_xy - sum_x * sum_y
        denominator = math.sqrt((n * sum_x2 - sum_x ** 2) * (n * sum_y2 - sum_y ** 2))
        
        return numerator / denominator if denominator != 0 else 0.0
    
    @staticmethod
    def t_test(sample1: List[float], sample2: List[float]) -> Tuple[float, float]:
        """Perform independent t-test"""
        n1, n2 = len(sample1), len(sample2)
        
        if n1 <= 1 or n2 <= 1:
            return 0.0, 1.0
        
        mean1 = statistics.mean(sample1)
        mean2 = statistics.mean(sample2)
        var1 = statistics.variance(sample1)
        var2 = statistics.variance(sample2)
        
        # Pooled standard error
        pooled_se = math.sqrt(var1 / n1 + var2 / n2)
        
        if pooled_se == 0:
            return 0.0, 1.0
        
        t_statistic = (mean1 - mean2) / pooled_se
        degrees_freedom = n1 + n2 - 2
        
        # Simplified p-value calculation (approximation)
        p_value = 2 * (1 - abs(t_statistic) / (abs(t_statistic) + degrees_freedom))
        
        return t_statistic, p_value
    
    @staticmethod
    def chi_square_test(observed: List[List[int]]) -> Tuple[float, float]:
        """Perform chi-square test of independence"""
        rows, cols = len(observed), len(observed[0])
        
        # Calculate row and column totals
        row_totals = [sum(row) for row in observed]
        col_totals = [sum(observed[i][j] for i in range(rows)) for j in range(cols)]
        total = sum(row_totals)
        
        if total == 0:
            return 0.0, 1.0
        
        # Calculate expected frequencies
        expected = []
        for i in range(rows):
            expected_row = []
            for j in range(cols):
                expected_freq = (row_totals[i] * col_totals[j]) / total
                expected_row.append(expected_freq)
            expected.append(expected_row)
        
        # Calculate chi-square statistic
        chi_square = 0.0
        for i in range(rows):
            for j in range(cols):
                if expected[i][j] > 0:
                    chi_square += ((observed[i][j] - expected[i][j]) ** 2) / expected[i][j]
        
        # Degrees of freedom
        df = (rows - 1) * (cols - 1)
        
        # Simplified p-value calculation (approximation)
        p_value = 1 / (1 + chi_square / (2 * df)) if df > 0 else 1.0
        
        return chi_square, p_value


def generate_regression_data(n_samples: int = 100) -> Tuple[List[List[float]], List[float]]:
    """Generate synthetic data for regression"""
    X = []
    y = []
    
    for _ in range(n_samples):
        x1 = random.uniform(-10, 10)
        x2 = random.uniform(-5, 5)
        
        # y = 2*x1 + 3*x2 + noise
        noise = random.gauss(0, 1)
        target = 2 * x1 + 3 * x2 + noise
        
        X.append([x1, x2])
        y.append(target)
    
    return X, y


def generate_classification_data(n_samples: int = 200) -> Tuple[List[List[float]], List[str]]:
    """Generate synthetic data for classification"""
    X = []
    y = []
    
    for _ in range(n_samples):
        x1 = random.uniform(-5, 5)
        x2 = random.uniform(-5, 5)
        
        # Simple decision boundary: x1 + x2 > 0
        if x1 + x2 > 0:
            label = "A"
        else:
            label = "B"
        
        # Add some noise
        if random.random() < 0.1:
            label = "B" if label == "A" else "A"
        
        X.append([x1, x2])
        y.append(label)
    
    return X, y


def generate_clustering_data(n_samples: int = 150) -> List[List[float]]:
    """Generate synthetic data for clustering"""
    X = []
    
    # Generate 3 clusters
    centers = [[-2, -2], [2, 2], [0, 3]]
    
    for _ in range(n_samples):
        center = random.choice(centers)
        x1 = center[0] + random.gauss(0, 0.8)
        x2 = center[1] + random.gauss(0, 0.8)
        X.append([x1, x2])
    
    return X


def evaluate_classification(y_true: List[str], y_pred: List[str]) -> Dict[str, float]:
    """Evaluate classification performance"""
    if len(y_true) != len(y_pred):
        return {"accuracy": 0.0, "precision": 0.0, "recall": 0.0, "f1": 0.0}
    
    # Calculate confusion matrix elements
    tp = fp = tn = fn = 0
    
    # For binary classification (assuming first unique label is positive)
    unique_labels = list(set(y_true))
    positive_label = unique_labels[0] if unique_labels else None
    
    for true_label, pred_label in zip(y_true, y_pred):
        if true_label == positive_label and pred_label == positive_label:
            tp += 1
        elif true_label == positive_label and pred_label != positive_label:
            fn += 1
        elif true_label != positive_label and pred_label == positive_label:
            fp += 1
        else:
            tn += 1
    
    # Calculate metrics
    accuracy = (tp + tn) / len(y_true) if len(y_true) > 0 else 0.0
    precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0
    recall = tp / (tp + fn) if (tp + fn) > 0 else 0.0
    f1 = 2 * (precision * recall) / (precision + recall) if (precision + recall) > 0 else 0.0
    
    return {
        "accuracy": accuracy,
        "precision": precision,
        "recall": recall,
        "f1": f1
    }


def main():
    print("=== Machine Learning and Data Science Demo ===")
    
    # 1. Linear Regression
    print("\n1. Linear Regression:")
    X_reg, y_reg = generate_regression_data(100)
    
    # Split data (80-20)
    split_idx = int(0.8 * len(X_reg))
    X_train, X_test = X_reg[:split_idx], X_reg[split_idx:]
    y_train, y_test = y_reg[:split_idx], y_reg[split_idx:]
    
    lr_model = LinearRegression()
    lr_model.fit(X_train, y_train, epochs=1000)
    
    y_pred = lr_model.predict(X_test)
    r2_score = lr_model.score(X_test, y_test)
    
    print(f"R-squared score: {r2_score:.4f}")
    print(f"Weights: {[f'{w:.4f}' for w in lr_model.weights]}")
    print(f"Bias: {lr_model.bias:.4f}")
    
    # Calculate RMSE
    rmse = math.sqrt(statistics.mean((true - pred) ** 2 for true, pred in zip(y_test, y_pred)))
    print(f"RMSE: {rmse:.4f}")
    
    # 2. K-Means Clustering
    print("\n2. K-Means Clustering:")
    X_cluster = generate_clustering_data(150)
    
    kmeans = KMeans(k=3, max_iters=100)
    kmeans.fit(X_cluster)
    
    cluster_labels = kmeans.predict(X_cluster)
    cluster_counts = Counter(cluster_labels)
    
    print(f"Cluster assignments: {dict(cluster_counts)}")
    print("Cluster centers:")
    for i, center in enumerate(kmeans.centroids):
        print(f"  Cluster {i}: ({center[0]:.2f}, {center[1]:.2f})")
    
    # 3. Decision Tree Classification
    print("\n3. Decision Tree Classification:")
    X_class, y_class = generate_classification_data(200)
    
    # Split data
    split_idx = int(0.8 * len(X_class))
    X_train_dt, X_test_dt = X_class[:split_idx], X_class[split_idx:]
    y_train_dt, y_test_dt = y_class[:split_idx], y_class[split_idx:]
    
    dt_model = DecisionTree(max_depth=5, min_samples_split=5)
    dt_model.fit(X_train_dt, y_train_dt)
    
    y_pred_dt = dt_model.predict(X_test_dt)
    dt_metrics = evaluate_classification(y_test_dt, y_pred_dt)
    
    print(f"Decision Tree Performance:")
    for metric, value in dt_metrics.items():
        print(f"  {metric.capitalize()}: {value:.4f}")
    
    # 4. Naive Bayes Classification
    print("\n4. Naive Bayes Classification:")
    
    # Convert numerical features to categorical for Naive Bayes
    def discretize_features(X):
        X_discrete = []
        for sample in X:
            discrete_sample = []
            for feature in sample:
                if feature < -1:
                    discrete_sample.append("low")
                elif feature > 1:
                    discrete_sample.append("high")
                else:
                    discrete_sample.append("medium")
            X_discrete.append(discrete_sample)
        return X_discrete
    
    X_train_nb = discretize_features(X_train_dt)
    X_test_nb = discretize_features(X_test_dt)
    
    nb_model = NaiveBayes()
    nb_model.fit(X_train_nb, y_train_dt)
    
    y_pred_nb = nb_model.predict(X_test_nb)
    nb_metrics = evaluate_classification(y_test_dt, y_pred_nb)
    
    print(f"Naive Bayes Performance:")
    for metric, value in nb_metrics.items():
        print(f"  {metric.capitalize()}: {value:.4f}")
    
    # 5. Statistical Analysis
    print("\n5. Statistical Analysis:")
    
    # Generate two samples for comparison
    sample1 = [random.gauss(10, 2) for _ in range(50)]
    sample2 = [random.gauss(12, 2) for _ in range(50)]
    
    # Correlation analysis
    features1 = [random.gauss(0, 1) for _ in range(100)]
    features2 = [f + random.gauss(0, 0.5) for f in features1]  # Correlated
    
    correlation = StatisticalAnalyzer.correlation_coefficient(features1, features2)
    print(f"Correlation coefficient: {correlation:.4f}")
    
    # T-test
    t_stat, p_value = StatisticalAnalyzer.t_test(sample1, sample2)
    print(f"T-test: t-statistic = {t_stat:.4f}, p-value = {p_value:.4f}")
    
    # Chi-square test
    observed_data = [[10, 15, 5], [8, 12, 10], [12, 8, 15]]
    chi2_stat, chi2_p = StatisticalAnalyzer.chi_square_test(observed_data)
    print(f"Chi-square test: χ² = {chi2_stat:.4f}, p-value = {chi2_p:.4f}")
    
    # 6. Feature Analysis
    print("\n6. Feature Analysis:")
    
    # Analyze the clustering dataset
    features = list(zip(*X_cluster))
    feature1, feature2 = features[0], features[1]
    
    stats1 = {
        'mean': statistics.mean(feature1),
        'median': statistics.median(feature1),
        'std_dev': statistics.stdev(feature1),
        'min': min(feature1),
        'max': max(feature1)
    }
    
    stats2 = {
        'mean': statistics.mean(feature2),
        'median': statistics.median(feature2),
        'std_dev': statistics.stdev(feature2),
        'min': min(feature2),
        'max': max(feature2)
    }
    
    print("Feature 1 statistics:")
    for stat, value in stats1.items():
        print(f"  {stat}: {value:.4f}")
    
    print("Feature 2 statistics:")
    for stat, value in stats2.items():
        print(f"  {stat}: {value:.4f}")
    
    feature_correlation = StatisticalAnalyzer.correlation_coefficient(feature1, feature2)
    print(f"Feature correlation: {feature_correlation:.4f}")
    
    # 7. Model Comparison
    print("\n7. Model Comparison:")
    
    models_performance = {
        'Decision Tree': dt_metrics,
        'Naive Bayes': nb_metrics
    }
    
    print("Classification Model Comparison:")
    print(f"{'Model':<15} {'Accuracy':<10} {'Precision':<11} {'Recall':<8} {'F1-Score':<8}")
    print("-" * 55)
    
    for model_name, metrics in models_performance.items():
        print(f"{model_name:<15} {metrics['accuracy']:<10.4f} "
              f"{metrics['precision']:<11.4f} {metrics['recall']:<8.4f} "
              f"{metrics['f1']:<8.4f}")
    
    # 8. Cross-Validation Simulation
    print("\n8. Cross-Validation Simulation (3-fold):")
    
    # Simulate 3-fold cross-validation for Decision Tree
    fold_size = len(X_class) // 3
    cv_scores = []
    
    for fold in range(3):
        # Create fold splits
        start_idx = fold * fold_size
        end_idx = start_idx + fold_size
        
        X_val_fold = X_class[start_idx:end_idx]
        y_val_fold = y_class[start_idx:end_idx]
        
        X_train_fold = X_class[:start_idx] + X_class[end_idx:]
        y_train_fold = y_class[:start_idx] + y_class[end_idx:]
        
        # Train model on fold
        fold_model = DecisionTree(max_depth=5, min_samples_split=5)
        fold_model.fit(X_train_fold, y_train_fold)
        
        # Evaluate on validation set
        y_pred_fold = fold_model.predict(X_val_fold)
        fold_metrics = evaluate_classification(y_val_fold, y_pred_fold)
        cv_scores.append(fold_metrics['accuracy'])
        
        print(f"Fold {fold + 1} accuracy: {fold_metrics['accuracy']:.4f}")
    
    avg_cv_score = statistics.mean(cv_scores)
    std_cv_score = statistics.stdev(cv_scores) if len(cv_scores) > 1 else 0
    print(f"Average CV accuracy: {avg_cv_score:.4f} (+/- {std_cv_score:.4f})")
    
    # 9. Data Preprocessing Demo
    print("\n9. Data Preprocessing:")
    
    # Feature scaling (Min-Max normalization)
    def min_max_scale(data):
        features = list(zip(*data))
        scaled_features = []
        
        for feature in features:
            min_val, max_val = min(feature), max(feature)
            if max_val - min_val != 0:
                scaled = [(x - min_val) / (max_val - min_val) for x in feature]
            else:
                scaled = [0.0] * len(feature)
            scaled_features.append(scaled)
        
        return list(zip(*scaled_features))
    
    original_data = X_cluster[:10]  # Sample data
    scaled_data = min_max_scale(original_data)
    
    print("Original data (first 3 samples):")
    for i in range(3):
        print(f"  Sample {i+1}: {[f'{x:.3f}' for x in original_data[i]]}")
    
    print("Scaled data (first 3 samples):")
    for i in range(3):
        print(f"  Sample {i+1}: {[f'{x:.3f}' for x in scaled_data[i]]}")
    
    # 10. Performance Timing
    print("\n10. Algorithm Performance Timing:")
    
    # Time different algorithms
    algorithms = [
        ("Linear Regression", lambda: LinearRegression().fit(X_train[:50], y_train[:50], epochs=100)),
        ("K-Means", lambda: KMeans(k=3, max_iters=50).fit(X_cluster[:100])),
        ("Decision Tree", lambda: DecisionTree(max_depth=3).fit(X_train_dt[:100], y_train_dt[:100])),
        ("Naive Bayes", lambda: NaiveBayes().fit(X_train_nb[:100], y_train_dt[:100]))
    ]
    
    print(f"{'Algorithm':<20} {'Time (seconds)':<15}")
    print("-" * 35)
    
    for name, algorithm in algorithms:
        start_time = time.time()
        algorithm()
        end_time = time.time()
        execution_time = end_time - start_time
        print(f"{name:<20} {execution_time:<15.6f}")
    
    print("\n=== Machine Learning Demo Complete ===")


if __name__ == "__main__":
    main()