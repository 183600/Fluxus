import numpy as np
import pandas as pd
from sklearn.datasets import make_classification, make_regression
from sklearn.model_selection import train_test_split, cross_val_score, GridSearchCV
from sklearn.ensemble import RandomForestClassifier, GradientBoostingRegressor
from sklearn.linear_model import LogisticRegression, LinearRegression
from sklearn.svm import SVC, SVR
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score
from sklearn.metrics import mean_squared_error, r2_score, mean_absolute_error
from sklearn.preprocessing import StandardScaler, LabelEncoder
from sklearn.decomposition import PCA
from sklearn.cluster import KMeans, DBSCAN
import matplotlib.pyplot as plt
import seaborn as sns
from typing import Dict, List, Tuple, Any
import warnings
warnings.filterwarnings('ignore')

class AdvancedMLPipeline:
    """Advanced machine learning pipeline with comprehensive analysis"""
    
    def __init__(self, random_state: int = 42):
        self.random_state = random_state
        self.scaler = StandardScaler()
        self.pca = None
        self.models = {}
        self.results = {}
        
        # Set random seeds for reproducibility
        np.random.seed(random_state)
    
    def generate_classification_dataset(self, n_samples: int = 1000, n_features: int = 20, 
                                       n_classes: int = 3, n_informative: int = 15) -> Tuple[np.ndarray, np.ndarray]:
        """Generate synthetic classification dataset"""
        X, y = make_classification(
            n_samples=n_samples,
            n_features=n_features,
            n_classes=n_classes,
            n_informative=n_informative,
            n_redundant=n_features - n_informative,
            n_clusters_per_class=1,
            random_state=self.random_state
        )
        return X, y
    
    def generate_regression_dataset(self, n_samples: int = 1000, n_features: int = 20, 
                                   n_informative: int = 15, noise: float = 0.1) -> Tuple[np.ndarray, np.ndarray]:
        """Generate synthetic regression dataset"""
        X, y = make_regression(
            n_samples=n_samples,
            n_features=n_features,
            n_informative=n_informative,
            noise=noise,
            random_state=self.random_state
        )
        return X, y
    
    def preprocess_data(self, X: np.ndarray, apply_pca: bool = False, 
                       n_components: float = 0.95) -> np.ndarray:
        """Advanced data preprocessing with scaling and optional PCA"""
        # Standardize features
        X_scaled = self.scaler.fit_transform(X)
        
        if apply_pca:
            self.pca = PCA(n_components=n_components, random_state=self.random_state)
            X_scaled = self.pca.fit_transform(X_scaled)
            print(f"PCA applied: {X.shape[1]} -> {X_scaled.shape[1]} features")
            print(f"Explained variance ratio: {self.pca.explained_variance_ratio_.sum():.3f}")
        
        return X_scaled
    
    def setup_classification_models(self) -> Dict[str, Any]:
        """Setup various classification models"""
        models = {
            'Logistic Regression': LogisticRegression(
                random_state=self.random_state, max_iter=1000
            ),
            'Random Forest': RandomForestClassifier(
                n_estimators=100, random_state=self.random_state
            ),
            'SVM': SVC(
                random_state=self.random_state, probability=True
            ),
        }
        return models
    
    def setup_regression_models(self) -> Dict[str, Any]:
        """Setup various regression models"""
        models = {
            'Linear Regression': LinearRegression(),
            'Random Forest': RandomForestRegressor(
                n_estimators=100, random_state=self.random_state
            ),
            'Gradient Boosting': GradientBoostingRegressor(
                n_estimators=100, random_state=self.random_state
            ),
            'SVR': SVR(),
        }
        return models
    
    def evaluate_classification_models(self, X: np.ndarray, y: np.ndarray) -> Dict[str, Dict[str, float]]:
        """Comprehensive evaluation of classification models"""
        models = self.setup_classification_models()
        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=0.2, random_state=self.random_state, stratify=y
        )
        
        results = {}
        
        for name, model in models.items():
            print(f"Training {name}...")
            
            # Cross-validation
            cv_scores = cross_val_score(model, X_train, y_train, cv=5, scoring='accuracy')
            
            # Train and predict
            model.fit(X_train, y_train)
            y_pred = model.predict(X_test)
            
            # Calculate metrics
            accuracy = accuracy_score(y_test, y_pred)
            precision = precision_score(y_test, y_pred, average='weighted')
            recall = recall_score(y_test, y_pred, average='weighted')
            f1 = f1_score(y_test, y_pred, average='weighted')
            
            results[name] = {
                'cv_accuracy_mean': cv_scores.mean(),
                'cv_accuracy_std': cv_scores.std(),
                'test_accuracy': accuracy,
                'precision': precision,
                'recall': recall,
                'f1_score': f1
            }
            
            # Store model for later use
            self.models[name] = model
        
        return results
    
    def evaluate_regression_models(self, X: np.ndarray, y: np.ndarray) -> Dict[str, Dict[str, float]]:
        """Comprehensive evaluation of regression models"""
        models = self.setup_regression_models()
        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=0.2, random_state=self.random_state
        )
        
        results = {}
        
        for name, model in models.items():
            print(f"Training {name}...")
            
            # Cross-validation
            cv_scores = cross_val_score(model, X_train, y_train, cv=5, scoring='r2')
            
            # Train and predict
            model.fit(X_train, y_train)
            y_pred = model.predict(X_test)
            
            # Calculate metrics
            mse = mean_squared_error(y_test, y_pred)
            rmse = np.sqrt(mse)
            mae = mean_absolute_error(y_test, y_pred)
            r2 = r2_score(y_test, y_pred)
            
            results[name] = {
                'cv_r2_mean': cv_scores.mean(),
                'cv_r2_std': cv_scores.std(),
                'test_r2': r2,
                'mse': mse,
                'rmse': rmse,
                'mae': mae
            }
            
            # Store model for later use
            self.models[name] = model
        
        return results
    
    def hyperparameter_tuning(self, X: np.ndarray, y: np.ndarray, 
                             model_type: str = 'classification') -> Dict[str, Any]:
        """Perform hyperparameter tuning using GridSearchCV"""
        print("Performing hyperparameter tuning...")
        
        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=0.2, random_state=self.random_state
        )
        
        if model_type == 'classification':
            # Random Forest hyperparameter tuning
            param_grid = {
                'n_estimators': [50, 100, 200],
                'max_depth': [None, 10, 20, 30],
                'min_samples_split': [2, 5, 10],
                'min_samples_leaf': [1, 2, 4]
            }
            
            rf = RandomForestClassifier(random_state=self.random_state)
            grid_search = GridSearchCV(
                rf, param_grid, cv=5, scoring='accuracy', n_jobs=-1
            )
            
        else:  # regression
            param_grid = {
                'n_estimators': [50, 100, 200],
                'max_depth': [3, 5, 7, None],
                'learning_rate': [0.01, 0.1, 0.2]
            }
            
            gb = GradientBoostingRegressor(random_state=self.random_state)
            grid_search = GridSearchCV(
                gb, param_grid, cv=5, scoring='r2', n_jobs=-1
            )
        
        grid_search.fit(X_train, y_train)
        
        return {
            'best_params': grid_search.best_params_,
            'best_score': grid_search.best_score_,
            'best_model': grid_search.best_estimator_
        }
    
    def perform_clustering_analysis(self, X: np.ndarray) -> Dict[str, Any]:
        """Perform clustering analysis using multiple algorithms"""
        print("Performing clustering analysis...")
        
        # K-Means clustering
        kmeans_results = {}
        silhouette_scores = []
        
        for k in range(2, 8):
            kmeans = KMeans(n_clusters=k, random_state=self.random_state, n_init=10)
            cluster_labels = kmeans.fit_predict(X)
            
            from sklearn.metrics import silhouette_score
            sil_score = silhouette_score(X, cluster_labels)
            silhouette_scores.append(sil_score)
            
            kmeans_results[k] = {
                'silhouette_score': sil_score,
                'inertia': kmeans.inertia_,
                'labels': cluster_labels
            }
        
        # Find optimal number of clusters
        optimal_k = max(kmeans_results.keys(), key=lambda k: kmeans_results[k]['silhouette_score'])
        
        # DBSCAN clustering
        from sklearn.metrics import silhouette_score
        dbscan = DBSCAN(eps=0.5, min_samples=5)
        dbscan_labels = dbscan.fit_predict(X)
        
        # Calculate DBSCAN metrics
        n_clusters_dbscan = len(set(dbscan_labels)) - (1 if -1 in dbscan_labels else 0)
        n_noise = list(dbscan_labels).count(-1)
        
        dbscan_results = {
            'n_clusters': n_clusters_dbscan,
            'n_noise_points': n_noise,
            'labels': dbscan_labels
        }
        
        if n_clusters_dbscan > 1:
            # Only calculate silhouette score if we have more than 1 cluster
            mask = dbscan_labels != -1  # Exclude noise points
            if np.sum(mask) > 0 and len(set(dbscan_labels[mask])) > 1:
                dbscan_results['silhouette_score'] = silhouette_score(X[mask], dbscan_labels[mask])
        
        return {
            'kmeans': kmeans_results,
            'optimal_k': optimal_k,
            'dbscan': dbscan_results
        }
    
    def feature_importance_analysis(self, X: np.ndarray, y: np.ndarray, 
                                   feature_names: List[str] = None) -> Dict[str, np.ndarray]:
        """Analyze feature importance using tree-based models"""
        if feature_names is None:
            feature_names = [f"feature_{i}" for i in range(X.shape[1])]
        
        # Random Forest feature importance
        rf = RandomForestClassifier(n_estimators=100, random_state=self.random_state)
        rf.fit(X, y)
        
        # Sort features by importance
        importance_indices = np.argsort(rf.feature_importances_)[::-1]
        
        return {
            'feature_names': np.array(feature_names)[importance_indices],
            'importance_scores': rf.feature_importances_[importance_indices],
            'top_features': np.array(feature_names)[importance_indices[:10]]
        }

def main():
    """Main function to demonstrate advanced ML capabilities"""
    print("Advanced Machine Learning Pipeline Demo")
    print("======================================")
    
    # Initialize pipeline
    ml_pipeline = AdvancedMLPipeline(random_state=42)
    
    # 1. Classification Analysis
    print("\n1. CLASSIFICATION ANALYSIS")
    print("=" * 30)
    
    X_class, y_class = ml_pipeline.generate_classification_dataset(
        n_samples=2000, n_features=25, n_classes=3
    )
    print(f"Generated classification dataset: {X_class.shape}, {len(np.unique(y_class))} classes")
    
    # Preprocess data
    X_class_processed = ml_pipeline.preprocess_data(X_class, apply_pca=True, n_components=0.95)
    
    # Evaluate models
    class_results = ml_pipeline.evaluate_classification_models(X_class_processed, y_class)
    
    print("\nClassification Results:")
    print("-" * 50)
    for model_name, metrics in class_results.items():
        print(f"{model_name}:")
        print(f"  CV Accuracy: {metrics['cv_accuracy_mean']:.3f} (±{metrics['cv_accuracy_std']:.3f})")
        print(f"  Test Accuracy: {metrics['test_accuracy']:.3f}")
        print(f"  F1 Score: {metrics['f1_score']:.3f}")
        print()
    
    # Hyperparameter tuning
    tuning_results = ml_pipeline.hyperparameter_tuning(X_class_processed, y_class, 'classification')
    print(f"Best hyperparameters: {tuning_results['best_params']}")
    print(f"Best CV score: {tuning_results['best_score']:.3f}")
    
    # Feature importance
    feature_importance = ml_pipeline.feature_importance_analysis(X_class_processed, y_class)
    print(f"\nTop 5 most important features:")
    for i in range(5):
        print(f"  {feature_importance['feature_names'][i]}: {feature_importance['importance_scores'][i]:.3f}")
    
    # 2. Regression Analysis
    print("\n\n2. REGRESSION ANALYSIS")
    print("=" * 25)
    
    X_reg, y_reg = ml_pipeline.generate_regression_dataset(
        n_samples=1500, n_features=20, noise=0.1
    )
    print(f"Generated regression dataset: {X_reg.shape}")
    
    X_reg_processed = ml_pipeline.preprocess_data(X_reg, apply_pca=False)
    
    # Evaluate regression models
    reg_results = ml_pipeline.evaluate_regression_models(X_reg_processed, y_reg)
    
    print("\nRegression Results:")
    print("-" * 40)
    for model_name, metrics in reg_results.items():
        print(f"{model_name}:")
        print(f"  CV R²: {metrics['cv_r2_mean']:.3f} (±{metrics['cv_r2_std']:.3f})")
        print(f"  Test R²: {metrics['test_r2']:.3f}")
        print(f"  RMSE: {metrics['rmse']:.3f}")
        print()
    
    # 3. Clustering Analysis
    print("\n3. CLUSTERING ANALYSIS")
    print("=" * 22)
    
    # Use classification data for clustering (without labels)
    clustering_results = ml_pipeline.perform_clustering_analysis(X_class_processed)
    
    print(f"Optimal number of clusters (K-Means): {clustering_results['optimal_k']}")
    print(f"Best silhouette score: {clustering_results['kmeans'][clustering_results['optimal_k']]['silhouette_score']:.3f}")
    
    print(f"\nDBSCAN Results:")
    print(f"  Number of clusters: {clustering_results['dbscan']['n_clusters']}")
    print(f"  Number of noise points: {clustering_results['dbscan']['n_noise_points']}")
    if 'silhouette_score' in clustering_results['dbscan']:
        print(f"  Silhouette score: {clustering_results['dbscan']['silhouette_score']:.3f}")
    
    # 4. Model Comparison Summary
    print("\n4. MODEL COMPARISON SUMMARY")
    print("=" * 30)
    
    print("Classification Models (by accuracy):")
    sorted_class = sorted(class_results.items(), key=lambda x: x[1]['test_accuracy'], reverse=True)
    for i, (name, metrics) in enumerate(sorted_class, 1):
        print(f"  {i}. {name}: {metrics['test_accuracy']:.3f}")
    
    print("\nRegression Models (by R²):")
    sorted_reg = sorted(reg_results.items(), key=lambda x: x[1]['test_r2'], reverse=True)
    for i, (name, metrics) in enumerate(sorted_reg, 1):
        print(f"  {i}. {name}: {metrics['test_r2']:.3f}")
    
    print("\nAdvanced ML pipeline demonstration completed!")
    print(f"Total models trained: {len(class_results) + len(reg_results)}")

if __name__ == "__main__":
    main()