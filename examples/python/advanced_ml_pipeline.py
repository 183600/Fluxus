import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split, cross_val_score, GridSearchCV
from sklearn.preprocessing import StandardScaler, LabelEncoder, PolynomialFeatures
from sklearn.linear_model import LinearRegression, LogisticRegression, Ridge, Lasso
from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier, GradientBoostingRegressor
from sklearn.svm import SVR, SVC
from sklearn.neural_network import MLPRegressor, MLPClassifier
from sklearn.cluster import KMeans, DBSCAN, AgglomerativeClustering
from sklearn.decomposition import PCA, FastICA
from sklearn.metrics import (mean_squared_error, mean_absolute_error, r2_score,
                           accuracy_score, precision_score, recall_score, f1_score,
                           classification_report, confusion_matrix, silhouette_score)
from sklearn.pipeline import Pipeline
from sklearn.feature_selection import SelectKBest, f_regression, f_classif
import joblib
import warnings
from typing import Dict, List, Tuple, Any, Optional
from dataclasses import dataclass
import time
import os

warnings.filterwarnings('ignore')

@dataclass
class ModelResult:
    model_name: str
    model: Any
    train_score: float
    test_score: float
    cv_score: float
    training_time: float
    prediction_time: float
    feature_importance: Optional[np.ndarray] = None

class AdvancedMLPipeline:
    def __init__(self, random_state: int = 42):
        self.random_state = random_state
        self.scaler = StandardScaler()
        self.label_encoder = LabelEncoder()
        self.models = {}
        self.results = []
        self.best_model = None
        self.feature_names = []
        
        # Set random seeds for reproducibility
        np.random.seed(random_state)
        
        # Configure plotting
        plt.style.use('seaborn-v0_8' if 'seaborn-v0_8' in plt.style.available else 'default')
        sns.set_palette("husl")
    
    def load_and_prepare_data(self, file_path: str = None, synthetic: bool = True) -> Tuple[pd.DataFrame, str]:
        """Load and prepare dataset"""
        if synthetic or file_path is None:
            return self._generate_synthetic_dataset()
        else:
            # Load real dataset
            if file_path.endswith('.csv'):
                data = pd.read_csv(file_path)
            elif file_path.endswith('.json'):
                data = pd.read_json(file_path)
            else:
                raise ValueError("Unsupported file format. Use CSV or JSON.")
            
            return data, self._detect_task_type(data)
    
    def _generate_synthetic_dataset(self) -> Tuple[pd.DataFrame, str]:
        """Generate synthetic dataset for demonstration"""
        np.random.seed(self.random_state)
        
        # Create regression dataset
        n_samples = 1000
        n_features = 10
        
        # Generate feature matrix
        X = np.random.randn(n_samples, n_features)
        
        # Create meaningful feature names
        feature_names = [f'feature_{i+1}' for i in range(n_features)]
        
        # Generate target with some non-linear relationships
        y_reg = (2 * X[:, 0] + 1.5 * X[:, 1] - 0.8 * X[:, 2] + 
                0.5 * X[:, 3] * X[:, 4] + 0.3 * X[:, 5]**2 + 
                np.random.randn(n_samples) * 0.1)
        
        # Create classification target
        y_class = (y_reg > np.median(y_reg)).astype(int)
        
        # Combine into DataFrame
        data = pd.DataFrame(X, columns=feature_names)
        data['target_regression'] = y_reg
        data['target_classification'] = y_class
        
        # Add some categorical features
        data['category_A'] = np.random.choice(['type1', 'type2', 'type3'], n_samples)
        data['category_B'] = np.random.choice(['group_X', 'group_Y'], n_samples)
        
        return data, 'mixed'
    
    def _detect_task_type(self, data: pd.DataFrame) -> str:
        """Detect whether the problem is regression, classification, or clustering"""
        target_cols = [col for col in data.columns if 'target' in col.lower()]
        
        if not target_cols:
            return 'clustering'
        
        target_col = target_cols[0]
        unique_values = data[target_col].nunique()
        
        if unique_values <= 10 and data[target_col].dtype in ['object', 'category', 'bool']:
            return 'classification'
        elif unique_values <= 20:
            return 'classification'
        else:
            return 'regression'
    
    def preprocess_data(self, data: pd.DataFrame, target_col: str = None) -> Tuple[np.ndarray, np.ndarray]:
        """Comprehensive data preprocessing"""
        print("Preprocessing data...")
        
        # Separate features and target
        if target_col is None:
            # Auto-detect target column
            target_cols = [col for col in data.columns if 'target' in col.lower()]
            if target_cols:
                target_col = target_cols[0]
            else:
                # No target column found, return features only for clustering
                X = data.select_dtypes(include=[np.number])
                return self.scaler.fit_transform(X), None
        
        X = data.drop(columns=[target_col])
        y = data[target_col]
        
        # Handle categorical features
        categorical_cols = X.select_dtypes(include=['object', 'category']).columns
        for col in categorical_cols:
            # One-hot encode categorical variables
            dummies = pd.get_dummies(X[col], prefix=col)
            X = pd.concat([X.drop(columns=[col]), dummies], axis=1)
        
        # Store feature names
        self.feature_names = X.columns.tolist()
        
        # Convert to numpy arrays
        X = X.select_dtypes(include=[np.number]).values
        
        # Handle missing values
        X = np.nan_to_num(X, nan=0.0)
        
        # Scale features
        X = self.scaler.fit_transform(X)
        
        # Encode target if necessary
        if y.dtype == 'object' or y.dtype.name == 'category':
            y = self.label_encoder.fit_transform(y)
        
        print(f"Data shape: {X.shape}")
        print(f"Target distribution: {np.bincount(y.astype(int)) if len(np.unique(y)) < 20 else 'Continuous'}")
        
        return X, y
    
    def build_regression_models(self) -> Dict[str, Any]:
        """Build regression models"""
        models = {
            'Linear Regression': LinearRegression(),
            'Ridge Regression': Ridge(alpha=1.0, random_state=self.random_state),
            'Lasso Regression': Lasso(alpha=1.0, random_state=self.random_state),
            'Random Forest': RandomForestRegressor(n_estimators=100, random_state=self.random_state),
            'Gradient Boosting': GradientBoostingRegressor(n_estimators=100, random_state=self.random_state),
            'Support Vector Regressor': SVR(kernel='rbf', C=1.0),
            'Neural Network': MLPRegressor(hidden_layer_sizes=(100, 50), max_iter=500, random_state=self.random_state)
        }
        return models
    
    def build_classification_models(self) -> Dict[str, Any]:
        """Build classification models"""
        models = {
            'Logistic Regression': LogisticRegression(random_state=self.random_state, max_iter=1000),
            'Random Forest': RandomForestClassifier(n_estimators=100, random_state=self.random_state),
            'Support Vector Classifier': SVC(kernel='rbf', C=1.0, random_state=self.random_state),
            'Neural Network': MLPClassifier(hidden_layer_sizes=(100, 50), max_iter=500, random_state=self.random_state)
        }
        return models
    
    def build_clustering_models(self) -> Dict[str, Any]:
        """Build clustering models"""
        models = {
            'K-Means': KMeans(n_clusters=3, random_state=self.random_state),
            'DBSCAN': DBSCAN(eps=0.5, min_samples=5),
            'Agglomerative': AgglomerativeClustering(n_clusters=3)
        }
        return models
    
    def train_and_evaluate_models(self, X: np.ndarray, y: np.ndarray, task_type: str) -> List[ModelResult]:
        """Train and evaluate all models"""
        print(f"\nTraining {task_type} models...")
        
        if task_type == 'regression':
            models = self.build_regression_models()
            scoring = 'r2'
        elif task_type == 'classification':
            models = self.build_classification_models()
            scoring = 'accuracy'
        else:  # clustering
            models = self.build_clustering_models()
            return self._evaluate_clustering_models(models, X)
        
        # Split data
        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=0.2, random_state=self.random_state, stratify=y if task_type == 'classification' else None
        )
        
        results = []
        
        for name, model in models.items():
            print(f"Training {name}...")
            
            # Train model
            start_time = time.time()
            model.fit(X_train, y_train)
            training_time = time.time() - start_time
            
            # Make predictions
            start_time = time.time()
            train_pred = model.predict(X_train)
            test_pred = model.predict(X_test)
            prediction_time = time.time() - start_time
            
            # Calculate scores
            if task_type == 'regression':
                train_score = r2_score(y_train, train_pred)
                test_score = r2_score(y_test, test_pred)
            else:
                train_score = accuracy_score(y_train, train_pred)
                test_score = accuracy_score(y_test, test_pred)
            
            # Cross-validation
            cv_scores = cross_val_score(model, X_train, y_train, cv=5, scoring=scoring)
            cv_score = cv_scores.mean()
            
            # Feature importance (if available)
            feature_importance = None
            if hasattr(model, 'feature_importances_'):
                feature_importance = model.feature_importances_
            elif hasattr(model, 'coef_'):
                feature_importance = np.abs(model.coef_).flatten()
            
            result = ModelResult(
                model_name=name,
                model=model,
                train_score=train_score,
                test_score=test_score,
                cv_score=cv_score,
                training_time=training_time,
                prediction_time=prediction_time,
                feature_importance=feature_importance
            )
            
            results.append(result)
            
            print(f"  Train Score: {train_score:.4f}")
            print(f"  Test Score: {test_score:.4f}")
            print(f"  CV Score: {cv_score:.4f} (+/- {cv_scores.std() * 2:.4f})")
            print(f"  Training Time: {training_time:.4f}s")
            print()
        
        return results
    
    def _evaluate_clustering_models(self, models: Dict[str, Any], X: np.ndarray) -> List[ModelResult]:
        """Evaluate clustering models"""
        results = []
        
        for name, model in models.items():
            print(f"Training {name}...")
            
            start_time = time.time()
            labels = model.fit_predict(X)
            training_time = time.time() - start_time
            
            # Calculate silhouette score
            if len(np.unique(labels)) > 1:
                silhouette = silhouette_score(X, labels)
            else:
                silhouette = -1  # Invalid clustering
            
            result = ModelResult(
                model_name=name,
                model=model,
                train_score=silhouette,
                test_score=silhouette,
                cv_score=silhouette,
                training_time=training_time,
                prediction_time=0.0
            )
            
            results.append(result)
            
            print(f"  Silhouette Score: {silhouette:.4f}")
            print(f"  Number of Clusters: {len(np.unique(labels))}")
            print(f"  Training Time: {training_time:.4f}s")
            print()
        
        return results
    
    def hyperparameter_tuning(self, X: np.ndarray, y: np.ndarray, task_type: str) -> Dict[str, Any]:
        """Perform hyperparameter tuning for best models"""
        print("\nPerforming hyperparameter tuning...")
        
        if task_type == 'regression':
            # Random Forest tuning
            param_grid = {
                'n_estimators': [50, 100, 200],
                'max_depth': [10, 20, None],
                'min_samples_split': [2, 5, 10]
            }
            model = RandomForestRegressor(random_state=self.random_state)
            scoring = 'r2'
            
        elif task_type == 'classification':
            # Random Forest tuning
            param_grid = {
                'n_estimators': [50, 100, 200],
                'max_depth': [10, 20, None],
                'min_samples_split': [2, 5, 10]
            }
            model = RandomForestClassifier(random_state=self.random_state)
            scoring = 'accuracy'
        
        else:
            return {}
        
        grid_search = GridSearchCV(
            model, param_grid, cv=5, scoring=scoring, n_jobs=-1
        )
        
        grid_search.fit(X, y)
        
        print(f"Best parameters: {grid_search.best_params_}")
        print(f"Best cross-validation score: {grid_search.best_score_:.4f}")
        
        return {
            'best_model': grid_search.best_estimator_,
            'best_params': grid_search.best_params_,
            'best_score': grid_search.best_score_
        }
    
    def feature_analysis(self, X: np.ndarray, y: np.ndarray, task_type: str):
        """Perform feature analysis"""
        print("\nPerforming feature analysis...")
        
        if task_type == 'clustering':
            # PCA for clustering
            pca = PCA(n_components=min(10, X.shape[1]))
            X_pca = pca.fit_transform(X)
            
            plt.figure(figsize=(12, 4))
            
            plt.subplot(1, 2, 1)
            plt.bar(range(len(pca.explained_variance_ratio_)), pca.explained_variance_ratio_)
            plt.title('PCA Explained Variance Ratio')
            plt.xlabel('Component')
            plt.ylabel('Explained Variance Ratio')
            
            plt.subplot(1, 2, 2)
            plt.plot(np.cumsum(pca.explained_variance_ratio_))
            plt.title('Cumulative Explained Variance')
            plt.xlabel('Component')
            plt.ylabel('Cumulative Explained Variance')
            
            plt.tight_layout()
            plt.savefig('feature_analysis_clustering.png', dpi=300, bbox_inches='tight')
            plt.show()
            
            return
        
        # Feature selection for supervised learning
        if task_type == 'regression':
            selector = SelectKBest(score_func=f_regression, k=min(10, X.shape[1]))
        else:
            selector = SelectKBest(score_func=f_classif, k=min(10, X.shape[1]))
        
        X_selected = selector.fit_transform(X, y)
        selected_features = selector.get_support(indices=True)
        
        # Plot feature scores
        plt.figure(figsize=(12, 6))
        
        plt.subplot(1, 2, 1)
        scores = selector.scores_
        plt.bar(range(len(scores)), scores)
        plt.title('Feature Importance Scores')
        plt.xlabel('Feature Index')
        plt.ylabel('Score')
        
        # Plot selected features
        plt.subplot(1, 2, 2)
        selected_scores = scores[selected_features]
        plt.bar(range(len(selected_scores)), selected_scores)
        plt.title('Top Selected Features')
        plt.xlabel('Selected Feature Index')
        plt.ylabel('Score')
        
        plt.tight_layout()
        plt.savefig('feature_analysis.png', dpi=300, bbox_inches='tight')
        plt.show()
        
        print(f"Selected {len(selected_features)} features out of {X.shape[1]}")
        if len(self.feature_names) == X.shape[1]:
            print("Top selected features:")
            for i, idx in enumerate(selected_features[:5]):
                print(f"  {self.feature_names[idx]}: {scores[idx]:.4f}")
    
    def visualize_results(self, results: List[ModelResult], task_type: str):
        """Visualize model results"""
        print("\nGenerating visualizations...")
        
        # Extract data for plotting
        model_names = [r.model_name for r in results]
        train_scores = [r.train_score for r in results]
        test_scores = [r.test_score for r in results]
        cv_scores = [r.cv_score for r in results]
        training_times = [r.training_time for r in results]
        
        fig, axes = plt.subplots(2, 2, figsize=(15, 10))
        
        # Model performance comparison
        x = np.arange(len(model_names))
        width = 0.25
        
        axes[0, 0].bar(x - width, train_scores, width, label='Train Score', alpha=0.8)
        axes[0, 0].bar(x, test_scores, width, label='Test Score', alpha=0.8)
        axes[0, 0].bar(x + width, cv_scores, width, label='CV Score', alpha=0.8)
        axes[0, 0].set_xlabel('Models')
        axes[0, 0].set_ylabel('Score')
        axes[0, 0].set_title('Model Performance Comparison')
        axes[0, 0].set_xticks(x)
        axes[0, 0].set_xticklabels(model_names, rotation=45, ha='right')
        axes[0, 0].legend()
        axes[0, 0].grid(True, alpha=0.3)
        
        # Training time comparison
        axes[0, 1].bar(model_names, training_times, alpha=0.8, color='orange')
        axes[0, 1].set_xlabel('Models')
        axes[0, 1].set_ylabel('Training Time (s)')
        axes[0, 1].set_title('Training Time Comparison')
        axes[0, 1].tick_params(axis='x', rotation=45)
        axes[0, 1].grid(True, alpha=0.3)
        
        # Performance vs Time scatter plot
        axes[1, 0].scatter(training_times, test_scores, s=100, alpha=0.7)
        for i, name in enumerate(model_names):
            axes[1, 0].annotate(name, (training_times[i], test_scores[i]), 
                              xytext=(5, 5), textcoords='offset points', fontsize=8)
        axes[1, 0].set_xlabel('Training Time (s)')
        axes[1, 0].set_ylabel('Test Score')
        axes[1, 0].set_title('Performance vs Training Time')
        axes[1, 0].grid(True, alpha=0.3)
        
        # Feature importance (if available)
        best_result = max(results, key=lambda x: x.test_score)
        if best_result.feature_importance is not None:
            importance = best_result.feature_importance
            top_indices = np.argsort(importance)[-10:]  # Top 10 features
            
            axes[1, 1].barh(range(len(top_indices)), importance[top_indices], alpha=0.8)
            axes[1, 1].set_xlabel('Feature Importance')
            axes[1, 1].set_title(f'Top Features - {best_result.model_name}')
            
            if len(self.feature_names) == len(importance):
                axes[1, 1].set_yticks(range(len(top_indices)))
                axes[1, 1].set_yticklabels([self.feature_names[i] for i in top_indices])
            else:
                axes[1, 1].set_ylabel('Feature Index')
        else:
            axes[1, 1].text(0.5, 0.5, 'Feature importance not available', 
                          ha='center', va='center', transform=axes[1, 1].transAxes)
            axes[1, 1].set_title('Feature Importance')
        
        plt.tight_layout()
        plt.savefig('model_comparison.png', dpi=300, bbox_inches='tight')
        plt.show()
    
    def generate_report(self, results: List[ModelResult], task_type: str, tuning_results: Dict[str, Any] = None):
        """Generate comprehensive report"""
        print("\n" + "="*80)
        print("MACHINE LEARNING PIPELINE REPORT")
        print("="*80)
        
        print(f"\nTask Type: {task_type.title()}")
        print(f"Number of Models Evaluated: {len(results)}")
        
        # Sort results by test score
        sorted_results = sorted(results, key=lambda x: x.test_score, reverse=True)
        
        print(f"\nModel Performance Ranking:")
        print("-" * 50)
        for i, result in enumerate(sorted_results, 1):
            print(f"{i:2d}. {result.model_name:<25} Test Score: {result.test_score:.4f}")
        
        # Best model details
        best_result = sorted_results[0]
        print(f"\nBest Model: {best_result.model_name}")
        print("-" * 30)
        print(f"Train Score: {best_result.train_score:.4f}")
        print(f"Test Score: {best_result.test_score:.4f}")
        print(f"Cross-Validation Score: {best_result.cv_score:.4f}")
        print(f"Training Time: {best_result.training_time:.4f}s")
        print(f"Prediction Time: {best_result.prediction_time:.4f}s")
        
        # Hyperparameter tuning results
        if tuning_results:
            print(f"\nHyperparameter Tuning Results:")
            print("-" * 30)
            print(f"Best Parameters: {tuning_results['best_params']}")
            print(f"Best Score: {tuning_results['best_score']:.4f}")
        
        # Performance statistics
        test_scores = [r.test_score for r in results]
        print(f"\nPerformance Statistics:")
        print("-" * 30)
        print(f"Mean Test Score: {np.mean(test_scores):.4f}")
        print(f"Std Test Score: {np.std(test_scores):.4f}")
        print(f"Min Test Score: {np.min(test_scores):.4f}")
        print(f"Max Test Score: {np.max(test_scores):.4f}")
        
        # Save best model
        model_filename = f'best_model_{task_type}.joblib'
        joblib.dump(best_result.model, model_filename)
        print(f"\nBest model saved as: {model_filename}")
        
        self.best_model = best_result.model
        self.results = results
        
        return sorted_results[0]
    
    def run_complete_pipeline(self, file_path: str = None, target_col: str = None) -> ModelResult:
        """Run the complete ML pipeline"""
        print("Starting Advanced Machine Learning Pipeline")
        print("=" * 50)
        
        # Load and prepare data
        data, detected_task = self.load_and_prepare_data(file_path)
        print(f"Dataset loaded: {data.shape}")
        print(f"Detected task type: {detected_task}")
        
        # Determine task type and target column
        if target_col is None:
            if detected_task == 'mixed':
                # For demo dataset, choose regression
                target_col = 'target_regression'
                task_type = 'regression'
            elif detected_task == 'clustering':
                task_type = 'clustering'
            else:
                task_type = detected_task
                target_cols = [col for col in data.columns if 'target' in col.lower()]
                target_col = target_cols[0] if target_cols else data.columns[-1]
        else:
            task_type = self._detect_task_type(data)
        
        # Preprocess data
        X, y = self.preprocess_data(data, target_col)
        
        # Feature analysis
        self.feature_analysis(X, y, task_type)
        
        # Train and evaluate models
        results = self.train_and_evaluate_models(X, y, task_type)
        
        # Hyperparameter tuning
        tuning_results = None
        if task_type in ['regression', 'classification']:
            tuning_results = self.hyperparameter_tuning(X, y, task_type)
        
        # Visualize results
        self.visualize_results(results, task_type)
        
        # Generate report
        best_result = self.generate_report(results, task_type, tuning_results)
        
        return best_result

def demo_advanced_pipeline():
    """Demonstrate the advanced ML pipeline"""
    pipeline = AdvancedMLPipeline(random_state=42)
    
    # Run with synthetic data
    print("Running pipeline with synthetic dataset...")
    best_model = pipeline.run_complete_pipeline()
    
    print(f"\nPipeline completed successfully!")
    print(f"Best model: {best_model.model_name}")
    print(f"Best test score: {best_model.test_score:.4f}")
    
    return pipeline

def main():
    """Main function"""
    # Set up environment
    os.makedirs('ml_outputs', exist_ok=True)
    os.chdir('ml_outputs')
    
    # Run the demo
    pipeline = demo_advanced_pipeline()
    
    print("\nAll outputs saved in 'ml_outputs' directory")
    print("Files generated:")
    print("- best_model_regression.joblib (trained model)")
    print("- model_comparison.png (visualization)")
    print("- feature_analysis.png (feature analysis)")
    
    return pipeline

if __name__ == "__main__":
    main()