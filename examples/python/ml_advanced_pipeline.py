import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import make_classification, make_regression
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.ensemble import RandomForestClassifier, GradientBoostingRegressor
from sklearn.linear_model import LogisticRegression, LinearRegression
from sklearn.svm import SVC, SVR
from sklearn.neural_network import MLPClassifier, MLPRegressor
from sklearn.metrics import accuracy_score, mean_squared_error, classification_report
import pandas as pd
import seaborn as sns
from datetime import datetime
import warnings
warnings.filterwarnings('ignore')

class MLPipeline:
    def __init__(self):
        self.models = {}
        self.results = {}
        
    def generate_classification_data(self, n_samples=1000, n_features=20, n_classes=3):
        """Generate synthetic classification data"""
        X, y = make_classification(
            n_samples=n_samples,
            n_features=n_features,
            n_classes=n_classes,
            n_informative=n_features//2,
            n_redundant=n_features//4,
            random_state=42
        )
        return X, y
    
    def generate_regression_data(self, n_samples=1000, n_features=10, noise=0.1):
        """Generate synthetic regression data"""
        X, y = make_regression(
            n_samples=n_samples,
            n_features=n_features,
            noise=noise,
            random_state=42
        )
        return X, y
    
    def setup_classification_models(self):
        """Setup classification models"""
        self.models['classification'] = {
            'Logistic Regression': LogisticRegression(random_state=42, max_iter=1000),
            'Random Forest': RandomForestClassifier(n_estimators=100, random_state=42),
            'SVM': SVC(random_state=42),
            'Neural Network': MLPClassifier(hidden_layer_sizes=(100, 50), random_state=42, max_iter=1000)
        }
    
    def setup_regression_models(self):
        """Setup regression models"""
        self.models['regression'] = {
            'Linear Regression': LinearRegression(),
            'Random Forest': RandomForestRegressor(n_estimators=100, random_state=42),
            'Gradient Boosting': GradientBoostingRegressor(random_state=42),
            'SVR': SVR(),
            'Neural Network': MLPRegressor(hidden_layer_sizes=(100, 50), random_state=42, max_iter=1000)
        }
    
    def run_classification_pipeline(self):
        """Run complete classification pipeline"""
        print("Running Classification Pipeline")
        print("=" * 40)
        
        # Generate data
        X, y = self.generate_classification_data()
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
        
        # Setup models
        self.setup_classification_models()
        
        results = {}
        for name, model in self.models['classification'].items():
            print(f"\nTraining {name}...")
            
            # Train model
            start_time = datetime.now()
            model.fit(X_train, y_train)
            training_time = (datetime.now() - start_time).total_seconds()
            
            # Make predictions
            y_pred = model.predict(X_test)
            accuracy = accuracy_score(y_test, y_pred)
            
            # Cross-validation
            cv_scores = cross_val_score(model, X_train, y_train, cv=5)
            
            results[name] = {
                'accuracy': accuracy,
                'cv_mean': cv_scores.mean(),
                'cv_std': cv_scores.std(),
                'training_time': training_time,
                'predictions': y_pred
            }
            
            print(f"  Accuracy: {accuracy:.4f}")
            print(f"  CV Score: {cv_scores.mean():.4f} (+/- {cv_scores.std() * 2:.4f})")
            print(f"  Training Time: {training_time:.2f}s")
        
        self.results['classification'] = results
        return results
    
    def run_regression_pipeline(self):
        """Run complete regression pipeline"""
        print("\nRunning Regression Pipeline")
        print("=" * 40)
        
        # Generate data
        X, y = self.generate_regression_data()
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
        
        # Setup models
        self.setup_regression_models()
        
        results = {}
        for name, model in self.models['regression'].items():
            print(f"\nTraining {name}...")
            
            # Train model
            start_time = datetime.now()
            model.fit(X_train, y_train)
            training_time = (datetime.now() - start_time).total_seconds()
            
            # Make predictions
            y_pred = model.predict(X_test)
            mse = mean_squared_error(y_test, y_pred)
            rmse = np.sqrt(mse)
            
            # Cross-validation
            cv_scores = cross_val_score(model, X_train, y_train, cv=5, scoring='neg_mean_squared_error')
            cv_rmse = np.sqrt(-cv_scores)
            
            results[name] = {
                'mse': mse,
                'rmse': rmse,
                'cv_rmse_mean': cv_rmse.mean(),
                'cv_rmse_std': cv_rmse.std(),
                'training_time': training_time,
                'predictions': y_pred,
                'actual': y_test
            }
            
            print(f"  RMSE: {rmse:.4f}")
            print(f"  CV RMSE: {cv_rmse.mean():.4f} (+/- {cv_rmse.std() * 2:.4f})")
            print(f"  Training Time: {training_time:.2f}s")
        
        self.results['regression'] = results
        return results
    
    def create_visualizations(self):
        """Create visualizations for results"""
        if 'classification' in self.results:
            self.plot_classification_results()
        
        if 'regression' in self.results:
            self.plot_regression_results()
    
    def plot_classification_results(self):
        """Plot classification results"""
        results = self.results['classification']
        
        # Accuracy comparison
        models = list(results.keys())
        accuracies = [results[model]['accuracy'] for model in models]
        
        plt.figure(figsize=(12, 8))
        
        plt.subplot(2, 2, 1)
        bars = plt.bar(models, accuracies, color=['skyblue', 'lightgreen', 'lightcoral', 'gold'])
        plt.title('Model Accuracy Comparison')
        plt.ylabel('Accuracy')
        plt.xticks(rotation=45)
        
        # Add value labels on bars
        for bar, acc in zip(bars, accuracies):
            plt.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.01, 
                    f'{acc:.3f}', ha='center', va='bottom')
        
        # Training time comparison
        training_times = [results[model]['training_time'] for model in models]
        
        plt.subplot(2, 2, 2)
        plt.bar(models, training_times, color=['skyblue', 'lightgreen', 'lightcoral', 'gold'])
        plt.title('Training Time Comparison')
        plt.ylabel('Time (seconds)')
        plt.xticks(rotation=45)
        
        # Cross-validation scores
        plt.subplot(2, 2, 3)
        cv_means = [results[model]['cv_mean'] for model in models]
        cv_stds = [results[model]['cv_std'] for model in models]
        
        plt.errorbar(models, cv_means, yerr=cv_stds, fmt='o', capsize=5)
        plt.title('Cross-Validation Scores')
        plt.ylabel('CV Score')
        plt.xticks(rotation=45)
        
        plt.tight_layout()
        plt.show()
    
    def plot_regression_results(self):
        """Plot regression results"""
        results = self.results['regression']
        
        # RMSE comparison
        models = list(results.keys())
        rmses = [results[model]['rmse'] for model in models]
        
        plt.figure(figsize=(15, 10))
        
        plt.subplot(2, 3, 1)
        bars = plt.bar(models, rmses, color=['skyblue', 'lightgreen', 'lightcoral', 'gold', 'lightpink'])
        plt.title('Model RMSE Comparison')
        plt.ylabel('RMSE')
        plt.xticks(rotation=45)
        
        # Add value labels
        for bar, rmse in zip(bars, rmses):
            plt.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.5, 
                    f'{rmse:.2f}', ha='center', va='bottom')
        
        # Training time
        training_times = [results[model]['training_time'] for model in models]
        
        plt.subplot(2, 3, 2)
        plt.bar(models, training_times, color=['skyblue', 'lightgreen', 'lightcoral', 'gold', 'lightpink'])
        plt.title('Training Time Comparison')
        plt.ylabel('Time (seconds)')
        plt.xticks(rotation=45)
        
        # Prediction vs Actual scatter plots for best model
        best_model = min(models, key=lambda x: results[x]['rmse'])
        best_results = results[best_model]
        
        plt.subplot(2, 3, 3)
        plt.scatter(best_results['actual'], best_results['predictions'], alpha=0.6)
        plt.plot([best_results['actual'].min(), best_results['actual'].max()], 
                [best_results['actual'].min(), best_results['actual'].max()], 'r--', lw=2)
        plt.xlabel('Actual Values')
        plt.ylabel('Predicted Values')
        plt.title(f'Best Model: {best_model}')
        
        plt.tight_layout()
        plt.show()
    
    def generate_report(self):
        """Generate comprehensive report"""
        print("\n" + "="*60)
        print("MACHINE LEARNING PIPELINE REPORT")
        print("="*60)
        
        if 'classification' in self.results:
            print("\nCLASSIFICATION RESULTS:")
            print("-" * 30)
            
            results = self.results['classification']
            best_model = max(results.keys(), key=lambda x: results[x]['accuracy'])
            
            for model, metrics in results.items():
                status = " ⭐ BEST" if model == best_model else ""
                print(f"\n{model}{status}:")
                print(f"  • Accuracy: {metrics['accuracy']:.4f}")
                print(f"  • CV Score: {metrics['cv_mean']:.4f} ± {metrics['cv_std']:.4f}")
                print(f"  • Training Time: {metrics['training_time']:.2f}s")
        
        if 'regression' in self.results:
            print("\nREGRESSION RESULTS:")
            print("-" * 30)
            
            results = self.results['regression']
            best_model = min(results.keys(), key=lambda x: results[x]['rmse'])
            
            for model, metrics in results.items():
                status = " ⭐ BEST" if model == best_model else ""
                print(f"\n{model}{status}:")
                print(f"  • RMSE: {metrics['rmse']:.4f}")
                print(f"  • CV RMSE: {metrics['cv_rmse_mean']:.4f} ± {metrics['cv_rmse_std']:.4f}")
                print(f"  • Training Time: {metrics['training_time']:.2f}s")

def main():
    """Main execution function"""
    print("Advanced Machine Learning Pipeline")
    print("=" * 50)
    
    # Initialize pipeline
    pipeline = MLPipeline()
    
    # Run classification pipeline
    classification_results = pipeline.run_classification_pipeline()
    
    # Run regression pipeline  
    regression_results = pipeline.run_regression_pipeline()
    
    # Generate comprehensive report
    pipeline.generate_report()
    
    # Create visualizations (commented out to avoid display issues in headless environment)
    # pipeline.create_visualizations()
    
    print(f"\nPipeline completed successfully!")
    print(f"Total models trained: {len(classification_results) + len(regression_results)}")

if __name__ == "__main__":
    main()