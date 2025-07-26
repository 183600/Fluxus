import numpy as np
import matplotlib.pyplot as plt
from scipy import stats, optimize
import pandas as pd
from sklearn.linear_model import LinearRegression, LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, mean_squared_error
from sklearn.preprocessing import StandardScaler
import seaborn as sns
import warnings
warnings.filterwarnings('ignore')

class DataAnalyzer:
    def __init__(self, data=None):
        self.data = data
        self.models = {}
        self.scalers = {}
        
    def generate_sample_data(self, n_samples=1000):
        """Generate sample dataset for demonstration"""
        np.random.seed(42)
        
        # Generate features
        age = np.random.normal(35, 10, n_samples)
        income = np.random.normal(50000, 15000, n_samples)
        education_years = np.random.randint(12, 20, n_samples)
        
        # Generate target with some noise
        satisfaction = (
            0.3 * (age - 35) / 10 + 
            0.4 * (income - 50000) / 15000 + 
            0.3 * (education_years - 16) / 4 + 
            np.random.normal(0, 0.2, n_samples)
        )
        
        # Create binary classification target
        high_satisfaction = (satisfaction > np.median(satisfaction)).astype(int)
        
        self.data = pd.DataFrame({
            'age': age,
            'income': income,
            'education_years': education_years,
            'satisfaction_score': satisfaction,
            'high_satisfaction': high_satisfaction
        })
        
        return self.data
    
    def descriptive_statistics(self):
        """Calculate descriptive statistics"""
        if self.data is None:
            return None
            
        stats_dict = {}
        
        for column in self.data.select_dtypes(include=[np.number]).columns:
            col_data = self.data[column]
            stats_dict[column] = {
                'mean': np.mean(col_data),
                'median': np.median(col_data),
                'std': np.std(col_data),
                'min': np.min(col_data),
                'max': np.max(col_data),
                'q25': np.percentile(col_data, 25),
                'q75': np.percentile(col_data, 75),
                'skewness': stats.skew(col_data),
                'kurtosis': stats.kurtosis(col_data)
            }
        
        return stats_dict
    
    def correlation_analysis(self):
        """Perform correlation analysis"""
        if self.data is None:
            return None
            
        numeric_data = self.data.select_dtypes(include=[np.number])
        correlation_matrix = numeric_data.corr()
        
        return correlation_matrix
    
    def linear_regression_analysis(self, target_col, feature_cols):
        """Perform linear regression analysis"""
        if self.data is None:
            return None
            
        X = self.data[feature_cols]
        y = self.data[target_col]
        
        # Split data
        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=0.2, random_state=42
        )
        
        # Scale features
        scaler = StandardScaler()
        X_train_scaled = scaler.fit_transform(X_train)
        X_test_scaled = scaler.transform(X_test)
        
        # Train model
        model = LinearRegression()
        model.fit(X_train_scaled, y_train)
        
        # Make predictions
        y_train_pred = model.predict(X_train_scaled)
        y_test_pred = model.predict(X_test_scaled)
        
        # Calculate metrics
        train_mse = mean_squared_error(y_train, y_train_pred)
        test_mse = mean_squared_error(y_test, y_test_pred)
        
        results = {
            'model': model,
            'scaler': scaler,
            'train_mse': train_mse,
            'test_mse': test_mse,
            'r2_score': model.score(X_test_scaled, y_test),
            'coefficients': dict(zip(feature_cols, model.coef_)),
            'intercept': model.intercept_
        }
        
        self.models['linear_regression'] = results
        return results
    
    def logistic_regression_analysis(self, target_col, feature_cols):
        """Perform logistic regression analysis"""
        if self.data is None:
            return None
            
        X = self.data[feature_cols]
        y = self.data[target_col]
        
        # Split data
        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=0.2, random_state=42
        )
        
        # Scale features
        scaler = StandardScaler()
        X_train_scaled = scaler.fit_transform(X_train)
        X_test_scaled = scaler.transform(X_test)
        
        # Train model
        model = LogisticRegression(random_state=42)
        model.fit(X_train_scaled, y_train)
        
        # Make predictions
        y_train_pred = model.predict(X_train_scaled)
        y_test_pred = model.predict(X_test_scaled)
        
        # Calculate metrics
        train_accuracy = accuracy_score(y_train, y_train_pred)
        test_accuracy = accuracy_score(y_test, y_test_pred)
        
        results = {
            'model': model,
            'scaler': scaler,
            'train_accuracy': train_accuracy,
            'test_accuracy': test_accuracy,
            'coefficients': dict(zip(feature_cols, model.coef_[0])),
            'intercept': model.intercept_[0]
        }
        
        self.models['logistic_regression'] = results
        return results
    
    def hypothesis_testing(self, col1, col2):
        """Perform statistical hypothesis testing"""
        if self.data is None:
            return None
            
        data1 = self.data[col1].dropna()
        data2 = self.data[col2].dropna()
        
        # Pearson correlation test
        corr_coef, corr_p_value = stats.pearsonr(data1, data2)
        
        # Independent t-test (assuming different groups)
        # For demonstration, split data at median
        group1 = data1[data1 <= data1.median()]
        group2 = data1[data1 > data1.median()]
        
        t_stat, t_p_value = stats.ttest_ind(group1, group2)
        
        # Normality tests
        shapiro_stat1, shapiro_p1 = stats.shapiro(data1[:5000])  # Limit for shapiro
        shapiro_stat2, shapiro_p2 = stats.shapiro(data2[:5000])
        
        results = {
            'correlation': {
                'coefficient': corr_coef,
                'p_value': corr_p_value,
                'significant': corr_p_value < 0.05
            },
            't_test': {
                'statistic': t_stat,
                'p_value': t_p_value,
                'significant': t_p_value < 0.05
            },
            'normality_test': {
                col1: {'statistic': shapiro_stat1, 'p_value': shapiro_p1, 'normal': shapiro_p1 > 0.05},
                col2: {'statistic': shapiro_stat2, 'p_value': shapiro_p2, 'normal': shapiro_p2 > 0.05}
            }
        }
        
        return results
    
    def time_series_analysis(self, values, periods=12):
        """Basic time series analysis"""
        if isinstance(values, str):
            values = self.data[values].values
            
        # Create time series
        ts = pd.Series(values)
        
        # Moving average
        moving_avg = ts.rolling(window=periods).mean()
        
        # Trend analysis
        x = np.arange(len(values))
        slope, intercept, r_value, p_value, std_err = stats.linregress(x, values)
        
        # Seasonal decomposition (simplified)
        seasonal_pattern = []
        for i in range(periods):
            seasonal_values = values[i::periods]
            seasonal_pattern.append(np.mean(seasonal_values))
        
        results = {
            'original_series': values,
            'moving_average': moving_avg.values,
            'trend': {
                'slope': slope,
                'intercept': intercept,
                'r_squared': r_value**2,
                'p_value': p_value
            },
            'seasonal_pattern': seasonal_pattern,
            'mean': np.mean(values),
            'std': np.std(values),
            'autocorrelation': np.corrcoef(values[:-1], values[1:])[0, 1]
        }
        
        return results
    
    def outlier_detection(self, column, method='iqr'):
        """Detect outliers using different methods"""
        if self.data is None:
            return None
            
        data = self.data[column].dropna()
        
        if method == 'iqr':
            Q1 = data.quantile(0.25)
            Q3 = data.quantile(0.75)
            IQR = Q3 - Q1
            lower_bound = Q1 - 1.5 * IQR
            upper_bound = Q3 + 1.5 * IQR
            outliers = data[(data < lower_bound) | (data > upper_bound)]
            
        elif method == 'zscore':
            z_scores = np.abs(stats.zscore(data))
            outliers = data[z_scores > 3]
            
        elif method == 'modified_zscore':
            median = np.median(data)
            mad = np.median(np.abs(data - median))
            modified_z_scores = 0.6745 * (data - median) / mad
            outliers = data[np.abs(modified_z_scores) > 3.5]
        
        results = {
            'method': method,
            'outliers': outliers.values,
            'outlier_count': len(outliers),
            'outlier_percentage': len(outliers) / len(data) * 100,
            'outlier_indices': outliers.index.tolist()
        }
        
        return results

def run_comprehensive_analysis():
    """Run comprehensive data analysis demonstration"""
    print("Advanced Data Science and Statistics Analysis")
    print("=" * 50)
    
    # Initialize analyzer
    analyzer = DataAnalyzer()
    
    # Generate sample data
    print("Generating sample dataset...")
    data = analyzer.generate_sample_data(1000)
    print(f"Dataset shape: {data.shape}")
    print(f"Columns: {list(data.columns)}")
    
    # Descriptive statistics
    print("\n1. Descriptive Statistics:")
    print("-" * 30)
    desc_stats = analyzer.descriptive_statistics()
    for col, stats in desc_stats.items():
        print(f"\n{col}:")
        for stat, value in stats.items():
            print(f"  {stat}: {value:.4f}")
    
    # Correlation analysis
    print("\n2. Correlation Analysis:")
    print("-" * 30)
    corr_matrix = analyzer.correlation_analysis()
    print(corr_matrix.round(4))
    
    # Linear regression
    print("\n3. Linear Regression Analysis:")
    print("-" * 30)
    lr_results = analyzer.linear_regression_analysis(
        'satisfaction_score', 
        ['age', 'income', 'education_years']
    )
    print(f"Train MSE: {lr_results['train_mse']:.4f}")
    print(f"Test MSE: {lr_results['test_mse']:.4f}")
    print(f"R² Score: {lr_results['r2_score']:.4f}")
    print("Coefficients:")
    for feature, coef in lr_results['coefficients'].items():
        print(f"  {feature}: {coef:.4f}")
    
    # Logistic regression
    print("\n4. Logistic Regression Analysis:")
    print("-" * 30)
    log_results = analyzer.logistic_regression_analysis(
        'high_satisfaction', 
        ['age', 'income', 'education_years']
    )
    print(f"Train Accuracy: {log_results['train_accuracy']:.4f}")
    print(f"Test Accuracy: {log_results['test_accuracy']:.4f}")
    print("Coefficients:")
    for feature, coef in log_results['coefficients'].items():
        print(f"  {feature}: {coef:.4f}")
    
    # Hypothesis testing
    print("\n5. Hypothesis Testing:")
    print("-" * 30)
    hyp_results = analyzer.hypothesis_testing('age', 'satisfaction_score')
    print(f"Correlation coefficient: {hyp_results['correlation']['coefficient']:.4f}")
    print(f"Correlation p-value: {hyp_results['correlation']['p_value']:.4f}")
    print(f"Correlation significant: {hyp_results['correlation']['significant']}")
    
    # Time series analysis
    print("\n6. Time Series Analysis:")
    print("-" * 30)
    ts_results = analyzer.time_series_analysis('satisfaction_score')
    print(f"Trend slope: {ts_results['trend']['slope']:.6f}")
    print(f"Trend R²: {ts_results['trend']['r_squared']:.4f}")
    print(f"Autocorrelation: {ts_results['autocorrelation']:.4f}")
    
    # Outlier detection
    print("\n7. Outlier Detection:")
    print("-" * 30)
    for method in ['iqr', 'zscore', 'modified_zscore']:
        outliers = analyzer.outlier_detection('income', method)
        print(f"{method.upper()} method: {outliers['outlier_count']} outliers "
              f"({outliers['outlier_percentage']:.2f}%)")
    
    print("\nAnalysis complete!")

if __name__ == "__main__":
    run_comprehensive_analysis()