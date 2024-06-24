# General Guidelines

## Model Preprocessing Requirements

| Model                    | Scaling Needed | Encoding Needed | Outliers Handling Needed | Regularization (Lasso/Ridge/ElasticNet) | Regression | Classification |
|--------------------------|----------------|-----------------|-------------------------|----------------------------------------|------------|----------------|
| Linear Regression        | Yes            | Yes             | Yes                     | Yes                                    | Yes        | No             |
| Logistic Regression      | Yes            | Yes             | Yes                     | Yes                                    | No         | Yes            |
| Support Vector Machines  | Yes            | Yes             | Yes                     | No                                     | Yes        | Yes            |
| K-Nearest Neighbors      | Yes            | Yes             | Yes                     | No                                     | Yes        | Yes            |
| Decision Trees           | No             | Yes             | No                      | No                                     | Yes        | Yes            |
| Random Forests           | No             | Yes             | No                      | No                                     | Yes        | Yes            |
| Gradient Boosting (GBM)  | No             | Yes             | No                      | No                                     | Yes        | Yes            |
| XGBoost                  | No             | Yes             | No                      | No                                     | Yes        | Yes            |
| Neural Networks (NN)     | Yes            | Yes             | Yes                     | No (But other regularization methods like dropout are used) | Yes | Yes        |
| Principal Component Analysis (PCA) | Yes | Yes             | No                      | No                                     | No         | No             |
| Generalized Additive Models (GAM) | Yes | Yes             | Yes                     | No (But can include smoothers)         | Yes        | Yes            |

## Recommended Metrics by Model

| Model                    | MAE | MSE | RMSE | R² | Adjusted R² | Accuracy | Precision | Recall | F1 Score | Gini |
|--------------------------|-----|-----|------|----|-------------|----------|-----------|--------|----------|------|
| Linear Regression        | Yes | Yes | Yes  | Yes | Yes         | No       | No        | No     | No       | Yes  |
| Logistic Regression      | No  | No  | No   | No  | No          | Yes      | Yes       | Yes    | Yes      | Yes  |
| Decision Trees           | Yes | Yes | Yes  | Yes | No          | Yes      | Yes       | Yes    | Yes      | Yes  |
| Random Forests           | Yes | Yes | Yes  | Yes | No          | Yes      | Yes       | Yes    | Yes      | Yes  |
| Gradient Boosting (GBM)  | Yes | Yes | Yes  | Yes | No          | Yes      | Yes       | Yes    | Yes      | Yes  |
| XGBoost                  | Yes | Yes | Yes  | Yes | No          | Yes      | Yes       | Yes    | Yes      | Yes  |
| Support Vector Machines  | No  | No  | No   | No  | No          | Yes      | Yes       | Yes    | Yes      | No   |
| K-Nearest Neighbors      | No  | No  | No   | No  | No          | Yes      | Yes       | Yes    | Yes      | No   |
| Neural Networks (NN)     | Yes | Yes | Yes  | No  | No          | Yes      | Yes       | Yes    | Yes      | No   |
| Generalized Additive Models (GAM) | Yes | Yes | Yes  | Yes | Yes         | Yes      | Yes       | Yes    | Yes      | Yes  |

## Pros and Cons of Regression Metrics

| Metric            | Description | Best Models | Pros | Cons |
|-------------------|-------------|-------------|------|------|
| **MAE** (Mean Absolute Error) | Measures the average magnitude of the errors without considering their direction. | Linear Regression, Decision Trees, Random Forests, Gradient Boosting, XGBoost, GAM | Easy to interpret | Less sensitive to outliers than MSE |
| **MSE** (Mean Squared Error)  | Measures the average of the squared differences between predicted and actual values. | Linear Regression, Decision Trees, Random Forests, Gradient Boosting, XGBoost, GAM | Penalizes larger errors more than MAE | Heavily influenced by outliers |
| **RMSE** (Root Mean Squared Error) | Square root of the average squared differences between predicted and actual values. | Linear Regression, Decision Trees, Random Forests, Gradient Boosting, XGBoost, GAM | Similar to MSE but in the same units as the target variable | Still sensitive to outliers |
| **R-squared (R²)**  | Proportion of the variance in the dependent variable that is predictable from the independent variables. | Linear Regression, Decision Trees, Random Forests, Gradient Boosting, XGBoost, GAM | Provides a measure of how well observed outcomes are replicated by the model | Can be misleading for small datasets |
| **Adjusted R-squared** | Adjusted version of R² that accounts for the number of predictors in the model. | Linear Regression, GAM | More accurate measure for models with multiple predictors | Complex to interpret compared to R² |
| **Gini Coefficient** | Measures the inequality among values of a frequency distribution (e.g., levels of income). | Linear Regression, Decision Trees, Random Forests, Gradient Boosting, XGBoost, GAM | Provides a single measure of inequality or dispersion | Can be difficult to interpret in the context of regression |

## Pros and Cons of Classification Metrics

| Metric            | Description | Best Models | Pros | Cons |
|-------------------|-------------|-------------|------|------|
| **Accuracy**      | Proportion of correctly classified instances out of the total instances. | Logistic Regression, Decision Trees, Random Forests, Gradient Boosting, XGBoost, Support Vector Machines, K-Nearest Neighbors, GAM | Simple to interpret | Can be misleading for imbalanced datasets |
| **Precision**     | Proportion of true positive predictions out of all positive predictions. | Logistic Regression, Decision Trees, Random Forests, Gradient Boosting, XGBoost, Support Vector Machines, K-Nearest Neighbors, GAM | Important for scenarios where false positives are costly | Does not account for false negatives |
| **Recall (Sensitivity)** | Proportion of true positive predictions out of all actual positives. | Logistic Regression, Decision Trees, Random Forests, Gradient Boosting, XGBoost, Support Vector Machines, K-Nearest Neighbors, GAM | Important for scenarios where false negatives are costly | Does not account for false positives |
| **F1 Score**      | Harmonic mean of precision and recall. | Logistic Regression, Decision Trees, Random Forests, Gradient Boosting, XGBoost, Support Vector Machines, K-Nearest Neighbors, GAM | Balances precision and recall, useful for imbalanced datasets | Can be difficult to interpret compared to individual precision and recall |
| **AUC-ROC**       | Measures the ability of the model to distinguish between classes. | Logistic Regression, Decision Trees, Random Forests, Gradient Boosting, XGBoost, Support Vector Machines, GAM | Useful for binary classification problems, provides a single measure of performance | Not as intuitive as accuracy |
| **Confusion Matrix** | A table showing the true positives, false positives, true negatives, and false negatives. | Logistic Regression, Decision Trees, Random Forests, Gradient Boosting, XGBoost, Support Vector Machines, K-Nearest Neighbors, GAM | Provides a complete picture of model performance | Requires further metrics like precision and recall for a full evaluation |
| **Gini Coefficient** | Measures the inequality among values of a frequency distribution (e.g., levels of income or binary classification). | Logistic Regression, Decision Trees, Random Forests, Gradient Boosting, XGBoost, GAM | Provides a single measure of inequality or dispersion | Not as commonly used as other classification metrics |

