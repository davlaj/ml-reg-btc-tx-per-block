## Model Evaluation Metrics Applicability

| Model                    | MAE | MSE | RMSE | R²  | Adjusted R² | Accuracy | Precision | Recall | F1 Score |
|--------------------------|-----|-----|------|-----|-------------|----------|-----------|--------|----------|
| Linear Regression        | Yes | Yes | Yes  | Yes | Yes         | No       | No        | No     | No       |
| Logistic Regression      | No  | No  | No   | No  | No          | Yes      | Yes       | Yes    | Yes      |
| Decision Trees           | Yes | Yes | Yes  | Yes | No          | Yes      | Yes       | Yes    | Yes      |
| Random Forests           | Yes | Yes | Yes  | Yes | No          | Yes      | Yes       | Yes    | Yes      |
| Gradient Boosting (GBM)  | Yes | Yes | Yes  | Yes | No          | Yes      | Yes       | Yes    | Yes      |
| XGBoost                  | Yes | Yes | Yes  | Yes | No          | Yes      | Yes       | Yes    | Yes      |
| Support Vector Machines  | No  | No  | No   | No  | No          | Yes      | Yes       | Yes    | Yes      |
| K-Nearest Neighbors      | No  | No  | No   | No  | No          | Yes      | Yes       | Yes    | Yes      |
| Neural Networks (NN)     | Yes | Yes | Yes  | No  | No          | Yes      | Yes       | Yes    | Yes      |



