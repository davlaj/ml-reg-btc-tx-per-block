# Load packages
source("setup-packages.R") 

set.seed(123)  # for reproducibility
  
# 1.Data Splitting
splitting <- function(data) {
  
  # Setting up the time series cross-validation
  training_rows <- createTimeSlices(
    1:nrow(data),  # The total number of rows in the data 
    initialWindow = round(nrow(data) * 0.8),  # Initial training set size
    horizon = 1,  # Forecast horizon of 1 period ahead
    fixedWindow = TRUE,  # Whether to keep the training size constant
    skips = 0  # No skips between slices
  )$train
  
  # The output of createTimeSlices is a list of indices for training sets ($train)
  # [[1]] corresponds to the indices of the first fold's training data
  train_set <- data[training_rows[[1]], ]
  test_set <- data[-training_rows[[1]], ]
}

# 2.Scaling - Apply to continuous numeric non-categorical variables, excluding the target variable
scaling <- function(data, numeric_cols_to_scale, scale_data = TRUE) {
  
  # NORMALIZATION = (x-xmin)/(xmax-xmin)
  # no
  
  # STANDARDIZATION (Z-SCORE SCALING = (x-mu)/sd )
  if (scale_data) {
    data %>%
      mutate(across(all_of(numeric_cols_to_scale), scale))
  } else {
    data
  }
}

# 3. Model Evaluation Framework
evaluate_model <- function(train_data, test_data, model_function, model_name, scale_data = TRUE) {
  target <- "TotalTransactions"
  
  # Preprocess data if scaling is required
  numeric_cols_to_scale <- names(train_data)[sapply(train_data, is.numeric) & !names(train_data) %in% c("TotalTransactions", "BlockNumber", "HourOfDay", "DayOfWeek", "IsWeekend")]
  train_data <- preprocess_data(train_data, numeric_cols_to_scale, scale_data)
  test_data <- preprocess_data(test_data, numeric_cols_to_scale, scale_data)
  
  # Separate predictors and target
  train_x <- train_data %>% select(-all_of(target))
  train_y <- train_data[[target]]
  test_x <- test_data %>% select(-all_of(target))
  test_y <- test_data[[target]]
  
  # Train the model
  model <- model_function(train_x, train_y)
  
  # Make predictions
  predictions <- predict(model, test_x)
  
  # Evaluate the model
  results <- postResample(predictions, test_y)
  
  # Print results
  cat("\nResults for", model_name, ":\n")
  print(results)
  
  # Return model and results
  list(model = model, results = results)
}

# Define XGBoost training function (no scaling required)
train_xgboost <- function(train_x, train_y) {
  dtrain <- xgb.DMatrix(data = as.matrix(train_x), label = train_y)
  params <- list(objective = "reg:squarederror", eta = 0.1, max_depth = 6)
  xgb.train(params = params, data = dtrain, nrounds = 100)
}

# Define Linear Regression training function (scaling recommended)
train_lm <- function(train_x, train_y) {
  lm(as.formula(paste("TotalTransactions ~ .")), data = cbind(train_x, TotalTransactions = train_y))
}

# Evaluate XGBoost Model (without scaling)
xgboost_results <- evaluate_model(train_set, test_set, train_xgboost, "XGBoost", scale_data = FALSE)

# Evaluate Linear Regression Model (with scaling)
lm_results <- evaluate_model(train_set, test_set, train_lm, "Linear Regression", scale_data = TRUE)



#NOTES:
  
  # Graph scatter plot at this stage to include predictors that respect the 5 linear regression assumptions
  # in order to choose inputs for GLM?
  
  # Implement automatic backward elimination function
  
  # lm() -> does it really handle itself scaling?
  # lm() also takes care of categorical onehot encoding and creates dummy variables. 
  # We only have to  define the categorical variable as factor and the function will understand how to handle it

  # Dimension reduction: PCA (have to scale/standardize before), Heatmaps, t-SNE plots, Multidimensional Scaling (MDS) plots


# Boxplot after scaling 
# data_scaled %>%
#   select(where(function(x) is.numeric(x)), -c(TotalTransactions, BlockNumber, HourOfDay, DayOfWeek, IsWeekend)) %>%  # Select only numeric columns
#   pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%  # Reshape data
#   ggplot(aes(x = "", y = Value)) +  # Setup axes
#   geom_boxplot() +  # Create boxplots
#   facet_wrap(~ Variable, scales = "free_y") +  # Separate plot for each variable with free y scale
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +  # Remove x-axis elements
#   labs(y = "Values", title = "Separate Boxplot for Each Non-Categorical Numeric Variables after Scaling")  # Label the y-axis and provide a title
# 
