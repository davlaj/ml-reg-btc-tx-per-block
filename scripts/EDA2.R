# EDA

# Correlation Analysis - For Numeric Variables
correlation_analysis <- function(data) {
  numeric_data <- select(data, where(is.numeric))
  corr_matrix <- cor(numeric_data, use = "complete.obs") # Handling missing values
  pdf("output/exploratory-data-analysis/correlation_analysis.pdf")
  corrplot(corr_matrix, method = "circle")
  dev.off()
}
correlation_analysis(data)

# ANOVA or Chi-Square - For Categorical Variables vs Numeric Response
anova_chi_square <- function(data, categorical_var, response_var) {
  # Assuming response_var is numeric and categorical_var is factor
  data[[categorical_var]] <- as.factor(data[[categorical_var]])
  aov_result <- aov(data[[response_var]] ~ data[[categorical_var]], data = data)
  pdf(paste0("output/exploratory-data-analysis/anova_", categorical_var, ".pdf"))
  plot(aov_result)
  dev.off()
  return(summary(aov_result))
}
