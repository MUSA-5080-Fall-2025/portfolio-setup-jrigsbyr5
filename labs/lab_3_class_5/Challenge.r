challenge_data <- get_acs(
  geography = "county",
  state = "PA",
  variables = c(
    home_value = "B25077_001",      # YOUR TARGET
    total_pop = "B01003_001",       # Total population
    median_age = "B01002_001",      # Median age
    percent_college = "B15003_022", # Bachelor's degree or higher
    poverty_rate = "B17001_002"     # Population in poverty
  ),
  year = 2022,
  output = "wide"
)

## Do the regression
model1 <- lm(home_valueE ~ total_popE, median_ageE,percent_collegeE,data = challenge_data)
summary(model1)

## Do cross validation
set.seed(123)
n <- nrow(challenge_data)

# 70% training, 30% testing
train_indices <- sample(1:n, size = 0.7 * n)
train_data <- challenge_data[train_indices, ]
test_data <- challenge_data[-train_indices, ]

# Fit on training data only
model_train <- lm(home_valueE ~ total_popE, median_ageE,percent_collegeE,data = train_data)

# Predict on test data
test_predictions <- predict(model_train, newdata = test_data)

## RMSE
# Calculate prediction error (RMSE)
rmse_test <- sqrt(mean((test_data$median_incomeE - test_predictions)^2))
rmse_train <- summary(model_train)$sigma

cat("Training RMSE:", round(rmse_train, 0), "\n")

cat("Test RMSE:", round(rmse_test, 0), "\n")

