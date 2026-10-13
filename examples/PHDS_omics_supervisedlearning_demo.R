# Load necessary library
library(ggplot2)
library(tidyr)
library(MASS)
library(dplyr)
library(pls)
library(caret)
library(mixOmics)
library(gridExtra)
library(Metrics)
library(glmnet)
library(reshape2)
library(here)

# Figures are saved to <project root>/figures (the folder is created if needed)
figure_directory = here("output", "figures")
dir.create(figure_directory, showWarnings = FALSE)


# Set the seed for reproducibility
set.seed(123)

# Simulate some data
n <- 100  # Number of observations

# Create two highly correlated predictors
x1 <- rnorm(n)
x2 <- x1 + rnorm(n, sd = 0.1)  # x2 is almost a perfect linear combination of x1

# Correlation between the predictors
cor(x1, x2)

# Create a response variable
y <- 3 + 2 * x1 + rnorm(n)

# form dataframe
data = cbind(y, x1, x2)
data %>% head()

p1 = data %>% ggplot(aes(x = x1, y = y)) + 
  geom_point() + 
  labs(title = "X1 vs Y", x = "X1", y = "Y") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

p2 = data %>% ggplot(aes(x = x2, y = y)) + 
  geom_point() + 
  labs(title = "X2 vs Y", x = "X2", y = "Y") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

p1
p2

ggsave(plot = p1, 
       path = figure_directory,
       filename = "lm_demo1.png",
       units = "cm", width = 15, height = 15)

ggsave(plot = p2, 
       path = figure_directory,
       filename = "lm_demo2.png",
       units = "cm", width = 15, height = 15)

# Fit a linear regression model
model <- lm(y ~ x1 + x2)

# Display the summary of the regression model
summary(model)

# Now demonstrate that linear regression breaks in high dimensions

n <- 30     # observations
p <- 100    # total predictors
k_signal <- 10  # number of true predictors

# --- generate 1 latent predictor ---
latent <- rnorm(n)

# --- create 10 highly correlated predictors by perturbing the latent ---
X_signal <- sapply(1:k_signal, function(i) latent + rnorm(n, sd = 0.3))

# --- generate 90 independent noise predictors ---
X_noise <- matrix(rnorm(n * (p - k_signal)), nrow = n, ncol = p - k_signal)

# --- combine predictors ---
X <- cbind(X_signal, X_noise)
colnames(X) <- paste0("X", 1:p)

# --- create response using only the first 10 predictors ---
beta <- c(rep(2, k_signal), rep(0, p - k_signal))
y <- 3 + X %*% beta + rnorm(n, sd = 1)

# --- form data frame ---
data <- data.frame(y = as.vector(y), X)

# --- correlation matrix ---
corr_mat <- cor(data)

# --- melt for ggplot ---
corr_long <- melt(corr_mat, varnames = c("Var1", "Var2"), value.name = "Correlation")

# --- plot heatmap ---
p3 = ggplot(corr_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1,1)) +
  theme_minimal(base_size = 25) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),) +
  labs(title = "Correlation Heatmap", fill = "Correlation")

p3

ggsave(plot = p3, 
       path = figure_directory,
       filename = "lm_demo3.png",
       units = "cm", width = 40, height = 35)

model <- lm(y ~., data = data)

summary(model)

# Regularised methods

# Ridge regression

# Simulate 2 dimensional data to demonstrate how regularisation shrinks the coefficients

# --- Simulate data ---
n <- 50
x <- rnorm(n, mean = 0, sd = 5)
y <- 3 + 2 * x + rnorm(n, sd = 5)
data <- data.frame(x = x, y = y)

# --- Fit OLS ---
ols_fit <- lm(y ~ x, data = data)
ols_coef <- coef(ols_fit)

# --- Fit Ridge manually ---
lambda <- 500
X <- cbind(1, x)
I <- diag(ncol(X))
I[1,1] <- 0  # do not penalize intercept
ridge_coef <- solve(t(X) %*% X + lambda * I) %*% t(X) %*% y
ridge_coef <- as.vector(ridge_coef)

# --- Plot 1: OLS only ---
p4 <- ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = ols_coef[1], slope = ols_coef[2], color = "red", size = 1.5) +
  labs(title = "OLS Regression", x = "X", y = "Y") +
  theme_minimal(base_size = 25) + 
  theme(plot.title = element_text(hjust = 0.5))

lines_df <- data.frame(
  type = c("OLS", "Ridge"),
  intercept = c(ols_coef[1], ridge_coef[1]),
  slope = c(ols_coef[2], ridge_coef[2])
)

p5 <- ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_abline(data = lines_df[lines_df$type == "OLS", ],
              aes(intercept = intercept, slope = slope, color = type),
              size = 1.5, linetype = "dashed") +
  geom_abline(data = lines_df[lines_df$type == "Ridge", ],
              aes(intercept = intercept, slope = slope, color = type),
              size = 1.5, linetype = "solid") +
  scale_color_manual(values = c("OLS" = "red", "Ridge" = "blue")) +
  labs(title = "Ridge Regression vs OLS",
       x = "X", y = "Y", color = "Regression") +
  theme_minimal(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5))

p4
p5

ggsave(plot = p4, 
       path = figure_directory,
       filename = "regularisation_demo1.png",
       units = "cm", width = 30, height = 20)


ggsave(plot = p5, 
       path = figure_directory,
       filename = "regularisation_demo2.png",
       units = "cm", width = 30, height = 20)


# Ridge regression on high-dimensional, multicollinear data

n <- 30
p <- 100
k_signal <- 10

# --- latent predictor ---
latent <- rnorm(n)

# --- 10 highly correlated predictors ---
X_signal <- sapply(1:k_signal, function(i) latent + rnorm(n, sd = 0.3))

# --- 90 independent noise predictors ---
X_noise <- matrix(rnorm(n * (p - k_signal)), nrow = n, ncol = p - k_signal)

# --- combine predictors ---
X <- cbind(X_signal, X_noise)
colnames(X) <- paste0("X", 1:p)

# --- create response ---
beta <- c(rep(2, k_signal), rep(0, p - k_signal))
y <- 3 + X %*% beta + rnorm(n, sd = 1)

# --- glmnet requires matrices ---
X_mat <- as.matrix(X)
y_vec <- as.vector(y)

# --- fit ridge regression (alpha = 0) ---
lambda <- 10  # fixed ridge penalty
ridge_fit <- glmnet(X_mat, y_vec, alpha = 0, lambda = lambda, standardize = TRUE)

# --- output coefficients like summary(model) ---
coef_ridge <- as.matrix(coef(ridge_fit))
coef_ridge

# Optional: convert to a data.frame for nicer display
ridge_df <- data.frame(
  Predictor = rownames(coef_ridge),
  Coefficient = coef_ridge[,1]
)
ridge_df

# Now increase the penalty term

lambda <- 200  # fixed ridge penalty
ridge_fit <- glmnet(X_mat, y_vec, alpha = 0, lambda = lambda, standardize = TRUE)

# --- output coefficients like summary(model) ---
coef_ridge <- as.matrix(coef(ridge_fit))
coef_ridge

# Optional: convert to a data.frame for nicer display
ridge_df <- data.frame(
  Predictor = rownames(coef_ridge),
  Coefficient = coef_ridge[,1]
)
ridge_df

# --- fit lasso regression
lambda <- 1  # fixed lasso penalty
lasso_fit <- glmnet(X_mat, y_vec, alpha = 1, lambda = lambda, standardize = TRUE)

# --- output coefficients like summary(model) ---
coef_lasso <- as.matrix(coef(lasso_fit))
coef_lasso

# Optional: convert to a data.frame for nicer display
lasso_df <- data.frame(
  Predictor = rownames(coef_lasso),
  Coefficient = coef_lasso[,1]
)
lasso_df

# Now increase the penalty

lambda <- 14  # fixed lasso penalty
lasso_fit <- glmnet(X_mat, y_vec, alpha = 1, lambda = lambda, standardize = TRUE)

# --- output coefficients like summary(model) ---
coef_lasso <- as.matrix(coef(lasso_fit))
coef_lasso

# Optional: convert to a data.frame for nicer display
lasso_df <- data.frame(
  Predictor = rownames(coef_lasso),
  Coefficient = coef_lasso[,1]
)
lasso_df

# Elastic net 

# --- fit en regression
lambda <- 5  # fixed en penalty
en_fit <- glmnet(X_mat, y_vec, alpha = 0.5, lambda = lambda, standardize = TRUE)

# --- output coefficients like summary(model) ---
coef_en <- as.matrix(coef(en_fit))
coef_en

# Optional: convert to a data.frame for nicer display
en_df <- data.frame(
  Predictor = rownames(coef_en),
  Coefficient = coef_en[,1]
)
en_df


# Step 1: Principal Component Analysis (PCA) using prcomp()
X = cbind(x1, x2)

pca <- prcomp(X, scale. = TRUE)  # X is the matrix of predictors

# Check the principal components
summary(pca)

# Step 2: View the loadings (relationship between original variables and principal components)
pca$rotation

# Step 3: Get the principal components (scores)
pc_scores <- pca$x

data_pca = cbind(data, pc_scores) %>% as.data.frame()

data_pca %>% head()
cor(data_pca$PC1, data_pca$PC2)

# Step 4: Perform linear regression using the principal components
# You can use the first few components, here we use the first 2 components
pcr_model_manual <- lm(y ~ PC1 + PC2, data = data_pca)  # Adjust the number of components as needed

# Step 5: Check the summary of the PCR model
summary(pcr_model_manual)

p3 = data_pca %>% ggplot(aes(x = PC1, y = y)) + 
  geom_point() + 
  labs(title = "PC1 vs Y", x = "PC1", y = "Y") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

p4 = data_pca %>% ggplot(aes(x = PC2, y = y)) + 
  geom_point() + 
  labs(title = "PC2 vs Y", x = "PC2", y = "Y") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

p3
p4

ggsave(plot = p3, 
       path = figure_directory,
       filename = "PLS_demo3.png",
       units = "cm", width = 15, height = 15)

ggsave(plot = p4, 
       path = figure_directory,
       filename = "PLS_demo4.png",
       units = "cm", width = 15, height = 15)


# PLS

# Number of observations
n <- 100

# Simulate the predictors
x1 <- rnorm(n)  # Predictor with high covariance with Y
x2 <- rnorm(n)

# Create the response variable Y
# Y has a strong relationship with x1 (coefficient = 5) and a weak one with x2 (coefficient = 0.2)
y <- 1.3 * x1 + 0.4 * x2 + rnorm(n, sd = 1)

data = cbind(y, x1, x2) %>% as.data.frame()

# Check covariance between Y and predictors
cor(y, x1)  # High covariance expected
cor(y, x2)  # Low covariance expected
cor(x1,x2)

# Fit a linear model to check the coefficients
model <- lm(y ~ x1 + x2)
summary(model)

p5 = data %>% ggplot(aes(x = x1, y = y)) + 
  geom_point() + 
  labs(title = "x1 vs Y", x = "x1", y = "Y") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

p6 = data %>% ggplot(aes(x = x2, y = y)) + 
  geom_point() + 
  labs(title = "x2 vs Y", x = "x2", y = "Y") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))


p5
p6

ggsave(plot = p5, 
       path = figure_directory,
       filename = "PLS_demo5.png",
       units = "cm", width = 15, height = 15)

ggsave(plot = p6, 
       path = figure_directory,
       filename = "PLS_demo6.png",
       units = "cm", width = 15, height = 15)

X = data[,-1] %>% as.matrix()

# Fit the PLS regression model
pls_model <- plsr(y ~ X, ncomp = 2, validation = "none", scale =T, center = T, model = F) 

# Extract weights
weights <- pls_model$loadings

# Transform the original data X into latent variables
X_centered <- scale(X, center = TRUE, scale = TRUE)

# Calculate latent variables using the centered and scaled data
latent_variables = X  %*% weights

# Display the results
latent_variables
weights

data_pls =  bind_cols(data, latent_variables)
colnames(data_pls)[c(4:5)] = c("LV1", "LV2")

data_pls %>% head()

cor(data_pls$x1, y)
cor(data_pls$x2, y)
cor(data_pls$LV1, y)
cor(data_pls$LV2, y)

p7 = data_pls %>% ggplot(aes(x = LV1, y = y)) + 
  geom_point() + 
  labs(title = "LV1 vs Y", x = "LV1", y = "Y") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

p8 = data_pls %>% ggplot(aes(x = LV2, y = y)) + 
  geom_point() + 
  labs(title = "LV2 vs Y", x = "LV2", y = "Y") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

p7
p8

ggsave(plot = p7, 
       path = figure_directory,
       filename = "PLS_demo7.png",
       units = "cm", width = 15, height = 15)

ggsave(plot = p8, 
       path = figure_directory,
       filename = "PLS_demo8.png",
       units = "cm", width = 15, height = 15)

pls_regression = lm(y ~ LV1 + LV2, data = data_pls)
summary(pls_regression)

pls_regression = lm(y ~ LV1, data = data_pls)
summary(pls_regression)


# Create a sequence of alpha_1 values
alpha1_values <- seq(-1, 1, by = 0.01)

# Initialize a data frame to store results
results <- data.frame(alpha1 = numeric(),alpha2 = numeric(), covariance = numeric())

# Loop through each alpha1 value to calculate covariance
for (alpha1 in alpha1_values) {
  alpha2 <- sqrt(1 - alpha1^2)  # Calculate alpha2
  
  # Calculate LV1
  LV1 <- alpha1 * X[, 1] + alpha2 * X[, 2]
  
  # Calculate covariance with y
  cov_value <- cov(LV1, y)
  
  # Store results
  results <- rbind(results, data.frame(alpha1 = alpha1, alpha2 = alpha2, covariance = cov_value))
}

max_cov <- max(results$covariance)
max_alpha1 <- results$alpha1[which.max(results$covariance)]

# Plotting
p9 = ggplot(results, aes(x = alpha1, y = covariance)) +
  geom_line() +
  geom_hline(yintercept = max_cov, linetype = "dashed", color = "red", size = 1.2) +  # Add horizontal line
  geom_vline(xintercept = max_alpha1, linetype = "dotted", color = "blue", size = 1.4) +  # Optional: Add vertical line at max
  labs(title = "Alpha_1 vs. Covariance with y",
       x = expression(alpha[1]),
       y = expression(cov(LV[1], y))) +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white')) +
  annotate("text", x = max_alpha1 - 0.2, y = -0.8, label = paste("Alpha1 =", round(max_alpha1, 2)), 
           vjust = -1, color = "blue", size = 4) 

ggsave(plot = p9, 
       path = figure_directory,
       filename = "PLS_demo9.png",
       units = "cm", width = 25, height = 15)

# Prepare the training control for 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Model 1: Intercept only
# Number of folds for cross-validation
k <- 10

# Create a vector to hold RMSEP values for each model
rmsep_results <- numeric(3)

# Create a function to calculate RMSEP
calculate_rmsep <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Perform 10-fold cross-validation
folds <- cut(seq(1, nrow(data_pls)), breaks = k, labels = FALSE)

for (i in 1:k) {
  # Separate training and test data
  test_indices <- which(folds == i, arr.ind = TRUE)
  train_data <- data_pls[-test_indices, ]
  test_data <- data_pls[test_indices, ]
  
  # Model 1: Intercept only
  model_intercept <- lm(y ~ 1, data = train_data)
  pred_intercept <- predict(model_intercept, newdata = test_data)
  rmsep_results[1] <- rmsep_results[1] + calculate_rmsep(test_data$y, pred_intercept)
}

rmsep_intercept <-rmsep_results[1]

# Model 2: Intercept and LV1
model_lv1 <- train(y ~ LV1, data = data_pls, method = "lm", trControl = train_control)
rmsep_lv1 <- sqrt(model_lv1$results$RMSE)

# Model 3: Intercept, LV1, and LV2
model_lv1_lv2 <- train(y ~ LV1 + LV2, data = data_pls, method = "lm", trControl = train_control)
rmsep_lv1_lv2 <- sqrt(model_lv1_lv2$results$RMSE)

# Compile results into a data frame for easy viewing
rmsep_results <- data.frame(
  Model = c("Intercept", "Intercept + LV1", "Intercept + LV1 + LV2"),
  RMSEP = c(rmsep_intercept, rmsep_lv1, rmsep_lv1_lv2)
)

# Display the results
print(rmsep_results)

# Regularisation 

# Generate synthetic data
set.seed(123)
n <- 100
x <- seq(-3, 3, length.out = n)
y_true <- 1 + 2 * x - 0.5 * x^2  # True relationship
y <- y_true + rnorm(n, sd = 1)    # Add noise

# Create a new data frame for plotting
data <- data.frame(x = x, y = y)

# Create new data for testing generalization
new_x <- seq(-3, 3, length.out = 50)
new_y <- 2 * new_x + 0.5 * new_x^2 + rnorm(50, sd = 2)
new_data <- data.frame(x = new_x, y = new_y)

# Fit models
linear_model <- lm(y ~ x, data = data)
poly_model <- lm(y ~ poly(x, 2), data = data)
overfit_model <- lm(y ~ poly(x, 25), data = data)

# Predictions for training data
data$linear_pred <- predict(linear_model)
data$poly_pred <- predict(poly_model)
data$overfit_pred <- predict(overfit_model)

# Predictions for new data
new_data$linear_pred <- predict(linear_model, newdata = new_data)
new_data$poly_pred <- predict(poly_model, newdata = new_data)
new_data$overfit_pred <- predict(overfit_model, newdata = new_data)

# Define axis limits for consistent plotting
x_limits <- c(-5, 5)
y_limits <- c(-5, 5)

# Plotting pairs

# 1. Underfitting
p1_train <- ggplot(data) +
  geom_point(aes(x = x, y = y), color = "grey", size = 2) +  # Training data
  geom_line(aes(x = x, y = linear_pred), color = "blue", size = 1) +
  ggtitle("Linear Model (training)") +
  xlab("X") + ylab("Y") +
  xlim(x_limits) + ylim(y_limits) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))
  theme_minimal()

p1_new <- ggplot(new_data) +
  geom_point(aes(x = x, y = y), color = "blue", size = 2) +  # New data points
  geom_line(aes(x = new_x, y = linear_pred), color = "blue", size = 1) +
  ggtitle("Linear Model (testing)") +
  xlab("X") + ylab("Y") +
  xlim(x_limits) + ylim(y_limits) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

# 2. Good Fitting
p2_train <- ggplot(data) +
  geom_point(aes(x = x, y = y), color = "grey", size = 2) +  # Training data
  geom_line(aes(x = x, y = poly_pred), color = "green", size = 1) +
  ggtitle("Polynomial Model (training)") +
  xlab("X") + ylab("Y") +
  xlim(x_limits) + ylim(y_limits) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

p2_new <- ggplot(new_data) +
  geom_point(aes(x = x, y = y), color = "blue", size = 2) +  # New data points
  geom_line(aes(x = new_x, y = poly_pred), color = "green", size = 1) +
  ggtitle("Polynomial Model (testing)") +
  xlab("X") + ylab("Y") +
  xlim(x_limits) + ylim(y_limits) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

# 3. Overfitting
p3_train <- ggplot(data) +
  geom_point(aes(x = x, y = y), color = "grey", size = 2) +  # Training data
  geom_path(aes(x = x, y = overfit_pred), color = "red", size = 1) +  # Join all points
  ggtitle("High-Degree Polynomial Model (training)") +
  xlab("X") + ylab("Y") +
  xlim(x_limits) + ylim(y_limits) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

p3_new <- ggplot(new_data) +
  geom_point(aes(x = x, y = y), color = "blue", size = 2) +  # New data points
  geom_line(aes(x = new_x, y = overfit_pred), color = "red", size = 1) +
  ggtitle("High-Degree Polynomial Model (testing)") +
  xlab("X") + ylab("Y") +
  xlim(x_limits) + ylim(y_limits)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

p11 = grid.arrange(p1_train, p1_new)
p12 = grid.arrange(p2_train, p2_new)
p13 = grid.arrange(p3_train, p3_new)

p11
p12
p13

ggsave(plot = p11, 
       path = figure_directory,
       filename = "PLS_demo11.png",
       units = "cm", width = 35, height = 15)

ggsave(plot = p12, 
       path = figure_directory,
       filename = "PLS_demo12.png",
       units = "cm", width = 35, height = 15)

ggsave(plot = p13, 
       path = figure_directory,
       filename = "PLS_demo13.png",
       units = "cm", width = 35, height = 15)


## Lasso and ridge regression

# Step 1: Simulate training data (2 points)
set.seed(123)
train_data <- data.frame(
  x = c(1, 1.5),
  y = c(2, 4)
)

test_data<- data.frame(
  x = c(0,0.3,0.6,1.8,2.1,2.4,2.7),
  y = c(1, 1.2, 2.5, 5.2, 4, 3.6, 5)
)

# Step 2: Fit a linear model (least squares regression)
fit <- lm(y ~ x, data = train_data)

# Mean of x and y
x_mean <- mean(train_data$x)
y_mean <- mean(train_data$y)
lambda = 0.1

# Numerator and denominator for ridge regression slope
numerator <- sum((train_data$x - x_mean) * (train_data$y - y_mean))
denominator <- sum((train_data$x - x_mean)^2) + lambda

# Calculate ridge slope and intercept
beta_ridge <- numerator / denominator
intercept_ridge <- y_mean - beta_ridge * x_mean

# Step 5: Generate predictions for the ridge regression
x_seq <- seq(0, 3, by = 0.5)
ridge_preds <- intercept_ridge + beta_ridge * x_seq


# Soft-thresholding operator for Lasso
soft_threshold <- function(rho, lambda) {
  if (rho > lambda) {
    return(rho - lambda)
  } else if (rho < -lambda) {
    return(rho + lambda)
  } else {
    return(0)
  }
}

# Step 3: Coordinate descent to compute Lasso solution manually

lasso_coord_descent <- function(x, y, lambda, tol = 1e-6, max_iter = 1000) {
  n <- length(y)
  
  # Initialize beta and intercept
  beta <- 0
  intercept <- mean(y)  # Initialize intercept as mean of y
  prev_beta <- beta
  
  for (iter in 1:max_iter) {
    # Update the intercept (mean-centered)
    intercept <- mean(y - beta * x)
    
    # Update the slope (beta) using soft-thresholding
    rho <- sum(x * (y - intercept)) / sum(x^2)
    beta <- soft_threshold(rho, lambda / sum(x^2))
    
    # Check for convergence
    if (abs(beta - prev_beta) < tol) {
      break
    }
    
    prev_beta <- beta
  }
  
  return(list(intercept = intercept, beta = beta))
}

# Step 4: Apply manual Lasso computation
lambda <- 0.1  # regularization parameter
lasso_fit <- lasso_coord_descent(train_data$x, train_data$y, lambda)

# Get the intercept and slope
intercept_lasso <- lasso_fit$intercept
beta_lasso <- lasso_fit$beta

# Step 5: Generate predictions for the Lasso regression
x_seq <- seq(0, 3, by = 0.5)
lasso_preds <- intercept_lasso + beta_lasso * x_seq



# Step 4: Create the plot with training points, regression line, and testing points
p14 = ggplot() +
  # Training points in grey
  geom_point(data = train_data, aes(x = x, y = y), color = "red", size = 3) +
  # Regression line from training data
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "black", linetype = "dashed") +
  # Labels and theme adjustments
  labs(title = "Least Squares Regression Line",
       subtitle = "Red = training, blue = testing",
       x = "X",
       y = "Y") +
  xlim(-0.5,4) + 
  ylim(-0.5,6) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

p15 = ggplot() +
  # Training points in grey
  geom_point(data = train_data, aes(x = x, y = y), color = "red", size = 3) +
  # Regression line from training data
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "black", linetype = "dashed") +
  # Testing points in blue
  geom_point(data = test_data, aes(x = x, y = y), color = "blue", size = 3) +
  # Labels and theme adjustments
  labs(title = "Least Squares Regression Line",
       subtitle = "Red = training, blue = testing",
       x = "X",
       y = "Y") +
  xlim(-0.5,4) + 
  ylim(-0.5,6) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

p16 = ggplot() +
  # Training points in grey
  geom_point(data = train_data, aes(x = x, y = y), color = "red", size = 3) +
  # Regression line from training data
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "grey", linetype = "dashed") +
  # Testing points in blue
  geom_point(data = test_data, aes(x = x, y = y), color = "blue", size = 3) +
  geom_line(aes(x = x_seq, y = ridge_preds), color = "darkgreen", linetype = "solid") +
  # Labels and theme adjustments
  labs(title = "Ridge regression line",
       subtitle = "Red = training, blue = testing",
       x = "X",
       y = "Y") +
  xlim(-0.5,4) + 
  ylim(-0.5,6) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

# Step 7: Create the plot (similar to p16 but for Lasso)
p17 = ggplot() +
  # Training points in red
  geom_point(data = train_data, aes(x = x, y = y), color = "red", size = 3) +
  # Least squares regression line (grey dashed)
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "grey", linetype = "dashed") +
  # Testing points in blue
  geom_point(data = test_data, aes(x = x, y = y), color = "blue", size = 3) +
  # Lasso regression line (purple solid)
  geom_line(aes(x = x_seq, y = lasso_preds), color = "purple", linetype = "solid") +
  # Labels and theme adjustments
  labs(title = "Lasso Regression Line",
       subtitle = "Red = training, blue = testing",
       x = "X",
       y = "Y") +
  xlim(-0.5, 4) + 
  ylim(-0.5, 6) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill = 'white', color = 'white'))

p14
p15
p16
p17

ggsave(plot = p14, 
       path = figure_directory,
       filename = "PLS_demo14.png",
       units = "cm", width = 15, height = 11)

ggsave(plot = p15, 
       path = figure_directory,
       filename = "PLS_demo15.png",
       units = "cm", width = 15, height = 11)

ggsave(plot = p16, 
       path = figure_directory,
       filename = "PLS_demo16.png",
       units = "cm", width = 15, height = 11)

ggsave(plot = p17, 
       path = figure_directory,
       filename = "PLS_demo17.png",
       units = "cm", width = 15, height = 11)

# Demonstrating random associations

rm(list = ls())
