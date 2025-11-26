### Created by Samantha Summerfield

## HW 13
##I was still a bit confused about some of the linear regression methods since I had to miss class to travel 
##(but  watched the recorded lecture) so i used chatgpt to explain a bit more and clean up the code
##I am going back to read a gain to see if that helps

###--------------------------------Objective 1---------------------------###
### Analytical solution for linear regression:
###    area_burned = β0 + β1 * dragon_size
###
###    β̂ = (X'X)^(-1) X'y

## Load libraries (if needed)
library(dplyr)

## ---- Load your data ----

dragon_data <- read.csv("dragon_data.csv")

## Inspect data structure
str(dragon_data)

## Extract variables
y <- dragon_data$acres_on_fire          # response
x <- dragon_data$size         # predictor

## ---- Construct the design matrix X ----
## Column of 1s for intercept and predictor variable
X <- cbind(1, x)

## ---- Analytical solution: β̂ = (X'X)^(-1) X'y ----
XtX  <- t(X) %*% X
XtY  <- t(X) %*% y

beta_hat <- solve(XtX) %*% XtY

## Print results
colnames(beta_hat) <- "Estimate"
rownames(beta_hat) <- c("Intercept (β0)", "Slope (β1)")
beta_hat

#now save these as variable to use later
beta0_analytical <- beta_hat[1]
beta1_analytical <- beta_hat[2]

###--------------------------------Objective 2---------------------------###

### Estimate linear regression parameters using
### ordinary least squares (OLS)
###   area_burned = β0 + β1 * dragon_size

#define the columns in the data frame that was imported earlier
y <- dragon_data$acres_on_fire
x <- dragon_data$size
n <- length(y)


### 2A. OLS via GRID SEARCH (minimize SSE)

## Define coarse grids (nearest 0.1)
beta0_grid <- seq(-50, 50, by = 0.1)
beta1_grid <- seq(-10, 10, by = 0.1)

## Matrix to store SSE
SSE_grid <- matrix(NA, 
                   nrow = length(beta0_grid), 
                   ncol = length(beta1_grid))

## Loop through all combinations
for (i in seq_along(beta0_grid)) {
  for (j in seq_along(beta1_grid)) {
    
    b0 <- beta0_grid[i]
    b1 <- beta1_grid[j]
    
    y_pred <- b0 + b1 * x
    SSE <- sum((y - y_pred)^2)
    
    SSE_grid[i, j] <- SSE
  }
}

## Find best OLS parameters
best <- which(SSE_grid == min(SSE_grid), arr.ind = TRUE)

beta0_grid_OLS <- beta0_grid[best[1]]
beta1_grid_OLS <- beta1_grid[best[2]]

cat("OLS via Grid Search:\n")
cat("  β0 =", beta0_grid_OLS, "\n")
cat("  β1 =", beta1_grid_OLS, "\n\n")

### 2B. OLS via optim() (minimize SSE)


## Objective function: SSE
SSE_fn <- function(par) {
  b0 <- par[1]
  b1 <- par[2]
  y_pred <- b0 + b1 * x
  sum((y - y_pred)^2)   # OLS objective
}

## Use optim starting at (0,0)
opt_fit <- optim(par = c(0, 0), fn = SSE_fn)

beta0_optim_OLS <- opt_fit$par[1]
beta1_optim_OLS <- opt_fit$par[2]

cat("OLS via optim():\n")
cat("  β0 =", beta0_optim_OLS, "\n")
cat("  β1 =", beta1_optim_OLS, "\n")
cat("Convergence code =", opt_fit$convergence, " (0 = successful)\n\n")

### 2C. Convergence & Starting Value Sensitivity

start_vals <- list(
  c(-30, -5),
  c(-10, 10),
  c(20, -10),
  c(50, 10),
  c(0, 0)
)

results <- matrix(NA, nrow = length(start_vals), ncol = 3)
colnames(results) <- c("beta0", "beta1", "converged")

for (k in seq_along(start_vals)) {
  fit_k <- optim(par = start_vals[[k]], fn = SSE_fn)
  
  results[k, 1] <- fit_k$par[1]
  results[k, 2] <- fit_k$par[2]
  results[k, 3] <- fit_k$convergence
}

cat("Starting Value Sensitivity Test:\n")
print(results)


### --------------------------Objective 3---------------------------------###
### Maximum likelihood estimation (MLE) for linear regression
### Model: area_burned = β0 + β1 * dragon_size, errors ~ N(0, σ^2)

### 3A. MLE via GRID SEARCH

## Grids for β0 and β1 (to nearest 0.1)
beta0_grid <- seq(-50, 50, by = 0.1)
beta1_grid <- seq(-10, 10, by = 0.1)

## We will estimate σ analytically for each β0, β1:
## σ_hat = sqrt( SSE / n )

loglik_grid <- matrix(NA, nrow = length(beta0_grid), ncol = length(beta1_grid))

for (i in seq_along(beta0_grid)) {
  for (j in seq_along(beta1_grid)) {
    
    b0 <- beta0_grid[i]
    b1 <- beta1_grid[j]
    
    residuals <- y - (b0 + b1 * x)
    SSE <- sum(residuals^2)
    
    sigma_hat <- sqrt(SSE / n)
    
    ## log-likelihood
    loglik <- - (n/2)*log(2*pi*sigma_hat^2) - SSE/(2*sigma_hat^2)
    
    loglik_grid[i,j] <- loglik
  }
}

## Find maximum likelihood estimates
max_idx <- which(loglik_grid == max(loglik_grid), arr.ind = TRUE)

beta0_MLE_grid <- beta0_grid[max_idx[1]]
beta1_MLE_grid <- beta1_grid[max_idx[2]]

cat("MLE via Grid Search:\n")
cat("  β0 =", beta0_MLE_grid, "\n")
cat("  β1 =", beta1_MLE_grid, "\n\n")


### 3B. MLE via optim()

## Negative log-likelihood function
neg_loglik_fn <- function(par) {
  b0 <- par[1]
  b1 <- par[2]
  sigma <- par[3]
  
  ## enforce sigma > 0
  if (sigma <= 0) return(Inf)
  
  residuals <- y - (b0 + b1 * x)
  SSE <- sum(residuals^2)
  
  nll <- (n/2)*log(2*pi*sigma^2) + SSE/(2*sigma^2)
  return(nll)
}

## initial guess
start <- c(0, 0, sd(y))

opt_mle <- optim(par = start, fn = neg_loglik_fn)

beta0_MLE <- opt_mle$par[1]
beta1_MLE <- opt_mle$par[2]
sigma_MLE <- opt_mle$par[3]

cat("MLE via optim():\n")
cat("  β0 =", beta0_MLE, "\n")
cat("  β1 =", beta1_MLE, "\n")
cat("  σ  =", sigma_MLE, "\n")
cat("Convergence code =", opt_mle$convergence, " (0 = OK)\n\n")

### 3C. Sensitivity to Starting Values
start_vals <- list(
  c(-50, -5, 10),
  c(-10, 10, 5),
  c(30, -10, 20),
  c(50, 10, 30),
  c(0, 0, sd(y))
)


results <- matrix(NA, nrow = length(start_vals), ncol = 4)
colnames(results) <- c("beta0", "beta1", "sigma", "converged")

for (k in seq_along(start_vals)) {
  fit_k <- optim(par = start_vals[[k]], fn = neg_loglik_fn)
  
  results[k, 1] <- fit_k$par[1]
  results[k, 2] <- fit_k$par[2]
  results[k, 3] <- fit_k$par[3]
  results[k, 4] <- fit_k$convergence
}

cat("Starting Value Sensitivity Test (MLE):\n")
print(results)


### --------------------------Objective 4---------------------------------###
### Compare estimates from three approaches


comparison <- data.frame(
  Method = c("Analytical OLS", "Grid Search OLS", "MLE"),
  
  Beta0_Intercept = c(beta0_analytical,
                      beta0_grid_OLS,
                      beta0_MLE),
  
  Beta1_Slope = c(beta1_analytical,
                  beta1_grid_OLS,
                  beta1_MLE)
)

print(comparison)


###these all seem pretty similar. the only difference is the intercept with the grid search
