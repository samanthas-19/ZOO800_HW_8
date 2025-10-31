##Created by Samantha Summerfield
##10/30/25 HW9

##I did use chatgpt to help debug the code (combining the data simulations and making looped coin flips)



###-----------------------Objective 1------------------###


####----A----####

#load libraries
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 100

# Parameters for the linear regression, choose these numbers randomly, 
# but made sure they worked mathmatically in the slope equation
alpha <- 10     # intercept
beta <- 5      # slope
sigma <- 10     # standard deviation of the error term

# Generate predictor variable x (0, 10)
x <- runif(n, min = 0, max = 10)

# Generate normally distributed errors epsilon
epsilon <- rnorm(n, mean = 0, sd = sigma)

# put it all together in the equation
y <- alpha + beta * x + epsilon

# Combine into a data frame
sim_data <- data.frame(x = x, y = y)

# Check the range of y (should fall roughly between -100 and +100) and x (should fall between 0-10)
range(y)
range(x)


####----B----####

##need to remake the standard deviations for 1, 10, 25 in the equation
##then plot in multipanel figure

##------this is for std 1------##
sigma1 <- 1     # standard deviation of the error term

# Generate normally distributed errors epsilon
epsilon1 <- rnorm(n, mean = 0, sd = sigma1)

# put it all together in the equation
y1 <- alpha + beta * x + epsilon1

# Combine into a data frame
sim_data1 <- data.frame(x = x, y = y1)



##-----this is for std 25-----##
sigma25 <- 25     # standard deviation of the error term

# Generate normally distributed errors epsilon
epsilon25 <- rnorm(n, mean = 0, sd = sigma25)

# put it all together in the equation
y25 <- alpha + beta * x + epsilon25

# Combine into a data frame
sim_data25 <- data.frame(x = x, y = y25)

##-----now we plot-----##

# --- Combine all datasets ---
sim_data1 <- sim_data1 %>% mutate(sigma = "σ = 1")
sim_data  <- sim_data  %>% mutate(sigma = "σ = 10")
sim_data25 <- sim_data25 %>% mutate(sigma = "σ = 25")

# Combine into one dataframe
sim_all <- bind_rows(sim_data1, sim_data, sim_data25)

# --- Multipanel plot using facet_wrap ---
ggplot(sim_all, aes(x = x, y = y)) +
  geom_point(color = "purple", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  facet_wrap(~ sigma, nrow = 1) +
  labs(
    title = "Simple Linear Regression Trends with Increasing Error",
    x = "Predictor (x)",
    y = "Response (y)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


###-----------------------Objective 2------------------###

####----A----####

set.seed(123)

# Function to simulate unfair coin detection for given n flips and true p
detect_unfairness <- function(p_true, n_flips, n_sims = 100, alpha = 0.05) {
  # Run n_sims experiments
  results <- replicate(n_sims, {
    heads <- rbinom(1, n_flips, p_true)   # number of heads in n_flips
    test <- binom.test(heads, n_flips, p = 0.5, alternative = "greater")
    test$p.value < alpha                   # TRUE if we detect unfairness
  })
  mean(results)  # proportion of significant results
}

# Variables
p_true <- 0.55
n_flips <- 1:20
n_sims <- 100

# Run simulation for each number of flips
prob_detected <- sapply(n_flips, detect_unfairness, p_true = p_true, n_sims = n_sims)

# Combine results into a data frame
sim_data <- data.frame(
  n_flips = n_flips,
  prob_detected = prob_detected
)

# --- Plot results ---
ggplot(sim_data, aes(x = n_flips, y = prob_detected)) +
  geom_line(color = "purple", linewidth = 1.2) +
  geom_point(color = "darkblue", size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Probability of Coin Unfairness (p = 0.55, α = 0.05)",
    x = "Number of Coin Flips",
    y = "Simulations"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


####----B----####

# redefine variables
n_flips <- 1:20
p_values <- c(0.55, 0.6, 0.65)

# Run simulation for each combination
sim_results <- expand.grid(p = p_values, n = n_flips) %>%
  rowwise() %>%
  mutate(prob_detected = detect_unfairness(p, n, n_sims = 100)) %>%
  ungroup()

# --- Plot results ---
ggplot(sim_results, aes(x = n, y = prob_detected, color = as.factor(p))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Probability of Coin Unfairness (α = 0.05)",
    x = "Number of Coin Flips",
    y = "Simulations",
    color = "True p(heads)"
  ) +
  theme_minimal(base_size = 14) +
  ylim(0, 1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )










