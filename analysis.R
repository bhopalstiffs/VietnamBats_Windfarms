################################################################################
# Set up
################################################################################

par(family="serif", las=1, bty="l",
    cex.axis=1, cex.lab=1, cex.main=1,
    xaxs="i", yaxs="i", mar = c(5, 5, 3, 5))

library(rstan)
library(tidyverse)
rstan_options(auto_write = TRUE)            # Cache compiled Stan programs
options(mc.cores = parallel::detectCores()) # Parallelize chains
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
setwd("~/VietBats")

util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)

################################################################################
# Data
################################################################################

knots <- c(2, 4, 7, 12, 14)  # Knots at 5 PM, 7 PM, 10 PM, 4 AM, and 5 AM

data <- list(
  N = nrow(dat2),
  hour_of_night = as.integer(dat2$hour.ord),
  activity = dat2$act,
  K = length(knots),
  knots = knots
)

data1 <- list(
  N = nrow(dat2),
  hour_of_night = dat2$hour.ord,
  activity = as.integer(dat2$act)
)

################################################################################
# Simple Regression on Wind Speed
################################################################################

fit1 <- stan(file="stan_programs/hour.stan",
            data=data1, seed=8438338,
            warmup=500, iter=2024, refresh=0)

# Computational diagnostics
diagnostics <- util$extract_hmc_diagnostics(fit)
util$check_all_hmc_diagnostics(diagnostics)

samples <- util$extract_expectand_vals(fit)
base_samples <- util$filter_expectands(samples,
                                       c('alpha', 'beta', 
                                         'spline_basis'),
                                       check_arrays=TRUE)
util$check_all_expectand_diagnostics(base_samples)

# Extract posterior samples
posterior_samples <- extract(fit)

# Get posterior predictions (y_pred) for each observation
y_pred <- posterior_samples$y_pred

# Get posterior samples for the spline coefficients (beta)
beta_samples <- posterior_samples$beta


# Create a grid of hour values for predictions (transformed)
hour_grid <- seq(2, 14, by = 0.1)

# Create spline basis for the hour grid (based on your knots)
# We'll calculate the spline basis at each hour in the grid
spline_basis_grid <- matrix(0, nrow = length(hour_grid), ncol = length(knots))

for (i in 1:length(hour_grid)) {
  for (k in 1:length(knots)) {
    spline_basis_grid[i, k] <- max(0, hour_grid[i] - knots[k])^3  # Cubic B-spline basis
  }
}

# Calculate posterior predictions for each value in the hour grid
posterior_predictions <- matrix(NA, nrow = nrow(beta_samples), ncol = length(hour_grid))

for (i in 1:nrow(beta)) {
  # Use the dot product of the spline basis and spline coefficients for each posterior sample
  posterior_predictions[i, ] <- beta_samples[i, ] %*% t(spline_basis_grid)
}


mean_prediction <- apply(posterior_predictions, 2, mean)
lower_95 <- apply(posterior_predictions, 2, function(x) quantile(x, 0.025))
upper_95 <- apply(posterior_predictions, 2, function(x) quantile(x, 0.975))

plot_data <- data.frame(
  hour = hour_grid,
  mean_activity = mean_prediction,
  lower_95 = lower_95,
  upper_95 = upper_95
)

ggplot(plot_data, aes(x = hour)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = mean_activity), color = "blue", size = 1) +
  labs(
    x = "Hour of Night",
    y = "Predicted Bat Activity",
    title = "Posterior Predictions for Bat Activity"
  ) +
  theme_minimal()

library(cowplot)

# Plot the posterior distributions of each spline coefficient
beta_plots <- lapply(1:ncol(beta_samples), function(i) {
  ggplot(data.frame(beta = beta_samples[, i]), aes(x = beta)) +
    geom_density(fill = "lightblue") +
    labs(title = paste("Posterior of Beta", i))
})

# Arrange the plots
plot_grid(plotlist = beta_plots, ncol = 2)

# Rhat statistics
summary(fit)$summary[,"Rhat"]

# Extract observed data and posterior predictions
observed_data <- dat2$act  # observed bat activity counts
predicted_data <- posterior_samples$y_pred  # posterior predictions


# Convert to a long format for plotting
predicted_data_long <- data.frame(
  y_pred = as.vector(predicted_data),
  observation = rep(1:length(observed_data), each = nrow(predicted_data))
)

ggplot(predicted_data_long, aes(x = y_pred)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_vline(xintercept = mean(observed_data), color = "red", linetype = "dashed") +
  labs(
    x = "Posterior Predictive Values",
    y = "Density",
    title = "Posterior Predictive Checks"
  )

library(coda)
# Extract posterior samples as a coda object
fit_coda <- as.mcmc(fit)

# Pair plots (or scatter plots) for parameters
pairs(fit_coda)

# Trace plots for key parameters
rstan::traceplot(fit, pars = c("alpha", "sigma", "beta"))

rstan::stan_trace(fit, pars = c("spline_basis"))

# Check for divergences
divergent_samples <- sum(fit@sim$divergent__ == 1)
cat("Number of divergent samples:", divergent_samples, "\n")


