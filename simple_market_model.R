#####################################################################################################
## See accompanying blog post:                                                                     ##
## https://blog.ephorie.de/can-a-simple-multi-agent-model-replicate-complex-stock-market-behaviour ##
#####################################################################################################

# For reproducibility
set.seed(2718)

# Initialize no. of trading days (5000 days -> about 20 years)
N <- 5000

# Initialize no. of traders
trader_count <- 500

traders <- data.frame(
  indicator = rep(1, trader_count),
  behavior = runif(trader_count),  # Random uniform values between 0 and 1
  color = rep(NA, trader_count)
)

# Set behavior and color based on random value
traders$behavior <- ifelse(traders$behavior < 0.5, 0, 1)
traders$color <- ifelse(traders$behavior == 0, "black", "yellow")

# Define global variables
global_vars <- list("log_price" = 0,
                    "last_price" = 0,
                    "return" = 0,
                    "F" = 0,
                    "orders_by_technical_rules" = 0,
                    "orders_by_fundamental_rules" = 0,
                    "orders_by_technical_rules2" = 0,
                    "orders_by_fundamental_rules2" = 0,
                    "orders_by_technical_rules3" = 0,
                    "orders_by_fundamental_rules3" = 0,
                    "weight_technical_traders" = 0,
                    "weight_fundamental_traders" = 0,
                    "fitness_technical_rules" = 0,
                    "fitness_fundamental_rules" = 0,
                    "fitness_technical_rules2" = 0,
                    "fitness_fundamental_rules2" = 0,
                    "K" = sum(traders$indicator[traders$behavior == 1]),
                    "K2" = 0,
                    "N" = sum(traders$indicator),
                    "alpha" = 0,
                    "beta" = 0,
                    "gamma" = 0,
                    "agent2_behavior" = 0,
                    "talks_done" = 0,
                    "probab_change_tech_fund" = 0,
                    "probab_change_fund_tech" = 0,
                    "transition_prob_tech_plus" = 0,
                    "transition_prob_tech_minus" = 0,
                    "transition_prob" = 0,
                    "random_no" = 0,
                    "a" = 1,
                    "b" = 0.05,
                    "c" = 0.02,
                    "d" = 0.95,
                    "epsilon" = 0.10,
                    "lambda" = 0.45,
                    "sigma_alfa" = 0.0025,
                    "sigma_beta" = 0.025,
                    "sigma_gamma" = 0.0025,
                    "talks_total" = 20)

# Define performance metrics
performance <- list("returns" = c(),
                    "log_prices" = c(),
                    "weights_technical_traders" = c())

# Define function for agent interaction
agent_talk_and_decision <- function(traders, global_vars) {
  
  talks_done <- 0
  
  while (global_vars$talks_total >= talks_done) {
    random_no <- runif(1, 0, 1)
    
    if (random_no < global_vars$transition_prob) {
      talks_done <- talks_done + 1
    } else {
      if (random_no >= global_vars$transition_prob && random_no < (global_vars$transition_prob + global_vars$transition_prob_tech_plus)) {
        patch_to_change <- sample(which(traders$behavior == 0), 1)
        traders$behavior[patch_to_change] <- 1
        traders$color[patch_to_change] <- "yellow"
        talks_done <- talks_done + 1
      } else {
        patch_to_change <- sample(which(traders$behavior == 1), 1)
        traders$behavior[patch_to_change] <- 0
        traders$color[patch_to_change] <- "black"
        talks_done <- talks_done + 1
      }
    }
  }
  
  list(traders = traders, global_vars = global_vars)
}

# Define function for market mechanism
market_clearing <- function(traders, global_vars) {
  global_vars$alpha <- rnorm(1, 0, global_vars$sigma_alfa)
  global_vars$beta <- rnorm(1, 0, global_vars$sigma_beta)
  global_vars$gamma <- rnorm(1, 0, global_vars$sigma_gamma)
  
  # updating order variables
  global_vars$orders_by_technical_rules3 <- global_vars$orders_by_technical_rules2
  global_vars$orders_by_technical_rules2 <- global_vars$orders_by_technical_rules
  global_vars$orders_by_technical_rules <- global_vars$b * (global_vars$log_price - global_vars$last_price) + global_vars$beta
  
  global_vars$orders_by_fundamental_rules3 <- global_vars$orders_by_fundamental_rules2
  global_vars$orders_by_fundamental_rules2 <- global_vars$orders_by_fundamental_rules
  global_vars$orders_by_fundamental_rules <- global_vars$c * (global_vars$F - global_vars$log_price) + global_vars$gamma
  
  # updating weights
  global_vars$K2 <- global_vars$K
  global_vars$K <- sum(traders$indicator[traders$behavior == 1])
  global_vars$weight_technical_traders <- global_vars$K / global_vars$N
  global_vars$weight_fundamental_traders <- (global_vars$N - global_vars$K) / global_vars$N
  
  # updating price
  global_vars$last_price <- global_vars$log_price
  global_vars$log_price <- global_vars$last_price + global_vars$a * (global_vars$orders_by_technical_rules * global_vars$weight_technical_traders + 
                                                                       global_vars$orders_by_fundamental_rules * global_vars$weight_fundamental_traders) + global_vars$alpha
  
  # calculating return
  if (global_vars$last_price == 0) {
    global_vars$return <- 0.0
  } else {
    global_vars$return <- global_vars$log_price - global_vars$last_price
  }
  
  # fitness rule calculations
  global_vars$fitness_technical_rules2 <- global_vars$fitness_technical_rules
  global_vars$fitness_fundamental_rules2 <- global_vars$fitness_fundamental_rules
  global_vars$fitness_technical_rules <- ((exp(global_vars$log_price) - exp(global_vars$last_price)) * global_vars$orders_by_technical_rules3 + global_vars$d * global_vars$fitness_technical_rules2)
  global_vars$fitness_fundamental_rules <- ((exp(global_vars$log_price) - exp(global_vars$last_price)) * global_vars$orders_by_fundamental_rules3 + global_vars$d * global_vars$fitness_fundamental_rules2)
  
  # probabilities that agents change their opinion and use different rules
  if (global_vars$fitness_technical_rules > global_vars$fitness_fundamental_rules) {
    global_vars$probab_change_fund_tech <- (0.5 + global_vars$lambda)
    global_vars$probab_change_tech_fund <- (0.5 - global_vars$lambda)
  } else {
    global_vars$probab_change_fund_tech <- (0.5 - global_vars$lambda)
    global_vars$probab_change_tech_fund <- (0.5 + global_vars$lambda)
  }
  
  # transition probabilities
  global_vars$transition_prob_tech_plus <- ((global_vars$N - global_vars$K2) / global_vars$N) * (global_vars$epsilon + global_vars$probab_change_fund_tech * (global_vars$K2 / (global_vars$N - 1)))
  global_vars$transition_prob_tech_minus <- (global_vars$K2 / global_vars$N) * (global_vars$epsilon + global_vars$probab_change_tech_fund * ((global_vars$N - global_vars$K2) / (global_vars$N - 1)))
  global_vars$transition_prob <- 1 - global_vars$transition_prob_tech_plus - global_vars$transition_prob_tech_minus
  
  list(traders = traders, global_vars = global_vars)
}

# Define function for storing performance metrics
do_report <- function(global_vars, performance) {
  performance$returns <- c(performance$returns, global_vars$return)
  performance$log_prices <- c(performance$log_prices, global_vars$log_price)
  performance$weights_technical_traders <- c(performance$weights_technical_traders, global_vars$weight_technical_traders)
  performance
}

# Run the simulation
start.time <- Sys.time()
for(ticks in 1:N) {
  if (ticks > 2) {
    results <- agent_talk_and_decision(traders, global_vars)
    traders <- results$traders
    global_vars <- results$global_vars
  }
  
  results <- market_clearing(traders, global_vars)
  traders <- results$traders
  global_vars <- results$global_vars
  
  performance <- do_report(global_vars, performance)
}
Sys.time() - start.time

returns <- performance$returns[-1]
logprices <- performance$log_prices
weights <- performance$weights_technical_traders

# Plot results
library(forecast)
library(laeken)

tailindex <- function(data) {
  len <- length(data)
  pc <- seq(5, len/10, 5)
  plot_data <- sapply(pc, thetaHill, x = data)
  plot(plot_data,type = "l", main = "", ylab = "tail index", xlab = "largest observations", xaxt = "n")
  axis(1, at = c(0, length(plot_data)/2, length(plot_data)), labels = c(0, 5, 10))
  paste("Tailindex at 5%:", round(thetaHill(data, len/20), 1))
}

plot(logprices, ylab = "log price", xlab = "time", type = "l")
abline(h = 0)

plot(returns, ylab = "return", xlab = "time", type = "l")
abline(h = 0)

plot(weights, ylab = "weights", xlab = "time", type = "l")
abline(h = 0.5)

plot(density(returns), main = "", ylab = "prob frequency", xlab = "return")
curve(dnorm(x, mean(returns), sd(returns)), col = "red", add = TRUE)
tailindex(abs(returns))

Acf(returns, lag.max = 100, main = "", ylab = "acf r", xlab = "lag")

Acf(abs(returns), lag.max = 100, main = "", ylab = "acf |r|", xlab = "lag")
