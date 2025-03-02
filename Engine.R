# Load necessary library
if (!requireNamespace("stats", quietly = TRUE)) {
  install.packages("stats")
}

# Black-Scholes Option Pricing Formula
black_scholes <- function(S, K, T, r, sigma, type = "call") {
  d1 <- (log(S/K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  # Compute option price
  if (type == "call") {
    price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else if (type == "put") {
    price <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  } else {
    stop("Invalid option type. Use 'call' or 'put'.")
  }
  
  return(price)
}

# Main function
main <- function() {
  # Define input parameters
  S <- 100   # Current stock price
  K <- 100   # Strike price
  T <- 1     # Time to maturity (in years)
  r <- 0.05  # Risk-free interest rate
  sigma <- 0.2  # Volatility
  
  # Compute option prices
  call_price <- black_scholes(S, K, T, r, sigma, "call")
  put_price <- black_scholes(S, K, T, r, sigma, "put")
  
  # Print results
  cat("Call Option Price:", round(call_price, 2), "\n")
  cat("Put Option Price:", round(put_price, 2), "\n")
}

# Run main function only if the script is executed (not sourced)
if (interactive()) {
  main()
}
