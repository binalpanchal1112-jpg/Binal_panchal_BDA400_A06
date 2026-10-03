# stoch_rsi.R - Stochastic RSI (StochRSI)
# This function uses rsi() and sma(), so rsi.R and sma.R must be loaded first.
if (!exists("rsi", mode = "function")) source("rsi.R")
if (!exists("sma", mode = "function")) source("sma.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI (ignore the NA values at the start of the RSI)
  valid <- rsi_values[!is.na(rsi_values)]
  if (length(valid) == 0) {
    k_values <- rep(NA_real_, length(rsi_values))
  } else {
    min_rsi <- min(valid)
    max_rsi <- max(valid)
    if (max_rsi == min_rsi) {
      k_values <- ifelse(is.na(rsi_values), NA_real_, 0)
    } else {
      k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
    }
  }

  # Calculate the %K line (StochRSI)
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  return(result)
}
