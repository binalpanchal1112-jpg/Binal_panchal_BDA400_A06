# test_indicators.R - runs every function with the example data from the assignment
# Run from the folder that holds all the .R files:  Rscript test_indicators.R
for (f in c("sma", "ema", "macd", "stdev", "linreg", "rsi", "stoch_rsi", "crossover", "crossunder")) {
  source(paste0(f, ".R"))
}

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
cat("SMA (period 3):\n");  print(sma(data, 3))
cat("EMA (period 3):\n");  print(ema(data, 3))
cat("Stdev:\n");           print(stdev(data))

cat("MACD (3, 5, 2):\n")
print(macd(c(100, 105, 110, 115, 120, 125, 130), 3, 5, 2))

cat("Linear regression (length 5, offset 0):\n")
print(linreg(data, 5, 0))

prices <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
cat("RSI (period 5):\n");  print(rsi(prices, 5))
cat("StochRSI (period 3, k 2, d 2):\n"); print(stoch_rsi(prices, 3, 2, 2))
cat("StochRSI (assignment example, period 14):\n"); print(stoch_rsi(prices, 14, 3, 3))

arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
cat("Crossover:\n");  print(crossover(arr1, arr2))
cat("Crossunder:\n"); print(crossunder(arr1, arr2))

# ---- checks against R's built-in functions (only used for testing, not in the indicators) ----
ok <- function(name, cond) cat(sprintf("%-40s %s\n", name, if (isTRUE(cond)) "PASS" else "FAIL"))
cat("\nAutomatic checks:\n")
ok("sma matches stats::filter",       all.equal(sma(data, 3), as.numeric(stats::filter(data, rep(1/3, 3), sides = 1))[3:9]))
ok("stdev matches population sd",     all.equal(stdev(data), sqrt(mean((data - mean(data))^2))))
ok("ema first value equals data[1]",  ema(data, 3)[1] == data[1])
ok("ema period 3 last value",         all.equal(ema(data, 3)[9], 22.078125))
ok("macd histogram = line - signal",  { m <- macd(data, 3, 5, 2); all.equal(m$histogram, m$macd_line - m$signal_line) })
lr <- linreg(c(2, 4, 6, 8, 10), 5, 0)  # note: pseudocode indexing uses start = n - length, so check slope on a perfect line
ok("linreg slope on a straight line", all.equal(lr$slope, 2))
ok("rsi stays between 0 and 100",     all(rsi(prices, 5)[!is.na(rsi(prices, 5))] >= 0 & rsi(prices, 5)[!is.na(rsi(prices, 5))] <= 100))
ok("crossover length matches input",  length(crossover(arr1, arr2)) == length(arr1))
res <- tryCatch(sma(c(1, 2), 5), error = function(e) "error")
ok("sma errors when data too short",  identical(res, "error"))
res <- tryCatch(crossover(1:3, 1:4), error = function(e) "error")
ok("crossover errors on unequal length", identical(res, "error"))
ok("crossunder finds the swapped crossing", crossunder(arr2, arr1)[4] == "True")
