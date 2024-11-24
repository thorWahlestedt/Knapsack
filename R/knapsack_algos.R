library(parallel)

brute_force_knapsack <- function(x, W, parallel = FALSE) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x)) || any(x$w <= 0) || any(x$v <= 0)) {
    stop("Input x must be a data frame with positive 'w' and 'v' columns")
  }
  if (!is.numeric(W) || W <= 0) stop("W must be a positive number")

  n <- nrow(x)
  max_value <- 0
  best_elements <- NULL

  if (parallel){
    cl <- makeCluster(2)
    registerDoParallel(cl)
    foreach(i=0:(2^n -1)) %dopar% {
      combination <- as.logical(intToBits(i)[1:n])
      total_weight <- sum(x$w[combination])
      total_value <- sum(x$v[combination])

      if (total_weight <= W && total_value > max_value) {
        max_value <- total_value
        best_elements <- which(combination)
      }
    }
    stopCluster(cl)
  }
  else{
    for (i in 0:(2^n - 1)) {
      combination <- as.logical(intToBits(i)[1:n])
      total_weight <- sum(x$w[combination])
      total_value <- sum(x$v[combination])

      if (total_weight <= W && total_value > max_value) {
        max_value <- total_value
        best_elements <- which(combination)
      }
    }
  }

  list(value = max_value, elements = best_elements)
}

knapsack_dynamic <- function(x, W) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x)) || any(x$w <= 0) || any(x$v <= 0)) {
    stop("Input x must be a data frame with positive 'w' and 'v' columns")
  }
  if (!is.numeric(W) || W <= 0) stop("W must be a positive number")

  n <- nrow(x)
  dp <- matrix(0, n + 1, W + 1)

  for (i in 1:n) {
    for (w in 0:W) {
      if (x$w[i] > w) {
        dp[i + 1, w + 1] <- dp[i, w + 1]
      } else {
        dp[i + 1, w + 1] <- max(dp[i, w + 1], dp[i, w + 1 - x$w[i]] + x$v[i])
      }
    }
  }

  max_value <- dp[n + 1, W + 1]
  w <- W
  elements <- numeric()

  for (i in n:1) {
    if (dp[i + 1, w + 1] != dp[i, w + 1]) {
      elements <- c(elements, i)
      w <- w - x$w[i]
    }
  }

  list(value = max_value, elements = elements)
}

greedy_knapsack <- function(x, W) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x)) || any(x$w <= 0) || any(x$v <= 0)) {
    stop("Input x must be a data frame with positive 'w' and 'v' columns")
  }
  if (!is.numeric(W) || W <= 0) stop("W must be a positive number")

  x <- x[order(-x$v / x$w), ]
  total_value <- 0
  total_weight <- 0
  elements <- numeric()

  for (i in 1:nrow(x)) {
    if (total_weight + x$w[i] <= W) {
      elements <- c(elements, i)
      total_value <- total_value + x$v[i]
      total_weight <- total_weight + x$w[i]
    }
  }

  list(value = total_value, elements = elements)
}
