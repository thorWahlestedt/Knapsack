
brute_force_knapsack <- function(x, W) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% names(x)) || any(x$w <= 0) || any(x$v <= 0) || W < 0) {
    stop("Invalid input.")
  }

  n <- nrow(x)
  best_value <- 0
  best_combination <- NULL

  # Loop through all possible combinations (from 1 to 2^n)
  for (i in 1:(2^n)) {
    included <- as.logical(intToBits(i)[1:n])
    total_weight <- sum(x$w[included])
    total_value <- sum(x$v[included])

    if (total_weight <= W && total_value > best_value) {
      best_value <- total_value
      best_combination <- which(included)
    }
  }

  return(list(value = best_value, elements = best_combination))
}

knapsack_dynamic <- function(x, W) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% names(x)) || any(x$w <= 0) || any(x$v <= 0) || W < 0) {
    stop("Invalid input.")
  }

  n <- nrow(x)
  dp <- matrix(0, n+1, W+1)

  for (i in 1:n) {
    for (w in 0:W) {
      if (x$w[i] > w) {
        dp[i+1, w+1] <- dp[i, w+1]
      } else {
        dp[i+1, w+1] <- max(dp[i, w+1], dp[i, w - x$w[i] + 1] + x$v[i])
      }
    }
  }

  best_value <- dp[n+1, W+1]
  w <- W
  elements <- c()

  for (i in n:1) {
    if (dp[i+1, w+1] != dp[i, w+1]) {
      elements <- c(elements, i)
      w <- w - x$w[i]
    }
  }

  return(list(value = best_value, elements = elements))
}

greedy_knapsack <- function(x, W) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% names(x)) || any(x$w <= 0) || any(x$v <= 0) || W < 0) {
    stop("Invalid input.")
  }

  # Sort items by value-to-weight ratio (descending)
  x <- x[order(x$v / x$w, decreasing = TRUE), ]

  total_value <- 0
  total_weight <- 0
  elements <- c()

  for (i in 1:nrow(x)) {
    if (total_weight + x$w[i] <= W) {
      total_weight <- total_weight + x$w[i]
      total_value <- total_value + x$v[i]
      elements <- c(elements, i)
    }
  }

  return(list(value = total_value, elements = elements))
}
