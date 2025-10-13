# R/knapsack_algos.R
# Knapsack solvers: brute-force (optionally parallel), dynamic programming, greedy heuristic
# Dependencies: base R + parallel + foreach

library(parallel)
library(foreach)

#' Brute-force knapsack solver
#'
#' @param x data.frame with columns w (weights, positive integers) and v (values, positive numerics)
#' @param W numeric capacity (positive)
#' @param parallel logical; if TRUE attempt to use multiple cores (detects available cores)
#' @return list(value = numeric total value, elements = integer vector of original indices chosen)
#' @examples
#' x <- data.frame(w = c(2,3,4), v = c(3,4,5)); brute_force_knapsack(x, 5)
#' @export
brute_force_knapsack <- function(x, W, parallel = FALSE) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x)) || any(x$w <= 0) || any(x$v <= 0)) {
    stop("Input x must be a data frame with positive 'w' and 'v' columns")
  }
  if (!is.numeric(W) || W <= 0) stop("W must be a positive number")

  n <- nrow(x)
  max_value <- 0
  best_elements <- NULL

  if (parallel){
    cores <- parallel::detectCores()
    cl <- makeCluster(cores)
    registerDoParallel(cl)

    results <- foreach(i = 0:(2^n - 1), .combine = 'c') %dopar% {
      combination <- as.logical(intToBits(i)[1:n])
      total_weight <- sum(x$w[combination])
      total_value <- sum(x$v[combination])
      if (total_weight <= W) list(list(value = total_value, elements = which(combination))) else NULL
    }

    stopCluster(cl)

    if (length(results) > 0) {
      values <- sapply(results, function(r) r$value)
      idx <- which.max(values)
      max_value <- results[[idx]]$value
      best_elements <- results[[idx]]$elements
    }

  } else {
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

#' Dynamic programming knapsack solver
#'
#' @param x data.frame with columns w, v (weights should be integers or coercible)
#' @param W numeric capacity
#' @return list(value = numeric, elements = integer vector of selected original indices)
#' @examples
#' x <- data.frame(w = c(2,3,4), v = c(3,4,5)); knapsack_dynamic(x, 5)
#' @export
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

#' Greedy knapsack heuristic
#'
#' Note: greedy is approximate; it returns a feasible solution quickly.
#' @param x data.frame with columns w, v
#' @param W numeric capacity
#' @return list(value = numeric, elements = integer vector of original indices chosen)
#' @examples
#' x <- data.frame(w = c(2,3,4), v = c(3,4,5)); greedy_knapsack(x, 5)
#' @export
greedy_knapsack <- function(x, W) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% colnames(x)) || any(x$w <= 0) || any(x$v <= 0)) {
    stop("Input x must be a data frame with positive 'w' and 'v' columns")
  }
  if (!is.numeric(W) || W <= 0) stop("W must be a positive number")

  # Save original indices before sorting
  orig_idx <- seq_len(nrow(x))
  ord <- order(- (x$v / x$w))   # decreasing ratio
  x_sorted <- x[ord, , drop = FALSE]
  idx_sorted <- orig_idx[ord]

  total_value <- 0
  total_weight <- 0
  elements <- integer(0)

  for (i in seq_len(nrow(x_sorted))) {
    if (total_weight + x_sorted$w[i] <= W) {
      elements <- c(elements, idx_sorted[i])  # append original index
      total_value <- total_value + x_sorted$v[i]
      total_weight <- total_weight + x_sorted$w[i]
    }
  }

  elements <- sort(as.integer(elements))
  list(value = total_value, elements = elements)
}
