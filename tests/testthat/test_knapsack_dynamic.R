context("knapsack_dynamic")

suppressWarnings(RNGversion(min(as.character(getRversion()), "3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(gk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))
  expect_named(gk, c("value", "elements"))
})

test_that("Function rejects erroneous input", {
  expect_error(knapsack_dynamic("hej", 3500))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function returns correct results", {

  # For small n, use brute force as reference
  bf_8 <- brute_force_knapsack(knapsack_objects[1:8,], 3500)
  gk <- knapsack_dynamic(knapsack_objects[1:8,], 3500)
  expect_equal(round(gk$value), round(bf_8$value))
  expect_true(all(gk$elements %in% bf_8$elements))

  bf_12 <- brute_force_knapsack(knapsack_objects[1:12,], 3500)
  gk <- knapsack_dynamic(knapsack_objects[1:12,], 3500)
  expect_equal(round(gk$value), round(bf_12$value))
  expect_true(all(gk$elements %in% bf_12$elements))

  bf_8_W2000 <- brute_force_knapsack(knapsack_objects[1:8,], 2000)
  gk <- knapsack_dynamic(knapsack_objects[1:8,], 2000)
  expect_equal(round(gk$value), round(bf_8_W2000$value))
  expect_true(all(gk$elements %in% bf_8_W2000$elements))

  bf_12_W2000 <- brute_force_knapsack(knapsack_objects[1:12,], 2000)
  gk <- knapsack_dynamic(knapsack_objects[1:12,], 2000)
  expect_equal(round(gk$value), round(bf_12_W2000$value))
  expect_true(all(gk$elements %in% bf_12_W2000$elements))

  # Timing test for n = 16
  st <- system.time(gk <- knapsack_dynamic(knapsack_objects[1:16,], 2000))
  expect_true(as.numeric(st)[2] <= 0.01)

  # For larger n, just check consistency: total value matches sum of selected elements
  gk <- knapsack_dynamic(knapsack_objects[1:800,], 3500)
  expect_equal(round(gk$value), round(sum(knapsack_objects$v[gk$elements])))

  gk <- knapsack_dynamic(knapsack_objects[1:1200,], 3500)
  expect_equal(round(gk$value), round(sum(knapsack_objects$v[gk$elements])))
})
