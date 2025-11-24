
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Knapsack

<!-- badges: start -->

[![R-CMD-check](https://github.com/thorWahlestedt/Knapsack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thorWahlestedt/Knapsack/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of Knapsack is to …

## Installation

You can install the development version of Knapsack like so:

``` r
This package implements several knapsack algorithms with different computational
complexities. It is created as part of the course work for demonstrating 
algorithmic efficiency and optimization in R.

## Installation

You can install the development version of **Knapsack** from GitHub using:

```r
# install.packages("devtools")
devtools::install_github("thorWahlestedt/Knapsack")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Knapsack)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
library(Knapsack)

items <- data.frame(
  w = c(12, 7, 11, 8, 9),
  v = c(24, 13, 23, 15, 16)
)

W <- 26

greedy_knapsack(items, W)
knapsack_dynamic(items, W)


```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
