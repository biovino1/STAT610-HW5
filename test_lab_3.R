context("Check local linear regression function")
source("llr_functions.R")

n = 15

## a very simple regression model
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)

test_that("llr output has correct length", {
    expect_equal(length(llr(x, y, z, omega = 1)), length(z))
})


test_that("make_weight_matrix works on simple cases", {
    #' check that the output is a diagonal matrix, all elements are positive,
    #' and that weights are correct in cases where you know what output is
    
    mat <- llr(x, y, z, omega=1)
    expect_equal(nrow(mat), ncol(mat))
    expect_true(all(mat >= 0))
})

test_that("make_predictor_matrix works on simple cases", {
    #`` write tests to check that the dimensions are correct, the first column is
    #` all 1's, etc.
  
   mat <- llr(x, y, z, omega=1)
   expect_true(all(mat[, 1] == 1))
   expect_equal(nrow(mat), length(x))
})
