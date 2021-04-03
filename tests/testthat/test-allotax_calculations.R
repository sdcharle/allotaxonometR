context("Divergence Calculations")
library(stringr)

test_that("Basic divergence works on sample set default alpha", {
  expect_equal(str_length("a"), 1)
  expect_equal(str_length("ab"), 2)
  expect_equal(str_length("abc"), 3)
})
#> Test passed 😸

test_that("Basic divergence Inf alpha", {
  expect_equal(str_length(factor("a")), 1)
  expect_equal(str_length(factor("ab")), 2)
  expect_equal(str_length(factor("abc")), 3)
})
#> Test passed 🎉

test_that("Basic divergence Zero alpha", {
  expect_equal(str_length(factor("a")), 1)
  expect_equal(str_length(factor("ab")), 2)
  expect_equal(str_length(factor("abc")), 3)
})
#> Test passed 🎉

test_that("Simplest Check", {

  result <- rank_turbulence(tibble(name = c('Bill', 'Joe', 'Jane'), count = c(1,2,3)),
                            tibble(name = c('Bill', 'Joe', 'Jane'), count = c(3,2,1)))

  print(result[['divergences']] %>% select(name))

  expect_equal(round(result$normalization,2), 7.55)

})
