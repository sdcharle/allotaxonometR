context("Divergence Calculations")
library(stringr)

test_that("Basic divergence Inf alpha", {
  result <- rank_turbulence(tibble(name = c('Bill', 'Joe', 'Jane'), count = c(1,2,3)),
                            tibble(name = c('Bill', 'Joe', 'Jane'), count = c(3,2,1)),
                            Inf)

  expect_equal(round(result$normalization,2), 3.67)

})

test_that("Basic divergence Zero alpha", {
  result <- rank_turbulence(tibble(name = c('Bill', 'Joe', 'Jane'), count = c(1,2,3)),
                            tibble(name = c('Bill', 'Joe', 'Jane'), count = c(3,2,1)),
                            0)

  expect_equal(round(result$normalization,2), 5.44)
})
#> Test passed 🎉

test_that("Simplest Check", {

  result <- rank_turbulence(tibble(name = c('Bill', 'Joe', 'Jane'), count = c(1,2,3)),
                            tibble(name = c('Bill', 'Joe', 'Jane'), count = c(3,2,1)))

  expect_equal(round(result$normalization,2), 7.55)

})


# TODO - balances, summaries
