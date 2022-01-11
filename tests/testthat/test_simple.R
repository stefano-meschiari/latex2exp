library(latex2exp)

test_that("Simple greek letter strings are rendered correctly", {
  expect_plots_same("\\alpha")
})