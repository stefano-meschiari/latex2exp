library(latex2exp)

test_that("LaTeX examples are rendered correctly", {
  expect_true(latex2exp_examples())
})
