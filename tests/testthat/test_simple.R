library(latex2exp)

test_that("Simple greek letter strings and symbols are rendered correctly", {
  expect_renders_same("$\\alpha$", alpha)
  expect_renders_same("$\\gamma$", gamma)
  expect_renders_same("$\\nabla$", nabla)
})

test_that("Simple text is rendered correctly", {
  expect_renders_same("Some simple text", paste("Some simple text"))
})