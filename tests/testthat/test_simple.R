library(latex2exp)

test_that("Simple greek letter strings and symbols are rendered correctly", {
  expect_renders_same("$\\alpha$", alpha)
  expect_renders_same("$\\gamma$", gamma)
  expect_renders_same("$\\nabla$", nabla)
  expect_renders_same("$\\nabla$", nabla)
})

test_that("Simple text outside of math mode is rendered correctly", {
  expect_renders_same("Some simple text", paste("Some simple text"))
  expect_renders_same("a + b", paste("a + b"))
  expect_renders_same("$a + b$", a+b)
})

test_that("Operators are rendered correctly, regardless of spacing", {
  expect_renders_same("$a+b$",
                      a + b)
  expect_renders_same("$a +b$",
                      a + b)
  expect_renders_same("$a+ b$",
                      a + b)
  expect_renders_same("$a_{b+1}$",
                      a[b+1])
})

test_that("Superscripts and subscripts are rendered correctly", {
  expect_renders_same("$\\alpha^\\beta$", 
                      alpha^beta)
  expect_renders_same("$\\alpha_\\beta$", 
                      alpha[beta])
  expect_renders_same("$\\alpha_{\\gamma\\beta}$", 
                      alpha[gamma*beta])
  expect_renders_same("$\\A_\\gamma\\beta$", 
                      A[gamma]*beta)
})

test_that("Superscript and subscript for operators are rendered correctly", {
  expect_renders_same("$\\sum_{i=1}^{N} x_i$",
                      sum(x[i], i==1, N))
  
  expect_renders_same("$\\lim_{x \\to 0} \\frac{x^2}{x}$",
                      lim(frac(x^2, x) * phantom(.), x %->% 0))
})

test_that("Round parentheses are rendered correctly", {
  expect_renders_same("$\\frac{\\sin(x)}{\\cos(x)}$",
                      frac(sin(x), cos(x)) * phantom(.))
})

test_that("Opening and closing math mode renders correctly", {
  expect_renders_same("$\\alpha^\\beta$ and $\\gamma$",
                      paste(alpha^beta, ' and ', gamma))
})

test_that("User-defined latex renders correctly", {
  expect_renders_same(TeX("$\\mycommand$", user_defined = list(
    "\\mycommand" = "alpha"
  )), alpha)
})