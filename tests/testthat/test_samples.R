# This file contains samples of latex2exp uses
# gathered from GitHub.

test_that("Equation samples render correctly", {
  expect_renders_same("$P(\\hat{Y}) = \\frac{odds}{1+odds}$",
                      P(hat(Y)) == frac(odds, 1+odds) * phantom(.))
  
  expect_renders_same("$P(y_{ij} = 1)=1$ when $y_{ij}$ is 0",
                      paste(P(y[ij] == 1) == 1, ' when ', y[ij], ' is 0'))
})
  