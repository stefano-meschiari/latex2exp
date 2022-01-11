library(latex2exp)

test_that("LaTeX examples are rendered correctly", {
  expect_silent(latex2exp_examples())
})

test_that("Famous equations are rendered correctly", {
  expect_renders_same("$\\bar{F} = m\\bar{a}", 
                      bar(F) == m * bar(a))
  
  expect_renders_same("$\\hat{H} \\Psi = E \\Psi$",
                      hat(H) * Psi == E * Psi)
  
  expect_renders_same("$\\frac{ih}{2\\pi}\\ \\frac{d}{dt} | \\Psi(t) \\rangle$",
                      hat(H) * Psi == E * Psi)
})