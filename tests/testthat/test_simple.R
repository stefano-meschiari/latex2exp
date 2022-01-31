library(latex2exp)

test_that("Returned values are of the correct type", {
  expect_s3_class(TeX("$\\alpha$"), "expression")
})

test_that("Simple greek letter strings and symbols are rendered correctly", {
  expect_renders_same("$\\alpha$", alpha)
  expect_renders_same("$\\gamma$", gamma)
  expect_renders_same("$\\nabla$", nabla)
})

test_that("Simple text outside of math mode is rendered correctly", {
  expect_renders_same("Some simple text", paste("Some simple text"))
  expect_renders_same("Some very, very simple text; with punctuation.", 
                      paste("Some very, very simple text; with punctuation."))
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
  
  # multiple equality signs, like a = b = c, can cause a parse error in expression();
  # wrap with phantom()
  expect_renders_same("$a = b = c$",
                      a * {phantom() == phantom()} * b * {phantom() == phantom()} * c)
  expect_renders_same("$a > b < c$",
                      a * {phantom() > phantom()} * b * {phantom() < phantom()} * c)
  expect_renders_same("$a \\ne b \\leq c \\geq d$",
                      {a != b} <= {c >= d})
  
})
test_that("Special characters in math or text mode do not cause errors", {
  expect_renders_same("$a?b$",
                      a * '?' * b)
  expect_renders_same("$a'$",
                      a * minute)
  expect_renders_same("$a''$",
                      a * second)
  expect_renders_same("R's plotmath system",
                      'R\'s plotmath system')
  
  # commas should render differently depending on whether they 
  # are inside math mode or not.
  expect_renders_same("a, b, c",
                      "a, b, c")
  expect_renders_same("$a,b,c$",
                      list(a, b, c))
})

test_that("Grouping over deeply nested commands renders correctly", {
  expect_renders_same("$\\widehat{a^b_{\\hat{x^2}}}$",
                      widehat(a[hat(x^2)]^b))
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
  
  expect_renders_same("$\\A_ \\gamma\\beta$", 
                      A[gamma]*beta)
  expect_renders_same("$\\A _ \\gamma\\beta$", 
                      A[gamma]*beta)
  expect_renders_same("$\\A _ {\\gamma\\beta}$", 
                      A[gamma*beta])
  
  expect_renders_same("$NO^{3}_{-}$", 
                      NO[phantom()-phantom()]^3)
  expect_renders_same("$NO_{-}^3$", 
                      NO[phantom()-phantom()]^3)
})

test_that("Superscript and subscript for operators are rendered correctly", {
  expect_renders_same("$\\sum_{i=1}^{N} x_i$",
                      sum(x[i], i==1, N))
  
  expect_renders_same("$\\sum^{N}_{i=1} x_i$",
                      sum(x[i], i==1, N))
  
  expect_renders_same("$\\lim_{x \\to 0}{\\frac{x^2}{x}}$",
                      lim(frac(x^2, x), x %->% 0))
  
  expect_renders_same("$\\min{x}$",
                      min(x,))
})

test_that("Argument parentheses are rendered correctly", {
  expect_renders_same("$\\frac{\\sin(x)}{\\cos(x)}$",
                      frac(sin(x), cos(x)))
})

test_that("Big brackets are rendered correctly", {
  expect_renders_same("$\\left(\\overset{a}{b}\\right)$",
                      bgroup('(', atop(a, b), ')'))
  
  expect_renders_same("$\\left[\\overset{a}{b}\\right]$",
                      bgroup('[', atop(a, b), ']'))
  
  expect_renders_same("$\\left{\\overset{a}{b}\\right}$",
                      bgroup('{', atop(a, b), '}'))
  
  expect_renders_same("$\\lceil{} x \\rceil$",
                      group(lceil, x, rceil))
  
  expect_renders_same("$\\| a \\|$",
                      group('|', group('|', a, '|'), '|')) 
})

test_that("Mismatched brackets are rendered correctly", {
  expect_renders_same("$\\left[A\\right)$",
                      bgroup('[', A, ')'))
  expect_renders_same("$\\left(A\\right}$",
                      bgroup('(', A, '}'))
})

test_that("Opening and closing math mode renders correctly", {
  expect_renders_same("$\\alpha^\\beta$ and $\\gamma$",
                      paste(alpha^beta, ' and ', gamma))
})

test_that("Escaped symbols renders correctly", {
  expect_renders_same("\\$", '$')
})

test_that("Spacing renders correctly", {
  expect_renders_same("$a\\ b$", a ~~ b)
  expect_renders_same("$a\\, b", a * phantom(.) * b)
})

test_that("Overall formatting renders correctly", {
  expect_renders_same(TeX("Test", bold=TRUE),
                      bold('Test'))
  expect_renders_same(TeX("Test", italic=TRUE),
                      italic('Test'))
  expect_renders_same(TeX("Test", bold=TRUE, italic=TRUE),
                      bolditalic('Test'))
})

test_that("Mix of numbers and letters renders correctly", {
  expect_renders_same(TeX("2a"), '2a')
  expect_renders_same(TeX("$2a$"), 2*a)
  expect_renders_same(TeX("$2 a$"), 2*a)
  expect_renders_same(TeX("$2\\alpha$"), 2*alpha)
})

test_that("User-defined latex renders correctly", {
  expect_renders_same(TeX("$\\mycommand$", user_defined = list(
    "\\mycommand" = "alpha"
  )), alpha)
})

test_that("Certain invalid LaTeX fails", {
  expect_error({ TeX("$\\bar{A$") })
  expect_error({ TeX("$\\left{\\left{A\\right}$") })
})