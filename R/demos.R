#' latex2exp Examples
#' 
#' Plots a number of example LaTeX string, as parsed 
#' by \code{\link{TeX}}.
#' 
#' @param cex Multiplier for font size
#' @export
latex2exp_examples <- function(cex=1) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(oldpar)))
  
  plot.new()
  par(mar = c(0, 0, 0, 0))
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  examples <- c(
    "$\\alpha_{\\beta}^{\\gamma}$",
    "$\\bar{x}'' = \\bar{v}' = \\frac{\\partial \\bar{\\Phi}}{\\partial t}$",
    "$\\sum_{i=1}^{10} x_i \\beta^i$",
    "$\\prod_{i = 1}^{100} x^i$",
    "$\\left(\\int_{0}^{1} \\sin(x) dx \\right)$",
    "The \\it{fine structure constant} is $\\alpha \\approx \\frac{1}{137}$.",
    "$\\nabla \\times \\bar{x}$ and $\\nabla \\cdot \\bar{x}$",
    "$\\sqrt[\\alpha\\beta]{x_i^2}$",
    "\\textbf{Bold} and \\textit{italic} text!",
    "$\\left{\\left(\\left[\\left|BRACES!\\right|\\right]\\right)\\right}$",
    "Whitespace compliant: $x ^ 2 \\times \\sum_ 0 ^ 1 y _ i$",
    "Numbers: $0.05$, $0.03$, $0.005^{0.002}_{0.01}$",
    "Phantom: $a\\phantom{test}b$"
  )
  
  x <- 0
  y <- seq(0.95, 0.05, length.out = length(examples))
  
  text(
    0.5, y, str_c("TeX(r\"(", examples, ")\")"), pos = 2, cex = 0.5 * cex, family = 'mono'
  )
  text(0.5, y, TeX(examples), pos = 4, cex=cex)
  return(invisible(TRUE))
}