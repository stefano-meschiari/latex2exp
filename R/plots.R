
#' Plots an expression on the current graphical device.
#'
#' @param x A \code{\link{plotmath}} expression.
#' @param ... Parameters to be passed to the \code{\link{text}} function.
#' @export
plot.expression <- function(x, ..., main="") {
  oldpar <- par(no.readonly = TRUE)
  dots <- list(...)
  on.exit(suppressWarnings(par(oldpar)))
  par(mar = c(0, 0, 0, 0), mfrow=c(length(x), 1))
  
  for (expr in x) {
    plot(0, 0, type = 'n', axes = F, xlab = '', ylab = '')
    text(0, 0, x, ...)
  }
  invisible()
}

#' @export
plot_expressions <- function(expr, ..., main="") {
  plot.expression(x, ..., main=main)
}

#' Returns a list of all supported LaTeX symbols and expressions that can be converted with \code{\link{latex2exp}}.
#'
#' @param plot whether to plot the table (FALSE by default)
#' @return a character vector of supported LaTeX expressions
#' @export
latex2exp_supported <- function(plot = FALSE) {
  .talls <- c('\\overset', '\\frac', .supsub)
  
  if (!plot) {
    return(sort(Filter(function(d) {
      return(!str_detect(d, "@$") && str_detect(d, '^\\\\'))
    }, names(.subs))))
  } else {
    syms <- sort(latex2exp_supported())
    syms <- syms[!(syms %in% .talls)]
    syms <-
      c(syms, 'TALLS', Reduce(c, lapply(sort(.talls), function(t)
        c(t, ''))))
    oldpar <- par(no.readonly = TRUE)
    on.exit(suppressWarnings(par(oldpar)))
    
    
    
    cols <- 2
    rows <- length(syms) %/% cols
    
    plot.new()
    par(mar = c(0, 0, 0, 0))
    plot.window(xlim = c(1, cols + 2), ylim = c(1, rows))
    
    col <- 1
    row <- 1
    for (sym in syms) {
      osym <- sym
      if (row == rows) {
        row <- 1
        col <- col + 1
      }
      
      if (sym == '') {
        row <- row + 3
        next
      }
      if (sym == 'TALLS') {
        col <- col + 1
        row <- 1
        next
      }
      
      sub <- .subs[[sym]]
      
      offset <- if (sym %in% .talls)
        2
      else
        0.5
      
      if (str_detect(sub, "@S@"))
        sym <- str_c(sym, "[2]")
      if (str_detect(sub, "@1@")) {
        if (osym %in% .supsub)
          sym <- str_c(sym, "_{x}")
        else
          sym <- str_c(sym, "{x}")
      }
      if (str_detect(sub, "@2@")) {
        if (osym %in% .supsub)
          sym <- str_c(sym, "^{y}")
        else
          sym <- str_c(sym, "{y}")
      }
      sym <- str_c("$", sym, "$")
      text(col, rows - row, sym, family = 'mono', pos = 4, cex=0.7)
      
      try(text(col + 0.6, rows - row,
               TeX(sym), pos = 4, offset = offset, cex=0.7))
      row <- row + 1
    }
    
    
  }
  
}

#' Plots a number of example LaTeX string, as parsed by \code{\link{TeX}}.
#' 
#' @export
latex2exp_examples <- function() {
  oldpar <- par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(oldpar)))
  
  plot.new()
  par(mar = c(0, 0, 0, 0))
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  examples <- c(
    "$\\alpha_{\\beta}^{\\gamma}$",
    "$\\frac{\\partial \\bar{x}}{\\partial t}$",
    "$\\sum_{i=1}^{10} x_i \\beta^i$",
    "$\\prod_{i = 1}^{100} x^i$",
    "$\\left(\\int_{0}^{1} \\sin(x) dx \\right)$",
    "The value of the fine structure constant is $\\alpha \\approx \\frac{1}{137}$.",
    "$\\nabla \\times \\bar{x}$ and $\\nabla \\cdot \\bar{x}$",
    "$\\sqrt[\\alpha\\beta]{x_i^2}$",
    "\\textbf{Bold} and \\textit{italic} text!",
    "$\\left{\\left(\\left[BRACES\\right]\\right)\\right}$",
    "Whitespace compliant: $x ^ 2 \\times \\sum_ 0 ^ 1 y _ i$",
    "Numbers: $0.05$, $0.03$, $0.005^{0.002}_{0.01}$",
    "Phantom: $a\\phantom{test}b$"
  )
  
  x <- 0
  y <- seq(0.95, 0.05, length.out = length(examples))
  
  text(
    0.5, y, examples, pos = 2, cex = 0.5, family = 'mono'
  )
  text(0.5, y, TeX(examples), pos = 4)
  return(TRUE)
}