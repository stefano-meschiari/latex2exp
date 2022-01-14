
#' Plots an expression on the current graphical device.
#'
#' @param x A \code{\link{plotmath}} expression.
#' @param ... Parameters to be passed to the \code{\link{text}} function.
#' @export
plot.expression <- function(x, ..., titles="") {
  oldpar <- par(no.readonly = TRUE)
  dots <- list(...)
  on.exit(suppressWarnings(par(oldpar)))
  par(mar = c(1, 1, 1, 1), mfrow=c(length(x), 1))
  
  mapply(function(expr, title) {
    plot(0, 0, type = 'n', axes = F, xlab = '', ylab = '')
    text(0, 0, expr, ...)
    mtext(title)
    NULL
  }, x, titles)
  
  invisible()
}

#' @export
plot_expressions <- function(expr, ..., titles="") {
  plot.expression(expr, ..., titles=titles)
}

