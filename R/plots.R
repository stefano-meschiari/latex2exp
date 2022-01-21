
#' Plots an expression on the current graphical device.
#'
#' @param x A \code{\link{plotmath}} expression.
#' @param ... Parameters to be passed to the \code{\link{text}} function.
#' @export
plot.expression <- function(x, ..., main=NULL) {
  oldpar <- par(no.readonly = TRUE)
  dots <- list(...)
  on.exit(suppressWarnings(par(oldpar)))
  par(mar = c(1, 1, 1, 1))
  
  plot(0, 0, type = 'n', axes = FALSE, xlab = '', ylab = '')
  text(0, 0, x, ...)
  
  if (is.null(main)) {
      if (!is.null(attr(x, "latex"))) {
        mtext(attr(x, "latex"), family="mono")
      }
  } else {
    mtext(main)
  }
  invisible(NULL)
}

#' Returns the list of supported LaTeX commands.
#' 
#' If `show` is TRUE, also show a searchable table of symbols.
#' 
#' @param show Show a searchable table of symbols
#' @param ... 
#'
#' @export
latex2exp_supported <- function(show=FALSE, ...) {
  dots <- list(...)
   
  # the previous version of latex2exp accepted the parameter `plot` with the same
  # meaning as show=TRUE. 
  if (!is.null(dots$plot) && dots$plot) {
    .Deprecated("Use the parameter show=TRUE instead of plot.")
    show <- TRUE
  }
  
  if (!show) {
    supp <- lapply(latex_supported, function(it) {
        # remove all commands that include the '@' character, which are used internally to
        # escape certain commands.
        names(it)[!str_detect(names(it), fixed("@"))]
      }) 
    supp <- mapply(function(category, commands) {
      data.frame(category, commands)
    }, names(supp), supp, SIMPLIFY = FALSE)
    
    return(do.call(function(...) rbind.data.frame(..., make.row.names=FALSE, stringsAsFactors = FALSE), 
                   supp))
  } else {
    vignette("supported-commands", package = "latex2exp")
  }
}