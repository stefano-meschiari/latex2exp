`%??%` <- function(x, y) {
  return(if (is.null(x) || is.na(x) || !x)  y else x)
}

#' Prints out a parsed LaTeX object, as returned by TeX(..., output='ast').
#' This is primarily used for debugging.
#' @param x The latex2exp object.
#' @param ... (ignored)
print.latextoken <- function(x, ...) {
  dots <- list(...)
  level <- dots$level %??% 0
  n <- dots$n %??% 1
  
  ind <- rep(' ', level)
  cat(ind, n, '. \'', x$s, '\' ', x$textmode, ' ', x$ch, '\n', 
      sep = '')
  sapply(x$args, print, level = level + 1, n = 1, ch = '{')
  sapply(x$sqarg, print, level = level + 1, n = 1, ch = '[')
  
  if (!is.null(x$succ)) {
    print.latextoken(x$succ, level=level, n=n + 1)
  }
}