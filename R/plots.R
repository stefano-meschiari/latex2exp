#' @importFrom utils vignette
#' @importFrom graphics mtext par plot.new plot.window text
NULL

#' Previews a LaTeX equation
#' 
#' Plots the result of a call to [TeX]
#' on the current graphical device. This is useful to preview the
#' output before placing it on a plot.
#'
#' @param x A \code{\link{plotmath}} expression returned by \code{\link{TeX}}.
#' @param ... Parameters to be passed to the \code{\link{text}} function.
#' @param main Title of the plot
#' @export
#' @examples 
#' plot(TeX("Example equation: $a \\geq b$"))
plot.expression <- function(x, ..., main=NULL) {
  oldpar <- par(no.readonly = TRUE)
  dots <- list(...)
  on.exit(suppressWarnings(par(oldpar)))
  par(mar = c(1, 1, 1, 1))
  
  plot(0, 0, type = 'n', axes = FALSE, xlab = '', ylab = '')
  text(0, 0, x, ...)
  
  if (!is.null(main)) {
    mtext(main)
  }
  invisible(NULL)
}

#' Returns the list of supported LaTeX commands.
#' 
#' If \code{show} is TRUE, also show a searchable table of symbols.
#' 
#' @param show Show a searchable table of symbols
#' @param ... Other parameters (not used)
#' @return A data frame containing a table of supported LaTeX commands.
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
        names(it)[!grepl("@", names(it), fixed = TRUE)]
      }) 
    supp <- mapply(function(category, commands) {
      # create an example suitable to demo each category of latex commands
      examples <- sapply(commands, function(commands) {
          if (category %in% c("arithmetic operators", "binary operators", "arrows")) {
          paste0("$\\alpha ", commands, " \\beta$")
        } else if (category == "set operators") {
          paste0("$A ", commands, " B$")
        } else if (commands == "\\frac" || commands == "\\overset") {
          "$\\frac{x+y}{x-y}$"
        } else if (commands == "\\lim") {
          "$\\lim_{x \\to \\infty} \\frac{1}{x}$" 
        } else if (commands %in% c("\\sum", "\\prod", "\\int")) {
          paste0("$", commands, "_{i=0}^{\\infty}$")
        } else if (commands == "\\sqrt") {
          "$\\sqrt[z]{x+y}$"
        } else if (commands %in% c("\\max", "\\min")) {
          paste0("$", commands, "_{x \\in X} x^2$") 
        } else if (commands %in% c("\\bigcup", "\\bigcap")) {
          paste0("$", commands, "_{i} A_i$") 
        } else if (category == "text size" || category == "formatting") {
          paste0(commands, "{example text}")
        } else if (category %in% c("\\ ", "\\;", "\\,")) {
          paste0("$x ", commands, " y$")
        } else if (commands == "\\braket") {
          paste0("$\\braket{\\Psi | \\Psi}$")  
        } else if (category == "decorations" || category == "vector") {
          paste0("$", commands, "{\\Psi}$")
        } else if (category == "layout and spacing") {
          paste0("$A ", commands, " B$")
        } else if (category == "parentheses" || category == "parentheses (not scalable)") {
          op <- commands
          if (commands == "\\left(") {
            clo <- "\\right)"
          } else if (commands == "\\left{") {
            clo <- "\\right}"
          } else if (commands == "\\left[") {
            clo <- "\\right]"
          } else if (commands == "\\left|") {
            clo <- "\\right|"
          } else if (commands == "\\left.") {
            clo <- "\\right."
          } else if (grepl("^\\\\l", commands)) {
            clo <- paste0("\\r", str_replace(commands, fixed("\\l"), ""))
          } else if (grepl("^\\\\r", commands)) {
            return(NA)
          } else {
            clo <- op
          }
          paste0("$", op, " a+b ", clo, "$")
        } else {
          paste0("$", commands, "$")
        }
      })
      data.frame(category, command=commands, example=examples)
    }, names(supp), supp, SIMPLIFY = FALSE)
    
    do.call(function(...) rbind.data.frame(..., make.row.names=FALSE, stringsAsFactors = FALSE), 
            supp)
  } else {
    vignette("supported-commands", package = "latex2exp")
  }
}