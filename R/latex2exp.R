#' @importFrom stringr str_trim
#' @importFrom stringr str_match
#' @importFrom stringr str_match_all
#' @importFrom stringr fixed
#' @importFrom stringr str_length
NULL

#' Deprecated; use \code{\link{TeX}} instead.
#'
#' @param string A character vector containing LaTeX expressions. Note that any
#'   backslashes must be escaped (e.g. "$\\alpha").
#' @param output The returned object, one of "expression" (default, returns a
#'   plotmath expression ready for plotting), "character" (returns the
#'   expression as a string), and "ast" (returns the tree used to generate the
#'   expression).
#'
#' @return Returns an expression (see the \code{output} parameter).
#' @export
latex2exp <- function(string, output = c('expression', 'character', 'ast')) {
  .Deprecated('TeX', 'latex2exp')
  return(TeX(string, output = output))
}

#' Converts LaTeX to a \code{\link{plotmath}} expression.
#' 
#' \code{TeX} converts a string comprising LaTeX commands (such as
#' a math equation) to a \code{\link{plotmath}} expression. Plotmath
#' expressions can be used throught R's graphic system to represent
#' formatted text and equations.
#' 
#' @param input  A character vector containing LaTeX strings. 
#'               Note that any backslashes must be escaped (e.g. "$\\alpha").
#' @param bold   Whether to make the entire label bold
#' @param italic Whether to make the entire label italic
#' @param user_defined Described in the "Adding New Commands" section.
#' @param output The returned object, one of "expression" (default, returns 
#'               a plotmath expression ready for plotting), 
#'               "character" (returns the expression as a string), 
#'               and "ast" (returns the tree used to generate the expression).
#'
#' @return Returns a plotmath expression by default. The \code{output} parameter
#' can modify the type of the returned value. 
#' 
#' If more than one string is specified in the \code{input} parameter, returns a
#' list of expressions.
#'
#' @section Adding new commands: 
#' New LaTeX commands can be defined by supplying the \code{user_defined}
#' parameter. The \code{user_defined} parameter is a list that contains LaTeX
#' commands as names, and template strings as values. A LaTeX command that
#' matches one of the names is translated into the corresponding string and
#' included in the final plotmath expression. The file \code{symbols.R} in the
#' source code of this package contains one such table that can be used as a
#' reference.
#' 
#' The template string can include one of the following special template
#' parameters:
#' 
#' \itemize{
#' \item \code{$arg1, $arg2, ...} represent the first, second, ... brace
#'   argument. E.g. for \code{\\frac{x}{y}}, \code{$arg1} is \code{x} and
#'   \code{$arg2} is \code{y}.
#' \item \code{$opt} is an optional argument in square brackets. E.g. for
#'   \code{\\sqrt[2]{x}}, \code{$opt} is \code{2}.
#' \item \code{$sub} and \code{$sup} are arguments in the exponent (\code{^}) or
#'   subscript (\code{_}) following the current expression. E.g. for
#'   \code{\\sum^{x}}, \code{$sup} is \code{x}.
#' \item \code{$LEFT} and \code{$RIGHT} are substituted the previous and
#'   following LaTeX expression relative to the current token.
#' }
#' See the Examples section for an example of using the \code{user_defined}
#' option.
#' 
#' @examples
#' TeX("$\\alpha$") # plots the greek alpha character
#' TeX("The ratio of 1 and 2 is $\\frac{1}{2}$")
#'
#' a <- 1:100
#' plot(a, a^2, xlab = TeX("$\\alpha$"), ylab = TeX("$\\alpha^2$"))
#' 
#' # create a \\variance command that takes a single argument
#' TeX("$\\variance{X} = 10$",
#'     user_defined = list("\\variance" = "sigma[$arg1]^2"))
#' @export
TeX <- function(input, bold = FALSE, italic = FALSE, user_defined = list(),
  output = c('expression', 'character', 'ast')) {
  if (length(input) > 1) {
    return(sapply(input, TeX, bold = bold, italic = italic,
      user_defined = user_defined, output = output))
  }
  stopifnot(is.character(input))
  
  output <- match.arg(output)
  parsed <- parse_latex(input)
    
  # Try all combinations of "hacks" in this grid, until one succeeds.
  # As more hacks are introduced, the resulting expression will be less and
  # less tidy, although it should still be visually equivalent to the 
  # desired output given the latex string.
  grid <- expand.grid(hack_parentheses = c(FALSE, TRUE))
  successful <- FALSE
  for (row in seq_len(nrow(grid))) {
    # Make a deep clone of the LaTeX token tree
    parsed_clone <- clone_token(parsed)
    rendered <- render_latex(parsed_clone, user_defined,
      hack_parentheses = grid$hack_parentheses[[row]])
    
    if (bold && italic) { 
      rendered <- paste0("bolditalic(", rendered, ")")
    } else if (bold) {
      rendered <- paste0("bold(", rendered, ")")
    } else if (italic) {
      rendered <- paste0("italic(", rendered, ")")
    }
    
    cat_trace("Rendered as ", rendered, " with parameters ",
      toString(grid[row, ]))
    
    if (output == "ast") {
      return(parsed)
    }
    
    rendered_expression <- try({
      str2expression(rendered)
    }, silent = TRUE) 
    
    if (inherits(rendered_expression, "try-error")) {
      error <- rendered_expression
      cat_trace("Failed, trying next combination of hacks, error:", error, 
                " parsed as: ", rendered)
      
      if (row == 1) {
        original_error <- error
      }
    } else {
      successful <- TRUE
      break
    }
  }
  
  if (!successful) {
    stop("Error while converting LaTeX into valid plotmath.\n",
         "Original string: ", input, "\n",
         "Parsed expression: ", rendered, "\n",
         original_error)
  }
  if (output == "character") {
    return(rendered)
  }
  
  # if the rendered expression is empty, return expression('') instead.
  if (length(rendered_expression) == 0) {
    rendered_expression <- expression('')
  }
  
  class(rendered_expression) <- c("latexexpression", "expression")
  attr(rendered_expression, "latex") <- input
  attr(rendered_expression, "plotmath") <- rendered
  
  rendered_expression
}
