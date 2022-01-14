#' @importFrom magrittr %>%
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_split
#' @importFrom stringr str_sub
#' @importFrom stringr str_match
#' @importFrom stringr str_trim
#' @importFrom stringr str_starts
#' @importFrom stringr str_match
#' @importFrom stringr str_match_all
#' @importFrom stringr fixed
NULL

#' Converts a LaTeX string to a \code{\link{plotmath}} expression. Deprecated; use \code{\link{TeX}} instead.
#' @param string A character vector containing LaTeX expressions. Note that any backslashes must be escaped (e.g. "$\\alpha").
#' @param output The returned object, one of "expression" (default, returns a plotmath expression ready for plotting), "character" (returns the expression as a string), and "ast" (returns the tree used to generate the expression).
#'
#' @return Returns an expression (see the \code{output} parameter).
#'
latex2exp <-
  function(string, output = c('expression', 'character', 'ast')) {
    .Deprecated('TeX', 'latex2exp')
    return(TeX(string, output=output))
  }

#' Converts a LaTeX string to a \code{\link{plotmath}} expression.
#'
#' @param input  A character vector containing LaTeX strings. 
#'               Note that any backslashes must be escaped (e.g. "$\\alpha").
#' @param bold   Whether to make the entire label bold
#' @param italic Whether to make the entire label italic
#' @param output The returned object, one of "expression" (default, returns 
#'               a plotmath expression ready for plotting), 
#'               "character" (returns the expression as a string), 
#'               and "ast" (returns the tree used to generate the expression).
#'
#' @return Returns a plotmath expression by default. The `output` parameter can 
#' modify the type of the returned value. 
#' 
#' If more than one string is specified in the `input` parameter, returns a list
#' of expressions.
#'
#' @examples
#' TeX("$\\alpha$")
#' TeX("The ratio of 1 and 2 is $\\frac{1}{2}$")
#'
#' a <- 1:100
#' plot(a, a^2, xlab=TeX("$\\alpha$"), ylab=TeX("$\\alpha^2$"))
#' @export
TeX <-
  function(input, bold=FALSE, italic=FALSE, user_defined=list(), output = c('expression', 'character', 'ast')) {
    if (length(input) > 1) {
      return(sapply(input, TeX, bold=bold, italic=italic, user_defined=user_defined, output = output))
    }
    
    output <- match.arg(output)
    parsed <- parse_latex(input)
    
    rendered <- render_latex(parsed, user_defined)
    if (output == "ast") {
      return(parsed)
    }
    
    if (bold) {
      rendered <- str_c("bold(", rendered, ")")
    }
    if (italic) {
      rendered <- str_c("italic(", rendered, ")")
    }
    if (output == "character") {
      return(rendered)
    }
    
    expression <- tryCatch(str2expression(rendered), error=function(e) {
      stop("Error while converting LaTeX into plotmath.\n",
           "Original string: ", input, "\n",
           "Parsed expression: ", rendered, "\n",
           e)
    })  
    
    class(expression) <- c("latexexpression", "expression")
    attr(expression, "latex") <- input
    attr(expression, "plotmath") <- rendered
    
    expression
  }
