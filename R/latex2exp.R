#' @importFrom magrittr %>%
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_split
#' @importFrom stringr fixed
NULL

#' Converts a token created by TeX() to a string, later to be parsed into an expression (for internal use).
#' 
#' @param x The TeX() token
#' @param ... Additional arguments (ignored)
#' @return A string
toString.latextoken <- function(x, ..., translations=.subs) {
  tok <- x
  
  if (is.null(tok$prev)) {
    pre <- 'paste('
  } else {
    pre <- ','
  }
  
  tok$args[(length(tok$args) + 1):3] <- ''

  if (!is.null(tok$sym)) {
    if (tok$sym == "^") {
      tok$args = list("", tok$args[[1]], tok$args[[2]])
    } else if (tok$sym == "^_") {
      tok$args = list(tok$args[[2]], tok$args[[1]], tok$args[[3]])
    }
  }

  tok$string <- tok$string %>%
    str_replace_all("\\\\COMMA@", ',') %>%
    str_replace_all("\\\\PERIOD@", '.') %>%
    str_replace_all("\\\\SEMICOLON@", ';')

  translator <- translations[[tok$string]]
  
  if (!is.null(translator)) {
    
    if (is.function(translator)) {
      expression <- translator(content=tok$string,
                               args=tok$args,
                               square_args=tok$sqarg)
    } else {
      first_arg <- if (length(tok$args) > 0) toString(tok$args[[1]], translations=translations) else ""
      second_arg <- if (length(tok$args) > 1) toString(tok$args[[2]], translations=translations) else ""
      third_arg <- if (length(tok$args) > 2) toString(tok$args[[3]], translations=translations) else ""
      square_arg <- if (length(tok$sqarg) > 0) toString(tok$sqarg[[1]], translations=translations) else ""
      
      expression <- translator %>%
        str_replace_all("@P@", 'phantom()') %>%
        str_replace_all("@1@", first_arg) %>%
        str_replace_all("@2@", second_arg) %>%
        str_replace_all("@S@", square_arg) %>%
        str_replace_all("@3@", third_arg)
    }
  } else if (tok$string != '\\' &&
             str_detect(tok$string, '^\\\\') && !tok$textmode) {
    expression <- str_replace(tok$string, "\\\\", "")

    if (length(tok$args) > 0) {
      expression <-
        str_c(expression, ',', str_c(sapply(tok$args, toString, translations=translations), collapse = ','))
    }
  } else if (str_detect(tok$string, "^[0-9]*$")) {
    expression <- str_c('\'', tok$string, '\'')
  } else {
    expression <- str_c('\'', str_replace_all(tok$string, '\\\\', '\\\\\\\\'), '\'')
  }

  expression <- str_c(pre, expression)

  if (is.null(tok$succ)) {
    expression <- str_c(expression, ')')
  } else {
    expression <- str_c(expression, toString(tok$succ, translations=translations))
  }

  return(expression)
}


.token <-
  function(string = '', parent = NULL, prev = NULL, ch = '', textmode = TRUE) {
    tok <- new.env()
    tok$string <- string
    tok$args <- list()
    tok$sqarg <- list()
    tok$parent <- parent
    tok$prev <- prev
    tok$textmode <- textmode

    if (!is.null(prev)) {
      prev$succ <- tok
    }
    tok$r <- ""
    tok$ch <- ch
    class(tok) <- 'latextoken'
    return(tok)
  }

.str_replace_all <- function(x, pattern, replacement) {
  str_replace_all(x, fixed(pattern), replacement)
}

## Takes a LaTeX string, or a vector of LaTeX strings, and converts it into
## the closest plotmath expression possible.
##
## Returns an expression by default; can either return 'character' (return the expression
## as a string) or 'ast' (returns the tree as parsed from the LaTeX string; useful for debug).
.parseTeX <-
  function(string, bold=FALSE, italic=FALSE, extras=list(), output = c('expression', 'character', 'ast')) {
    output <- match.arg(output)
    original <- string
    # Create the root node
    textmode <- TRUE
    root <- .token(textmode = textmode)
    token <- root
    
    # Treat \left( / \right) and company specially in order to not have to special-case them in the
    # parser
    string <- string %>%
      .str_replace_all('\\left{', '\\leftBRACE@{') %>%
      .str_replace_all('\\left[', '\\leftSQUARE@{') %>%
      .str_replace_all('\\left|', '\\leftPIPE@{') %>%
      .str_replace_all('\\left.', '\\leftPERIOD@{') %>%
      .str_replace_all('\\middle|', '\\middlePIPE@{') %>%
      .str_replace_all('\\|', '\\PIPE@ ') %>%
      .str_replace_all('\\left(', '\\leftPAR@{') %>%
      .str_replace_all('\\right}', '}\\rightBRACE@ ') %>%
      .str_replace_all('\\right]', '}\\rightSQUARE@ ') %>%
      .str_replace_all('\\right)', '}\\rightPAR@ ') %>%
      .str_replace_all('\\right|', '}\\rightPIPE@ ') %>%
      .str_replace_all('\\right.', '\\rightPERIOD@{') %>%

      .str_replace_all("\\,", "\\SPACE1@ ") %>%
      .str_replace_all("\\;", "\\SPACE2@ ") %>%

      .str_replace_all(",", "\\\\COMMA@ ") %>%
      .str_replace_all(";", "\\\\SEMICOLON@ ") %>%
      .str_replace_all("\\.", "\\\\PERIOD@ ") %>%
  
      str_replace_all("([ ]+)", " ") %>%
      str_replace_all(" \\^ ", "\\^") 
    # Split the input into characters
    str <- str_split(string, '')[[1]]
    prevch <- ''

    # If within a tag contained in .textmode, preserve spaces
    nextisarg <- 0
    needsnew <- FALSE

    i <- 0
    while (i < length(str)) {
      i <- i + 1
      ch = str[i]
      nextch = if (!is.na(str[i + 1])) {
        str[i + 1]
      } else {
        ''
      }
      
      if (ch == '\\') {
        # Char is \ (start a new node, unless preceded by another \)
        if (nextch %in% c("[", "]", "{", "}")) {
          needsnew <- TRUE
        } else if (prevch != '\\') {
          old <- token
          needsnew <- FALSE
          if (nextisarg == 2) {
            nextisarg <- 0
            token <-
              .token(
                s = '\\', prev = token$parent, parent = token$parent, ch = ch, textmode =
                  textmode
              )
          } else if (nextisarg == 1) {
            nextisarg <- 2
            ntoken <-
              .token(
                parent = token, s = ch, ch = ch, textmode = textmode
              )
            token$args[[length(token$args) + 1]] <- ntoken
            token <- ntoken
          } else {
            token <-
              .token(
                s = '\\', parent = token$parent, prev = old, ch = ch, textmode = textmode
              )
          }
        } else {
          ch <- ''
        }
      } else if (ch == " " && !textmode) {
        # Ignore spaces, unless in text mode
        if (prevch != ' ') {
          if (nextisarg == 1) {
            nextisarg <- 1
            
          } else {
            token <-
              .token(
                prev = token, parent = token$parent, ch = ch, textmode = textmode
              )
            nextisarg <- 0
          }
        }
      } else if (ch == "{" && !prevch == "\\") {
        # Brace parameter starting, create new child node

        nextisarg <- 0

        old <- token
        token <- .token(parent = old, ch = ch, textmode = textmode)
        old$args[[length(old$args) + 1]] <- token
      } else if ((ch == "}" || ch == "]") && !prevch == "\\") {
        # Square or brace parameter ended, return to parent node
        token <- token$parent
        needsnew <- TRUE
      } else if (ch == "$" && !prevch == "\\") {
        textmode <- !textmode
        token <-
          .token(
            prev = token, parent = token$parent, ch = ch, textmode = textmode
          )
      } else if (ch == "[" && !prevch == "\\" && !textmode) {
        # Square parameter started, create new child node, put in $sqarg
        nextisarg <- 0
        old <- token
        token <- .token(parent = old, ch = ch, textmode = textmode)
        old$sqarg[[1]] <- token
      } else if (ch == ")" || ch == "(" || ch == "'") {
        if (ch == "'" && prevch == "'") {
          next
        }
        
        if (ch == "'" && nextch == "'") {
          token <-
            .token(
              s = "''", parent = token$parent, prev = token, textmode = textmode
            )
        } else {
          token <-
            .token(
              s = ch, parent = token$parent, prev = token,textmode = textmode
            )
        }
        token <-
          .token(prev = token, parent = token$parent, textmode = textmode)
      } else if ((ch == "^" || ch == "_") && !textmode) {
        # Sup or sub. Treat them as new nodes, unless preceded by a LaTeX expression
        # such as \sum, in which case sup and sub should become a parameter
        if (token$string %in% .supsub) {
          token$sym <- str_c(token$sym, ch)
        } else {
          old <- token
          token <-
            .token(
              prev = old, s = ch, parent = old$parent, ch = ch, textmode = textmode
            )
        }

        nextisarg <- 1
      } else {
        # Any other character
        if (nextisarg == 1) {
          token$args[[length(token$args) + 1]] <-
            .token(
              s = ch, parent = token, ch = ch, textmode = textmode
            )
          if (nextch == '^' || nextch == '_') {

          } else {
            if (nextisarg == 1) {
              nextisarg <- 0
              token <-
                .token(
                  prev = token, parent = token$parent, ch = ch, textmode = textmode
                )
            }
          }
        } else {
          if (needsnew || ch %in% .separators || prevch %in% .separators) {
            token <-
              .token(
                prev = token, parent = token$parent, ch = ch, textmode = textmode
              )
            needsnew <- FALSE
          }
          token$string <- str_c(token$string, ch)
        }
      }

      prevch <- ch
    }

    # Dollar signs are unbalanced
    if (!textmode) {
      stop("Dollar signs ($) unbalanced in LaTeX expression")
    }
    
    post_process(root)
    if (output == 'ast') {
      return(root)
    }
      
    str <- toString(root, translations=c(extras, .subs))
    if (bold && italic) {
      str <- str_c("bolditalic(", str, ")")
    } else if (bold) {
      str <- str_c("bold(", str, ")")
    } else if (italic) {
      str <- str_c("italic(", str, ")")
    }
    
    exp <- unname(tryCatch(
      parse(text = str), error = function(e) {
        message("Original string: ", original)
        message("Parsed expression: ", str)
        stop(e)
      }
    ))

    if (output == 'character') {
      return(str)
    } else {
      return(exp)
    }
  }

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
  function(input, bold=FALSE, italic=FALSE, extras=list(), output = c('expression', 'character', 'ast')) {
    if (length(input) > 1) {
      lapply(input, .parseTeX, bold=bold, italic=italic, extras=extras, output = output)
    } else {
      .parseTeX(input, bold=bold, italic=italic, extras=extras, output=output)
    }
  }


post_process <- function(tok) {
  if (tok$string == "^" && !is.null(tok$succ) && tok$succ$string == "_") {
    tok$string = "\\SUB_AND_EXP@"
    tok$args = c(tok$succ$args, tok$args)
    tok$succ <- tok$succ$succ
  } else if (tok$string == "_" && !is.null(tok$succ) && tok$succ$string == "^") {
    tok$string = "\\SUB_AND_EXP@"
    tok$args = c(tok$args, tok$succ$args)
    tok$succ <- tok$succ$succ
  }
  
  if (!is.null(tok$succ)) {
    post_process(tok$succ)
  }
  sapply(tok$args, post_process)
  sapply(tok$sqargs, post_process)
}

