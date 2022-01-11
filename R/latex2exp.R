

#' Plots an expression on the current graphical device.
#'
#' @param x A \code{\link{plotmath}} expression.
#' @param ... Parameters to be passed to the \code{\link{text}} function.
#' @export
plot.expression <- function(x, ...) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(oldpar)))
  par(mar = c(0, 0, 0, 0))
  plot(
    0, 0, type = 'n', axes = F, xlab = '', ylab = ''
  )
  text(0, 0, x, ...)
  invisible()
}

#' Converts a token created by TeX() to a string, later to be parsed into an expression (for internal use).
#' 
#' @param x The TeX() token
#' @param ... Additional arguments (ignored)
#' @return A string
#' @export
toString.latextoken <- function(x, ...) {
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

  if (!is.na(.subs[tok$s])) {
    p <- .subs[tok$s] %>%
      str_replace_all("@P@", 'phantom()') %>%
      str_replace_all("@1@", if (length(tok$args) > 0)
        toString(tok$args[[1]])
        else
          "") %>%
      str_replace_all("@2@", if (length(tok$args) > 1)
        toString(tok$args[[2]])
        else
          "") %>%
      str_replace_all("@S@", if (length(tok$sqarg) > 0)
        toString(tok$sqarg[[1]])
        else
          "") %>%
      str_replace_all("@3@", if (length(tok$args) > 2)
        toString(tok$args[[3]])
        else
          "")
  } else if (tok$string != '\\' &&
             str_detect(tok$string, '^\\\\') && !tok$textmode) {
    p <- str_replace(tok$string, "\\\\", "")

    if (length(tok$args) > 0) {
      p <-
        str_c(p, ',', str_c(sapply(tok$args, toString), collapse = ','))
    }
  } else if (str_detect(tok$string, "^[0-9]*$")) {
    p <- str_c('\'', tok$string, '\'')
  } else {
    p <- str_c('\'', str_replace_all(tok$string, '\\\\', '\\\\\\\\'), '\'')
  }

  p <- str_c(pre, p)

  if (is.null(tok$succ)) {
    p <- str_c(p, ')')
  } else {
    p <- str_c(p, toString(tok$succ))
  }

  return(p)
}


.token <-
  function(string = '', parent = NULL, prev = NULL, ch = '', textmode = TRUE, skip=FALSE) {
    tok <- new.env()
    tok$string <- string
    tok$args <- list()
    tok$sqarg <- list()
    tok$parent <- parent
    tok$prev <- prev
    tok$textmode <- textmode
    tok$skip <- skip

    if (!is.null(prev)) {
      prev$succ <- tok
    }
    tok$r <- ""
    tok$ch <- ch
    class(tok) <- 'latextoken'
    return(tok)
  }

## Takes a LaTeX string, or a vector of LaTeX strings, and converts it into
## the closest plotmath expression possible.
##
## Returns an expression by default; can either return 'character' (return the expression
## as a string) or 'ast' (returns the tree as parsed from the LaTeX string; useful for debug).
.parseTeX <-
  function(string, bold=FALSE, italic=FALSE, output = c('expression', 'character', 'ast')) {
    output <- match.arg(output)
    original <- string
    # Create the root node
    textmode <- TRUE
    root <- .token(textmode = textmode)
    token <- root

    # Treat \left( / \right) and company specially in order to not have to special-case them in the
    # parser
    string <- string %>%
      str_replace_all('\\\\left\\{', '\\\\leftBRACE@{') %>%
      str_replace_all('\\\\left\\[', '\\\\leftSQUARE@{') %>%
      str_replace_all('\\\\left\\|', '\\\\leftPIPE@{') %>%
      str_replace_all('\\\\left\\.', '\\\\leftPERIOD@{') %>%
      str_replace_all('\\\\middle\\|', '\\\\middlePIPE@{') %>%
      str_replace_all('\\\\left\\(', '\\\\leftPAR@{') %>%
      str_replace_all('\\\\right\\}', '}\\\\rightBRACE@ ') %>%
      str_replace_all('\\\\right\\]', '}\\\\rightSQUARE@ ') %>%
      str_replace_all('\\\\right\\)', '}\\\\rightPAR@ ') %>%
      str_replace_all('\\\\right\\|', '}\\\\rightPIPE@ ') %>%
      str_replace_all('\\\\right\\.', '\\\\rightPERIOD@{') %>%

      str_replace_all("\\\\,", "\\\\SPACE1@ ") %>%
      str_replace_all("\\\\;", "\\\\SPACE2@ ") %>%

      str_replace_all(",", "\\\\COMMA@ ") %>%
      str_replace_all(";", "\\\\SEMICOLON@ ") %>%
      str_replace_all("\\.", "\\\\PERIOD@ ") %>%
  
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
          if (needsnew) {
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

    post_process(root)
    if (output == 'ast') {
      return(root)
    }
      
    str <- toString(root)
    if (bold && italic) {
      str <- paste0("bolditalic(", str, ")")
    } else if (bold) {
      str <- paste0("bold(", str, ")")
    } else if (italic) {
      str <- paste0("italic(", str, ")")
    }
    
    exp <- tryCatch(
      parse(text = str), error = function(e) {
        message("Original string: ", original)
        message("Parsed expression: ", str)
        stop(e)
      }
    )

    if (output == 'character') {
      return(str)
    } else
      return(exp)
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
#' @param string A character vector containing LaTeX expressions. Note that any backslashes must be escaped (e.g. "$\\alpha").
#' @param bold   Whether to make the entire label bold
#' @param italic Whether to make the entire label italic
#' @param output The returned object, one of "expression" (default, returns a plotmath expression ready for plotting), "character" (returns the expression as a string), and "ast" (returns the tree used to generate the expression).
#'
#' @return Returns an expression (see the \code{output} parameter).
#'
#' @examples
#' TeX("$\\alpha$")
#' TeX("The ratio of 1 and 2 is $\\frac{1}{2}$")
#'
#' a <- 1:100
#' plot(a, a^2, xlab=TeX("$\\alpha$"), ylab=TeX("$\\alpha^2$"))
#' @export
TeX <-
  function(string, bold=FALSE, italic=FALSE, output = c('expression', 'character', 'ast')) {
    return(sapply(string, .parseTeX, bold=bold, italic=italic, output = output))
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

