`%??%` <- function(x, y) {
  return(if (is.null(x) || is.na(x) || !x)  y else x)
}

#' Prints out a parsed LaTeX object, as returned by TeX(..., output='ast').
#' @param x The latex2exp object.
#' @param ... (ignored)
print.latextoken <- function(x, ...) {
  dots <- list(...)
  level <- dots$level %??% 0
  n <- dots$n %??% 1
  ch <- dots$n %??% ''
  
  ind <- rep(' ', level)
  cat(ind, n, ch, '. \'', x$s, '\' ', x$textmode, ' ', x$ch, '\n', sep =
        '')
  sapply(x$args, print, level = level + 1, n = 1, ch = '{')
  sapply(x$sqarg, print, level = level + 1, n = 1, ch = '[')

  if (!is.null(x$succ))
    print.latextoken(x$succ, level, n + 1)
}

post_process <- function(tok) {
  if (tok$s == "^" && !is.null(tok$succ) && tok$succ$s == "_") {
    tok$s = "\\SUB_AND_EXP@"
    tok$args = c(tok$succ$args, tok$args)
    tok$succ <- tok$succ$succ
  } else if (tok$s == "_" && !is.null(tok$succ) && tok$succ$s == "^") {
    tok$s = "\\SUB_AND_EXP@"
    tok$args = c(tok$args, tok$succ$args)
    tok$succ <- tok$succ$succ
  }
  
  if (!is.null(tok$succ))
    post_process(tok$succ)
  sapply(tok$args, post_process)
  sapply(tok$sqargs, post_process)

}

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

.tomap <- function(...) {
  dots <- c(...)
  map <- dots[seq(2, length(dots), 2)]
  names(map) <- dots[seq(1, length(dots), 2)]
  return(map)
}

# A map of LaTeX expressions to R expressions.
#
# Some special strings are substituted:
# @P@ is a phantom character (used to paste operators)
# @1@ is the first brace argument in a LaTeX expression, \command{1}
# @2@ is the second brace argument in a LaTeX expression, \command{1}{2}
# @S@ is the square argument in a LaTeX expression, \command[S]{1}{2}
# @^@ is the exponent argument (for \int, \sum, etc.)
# @_@ is the subscript argument (for \int, \sum, etc.)
# if the argument is missing, an empty string is substituted instead


.subs <- .tomap(
  # Operators
  "+", "@P@ + @P@",
  "-", "@P@ - @P@",
  "/", "@P@ / @P@",
  "=", "@P@ == @P@",
  "*", "symbol('\052')",
  "\\div", "@P@ %/% @P@",

  "\\pm", "@P@ %+-% @P@",
  "\\neq", "@P@ != @P@",
  "\\geq", "@P@ >= @P@",
  "\\leq", "@P@ <= @P@",
  "\\approx", " @P@ %~~% @P@",
  "\\sim", " @P@ %~% @P@",
  "\\propto", " @P@ %prop% @P@",
  "\\equiv", " @P@ %==% @P@",
  "\\cong", " @P@ %=~% @P@",
  "\\in", " @P@ %in% @P@ ",
  "\\notin", " @P@ %notin% @P@",
  "\\cdot", " @P@ %.% @P@",
  "\\times", "@P@ %*% @P@",
  "\\subset", " @P@ %subset% @P@",
  "\\subseteq", "@P@ %subseteq% @P@",
  "\\nsubset", "@P@ %notsubset% @P@",
  "\\supset", "@P@ %supset% @P@",
  "\\supseteq", "@P@ %supseteq% @P@",
  "\\rightarrow", "@P@ %->% @P@",
  "\\leftarrow", "@P@ %<-% @P@",
  "\\Rightarrow", "@P@ %=>% @P@",
  "\\Leftarrow", "@P@ %<=% @P@",
  "\\forall", "symbol('\\042')",
  "\\exists", "symbol('\\044')",
  "\\%", "symbol('\\045')",
  "\\ast", "symbol('\\053')",
  "\\perp", "symbol('\\136')",
  "\\bullet", "symbol('\\267')",
  "\\Im", "symbol('\\301')",
  "\\Re", "symbol('\\302')",
  "\\wp", "symbol('\\303')",
  "\\otimes", "symbol('\\304')",
  "\\oplus", "symbol('\\305')",
  "\\oslash", "symbol('\\306')",
  "\\surd", "symbol('\\326')",
  "\\neg", "symbol('\\330')",
  "\\vee", "symbol('\\331')",
  "\\wedge", "symbol('\\332')",

  # Square root, sum, prod, integral, etc.
  "\\sqrt", "sqrt(@1@, @S@)",
  "\\sum", "sum(@3@,@1@,@2@)",
  "\\prod", "prod(@3@,@1@,@2@)",
  "\\int", "integral(@3@,@1@,@2@)",
  "\\frac", "frac(@1@, @2@)",
  "\\bigcup", "union(@3@,@1@,@2@)",
  "\\bigcap", "intersect(@3@,@1@,@2@)",
  "\\lim", "lim(@3@, @1@)",

  "\\overset", "atop(@1@, @2@)",
  "\\normalsize", "displaystyle(@1@)",
  "\\small", "scriptstyle(@1@)",
  "\\tiny", "scriptscriptstyle(@1@)",


  # Exponent and subscript
  "^", "@P@ ^ {@1@}",
  "_", "@P@ [ {@1@} ]",

  # Text
  "\\textbf", "bold(@1@)",
  "\\textit", "italic(@1@)",
  "\\mathbf", "bold(@1@)",
  "\\mathit", "italic(@1@)",
  "\\mathrm", "plain(@1@)",

  # Symbols
  "\\infty", " infinity ",
  "\\partial", " partialdiff ",
  "\\cdots", " cdots ",
  "\\ldots", " ldots ",
  "\\degree", " degree ",
  "\\clubsuit", "symbol('\\247')",
  "\\diamondsuit", "symbol('\\250')",
  "\\heartsuit", "symbol('\\251')",
  "\\spadesuit", "symbol('\\252')",
  "\\aleph", "symbol('\\300')",

  "''", " second ",
  "'", " minute ",
  "\\prime", " minute ",
  "\\LaTeX", "L^{phantom()[phantom()[phantom()[scriptstyle(A)]]]}*T[textstyle(E)]*X",
  "\\TeX", "T[textstyle(E)]*X",

  # Decorations
  "\\tilde", "tilde(@1@)",
  "\\hat", "hat(@1@)",
  "\\widehat", "widehat(@1@)",
  "\\widetilde", "widetilde(@1@)",
  "\\bar", "bar(@1@)",
  "\\dot", "dot(@1@)",
  "\\underline", "underline(@1@)",

  # Spacing
  "\\SPACE1@", "paste(' ')",
  "\\SPACE2@", "phantom(0)",
  "\\,", "phantom(0)",
  "\\;", "phantom() ~~ phantom()",

  # Specials
  "\\COMMA@", "','",
  "\\SEMICOLON@", "';'",
  "\\PERIOD@", "'.'",
  "\\SUB_AND_EXP@", "@P@ [@1@] ^{@2@}",

  # Parentheses
  "\\leftPAR@", "bgroup('(', @1@ ",
  "\\rightPAR@", "')')",
  "\\leftBRACE@", "bgroup('{', @1@ ",
  "\\rightBRACE@", "'}')",
  "\\leftSQUARE@", "bgroup('[', @1@ ",
  "\\rightSQUARE@", "']')",
  "\\leftPIPE@", "bgroup('|', @1@ ",
  "\\rightPIPE@", "'|')",
  "\\middlePIPE@", "bgroup('|', @P@, '')",
  "\\leftPERIOD@", "bgroup('', @1@ ",
  "\\rightPERIOD@", "'')",
  "\\lbrack", "paste('[')",
  "\\rbrack", "paste(']')"  
)


#' Converts a token created by TeX() to a string, later to be parsed into an expression (for internal use).
#' 
#' @param x The TeX() token
#' @param ... Additional arguments (ignored)
#' @return A string
#' @export
toString.latextoken <- function(x, ...) {
  tok <- x
  
  if (is.null(tok$prev))
    pre <- 'paste('
  else
    pre <- ','

  tok$args[(length(tok$args) + 1):3] <- ''

  if (!is.null(tok$sym)) {
    if (tok$sym == "^") {
      tok$args = list("", tok$args[[1]], tok$args[[2]])
    } else if (tok$sym == "^_") {
      tok$args = list(tok$args[[2]], tok$args[[1]], tok$args[[3]])
    }
  }

  tok$s <- tok$s %>%
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
  } else if (tok$s != '\\' &&
             str_detect(tok$s, '^\\\\') && !tok$textmode) {
    p <- str_replace(tok$s, "\\\\", "")

    if (length(tok$args) > 0)
      p <-
        str_c(p, ',', str_c(sapply(tok$args, toString), collapse = ','))
  } else if (str_detect(tok$s, "^[0-9]*$")) {
    p <- str_c('\'', tok$s, '\'')
  } else {
    p <- str_c('\'', str_replace_all(tok$s, '\\\\', '\\\\\\\\'), '\'')
  }

  p <- str_c(pre, p)

  if (is.null(tok$succ))
    p <- str_c(p, ')')
  else
    p <- str_c(p, toString(tok$succ))

  #p <- str_replace_all(p, "''", '')
  return(p)
}

# LaTeX expressions in the form \tag_sub^exp
.supsub <-
  c("\\sqrt", "\\sum", "\\int", "\\prod", "\\bigcup", "\\bigcap", "\\lim")

.token <-
  function(s = '', parent = NULL, prev = NULL, ch = '', textmode = TRUE) {
    tok <- new.env()
    tok$s <- s
    tok$args <- list()
    tok$sqarg <- list()
    tok$parent <- parent
    tok$prev <- prev
    tok$textmode <- textmode

    if (!is.null(prev))
      prev$succ <- tok

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
  function(string, output = c('expression', 'character', 'ast')) {
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
      str_replace_all(" \\^ ", "\\^") %>%
      str_replace_all(" _ ", "_")
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
      nextch = if (!is.na(str[i + 1]))
        str[i + 1]
      else
        ''

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
      } else if (ch == "}" || ch == "]" && !prevch == "\\") {
        # Square or brace parameter ended, return to parent node
        token <- token$parent
        needsnew <- TRUE
      } else if (ch == "$" && !prevch == "\\") {
        textmode <- !textmode
        token <-
          .token(
            prev = token, parent = token$parent, ch = ch, textmode = textmode
          )
      } else if (ch == "[" && !prevch == "\\") {
        # Square parameter started, create new child node, put in $sqarg
        nextisarg <- 0
        old <- token
        token <- .token(parent = old, ch = ch, textmode = textmode)
        old$sqarg[[1]] <- token
      } else if (ch == ")" || ch == "(" || ch == "'") {
        if (ch == "'" && prevch == "'")
          next
        if (ch == "'" && nextch == "'")
          token <-
            .token(
              s = "''", parent = token$parent, prev = token, textmode = textmode
            )
        else
          token <-
            .token(
              s = ch, parent = token$parent, prev = token,textmode = textmode
            )
        token <-
          .token(prev = token, parent = token$parent, textmode = textmode)
      } else if (ch == "^" || ch == "_") {
        # Sup or sub. Treat them as new nodes, unless preceded by a LaTeX expression
        # such as \sum, in which case sup and sub should become a parameter
        if (token$s %in% .supsub) {
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
          token$s <- str_c(token$s, ch)
        }
      }

      prevch <- ch
    }

    post_process(root)
    if (output[1] == 'ast')
      return(root)

    
    str <- toString(root)
    exp <- tryCatch(
      parse(text = str), error = function(e) {
        cat("Original string: ", original, "\n")
        cat("Parsed expression: ", str, "\n")
        stop(e)
      }
    )
    
    

    if (output[1] == 'character') {
      return(str)
    } else
      return(exp)
  }

#' Converts a LaTeX string to a \code{\link{plotmath}} expression. Deprecated; use \code{\link{TeX}} instead.
#' @param string A character vector containing LaTeX expressions. Note that any backslashes must be escaped (e.g. "$\\alpha").
#' @param output The returned object, one of "expression" (default, returns a plotmath expression ready for plotting), "text" (returns the expression as a string), and "ast" (returns the tree used to generate the expression).
#'
#' @return Returns an expression (see the \code{output} parameter).
#'
latex2exp <-
  function(string, output = c('expression', 'text', 'ast')) {
    .Deprecated('TeX', 'latex2exp')
    return(TeX(string, output))
  }

#' Converts a LaTeX string to a \code{\link{plotmath}} expression.
#'
#' @param string A character vector containing LaTeX expressions. Note that any backslashes must be escaped (e.g. "$\\alpha").
#' @param output The returned object, one of "expression" (default, returns a plotmath expression ready for plotting), "text" (returns the expression as a string), and "ast" (returns the tree used to generate the expression).
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
  function(string, output = c('expression', 'text', 'ast')) {
    if (missing(string))
      stop("Specify a LaTeX string.")
    return(sapply(string, .parseTeX, output = output))
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
      text(col, rows - row, sym, family = 'mono', pos = 4)

      try(text(col + 0.6, rows - row,
               TeX(sym), pos = 4, offset = offset))
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
    "Numbers: $0.05$, $0.03$, $0.005^{0.002}_{0.01}$"
  )

  x <- 0
  y <- seq(0.95, 0.05, length.out = length(examples))

  text(
    0.5, y, sapply(examples, function(e)
      str_replace_all(e, "\\\\", "\\\\\\\\")), pos = 2, cex = 0.7, family = 'mono'
  )
  text(0.5, y, TeX(examples), pos = 4)
  return(TRUE)
}
