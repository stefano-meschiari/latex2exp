library(stringr)
library(magrittr)

# Utility function to print the AST
print.latextoken <- function(tok, level=0, n=1, ch='') {
    ind <- rep(' ', level)
    cat(ind, n, '. \'', tok$s, '\'', '\n', sep='')
    sapply(tok$args, print, level=level+1, n=1)
    sapply(tok$sqarg, print, level=level+1, n=1)
    if (!is.null(tok$succ))
        print(tok$succ, level, n+1)
}

plot.expression <- function(e) {
    plot(0, 0, type='n', axes=F, xlab='', ylab='')
    text(0, 0, e)
}

.tomap <- function(...) {
    list <- list(...)
    map <- c()
    names <- c()
    for (i in seq(1, length(list), 2)) {
        names <- c(names, list[[i]])
        map <- c(map, list[[i+1]])
    }
    names(map) <- names
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
    "\\pm", "@P@ %+-% @P@",
    "\\neq", "@P@ %!=% @P@",
    "\\geq", "@P@ %>=% @P@",
    "\\leq", "@P@ %<=% @P@",
    "\\approx", " @P@ %~~% @P@",
    "\\sim", " @P@ %~% @P@",
    "\\propto", " @P@ %prop% @P@",
    "\\equiv", " @P@ %==% @P@",
    "\\in", " @P@ %in% @P@ ",
    "\\notin", " @P@ %notin% @P@",
    "\\cdot", " @P@ %.% @P@",
    "\\times", "@P@ %*% @P@",
    "\\subset", " @P@ %subset% @P@",
    "\\subseteq", "@P@ %subseteq% @P@",
    "\\nsubset", "@P@ %notsubset% @P@",
    "\\supset", "@P@ %supset% @P@",
    "\\supseteq", "@P@ %supseteq% @P@",
    
    
    # Square root, sum, prod, integral, etc.
    "\\sqrt", "sqrt(@1@, @S@)",
    "\\sum", "sum(@3@,@1@,@2@)",
    "\\prod", "prod(@3@,@1@,@2@)",
    "\\int", "int(@3@,@1@,@2@)",
    "\\frac", "frac(@1@, @2@)",
    
    # Exponent and subscript
    "^", "@P@ ^ {@1@}",
    "_", "@P@ [ {@1@} ]",
    
    # Text
    "\\text", "'@1@'",
    "\\textbf", "bold(@1@)",
    "\\textit", "italic(@1@)",
    "\\mathbf", "bold(@1@)",
    "\\mathit", "italic(@1@)",
    "\\mathrm", "plain(@1@)",

    # Symbols
    "\\infty", " infinity ",
    "\\partial", " partial ",
    "\\cdots", " cdots ",
    "\\ldots", " ldots ",

    # Decorations
    "\\tilde", "tilde(@1@)",
    "\\hat", "hat(@1@)",
    "\\widehat", "widehat(@1@)",
    "\\widetilde", "widetilde(@1@)",
    "\\bar", "bar(@1@)",
    "\\dot", "dot(@1@)",
    "\\underline", "underline(@1@)",

    # Parentheses
    "\\leftPAR", "bgroup('(', @1@ ",
    "\\rightPAR", "')')",
    "\\leftBRACE", "bgroup('{', @1@ ",
    "\\rightBRACE", "'}')",
    "\\leftSQUARE", "bgroup('[', @1@ ",
    "\\rightSQUARE", "']')"
)

toString.latextoken <- function(tok) {
    if (is.null(tok$prev))
        pre <- 'paste('
    else
        pre <- ','

    tok$args[(length(tok$args)+1):3] <- ''
    
    if (!is.null(tok$sym)) {
        if (tok$sym == "^") {
            tok$args = list("", tok$args[[1]], tok$args[[2]])
        } else if (tok$sym == "^_") {
            tok$args = list(tok$args[[2]], tok$args[[1]], tok$args[[3]])
        }
    }
    
    if (!is.na(.subs[tok$s])) {
        p <- .subs[tok$s] %>%
            str_replace_all("@P@", 'phantom()') %>%
                str_replace_all("@1@", if (length(tok$args) > 0) toString(tok$args[[1]]) else "") %>%
                    str_replace_all("@2@", if (length(tok$args) > 1) toString(tok$args[[2]]) else "") %>%
                        str_replace_all("@S@", if (length(tok$sqargs) > 1) toString(tok$sqargs[[1]]) else "") %>%
                            str_replace_all("@3@", if (length(tok$args) > 2) toString(tok$args[[3]]) else "") 
        
    } else if (tok$s != '\\' && str_detect(tok$s, '\\\\')) {
        p <- str_replace(tok$s, "\\\\", "")
    } else if (str_detect(tok$s, "^[0-9]*$")) {
        p <- tok$s
    } else {
        p <- str_c('\'', tok$s, '\'')
    }
    
    p <- str_c(pre, p)
    
    if (is.null(tok$succ))
        p <- str_c(p, ')')
    else
        p <- str_c(p, toString(tok$succ))
    
    return(p)
}

# LaTeX expressions in the form \tag_sub^exp
.supsub <- c("\\sqrt", "\\sum", "\\int", "\\prod")
# LaTeX expressions that will preserve spaces
.textmode <- c("\\text", "\\textit", "\\textbf", "\\mbox")

.token <- function(s='', parent=NULL, prev=NULL) {
    tok <- new.env()
    tok$s <- s
    tok$args <- list()
    tok$sqarg <- list()
    tok$parent <- parent
    tok$prev <- prev
    if (!is.null(prev))
        prev$succ <- tok
    
    tok$r <- ""
    class(tok) <- 'latextoken'
    return(tok)
}

## Takes a LaTeX string, or a vector of LaTeX strings, and converts it into
## the closest plotmath expression possible.
##
## Returns an expression by default; can either return 'text' (return the expression
## as a string) or 'ast' (returns the tree as parsed from the LaTeX string; useful for debug).
.parseTeX <- function(string, output=c('expression', 'text', 'ast')) {
    # Create the root node
    root <- .token()
    
    token <- root

    # Treat \left( / \right) and company specially in order to not have to special-case them in the
    # parser
    string <- string %>%
        str_replace_all('\\\\left\\{', '\\\\leftBRACE{') %>%
            str_replace_all('\\\\left\\[', '\\\\leftSQUARE{') %>%
                str_replace_all('\\\\left\\(', '\\\\leftPAR{') %>%
                    str_replace_all('\\\\right\\]', '}\\\\rightBRACE ') %>%
                        str_replace_all('\\\\right\\]', '}\\\\rightSQUARE ') %>%
                            str_replace_all('\\\\right\\)', '}\\\\rightPAR ') 

    # Split the input into characters
    str <- str_split(string, '')[[1]]
    prevch <- ''

    # If within a tag contained in .textmode, preserve spaces
    textmode <- FALSE

    nextisarg <- 0
    needsnew <- FALSE

    
    for (i in 1:length(str)) {
        ch = str[i]
        if (token$s != "" && ch == '\\') {
            # Char is \ (start a new node, unless preceded by another \)
            if (prevch != '\\') {
                old <- token
                if (nextisarg == 2)
                    nextisarg <- 0
                if (nextisarg == 1) {
                    nextisarg <- 2
                    ntoken <- .token(parent=token, s=ch)
                    token$args[[length(token$args)+1]] <- ntoken
                    token <- ntoken
                } else
                    token <- .token(s='\\', parent=token$parent, prev=old)
            } else {
                ch <- ''
            }
        } else if (ch == " " &&! textmode) {
            # Ignore spaces, unless in text mode
            if (prevch != ' ' && !(nextisarg == 0)) {
                old <- token
                token <- .token(parent=token$parent, prev=old)
            }
        } else if (ch == "{") {
            # Brace parameter starting, create new child node
            if (token$s %in% .textmode)
                textmode <- TRUE
            nextisarg <- 0
            
            old <- token
            token <- .token(parent=old)
            old$args[[length(old$args)+1]] <- token
        } else if (ch == "}" || ch == "]") {
            # Square or brace parameter ended, return to parent node
            token <- token$parent
            textmode <- FALSE
            needsnew <- TRUE
        } else if (ch == "[") {
            # Square parameter started, create new child node, put in $sqargs
            nextisarg <- 0            
            old <- token
            token <- .token(parent=old)
            old$sqargs[[length(old$sqargs)+1]] <- token        
        } else if (ch == "^" || ch == "_") {
            # Sup or sub. Treat them as new nodes, unless preceded by a LaTeX expression
            # such as \sum, in which case sup and sub should become a parameter
            if (token$s %in% .supsub) {
                nextisarg <- 1
                token$sym <- str_c(token$sym, ch)
            } else {
                old <- token
                token <- .token(prev=old, s=ch)
                nextisarg <- 1
            }
        } else {
            # Any other character
            if (nextisarg == 1) {
                token$args[[length(token$args)+1]] <- .token(s=ch, parent=token)
                if (!is.na(str[i+1]) && ((str[i+1] == '^') || (str[i+1] == '_'))) {

                } else {
                    if (nextisarg == 1) {
                        nextisarg <- 0
                        token <- .token(prev=token)
                    }
                }
            } else {
                if (needsnew) {
                    token <- .token(prev=token)
                } 
                token$s <- str_c(token$s, ch)
            }
        }
        
        prevch <- ch
    }

    if (output[1] == 'ast')
        return(root)
    
    str <- toString(root)
    exp <- tryCatch(parse(text=str), error=function(e) {
        cat("Parsed expression: ", str, "\n")
        stop(e)
    })

    if (output[1] == 'text') {
        return(str)
    } else
        return(exp)
}

latex2exp <- function(string, output=c('expression', 'text', 'ast')) {    
    return(sapply(string, .parseTeX, output=output))
}
