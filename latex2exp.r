library(stringr)
library(magrittr)
digits <- as.character(0:9)

# Utility function to print the AST
print.latextoken <- function(tok, level=0, n=1, ch='') {
    ind <- rep(' ', level)
    cat(ind, n, ch, '. \'', tok$s, '\' ', tok$textmode, ' ', tok$ch, '\n', sep='')
    sapply(tok$args, print, level=level+1, n=1, ch='{')
    sapply(tok$sqarg, print, level=level+1, n=1, ch='[')

    if (!is.null(tok$succ))
        print(tok$succ, level, n+1)
}

# To test conversion, use plot(latex2token('...'))
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
    
    # Square root, sum, prod, integral, etc.
    "\\sqrt", "sqrt(@1@, @S@)",
    "\\sum", "sum(@3@,@1@,@2@)",
    "\\prod", "prod(@3@,@1@,@2@)",
    "\\int", "integral(@3@,@1@,@2@)",
    "\\frac", "frac(@1@, @2@)",
    
    # Exponent and subscript
    "^", "@P@ ^ {@1@}",
    "_", "@P@ [ {@1@} ]",
    
    # Text
    "\\text", "@1@",
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
    "''", " second ",
    "'", " minute ",
    "\\prime", " second ",

    # Decorations
    "\\tilde", "tilde(@1@)",
    "\\hat", "hat(@1@)",
    "\\widehat", "widehat(@1@)",
    "\\widetilde", "widetilde(@1@)",
    "\\bar", "bar(@1@)",
    "\\dot", "dot(@1@)",
    "\\underline", "underline(@1@)",

    # Spacing
    "\\,", "phantom(0)",
    "\\;", "phantom() ~~ phantom()",

    # Specials
    "\\COMMA@", "','",
    "\\SEMICOLON@", "';'",
    "\\PERIOD@", "'.'",
    
    
    # Parentheses
    "\\leftPAR@", "bgroup('(', @1@ ",
    "\\rightPAR@", "')')",
    "\\leftBRACE@", "bgroup('{', @1@ ",
    "\\rightBRACE@", "'}')",
    "\\leftSQUARE@", "bgroup('[', @1@ ",
    "\\rightSQUARE@", "']')"
)

toString.latextoken <- function(tok, textmode=FALSE) {
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

    tok$s <- tok$s %>%
            str_replace_all("\\\\COMMA@", ',') %>%
                str_replace_all("\\\\PERIOD@", '.') %>%
                    str_replace_all("\\\\SEMICOLON@", ';')
        

    if (!is.na(.subs[tok$s])) {
        p <- .subs[tok$s] %>%
            str_replace_all("@P@", 'phantom()') %>%
                str_replace_all("@1@", if (length(tok$args) > 0) toString(tok$args[[1]]) else "") %>%
                    str_replace_all("@2@", if (length(tok$args) > 1) toString(tok$args[[2]]) else "") %>%
                        str_replace_all("@S@", if (length(tok$sqarg) > 1) toString(tok$sqarg[[1]]) else "") %>%
                            str_replace_all("@3@", if (length(tok$args) > 2) toString(tok$args[[3]]) else "") 
        
    } else if (tok$s != '\\' && str_detect(tok$s, '^\\\\') && !tok$textmode) {
        p <- str_replace(tok$s, "\\\\", "")
        
        if (length(tok$args) > 0)            
            p <- str_c(p, ',', str_c(sapply(tok$args, toString), collapse=','))
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

    p <- str_replace_all(p, "''", '')
    return(p)
}

# LaTeX expressions in the form \tag_sub^exp
.supsub <- c("\\sqrt", "\\sum", "\\int", "\\prod")
# LaTeX expressions that will preserve spaces
.textmode <- c("\\text", "\\textit", "\\textbf", "\\mbox")

## TODO: Figure out how to treat "peer" and "children" exp
## \\int_{ 0 a b \\text{C} d}
## \\int_{ 0 a b \\text{C} d} 2 not wokring


.token <- function(s='', parent=NULL, prev=NULL, ch='') {
    tok <- new.env()
    tok$s <- s
    tok$args <- list()
    tok$sqarg <- list()
    tok$parent <- parent
    tok$prev <- prev
    tok$textmode <- FALSE
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
## Returns an expression by default; can either return 'text' (return the expression
## as a string) or 'ast' (returns the tree as parsed from the LaTeX string; useful for debug).
.parseTeX <- function(string, output=c('expression', 'text', 'ast')) {

    original <- string
    # Create the root node
    root <- .token()
    root$parent = list(textmode=FALSE)
    
    token <- root

    # Treat \left( / \right) and company specially in order to not have to special-case them in the
    # parser
    string <- string %>%
        str_replace_all('\\\\left\\{', '\\\\leftBRACE@{') %>%
            str_replace_all('\\\\left\\[', '\\\\leftSQUARE@{') %>%
                str_replace_all('\\\\left\\(', '\\\\leftPAR@{') %>%
                    str_replace_all('\\\\right\\]', '}\\\\rightBRACE@ ') %>%
                        str_replace_all('\\\\right\\]', '}\\\\rightSQUARE@ ') %>%
                            str_replace_all('\\\\right\\)', '}\\\\rightPAR@ ') %>%
                                str_replace_all(",", "\\\\COMMA@") %>%
                                str_replace_all(";", "\\\\SEMICOLON@") %>%
                                str_replace_all("\\.", "\\\\PERIOD@") 


    # Split the input into characters
    str <- str_split(string, '')[[1]]
    prevch <- ''

    # If within a tag contained in .textmode, preserve spaces
    nextisarg <- 0
    needsnew <- FALSE

    
    for (i in 1:length(str)) {
        ch = str[i]
        nextch = if (!is.na(str[i+1])) str[i+1] else ''
        
        if (token$s != "" && ch == '\\') {
            # Char is \ (start a new node, unless preceded by another \)
            if (prevch != '\\') {
                old <- token
                needsnew <- FALSE
                if (nextisarg == 2) {
                    nextisarg <- 0
                    token <- .token(s='\\', prev=token$parent, parent=token$parent, ch=ch)
                } else if (nextisarg == 1) {
                    nextisarg <- 2
                    ntoken <- .token(parent=token, s=ch, ch=ch)
                    token$args[[length(token$args)+1]] <- ntoken
                    token <- ntoken
                } else
                    token <- .token(s='\\', parent=token$parent, prev=old, ch=ch)
            } else {
                ch <- ''
            }
        } else if (ch == " " && !token$parent$textmode) {
            # Ignore spaces, unless in text mode
            if (prevch != ' ') {
                if (nextisarg == 1) {
                    nextisarg <- 0
                    token <- token$parent
                    token <- .token(prev=token, parent=token, ch=ch)                    
                } else {
                    old <- token
                    token <- .token(prev=token, parent=token$parent, ch=ch)
                    nextisarg <- 0
                }
            }
        } else if (ch == "{") {
            # Brace parameter starting, create new child node
                
            nextisarg <- 0
            
            old <- token
            token <- .token(parent=old, ch=ch)
            old$args[[length(old$args)+1]] <- token
            if (token$parent$s %in% .textmode) {
                token$parent$textmode <- TRUE
            }
        } else if (ch == "}" || ch == "]") {
            # Square or brace parameter ended, return to parent node
            token <- token$parent
            needsnew <- TRUE
        } else if (ch == "[") {
            # Square parameter started, create new child node, put in $sqarg
            nextisarg <- 0            
            old <- token
            token <- .token(parent=old, ch=ch)
            old$sqarg[[1]] <- token        
        } else if (ch == ")" || ch == "(" || ch == "'") {
            if (ch == "'" && prevch == "'")
                next
            if (ch == "'" && nextch == "'")
                token <- .token(s="''", parent=token$parent, prev=token)
            else
                token <- .token(s=ch, parent=token$parent, prev=token)
            token <- .token(prev=token, parent=token$parent)
        } else if (ch == "^" || ch == "_") {
            # Sup or sub. Treat them as new nodes, unless preceded by a LaTeX expression
            # such as \sum, in which case sup and sub should become a parameter
            if (token$s %in% .supsub) {
                token$sym <- str_c(token$sym, ch)
            } else {
                old <- token
                token <- .token(prev=old, s=ch, parent=old$parent, ch=ch)
            }
            
            nextisarg <- 1            
        } else {
            # Any other character
            if (nextisarg == 1) {
                token$args[[length(token$args)+1]] <- .token(s=ch, parent=token, ch=ch)
                if (nextch == '^' || nextch == '_') {
                    
                } else {
                    if (nextisarg == 1) {
                        nextisarg <- 0
                        token <- .token(prev=token, parent=token$parent, ch=ch)
                    }
                }
            } else {
                if (needsnew) {
                    token <- .token(prev=token, parent=token$parent, ch=ch)
                    needsnew <- FALSE
                } 
                token$s <- str_c(token$s, ch)
            }
        }

        prevch <- ch
#        cat(ch, token$s, '\n')
    }

    if (output[1] == 'ast')
        return(root)
    
    str <- toString(root)
    exp <- tryCatch(parse(text=str), error=function(e) {
        cat("Original string: ", original, "\n")
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

latex2exp.supported <- function() {
    return(names(.subs) %>%
                 Filter(function(d) {
                     return(!str_detect(d, "@$") && str_detect(d, '^\\\\'))
                 }, .)
             )
    
}

latex2exp.examples <- function() {
    plot.new()
    plot.window(xlim=c(0, 1), ylim=c(0, 1))
    examples <- c(
        "\\alpha_{\\beta}^{\\gamma}",
        "\\frac{\\partial \\bar{x}}{\\partial t}",
        "\\sum_{i=1}^{10} x_i \\beta^i",
        "\\prod_{i = 1}^{100} x^i",
        "\\left(\\int_{0}^{1} \\sin(x) dx \\right)",
        "\\text{The value of the fine structure constant is } \\alpha \\approx \\frac{1}{137}.",
        "\\nabla \\times \\bar{x}\\text{ and }\\nabla \\cdot \\bar{x}",
        "\\sqrt[\\alpha\\beta]{x^2}",
        "\\textbf{Bold}\\text{ and }\\textit{italic}\\text{ text! }"
    )

    x <- 0
    y <- seq(0.95, 0.05, length.out=length(examples))

    text(0.5, y, examples, pos=2, cex=0.75)
    text(0.5, y, latex2exp(examples), pos=4)
}
