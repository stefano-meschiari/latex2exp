require(stringr)


.sub <- list(
    "\\\\,", ", phantom(.), ",
    "\\\\:", " ~~ ",
    "=", "==",

    "([[:alnum:]\\}]){1}\\\\{1}", "\\1 \\\\",
    "([[:alnum:]\\}]) ([\\\\])", "\\1 , \\2",
    "\\}", "} ",
    "_([[:alnum:]]){1}", " [{\\1}] ",
    "_([\\\\[:alnum:]]+?)[ \\{\\}]", " [\\1] ",
    "\\\\", " \\\\",
    "\\\\sum_\\{(.*?)\\}\\^\\{(.*?)\\}", "sum(,{\\1}, {\\2}),",    
    "\\\\sum_\\{(.*?)\\}", "sum(,{\\1}, ),",
    "\\\\sum", "sum(,,,),",
    "\\\\prod_\\{(.*?)\\} \\^\\{(.*?)\\}", "prod(,{\\1}, {\\2}),",    
    "\\\\prod_\\{(.*?)\\}", "prod(,{\\1}, ),",
    "\\\\prod", "prod(,,,),",
    "\\\\int_\\{(.*?)\\} \\^\\{(.*?)\\}", ",integral(,{\\1}, {\\2}),",    
    "\\\\int", ",integral(,,),",
    "\\\\cdot", "phantom() %.% phantom()",
    "\\\\pm", " %+-% ",
    "\\\\neq", " %!=% ",
    "\\\\geq", " %>=% ",
    "\\\\leq", " %<=% ",
    
    "\\\\partial", " ,partialdiff, ",
    "\\\\approx", " phantom() %~~% phantom()",
    "\\\\sim", " %~% ",
    "\\\\propto", " %prop% ",
    "\\\\equiv", " %==% ",
    "\\\\infty", " infinity ",
    "\\\\in", " phantom() %in% phantom() ",
    "\\\\notin", " %notin% ",
    "\\\\sqrt\\{(.*?)\\}", " sqrt(paste(\\1)) ",
    "\\\\sqrt\\[(.*?)\\]\\{(.*?)\\}", " sqrt(paste(\\2), paste(\\1)) ",
    "\\\\left\\((.*?)\\\\right\\)", " bgroup('(', paste(\\1), ')') ",
    "\\\\text\\{(.*?)\\}", ",'\\1',",
    "\\\\textbf\\{(.*?)\\}", "bold(\\1)",
    "\\\\textit\\{(.*?)\\}", "italic(\\1)",
    "\\\\mathbf\\{(.*?)\\}", "bold(\\1)",
    "\\\\mathit\\{(.*?)\\}", "italic(\\1)",
    "\\\\mathrm\\{(.*?)\\}", "plain(\\1)",
    "\\\\frac\\{(.*?)\\} \\{(.*?)\\}", "frac(paste(\\1), paste(\\2))",
    "\\\\bar\\{(.*?)\\}", "bar(\\1)",
    "\\\\dot\\{(.*?)\\}", "dot(\\1)",
    "\\\\widehat\\{(.*?)\\}", "widehat(\\1)",
    "\\\\tilde\\{(.*?)\\}", "tilde(\\1)",
    "\\\\hat\\{(.*?)\\}", "hat(\\1)",
    "\\\\underline\\{(.*?)\\}", "underline(\\1)",
    "\\\\times", " phantom() %*% phantom() ",
    "_\\{(.*?)\\}", " [paste(\\1)] ",
    "\\{\\}", ""
    
)

str_insert <- function(string, where, insert="") {
    return(str_c(substr(string, 1, where-1), insert, substr(string, where, nchar(string))))
}

latex2exp <- function(string, return.expression=TRUE, .debug=FALSE) {
    return(sapply(string, function(string) {
        orig <- string
        string <- str_c(" ", str_trim(string), " ")

        cur <- string
        for (i in seq(1, length(.sub), 2)) {
            if (str_detect(string, .sub[[i]])) {
                str <- str_replace_all(string, .sub[[i]], .sub[[i+1]])
                if (.debug)
                    cat(string, '->', str, ' [', .sub[[i]], ']\n')
                string <- str
            }
        }

        string <- str_trim(str_replace_all(string, "\\\\", " "))
        string <- str_replace(string, "\\*$", "")
        string <- paste0('paste(', string, ')')
        trials <- 0
        while (trials < 4) {
            tryCatch({ parse(text=string); break }, error=function(e) {
                where <- str_match(e$message, "([[:digit:]]+?): ")[1,2]
                str_corrected <- str_insert(string, as.numeric(where), "*")
                if (.debug)
                    cat(string, '->', str_corrected, '\n')
                string <<- str_corrected
                trials <<- trials + 1
            })
            
        }
        
        tryCatch(parse(text=string), error=function(e) {
            e$message = str_c(e$message, "\nOriginal LaTeX string: ", orig, "\nCurrent translation: ", string)
            stop(e)
        })
        
        if (return.expression) {
            return(parse(text=string))
        } else {
            return(string)
        }
    }))
}

latex2exp.examples <- function() {
    plot.new()
    plot.window(xlim=c(0, 1), ylim=c(0, 1))
    examples <- c(
        "\\alpha_{\\beta}^{\\gamma}",
        "\\frac{\\partial \\bar{x}}{\\partial t}",
        "\\sum_{i}x_i \\beta^i",
        "\\prod_{i = 1}^{100} x^i",
        "\\left(\\int_{0}^{1} \\sin(x) dx \\right)",
        "\\text{The value of the fine structure constant is} \\approx \\frac{1}{137}\\text{.}",
        "\\nabla \\times \\bar{x}\\text{ and }\\nabla \\cdot \\bar{x} ",
        "\\sqrt[\\alpha\\beta]{x^2}",
        "\\textbf{Bold}\\text{ and }\\textit{italic}\\text{ text! }"
    )

    x <- 0
    y <- seq(0.95, 0.05, length.out=length(examples))

    text(0.5, y, examples, pos=2, cex=0.75)
    text(0.5, y, latex2exp(examples), pos=4)
}

str_replacef_all <- function(string, pattern, sub) {
    isf <- is.function(sub)
    
    return(sapply(string, function(string) {
        m <- str_locate_all(string, pattern)[[1]]
        i <- 1
        ret <- c()
        subs <- apply(m, 1, function(idx) {
            ret <<- c(ret, substr(string, i, idx[1]-1))
            i <<- idx[2]+1
            
            if (idx[1] <= idx[2]) {
                match <- substring(string, idx[1], idx[2])
                if (isf)
                    subst <- sub(match)
                else
                    subst <- sub
                
                if (subst != FALSE) {
                    match <- str_replace(match, pattern, subst) 
                }
                ret <<- c(ret, match)
            }
        })
        
        ret <- c(ret, substr(string, i, nchar(string)))
        return(paste0(ret, collapse=''))
    }))
}

