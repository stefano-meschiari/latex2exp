.token2 <- function(command, text_mode) {
  tok <- new.env()
  tok$args <- list()
  tok$optional_arg <- list()
  tok$sup_arg <- list()
  tok$sub_arg <- list()
  tok$children <- list()
  tok$command <- command
  tok$is_command <- str_starts(command, fixed("\\"))
  tok$text_mode <- text_mode
  tok$left_operator <- tok$right_operator <- FALSE
  class(tok) <- "latextoken2"
  tok
}

clone_token <- function(tok) {
  if (is.list(tok)) {
    return(lapply(tok, clone_token))
  }
  new_tok <- .token2(tok$command, tok$text_mode)
  # clone all the linked tokens
  for (field in c("children", "args", "optional_arg", "sup_arg", "sub_arg")) {
    new_tok[[field]] <- lapply(tok[[field]], clone_token)
  }
  for (field in c("is_command", "left_operator", "right_operator")) {
    new_tok[[field]] <- tok[[field]]
  }
  new_tok
}

.find_substring <- function(string, boundary_characters) {
  pattern <- str_c("^[^",
                   str_c("\\", boundary_characters, collapse=""),
                   "]+")
  ret <- str_match(string, pattern)[1,1]
  if ((is.na(ret) || nchar(ret) == 0) && nchar(string) > 0) {
    str_sub(string, 1, 1)
  } else {
    ret
  }
}

.find_substring_matching <- function(string, opening, closing) {
  chars <- str_split(string, "")[[1]]
  depth <- 0
  start_expr <- -1
  
  for (i in seq_along(chars)) {
    if (chars[i] == opening) {
      if (depth == 0) {
        start_expr <- i
      }
      depth <- depth + 1
    } else if (chars[i] == closing) {
      depth <- depth - 1
      if (depth == 0) {
        return(str_sub(string, start_expr+1, i-1))
      }
    }
  }
  if (depth != 0) {
    stop("Unmatched '", opening, "' (opened at position: ", start_expr, ") while parsing '", string, "'")
  } else {
    return(string)
  }
}


parse_latex <- function(latex_string,
                        text_mode=TRUE,
                        depth=0,
                        pos=0,
                        parent=NULL) {
  input <- latex_string

  if (depth == 0) {
    validate_input(latex_string)
  }
  if (depth == 0) {
    latex_string <- latex_string %>%
      str_replace_fixed('\\|', '\\@pipe ') %>%
      
      str_replace_all("\\\\['\\$\\{\\}\\[\\]\\!\\?\\_\\^]", function(char) {
        str_c("\\ESCAPED@", 
              as.integer(charToRaw(str_replace_fixed(char, "\\", ""))),
              "{}")
      }) %>%
      
      str_replace_all("([^\\\\]?)\\\\,", "\\1\\\\@SPACE1{}") %>%
      str_replace_all("([^\\\\]?)\\\\;", "\\1\\\\@SPACE2{}") %>%
      str_replace_all("([^\\\\]?)\\\\\\s", "\\1\\\\@SPACE2{}")
    
    cat_trace("String with special tokens substituted: ", latex_string)
  }
  
  

  i <- 1

  tokens <- list()
  token <- NULL
  
  withCallingHandlers({
    while (i <= nchar(latex_string)) {
      # Look at current character, previous character, and next character
      ch <- str_sub(latex_string, i, i)
      prevch <- if (i == 1) "" else str_sub(latex_string, i-1, i-1)
      nextch <- if (i == nchar(latex_string)) "" else str_sub(latex_string, i+1, i+1)
      
      # LaTeX string left to be processed
      current_fragment <- str_sub(latex_string, i)
      
      cat_trace("Position: ", i, " ch: ", ch, " next: ", nextch, 
                " current fragment: ", current_fragment, 
                " current token: ", token$command,
                " text mode: ", text_mode)
      
      
      separators <- if (text_mode) {
        .base_separators
      } else {
        .math_separators
      }
      
      # We encountered a backslash. Continue until we encounter
      # another backslash, or a separator, or a dollar
      if (ch == "\\" && nextch != "\\") {
        # Continue until we encounter a separator
        current_fragment <- str_sub(current_fragment, 2)
        
        command <- str_c("\\",
                         .find_substring(current_fragment, separators))
        
        token <- .token2(command, text_mode)
        tokens <- c(tokens, token)
        
        i <- i + nchar(command)
      } else if (!text_mode && 
                 !is.null(token) && 
                 token$command %in% c("\\left", "\\right") &&
                 ch %in% c(".", "{", "}", "[", "]", "(", ")", "|")) {
        # a \\left or \\right command has started. eat up the next character
        # and append it to the command.
        token$command <- str_c(token$command, ch)
        i <- i + 1
      } else if (ch == "{") {
        argument <- .find_substring_matching(current_fragment,
                                             "{",
                                             "}")
        if (is.null(token)) {
          token <- .token2("", text_mode)
          tokens <- c(tokens, token)
        }
        
        args <- parse_latex(argument, text_mode=text_mode,
                            depth=depth+1, parent=token, pos=i)
        if (length(args) > 0) {
          token$args <- c(token$args, list(args))
        }
        # advance by two units (the content of the braces + two braces)
        i <- i + nchar(argument) + 2
      }  else if (ch == "[") {
        argument <- .find_substring_matching(current_fragment,
                                             "[",
                                             "]")
        if (is.null(token)) {
          token <- .token2("", text_mode)
          tokens <- c(tokens, token)
        }
        
        token$optional_arg <- c(
          token$optional_arg,
          parse_latex(argument, text_mode=text_mode,
                      depth=depth+1, parent=token, pos=i)
        )
        
        # advance by two units (the content of the braces + two braces)
        i <- i + nchar(argument) + 2
      } else if (ch %in% c("^", "_") && !text_mode) {
        if (is.null(token)) {
          token <- .token2("", text_mode)
          tokens <- c(tokens, token)
        }
        
        arg_type <- if (ch == "^") "sup_arg" else "sub_arg"
        
        advance <- 1
        
        # If there are spaces after the ^ or _ character,
        # consume them and advance past the spaces
        if (nextch == " ") {
          n_spaces <- str_match(str_sub(current_fragment, 2), "\\s+")[1,1]
          advance <- advance + nchar(n_spaces)
          nextch <- str_sub(current_fragment, advance + 1, advance+1)
        } 
        
        # Sub or sup arguments grouped with braces. This is easy!
        if (nextch == "{") {
          argument <- .find_substring_matching(str_sub(current_fragment, advance+1),
                                               "{",
                                               "}")
          
          # advance by two units (the content of the braces + two braces)
          advance <- advance + nchar(argument) + 2
        } else if (nextch == "\\") {
          # Advance until a separator is found
          argument <- str_c("\\",
                            .find_substring(str_sub(current_fragment, advance+2), separators))
          advance <- advance + nchar(argument)
        } else {
          argument <- str_sub(current_fragment, advance+1, advance+1)
          advance <- advance + nchar(argument)
        }
        
        token[[arg_type]] <- parse_latex(argument, text_mode=text_mode,
                                         depth=depth+1, parent=token, pos=i)
        
        i <- i + advance
      } else if (ch == "$") {
        # Switch between "text mode" and "math mode", and advance.
        text_mode <- !text_mode  
        if (text_mode) {
          token <- NULL
        }
        i <- i + 1
      } else if (ch == " ") {
        if (text_mode) {
          
          if (is.null(token) || token$is_command) {
            token <- .token2(" ", text_mode)
            tokens <- c(tokens, token)
          } else {
            token$command <- str_c(token$command, " ")
          }
        }
        i <- i + 1
      } else if (ch == ",") {
        if (text_mode && !is.null(token)) {
          token$command <- str_c(token$command, ",")
        } else {
          token <- .token2(",", text_mode)
          tokens <- c(tokens, token)
        }
        i <- i + 1
      } else {
        # Other characters:
        if (text_mode) {
          # either add to a string-type token...
          if (is.null(token) || !token$text_mode || token$is_command) {
            token <- .token2("", TRUE)
            tokens <- c(tokens, token)
          }
          if (ch == "'") {
            ch <- "\\'"
          }
          token$command <- str_c(token$command, ch)
          i <- i+1
        } else if (ch %in% c("?", "!", "@", ":", ";")) {
          # ...or escape them to avoid introducing illegal characters in the
          # plotmath expression...
          token <- .token2(str_c("\\ESCAPED@", utf8ToInt(ch)), TRUE)
          tokens <- c(tokens, token)
          i <- i + 1
        } else if (ch == "'") {
          # special-case single quotes in math mode to render them as \prime
          # or \second
          if (nextch == "'") {
            token <- .token2("\\second", TRUE)
            i <- i + 2
          } else {
            token <- .token2("\\prime", TRUE)
            i <- i + 1
          }
          tokens <- c(tokens, token)
        } else {
          # or, just add everything to a single token
          str <- .find_substring(current_fragment, separators)
        
          # If in math mode, ignore spaces
          token <- .token2(str_replace_all(str,
                                           "\\s+", ""),
                           text_mode)
          tokens <- c(tokens, token)
          i <- i + nchar(str)
        }
      }
    }
    
  }, error=function(e) {
    token_command <- if (is.null(token)) {
      ""
    } else {
      token$command
    }
    message("Error while parsing LaTeX string: ", input)
    message("Parsing stopped at position ", i + pos)
    if (!is.null(token)) {
      message("Last token parsed:", token$command)
    }
    if (!is.null(parent)) {
      message("The error happened within the arguments of :", parent$command, "\n")
    }
  })
  
  if (depth == 0) {
    root <- .token2("<root>", TRUE)
    root$children <- tokens
    root
  } else {
    tokens
  }
}

#' Renders a LaTeX tree
#' 
#' Returns a string that is a valid plotmath expression, given a LaTeX tree
#' returned by \code{parse_latex}.
#'
#' @param tokens tree of tokens
#' @param user_defined any custom definitions of commands passed to \code{\link{TeX}}
#' @param hack_parentheses render parentheses using \code{group('(', phantom(), '.')} and
#'                         \code{group(')', phantom(), '.')}. This is useful to return
#'                         valid expressions when the LaTeX source contains mismatched
#'                         parentheses, but makes the returned expression much 
#'                         less tidy.
#' @return String that should be parseable as a valid plotmath expression
render_latex <- function(tokens, user_defined=list(), hack_parentheses=FALSE) {
  if (!is.null(tokens$children)) {
    return(render_latex(tokens$children, user_defined, hack_parentheses=hack_parentheses))
  }
  translations <- c(user_defined, latex_supported_map)
  
  for (tok_idx in seq_along(tokens)) {
    tok <- tokens[[tok_idx]]
    tok$skip <- FALSE
    
    tok$rendered <- if (str_detect(tok$command, "^\\\\ESCAPED@")) {
      # a character, like '!' or '?' was escaped as \\ESCAPED@ASCII_SYMBOL.
      # return it as a string.
      arg <- str_match(tok$command, "@(\\d+)")[1,2]
      arg <- intToUtf8(arg)
      
      if (arg == "'") {
        arg <- "\\'"
      }
      
      tok$rendered <- str_c("'", arg, "'")
      if (tok_idx == 1) {
        tok$left_separator <- ''
      }
      next
    } else if (!tok$text_mode || tok$is_command) {
      # translate using the translation table in symbols.R
      translations[[str_trim(tok$command)]] %??% tok$command
    } else {
      # leave as-is
      tok$command
    }
    
    # empty command; if followed by arguments such as sup or sub, render as
    # an empty token, otherwise skip
    if (tok$rendered == "") {
      if (length(tok$args) > 0) {
        tok$rendered <- "{}"
      } else {
        tok$skip <- TRUE
      }
    }
    
    if (tok$text_mode && !tok$is_command) {
      tok$rendered <- str_c("'", tok$rendered, "'")
    }
    
    
    # If the token starts with a number, break the number from
    # the rest of the string. This is because a plotmath symbol
    # cannot start with a number.
    if (str_detect(tok$rendered, "^[0-9]") && !tok$text_mode) {
      split <- str_match(tok$rendered, "(^[0-9\\.]*)(.*)")
      
      if (split[1, 3] != "") {
        tok$rendered <- str_c(split[1,2], "*", split[1,3])
      } else {
        tok$rendered <- split[1,2]
      }
    }
    
    tok$left_operator <- str_detect(tok$rendered, fixed("$LEFT"))
    tok$right_operator <- str_detect(tok$rendered, fixed("$RIGHT"))
    
    if (tok_idx == 1) {
      tok$left_separator <- ""
    }
    
    if (tok$left_operator) {
      if (tok_idx == 1) {
        # Either this operator is the first token...
        tok$rendered <- str_replace_all(tok$rendered,
                                        fixed("$LEFT"),
                                        "phantom()")
      } else if (tokens[[tok_idx-1]]$right_operator) {
        # or the previous token was also an operator or an open parentheses.
        # Bind the tokens using phantom()
        tok$rendered <- str_replace_all(tok$rendered,
                                        fixed("$LEFT"),
                                        "phantom()")
      } else {
        tok$rendered <- str_replace_all(tok$rendered,
                                          fixed("$LEFT"),
                                          "")
        tok$left_separator <- ""
      }
    }
    if (tok$right_operator) {
      if (tok_idx == length(tokens)) {
        tok$rendered <- str_replace_all(tok$rendered,
                                        fixed("$RIGHT"),
                                        "phantom()")
      } else {
        tok$rendered <- str_replace_all(tok$rendered,
                                          fixed("$RIGHT"),
                                          "")
        tokens[[tok_idx+1]]$left_separator <- ""
      }
    }
    if (length(tok$args) > 0) {
      for (argidx in seq_along(tok$args)) {
        args <- render_latex(tok$args[[argidx]], user_defined, hack_parentheses=hack_parentheses)
        argfmt <- str_c("$arg", argidx)
        if (str_detect(tok$rendered, fixed(argfmt))) {
          tok$rendered <- str_replace_all(tok$rendered,
                                          fixed(argfmt),
                                          args)
        } else {
          if (tok$rendered != "{}") {
            tok$rendered <- str_c(tok$rendered, " * {",
                                  args, "}")
          } else {
            tok$rendered <- str_c("{", args, "}")    
          }
        }
      }
    } 
    
    if (length(tok$optional_arg) > 0) {
      optarg <- render_latex(tok$optional_arg, user_defined, hack_parentheses=hack_parentheses)
      tok$rendered <- str_replace_all(tok$rendered,
                                        fixed("$opt"),
                                        optarg)
    }
    
    for (type in c("sub", "sup")) {
      arg <- tok[[str_c(type, "_arg")]]
      argfmt <- str_c("$", type)
      
      if (length(arg) > 0) {
        rarg <- render_latex(arg, user_defined, hack_parentheses=hack_parentheses)
        
        if (str_detect(tok$rendered, fixed(argfmt))) {
          tok$rendered <- str_replace_all(tok$rendered, fixed(argfmt), rarg)
        } else {
          if (type == "sup") {
            tok$rendered <- sprintf("%s^{%s}", 
                                      tok$rendered,
                                      rarg)
          } else {
            tok$rendered <- sprintf("%s[%s]", 
                                      tok$rendered,
                                      rarg)
          }
        } 
        
      }
    }
    
    
    # Replace all $P tokens with phantom(), and consume
    # any arguments that were not specified (e.g. if 
    # there is no argument specified for the command,
    # substitute '' for '$arg1')
    tok$rendered <- tok$rendered %>%
      str_replace_fixed("$P", "phantom()") %>%
      str_replace_fixed("$arg1", "") %>%
      str_replace_fixed("$arg2", "") %>%
      str_replace_fixed("$sup", "") %>%
      str_replace_fixed("$sub", "") %>%
      str_replace_fixed("$opt", "") 
    
    if (tok_idx != length(tokens) && tok$command == "\\frac") {
      tok$right_separator <- " * phantom(.)"
    }
    
    if (!hack_parentheses) {
      if (tok$command %in% c("(", ")")) {
        tok$left_separator <- ""
        tok$right_separator <- ""
      } 
      if (tok_idx > 1 && tokens[[tok_idx-1]]$command == "(") {
        tok$left_separator <- ""
      }
    } else {
      if (tok$command %in% c("(", ")") && !tok$text_mode) {
        cat_trace("Using hack for parentheses")
        if (tok$command == "(") {
          tok$rendered <- "group('(', phantom(), '.')"
        } else if (tok$command == ")") {
          tok$rendered <- "group(')', phantom(), '.')"
        }
      }
    }
    
    # If the token still starts with a "\", substitute it
    # with the corresponding expression
    tok$rendered <- str_replace(tok$rendered, "^\\\\", "")

    if (tok$rendered == "{}") {
      tok$skip <- TRUE
    }
  }
  
  
  rendered_tokens <- sapply(tokens, function(tok) {
    if (tok$skip) {
      ""
    } else {
      str_c(tok$left_separator %??% "*",
            tok$rendered,
            tok$right_separator %??% "")
    }
  })
  str_c(rendered_tokens, collapse="")
}

# Validates the input LaTeX string
# 
# Checks for common issues in the LaTeX string, like
# unmatched braces.
# 
# Also, warns if any of the less common special characters
# are present, indicating that perhaps the user accidentally forgot
# to escape backslashes.
#
validate_input <- function(latex_string) {
  for (possible_slash_pattern in c("\a", "\b", "\f", "\v")) {
    if (str_detect(latex_string, fixed(possible_slash_pattern))) {
      repr <- deparse(possible_slash_pattern)
      message("latex2exp: Detected possible missing backslash: you entered ",
              repr, ", did you mean to type ", 
              str_replace(repr, fixed("\\"), "\\\\"), "?")
    }
  }
  
  if (str_detect(latex_string, fixed("\\\\"))) {
    stop("The LaTeX string '",
         latex_string,
         "' includes a '\\\\' command. Line breaks are not currently supported.")
  }
  
  test_string <- latex_string %>%
    str_replace_fixed("\\{", "") %>%
    str_replace_fixed("\\}", "")
    
  # check that opened and closed braces match in number
  opened_braces <- nrow(str_match_all(test_string, "[^\\\\]*?(\\{)")[[1]]) -
    nrow(str_match_all(test_string, "\\\\left\\{")[[1]])
  closed_braces <- nrow(str_match_all(test_string, "[^\\\\]*?(\\})")[[1]]) -
    nrow(str_match_all(test_string, "\\\\right\\}")[[1]])
  
  if (opened_braces != closed_braces) {
    stop("Mismatched number of braces in '", latex_string, "' (",
         opened_braces, " { opened, ",
         closed_braces, " } closed)")
  }
  
  # check that the number of \left* and \right* commands match
  lefts <- nrow(str_match_all(test_string, "[^\\\\]*\\\\left[\\(\\{\\|\\[\\.]")[[1]])
  rights <- nrow(str_match_all(test_string, "[^\\\\]*\\\\right[\\)\\}\\|\\]\\.]")[[1]])
  
  if (lefts != rights) {
    stop("Mismatched number of \\left and \\right commands in '", latex_string, "' (",
         lefts, " left commands, ",
         rights, " right commands.")
  }
  
  TRUE
}