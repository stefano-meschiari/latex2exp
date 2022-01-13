# .token2 <- function(command) {
#   tok <- new.env()
#   tok$args <- list()
#   tok$sqargs <- list()
#   tok$supargs <- list()
#   tok$subargs <- list()
#   tok$command <- command
#   class(tok) <- "latextoken2"
#   tok
# }
# 
# .find_substring <- function(string, boundary_characters, start=1L) {
#   pattern <- str_c("[^", 
#                    str_c("\\", boundary_characters, collapse=""),
#                    "]+")
#   string <- str_sub(string, start)
#   str_match(string, pattern)[1,1]
# }
# 
# parse_latex <- function(latex_string, 
#                        user_defined=user_defined,
#                        text_mode=TRUE) {
#   input <- latex_string
# 
#   # Treat \left( / \right) and company specially in order to not have to special-case them in the
#   # parser
#   latex_string <- latex_string %>%
#     .str_replace_all('\\left{', '\\leftBRACE@{') %>%
#     .str_replace_all('\\left[', '\\leftSQUARE@{') %>%
#     .str_replace_all('\\left|', '\\leftPIPE@{') %>%
#     .str_replace_all('\\left.', '\\leftPERIOD@{') %>%
#     .str_replace_all('\\middle|', '\\middlePIPE@{') %>%
#     .str_replace_all('\\|', '\\PIPE@ ') %>%
#     .str_replace_all('\\left(', '\\leftPAR@{') %>%
#     .str_replace_all('\\right}', '}\\rightBRACE@ ') %>%
#     .str_replace_all('\\right]', '}\\rightSQUARE@ ') %>%
#     .str_replace_all('\\right)', '}\\rightPAR@ ') %>%
#     .str_replace_all('\\right|', '}\\rightPIPE@ ') %>%
#     .str_replace_all('\\right.', '\\rightPERIOD@{') %>%
#     
#     .str_replace_all("\\,", "\\SPACE1@ ") %>%
#     .str_replace_all("\\;", "\\SPACE2@ ") %>%
#     
#     .str_replace_all(",", "\\\\COMMA@ ") %>%
#     .str_replace_all(";", "\\\\SEMICOLON@ ") %>%
#     .str_replace_all("\\.", "\\\\PERIOD@ ") %>%
#     
#     .str_replace_all("\\\\$", "\\\\ESCAPED_DOLLAR@ ") %>%
#     .str_replace_all("\\\\{", "\\\\ESCAPED_BRACE1@ ") %>%
#     .str_replace_all("\\\\}", "\\\\ESCAPED_BRACE2@ ") %>%
#     .str_replace_all("\\\\[", "\\\\ESCAPED_BRACKET1@ ") %>%
#     .str_replace_all("\\\\]", "\\\\ESCAPED_BRACKET2@ ") %>%
#     
#     str_replace_all("([ ]+)", " ") %>%
#     str_replace_all(" \\^ ", "\\^") 
#   
#   # Split the input into characters
#   prevch <- ''
#   
#   # If within a tag contained in .textmode, preserve spaces
#   nextisarg <- 0
#   needsnew <- FALSE
#   
#   i <- 1
#   
#   tokens <- list()
#   
#   while (i < nchar(latex_string)) {
#     # Look at current character, previous character, and next character
#     ch <- str_sub(latex_string, i, i+1)
#     prevch <- if (i == 1) "" else str_sub(latex_string, i-1, i)
#     nextch <- if (i == nchar(latex_string)) "" else str_sub(latex_string, i+1, i+2)
#     
#     # We encountered a backslash. Continue until we encounter
#     # another backslash, or a separator, or a dollar
#     if (ch == "\\" && nextch != "\\") {
#       # Continue until we encounter a separator
#       separators <- c("$", "{", "\\")
#       if (!text_mode) {
#         separators <- c(separators, .separators)
#       }
#       command <- .find_substring(latex_string, separators, i)
#       
#       i <- i + length(command) + 1
#     } else if (ch == {
#       
#     }
#   }
# }