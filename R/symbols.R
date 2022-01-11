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

.operators <- c(
  # Operators
  "+" = "@P@ + @P@",
  "-" = "@P@ - @P@",
  "/" = "@P@ / @P@",
  "=" = "@P@ == @P@",
  "*" = "symbol('\052')",
  "\\div" = "@P@ %/% @P@",
  
  "\\pm" = "@P@ %+-% @P@",
  "\\neq" = "@P@ != @P@",
  "\\geq" = "@P@ >= @P@",
  "\\leq" = "@P@ <= @P@",
  "\\approx" = " @P@ %~~% @P@",
  "\\sim" = " @P@ %~% @P@",
  "\\propto" = " @P@ %prop% @P@",
  "\\equiv" = " @P@ %==% @P@",
  "\\cong" = " @P@ %=~% @P@",
  "\\in" = " @P@ %in% @P@ ",
  "\\notin" = " @P@ %notin% @P@",
  "\\cdot" = " @P@ %.% @P@",
  "\\times" = "@P@ %*% @P@",
  "\\subset" = " @P@ %subset% @P@",
  "\\subseteq" = "@P@ %subseteq% @P@",
  "\\nsubset" = "@P@ %notsubset% @P@",
  "\\supset" = "@P@ %supset% @P@",
  "\\supseteq" = "@P@ %supseteq% @P@",
  "\\rightarrow" = "@P@ %->% @P@",
  "\\leftarrow" = "@P@ %<-% @P@",
  "\\Rightarrow" = "@P@ %=>% @P@",
  "\\Leftarrow" = "@P@ %<=% @P@",
  "\\forall" = "symbol('\\042')",
  "\\exists" = "symbol('\\044')",
  "\\%" = "symbol('\\045')",
  "\\ast" = "symbol('\\053')",
  "\\perp" = "symbol('\\136')",
  "\\bullet" = "symbol('\\267')",
  "\\Im" = "symbol('\\301')",
  "\\Re" = "symbol('\\302')",
  "\\wp" = "symbol('\\303')",
  "\\otimes" = "symbol('\\304')",
  "\\oplus" = "symbol('\\305')",
  "\\oslash" = "symbol('\\306')",
  "\\surd" = "symbol('\\326')",
  "\\neg" = "symbol('\\330')",
  "\\vee" = "symbol('\\331')",
  "\\wedge" = "symbol('\\332')"
)

# Square root, sum, prod, integral, etc.
.big_operators <- c(
  "\\sqrt" = "sqrt(@1@, @S@)",
  "\\sum" = "sum(@3@,@1@,@2@)",
  "\\prod" = "prod(@3@,@1@,@2@)",
  "\\int" = "integral(@3@,@1@,@2@)",
  "\\frac" = "frac(@1@, @2@)",
  "\\bigcup" = "union(@3@,@1@,@2@)",
  "\\bigcap" = "intersect(@3@,@1@,@2@)",
  "\\lim" = "lim(@3@, @1@)"
)

# Text size
.fontsizes = c(
  "\\normalsize" = "displaystyle(@1@)",
  "\\small" = "scriptstyle(@1@)",
  "\\tiny" = "scriptscriptstyle(@1@)"
)

# Layout
.layout_and_spacing = c(
  "\\overset" = "atop(@1@, @2@)",
  
  "\\SPACE1@" = "paste(' ')",
  "\\SPACE2@" = "phantom(0)",
  "\\," = "phantom(0)",
  "\\;" = "phantom() ~~ phantom()",
  "\\phantom" = "phantom(@1@)"
)

# Formatting
.formatting <- c(
  "\\textbf" = "bold(@1@)",
  "\\textit" = "italic(@1@)",
  "\\textrm" = "plain(@1@)",
  "\\mathbf" = "bold(@1@)",
  "\\mathit" = "italic(@1@)",
  "\\mathrm" = "plain(@1@)"
)

# Exponent and subscript
.exp_subs <- c(
  "^" = "@P@ ^ {@1@}",
  "_" = "@P@ [ {@1@} ]"
)

# Symbols
.symbols <- c(
  # Symbols
  "\\infty" = " infinity ",
  "\\partial" = " partialdiff ",
  "\\cdots" = " cdots ",
  "\\ldots" = " ldots ",
  "\\degree" = " degree ",
  "\\clubsuit" = "symbol('\\247')",
  "\\diamondsuit" = "symbol('\\250')",
  "\\heartsuit" = "symbol('\\251')",
  "\\spadesuit" = "symbol('\\252')",
  "\\aleph" = "symbol('\\300')"
)

# Degrees
.degrees <- c(
  "''" = " second ",
  "'" = " minute ",
  "\\prime" = " minute ",
  "\\circ" = "paste('°')"
)

# Decorations
.decorations <- c(
  "\\tilde" = "tilde(@1@)",
  "\\hat" = "hat(@1@)",
  "\\widehat" = "widehat(@1@)",
  "\\widetilde" = "widetilde(@1@)",
  "\\bar" = "bar(@1@)",
  "\\dot" = "dot(@1@)",
  "\\underline" = "underline(@1@)",
  "\\mathring" = "ring(@1@)"
)

# Characters that need to be treated in a special way by the parser
.specials <- c(
  "\\COMMA@" = "','",
  "\\SEMICOLON@" = "';'",
  "\\PERIOD@" = "'.'",
  "\\SUB_AND_EXP@" = "@P@ [@1@] ^{@2@}"
)

# Parentheses
.parentheses <- c(
  "\\leftPAR@" = "bgroup('(', @1@ ",
  "\\rightPAR@" = "')')",
  "\\leftBRACE@" = "bgroup('{', @1@ ",
  "\\rightBRACE@" = "'}')",
  "\\leftSQUARE@" = "bgroup('[', @1@ ",
  "\\rightSQUARE@" = "']')",
  "\\leftPIPE@" = "bgroup('|', @1@ ",
  "\\rightPIPE@" = "'|')",
  "\\middlePIPE@" = "bgroup('|', @P@, '')",
  "\\leftPERIOD@" = "bgroup('', @1@ ",
  "\\rightPERIOD@" = "'')",
  "\\lbrack" = "paste('[')",
  "\\rbrack" = "paste(']')",
  "\\langle" = "bgroup(langle, @1@ ",
  "\\rangle" = "rangle)",
  "\\lceil" = "bgroup(lceil, @1@ ",
  "\\rceil" = "rceil)",
  "\\lfloor" = "bgroup(lfloor, @1@ ",
  "\\rfloor" = "rfloor)"
)

# Approximations to the TeX and LaTeX symbols
.others <- c(
  "\\LaTeX" = "L^{phantom()[phantom()[phantom()[scriptstyle(A)]]]}*T[textstyle(E)]*X",
  "\\TeX" = "T[textstyle(E)]*X"
)

.subs <- c(
  .operators,
  .big_operators,
  .layout_and_spacing,
  .formatting,
  .decorations,
  .exp_subs,
  .symbols,
  .degrees,
  .specials,
  .parentheses,
  .others
)


# LaTeX expressions in the form \tag_sub^exp
.supsub <- names(.big_operators)