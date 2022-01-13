# A translationE of LaTeX expressions into R expressions.
#
# Some special strings are substituted:
# @P@ is a phantom character (used to paste operators)
# @1@ is the first brace argument in a LaTeX expression, \command{1}, or 
# the first subscript argument \sum_1{}
# @2@ is the second brace argument in a LaTeX expression, \command{1}{2} or
# the first superscript argument \sum_1^2{} 
# @3@ is the third brace argument in a LaTeX expression, \command{1}{2}{3} or
# the first non-(super|sub)script argument \sum_1^2 3
# @S@ is the square argument in a LaTeX expression, \command[S]{1}{2}
# if the argument is missing, an empty string is substituted instead


.simple_operators <- list(
  "+" = "@P@ + @P@",
  "-" = "@P@ - @P@",
  "/" = "@P@ / @P@",
  "=" = "@P@ == @P@",
  "*" = "symbol('\052')",
  "(" = "group('(', @P@, '.')",
  ")" = "group(')', @P@, '.')"
)

.operators <- list(
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
  "\\wedge" = "symbol('\\332')",
  "\\ni" = "symbol('\\047')",
  "\\angle" = "symbol('\\320)"
)

# Square root, sum, prod, integral, etc.
.big_operators <- list(
  "\\sqrt" = "sqrt(@1@, @S@)",
  "\\sum" = "sum(@3@,@1@,@2@)",
  "\\prod" = "prod(@3@,@1@,@2@)",
  "\\int" = "integral(@3@,@1@,@2@)",
  "\\frac" = "frac(@1@, @2@) * phantom(.)",
  "\\bigcup" = "union(@3@,@1@,@2@)",
  "\\bigcap" = "intersect(@3@,@1@,@2@)",
  "\\lim" = "lim(@3@, @1@)"
)

# Text size
.fontsizes <- list(
  "\\normalsize" = "displaystyle(@1@)",
  "\\small" = "scriptstyle(@1@)",
  "\\tiny" = "scriptscriptstyle(@1@)"
)

# Greek letter vairants
.variants <- list(
  "\\Upsilon" = "Upsilon1",
  "\\varpi" = "omega1"
)

# Arrows
.arrows <- list(
  "\\rightarrow" = "@P@ %->% @P@",
  "\\leftarrow" = "@P@ %<-% @P@",
  "\\Rightarrow" = "@P@ %=>% @P@",
  "\\Leftarrow" = "@P@ %<=% @P@",
  "\\uparrow" = "@P@ %up% @P@",
  "\\downarrow" = "@P@ %down% @P@",
  "\\Uparrow" = "@P@ %dblup% @P@",
  "\\Downarrow" = "@P@ %dbldown% @P@",
  "\\to" = "@P@ %->% @P@"
)

# Layout
.layout_and_spacing <- list(
  "\\overset" = "atop(@1@, @2@)",
  
  "\\SPACE1@" = "paste(' ')",
  "\\SPACE2@" = "phantom(0)",
  "\\," = "phantom(0)",
  "\\;" = "phantom() ~~ phantom()",
  "\\phantom" = "phantom(@1@)"
)

# Formatting
.formatting <- list(
  "\\textbf" = "bold(@1@)",
  "\\textit" = "italic(@1@)",
  "\\textrm" = "plain(@1@)"
)

# Exponent and subscript
.exp_subs <- list(
  "^" = "@P@ ^ {@1@}",
  "_" = "@P@ [ {@1@} ]"
)

# Symbols
.symbols <- list(
  "\\infty" = " infinity ",
  "\\partial" = " partialdiff ",
  "\\cdots" = " cdots ",
  "\\ldots" = " ldots ",
  "\\degree" = " degree ",
  "\\clubsuit" = "symbol('\\247')",
  "\\diamondsuit" = "symbol('\\250')",
  "\\heartsuit" = "symbol('\\251')",
  "\\spadesuit" = "symbol('\\252')",
  "\\aleph" = "symbol('\\300')",
  "\\euro" = "symbol('\\240')"
)

# Degrees
.degrees <- list(
  "''" = " second ",
  "'" = " minute ",
  "\\prime" = " minute ",
  "\\circ" = "symbol('\\260')"
)

# Decorations
.decorations <- list(
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
.specials <- list(
  "\\COMMA@" = "','",
  "\\SEMICOLON@" = "';'",
  "\\PERIOD@" = "'.'",
  "\\SUB_AND_EXP@" = "@P@ [@1@] ^{@2@}",
  "\\ESCAPED_DOLLAR@" = "'$'",
  "\\ESCAPED_BRACE1@" = "'{'",
  "\\ESCAPED_BRACE2@" = "'}'",
  "\\ESCAPED_BRACKET1@" = "'['",
  "\\ESCAPED_BRACKET2@" = "']'"
)

# Parentheses
.parentheses <- list(
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
  "\\langle" = "group(langle,phantom(), '.')",
  "\\rangle" = "group('.', phantom(), rangle)",
  "\\lceil" = "group(lceil, phantom(), '.')",
  "\\rceil" = "group('.', phantom(), rceil)",
  "\\lfloor" = "group(lfloor, phantom(), '.')",
  "\\rfloor" = "group('.', phantom(), rfloor)",
  "\\PIPE@" = "group('|', group('|', phantom(), '.'), '.')"
)

.vector <- list(
  "\\norm" = "group('|', group('|', @1@, '|'), '|')",
  "\\bra" = "group(langle, @1@, '|')",
  "\\ket" = "group('|', @1@, rangle)"
)

# Approximations to the TeX and LaTeX symbols
.others <- list(
  "\\LaTeX" = "L^{phantom()[phantom()[phantom()[scriptstyle(A)]]]}*T[textstyle(E)]*X",
  "\\TeX" = "T[textstyle(E)]*X"
)

.subs <- c(
  .variants,
  .simple_operators,
  .operators,
  .arrows,
  .big_operators,
  .layout_and_spacing,
  .formatting,
  .decorations,
  .exp_subs,
  .symbols,
  .degrees,
  .specials,
  .parentheses,
  .vector,
  .others
)

# LaTeX expressions in the form \tag_sub^exp
.supsub <- names(.big_operators)

.separators <- c(
  names(.simple_operators),
  "|",
  "&",
  "^",
  "_"
)