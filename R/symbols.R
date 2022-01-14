# A translation of LaTeX expressions into R expressions.
#
# The following template strings are substituted:
# 
# * $arg1, $arg2, ... represent the first, second, ... brace argument.
#   E.g. for `\\frac{x}{y}`, $arg1 is `x` and $arg2 is `y`.
# * $opt is an optional argument in square brackets. E.g. for
#   `\\sqrt[2]{x}`, $opt is `2`.
# * $sub and $sup are arguments in the exponent (^) or subscript (_)
#   following the current expression.   
# * $1 and $2 are substituted the previous and following LaTeX expression
#   relative to the current token.
#   They are primarily used for operators.


.arithmetic_operators <- list(
  "+" = "$1 + $2",
  "-" = "$1 - $2",
  "/" = "$1 / $2",
  "=" = "$1 == $2",
  "*" = "symbol('\052')"
)

.binary_operators <- list(
  "\\div" = "$1 %/% $2",
  "\\pm" = "$1 %+-% $2",
  "\\neq" = "$1 != $2",
  "\\geq" = "$1 >= $2",
  "\\leq" = "$1 <= $2",
  "\\approx" = "$1 %~~% $2",
  "\\sim" = "$1 %~% $2",
  "\\propto" = "$1 %prop% $2",
  "\\equiv" = "$1 %==% $2",
  "\\cong" = "$1 %=~% $2",
  "\\in" = "$1 %in% $2",
  "\\notin" = "$1 %notin% $2",
  "\\cdot" = "$1 %.% $2",
  "\\times" = "$1 %*% $2",
  "\\subset" = "$1 %subset% $2",
  "\\subseteq" = "$1 %subseteq% $2",
  "\\nsubset" = "$1 %notsubset% $2",
  "\\supset" = "$1 %supset% $2",
  "\\supseteq" = "$1 %supseteq% $2",
  "\\setminus" = "$1 * '\\\\' * $2",
  "\\cup" = "$1 * symbol('\\310') * $2",
  "\\cap" = "$1 * symbol('\\311') * $2",
  "\\cap" = "$1 * phantom(.) * intersect() * $2"
)

.operators <- list(
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
  "\\sqrt" = "sqrt($arg1, $opt)",
  "\\sum" = "sum($arg1, $sub, $sup)",
  "\\prod" = "prod($arg1, $sub, $sup)",
  "\\int" = "integral($arg1, $sub, $sup)",
  "\\frac" = "frac($arg1, $arg2)",
  "\\bigcup" = "union($arg1, $sub, $sup)",
  "\\bigcap" = "intersect($arg1, $sub, $sup)",
  "\\lim" = "lim($arg1, $sub)",
  "\\min" = "min($arg1, $sub)",
  "\\max" = "max($arg1, $sub)"
)

# Text size
.fontsizes <- list(
  "\\normalsize" = "displaystyle($arg1)",
  "\\small" = "scriptstyle($arg1)",
  "\\tiny" = "scriptscriptstyle($arg1)"
)

# Greek letter vairants
.variants <- list(
  "\\Upsilon" = "Upsilon1",
  "\\varpi" = "omega1"
)

# Arrows
.arrows <- list(
  "\\rightarrow" = "$1 %->% $2",
  "\\leftarrow" = "$1 %<-% $2",
  "\\leftrightarrow" = "$1 %<->% $2",
  "\\Rightarrow" = "$1 %=>% $2",
  "\\Leftarrow" = "$1 %<=% $2",
  "\\Leftrightarrow" = "$1 %<=>% $2",
  "\\uparrow" = "$1 %up% $2",
  "\\downarrow" = "$1 %down% $2",
  "\\Uparrow" = "$1 %dblup% $2",
  "\\Downarrow" = "$1 %dbldown% $2",
  # Some synonyms
  "\\to" = "$1 %->% $2",
  "\\iff" = "$1 %<=>% $2"
)

# Layout
.layout_and_spacing <- list(
  "\\overset" = "atop($arg1, $arg2)",
  
  "\\@SPACE1" = "phantom(.)",
  "\\@SPACE2" = "$P ~~ $P",
  "\\phantom" = "phantom($arg1)"
)

# Formatting
.formatting <- list(
  "\\textbf" = "bold($arg1)",
  "\\textit" = "italic($arg1)",
  "\\textrm" = "plain($arg1)",
  "\\underline" = "underline($arg1)"
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
  "\\euro" = "symbol('\\240')",
  "\\textbackslash" = "'\\'",
  "\\bullet" = "'•'",
  "\\smile" = "'☺'",
  "\\ast" = "symbol('\\052')"
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
  "\\tilde" = "tilde($arg1)",
  "\\hat" = "hat($arg1)",
  "\\widehat" = "widehat($arg1)",
  "\\widetilde" = "widetilde($arg1)",
  "\\bar" = "bar($arg1)",
  "\\dot" = "dot($arg1)",
  "\\underline" = "underline($arg1)",
  "\\mathring" = "ring($arg1)"
)

# Characters that need to be treated in a special way by the parser
.specials <- list(
  "\\@SEMICOLON" = "';'",
  "\\@PERIOD" = "'.'",
  "\\@ESCAPEDDOLLAR" = "'$'",
  "\\@ESCAPEDBRACE1" = "'{'",
  "\\@ESCAPEDBRACE2" = "'}'",
  "\\@ESCAPEDBRACKET1" = "'['",
  "\\@ESCAPEDBRACKET2" = "']'",
  "\\@ESCAPEDQUOTE" = "'\\''",
  "\\@ESCAPEDBACKSLASH" = "'\\\\'",
  "," = "list(,)",
  "|" = "group('|', phantom(), '')"
)

# Parentheses
.parentheses <- list(
  "\\@leftPAR" = "bgroup('(', $arg1, ')')",
  "\\@leftBRACE" = "bgroup('{', $arg1, '}')",
  "\\@leftBRACKET" = "bgroup('[', $arg1, ']')",
  "\\@leftPIPE" = "bgroup('|', $arg1, '|')",
  "\\@middlePIPE" = "bgroup('|', $P, '.')",
  "\\lbrack" = "group('[', $P, '')",
  "\\rbrack" = "group('', $P, ']')",
  "\\langle" = "group(langle,$P, '.')",
  "\\rangle" = "group('.', $P, rangle)",
  "\\lceil" = "group(lceil, $P, '.')",
  "\\rceil" = "group('.', $P, rceil)",
  "\\lfloor" = "group(lfloor, $P, '.')",
  "\\rfloor" = "group('.', $P, rfloor)",
  "\\@pipe" = "group('|', group('|', $P, '.'), '.')",
  
  # dummy; these are used by `latex2exp_supported()`
  "\\left(" = "",
  "\\right)" = "",
  "\\left[" = "",
  "\\right]" = "",
  "\\left{" = "",
  "\\right}" = "",
  "\\left|" = "",
  "\\right|" = "",
  "\\|" = ""
)

.vector <- list(
  "\\norm" = "group('|', group('|', $arg1, '|'), '|')",
  "\\bra" = "group(langle, $arg1, '|')",
  "\\ket" = "group('|', $arg1, rangle)",
  "\\braket" = "group(langle, $arg1, rangle)"
)

# Approximations to the TeX and LaTeX symbols
.others <- list(
  "\\LaTeX" = "L^{$P[$P[$P[scriptstyle(A)]]]}*T[textstyle(E)]*X",
  "\\TeX" = "T[textstyle(E)]*X"
)

.subs <- c(
  .variants,
  .arithmetic_operators,
  .binary_operators,
  .operators,
  .arrows,
  .big_operators,
  .layout_and_spacing,
  .formatting,
  .decorations,
  .symbols,
  .degrees,
  .specials,
  .parentheses,
  .vector,
  .others
)

.base_separators <- c("$", "{", "\\", "[", ",", ";", " ")
.math_separators <- c(.base_separators, 
                      names(.arithmetic_operators),
                      "|",
                      "&",
                      "^",
                      "_",
                      "(",
                      ")")

