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

latex_supported = list(
  # Greek letters and variants
  "greek letters" = list(
    "\\alpha" = "alpha",
    "\\Alpha" = "Alpha",
    "\\beta" = "beta",
    "\\Beta" = "Beta",
    "\\chi" = "chi",
    "\\Chi" = "Chi",
    "\\delta" = "delta",
    "\\Delta" = "Delta",
    "\\epsilon" = "epsilon",
    "\\Epsilon" = "Epsilon",
    "\\eta" = "eta",
    "\\Eta" = "Eta",
    "\\gamma" = "gamma",
    "\\Gamma" = "Gamma",
    "\\iota" = "iota",
    "\\Iota" = "Iota",
    "\\kappa" = "kappa",
    "\\Kappa" = "Kappa",
    "\\lambda" = "lambda",
    "\\Lambda" = "Lambda",
    "\\mu" = "mu",
    "\\Mu" = "Mu",
    "\\nu" = "nu",
    "\\Nu" = "Nu",
    "\\omega" = "omega",
    "\\Omega" = "Omega",
    "\\omicron" = "omicron",
    "\\Omicron" = "Omicron",
    "\\phi" = "phi",
    "\\Phi" = "Phi",
    "\\pi" = "pi",
    "\\Pi" = "Pi",
    "\\psi" = "psi",
    "\\Psi" = "Psi",
    "\\rho" = "rho",
    "\\Rho" = "Rho",
    "\\sigma" = "sigma",
    "\\Sigma" = "Sigma",
    "\\tau" = "tau",
    "\\Tau" = "Tau",
    "\\theta" = "theta",
    "\\Theta" = "Theta",
    "\\upsilon" = "upsilon",
    "\\Upsilon" = "Upsilon",
    "\\zeta" = "zeta",
    "\\Zeta" = "Zeta",
    "\\Upsilon" = "Upsilon1",
    "\\varpi" = "omega1",
    "\\varphi" = "varphi"
  ),
  
  "arithmetic operators" = list(
    "+" = "$1 + $2",
    "-" = "$1 - $2",
    "/" = "$1 / $2",
    "*" = "symbol('\052')"
  ),

  "binary operators" = list(
    # need a phantom() here so that you could do multiple equalities.
    # expression(a = b = c) results in a parse error.
    "=" = "$1 * {phantom() == phantom()} * $2",
    ">" = "$1 * {phantom() > phantom()} * $2",
    "<" = "$1 * {phantom() < phantom()} * $2",
    "\\neq" = "$1 != $2",
    "\\geq" = "$1 >= $2",
    "\\leq" = "$1 <= $2",
    
    "\\div" = "$1 %/% $2",
    "\\pm" = "$1 %+-% $2",
    "\\approx" = "$1 %~~% $2",
    "\\sim" = "$1 %~% $2",
    "\\propto" = "$1 %prop% $2",
    "\\equiv" = "$1 %==% $2",
    "\\cong" = "$1 %=~% $2",
    "\\in" = "$1 %in% $2",
    "\\notin" = "$1 %notin% $2",
    "\\cdot" = "$1 %.% $2",
    "\\times" = "$1 %*% $2",
    "\\circ" = "$1 ~ '\u25e6' ~ $2",
    "\\ast" = "$1 ~ symbol('\\053') ~ $2",
    "\\%" = "$1 ~ symbol('\\045') ~ $2",
    "\\perp" = "$1 ~ symbol('\\136') ~ $2",
    "\\bullet" = "$1 ~ symbol('\\267') ~ $2",
    "\\otimes" = "$1 ~ symbol('\\304') ~ $2",
    "\\oplus" = "$1 ~ symbol('\\305') ~ $2",
    "\\oslash" = "$1 ~ symbol('\\306') ~ $2",
    "\\vee" = "$1 ~ symbol('\\331') ~ $2",
    "\\wedge" = "$1 ~ symbol('\\332') ~ $2",
    "\\angle" = "$1 ~ symbol('\\320') ~ $2"
  ),

  "set operators" = list(
    "\\subset" = "$1 %subset% $2",
    "\\subseteq" = "$1 %subseteq% $2",
    "\\nsubset" = "$1 %notsubset% $2",
    "\\supset" = "$1 %supset% $2",
    "\\supseteq" = "$1 %supseteq% $2",
    "\\setminus" = "$1 ~ '\\\\' ~ $2",
    "\\cup" = "$1 ~ symbol('\\310') ~ $2",
    "\\cap" = "$1 ~ symbol('\\311') ~ $2"
  ),
  
  "other operators" = list(
    "\\forall" = "symbol('\\042')",
    "\\exists" = "symbol('\\044')",
    "\\Im" = "symbol('\\301')",
    "\\Re" = "symbol('\\302')",
    "\\wp" = "symbol('\\303')",
    "\\surd" = "symbol('\\326')",
    "\\neg" = "symbol('\\330')",
    "\\ni" = "symbol('\\047')"
  ),

  # Square root, sum, prod, integral, etc.
  "operators with subscripts and superscripts" = list(
    "\\sqrt" = "sqrt($arg1, $opt)",
    "\\sum" = "sum($arg1, $sub, $sup)",
    "\\prod" = "prod($arg1, $sub, $sup)",
    "\\int" = "integral($arg1, $sub, $sup)",
    "\\bigcup" = "union($arg1, $sub, $sup)",
    "\\bigcap" = "intersect($arg1, $sub, $sup)",
    "\\lim" = "lim($arg1, $sub)",
    "\\min" = "min($arg1, $sub)",
    "\\max" = "max($arg1, $sub)"
  ),

  # Text size
  "text size" = list(
    "\\normalsize" = "displaystyle($arg1)",
    "\\small" = "scriptstyle($arg1)",
    "\\tiny" = "scriptscriptstyle($arg1)"
  ),

  # Arrows
  "arrows" = list(
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
  ),

  # Layout
  "layout and spacing" = list(
    "\\overset" = "atop($arg1, $arg2)",
    "\\frac" = "frac($arg1, $arg2)",
    
    
    "\\@SPACE1" = "$1 * phantom(.) * $2",
    "\\@SPACE2" = "$1 ~~ $2",
    "\\phantom" = "phantom($arg1)",
    
    # Dummy symbols
    "\\ " = "",
    "\\;" = "",
    "\\," = ""
  ),

  # Formatting
  "formatting" = list(
    "\\textbf" = "bold($arg1)",
    "\\textit" = "italic($arg1)",
    "\\bf" = "bold($arg1)",
    "\\it" = "italic($arg1)",
    "\\textrm" = "plain($arg1)"
  ),

  # Symbols
  "symbols" = list(
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
    "\\textbackslash" = "'\\\\'",
    "\\diamond" = "'\\u25ca'",
    "\\uptriangle" = "'\\u25b2'",
    "\\righttriangle" = "'\\u25ba'",
    "\\downtriangle" = "'\\u25bc'",
    "\\lefttriangle" = "'\\u25c4'",
    "\\smiley" = "'\u263a'",
    "\\sharp" = "'\u266f'",
    "\\eighthnote" = "'\u266a'",
    "\\twonotes" = "'\u266b'",
    "\\sun" = "'\u263c'",
    "\\venus" = "'\u2640'",
    "\\mars" = "'\u2642'",
    "\\Exclam" = "'\\u203c'",
    "\\dagger" = "'\\u2020'",
    "\\ddagger" = "'\\u2021'",
    "''" = "$1 * second ",
    "'" = "$1 * minute ",
    "\\degree" = "'\\u0b0'",
    "\\prime" = "$1 * minute ",
    "\\second" = "$1 * second ",
    "\\third" = "$1 * '\\u2034'"
  ),

  # Decorations
  "decorations" = list(
    "\\tilde" = "tilde($arg1)",
    "\\hat" = "hat($arg1)",
    "\\widehat" = "widehat($arg1)",
    "\\widetilde" = "widetilde($arg1)",
    "\\bar" = "bar($arg1)",
    "\\dot" = "dot($arg1)",
    "\\underline" = "underline($arg1)",
    "\\mathring" = "ring($arg1)"
  ),

  # Characters that need to be treated in a special way by the parser
  "specials" = list(
    "," = "list(,)",
    "|" = "group('|', phantom(), '')"
  ),
  
  # Parentheses
  "parentheses" = list(
    "\\@leftPAR" = "bgroup('(', $arg1, ')')",
    "\\@leftBRACE" = "bgroup('{', $arg1, '}')",
    "\\@leftBRACKET" = "bgroup('[', $arg1, ']')",
    "\\@leftPIPE" = "bgroup('|', $arg1, '|')",
    "\\@middlePIPE" = "bgroup('|', $P, '.')",
    # dummy; these are used by `latex2exp_supported()`
    "\\left(" = "",
    "\\left[" = "",
    "\\left{" = "",
    "\\left|" = "",
    "\\|" = ""
  ),
  
  "parentheses (not scalable)" = list(
    "\\lbrack" = "group('[', $P, '')",
    "\\rbrack" = "group('', $P, ']')",
    "\\langle" = "group(langle,$P, '.')",
    "\\rangle" = "group('.', $P, rangle)",
    "\\lceil" = "group(lceil, $P, '.')",
    "\\rceil" = "group('.', $P, rceil)",
    "\\lfloor" = "group(lfloor, $P, '.')",
    "\\rfloor" = "group('.', $P, rfloor)",
    "\\@pipe" = "group('|', group('|', $P, '.'), '.')"
  ),

  "vector" = list(
    "\\norm" = "group('|', group('|', $arg1, '|'), '|')",
    "\\bra" = "group(langle, $arg1, '|')",
    "\\ket" = "group('|', $arg1, rangle)",
    "\\braket" = "group(langle, $arg1, rangle)"
  ),

  # Approximations to the TeX and LaTeX symbols
  "miscellanea" = list(
    "\\LaTeX" = "L^{$P[$P[$P[scriptstyle(A)]]]}*T[textstyle(E)]*X",
    "\\TeX" = "T[textstyle(E)]*X"
  )
)

latex_supported_map <- Reduce(c, latex_supported)

.base_separators <- c("$", "{", "\\", "[", ",", ";", " ")
.math_separators <- c(.base_separators, 
                      names(latex_supported[['arithmetic operators']]),
                      "|",
                      "&",
                      "^",
                      "_",
                      "(",
                      ")",
                      "!",
                      "?",
                      "'",
                      "=", ">", "<")

