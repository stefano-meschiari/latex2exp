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
# * $LEFT and $RIGHT are substituted the previous and following LaTeX expression
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
    "+" = "$LEFT + $RIGHT",
    "-" = "$LEFT - $RIGHT",
    "/" = "$LEFT / $RIGHT",
    "*" = "$LEFT ~ symbol('\052') ~ $RIGHT"
  ),

  "binary operators" = list(
    "=" = "$LEFT * {phantom() == phantom()} * $RIGHT",
    ">" = "$LEFT * {phantom() > phantom()} * $RIGHT",
    "<" = "$LEFT * {phantom() < phantom()} * $RIGHT",
    "\\ne" = "$LEFT * {phantom() != phantom()} * $RIGHT",
    "\\neq" = "$LEFT * {phantom() != phantom()} * $RIGHT",
    "\\geq" = "$LEFT * {phantom() >= phantom()} * $RIGHT",
    "\\leq" = "$LEFT * {phantom() <= phantom()} * $RIGHT",
    
    "\\div" = "$LEFT %/% $RIGHT",
    "\\pm" = "$LEFT %+-% $RIGHT",
    "\\approx" = "$LEFT %~~% $RIGHT",
    "\\sim" = "$LEFT %~% $RIGHT",
    "\\propto" = "$LEFT %prop% $RIGHT",
    "\\equiv" = "$LEFT %==% $RIGHT",
    "\\cong" = "$LEFT %=~% $RIGHT",
    "\\in" = "$LEFT %in% $RIGHT",
    "\\notin" = "$LEFT %notin% $RIGHT",
    "\\cdot" = "$LEFT %.% $RIGHT",
    "\\times" = "$LEFT %*% $RIGHT",
    "\\circ" = "$LEFT ~ '\u25e6' ~ $RIGHT",
    "\\ast" = "$LEFT ~ symbol('\\053') ~ $RIGHT",
    "\\%" = "$LEFT ~ symbol('\\045') ~ $RIGHT",
    "\\perp" = "$LEFT ~ symbol('\\136') ~ $RIGHT",
    "\\bullet" = "$LEFT ~ symbol('\\267') ~ $RIGHT",
    "\\otimes" = "$LEFT ~ symbol('\\304') ~ $RIGHT",
    "\\oplus" = "$LEFT ~ symbol('\\305') ~ $RIGHT",
    "\\oslash" = "$LEFT ~ symbol('\\306') ~ $RIGHT",
    "\\vee" = "$LEFT ~ symbol('\\332') ~ $RIGHT",
    "\\wedge" = "$LEFT ~ symbol('\\331') ~ $RIGHT",
    "\\angle" = "$LEFT ~ symbol('\\320') ~ $RIGHT",
    "\\cdots" = "$LEFT ~ cdots ~ $RIGHT",
    "\\ldots" = "$LEFT ~ ldots ~ $RIGHT"
  ),

  "set operators" = list(
    "\\subset" = "$LEFT %subset% $RIGHT",
    "\\subseteq" = "$LEFT %subseteq% $RIGHT",
    "\\nsubset" = "$LEFT %notsubset% $RIGHT",
    "\\supset" = "$LEFT %supset% $RIGHT",
    "\\supseteq" = "$LEFT %supseteq% $RIGHT",
    "\\setminus" = "$LEFT ~ '\\\\' ~ $RIGHT",
    "\\cup" = "$LEFT ~ symbol('\\310') ~ $RIGHT",
    "\\cap" = "$LEFT ~ symbol('\\307') ~ $RIGHT"
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
    "\\rightarrow" = "$LEFT %->% $RIGHT",
    "\\leftarrow" = "$LEFT %<-% $RIGHT",
    "\\leftrightarrow" = "$LEFT %<->% $RIGHT",
    "\\Rightarrow" = "$LEFT %=>% $RIGHT",
    "\\Leftarrow" = "$LEFT %<=% $RIGHT",
    "\\Leftrightarrow" = "$LEFT %<=>% $RIGHT",
    "\\uparrow" = "$LEFT %up% $RIGHT",
    "\\downarrow" = "$LEFT %down% $RIGHT",
    "\\Uparrow" = "$LEFT %dblup% $RIGHT",
    "\\Downarrow" = "$LEFT %dbldown% $RIGHT",
    # Some synonyms
    "\\to" = "$LEFT %->% $RIGHT",
    "\\iff" = "$LEFT %<=>% $RIGHT"
  ),

  # Layout
  "layout and spacing" = list(
    "\\overset" = "atop($arg1, $arg2)",
    "\\frac" = "frac($arg1, $arg2)",
    
    
    "\\@SPACE1" = "$LEFT * phantom(.) * $RIGHT",
    "\\@SPACE2" = "$LEFT ~~ $RIGHT",
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
    "''" = "$LEFT * second ",
    "'" = "$LEFT * minute ",
    "\\degree" = "'\\u0b0'",
    "\\prime" = "$LEFT * minute ",
    "\\second" = "$LEFT * second ",
    "\\third" = "$LEFT * '\\u2034'"
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
  # when in math mode
  "specials" = list(
    "," = "list(,)",
    "|" = "group('|', phantom(), '')"
  ),
  
  # Parentheses
  "parentheses" = list(
    "\\left(" = "bgroup('(', $RIGHT",
    "\\left[" = "bgroup('[', $RIGHT",
    "\\left{" = "bgroup('{', $RIGHT",
    "\\left|" = "bgroup('|', $RIGHT",
    "\\left." = "bgroup('', $RIGHT",
    "\\right)" = "$LEFT, ')')",
    "\\right]" = "$LEFT, ']')",
    "\\right}" = "$LEFT, '}')",
    "\\right|" = "$LEFT, '|')",
    "\\right." = "$LEFT, '')",
    "\\|" = ""
  ),
  
  "parentheses (not scalable)" = list(
    "\\lbrack" = "group('[', $P, '')",
    "\\rbrack" = "group('', $P, ']')",
    "\\langle" = "group(langle,$P, '')",
    "\\rangle" = "group('', $P, rangle)",
    "\\lceil" = "group(lceil, $P, '')",
    "\\rceil" = "group('', $P, rceil)",
    "\\lfloor" = "group(lfloor, $P, '')",
    "\\rfloor" = "group('', $P, rfloor)",
    "\\@pipe" = "group('|', group('|', $P, ''), '')"
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

.base_separators <- c("$", "{", "}", "\\", "[", "]", ",", ";", " ")
.math_separators <- unique(
  c(.base_separators, 
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
  "=", ">", "<"))

