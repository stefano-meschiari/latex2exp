# 0.9.6 (11/27/2022)
## Bug fixes
* Fixes swapped wedge and vee symbols (#55, credit to @ebolove)

# 0.9.5 (09/10/2022)
## Bug fixes
* Round brakets after exponent render correctly (fixes #49)
* `\cap` now renders correctly (fixes #52)
* Escaped and non-escaped square brackets now render correctly, including with exponents (fixes #48)

# 0.9.4 (01/03/2022)
## Bug fixes
* Fixes bug where certain commands would fail outside of math mode (e.g. `TeX(r"(\alpha + \beta)")`) (fixes #45).
* Fixes edge case where `TeX(r"($^{18}$ O)")` (e.g. a free-standing superscript or
  subscript) would not render.

# 0.9.3 (02/02/2022)
## Bug fixes
* Fix an edge case encountered with back-to-back spacing or certain types of commands (e.g. `TeX(r"(a \,\, b)"`)) (fixes issue #40)
* Fix handling of compare operators (`=, <, >, \ge, \le`) (fixes issue #38)
* `TeX("")` returns `expression('')` (an empty expression of length 1) (fixes issue #40)

# 0.9.0 I01/20/2022)

## New features
* Completely rewritten parser that is much more robust, produces valid plotmath
  expressions in more situations, and improved rendering by making use of more
  plotmath features.
* Added a vignette containing a browsable gallery of LaTeX commands supported. You can pull it up using `vignette("supported-commands", package = "latex2exp")`.
* Added the following LaTeX commands and symbols:
  * `\Upsilon` and `\varpi` for Greek letter variants
  * `\uparrow`, `\downarrow`, `\Uparrow`, `\Downarrow`, `\to`
  * `\euro` for the euro character
  * `\langle`, `\rangle` for angle bracket (supported in R 4.1)
  * `\lceil`, `\rceil`, `\lfloor`, `\rfloor` for ceil/floor brackets
  * `\|` for double-pipe brackets (`\norm{expr}` is an alternative command for typesetting a vector norm).
  * Note that the angle, ceil, floor, pipe and double pipe brackets are *not* scalable delimiters, because of plotmath limitations.
  * `\bra`, `\ket`, `\braket` for representing vectors with the braket notation
  * `\smiley`, `\diamond`, `\sharp`, `\eightnote`, `\twonotes`, `\sun`, `\venus`,
    `\mars`, `\Exclam`, `\dagger`, `\ddagger`, `\(up|down|right|left)triangle`
* Improved the appearance of round parentheses. `TeX(r"($\alpha(\beta)$)")` now renders more similarly to how plotmath renders the expression `alpha(beta)`.
* Improved the appearance of `,`, `'` and `''` in math mode.
* Improved the appearance of `\frac` fractions. Now a small space is inserted after the fraction, so that multiple fractions are separated.
* Added a test suite covering a large number of LaTeX expressions, edge cases, and examples from GitHub.
* Refactored and improved code documentation.

## Bug fixes
* Fixes #33 (adds `\lceil, \rceil, \lfloor, \rfloor`)
* Fixes #24 (can mix & match different types of brackets, e.g. `TeX("$\\left(\\frac{M}{L}\\right.$")`)

# 0.5.0 (03/14/2021)
## New features
* Update documentation to use the new raw strings introduced in R 4.0 (which lets one do away with the quoting of the backslash character; e.g. one can use `TeX(r'($\alpha^\beta$)')` rather than `TeX('\\alpha^\\beta)`)
* Adds parameters `bold` and `italic` to `TeX()`. These can be used to make the entire expression bold or italic.
* Adds `\phantom{}` ([PR](https://github.com/stefano-meschiari/latex2exp/pull/22))

# 0.4.0 [08/29/2015]
## New features
* Deprecated the `latex2exp()` function; use `TeX()` instead.
* Added `\lbrack` and `\rbrack` to type left and right square brackets.

# 0.3.3 [08/11/2015]
## Bug fixes
Fixes bug #4 ("fix parsing of numbers"), where certain numbers inside formulas where not parsed correctly.

# 0.3.2 [07/28/2015]
## Bug fixes
Fixes bug #3 ("subscript and superscript style"). `latex2exp` now renders combined subscripts and superscripts correctly.

# 0.3.1 [07/02/2015]
## Bug fixes
Fixes bug #2 (white space causes unexpected behaviour). `latex2exp` should now be a bit more compliant with how LaTeX handles whitespace.

# 0.3.0 [06/30/2015]
`latex2exp` is now a proper package.

# 0.2.0 [06/29/2015]
Formulas must now be enclosed between dollar characters ($), as in LaTeX proper. Text does not need to be enclosed in \\text tags anymore.
