# 0.6.0

* Added the following LaTeX commands:
  * `\Upsilon` and `\varpi` for Greek letter variants
  * `\uparrow`, `\downarrow`, `\Uparrow` and `\Downarrow`
  * `\euro` for the euro character
  * `\circ` as a synonym for `\degree`
  * `\langle`, `\rangle` for angle bracket
  * `\lceil`, `\rceil`, `\lfloor`, `\rfloor` for ceil/floor brackets
  * `\|` for double-pipe brackets (`\norm{expr}` is an alternative command for typesetting a vector norm)
  * `\bra`, `\ket`, `\braket` for representing vectors with the braket notation
  Note that the angle, ceil, floor, pipe and double pipe brackets are *not* scalable delimiters, because of plotmath limitations.
* Improved the appearance of round parentheses. `TeX(r"($\alpha(\beta)$)")` now uses `group` to render parentheses, which makes it more similar to how plotmath renders the expression `alpha(beta)`. 
* Improved the appearance of `\frac` fractions. Now a small space is inserted after the fraction, so that multiple fractions are separated.
* `TeX()` will fail if the dollar signs (`$`) in the string are unbalanced, e.g. `TeX(r"($\alpha)")` will fail.
* Added a test suite covering a number of different LaTeX equations.
* Refactored and improved code documentation.

# 0.5.0 [03/14/2021]

* Update documentation to use the new raw strings introduced in R 4.0 (which lets one do away with the quoting of the backslash character; e.g. one can use `TeX(r'($\alpha^\beta$)')` rather than `TeX('\\alpha^\\beta)`)
* Adds parameters `bold` and `italic` to `TeX()`. These can be used to make the entire expression bold or italic.
* Adds `\phantom{}` ([PR](https://github.com/stefano-meschiari/latex2exp/pull/22))

### 0.4.0 [08/29/2015]
* Deprecated the `latex2exp()` function; use `TeX()` instead.
* Added `\lbrack` and `\rbrack` to type left and right square brackets.

### 0.3.3 [08/11/2015]
Fixes bug #4 ("fix parsing of numbers"), where certain numbers inside formulas where not parsed correctly.

### 0.3.2 [07/28/2015]
Fixes bug #3 ("subscript and superscript style"). `latex2exp` now renders combined subscripts and superscripts correctly.

### 0.3.1 [07/02/2015]
Fixes bug #2 (white space causes unexpected behaviour). `latex2exp` should now be a bit more compliant with how LaTeX handles whitespace.

### 0.3.0 [06/30/2015]
`latex2exp` is now a proper package.

### 0.2.0 [06/29/2015]
Formulas must now be enclosed between dollar characters ($), as in LaTeX proper. Text does not need to be enclosed in \\text tags anymore.
