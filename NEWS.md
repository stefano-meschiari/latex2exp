# latex2exp 0.5.0

* Update documentation to use the new raw strings introduced in R 4.0 (which lets one do away with the quoting of the backslash character; e.g. one can use `TeX(r'($\alpha^\beta$)')` rather than `TeX('\\alpha^\\beta)`)
* Adds parameters `bold` and `italic` to `TeX()`. These can be used to make the entire expression bold or italic.
* Adds `\phantom{}` ([PR](https://github.com/stefano-meschiari/latex2exp/pull/22))
