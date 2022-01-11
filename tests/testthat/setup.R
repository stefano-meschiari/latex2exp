
#' Do the LaTeX string and the plotmath expression render in the same way?
#' 
#' This function checks that the given LaTeX expression (as processed by TeX())
#' and the corresponding plotmath expression render in the same way. 
#' 
#' This is done by plotting the LaTeX expression and the plotmath expression
#' to two temporary PNG files and comparing its contents.
#' 
#' If the test fails, the two PNG files are returned for visual inspection.
#'
#' @param object LaTeX string
#' @param expected_expression Plotmath expression corresponding to the expected output
#'
#' @return TRUE if the two objects render in the same way
#'
#' @examples
#' expect_plots_same(r"($\alpha$"), expression(alpha))
expect_plots_same <- function(object, expected_expression, negate=FALSE) {
  act <- quasi_label(rlang::enquo(object), arg="object")
  
  plot_md5 <- function(expr, prefix) {
    fn <- tempfile(pattern = prefix, fileext = ".png")
    png(fn, 640, 480, res=150)
    plot(expr)
    dev.off()
    md5sum(fn)
  }
  
  act$md5_1 <- plot_md5(TeX(act$val), "latex2exp_")
  act$md5_2 <- plot_md5(expected_expression, "expression_")
  
  message <- "The TeX expression %s does not render the same as the given expression.
  Compare:
  * TeX: %s
  * Expression: %s"
  
  if (!identical(unname(act$md5_1), unname(act$md5_2))) {
    system(sprintf("open %s", names(act$md5_1)))
    system(sprintf("open %s", names(act$md5_2)))
  }
  
  expect(identical(unname(act$md5_1), unname(act$md5_2)),
         sprintf(message, act$lab, names(act$md5_1), names(act$md5_2)))
  
  invisible(act$val)
}

expect_plots_same