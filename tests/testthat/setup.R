library(testthat)

expect_plots_same_message <- "The TeX expression %s should %s render the same as the given expression.
Compare:
  * TeX: %s
  * Expression: %s

Note: TeX string evaluated to %s.
"

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
#' @param object LaTeX string or expression returned by TeX()
#' @param expected_expression Plotmath expression corresponding to the expected output
#'
#' @return TRUE if the two objects render in the same way
#'
#' @examples
#' expect_plots_same(r"($\alpha$"), expression(alpha))
expect_renders_same <- function(object, expected_expression) {
  expected_expression <- as.expression(rlang::enexpr(expected_expression))
  .expect_renders(object, expected_expression, negate=FALSE)
}

expect_renders_different <- function(object, expected_expression) {
  expected_expression <- as.expression(rlang::enexpr(expected_expression))
  .expect_renders(object, expected_expression, negate=TRUE)
}

expect_same_expression <- function(object, expected_expression) {
  expected_exp <- as.expression(rlang::enexpr(expected_expression))
  act <- testthat::quasi_label(rlang::enquo(object), arg="object")
  if (is.character(act$val)) {
    act$val <- TeX(act$val)
  }
  cmp <- waldo::compare(act$val, expected_exp, ignore_attr=TRUE)
  message <- sprintf("'%s' (TeX) not equal to '%s' (expected expression)",
                     act$val,
                     as.character(expected_exp))
  expect(length(cmp) == 0, message)
  invisible(act$val)
}

.expect_renders <- function(object, expected_expression, negate) {
  act <- testthat::quasi_label(rlang::enquo(object), arg="object")
  
  plot_md5 <- function(expr, prefix) {
    fn <- tempfile(pattern = prefix, fileext = ".png")
    
    png(fn, 640, 480, res=150)
    plot(expr, main=act$val)
    dev.off()
    tools::md5sum(fn)
  }
  
  if (is.character(act$val)) {
    result_expression <- TeX(act$val)
  } else{
    result_expression <- act$val
  }
  act$md5_1 <- plot_md5(result_expression, "latex2exp_")
  act$md5_2 <- plot_md5(expected_expression, "expression_")
  
  message <- sprintf(expect_plots_same_message,
                     act$val,
                     if (!negate) "" else "not",
                     names(act$md5_1),
                     names(act$md5_2),
                     result_expression)
  
  comparison_successful <- identical(unname(act$md5_1), unname(act$md5_2))
  if (negate) {
    comparison_successful <- !comparison_successful
  }
  
  if (!comparison_successful && interactive()) {
    plot(expected_expression, main="Expected")
    plot(result_expression, main="Result")
  }
  
  expect(comparison_successful,
         message)
  
  invisible(act$val)
} 