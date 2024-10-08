% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latex2exp.R
\name{latex2exp}
\alias{latex2exp}
\title{Deprecated; use \code{\link{TeX}} instead.}
\usage{
latex2exp(string, output = c("expression", "character", "ast"))
}
\arguments{
\item{string}{A character vector containing LaTeX expressions. Note that any
backslashes must be escaped (e.g. "$\\alpha").}

\item{output}{The returned object, one of "expression" (default, returns a
plotmath expression ready for plotting), "character" (returns the
expression as a string), and "ast" (returns the tree used to generate the
expression).}
}
\value{
Returns an expression (see the \code{output} parameter).
}
\description{
Deprecated; use \code{\link{TeX}} instead.
}
