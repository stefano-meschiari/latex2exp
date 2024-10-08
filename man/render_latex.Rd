% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parser.R
\name{render_latex}
\alias{render_latex}
\title{Renders a LaTeX tree}
\usage{
render_latex(tokens, user_defined = list(), hack_parentheses = FALSE)
}
\arguments{
\item{tokens}{tree of tokens}

\item{user_defined}{any custom definitions of commands passed to
\code{\link{TeX}}}

\item{hack_parentheses}{render parentheses using
\code{group('(', phantom(), '.')} and \code{group(')', phantom(), '.')}.
This is useful to return valid expressions when the LaTeX source contains
mismatched parentheses, but makes the returned expression much less tidy.}
}
\value{
String that should be parseable as a valid plotmath expression
}
\description{
Returns a string that is a valid plotmath expression, given a LaTeX tree
returned by \code{parse_latex}.
}
