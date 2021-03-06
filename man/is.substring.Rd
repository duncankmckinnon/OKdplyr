% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscutils.R
\name{is.substring}
\alias{is.substring}
\alias{\%substr\%}
\title{Check if string x is a substring of y}
\usage{
is.substring(x, y, start = FALSE)

lhs \%substr\% rhs
}
\arguments{
\item{x}{character - a single character string}

\item{y}{character - a single character string that may contain x}

\item{start}{logical - if TRUE only check the first subsequence of y for x}
}
\value{
logical - whether x is a substring of y
}
\description{
check if a character string x is contained within another character string y.
If the 'start' condition is set, only check if y starts with x (i.e. for partial match evaluations).
The \%substr\% operator works equivalently, returning true if the lhs is a substring of the rhs.
}
\examples{
is.substring('abc', 'abcdef', start = TRUE) # TRUE
is.substring(' abc', 'abcdef', start = TRUE) # FALSE
is.substring('xyz', 'abcdef') # FALSE
is.substring('xyz','uvwxyz') # TRUE
'abc' \%substr\% '_abcdef_' # TRUE
}
