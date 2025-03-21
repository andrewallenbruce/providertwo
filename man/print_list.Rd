% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_misc.R
\name{print_list}
\alias{print_list}
\title{Print a named list}
\usage{
print_list(ls, prefix = "")
}
\arguments{
\item{ls}{\verb{<list>} to print}

\item{prefix}{\verb{<chr>} to prepend to each line}
}
\value{
\verb{<list>} invisibly
}
\description{
Print a named list
}
\examples{
\dontshow{if (rlang::is_interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
print_list(list(a = 1, b = 2, c = 3))
\dontshow{\}) # examplesIf}
}
\keyword{internal}
