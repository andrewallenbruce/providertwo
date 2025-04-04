% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S7_args.R
\name{arg_npi}
\alias{arg_npi}
\alias{arg_state}
\title{\code{npi} argument class}
\usage{
arg_npi(x = NULL)

arg_state(x = NULL)
}
\arguments{
\item{x}{\verb{<chr>} vector of state abbreviations}
}
\value{
An S7 \verb{<arg_npi>} object.

An S7 \verb{<arg_state>} object.
}
\description{
\code{npi} argument class

\code{state} argument class
}
\examples{
arg_npi("1225701881")
arg_npi(1225701881)
arg_npi(npi_ex$k)
arg_npi(NULL)
arg_npi(NA_character_)

try(arg_npi(122570188))
try(arg_npi("12257O1881"))
try(arg_npi(1225701882))
arg_state("CA")

try(arg_state("C"))

try(arg_state("AA"))

# Allow duplicates?
arg_state(c("CA", "CA"))

arg_state(state.abb[1])

arg_state(state.abb)

arg_state(c(NULL, NULL))

arg_state(NA_character_)

}
