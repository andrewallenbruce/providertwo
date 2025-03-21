% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catalog_main.R
\name{main_temp_group}
\alias{main_temp_group}
\title{Load Temporal Main Endpoint Group}
\usage{
main_temp_group(alias)
}
\arguments{
\item{alias}{\verb{<chr>} dataset title alias}
}
\value{
\verb{<list>} of a group of temporal main endpoints
}
\description{
Load Temporal Main Endpoint Group
}
\examples{
\dontshow{if (rlang::is_interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
main_temp_group("utilization")
main_temp_group("prescribers")
main_temp_group("suppliers")
main_temp_group("outpatient")
main_temp_group("inpatient")
\dontshow{\}) # examplesIf}
}
