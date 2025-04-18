% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S7_open.R
\name{openMain}
\alias{openMain}
\title{Open Payments Endpoint}
\usage{
openMain(alias)
}
\arguments{
\item{alias}{\verb{<chr>} endpoint alias}
}
\value{
An S7 \verb{<openMain>} object.
}
\description{
Open Payments Endpoint
}
\examples{
openMain("profile_covered")
openMain("profile_physician")
openMain("profile_information")
openMain("profile_mapping")
openMain("profile_entity")
openMain("profile_teaching")
openMain("state_total")
openMain("state_group")
openMain("national_group")
openMain("national_total")
}
