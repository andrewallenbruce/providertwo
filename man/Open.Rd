% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S7_open.R
\name{openMain}
\alias{openMain}
\alias{openTemp}
\title{Open Payments Endpoint}
\usage{
openMain(alias)

openTemp(alias)
}
\arguments{
\item{alias}{\verb{<chr>} endpoint alias}
}
\value{
An S7 \verb{<openMain>} object.

An S7 \verb{<openTemp>} object.
}
\description{
Open Payments Endpoint

Open Payments Temporal Endpoint
}
\examples{
openMain("prof_cov")
openMain("prof_phys")
openMain("prof_info")
openMain("prof_map")
openMain("prof_entity")
openMain("prof_teach")
openMain("dashboard")
openMain("pay_state_total")
openMain("pay_state_group")
openMain("pay_nat_group")
openMain("pay_nat_total")
openTemp("general")
openTemp("ownership")
openTemp("research")
}
