% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qpp.R
\name{quality_payment}
\alias{quality_payment}
\alias{quality_metrics}
\alias{quality_eligibility}
\title{Quality Payment Program}
\usage{
quality_metrics(year)

quality_eligibility(year, npi)
}
\arguments{
\item{year}{A vector of years from 2018 to 2025}

\item{npi}{A vector of NPIs}
}
\description{
Quality Payment Program
}
\examples{
quality_metrics(year = 2018:2025)
quality_eligibility(year = 2018, npi = c(1144544834, 1043477615, 1932365699))
quality_eligibility(year = 2024, npi = c(1144544834, 1043477615, 1932365699))
quality_eligibility(year = 2025, npi = c(1144544834, 1043477615, 1932365699))
}
