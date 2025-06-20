% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods_base_request.R
\name{base_request}
\alias{base_request}
\title{Create a new \verb{<httr2_request>} by class}
\usage{
base_request(x, ...)
}
\arguments{
\item{x}{A \code{class_endpoint}, \code{class_temporal} or \code{class_group} object}

\item{...}{Additional arguments}
}
\value{
An \verb{<httr2_request>} object or list of \verb{<httr2_request>} objects
}
\description{
Create a new \verb{<httr2_request>} by class
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
care_endpoint("care_enrollees") |> base_request()
care_temporal("quality_payment") |> base_request()
care_group("care_hospital") |> base_request()
prov_endpoint("pdc_affiliations") |> base_request()
prov_group("pro_mips") |> base_request()
open_endpoint("profile_covered") |> base_request()
open_temporal("payment_general") |> base_request()
open_group("payment_grouped") |> base_request()
caid_endpoint("mlr_summary") |> base_request()
caid_temporal("healthcare_quality") |> base_request()
hgov_endpoint("hgov_catastrophic") |> base_request()
hgov_temporal("hgov_mlr") |> base_request()
\dontshow{\}) # examplesIf}
}
