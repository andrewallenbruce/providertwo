% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods_base_request.R
\name{query_nresults}
\alias{query_nresults}
\title{Request the number of results for a query}
\usage{
query_nresults(x, ...)
}
\arguments{
\item{x}{An \verb{<int>} vector}

\item{...}{Additional arguments}
}
\value{
An \verb{<int>} vector
}
\description{
Request the number of results for a query
}
\examples{
\dontshow{if (rlang::is_interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
care_endpoint("care_enrollees") |> query_nresults()
care_temporal("quality_payment") |> query_nresults()
care_group("care_hospital") |> query_nresults()
prov_endpoint("pdc_affiliations") |> query_nresults()
prov_group("pro_mips") |> query_nresults()
open_endpoint("profile_covered") |> query_nresults()
open_temporal("payment_general") |> query_nresults()
open_group("payment_grouped") |> query_nresults()
caid_endpoint("mlr_summary") |> query_nresults()
caid_temporal("healthcare_quality") |> query_nresults()
hgov_endpoint("hgov_catastrophic") |> query_nresults()
hgov_temporal("hgov_mlr") |> query_nresults()
\dontshow{\}) # examplesIf}
}
