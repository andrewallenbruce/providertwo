% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/endpoint.R
\name{group}
\alias{group}
\title{Load a group of endpoints by alias}
\usage{
group(alias, description = NULL, call = caller_env())
}
\arguments{
\item{alias}{\verb{<chr>} Alias representing the CMS data endpoint.}

\item{description}{\verb{<chr>} Group description. Defaults to \code{NULL},
which will use the aliases as the description.}

\item{call}{\verb{<env>} Environment from which to call the function.}
}
\value{
S7 \verb{<class_group>} object.
}
\description{
Load a group of endpoints by alias
}
\examples{
group(c("asc_facility", "enterprise"))
try(group("asc_facility"))
}
