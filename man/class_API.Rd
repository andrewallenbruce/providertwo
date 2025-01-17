% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\name{class_API}
\alias{class_API}
\title{API Class}
\usage{
class_API(
  title = character(0),
  description = character(0),
  accrualPeriodicity = character(0),
  modified = numeric(0),
  temporal = character(0),
  identifier = class_Identifier(),
  accessURL = character(0),
  resourcesAPI = class_Resources(),
  downloadURL = character(0),
  describedBy = character(0),
  landingPage = character(0)
)
}
\arguments{
\item{title}{\verb{<chr>} Dataset title}

\item{description}{\verb{<chr>} Dataset description}

\item{accrualPeriodicity}{\verb{<chr>} Dataset update frequency}

\item{modified}{\verb{<chr>} Date Dataset was last modified}

\item{temporal}{\verb{<chr>} Date range the Current dataset covers}

\item{identifier}{\verb{<S7_class>} dcat:Dataset url and nrows in dataset}

\item{accessURL}{\verb{<chr>} dcat:Distribution url}

\item{resourcesAPI}{\verb{<S7_class>} \code{data.frame} of available supplemental resources}

\item{downloadURL}{\verb{<chr>} dcat:Distribution url to csv versions}

\item{describedBy}{\verb{<chr>} Link to Data dictionary}

\item{landingPage}{\verb{<chr>} Link to API landing page}
}
\value{
\verb{<S7_class>} object
}
\description{
\code{class_API} object
}
