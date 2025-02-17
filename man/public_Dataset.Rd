% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/public_Dataset_Distribution.R
\name{public_Dataset}
\alias{public_Dataset}
\title{Load Public API \code{Dataset}}
\usage{
public_Dataset(dataset, fname = TRUE)
}
\arguments{
\item{dataset}{\verb{<chr>} dataset title}

\item{fname}{\verb{<lgl>} Is \code{dataset} a function name?; default is \code{TRUE}}
}
\value{
\verb{<Dataset>} object
}
\description{
Load Public API \code{Dataset}
}
\examples{
public_Dataset("enrollees")

public_Dataset("hospitals")

public_Dataset("reassignments")

public_Dataset("opt_out")

public_Dataset("laboratories")

}
