% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{query_formatter}
\alias{query_formatter}
\title{Format Queries}
\usage{
query_formatter(args)
}
\arguments{
\item{args}{named \verb{<list>} of \verb{<chr>} arguments}
}
\value{
\verb{<list>} of formatted query \verb{<exprs>}
}
\description{
Format Queries
}
\examples{
args = list(state = c("GA", "MD"),
            last_name = "SMITH",
            npi = 1234567890,
            PECOS = NULL)

query_formatter(args)

}
