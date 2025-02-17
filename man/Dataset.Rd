% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\name{Dataset}
\alias{Dataset}
\title{Dataset Class}
\usage{
Dataset(
  type = "dcat:Dataset",
  access = "public",
  bureau = "009:38",
  program = "009:000",
  contact = Contact(),
  identifier = Identifier(),
  publisher = Publisher(),
  resources = Resources(),
  modified = numeric(0),
  title = character(0),
  periodicity = character(0),
  dictionary = character(0),
  description = character(0),
  keyword = character(0),
  landingpage = character(0),
  references = character(0),
  temporal = character(0),
  theme = character(0)
)
}
\arguments{
\item{type}{\verb{<chr>} Schema type; default is \code{dcat:Dataset}}

\item{access}{\verb{<chr>} Dataset access level; default is \code{public}}

\item{bureau}{\verb{<chr>} Dataset bureau code; default is \code{009:38}}

\item{program}{\verb{<chr>} Dataset program code; default is \code{009:000}}

\item{contact}{\verb{<S7_class>} Dataset contact}

\item{identifier}{\verb{<S7_class>} dcat:Dataset url and nrows in dataset}

\item{publisher}{\verb{<S7_class>} Dataset publisher}

\item{resources}{\verb{<S7_class>} \code{data.frame} of available supplemental resource files; default is \code{NA}}

\item{modified}{\verb{<dbl> | <Date>} Date Dataset was last modified}

\item{title}{\verb{<chr>} Dataset title}

\item{periodicity}{\verb{<chr>} Dataset update frequency}

\item{dictionary}{\verb{<chr>} Hyperlink to Data dictionary}

\item{description}{\verb{<chr>} Dataset description}

\item{keyword}{\verb{<chr>} Hyperlink to API landing page}

\item{landingpage}{\verb{<chr>} Hyperlink to API landing page}

\item{references}{\verb{<chr>} Dataset references}

\item{temporal}{\verb{<chr>} Date range the Current dataset covers}

\item{theme}{\verb{<chr>} Dataset theme}
}
\value{
\verb{<S7_class>} Dataset object
}
\description{
Dataset Class
}
\seealso{
Other classes: 
\code{\link{Contact}()},
\code{\link{Identifier}()},
\code{\link{Publisher}()},
\code{\link{Resources}()}
}
\concept{classes}
