% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NglWidget.R
\name{setVisibility}
\alias{setVisibility}
\title{hide or show the named selection}
\usage{
setVisibility(session, representationName, newVisibilityState)
}
\arguments{
\item{session}{a Shiny server session object.}

\item{representationName}{a previously assigned character string}
}
\description{
hide or show the named selection
}
\examples{
\dontrun{
  setVisibility(session, "chromaphore", FALSE)
}

}
