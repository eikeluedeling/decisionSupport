% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eviSimulation.R
\name{summary.eviSimulation}
\alias{summary.eviSimulation}
\title{Summarize EVI Simulation Results}
\usage{
\method{summary}{eviSimulation}(object, ..., digits = max(3, getOption("digits") - 3))
}
\arguments{
\item{object}{An object of class \code{eviSimulation}.}

\item{...}{Further arguments passed to \code{\link{summary.welfareDecisionAnalysis}}.}

\item{digits}{how many significant digits are to be used for numeric and complex x.
The default, NULL, uses \code{getOption("digits")}. This is a suggestion: enough decimal places
will be used so that the smallest (in magnitude) number has this many significant digits,
and also to satisfy nsmall. (For the interpretation for complex numbers see \code{\link[base:Round]{signif}}.)}
}
\value{
An object of class \code{summary.eviSimulation}.
}
\description{
Produces result summaries of an Expected Value of Information (EVI) simulation obtained by 
 the function \code{\link{eviSimulation}}.
}
\seealso{
\code{\link{eviSimulation}}, \code{\link{print.summary.eviSimulation}}, 
\code{\link{summary.welfareDecisionAnalysis}}, \ifelse{latex}{\cr}{ }
 \code{\link{sort.summary.eviSimulation}}
}
