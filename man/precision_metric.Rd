% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/external_validation.R
\name{precision_metric}
\alias{precision_metric}
\title{Method to calculate the precision}
\usage{
precision_metric(true_positive, false_positive)
}
\arguments{
\item{true_positive}{array with matching elements of B is in the same cluster}

\item{false_positive}{array with non matching element of B is in the same
cluster}
}
\value{
returns a double with the precision calculation
}
\description{
Method to calculate the precision
}
\keyword{internal}
