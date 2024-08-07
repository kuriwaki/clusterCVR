% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{canada_mlogit_y}
\alias{canada_mlogit_y}
\alias{canada_mlogit_w}
\alias{canada_mlogit_m}
\title{varying choice set mlogit replication}
\format{
An object of class \code{numeric} of length 4324.

An object of class \code{integer} of length 4324.

An object of class \code{matrix} (inherits from \code{array}) with 4324 rows and 4 columns.
}
\usage{
canada_mlogit_y

canada_mlogit_w

canada_mlogit_m
}
\description{
Replication data from the \code{mlogit} package, which is available
by \code{ModeCanada}. This data shows the mode of transportation 14,000+
respondents in Canada took.
}
\details{
\code{canada_mlogit_y} is a length 4,324 vector of the outcome.
It is coded numerically with the reference category at 0. 0 is "train",
1 is "air", 2 is "bus", and 3 is "car".

\code{canada_mlogit_m} is a 4,324 - by - 4 matrix indicating the availability
of the choice on the menu. Cell (i, l) is 1 is individual i had option l
available on the menu, 0 otherwise. Because this is a R matrix, the columns are
1-based.

\code{canada_mlogit_w} is a length 4,324 vector of the weights, which are
are necessary in a clustering task. These are arbitrarily the individual-specific
variable "dist" in the ModeCanada dataset.
}
\keyword{datasets}
