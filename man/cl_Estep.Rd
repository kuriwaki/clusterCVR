% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clusterCVR.R
\name{cl_Estep}
\alias{cl_Estep}
\title{E-step for clusterCVR}
\usage{
cl_Estep(zeta = zeta_hat, .mu, .pi, y = data$uy, m = data$m, .IIA)
}
\arguments{
\item{zeta}{N by K matrix of Zetas to update}

\item{y}{the list of voters}

\item{m}{missingness}

\item{.IIA}{whether or not there is missingness. IIA = TRUE if yes.}

\item{mu}{parameters mu}

\item{pi}{parameters pi}
}
\description{
E-step for clusterCVR
}
\keyword{internal}
