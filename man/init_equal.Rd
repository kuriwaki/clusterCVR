% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initial_values.R
\name{init_equal}
\alias{init_equal}
\alias{init_kmeans}
\title{Initialize mu, pi, and zeta by equal assignment}
\usage{
init_equal(data, user_K, seed)

init_kmeans(data, user_K, seed)
}
\arguments{
\item{data}{from the object}

\item{user_K}{number of clusters to presume}

\item{seed}{seed for initial randomization}
}
\value{
\describe{
\item{pi}{A N by K matrix. Each row sums to 1.}
\item{mu}{A K by D by (L + 1) array. Each \code{mu[k, j, ]} sums to 1}
}
}
\description{
Initialize mu, pi, and zeta by equal assignment

Initialize mu, pi, and zeta by k-means guess
}
