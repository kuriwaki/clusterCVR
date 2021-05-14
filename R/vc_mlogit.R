#' Log likelihood for varying choice mlogit
#'
#' Computes the observed log likelihood for a multinomial logit with varying
#' choice sets and weights. \code{ll_vclogit} implements by Rcpp. \code{ll_vclogit_R}
#' implements in R, and is slower.
#'
#' @param par The parameter of interest. A (L)-length vector of mlogit
#'  coefficients for cluster k and office j. The coefficient for the reference
#'  category, corresponding to abstention, is abbreviated here. That coefficient
#'  is constrained to be 0. Therefore, the log likelihood appends 0 to the beginning
#'  each time.
#' @param zeta_k N-length vector of posterior for membership in cluster k
#' @param y_j N length vector of outcomes
#' @param m_j N by (L + 1) matrix of missingness indicators
#'
#' @return When used via \code{optim}, the \code{par} command returns the MLE
#' parameter. See example.
#'
#' @export
#'
#' @examples
#'
#' # fit data from mlogit and compare
#'
#' fit <- optim(par = c(1, -2, 1),
#'              fn = ll_vclogit,
#'              y_j = canada_mlogit_y,
#'              zeta_k = canada_mlogit_w,
#'              m_j = canada_mlogit_m,
#'              method = 'BFGS')
#' fit$par
#'
#'
#' # mlogit replication ----
#' \dontrun{
#' library(mlogit)
#' data("ModeCanada")
#' MC_mlogit <- mlogit.data(ModeCanada,
#'                          chid.var = "case",
#'                          alt.var = "alt",
#'                          drop.index = TRUE)
#'
#' fit_mlogit_pkg <- mlogit(choice ~ 1,
#'                          data = MC_mlogit,
#'                          weights = dist,
#'                          reflevel = "train")
#' coef(fit_mlogit_pkg)
#' }
#'
ll_vclogit <- function(par, y_j, zeta_k, m_j) {
  ll_vclogit_rcpp(par, y_j, zeta_k, m_j)
}



#' Evaluate gradient of the log likelihood
#' @inheritParams  ll_vclogit
#' @export
grad_vclogit <- function(par, y_j, zeta_k, m_j) {
  grad_vclogit_rcpp(par, y_j, zeta_k, m_j)
}


