# Diagnosis ----

# check sup-norm of changes in model parameters

#' Observed log likelihood
#'
#' @param iter the EM object with just one iteration
#' @param pi_it estimate of pi at iteration it
#' @param mu_it estimate of the mu array at iteration it
#' @param y A U by K matrix of the data
#' @param m A matrix of the missingness
#' @param n_u A vector of observation counts for each row in y
#' @param .IIA whether the model is using a multinomial logit.
#'
#'
#' @return the total log likelihood
#' @export
loglik_obs <- function(iter, pi_it, mu_it, y, m, n_u, .IIA) {
  loglik_obs <- c()

  # for each observation or profile
  for (i in seq_len(NROW(y))) {
    if (!.IIA)
      resp_i = mu3_yvec(mu_it, y[i, ])

    if (.IIA)
      resp_i = mu3_yvec_vchoice(mu_it, y[i, ], m[i, ])

    loglik_obs[i] <- n_u[i] * log(pi_it[i, ] %*% resp_i)
  }
  sum(loglik_obs)
}


#' Check change in log likelihood
#'
#' Print a warning if not improving
#'
#' @param loglik ll at iteration it
#' @param loglik_pre ll at previous iteration
#' @keywords internal
loglik_diff <- function(loglik, loglik_pre) {

  # log likelihood condition
  loglik_diff = loglik - loglik_pre
  if (loglik_diff < 0)
    warning(cat("log likelihood not monotonicaly increasing!"))

  loglik_diff
}

# Check change in parameters
#' @keywords internal
params_diff <- function(params, params_pre) {
  params_diff = max(abs(params - params_pre))
  params_diff
}
