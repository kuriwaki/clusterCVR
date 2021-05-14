#' Print summary of clusterCVR object
#'
#' @param x An object of class \link{clusterCVR}
#'
#' @importFrom glue glue
#' @importFrom stringr str_c
#'
#' @export
summary.clusterCVR <- function(x) {

  x_last <- x$ests
  pi_avg <- colMeans(x_last$pi)
  th_vec2 <- format(round(pi_avg, 2), nsmall = 2)
  th_vec3 <- round(pi_avg, 3)

  mu_str_mat <- x_last$mu[, , 3]
  mu_spl_mat <- x_last$mu[, , 2]
  mu_abs_mat <- x_last$mu[, , 1]

  # format
  colnames(mu_abs_mat) <-
    colnames(mu_str_mat) <-
    colnames(mu_spl_mat) <-
    abbreviate(x$aux$y_names, minlength = 3)

  rownames(mu_abs_mat) <-
    rownames(mu_str_mat) <-
    rownames(mu_spl_mat) <-
    glue("cl_{1:nrow(mu_spl_mat)} ({th_vec2})")

  # order by prevalence
  ind_ord <- order(pi_avg, decreasing = TRUE)
  mu_str_mat <- mu_str_mat[ind_ord, ]
  mu_spl_mat <- mu_spl_mat[ind_ord, ]
  mu_abs_mat <- mu_abs_mat[ind_ord, ]

  cat(glue("An EM object of class clusterCVR"), "\n")
  cat(glue("Contains {length(x$iters)} iterations"), "\n")
  cat(glue("Estimated with {length(pi_avg)} clusters"), "\n")
  cat(glue("Estimated proportions are [{str_c(th_vec3, collapse = ', ')}]."), "\n")
  cat("\n")
  cat("The estimated proportions for L = 2 ticket voting\nfor each cluster (rows) for each office (columns) are:", "\n")

  print(round(mu_str_mat, 2))
  cat("\n")
  cat("The estimated proportions for L = 1 ticket voting\nfor each cluster (rows) for each office (columns) are:", "\n")
  print(round(mu_spl_mat, 2))
  cat("\n")
  cat("The estimated proportions for L = 0\nfor each cluster (rows) for each office (columns) are:", "\n")
  print(round(mu_abs_mat, 2))
}
