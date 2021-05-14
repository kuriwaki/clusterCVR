
#' print iterations to console
#' @param iter Number of iterations
#'
#' @keywords internal
print_iter <- function(iter) {
  if (iter <= 10 | iter %% 10 == 0) cat("\n", glue("iter: {iter}"))
  if (iter >  10 & iter %% 10 != 0) cat(".")
}
