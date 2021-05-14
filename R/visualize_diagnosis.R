
# Summarize ----

#' Stack all pre-post comparisons
#'
#' @param obj An object of class \code{clusterCVR}
#'
#' @importFrom foreach `%do%` foreach
#' @importFrom dplyr bind_rows left_join mutate `%>%`
#' @importFrom tibble enframe
#'
#' @export
#'
#' @seealso [summary.clusterCVR()], [graph_trend()]
#'
#' @examples
#' data("simdata_full")
#' em_full_eq <- clusterCVR(simdata_full, init = "equal")
#'
#' params_eq <- summ_params(em_full_eq)
#' graph_trend(params_eq, simdata_full)
#'
#'
summ_params <- function(obj) {

  foreach(t = 2:length(obj$iters), .combine = "bind_rows") %do% {
    params_t    <- enframe(obj$iters[[t]]$params, name = "param_id", value = "values_now")
    params_tpre <- enframe(obj$iters[[(t - 1)]]$params, "param_id", "values_pre")

    left_join(params_tpre, params_t, by = c("param_id")) %>%
      mutate(iter = t,
             diff = abs(values_now - values_pre),
             loglik_obs = obj$iters[[t]]$loglik)
  }
}


#' Graph parameter fit and log likelihood
#'
#' EM algorithms should exhibit a monotonically increasing
#' log likelihood. This function plots the log likelihood
#' across iterations, as well as the change maximum change
#' in the parameter values.
#'
#' @param params_stacked Output from \link{summ_params}
#' @param dat The input data for the original clusterCVR object
#'
#'
#' @importFrom tidyr pivot_longer
#' @importFrom glue glue
#' @import ggplot2
#' @import dplyr
#' @export
#'
graph_trend <- function(params_stacked, dat = data) {

  if (!"loglik_obs" %in% colnames(params_stacked)) {
    summ_df <-  group_by(params_stacked, iter) %>%
      summarize(`Maximum Change in Parameter (probability scale)` = max(diff))
  }

  if ("loglik_obs" %in% colnames(params_stacked)) {
    summ_df <- params_stacked %>%
      mutate(llobs_scale = loglik_obs / (dat$N*dat$D)) %>%
      group_by(iter) %>%
      summarize(`Maximum Change in Parameter (probability scale)` = max(diff),
                `Observed Log Likelihood (per data point)` = unique(llobs_scale))
  }

  summ_df %>%
    pivot_longer(cols = -c(iter), names_to = "metric", values_to = "value") %>%
    ggplot(aes(iter, value)) +
    facet_wrap(~metric, scales = "free_y", ncol = 1) +
    geom_point(size = 0.5) +
    geom_line() +
    theme(plot.background = element_rect(color = NA),
          axis.line = element_line(color = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.caption = element_text(size = 6),
          strip.background = element_rect(fill = "lightgray")) +
    labs(x = "EM Iteration",
         y = "Metric",
         caption = glue("Note: The parmater vector is the estimated pi's and mu's combined.
                      Higher observed log likelihood and lower sup-norms both indicate better fit."))
}
