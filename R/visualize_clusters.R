#' Extract the latent choice parameter in tidy format
#'
#' @param object An object estimated by clusterCVR
#'
#' @details Currently it hard-codes the levels 1 as abstain, 2 as split, and 3
#' as straight.
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr `%>%` mutate recode select
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_c str_remove_all
#'
#' @return A tibble in tidy (long) format, where each row defines
#'  a value of mu for a particular cluster, office, and vote choice. In the
#'  current notation, there are `K` by `D` by `L + 1` observations.
#'
#' @examples
#' data(simdata_miss)
#' cl_out  <- clusterCVR(simdata_miss, IIA = TRUE, init = "kmeans")
#'
#' mu_out <- get_mus(cl_out)
#'
#'
#' @export
#'
get_mus <- function(object) {

  office_names <- object$aux$y_names
  if (is.null(office_names)) {
    # cat("names of variables not provided; assigning V1, V2, ...", "\n")
    office_names <- str_c("V", seq_len(dim(object$ests$mu)[2]))
  }

  # last iter
  data <- object$ests

  n_levels <- (object$aux$L) + 1

  # run and bind
  bind_rows(lapply(seq_len(n_levels), function(x) get_mu_ell(outcome = x, data, office_names)))

}

#' Get mu for ell
#' @keywords internal
#'
#' @rdname get_mus
#' @export
get_mu_ell <- function(outcome, data, names = office_names) {
  out_name <- recode(outcome,
                     `1` = "abstain",
                     `2` = "split",
                     `3` = "straight",
                     `4` = "other")

  mu_df <- as_tibble(data$mu[, , outcome])
  th_prop <- colMeans(data$pi)

  colnames(mu_df) <- names

  mu_long <- mu_df %>%
    mutate(cluster = str_c("c", 1:nrow(mu_df)),
           pi = th_prop) %>%
    pivot_longer(-c(cluster, pi),
                 names_to  = "office",
                 values_to = "mu") %>%
    mutate(office = str_remove_all(office, "_split"),
           vote = out_name) %>%
    select(cluster, pi, office, vote, mu)
}


#' Setup and visualize cluster characteristics
#'
#' \link{fmt_mu_viz} will do a few more formatting steps to prepare for visualization.
#' To actually run the graph, use the sample code in the examples below. I do not
#' make the sample code into a function for now because it likely differs case by case.
#'
#' @param tbl output of get_mus
#'
#' @importFrom dplyr recode_factor mutate `%>%` arrange left_join distinct desc n
#' @importFrom scales percent percent_format
#' @importFrom glue glue
#'
#' @examples
#'
#' \dontrun{
#' library(ggplot2)
#' data(simdata_miss)
#' cl_out  <- clusterCVR(simdata_miss, IIA = TRUE, init = "kmeans")
#'
#' mu_out <- get_mus(cl_out)
#' mu_fmt <- fmt_mu_viz(mu_out)
#'
#' ggplot(mu_fmt, aes(x =  mu, y = fct_rev(office), fill = fct_rev(vote))) +
#'   facet_wrap(~ cl_lbl) +
#'   geom_col(width = 0.5) +
#'   theme_classic() +
#'   guides(fill = guide_legend(name = "", reverse = TRUE)) +
#'   geom_text(aes(label = pct_lbl),
#'             position = position_stack(vjust = 0.5),
#'             size = 2.5,
#'             alpha = 0.8,
#'             color = "black",
#'             family = "mono") +
#'   theme(legend.position = "bottom",
#'         axis.text = element_text(color = "black"),
#'         axis.line.y = element_blank()) +
#'   labs(y = "",
#'        x = "Probability of Voting ...",
#'        fill = "") +
#'   scale_x_continuous(expand = c(0, 0)) +
#'   scale_fill_brewer(palette = "Greys") +
#'   theme(panel.spacing = unit(2, "lines"),
#'         legend.margin = unit(c(0, 0, 0, 0), "cm"),
#'         legend.box.spacing = unit(0.2, "lines"),
#'         legend.spacing = unit(0.2, "lines"))
#' }
#' @export
fmt_mu_viz <- function(tbl) {
  mu_all <- tbl %>%
    mutate(office = recode_factor(office,
                                  USS = "US Senate",
                                  USH = "US House",
                                  SEN = "State Senate",
                                  HOU = "State House",
                                  SHF = "Sheriff",
                                  GOV = "Governor",
                                  ATG = "Attorney General",
                                  SOS = "Secretary of State",
                                  CCD = "County Council")) %>%
    mutate(vote = recode_factor(vote,
                                straight = "Straight",
                                split = "Split",
                                other = "Third Party",
                                abstain = "Abstain")) %>%
    mutate(pct_lbl = percent(mu, accuracy = 1)) %>%
    arrange(office, vote)

  cl_label <- distinct(mu_all, cluster, pi) %>%
    arrange(desc(pi)) %>%
    mutate(ord = 1:n()) %>%
    mutate(cl_lbl = glue("Cluster {ord} ({round(100*pi)}%)"))

  left_join(mu_all, cl_label) %>%
    mutate(pct_lbl = replace(pct_lbl, !(vote == "Split"), NA))
}
