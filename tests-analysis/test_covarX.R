library(dplyr)
library(ggplot2)
library(patchwork)
library(clusterCVR)

data("simdata_full")
sim_with_X <- simdata_full
sim_no_X <- simdata_full
sim_no_X$X <- NULL

# with X and no X
em_km_X  <- clusterCVR(sim_with_X, init = "kmeans", verbose = FALSE,
                       seed = 08441)
em_km_noX <- clusterCVR(sim_no_X, init = "kmeans", verbose = FALSE,
                        seed = 08441)

# check -- only as many thetas as there are combinations of X
em_km_X$ests$theta %>% dplyr::as_tibble() %>% distinct()

# compare classifications
table(em_km_X$ests$Z,  em_km_noX$ests$Z)

# compare -- mus should be the same
summary(em_km_X)
summary(em_km_noX)

# compare graphs
pars_X <- summ_params(em_km_X)
gg_X <- graph_trend(pars_X, sim_with_X) +
  labs(title = "with covariates")

pars_noX <- summ_params(em_km_noX)
gg_noX <- graph_trend(pars_noX, sim_no_X) +
  labs(title = "without covariates")

gg_X + gg_noX


