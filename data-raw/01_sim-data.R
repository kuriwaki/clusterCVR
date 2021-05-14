library(tidyverse)
library(purrr)
library(foreach)
library(glue)

set.seed(02138)

# hyperparameter

#  assume missing by not available data?

gen_data <- function(L = 2L, D = 5L, N = 300L, K = 3L,
                     P = 2L,
                     missing = FALSE) {

  # individual-level covariates
  stopifnot(P == 2)
  X_cov <- tibble::frame_matrix(
    ~V1, ~V2,
    0.6, 0.1,
    0.1, 0.4)
  X_mat <- mvtnorm::rmvnorm(N, sigma = X_cov)

  # discretize. If there are 2 x 2 = 4 possibile
  # X profiles, then there will be 4 possible values of theta_i.
  X_mat_binary <- as.matrix(X_mat > 0)

  beta_init <- tibble::frame_matrix(
    ~K1,   ~K2,  ~K3,
    0,  -1.5, -2.00, # intercept
    0,  -2.0, -1.01, # X1
    0,  -0.4,  0.30, # X2
  )

  # convert to probabilities
  exp_XB <- exp(cbind(1, X_mat_binary) %*% beta_init) # N by K
  denom_XB <- apply(exp_XB, MARGIN = 1, sum)
  thetas <- apply(exp_XB, MARGIN = 2, function(x) x/denom_XB)

  # set theta parameters to cluster assignment
  Z_table <- foreach(i = seq_len(N), .combine = "cbind") %do% {
    rmultinom(1, size = 1, prob = thetas[i, ])
  }

  Z <-  map_dbl(1:N,  ~which(Z_table[, .x] == 1))

  # set mu parameters
  mu_k <- list(
    `1` = c(0.05, 0.05, 0.90),
    `2` = c(0.10, 0.10, 0.80),
    `3` = c(0.15, 0.20, 0.65)
  )

  # data$L
  mu <- array(NA, dim = c(K, D, L + 1))
  for (k in 1:K) {
    for (j in 1:D) {
      # scales vary by k primarily, but noise by j
      mu[k, j, ] <- mu_k[[k]] + 0.01*k*(j - 1)
      # rescale
      mu[k, j, ] <- mu[k, j, ] / sum(mu[k, j, ])
    }
  }

  # Missingness
  if (missing) {
    m <- array(sample(1:3, size = N*D, prob = c(0.25, 0.25, 0.50), replace = TRUE),
               dim = c(N, D))
    m[, 1:2] <- 3 # some top ticket races are "always contested"
  }
  if (!missing)
    m <- array(3, dim = c(N, D))

  # Generate data
  y <- array(NA, dim = c(N, D))
  indiv_mu <- array(NA, dim = c(N, D, L + 1))
  for (i in 1:N) {
    for (j in 1:D) {
      mu_i <- mu[Z[i], j, ]

      l_not_available <- switch(m[i, j], `1` = 2, `2` = 1, `3` = NA)
      if (!is.na(l_not_available)) mu_i[1 + l_not_available] <- 0

      y[i, j] <- sample(0:L, size = 1, prob = mu_i)
      indiv_mu[i, j, ] <- mu_i
    }
  }

  # unique profiles
  unique_y <- as_tibble(y) %>%
    mutate(voter = 1:n()) %>%
    pivot_longer(-voter, names_to = "j", values_to = "y") %>%
    group_by(voter) %>%
    summarize(profile = str_c(y, collapse = "")) %>%
    count(profile, name = "n_u") %>%
    separate(profile, into = str_c("v", 1:D), sep = 1:D) %>%
    mutate_all(as.integer) %>%
    as.matrix()

  unique_y_mat <- unique_y[, 1:D]
  n_u <- unique_y[, "n_u"]
  U <- length(n_u)


  # put together data
  simdata <- list(D = D,
                  K = K,
                  N = N,
                  L = L,
                  y = y,
                  m = m,
                  X = X_mat_binary,
                  uy = unique_y_mat,
                  n_u = n_u,
                  U = U)
  return(simdata)
}


simdata_miss <- gen_data(missing = TRUE)
simdata_full <- gen_data(missing = FALSE)


usethis::use_data(simdata_miss, overwrite = TRUE)
usethis::use_data(simdata_full, overwrite = TRUE)
