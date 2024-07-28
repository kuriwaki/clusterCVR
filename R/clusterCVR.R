#' Cluster CVR data with a EM algorithm
#'
#' Compute cluster assignment probabilities by a EM algorithm. The required
#' inputs are a numeric data matrix and the number of clusters.
#'
#'
#' @details See \link{fmt_mu_viz} for a quick way to visualize the output.
#'
#'
#' @param data the dataset, in list form, with the following slots.
#' \describe{
##'  \item{`y`}{A n by K matrix of split indicators}
##'  \item{`m`}{A n by K matrix of missingness indicators. 3 means no missing,
##'  2 means only straight is available, and 1 means only split is available.}
##'  \item{`X`}{An optional n by P matrix of covariates with respondent-specific covariates.}
##'  \item{`n_u`}{An integer scalar for the number of voters}
##'  \item{`L`}{An integer scalar for the number of possible y values (0-indexed)}
##'  \item{`uy`}{A n by K matrix of unique profiles y, needed if fast = TRUE}
##' }
#' @param user_K the number of clusters to presume / compute
#' @param runs Number of replications (with different starting values to run). Default is
#'    1 but more than 1 is highly recommended if computing time is not prohibitive.
#' @param loglik_thresh the threshold value for convergence. The EM will stop when the
#'  _relative_ change in log likelihood is less than the threshold.
#' @param n_iter manual limit to iterations
#' @param init method of initialization
#' @param seed seed for initialization
#' @param fast summarize data to unique profiles, so estimation is faster? Currently
#'  only possible if IIA = FALSE. Defaults to \code{FALSE}.
#' @param IIA assume that the data$y matrix is generated from a varying choice set
#'  as defined by data$m? Defaults to \code{FALSE}.
#' @param verbose Defaults to TRUE.
#' @param subset A vector of row indices or row names to subset all the data by.
#'  Useful when wanting to test a small subset of the data without modifying the
#'  \code{data} list. If \code{fast = TRUE}, it will subset \code{n_u} and \code{uy},
#'  If \code{fast = FALSE}, it will subset \code{y}.
#' @param recode_key A named vector to be passed on to \code{dplyr::recode}, in the
#' form \code{(old1 = new1, old2 = new2, ...)}
#' @param ignore_X Should X be set to NULL even if it is provided? Useful when
#' switching between covariates and non-covariates case. Defaults to \code{FALSE}.
#' @param pi,mu,zeta_hat initial values of the key parameters, if there
#' are any good guesses. If left \code{NULL}, it  will initialize based on the
#' method in \code{"init"}.  Follow the format of the output.
#'
#' @return \describe{
#' \item{ests}{The last iteration}
#' \item{iters}{Stored iterations}
#' \item{aux}{A list of stored items not specific to iterations. These include
#'  the initial values, parameters, total time data, and settings.}
#' \item{seeds_run}{A vector of `runs` seeds that were used.}
#' \item{loglik_run}{A vector of `runs` final loglikelihood estimates corresponding
#'   to each run of the model. Only The model with the highest log likelihood is stored.}
#' }
#'
#'
#' @importFrom Rcpp evalCpp
#' @importFrom stringr str_c
#' @importFrom glue glue
#' @importFrom rlang set_names
#' @importFrom emlogit emlogit
#' @importFrom stats optim
#'
#' @useDynLib clusterCVR, .registration = TRUE
#' @export
#'
#' @examples
#' em_full <- clusterCVR(simdata_full, init = "kmeans", runs = 2)
#'
#' summary(em_full)
#'
#' \dontrun{
#'  pars <- summ_params(em_full)
#'  graph_trend(pars, simdata_full)
#' }
#'
#' em_miss  <- clusterCVR(simdata_miss, IIA = TRUE)
#'
clusterCVR <- function(data,
                       user_K = 3,
                       loglik_thresh = 1e-5,
                       runs = 1,
                       n_iter = Inf,
                       fast = FALSE,
                       IIA = FALSE,
                       init = "kmeans",
                       subset = NULL,
                       ignore_X = FALSE,
                       recode_key = NULL,
                       seed = 02138,
                       verbose = TRUE,
                       pi = NULL, mu = NULL, zeta_hat  = NULL) {

  # subset rows -----
  if(!is.null(subset)) {
    data$X <- data$X[subset, ]
    data$m <- data$m[subset, ]

    if (fast) {
      # if fast update uy
      data$uy <- data$uy[subset, ]
      data$n_u <- data$n_u[subset]
    } else {
      # if normal update y
      data$y <- data$y[subset, ]
    }
  }

  if (ignore_X) {
    data$X <- NULL
  }

  # Recode values?
  if (!is.null(recode_key)) {
    data$y <- apply(data$y, MARGIN = 2, FUN = function(x) recode(x, !!!recode_key))
  }

  # Unique profile and speed up? ----
  if (!fast) {
    data$U <- data$N <- nrow(data$y)
    data$n_u <- rep(1, data$N)
    data$uy <- data$y
  }

  if (fast) {
    data$N <- sum(data$n_u)
    data$U <- length(data$n_u)
    data$y <- data$uy
  }

  data$D <- NCOL(data$y)

  # check before computing ---
  cl_check_start(data = data, fast = fast, IIA = IIA)

  # run an iteration of clusterCVR ------
  seed_runs <- loglik_runs <- array(dim = runs)

  for (run in seq_len(runs)) {

    if (run == 1) {
      out <- .cluster(data, user_K, seed, n_iter, fast = fast, IIA = IIA,
                      init = init, loglik_thresh = loglik_thresh, verbose = verbose)
      loglik_runs[[run]] <- loglik_it <-  out$aux$loglik
      seed_runs[[run]] <- seed

    }

    # run again
    if (run > 1) {
      new_seed <- sample.int(n = 1e3, size = 1)
      cl_next <- .cluster(data, user_K, seed = new_seed, n_iter, fast = fast, IIA = IIA,
                          init = init, loglik_thresh = loglik_thresh, verbose = verbose)
      loglik_runs[[run]] <- cl_next$aux$loglik
      seed_runs[[run]] <- new_seed

        # if next model has higher log likelihood, save that
        if (cl_next$aux$loglik > loglik_it) {
          out <- cl_next
          loglik_it <- cl_next$aux$loglik
        }
    }
    # end of run
  }

  # save final output
  out$seed_runs <- seed_runs # store seeds
  out$loglik_runs <- loglik_runs # store variation in log likelihoods
  out
}

#' Internal clusterCVR function
#'
#' @rdname clusterCVR
#' @keywords internal
.cluster <- function(data, user_K, seed,
                     n_iter, loglik_thresh,
                     fast, IIA, init, verbose) {

  t_all_start <- Sys.time()

  # Initialize -------
  if (init == "equal")  init_out <- init_equal(data, user_K, seed)
  if (init == "kmeans") init_out <- init_kmeans(data, user_K, seed)

  pi = init_out$init_pi
  mu = init_out$init_mu
  zeta_hat = matrix(NA, nrow = data$U, ncol = user_K)

  pi_names = str_c("pi_", seq_len(user_K))
  mu_names = str_c(
    str_c(
      str_c("mu_", seq_len(user_K), "_"),
      rep(seq_len(data$D), each = user_K), "_"
    ),
    rep(0:data$L, each = user_K*data$D)
  )

  # Loop setup
  iter = 1
  store = list()

  if (iter == 1) {
    loglik_change = NULL
    params_change = NULL
    loglik_cond = FALSE

    # only used when IIA
    psi_init = array(1, dim = c(user_K, data$D, data$L),
                     dimnames = list(pi_names, seq_len(data$D), seq_len(data$L)))
    psi_init[, , data$L] = 3 # prior that Pr(y = data$L) is more likely than Pr(y = 0)

    # only used with covariates
    gamma_init = NULL

    # only used for non-IIA
    l_mat = matrix(rep(0:data$L, each = data$U), nrow = data$U)
  }

  # start EM loop ------
  while (!loglik_cond) {

    ## E step ----
    t_Estep_start <- Sys.time()
    zeta_hat <- cl_Estep(zeta = zeta_hat,
                         .mu = mu,
                         .pi = pi,
                         y = data$uy,
                         m = data$m,
                         .IIA = IIA)
    t_Estep_end <- Sys.time()

    # M step ----
    t_Mstep_start <- Sys.time()

    ## update pi -----
    pi_obj <- cl_Mstep_pi(zeta = zeta_hat,
                                n_u = data$n_u,
                                X = data$X,
                                covs_gamma = gamma_init)
    pi = pi_obj$pi
    gamma_init = pi_obj$gamma # update gamma

    ##  update mu -----
    for (j in seq_len(data$D)) {
      ## In office j, use IIA or not
      use_IIA_j <- (!all(data$m[, j] == 3L) & IIA & !fast)

      # normal columns
      if (!use_IIA_j) {
        for (k in seq_len(user_K)) {
          mu[k, j, ] = cl_Mstep_mu_bsl(y_j = data$uy[, j],
                                       zeta_k = zeta_hat[, k],
                                       n_u = data$n_u,
                                       l_mat)
        }
      }

      # IIA columns
      if (use_IIA_j) {
        ms_vec = do.call("rbind", purrr::map(1:data$U, function(x) choice_ind(data$m[x, j], data$L)))

        for (k in seq_len(user_K)) {
          mlogit_psi <- cl_Mstep_mu_IIA(y_j = data$uy[, j],
                                        zeta_k = zeta_hat[, k],
                                        m_j_fmt = ms_vec,
                                        psi_init = psi_init[k, j, ])
          # update
          psi_init[k, j, ] <- mlogit_psi

          # rescale to probabilities
          exp_psi <- exp(c(0, mlogit_psi))
          mu[k, j, ] <- exp_psi / sum(exp_psi)
        }
      }
    }
    t_Mstep_end <- Sys.time()

    # parameters as a single vector ---
    params_vec = set_names(x = c(colMeans(pi), mu[, , 1:(data$L + 1)]),
                            nm = c(pi_names, mu_names))

    # compute log likelihood ----
    t_conv_start <- Sys.time()

    loglik = loglik_obs(store[[iter]],
                        pi_it = pi,
                        mu_it = mu,
                        y = data$uy,
                        m = data$m,
                        n_u = data$n_u,
                        .IIA = IIA)

    # Convergence check -----
    # only check after 1st iteration, and before it hits the iter_max
    # break if at max
    if (!is.infinite(n_iter) & iter >= n_iter)
      loglik_cond = TRUE

    # update condition ---
    if (iter > 1) {
      loglik_change <- loglik_diff(loglik, store[[iter - 1]]$loglik)
      params_change <- params_diff(params_vec, store[[iter - 1]]$params)
      loglik_cond = (abs(loglik_change) / abs(store[[iter - 1]]$loglik)) < loglik_thresh
    }

    t_conv_end <- Sys.time()

    # auxiliary information ----
    aux_iter = list(time_Estep = difftime(t_Mstep_end, t_Mstep_start),
                    time_Mstep = difftime(t_Mstep_end, t_Mstep_start),
                    time_conv  = difftime(t_conv_end, t_conv_start),
                    loglik_change = loglik_change,
                    params_change = params_change)

    # store object ---
    store[[iter]] = list(mu = mu,
                         pi = pi,
                         psi = psi_init,
                         params = params_vec,
                         zeta = zeta_hat,
                         Z = apply(zeta_hat, 1, which.max),
                         aux = aux_iter,
                         loglik = loglik)

    # update iterations
    if (verbose)
      print_iter(iter)
    iter = iter + 1
  }

  t_all_end <- Sys.time()
  t_all <- difftime(t_all_end, t_all_start, units = 'mins')

  # if covariates are estimated, store the full regression object
  # with SEs
  if (!is.null(gamma_init)) {
    gamma_obj = emlogit::emlogit(Y = zeta_hat,
                                 X = data$X,
                                 control = list(initial_values = gamma_init))
  } else {
    gamma_obj = NULL
  }

  # auxiliary information for the whole object
  aux_all <- list(fast = fast,
                  IIA = IIA,
                  init_values = list(pi = init_out$init_pi,
                                     mu = init_out$init_mu),
                  N = data$N,
                  D = data$D,
                  y_names = colnames(data$y),
                  L = data$L,
                  gamma = gamma_obj,
                  loglik = loglik,
                  time = t_all)

  if (verbose)
    cat("\n", glue("Done at iter: {iter - 1} after {round(t_all, 2)} mins."), "\n")

  out <- list(ests = store[[iter - 1]],
              iters = store,
              aux = aux_all)

  class(out) <- "clusterCVR"
  out
}

#' check initial settings
#' @keywords internal
cl_check_start <- function(data, IIA, fast) {
  stopifnot(all(require(purrr), require(glue)))
  if (data$L != 2) warning("Labelling may be off for L > 2")
  stopifnot(all(!is.na(data$X)) & all(!is.na(data$y)) & all(!is.na(data$m)))
  if (!is.null(data$X) & !all(data$n_u == 1)) stop("Cannot do covariates with weighted count data")
  if (IIA & fast) stop("Cannot do an IIA setup with fast option")
}


#' E-step for clusterCVR
#'
#' @param zeta N by K matrix of Zetas to update
#' @param mu parameters mu
#' @param pi parameters pi
#' @param y the list of voters
#' @param m missingness
#' @param .IIA whether or not there is missingness. IIA = TRUE if yes.
#'
#' @keywords internal
#'
cl_Estep <- function(zeta = zeta_hat, .mu, .pi, y = data$uy, m = data$m, .IIA) {
  N <- NROW(y)

  # for each row, update the simplex zeta
  for (u in seq_len(N)) {
    # responsibility parameter - mu weighted by the data
    if (!.IIA)
      resp_u = mu3_yvec(.mu, y[u, ])
    if (.IIA)
      resp_u = mu3_yvec_vchoice(.mu, y[u, ], m[u, ])

    # zeta \propto pi*resp_u
    numer_u = .pi[u, ] * resp_u # K x 1
    denom_u_k = sum(.pi[u, ] %*% resp_u) # scalar

    zeta[u, ] = numer_u / denom_u_k # K x 1
  }

  zeta
}

#' Estep (MLE) for pi
#'
#' @param zeta A N by K matrix of responsibility parameters
#' @param n_u A N by 1 vector for the number of observations for each profile.
#' u indexes "unique" profiles, instead of observations. If \code{fast} = \code{FALSE},
#' and the observations are simply voters, the value is 1 for everyone
#' @param X covariates, if available.
#' @param covs_gamma starting coefficients from \code{emlogit(zeta, X)}.
#'
#' @return A K by 1 vector of the mixing proportion pi (pi)
#'
#' @importFrom emlogit emlogit
#'
#'
#' @keywords internal
cl_Mstep_pi <- function(zeta, n_u, X = NULL, covs_gamma = NULL) {

  # no covariates
  if (is.null(X)) {
    sum_zeta = as.numeric(n_u %*% zeta)
    pi_i = (1/sum(n_u))*sum_zeta
    gamma = NULL
    pi = matrix(rep(pi_i, times = nrow(zeta)), byrow = TRUE, nrow = nrow(zeta))
  }

  # with covariates -- only doable with non-collapsed data
  if (!is.null(X) & all(n_u == 1)) {
    out_Zx = emlogit(Y = zeta,
                     X = X,
                     control = list(variance = FALSE,
                                    initial_values = covs_gamma))
    gamma = out_Zx$coef
    pi = emlogit:::predict.emlogit(out_Zx)
  }

  # pi should get updated
  stopifnot(!identical(round(pi,  2), 3.14))

  list(pi = pi,
       gamma  = gamma)
}


#' compute mu with a custom multinomial logit
#' @param psi_init L psi parameters (for coefficients excluding baseline abstain)
#' @param y_j vector of complete y at office j
#' @param zeta_k responsibility parameters, for each observation, for cluster k,
#' @param m_j_fmt missingness vector organized in particular way for optim
#'
#' @keywords internal
cl_Mstep_mu_IIA <- function(psi_init, y_j, zeta_k, m_j_fmt) {
  mfit <- optim(par = psi_init,
                fn = ll_vclogit_rcpp,
                gr = grad_vclogit_rcpp,
                y_j = y_j,
                zeta_k = zeta_k,
                m_j = m_j_fmt,
                method = 'BFGS')
  mfit$par
}

#' compute mu for cluster k and office j
#' @param y_j vector of complete y at office j
#' @param zeta_k responsibility parameters, for each observation, for cluster k,
#' @param n_u vector of sizes
#' @param l_mat A constant N by (L+1) matrix to transform the y_j vector into
#'  a matrix of indicators
#' @return A L + 1 -length simplex mu
#' @keywords internal
cl_Mstep_mu_bsl <- function(y_j, zeta_k, n_u, l_mat) {
  # 1(Y_{ij} = ell)
  y_is_l = (y_j - l_mat) == 0
  colSums(n_u*y_is_l*zeta_k) / sum(n_u*zeta_k)
}
