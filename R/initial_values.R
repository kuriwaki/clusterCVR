#' Initialize mu, pi, and zeta by equal assignment
#'
#' @param data from the object
#' @param user_K number of clusters to presume
#' @param seed
#'
#'
#' @return \describe{
#' \item{pi}{A N by K matrix. Each row sums to 1.}
#' \item{mu}{A K by D by (L + 1) array. Each `mu[k, j, ]` sums to 1}
#' }
#'
#' @importFrom purrr flatten_dbl map map_dbl
#' @importFrom stats rmultinom
#'
#' @export
#'
init_equal <- function(data, user_K,  seed) {
  # initialize pi {K x 1}
  init_pi_i = rep(1/user_K, user_K)
  init_pi = matrix(rep(init_pi_i, times = data$U),
                      byrow = TRUE, nrow = data$U)

  # initialize mu {K x D x L}
  set.seed(seed)
  init_Z_table = rmultinom(data$N, size = 1, prob = init_pi_i)
  init_Z = map_dbl(1:data$U, ~which(init_Z_table[, .x] == 1))

  # data$L
  mu <- init_mu <- array(NA, dim = c(user_K, data$D, data$L + 1))
  for (l in 0:data$L) {
    mu_vector <- flatten_dbl(map(1:user_K, ~colMeans(data$y[init_Z == .x, ] == l)))
    init_mu[, , l + 1] = matrix(mu_vector, nrow = user_K, byrow = TRUE)
  }
  return(list(init_pi = init_pi, init_mu = init_mu))
}

#' Initialize mu, pi, and zeta by k-means guess
#'
#'
#' @param data from the object
#' @param user_K number of clusters to presume
#' @param seed seed for initial randomization
#'
#' @importFrom dplyr recode
#' @importFrom tidyr uncount
#' @importFrom stringr str_which
#' @importFrom stats kmeans
#' @importFrom rlang `!!!`
#' @importFrom purrr flatten_dbl
#'
#' @rdname init_equal
#' @export
init_kmeans <- function(data, user_K, seed) {

  full_mat <- tidyr::uncount(as.data.frame(data$uy), weights = data$n_u)

  # run k-means on binarized data
  init_binary <- matrix(as.numeric(full_mat == data$L), # count the max category as success
                        nrow = data$N,
                        ncol = data$D,
                        byrow = FALSE)

  set.seed(seed)
  k_init <- kmeans(init_binary, centers = user_K)
  # pre-sort so largest cluster tends to be at front
  c_order <- order(table(k_init$cluster), decreasing = TRUE)

  switcher <- 1:user_K
  names(switcher) <- c_order

  k_cl <- dplyr::recode(k_init$cluster, !!!switcher)

  # initialize pi {K x 1}
  init_pi_i = prop.table(table(k_init$cluster))[c_order]
  init_pi = matrix(rep(init_pi_i, times = data$N),
                      byrow = TRUE, nrow = data$N)

  # initialize mu {K x D x L}x
  init_Z = k_cl

  # data$L
  mu <- init_mu <- array(NA, dim = c(user_K, data$D, data$L + 1))
  for (l in 0:data$L) {
    mu_vector <- flatten_dbl(map(1:user_K, ~colMeans(full_mat[init_Z == .x, ] == l)))
    init_mu[, , l + 1] = matrix(mu_vector, nrow = user_K, byrow = TRUE)
  }

  # correct so that no cell is exactly zero
  for (j in 1:data$D) {
    for (k in 1:user_K) {
      if (any(init_mu[k, j, ] < 1e-5))
        init_mu[k, j, ] = (init_mu[k, j, ]  + 0.05) /  sum((init_mu[k, j, ]  + 0.05))
    }
  }

  # undo uncount
  index_dup <- str_which(rownames(full_mat), "\\.[:digit:]+$")
  if (length(index_dup) > 0)
    init_pi <- init_pi[-c(index_dup), ]

  return(list(init_pi = init_pi, init_mu = init_mu))
}
