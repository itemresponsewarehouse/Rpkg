#' Simulate IRW-compliant dichotomous item response data
#'
#' Simulates item response data from 1PL, 2PL, or 3PL logistic IRT models.
#' Returns a long-format data frame with `id`, `item`, and `resp` columns.
#'
#' @param n_id Number of respondents. Ignored if `theta` is provided. Default is 1000.
#' @param n_item Number of items. Default is 20.
#' @param model Character string: one of `"1PL"`, `"2PL"`, or `"3PL"`. Default is `"1PL"`.
#' @param a Optional vector of item discriminations. If `NULL`, sampled from lognormal(0.2, 0.2) (ignored for 1PL).
#' @param b Optional vector of item difficulties. If `NULL`, sampled from N(0, 1).
#' @param g Optional vector of guessing parameters. Required for 3PL; if `NULL`, sampled from Beta(5, 17).
#' @param theta Optional vector of person abilities. If provided, overrides `n_id`, `theta_mean`, and `theta_sd`.
#' @param theta_mean Mean of theta distribution (if `theta` not supplied). Default is 0.
#' @param theta_sd Standard deviation of theta distribution. Default is 1.
#' @param seed Optional integer for reproducibility.
#' @param return_params Logical. If `TRUE`, returns a list with data and parameters. Default is `FALSE`.
#'
#' @return A data frame with columns `id`, `item`, `resp`, or a list with additional parameters if `return_params = TRUE`.
#'
#' @examples
#' \dontrun{
#' # 1PL (default)
#' dat <- irw_simdata(n_item = 5)
#'
#' # 3PL with latent traits drawn from N(-0.5, 1)
#' sim <- irw_simdata(n_item = 5, model = "3PL", theta_mean = -0.5, return_params = TRUE)
#' head(sim$data)
#' }
#'
#' @importFrom stats rnorm rlnorm rbeta rbinom
#' @export
irw_simdata <- function(n_id = 1000,
                        n_item = 20,
                        model = "1PL",
                        a = NULL,
                        b = NULL,
                        g = NULL,
                        theta = NULL,
                        theta_mean = 0,
                        theta_sd = 1,
                        seed = NULL,
                        return_params = FALSE) {
  if (!model %in% c("1PL", "2PL", "3PL")) {
    stop("model must be one of '1PL', '2PL', or '3PL'", call. = FALSE)
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  if (!is.null(theta)) {
    theta <- as.numeric(theta)
    n_id <- length(theta)
  } else {
    theta <- rnorm(n_id, mean = theta_mean, sd = theta_sd)
  }
  
  if (is.null(a)) {
    a <- switch(model,
                "1PL" = rep(1, n_item),
                "2PL" = rlnorm(n_item, 0.2, 0.2),
                "3PL" = rlnorm(n_item, 0.2, 0.2))
  }
  
  if (is.null(b)) {
    b <- rnorm(n_item, 0, 1)
  }
  
  if (model == "3PL" && is.null(g)) {
    g <- rbeta(n_item, 5, 17)
  }
  
  prob_correct <- function(th, a, b, g = 0) {
    p <- 1 / (1 + exp(-a * (th - b)))
    g + (1 - g) * p
  }
  
  response_matrix <- matrix(NA, nrow = n_id, ncol = n_item)
  for (i in seq_len(n_item)) {
    p <- prob_correct(theta, a[i], b[i], if (model == "3PL") g[i] else 0)
    response_matrix[, i] <- rbinom(n_id, 1, p)
  }
  
  df_long <- data.frame(
    id = rep(seq_len(n_id), times = n_item),
    item = rep(seq_len(n_item), each = n_id),
    resp = as.vector(response_matrix)
  )
  
  if (return_params) {
    return(list(
      data = df_long,
      theta = theta,
      a = a,
      b = b,
      g = if (model == "3PL") g else NULL
    ))
  } else {
    return(df_long)
  }
}



# gen.pair<-function(N=100,N_pairs=10000,nu= 0) {
#   pr.pair<-function(th1,th2,nu=nu) { ##based on davidson model, https://link.springer.com/article/10.3758/s13428-021-01714-2
#     K<-(exp(th1)+exp(th2)+exp(nu+(th1+th2)/2))
#     ##pr tie, ##see eqn 8
#     pr.tie<-exp(nu+(th1+th2)/2)/K
#     ##see eqn 7
#     pr.12<-exp(th1)/K
#     pr.21<-exp(th2)/K
#     x<-rmultinom(1,1,c(pr.tie,pr.12,pr.21))[,1]
#     ii<-which(x==1)
#     ii-1 ##0-draw, 1 1>2, 2 2>1
#   }
#   th<-rnorm(N)
#   pairs<-list()
#   for (i in 1:N_pairs) {
#     ii<-sample(1:length(th),2)
#     pairs[[i]]<-c(ii,pr.pair(th[ii[1]],th[ii[2]],nu=nu))
#   }
#   pairs<-do.call("rbind",pairs)
#   pairs<-data.frame(pairs)
#   names(pairs)<-c("agent_a","agent_b","winner")
#   tmp<-pairs$winner
#   pairs$winner<-ifelse(tmp==0,'draw','agent_a')
#   pairs$winner<-ifelse(tmp==2,'agent_b',pairs$winner)
#   pairs
# }
