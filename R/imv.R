# Internal: solve for the coin weight whose entropy matches a given
# geometric-mean likelihood. See Domingue et al. (2021).
.irw_imv_coin <- function(a) {
  f <- function(p, a) abs(p * log(p) + (1 - p) * log(1 - p) - log(a))
  stats::nlminb(0.5, f, lower = 1e-3, upper = 1 - 1e-3, a = a)$par
}

# Internal: geometric mean likelihood of predictions `p` for outcomes `resp`.
.irw_imv_gml <- function(resp, p) {
  exp(sum(log(p) * resp + log(1 - p) * (1 - resp)) / length(resp))
}

#' InterModel Vigorish (IMV)
#'
#' @description
#' Compare two sets of predicted probabilities for the same binary outcomes.
#' The IMV expresses how much better the second model predicts than the first,
#' on the scale of a weighted coin: each model's geometric mean likelihood is
#' converted to the weight of a coin with the same entropy, and the IMV is the
#' proportional gain in that weight.
#'
#' @details
#' An IMV of 0 means the two models predict equally well; 0.05 means model 2
#' is equivalent to a coin 5\% more predictable than model 1's. The statistic
#' is not symmetric: \code{irw_imv(data, "p1", "p2")} is the gain from moving
#' from \code{p1} to \code{p2}, and swapping the arguments does not simply
#' flip the sign.
#'
#' Predicted probabilities are clamped to \code{[eps, 1 - eps]} before the
#' likelihoods are computed, so predictions of exactly 0 or 1 do not produce
#' an infinite log-likelihood.
#'
#' Out-of-sample predictions are the intended use: comparing in-sample
#' predictions will favour the more flexible model by construction. The
#' "Comparing Models Out of Sample with the IMV" article works through a
#' cross-validated comparison of a Rasch model against a 2PL.
#'
#' @param data A data frame holding the outcomes and both sets of predicted
#'   probabilities, or a numeric vector of binary outcomes. When \code{data} is
#'   a vector, \code{p1} and \code{p2} must be numeric vectors of the same
#'   length.
#' @param p1 Predictions from the baseline model: a column name in \code{data},
#'   or a numeric vector when \code{data} is a vector of outcomes.
#' @param p2 Predictions from the comparison model, in the same form as
#'   \code{p1}.
#' @param resp The binary outcome. Ignored when \code{data} is a vector;
#'   otherwise a column name in \code{data}, defaulting to \code{"resp"}.
#' @param eps Clamping tolerance for predicted probabilities. Default 1e-6.
#'
#' @return A single numeric value: the proportional gain in coin weight from
#'   \code{p1} to \code{p2}.
#'
#' @references
#' Domingue, B. W., Rahal, C., Faul, J., Freese, J., Kanopka, K.,
#' Rigos, A., Stenhaug, B., & Tripathi, A. (2021). InterModel Vigorish (IMV):
#' A novel approach for quantifying predictive accuracy with binary outcomes.
#' \doi{10.31235/osf.io/8sgz5}
#'
#' @seealso \code{\link{irw_predict}} for producing \code{p1} and \code{p2}
#'   from fitted \pkg{mirt} models.
#'
#' @examples
#' set.seed(1)
#' truth <- rbinom(500, 1, 0.7)
#' # A model that knows the base rate beats one that guesses at random.
#' irw_imv(truth, p1 = rep(0.5, 500), p2 = rep(0.7, 500))
#'
#' @importFrom stats nlminb
#' @export
irw_imv <- function(data, p1, p2, resp = "resp", eps = 1e-6) {
  if (is.data.frame(data)) {
    for (nm in c(resp, p1, p2)) {
      if (!(is.character(nm) && length(nm) == 1L)) {
        stop("When `data` is a data frame, `resp`, `p1` and `p2` must be column names.")
      }
      if (!nm %in% names(data)) {
        stop("Column not found in `data`: ", nm)
      }
    }
    y <- data[[resp]]
    v1 <- data[[p1]]
    v2 <- data[[p2]]
  } else {
    y <- data
    v1 <- p1
    v2 <- p2
  }

  if (!is.numeric(eps) || length(eps) != 1L || is.na(eps) || eps <= 0 || eps >= 0.5) {
    stop("`eps` must be a single number in (0, 0.5).")
  }
  for (v in list(y, v1, v2)) {
    if (!is.numeric(v)) stop("Outcomes and predictions must be numeric.")
  }
  if (length(y) != length(v1) || length(y) != length(v2)) {
    stop(
      "Outcomes and predictions must have the same length: got ",
      length(y), ", ", length(v1), ", ", length(v2), "."
    )
  }
  if (length(y) == 0L) {
    stop("No observations to compare.")
  }
  if (anyNA(y) || anyNA(v1) || anyNA(v2)) {
    stop("Outcomes and predictions must not contain NA. Drop incomplete rows first.")
  }
  if (!all(y %in% c(0, 1))) {
    stop("The IMV is defined for binary outcomes; `resp` must be 0/1.")
  }
  if (any(v1 < 0 | v1 > 1) || any(v2 < 0 | v2 > 1)) {
    stop("Predictions must be probabilities in [0, 1].")
  }

  clamp <- function(p) pmin(pmax(p, eps), 1 - eps)
  v1 <- clamp(v1)
  v2 <- clamp(v2)

  c1 <- .irw_imv_coin(.irw_imv_gml(y, v1))
  c2 <- .irw_imv_coin(.irw_imv_gml(y, v2))

  (c2 - c1) / c1
}
