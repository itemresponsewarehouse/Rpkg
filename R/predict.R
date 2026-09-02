# Internal: apply the same item naming convention as irw_long2resp().
.irw_prefix_items <- function(x) {
  ifelse(grepl("^item_", x), x, paste0("item_", x))
}

#' Predicted Response Probabilities from a Fitted mirt Model
#'
#' @description
#' Attach per-response predicted probabilities from a fitted unidimensional,
#' dichotomous \pkg{mirt} model to long-format IRW data. This is the piece
#' needed to compare models out of sample with \code{\link{irw_imv}}.
#'
#' @details
#' Person abilities are taken from the fit itself with
#' \code{mirt::fscores()}, so row \emph{i} of the scored matrix is person
#' \code{resp$id[i]}. Passing the wide frame that produced the fit is what
#' lets the function recover that mapping; supplying ids in a different order
#' than the rows given to \code{mirt::mirt()} would silently mispair people
#' with abilities.
#'
#' Probabilities come from \code{mirt::probtrace()}, so any dichotomous
#' \pkg{mirt} item type (Rasch, 2PL, 3PL, 4PL, \dots) is handled without
#' assuming a particular parameterisation.
#'
#' \code{newdata} may contain any \code{id}/\code{item} pairs, including ones
#' held out of the fit, as long as each person appears in \code{resp} and each
#' item was estimated. That is the cross-validation case: fit on part of the
#' data, predict the rest.
#'
#' Requires the \pkg{mirt} package.
#'
#' @param model A fitted unidimensional \pkg{mirt} model with dichotomous
#'   items.
#' @param resp The wide response frame used to fit \code{model}, including its
#'   \code{id} column, as returned by \code{\link{irw_long2resp}}. The item
#'   columns (everything but \code{id}) are what was passed to
#'   \code{mirt::mirt()}.
#' @param newdata (Optional) A long-format data frame with columns \code{id}
#'   and \code{item} giving the responses to predict. Defaults to every
#'   \code{id}/\code{item} pair in \code{resp}.
#' @param ability Scoring method passed to \code{mirt::fscores()}. Default
#'   \code{"EAP"}.
#'
#' @return \code{newdata} with an added numeric column \code{p}, the predicted
#'   probability of a response of 1.
#'
#' @seealso \code{\link{irw_imv}}, \code{\link{irw_long2resp}}
#'
#' @examples
#' \dontrun{
#' df <- irw_simdata(n_id = 200, n_item = 10, model = "2PL", seed = 1)
#' wide <- irw_long2resp(df)
#' fit <- mirt::mirt(wide[setdiff(names(wide), "id")], 1, "Rasch")
#' preds <- irw_predict(fit, wide)
#' }
#'
#' @export
irw_predict <- function(model, resp, newdata = NULL, ability = "EAP") {
  if (!requireNamespace("mirt", quietly = TRUE)) {
    stop("`irw_predict()` requires the 'mirt' package. Install it with install.packages('mirt').")
  }
  if (!is.data.frame(resp)) {
    stop("`resp` must be a data frame with an `id` column, as returned by irw_long2resp().")
  }
  if (!"id" %in% names(resp)) {
    stop(
      "No `id` column found in `resp`.\n",
      "Pass the wide frame from irw_long2resp() with its ids still attached, ",
      "not the item-only matrix given to mirt::mirt()."
    )
  }
  if (anyDuplicated(resp$id)) {
    stop("`resp` has duplicate ids; it should hold one row per person.")
  }

  if (mirt::extract.mirt(model, "nfact") != 1L) {
    stop("`irw_predict()` supports unidimensional models only.")
  }
  item_names <- mirt::extract.mirt(model, "itemnames")
  if (!identical(setdiff(names(resp), "id"), item_names)) {
    stop(
      "The item columns in `resp` do not match the items in `model`.\n",
      "`resp` should be the same wide frame the model was fit on, with `id` still attached."
    )
  }

  theta <- mirt::fscores(model, method = ability)
  if (nrow(theta) != nrow(resp)) {
    stop(
      "`model` was fit on ", nrow(theta), " persons but `resp` has ", nrow(resp),
      " rows. Pass the frame the model was actually fit on."
    )
  }
  theta_by_id <- stats::setNames(theta[, 1], as.character(resp$id))

  if (is.null(newdata)) {
    newdata <- irw_resp2long(resp)
    newdata <- newdata[!is.na(newdata$resp), , drop = FALSE]
  }
  if (!is.data.frame(newdata) || !all(c("id", "item") %in% names(newdata))) {
    stop("`newdata` must be a long-format data frame with `id` and `item` columns.")
  }
  if (nrow(newdata) == 0L) {
    stop("`newdata` has no rows to predict.")
  }

  # irw_long2resp() prefixes item names; long data straight from the warehouse
  # may not be prefixed yet, so match either spelling.
  items <- as.character(newdata$item)
  if (!all(items %in% item_names)) {
    prefixed <- .irw_prefix_items(items)
    if (all(prefixed %in% item_names)) {
      items <- prefixed
    } else {
      missing_items <- unique(items[!items %in% item_names])
      stop(
        "Items in `newdata` were not estimated by `model`: ",
        paste(missing_items[seq_len(min(5L, length(missing_items)))], collapse = ", "),
        if (length(missing_items) > 5L) paste0(" (and ", length(missing_items) - 5L, " more)") else ""
      )
    }
  }

  ids <- as.character(newdata$id)
  missing_ids <- unique(ids[!ids %in% names(theta_by_id)])
  if (length(missing_ids) > 0L) {
    stop(
      "Persons in `newdata` are absent from `resp`, so they have no ability estimate: ",
      paste(missing_ids[seq_len(min(5L, length(missing_ids)))], collapse = ", "),
      if (length(missing_ids) > 5L) paste0(" (and ", length(missing_ids) - 5L, " more)") else ""
    )
  }

  th <- theta_by_id[ids]
  p <- rep(NA_real_, nrow(newdata))
  for (it in unique(items)) {
    rows <- which(items == it)
    trace <- mirt::probtrace(
      mirt::extract.item(model, it),
      matrix(th[rows], ncol = 1L)
    )
    if (ncol(trace) != 2L) {
      stop(
        "Item '", it, "' has ", ncol(trace), " response categories. ",
        "`irw_predict()` supports dichotomous items only."
      )
    }
    p[rows] <- trace[, 2L]
  }

  newdata$p <- p
  newdata
}
