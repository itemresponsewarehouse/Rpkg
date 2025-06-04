utils::globalVariables("diff_long")

#' Simulate Item Difficulties Based on IRW or Custom Distributions
#'
#' Generates simulated item difficulties by drawing from uncertainty distributions
#' based on real IRW data (`diff_long`) or a user-provided dataset of difficulty estimates and standard errors.
#' This method is based on Zhang et al. (2025), which models realistic difficulty distributions
#' by constructing a mixture of normals and sampling via inverse CDF.
#'
#' @param num_items Number of item difficulties to simulate per replication.
#' @param num_replications Number of independent sets of item difficulties to generate.
#' If `num_replications = 1`, a numeric vector is returned.
#' If `num_replications > 1`, a data frame with `replication` and `difficulty` columns is returned.
#' @param from_irw Logical. If `TRUE` (default), uses the built-in IRW dataset `diff_long`.
#' @param difficulty_data Optional. A data frame with columns `dataset`, `difficulty`, and `SE`.
#' Overrides `from_irw` if provided.
#'
#' @return A numeric vector (if one replication) or a data frame (if multiple replications).
#'
#' @examples
#' \dontrun{
#' # Default usage with IRW data
#' b <- irw_simu_diff(num_items = 5)
#'
#' # Multiple replications
#' reps <- irw_simu_diff(num_items = 5, num_replications = 3)
#'
#' # Custom difficulty pool
#' my_pool <- data.frame(dataset = "custom", difficulty = c(-0.2, 0.3), SE = c(0.1, 0.15))
#' b_custom <- irw_simu_diff(num_items = 5, from_irw = FALSE, difficulty_data = my_pool)
#' }
#'
#' @references
#' Zhang, L., Liu, Y., Molenaar, D., & Domingue, B. (2025, March 11). *Realistic Simulation of Item Difficulties*. https://doi.org/10.31234/osf.io/jbhxy_v1
#'
#' @importFrom stats approxfun density runif
#' @importFrom utils data
#' @export
irw_simu_diff <- function(num_items = 10,
                          num_replications = 1,
                          from_irw = TRUE,
                          difficulty_data = NULL) {
  
  # Load default data if not provided
  if (is.null(difficulty_data)) {
    if (from_irw) {
      if (!exists("diff_long")) data("diff_long", envir = environment())
      difficulty_data <- diff_long
    } else {
      stop("Please provide `difficulty_data` if `from_irw = FALSE`.")
    }
  }
  
  # Validate required columns
  required_cols <- c("dataset", "difficulty", "SE")
  if (!all(required_cols %in% names(difficulty_data))) {
    stop("`difficulty_data` must contain columns: dataset, difficulty, SE")
  }
  
  # Placeholder for generated difficulty parameters
  simulated_difficulties <- data.frame()
  
  for (replication in seq_len(num_replications)) {
    selected_dataset <- sample(unique(difficulty_data$dataset), 1)
    rows <- which(difficulty_data$dataset == selected_dataset)
    dataset_difficulties <- difficulty_data[rows, c("difficulty", "SE")]
    
    # Generate mixture distribution by adding normal noise around each difficulty
    sampled_difficulties <- unlist(
      lapply(seq_len(nrow(dataset_difficulties)), function(i) {
        rnorm(1000, mean = dataset_difficulties$difficulty[i], sd = dataset_difficulties$SE[i])
      })
    )
    
    density_obj <- density(sampled_difficulties, n = 10000) # Estimate density distribution of the generated mixture
    cdf_values <- cumsum(density_obj$y) / sum(density_obj$y) # Compute the cumulative density function (CDF)
    inverse_cdf <- approxfun(cdf_values, density_obj$x) # Create inverse CDF function
    
    # Sample from the continuous density distribution using inverse CDF
    sampled_final_difficulties <- inverse_cdf(runif(num_items))

    # Store the sampled difficulties for this replication
    simulated_difficulties <- rbind(simulated_difficulties,
                                    data.frame(replication = replication,
                                               difficulty = sampled_final_difficulties))
  }
  
  if (num_replications == 1) {
    return(simulated_difficulties$difficulty)
  } else {
    return(simulated_difficulties)
  }
}
