#' Simulate Item Difficulties from IRW or Custom Pools
#'
#' This function generates simulated item difficulties by drawing from normal
#' distributions centered around existing difficulty estimates and their associated
#' standard errors. The result is a mixture distribution, from which new difficulties
#' are sampled using inverse CDF sampling.
#'
#' By default, the function uses `diff_long`, a built-in dataset included in the `irw` package.
#' This dataset contains item difficulty estimates and standard errors from a curated
#' subset of IRW datasets. You can:
#'
#' - Use the full IRW difficulty pool (`diff_long`)
#' - Filter to specific IRW datasets via `irw_names`
#' - Provide your own difficulty pool via `difficulty_pool`
#'
#' This method is based on Zhang et al. (2025), which constructs realistic
#' empirical distributions by accounting for uncertainty around item difficulty estimates.
#'
#' @param num_items Number of item difficulties to simulate per replication.
#' @param num_replications Number of replications to perform.
#'   If 1, returns a numeric vector. If >1, returns a data frame.
#' @param irw_names Optional character vector of IRW dataset names to filter from `diff_long`.
#' @param difficulty_pool Optional custom data frame with columns `dataset`, `difficulty`, and `SE`.
#'   If provided, overrides the default IRW difficulty pool (`diff_long`).
#'
#' @return A numeric vector of difficulties (if `num_replications = 1`), or a data frame
#' with `replication` and `difficulty` columns (if `num_replications > 1`).
#'
#' @examples
#' \dontrun{
#' # Use all IRW data (default)
#' irw_simu_diff(num_items = 5)
#'
#' # Filter to specific IRW datasets
#' irw_simu_diff(num_items = 5, irw_names = c("psychtools_epi", "psychtools_blot"))
#'
#' # Use a custom difficulty pool
#' my_pool <- data.frame(dataset = "x",
#'                       difficulty = c(-0.2, 0.1),
#'                       SE = c(0.1, 0.2))
#' irw_simu_diff(num_items = 5, difficulty_pool = my_pool)
#'
#' # Explore built-in IRW difficulty pool
#' head(diff_long)
#' unique(diff_long$dataset)
#' }
#'
#' @references
#' Zhang, L., Liu, Y., Molenaar, D., & Domingue, B. (2025). *Realistic Simulation of Item Difficulties*.
#' https://doi.org/10.31234/osf.io/jbhxy_v1
#'
#' @importFrom stats approxfun density runif rnorm
#' @importFrom utils data
#' @export
irw_simu_diff <- function(num_items = 10,
                          num_replications = 1,
                          irw_names = NULL,
                          difficulty_pool = NULL) {
  
  # Load default IRW difficulty pool
  if (is.null(difficulty_pool)) {
    if (!exists("diff_long", envir = environment(), inherits = TRUE)) {
      data("diff_long", envir = environment())
    }
    pool <- get("diff_long", envir = environment())
    
    # Subset to specified IRW datasets if provided
    if (!is.null(irw_names)) {
      pool <- pool[pool[["dataset"]] %in% irw_names, , drop = FALSE]
      if (nrow(pool) == 0) stop("No matching datasets found in IRW difficulty pool.")
    }
  } else {
    pool <- difficulty_pool
  }
  
  # Validate required columns
  required_cols <- c("dataset", "difficulty", "SE")
  if (!all(required_cols %in% names(pool))) {
    stop("Difficulty pool must contain columns: dataset, difficulty, SE")
  }
  
  # Placeholder for results
  simulated_difficulties <- data.frame()
  
  for (replication in seq_len(num_replications)) {
    dataset_choices <- unique(pool[["dataset"]])
    selected_dataset <- sample(dataset_choices, 1)
    rows <- pool[["dataset"]] == selected_dataset
    dataset_difficulties <- pool[rows, c("difficulty", "SE"), drop = FALSE]
    
    # Generate mixture distribution by adding normal noise around each difficulty
    sampled_difficulties <- unlist(
      lapply(seq_len(nrow(dataset_difficulties)), function(i) {
        rnorm(1000,
              mean = dataset_difficulties[["difficulty"]][i],
              sd   = dataset_difficulties[["SE"]][i])
      })
    )
    
    # Estimate density distribution of the generated mixture
    density_obj <- density(sampled_difficulties, n = 10000)
    
    # Compute the cumulative density function (CDF)
    cdf_values <- cumsum(density_obj$y) / sum(density_obj$y)
    
    # Create inverse CDF function
    inverse_cdf <- approxfun(cdf_values, density_obj$x)
    
    # Sample from the continuous density distribution using inverse CDF
    sampled_final_difficulties <- inverse_cdf(runif(num_items))
    
    # Store results
    simulated_difficulties <- rbind(simulated_difficulties,
                                    data.frame(replication = replication,
                                               difficulty = sampled_final_difficulties))
  }
  
  # Return appropriate format
  if (num_replications == 1) {
    return(simulated_difficulties$difficulty)
  } else {
    return(simulated_difficulties)
  }
}
