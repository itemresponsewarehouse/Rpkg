#' Simulate Item Difficulties Based on Uncertainty
#'
#' This function generates simulated item difficulties by drawing from normal
#' distributions centered around existing difficulty estimates with associated
#' standard errors. The result is a mixture distribution from which new difficulties
#' are sampled.
#'
#' @param difficulty_data A data frame with columns `dataset`, `difficulty`, and `SE` 
#' (standard error).
#' If `NULL`, the function uses the internal dataset \code{diff_long} provided with the package.
#' @param num_items Integer. Number of item difficulties to simulate per replication.
#' @param num_replications Integer. Number of replications to perform.
#'
#' @return A data frame containing simulated item difficulties with replication labels.
#' @examples
#' \dontrun{
#'   sim_result <- simu_diff(diff_long, num_items = 5, num_replications = 2)
#'   head(sim_result)
#' }
#' @export
simu_diff <- function(difficulty_data, num_items = 10, num_replications = 100) {
  
  if (is.null(difficulty_data)) {
    if (!exists("diff_long")) data("diff_long", envir = environment())
    difficulty_data <- diff_long
  }

  # Placeholder for generated difficulty parameters
  simulated_difficulties = data.frame()
  
  # Loop over the number of replications
  for (replication in 1:num_replications) {
    # Randomly select a dataset
    selected_dataset <- sample(difficulty_data$dataset, size = 1, replace = TRUE)
    
    # Filter item difficulties and standard errors for the selected dataset
    rows <- which(difficulty_data$dataset == selected_dataset)
    dataset_difficulties <- difficulty_data[rows, c("difficulty", "SE")]
    

    # Generate mixture distribution by adding normal noise around each difficulty
    sampled_difficulties <- unlist(
      lapply(1:nrow(dataset_difficulties), function(i) {
        rnorm(1000, mean = dataset_difficulties$difficulty[i], sd = dataset_difficulties$SE[i])
      })
    )
    
    # Estimate density distribution of the generated mixture
    density_obj <- density(sampled_difficulties, n = 10000)  # Generate density with finer resolution

    # Compute the cumulative density function (CDF)
    cdf_values <- cumsum(density_obj$y) / sum(density_obj$y)

    # Create inverse CDF function
    inverse_cdf <- approxfun(cdf_values, density_obj$x)

    # Sample from the continuous density distribution using inverse CDF
    sampled_final_difficulties <- inverse_cdf(runif(num_items))

    # Store the sampled difficulties for this replication
    simulated_difficulties <- rbind(simulated_difficulties,
                                    data.frame(replication = replication,
                                               sampled_difficulties = sampled_final_difficulties))
  
  }
  
  return(simulated_difficulties)
}

