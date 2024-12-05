# ## Code to compute metadata from the Redivis database.
# This script is used to preprocess and generate the metadata_summary.csv file
# in the data/ folder. It should be run manually during package development.

library(irwpkg)

compute_metadata <- function(output_file = "data/metadata.csv") {
  # Initialize the datasource
  ds <- initialize_datasource()
  tables <- ds$list_tables()
  
  # Initialize metadata summary with fixed column structure
  metadata <- data.frame(
    table_name = character(0),
    numRows = integer(0),
    columns = I(list()),
    stringsAsFactors = FALSE
  )
  
  for (table in tables) {
    table_metadata <- table$properties
    columns <- tryCatch(
      sapply(table$list_variables(), function(var) var$name),
      error = function(e) {
        warning(paste("Failed to retrieve variables for table:", table_metadata$name))
        character(0)
      }
    )
    
    # Append the new row to metadata, ensuring consistent structure
    metadata <- rbind(metadata, data.frame(
      table_name = table_metadata$name,
      numRows = table_metadata$numRows,
      columns = I(list(columns)),
      stringsAsFactors = FALSE
    ))
  }
  
  # Ensure the "data" directory exists
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  # Save the metadata summary to a CSV file
  write.csv(metadata, file = output_file, row.names = FALSE)
  message("Metadata successfully saved to: ", output_file)
}

# Run the function manually
compute_metadata()

# Tests
# compute_metadata("data/testing.csv")