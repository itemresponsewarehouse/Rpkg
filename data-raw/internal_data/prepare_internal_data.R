# This script loads raw .tsv files (data_index and bibtex_manual) and processed metadata (metadata.csv)
# from specified directories and saves them as internal package data.

# Define file paths
raw_data_dir <- file.path("data-raw", "internal_data", "raw_data")

# Define specific file paths
data_index_path <- file.path(raw_data_dir, "data_index.tsv")
bibtex_manual_path <- file.path(raw_data_dir, "bibtex_manual.tsv")


# Load raw data
data_index <- read.delim(
  data_index_path,
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE,
  na.strings = "",
  quote = "",
  fill = TRUE,
  encoding = "UTF-8"
)

bibtex_manual <- read.delim(
  bibtex_manual_path,
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE,
  na.strings = "",
  quote = "",
  fill = TRUE,
  encoding = "UTF-8"
)


# Save data as internal objects
usethis::use_data(bibtex_manual,
                  data_index,
                  internal = TRUE,
                  overwrite = TRUE)