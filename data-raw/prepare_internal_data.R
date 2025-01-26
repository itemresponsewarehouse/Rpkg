# Load necessary files
data_index <- read.delim("data-raw/data_index.tsv",
                         header = TRUE,
                         sep = "\t",
                         stringsAsFactors = FALSE,
                         na.strings = "",  # Treat empty strings as NA
                         quote = "",
                         fill = TRUE,
                         encoding = "UTF-8")

bibtex_manual <- read.delim("data-raw/bibtex_manual.tsv",
                         header = TRUE,
                         sep = "\t",
                         stringsAsFactors = FALSE,
                         na.strings = "",  # Treat empty strings as NA
                         quote = "",
                         fill = TRUE,
                         encoding = "UTF-8")

metadata <- read.csv("data-raw/metadata.csv", stringsAsFactors = FALSE)



usethis::use_data(bibtex_manual, data_index, metadata, internal = TRUE, overwrite=TRUE)


