library(irw)

swmd_mokken <- irw_fetch("swmd_mokken")

# irw_fetch() returns arrow-backed columns. Saving those as-is embeds a
# reference to the arrow namespace in the .rda, which R CMD check reports
# under "checking namespace references in data files" -- a WARNING, since
# arrow is not a dependency of this package. Materializing each column into
# a plain base vector first keeps the data identical and the file clean.
swmd_mokken <- tibble::as_tibble(
  lapply(swmd_mokken, function(col) unlist(as.list(col), use.names = FALSE))
)

usethis::use_data(swmd_mokken, overwrite = TRUE)
