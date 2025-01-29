## ----setup--------------------------------------------------------------------
#| eval: false
# library(irwpkg)
# library(mirt)


## -----------------------------------------------------------------------------
#| eval: false
# datasets <- list_available_datasets()
# dim(datasets)
# head(datasets)
# 


## -----------------------------------------------------------------------------
#| eval: false
# db_info <- get_database_metadata()


## -----------------------------------------------------------------------------
#| eval: false
# table_info <- get_table_metadata("lessR_Mach4")


## -----------------------------------------------------------------------------
#| eval: false
# show_overall_statistics()


## -----------------------------------------------------------------------------
#| eval: false
# visualize_metadata_distributions()


## -----------------------------------------------------------------------------
#| eval: false
# visualize_metadata_distributions(ranges = list(
#   id_count = c(0, 10000),
#   resp_count = c(0, 100000),
#   item_count = c(0, 150),
#   density = c(0, 2)
# ))


## -----------------------------------------------------------------------------
#| eval: false
# matching_tables <- filter_tables(
#   id_count = c(100, 1000),
#   density = c(0.1, 0.5))
# print(matching_tables)


## -----------------------------------------------------------------------------
#| eval: false
# matching_tables <- filter_tables(has_rater = TRUE)
# 
# print(matching_tables)


## -----------------------------------------------------------------------------
#| eval: false
# matching_tables <- filter_tables(
#   id_count = c(100, 10000),
#   has_rater = TRUE
# )
# print(matching_tables)


## -----------------------------------------------------------------------------
#| eval: false
# swmd_mokken <- fetch_data(name = "swmd_mokken")
# str(swmd_mokken)
# 


## -----------------------------------------------------------------------------
#| eval: false
# datasets <- fetch_data(c("fractals_rating", "spelling2pronounce_edwards2023"))
# print(names(datasets))
# str(datasets$fractals_rating)
# 


## -----------------------------------------------------------------------------
#| eval: false
# matching_tables <- filter_tables(has_rater = TRUE)
# datasets <- fetch_data(matching_tables)
# print(names(datasets))


## -----------------------------------------------------------------------------
#| eval: false
# download_data("swmd_mokken", path = "mydata.csv", overwrite=TRUE)


## -----------------------------------------------------------------------------
#| eval: false
# test_vec <- c("InTeREsTiNG.VAR", "another.VAR", "this.$&^!_VAR")
# irw_rename(test_vec)


## -----------------------------------------------------------------------------
#| eval: false
# df <- fetch_data(name = "lessR_Mach4")
# rdf = reformat(df)
# mirtmod = mirt::mirt(rdf)
# plot(mirtmod, type="infotrace")


## -----------------------------------------------------------------------------
#| eval: false
# summary(mirtmod)


## -----------------------------------------------------------------------------
#| eval: false
# df <- fetch_data(name = "NAMPRB_Siwiak_2024_AOT")
# rdf = reformat(df, package = "mokken")
# mokmod = mokken::check.monotonicity(rdf)
# plot(mokmod, ask=FALSE)


## -----------------------------------------------------------------------------
#| eval: false
# summary(mokmod)


## -----------------------------------------------------------------------------
#| eval: false
# df <- fetch_data(name = "dd_rotation")
# rdf = reformat(df, package = "psych", resp = "rt")
# famod = psych::omega(rdf)
# plot(famod)
# summary(famod)


## -----------------------------------------------------------------------------
#| eval: false
# df <- fetch_data(name = "COACH_Chen_2022_ADL")
# rdf = reformat(df,package = "lavaan",item=c("item","wave"))
# lavmod = lavaan::efa(rdf)
# summary(lavmod)

