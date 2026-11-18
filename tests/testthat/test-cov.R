# The covariance family (source = "cov"). Published matrices are not reachable
# yet (irw_cov is unreleased); what ships now is computing a core table's item
# matrix on fetch, and turning the layout back into a matrix. Standard:
# ben-domingue/irw data/covariance/README.md.

long_frame <- function(n = 60, seed = 1) {
  set.seed(seed)
  f <- stats::rnorm(n)
  resp <- sapply(1:4, function(j) as.numeric(cut(f + stats::rnorm(n), c(-Inf, -0.5, 0.5, Inf))))
  data.frame(id = rep(seq_len(n), 4), item = rep(paste0("q", 1:4), each = n), resp = c(resp))
}

wide_of <- function(df) {
  w <- stats::reshape(df, idvar = "id", timevar = "item", direction = "wide")
  m <- as.matrix(w[, -1]); colnames(m) <- sub("^resp\\.", "", colnames(m)); m
}

test_that("pearson matches stats::cor and the layout is the lower triangle", {
  df <- long_frame()
  out <- irw_cov_compute(df)
  expect_named(out, c("stat", "var1", "var2", "value", "n"))
  expect_equal(nrow(out), 4 * 5 / 2)
  expect_true(all(out$stat == "cor"))
  m <- irw_cov2matrix(out)
  expect_equal(m$matrix, stats::cor(wide_of(df))[rownames(m$matrix), colnames(m$matrix)])
  expect_identical(m$n, 60L)
  expect_true(m$computed)
  expect_identical(m$method, "pearson")
})

test_that("method, type and use are passed through", {
  df <- long_frame()
  w <- wide_of(df)
  sp <- irw_cov2matrix(irw_cov_compute(df, method = "spearman", type = "cov"))
  expect_identical(sp$type, "cov")
  expect_equal(unname(sp$matrix), unname(stats::cov(w, method = "spearman")[colnames(sp$matrix), colnames(sp$matrix)]))
  df$resp[c(1, 70)] <- NA
  pw <- irw_cov2matrix(irw_cov_compute(df))
  cc <- irw_cov2matrix(irw_cov_compute(df, use = "complete"))
  expect_true(length(unique(c(pw$n_pairwise))) > 1)
  expect_identical(unique(c(cc$n_pairwise)), 58L)
  expect_error(irw_cov_compute(df, method = "polychoric", type = "cov"), "not defined")
})

test_that("a user-defined method gets the wide matrix and is checked", {
  df <- long_frame()
  seen <- NULL
  shrunk <- function(x) { seen <<- dim(x); 0.5 * stats::cor(x) + 0.5 * diag(ncol(x)) }
  m <- irw_cov2matrix(irw_cov_compute(df, method = shrunk))
  expect_identical(seen, c(60L, 4L))
  expect_identical(m$method, "user-defined")
  expect_equal(unname(diag(m$matrix)), rep(1, 4))
  expect_error(irw_cov_compute(df, method = function(x) diag(3)), "4 x 4")
  expect_error(irw_cov_compute(df, method = function(x) { m <- diag(4); m[1, 2] <- 0.3; m }), "not symmetric")
})

test_that("polychoric and tetrachoric go through psych", {
  skip_if_not_installed("psych")
  df <- long_frame()
  pc <- irw_cov2matrix(irw_cov_compute(df, method = "polychoric"))
  expect_identical(pc$method, "polychoric")
  expect_true(all(abs(pc$matrix) <= 1))
  expect_error(irw_cov_compute(df, method = "tetrachoric"), "dichotomous")
  df$resp <- as.numeric(df$resp > 1)
  expect_identical(irw_cov2matrix(irw_cov_compute(df, method = "tetrachoric"))$method, "tetrachoric")
})

test_that("repeated measures, too many items, flat items and non-core tables are handled", {
  df <- long_frame()
  expect_error(irw_cov_compute(rbind(df, df[1, ])), "more than once")
  expect_error(irw_cov_compute(df, max_items = 3), "max_items")
  flat <- rbind(df, data.frame(id = 1:60, item = "q5", resp = 2))
  expect_warning(out <- irw_cov_compute(flat), "no variation")
  expect_false("q5" %in% out$var1)
  expect_error(irw_cov_compute(data.frame(id = 1, task = 1, profile = 1)), "core table")
})

test_that("irw_cov2matrix reads published tables, groups and moments", {
  pub <- data.frame(
    stat = c("cor", "cor", "cor", "sd", "sd", "cor", "cor", "cor"),
    var1 = c("a", "b", "b", "a", "b", "a", "b", "b"),
    var2 = c("a", "a", "b", NA, NA, "a", "a", "b"),
    value = c(1, 0.4, 1, 2, 3, 1, 0.6, 1),
    n = c(rep(100, 5), rep(80, 3)),
    group = c(rep("boys", 5), rep("girls", 3)))
  expect_error(irw_cov2matrix(pub), "choose one")
  b <- irw_cov2matrix(pub, group = "boys")
  expect_equal(b$matrix["a", "b"], 0.4)
  expect_equal(b$sd, c(a = 2, b = 3))
  expect_false(b$computed)
  expect_identical(irw_cov2matrix(pub, group = "girls")$n, 80L)
  expect_error(irw_cov2matrix(data.frame(x = 1)), "missing")
})

test_that("irw_fetch(source = 'cov') fetches id/item/resp from core and computes", {
  df <- long_frame()
  asked <- NULL
  local_mocked_bindings(
    fetch_single_data = function(table_id, source, dedup, max_rows, columns, ...) {
      asked <<- list(table = table_id, source = source, columns = columns); df },
    .irw_source_note = function(...) invisible(NULL),
    .irw_cov_published_names = function() character(0))
  out <- irw_fetch("toy_table", source = "cov", cov_args = list(method = "spearman"))
  expect_identical(asked$source, "core")
  expect_identical(asked$columns, c("id", "item", "resp"))
  expect_identical(attr(out, "irw_method"), "spearman")
  expect_identical(attr(out, "irw_table"), "toy_table")
  expect_error(irw_fetch("toy_table", source = "cov", max_rows = 10), "biased")
  expect_error(irw_fetch("toy_table", source = "cov", resp = TRUE), "two different things")
})

test_that("cov is a registered source: irw_cov, one dataset", {
  expect_identical(irw:::.irw_resolve_source("cov"), "cov")
  expect_identical(irw:::.irw_single_datasource_cache_key("cov"), "cov_datasource")
  specs <- irw:::.irw_datasource_specs$cov
  expect_length(specs, 1L)
  expect_identical(specs[[1]]$dataset, "irw_cov:4e2q")
  expect_identical(irw:::.irw_source_noun("cov"), "covariance dataset")
})

test_that("a published matrix is returned as published, not computed", {
  pub <- data.frame(stat = "cor", var1 = c("a", "b", "b"), var2 = c("a", "a", "b"),
                    value = c(1, 0.3, 1), n = 145L)
  asked <- NULL
  local_mocked_bindings(
    .irw_cov_published_names = function() "holzinger_1939_grantwhite9",
    fetch_single_data = function(table_id, source, ...) { asked <<- source; pub },
    .irw_source_note = function(...) invisible(NULL),
    .fetch_cov_metadata_table_safe = function() NULL)
  out <- irw_fetch("holzinger_1939_grantwhite9", source = "cov")
  expect_identical(asked, "cov")
  expect_identical(out, pub)
  expect_warning(irw_fetch("holzinger_1939_grantwhite9", source = "cov",
                           cov_args = list(method = "spearman")), "ignored")
})

test_that("the cov metadata fetcher reads cov_metadata, filtered to live tables", {
  local_irw_binding(".irw_env", new.env())
  read <- character()
  local_mocked_bindings(
    .irw_open_meta_dataset = function() list(
      properties = list(version = list(tag = "v74.0")),
      table = function(name) { read <<- c(read, name)
        list(to_tibble = function() tibble::tibble(
          table = c("boer_2016_lmx", "boer_2016_lmx", "gone_2020"), group = c("s1", "s2", NA)))}),
    .irw_filter_rows_to_live_tables = function(df, source) {
      expect_identical(source, "cov"); df[df$table != "gone_2020", ] })
  m <- irw:::.fetch_cov_metadata_table()
  expect_identical(read, "cov_metadata")
  expect_identical(m$group, c("s1", "s2"))
  irw:::.fetch_cov_metadata_table()  # cached
  expect_identical(read, "cov_metadata")
})

test_that("irw_metadata(source = 'cov') dispatches; biblio reads cov_biblio", {
  # An earlier file's mock of irw_metadata can leak into this one
  # (helper-redivis.R), so call the real implementation explicitly.
  real_metadata <- .irw_pristine_bindings$irw_metadata
  local_mocked_bindings(.fetch_cov_metadata_table = function() tibble::tibble(table = "x"))
  expect_identical(real_metadata(source = "cov")$table, "x")
  local_mocked_bindings(.fetch_cov_biblio_table = function() tibble::tibble(table = "x", Derived_License = "GPL-3"))
  expect_identical(irw:::.irw_filter_biblio("cov")$Derived_License, "GPL-3")
})

test_that("the cov biblio fetcher reads cov_biblio, filtered to live tables", {
  local_irw_binding(".irw_env", new.env())
  read <- character()
  local_mocked_bindings(
    .irw_open_meta_dataset = function() list(
      properties = list(version = list(tag = "v75.0")),
      table = function(name) { read <<- c(read, name)
        list(to_tibble = function() tibble::tibble(table = c("burt_1915_emotions11", "gone_2020")))}),
    .irw_filter_rows_to_live_tables = function(df, source) {
      expect_identical(source, "cov"); df[df$table != "gone_2020", ] })
  b <- irw:::.fetch_cov_biblio_table()
  expect_identical(read, "cov_biblio")
  expect_identical(b$table, "burt_1915_emotions11")
  irw:::.fetch_cov_biblio_table()  # cached
  expect_identical(read, "cov_biblio")
})

test_that("irw_cov2list returns every sample, padded to the union of variables", {
  pub <- data.frame(
    stat  = c("cor", "cor", "cor", "sd", "cor", "cor", "cor", "cor", "cor", "cor"),
    var1  = c("a", "b", "b", "a", "a", "c", "c", "b", "b", "c"),
    var2  = c("a", "a", "b", NA, "a", "a", "c", "a", "b", "b"),
    value = c(1, 0.4, 1, 2, 1, 0.3, 1, 0.5, 1, 0.2),
    n     = c(rep(100, 4), rep(80, 6)),
    group = c(rep("Conner, Warren (1999a) 1", 4), rep("study (2)", 6)))
  x <- irw_cov2list(pub)
  expect_identical(names(x$matrices), c("Conner, Warren (1999a) 1", "study (2)"))
  expect_identical(x$n, c("Conner, Warren (1999a) 1" = 100L, "study (2)" = 80L))
  expect_identical(x$variables, c("a", "b", "c"))
  expect_identical(x$type, "cor")
  m1 <- x$matrices[[1]]
  expect_identical(dimnames(m1), list(c("a", "b", "c"), c("a", "b", "c")))
  expect_true(all(is.na(m1["c", ])) && all(is.na(m1[, "c"])))
  expect_equal(m1["a", "b"], 0.4)
  expect_equal(x$matrices[[2]]["c", "b"], 0.2)
  expect_equal(x$sd[[1]], c(a = 2))
  expect_length(x$sd[[2]], 0)
})

test_that("irw_cov2list handles one sample, cov tables and mixed types", {
  one <- data.frame(stat = "cov", var1 = c("a", "b", "b"), var2 = c("a", "a", "b"),
                    value = c(4, 1, 9), n = 50)
  x <- irw_cov2list(one)
  expect_identical(names(x$matrices), "all")
  expect_identical(x$type, "cov")
  expect_identical(x$n, c(all = 50L))
  mixed <- rbind(cbind(one, group = "g1"), cbind(transform(one, stat = "cor", value = c(1, .2, 1)), group = "g2"))
  expect_error(irw_cov2list(mixed), "mix correlation and covariance")
  expect_error(irw_cov2list(data.frame(x = 1)), "missing")
})

test_that("unreported cells are NA with a warning, and complete = TRUE drops the fewest variables", {
  # Ben, 2026-11-18 (ben-domingue/irw#3594): a cell the source did not report
  # is a row with an empty value.
  v <- c("a", "b", "c", "d")
  cells <- expand.grid(i = 1:4, j = 1:4)
  cells <- cells[cells$j <= cells$i, ]
  pub <- data.frame(stat = "cor", var1 = v[cells$i], var2 = v[cells$j],
                    value = ifelse(cells$i == cells$j, 1, 0.3), n = 120)
  pub$value[pub$var1 == "c" & pub$var2 == "a"] <- NA
  pub$value[pub$var1 == "d" & pub$var2 == "c"] <- NA
  expect_warning(m <- irw_cov2matrix(pub), "2 cells.*complete = TRUE.*\\(c\\)", class = "irw_cov_blank")
  expect_true(is.na(m$matrix["c", "a"]) && is.na(m$matrix["a", "c"]))
  expect_identical(m$dropped, character(0))
  m2 <- irw_cov2matrix(pub, complete = TRUE)
  expect_identical(m2$dropped, "c")
  expect_identical(rownames(m2$matrix), c("a", "b", "d"))
  expect_false(anyNA(m2$matrix))
  # a tie keeps the variable listed first
  one <- pub; one$value[one$var1 == "d" & one$var2 == "c"] <- 0.3
  expect_identical(irw_cov2matrix(one, complete = TRUE)$dropped, "c")
  # irw_cov2list keeps NA, silently
  pub$group <- "s1"
  expect_no_warning(x <- irw_cov2list(pub))
  expect_true(is.na(x$matrices$s1["c", "a"]))
})

test_that("a published table is put in the order its record gives", {
  # ben-domingue/irw#3594 (Ben, 2026-11-18): Redivis does not keep row order;
  # cov_metadata's `variables` does.
  pub <- data.frame(stat = c("cor", "cor", "sd", "cor", "cor", "cor", "cor"),
                    var1 = c("a", "a", "b", "b", "c", "c", "b"),
                    var2 = c("a", "b", NA, "b", "a", "c", "c"),
                    value = c(1, 0.4, 2, 1, 0.3, 1, 0.5), n = 50)
  meta <- data.frame(table = "t1", group = NA, variables = "c = third | b | a")
  out <- irw:::.irw_cov_order_rows(pub, meta, "t1")
  expect_identical(rownames(irw_cov2matrix(out)$matrix), c("c", "b", "a"))
  expect_identical(out$stat[nrow(out)], "sd")
  expect_identical(sort(out$value), sort(pub$value))
  # cells are the same either way round
  expect_equal(irw_cov2matrix(out)$matrix["a", "b"], 0.4)
  # no record, a blank record, or names that do not match: unchanged
  expect_identical(irw:::.irw_cov_order_rows(pub, NULL, "t1"), pub)
  expect_identical(irw:::.irw_cov_order_rows(pub, transform(meta, variables = ""), "t1"), pub)
  expect_identical(irw:::.irw_cov_order_rows(pub, transform(meta, variables = "a | b"), "t1"), pub)
  # groups follow the record's order
  two <- rbind(transform(pub, group = "g2"), transform(pub, group = "g1"))
  m2 <- data.frame(table = "t1", group = c("g1", "g2"), variables = c("a | b | c", "c | b | a"))
  o2 <- irw:::.irw_cov_order_rows(two, m2, "t1")
  expect_identical(unique(o2$group), c("g1", "g2"))
  expect_identical(rownames(irw_cov2matrix(o2, group = "g2")$matrix), c("c", "b", "a"))
})

test_that("reliability rows come back as `rel`, beside sd and mean", {
  # Ben, 2026-11-18: per-variable reliability is a stat = "rel" row.
  pub <- data.frame(stat = c("cor", "cor", "cor", "sd", "rel", "rel"),
                    var1 = c("a", "b", "b", "a", "a", "b"),
                    var2 = c("a", "a", "b", NA, NA, NA),
                    value = c(1, 0.4, 1, 2, 0.81, 0.77), n = 90)
  m <- irw_cov2matrix(pub)
  expect_identical(m$rel, c(a = 0.81, b = 0.77))
  expect_identical(m$sd, c(a = 2))
  x <- irw_cov2list(transform(pub, group = "s1"))
  expect_identical(x$rel$s1, c(a = 0.81, b = 0.77))
})
