library(data.table)
library(collapse)

githubURL <- "https://github.com/selinaZitrone/TestRMSEParallel/raw/master/DATASET.Rdata"
load(url(githubURL))

# compare functions -------------------------------------------------------

group_cols = c(
  "MAP", "MAT", "alpha", "lseas", "lambdar",
  "daily_diff_temp",
  "shift", "shiftdays", "soil"
)

not_parallel <- function(){
  dt3[, lapply(.SD, function(x) {
    ids <- ss(dt3, .I, "ID")

    Metrics::rmse(
      actual = fsubset(x, ids == "act"),
      predicted = fsubset(x, ids == "pred"))
  }),
  by = group_cols,
  .SDcols = letters]
}

parallel <- function(){
  dt3[, dapply(.SD, function(x) {
    ids <- ss(dt3, .I, "ID")

    Metrics::rmse(
      actual = fsubset(x, ids == "act"),
      predicted = fsubset(x, ids == "pred")
    )

  }, parallel = TRUE, mc.cores = parallel::detectCores(), drop = FALSE),
  by = group_cols,
  .SDcols = letters
  ]
}



