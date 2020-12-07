library(data.table)
library(collapse)

load("dt1.RData")

dt3 <- rbind(dt1,dt1)
dt3[,ID := rep(c("act","pred"), each = nrow(dt1))]
# compare functions -------------------------------------------------------

group_cols = c(
  "MAP", "MAT", "alpha", "lseas", "lambdar",
  "daily_diff_temp", "annual_diff_temp",
  "shift", "shiftdays", "soil"
)

# columns from which to calculate the rmse
# use all numeric columns except for the group_cols
rmse_cols <- names(dt3)[sapply(dt3, is.numeric)]
rmse_cols <- rmse_cols[!rmse_cols %in% group_cols]

not_parallel <- function(){
  dt3[, lapply(.SD, function(x) {
    ids <- ss(dt3, .I, "ID")

    Metrics::rmse(
      actual = fsubset(x, ids == "act"),
      predicted = fsubset(x, ids == "pred"))
  }),
  by = group_cols,
  .SDcols = rmse_cols]
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
  .SDcols = rmse_cols
  ]
}

microbenchmark::microbenchmark(not_parallel(), parallel())



