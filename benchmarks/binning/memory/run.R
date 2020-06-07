#devtools::load_all("~/repos/compboost")
library(compboost)

cat("Loading package!")

dat = as.data.frame(matrix(rnorm(5100000), nrow = 100000L))

mstop = 1000L
ubin = 2

cboost = Compboost$new(dat, "V1", loss = LossQuadratic$new())
temp = lapply(colnames(dat)[-1], function (feat) {
cboost$addBaselearner(feat, "spline", BaselearnerPSpline, degree = 3,
  n_knots = 20, penalty = 2, differences = 2, bin_root = 2)
})

cboost$train(mstop, trace = mstop/4)
