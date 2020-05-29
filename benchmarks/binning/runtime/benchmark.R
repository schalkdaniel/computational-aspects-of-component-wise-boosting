devtools::load_all("~/repos/compboost")

nobs = c(1000L, 5000L, 10000L, 50000L)
nfeat = c(5L, 10L, 20L, 50L)
nreps = 10L
binning = c(0, 2)
iters = 1:10 * 100

ll_times = list()
k = 1L

for (n in nobs) {
  for (p in nfeat) {
    for (i in seq_len(nreps)) {
      for (ubin in binning) {
        for (iter in iters) {
          dat = as.data.frame(matrix(rnorm((p + 1) * n), nrow = n))

          time_start = proc.time()

          cboost = Compboost$new(dat, "V1", loss = LossQuadratic$new())
          temp = lapply(colnames(dat)[-1], function (feat) {
            cboost$addBaselearner(feat, "spline", BaselearnerPSpline, degree = 3,
              n_knots = 20, penalty = 2, differences = 2, bin_root = ubin)
          })

          time_init = proc.time() - time_start

          cboost$train(iter, trace = 0L)

          time_fit = proc.time() - time_start + time_init

          ll_times[[k]] = data.frame(
            init = unname(time_init[3]),
            fit = unname(time_fit[3]),
            method = if (ubin > 0) "Yes" else "No",
            nrows = n,
            ncols = p,
            iter = iter,
            rep = i)

          k = k + 1L
        }
      }
    }
  }
}

save(ll_times, file = "ll_times.Rda")
load("ll_times.Rda")
bm = do.call(rbind, ll_times)

library(dplyr)
library(tidyr)
library(ggplot2)

p = bm %>%
  group_by(nrows, ncols, iter, rep) %>%
  mutate(seconds = init + fit) %>%
  summarize(fac = seconds[method == "No"] / seconds[method == "Yes"]) %>%
  ggplot(aes(x = as.factor(iter), y = fac, color = as.factor(nrows))) +
    geom_violin(show.legend = FALSE, fill = NA) +
    stat_summary(fun = median, geom = "smooth", aes(group = 0), lwd = 1, show.legend = FALSE) +
    geom_jitter(alpha = 0.6, width = 0.1, stroke = 0, show.legend = FALSE) +
    xlab("Number of iterations") +
    ylab("Relative speedup with binning\ncompared to no binning") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(breaks = c(0, 5, 10)) +
    facet_grid(reorder(paste0("n = ", ncols), ncols) ~ reorder(paste0("p = ", nrows), nrows)) +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      strip.text = element_text(color = "white", face = "bold"),
      axis.text.x = element_text(angle = 45, vjust = 0.5)
    )

ggsave(plot = p, filename = "bin_vs_nobin.pdf", width = 10, height = 10)
