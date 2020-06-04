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

# save(ll_times, file = "ll_times.Rda")
load("ll_times.Rda")
bm = do.call(rbind, ll_times)

library(dplyr)
library(tidyr)
library(ggplot2)

# Find font with `fc-list | grep "Gyre Bonum"`
sysfonts::font_add("Gyre Bonum",
    regular = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-regular.otf",
    bold = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-bold.otf")
showtext::showtext_auto()

font_scale = 3

p = bm %>%
  group_by(nrows, ncols, iter, rep) %>%
  mutate(seconds = init + fit) %>%
  summarize(fac = seconds[method == "No"] / seconds[method == "Yes"]) %>%
  ggplot(aes(x = as.factor(iter), y = fac, color = as.factor(nrows))) +
    geom_violin(show.legend = FALSE, fill = NA) +
    stat_summary(fun = median, geom = "line", aes(group = 0), lwd = 1, alpha = 0.7, show.legend = FALSE) +
    geom_jitter(alpha = 0.6, width = 0.1, stroke = 0, show.legend = FALSE) +
    xlab("Number of iterations") +
    ylab("Relative speedup with binning\ncompared to no binning") +
    theme_minimal(base_family = "Gyre Bonum") +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(breaks = c(0, 5, 10)) +
      scale_x_discrete(breaks = c(200,400,600,800,1000)) +
    facet_grid(reorder(paste0("p = ", ncols), ncols) ~ reorder(paste0("n = ", nrows), nrows)) +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale)
    )

dinA4width = 210 * font_scale
ggsave(plot = p, filename = "bin_vs_nobin.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3, units = "mm")








bm_sum = bm %>%
  group_by(nrows, ncols, iter, rep) %>%
  mutate(seconds = init + fit) %>%
  summarize(fac = seconds[method == "No"] / seconds[method == "Yes"]) %>%
  filter(iter %in% c(200,900))
#   summarize(fac = median(fac)) %>%
#   group_by(nrows, ncols, iter) %>%

ll_labs = lapply(unique(bm_sum$iter), function (i) {
  bm_temp = bm_sum[bm_sum$iter == i,]
  mods = lapply(unique(bm_temp$ncols), function (p) {
    mod = nls(fac ~ b + nrows^a, data = bm_temp[bm_temp$ncols == p,])
    pred = predict(mod, newdata = data.frame(nrows = 50000))
    return (data.frame(p = p, x = 50000, y = pred, a = coef(mod)["a"], b = coef(mod)["b"]))
  })

  df_labs = do.call(rbind, mods)
  df_labs$label = paste0("a = ", round(df_labs$a, 3))
  df_labs$ncols = df_labs$p
  df_labs$iter = i

  df_labs
})
df_labs = do.call(rbind, ll_labs)

bm_sum %>%
  ggplot(aes(x = nrows, y = fac, color = as.factor(ncols))) +
    geom_point() +
#     geom_smooth(method = "nls", formula = formula(y ~ b + x^a), method.args = list(start = c(a = 0.5, b = 0, c = 0))) +
    geom_smooth(method = "nls", formula = formula(y ~ 1 + x^b), se = FALSE) +
    geom_label(data = df_labs, aes(x = x, y = y, label = label), size = 7, show.legend = FALSE) +
    theme_minimal(base_family = "Gyre Bonum") +
    scale_color_brewer(palette = "Set1") +
    facet_grid(iter ~ .) +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale)
    )


