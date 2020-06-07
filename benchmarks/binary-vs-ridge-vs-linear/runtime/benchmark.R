devtools::load_all("~/repos/compboost")

nobs = c(5000L, 10000L, 20000L, 50000L)
# nobs = c(1000L, 5000L, 10000L, 50000L)
nfeat = c(5L, 10L, 20L)
nreps = 5L
nclasses = c(5L, 15L, 20L)
method = c("Binary", "Ridge")
iters = c(5L, 10L, 15L, 20L) * 100L

dim(expand.grid(nobs, nfeat, nreps, nclasses, method, iters)) * nreps

ll_times = list()
k = 1L

all_classes = apply(expand.grid(LETTERS, LETTERS), 1, paste, collapse = "")

for (n in nobs) {
  for (p in nfeat) {
    for (nc in nclasses) {
      for (i in seq_len(nreps)) {
        for (md in method) {
          for (iter in iters) {

            feats = lapply(seq_len(p), function (x) {
              classes = sample(x = all_classes, size = nc)
              gmean = sample(nc) * rnorm(1)
              idx = sample(x = seq_len(nc), size = n, replace = TRUE)
              classes[idx]
            })

            y = rnorm(n)
            dat = data.frame(y = y, do.call(cbind,feats))

            time_start = proc.time()

            response = ResponseRegr$new("y", as.matrix(dat$y))

            factory_list = BlearnerFactoryList$new()
            if (md == "Ridge") {
              temp = lapply(names(dat)[-which(names(dat) == "y")], function (fn) {
                cdata_source = CategoricalData$new(dat[[fn]], paste0("x", fn))
                bl = BaselearnerCategoricalRidge$new(cdata_source, list(df = 0))
                factory_list$registerFactory(bl)
              })
            }
            if (md == "Binary") {
              temp = lapply(names(dat)[-which(names(dat) == "y")], function (fn) {
                cdata_source = CategoricalData$new(dat[[fn]], paste0("x", fn))
                bl_list = lapply(unique(dat[[fn]]), function (cl) {
                  bl = BaselearnerCategoricalBinary$new(cdata_source, cl)
                  factory_list$registerFactory(bl)
                })
              })
            }
            loss_quadratic = LossQuadratic$new()
            optimizer = OptimizerCoordinateDescent$new()

            log_iterations = LoggerIteration$new(" iterations", TRUE, iter)
            logger_list = LoggerList$new()
            logger_list$registerLogger(log_iterations)

            cboost = Compboost_internal$new(
              response      = response,
              learning_rate = 0.05,
              stop_if_all_stopper_fulfilled = FALSE,
              factory_list = factory_list,
              loss         = loss_quadratic,
              logger_list  = logger_list,
              optimizer    = optimizer
            )

            time_init = proc.time() - time_start

            cboost$train(trace = as.integer(iter / 4))

            time_fit = proc.time() - time_start + time_init

            ll_times[[k]] = data.frame(
              init = unname(time_init[3]),
              fit = unname(time_fit[3]),
              method = md,
              nrows = n,
              ncols = p,
              iter = iter,
              nclasses = nc,
              rep = i)

            k = k + 1L
          }
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
  group_by(nrows, ncols, iter, nclasses, method) %>%
  mutate(seconds = init + fit) %>%
  summarize(Initialization = median(init), Training = median(fit), se_total = sd(init + fit), se_init = sd(init), se_fit = sd(fit),
    sd_min = median(init + fit)-sd(init+fit), sd_max = median(init+fit)+sd(init+fit)) %>%
  ggplot(aes(x = as.factor(iter), y = Initialization + Training, fill = method)) +
    geom_bar(stat = "identity", position = position_dodge(0.9)) +
    geom_errorbar(aes(ymin = sd_min, ymax = sd_max), color = "black", width = 0.2, position = position_dodge(0.9)) +
    xlab("Number of iterations") +
    ylab("Seconds") +
    theme_minimal(base_family = "Gyre Bonum") +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1") +
    labs(fill = "Method") +
    facet_grid(reorder(paste0("p = ", ncols), ncols) ~ reorder(paste0("n = ", nrows), nrows) + reorder(paste0("classes = ", nclasses), nclasses)) +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      strip.text = element_text(color = "white", face = "bold", size = 4 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale)
    )

dinA4width = 210 * font_scale
ggsave(plot = p, filename = "binary_vs_ridge_seconds.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3, units = "mm")








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


