createRunFile = function (n, p, iter, ubin, filename) {
  txt = paste0(
    "#devtools::load_all(\"~/repos/compboost\")\nlibrary(compboost)\n\ncat(\"Loading package!\")\n\ndat = as.data.frame(matrix(rnorm(", (p + 1) * n, "), nrow = ", n, "L))\n\nmstop = ", iter, "L\nubin = ", ubin,
  "\n\ncboost = Compboost$new(dat, \"V1\", loss = LossQuadratic$new())\n",
  "temp = lapply(colnames(dat)[-1], function (feat) {\n",
  "cboost$addBaselearner(feat, \"spline\", BaselearnerPSpline, degree = 3,\n",
  "  n_knots = 20, penalty = 2, differences = 2, bin_root = ", ubin,")\n",
  "})\n\n",
  "cboost$train(mstop, trace = mstop/4)")

  file_conn = file(filename)
  writeLines(txt, file_conn)
  close(file_conn)
}

extractInitFitTime = function (massif_file) {
  if (! file.exists(massif_file)) return (data.frame(init = NA, fit = NA, final = NA))

  txt = readLines(massif_file)
  snapshot_idx = grep(pattern = "snapshot", x = txt)

  compboost_idx = grep(pattern = "compboost", x = txt)
  snapshot_compboost = max(snapshot_idx[snapshot_idx < min(compboost_idx)])

  fit_idx = grep(pattern = "cboost::Compboost::train", x = txt)
  fit_snapshot = max(snapshot_idx[snapshot_idx < min(fit_idx)])

  final_snapshot = max(snapshot_idx)

  extractSnapshotBites = function (txt, idx) {
    if (! grepl(pattern = "snapshot", x = txt[idx])) {
      stop("idx is not of a snapshot!")
    }

    bites = txt[idx + 3]
    return (as.integer(strsplit(x = bites, split = "=")[[1]][2]))
  }

  bites_start = extractSnapshotBites(txt, snapshot_compboost)
  bites_fit = extractSnapshotBites(txt, fit_snapshot)
  bites_final = extractSnapshotBites(txt, final_snapshot)

  mb_max = unlist(lapply(snapshot_idx[snapshot_idx >= snapshot_compboost], function (idx,...) extractSnapshotBites(txt,idx), txt = txt))
  mb_max = (max(mb_max) - min(mb_max)) / 1024^2


  return (data.frame(init = (bites_fit - bites_start) / 1024^2, fit = (bites_final - bites_fit) / 1024^2, final = bites_final / 1024^2, cboost_max = mb_max))
}

nobs = c(1000L, 10000L, 40000L, 70000L, 100000L)
nfeat = c(10L, 25L, 50L, 75L, 100L)
binning = c(0, 2)
iters = c(2, 6, 10) * 100

# nobs = c(100000L)
# nfeat = 5L
# binning = 0
# iters = 400
reps = 1L

ll_memory = list()
k = 1L

for (n in nobs) {
  for (p in nfeat) {
    for (ubin in binning) {
      for (iter in iters) {
        for (i in seq_len(reps)) {

          filename = paste0(getwd(), "/run.R")
          file_suffix = paste0("n", n, "-p", p, "-it", iter, "-bin", ubin, "-rep", i)
          massif_out = paste0("out-files/massif.out.", file_suffix)
          log_file = paste0("out-files/log/log-cboost-", file_suffix, ".txt")

          cat(file_suffix, "\n")

          createRunFile(n = n, p = p, iter = iter, ubin = ubin, filename = filename)
          system(paste0("R -d \"valgrind --tool=massif --stacks=yes --threshold=0.1 --detailed-freq=1 --time-unit=B --verbose --trace-children=yes --massif-out-file=", massif_out, " --log-file=", log_file, "\" -e \"source('", filename, "')\""))
#           system(paste0("R -d \"valgrind --tool=massif --threshold=0 --detailed-freq=1 --time-unit=B --pages-as-heap=yes --verbose --trace-children=yes --massif-out-file=", massif_out, " --log-file=", log_file, "\" -e \"source('", filename, "')\""))

          file.remove(filename)

          ll_memory[[k]] = data.frame(
            method = if (ubin > 0) "Yes" else "No",
            nrows = n,
            ncols = p,
            iter = iter,
            rep = i,
            log_file = log_file,
            massif_file = massif_out)

          k = k + 1L
        }
      }
    }
  }
}


bm = do.call(rbind, ll_memory)
mems = do.call(rbind, lapply(bm[["massif_file"]], extractInitFitTime))
bm_mem = cbind(bm, mems)
bm_mem$load_r_mb = 79.2
bm_mem

save(bm_mem, file = "bm_mem.Rda")
load("bm_mem.Rda")

# massif_file = bm$massif_file[1]
# extractInitFitTime(massif_file)

library(dplyr)
library(tidyr)
library(ggplot2)

# Find font with `fc-list | grep "Gyre Bonum"`
sysfonts::font_add("Gyre Bonum",
    regular = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-regular.otf",
    bold = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-bold.otf")
showtext::showtext_auto()

font_scale = 3

bm_mem %>%
  group_by(nrows, ncols, iter, rep) %>%
  mutate(memory = final - load_r_mb) %>%
#   mutate(memory = final / 1024^2) %>%
  summarize(fac = memory[method == "No"] / memory[method == "Yes"]) %>%
  as.data.frame()

p = bm_mem %>%
  group_by(nrows, ncols, iter, rep) %>%
  mutate(memory = final - load_r_mb) %>%
  summarize(fac = memory[method == "No"] / memory[method == "Yes"]) %>%
  ggplot(aes(x = iter, y = fac, color = as.factor(nrows))) +
    geom_line() +
    xlab("Number of iterations") +
    ylab("Relative memory consumption with binning\ncompared to no binning") +
    theme_minimal(base_family = "Gyre Bonum") +
    scale_color_brewer(palette = "Set1") +
    ylim(0, 3) +
    facet_grid(reorder(paste0("p = ", ncols), ncols) ~ .) +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale)
    )
p

dinA4width = 210 * font_scale
ggsave(plot = p, filename = "bin_vs_nobin.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3, units = "mm")


bm_mem %>% filter(rep == 1, iter == 1000) %>% ggplot(aes(x = nrows, y = cboost_max, color = as.factor(ncols), linetype = method)) + geom_line()


mem100 = read.table("mem-n100-p10-it300-bin0-rep1.txt", header = FALSE)[[1]]
mem100 = mem100 - mem100[1]


mem50000 = read.table("mem-n10000-p10-it300-bin0-rep1.txt", header = FALSE)[[1]]
mem50000 = mem50000 - mem50000[1]

par(mfrow = c(1,2))
plot(mem100 / 1024, type = "b")
plot(mem50000 / 1024, type = "b")
par(mfrow = c(1,1))





mem1000 = read.table("mem-n1000-p5-it400-bin2-rep1.txt", header = FALSE)[[1]]
mem1000 = mem1000 - mem1000[1]

mem10000 = read.table("mem-n10000-p5-it400-bin2-rep1.txt", header = FALSE)[[1]]
mem10000 = mem10000 - mem10000[1]

par(mfrow = c(1,2))
plot(mem1000 / 1024, type = "b")
plot(mem10000 / 1024, type = "b")
par(mfrow = c(1,1))

mem100000 = read.table("mem-n100000-p5-it400-bin0-rep1.txt", header = FALSE)[[1]]
mem100000 = mem100000 - mem100000[1]

plot(mem100000 / 1024, type = "b")

memstart = read.table("start-r-compboost.txt", header = FALSE)[[1]]
memstart = memstart - memstart[1]

plot(memstart / 1024, type = "b")


relArmaSp = function (n, p, n_per_row)
{
  kb_sp_full = n * n_per_row * 3 * 8 / 1024
  kb_sp_bin = (trunc(sqrt(n)) * n_per_row * 3 + n) * 8 / 1024
  return (c(kb_full = kb_sp_full, kb_bin = kb_sp_bin, rel = kb_sp_full / kb_sp_bin))
}
relArmaSp(100000, 20, 5)


bm_sum = bm_mem %>%
  group_by(nrows, ncols, iter, rep) %>%
  mutate(memory = final - load_r_mb) %>%
  summarize(fac = memory[method == "No"] / memory[method == "Yes"])

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

po = bm_sum %>%
  ggplot(aes(x = nrows, y = fac, color = as.factor(ncols))) +
    geom_point() +
    geom_line() +
    theme_minimal(base_family = "Gyre Bonum") +
    scale_color_brewer(palette = "Set1") +
    ylim(0, 3) +
    labs(color = "Number of\nfeatures") +
    xlab("Number of observations") +
    ylab("Relative memory consumption with binning\ncompared to no binning") +
    facet_grid(reorder(paste0("iter = ", iter), iter) ~ .) +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale),
      legend.title = element_text(size = 6 * font_scale),
      legend.text = element_text(size = 6 * font_scale)
    )

dinA4width = 210 * font_scale
ggsave(plot = po, filename = "bin_vs_nobin_obs.pdf", width = dinA4width * 2/6, height = dinA4width * 2/3, units = "mm")


ggplot(data = bm_mem, aes(x = nrows, y = final, color = method)) +
  geom_point() +
  geom_line() +
  facet_grid(iter ~ ncols)



dat = as.data.frame(matrix(rnorm(10100000), nrow = 100000L))
library(mboost)

time = proc.time()
mod = mboost(V1 ~ ., data = dat, baselearner = "bbs", control = boost_control(mstop = 200L))
time = proc.time() - time

a = read.table("~/test/mboost.txt", header = FALSE)[[1]]
a = a-a[1]
plot(a / 1024, type = "b", ylab = "Mem Change in MB", xlab = "Time")

