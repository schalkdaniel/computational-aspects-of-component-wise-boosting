createRunFile = function (n, p, iter, ubin, filename) {
  txt = paste0(
    "devtools::load_all(\"~/repos/compboost\")\n\ncat(\"Loading package!\")\n\ndat = as.data.frame(matrix(rnorm(", (p + 1) * n, "), nrow = ", n, "L))\n\nmstop = ", iter, "L\nubin = ", ubin,
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


  return (data.frame(init = bites_fit - bites_start, fit = bites_final - bites_fit, final = bites_final, cboost_max = mb_max))
}

nobs = c(1000L, 5000L, 10000L, 50000L)
nfeat = c(5L, 10L, 20L, 50L)
binning = c(0, 2)
# iters = 1:10 * 100
iters = seq(2,10,2) * 100

# nobs = c(100L, 50000L)
# nfeat = 10L
# binning = c(0,2)
# iters = 300
reps = 3L

ll_memory = list()
k = 1L

for (n in nobs) {
  for (p in nfeat) {
    for (ubin in binning) {
      for (iter in iters) {
        for (i in seq_len(reps)) {

          filename = paste0(getwd(), "/run.R")
          file_suffix = paste0("n", n, "-p", p, "-it", iter, "-bin", ubin, "-rep", i)
          massif_out = paste0("massif.out.", file_suffix)
          log_file = paste0("log-cboost-", file_suffix, ".txt")

          cat(file_suffix, "\n")

          createRunFile(n = n, p = p, iter = iter, ubin = ubin, filename = filename)
#           system(paste0("R -d \"valgrind --tool=massif --threshold=0 --detailed-freq=1 --time-unit=B --max-snapshots=500 --pages-as-heap=yes --verbose --trace-children=yes --massif-out-file=", massif_out, " --log-file=", log_file, "\" -e \"source('", filename, "')\""))
          system(paste0("R -d \"valgrind --tool=massif --threshold=0 --detailed-freq=1 --time-unit=B --pages-as-heap=yes --verbose --trace-children=yes --massif-out-file=", massif_out, " --log-file=", log_file, "\" -e \"source('", filename, "')\""))

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
  mutate(memory = cboost_max) %>%
#   mutate(memory = final / 1024^2) %>%
  summarize(fac = memory[method == "No"] / memory[method == "Yes"]) %>%
  as.data.frame()

p = bm_mem %>%
  group_by(nrows, ncols, iter, rep) %>%
  mutate(memory = cboost_max) %>%
#   mutate(memory = final / 1024^2) %>%
  summarize(fac = memory[method == "No"] / memory[method == "Yes"]) %>%
  ggplot(aes(x = as.factor(iter), y = fac, color = as.factor(nrows))) +
    geom_violin(show.legend = FALSE, fill = NA) +
    stat_summary(fun = median, geom = "line", aes(group = 0), lwd = 1, alpha = 0.7, show.legend = FALSE) +
    geom_jitter(alpha = 0.6, width = 0.1, stroke = 0, show.legend = FALSE) +
    xlab("Number of iterations") +
    ylab("Relative memory consumption with binning\ncompared to no binning") +
    theme_minimal(base_family = "Gyre Bonum") +
    scale_color_brewer(palette = "Set1") +
   # scale_y_continuous(breaks = c(0, 5, 10)) +
   # scale_x_discrete(breaks = c(200,400,600,800,1000)) +
    facet_grid(reorder(paste0("p = ", ncols), ncols) ~ reorder(paste0("n = ", nrows), nrows)) +
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
